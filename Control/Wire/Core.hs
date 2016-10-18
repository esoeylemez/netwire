-- |
-- Module:     Control.Wire.Core
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Core
    ( -- * Wires
      Wire(..),
      stepWire,

      -- * Constructing wires
      mkConst,
      mkEmpty,
      mkGen,
      mkGen_,
      mkGenN,
      mkId,
      mkPure,
      mkPure_,
      mkPureN,
      mkSF,
      mkSF_,
      mkSFN,

      -- * Data flow and dependencies
      delay,
      evalWith,
      force,
      forceNF,

      -- * Utilities
      (&&&!),
      (***!),
      lstrict,
      mapWire
    )
    where

import qualified Data.Semigroup as Sg
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.DeepSeq hiding (force)
import Control.Monad
import Control.Monad.Fix
import Control.Parallel.Strategies
import Data.Profunctor
import Data.Monoid
import Data.String
import Prelude hiding ((.), id)


-- | A wire is a signal function.  It maps a reactive value to another
-- reactive value.

data Wire s e m a b where
    WArr   :: (Either e a -> Either e b) -> Wire s e m a b
    WConst :: Either e b -> Wire s e m a b
    WGen   :: (s -> Either e a -> m (Either e b, Wire s e m a b)) -> Wire s e m a b
    WId    :: Wire s e m a a
    WPure  :: (s -> Either e a -> (Either e b, Wire s e m a b)) -> Wire s e m a b

instance (Monad m, Monoid e) => Alternative (Wire s e m a) where
    empty = WConst (Left mempty)

    w1@(WConst (Right _)) <|> _ = w1
    w1@WId <|> _ = w1

    WConst (Left ex) <|> w2 = mapLeft (ex <>) w2

    w1' <|> w2' =
        WGen $ \ds mx' ->
            liftM2 (\(mx1, w1) (mx2, w2) -> lstrict (choose mx1 mx2, w1 <|> w2))
                   (stepWire w1' ds mx')
                   (stepWire w2' ds mx')

        where
        choose mx1@(Right _) _       = mx1
        choose _ mx2@(Right _)       = mx2
        choose (Left ex1) (Left ex2) = Left (ex1 <> ex2)

instance (Monad m) => Applicative (Wire s e m a) where
    pure = WConst . Right

    wf' <*> wx' =
        WGen $ \ds mx' ->
            liftM2 (\(mf, wf) (mx, wx) -> lstrict (mf <*> mx, wf <*> wx))
                   (stepWire wf' ds mx')
                   (stepWire wx' ds mx')

instance (Monad m) => Arrow (Wire s e m) where
    arr f = WArr (fmap f)

    first w' =
        WGen $ \ds mxy' ->
            liftM (\(mx, w) -> lstrict (liftA2 (,) mx (fmap snd mxy'), first w))
                  (stepWire w' ds (fmap fst mxy'))

instance (Monad m, Monoid e) => ArrowChoice (Wire s e m) where
    left w' =
        WGen $ \ds mmx' ->
            liftM (fmap Left ***! left) .
            stepWire w' ds $
            case mmx' of
              Right (Left x)  -> Right x
              Right (Right _) -> Left mempty
              Left ex         -> Left ex

    right w' =
        WGen $ \ds mmx' ->
            liftM (fmap Right ***! right) .
            stepWire w' ds $
            case mmx' of
              Right (Right x)  -> Right x
              Right (Left _)   -> Left mempty
              Left ex          -> Left ex

    wl' +++ wr' =
        WGen $ \ds mmx' ->
            case mmx' of
              Right (Left x) -> do
                  liftM2 (\(mx, wl) (_, wr) -> lstrict (fmap Left mx, wl +++ wr))
                         (stepWire wl' ds (Right x))
                         (stepWire wr' ds (Left mempty))
              Right (Right x) -> do
                  liftM2 (\(_, wl) (mx, wr) -> lstrict (fmap Right mx, wl +++ wr))
                         (stepWire wl' ds (Left mempty))
                         (stepWire wr' ds (Right x))
              Left ex ->
                  liftM2 (\(_, wl) (_, wr) -> lstrict (Left ex, wl +++ wr))
                         (stepWire wl' ds (Left ex))
                         (stepWire wr' ds (Left ex))

    wl' ||| wr' =
        WGen $ \ds mmx' ->
            case mmx' of
              Right (Left x) -> do
                  liftM2 (\(mx, wl) (_, wr) -> lstrict (mx, wl ||| wr))
                         (stepWire wl' ds (Right x))
                         (stepWire wr' ds (Left mempty))
              Right (Right x) -> do
                  liftM2 (\(_, wl) (mx, wr) -> lstrict (mx, wl ||| wr))
                         (stepWire wl' ds (Left mempty))
                         (stepWire wr' ds (Right x))
              Left ex ->
                  liftM2 (\(_, wl) (_, wr) -> lstrict (Left ex, wl ||| wr))
                         (stepWire wl' ds (Left ex))
                         (stepWire wr' ds (Left ex))

instance (MonadFix m) => ArrowLoop (Wire s e m) where
    loop w' =
        WGen $ \ds mx' ->
            liftM (fmap fst ***! loop) .
            mfix $ \ ~(mx, _) ->
                let d | Right (_, d) <- mx = d
                      | otherwise = error "Feedback broken by inhibition"
                in stepWire w' ds (fmap (, d) mx')

instance (Monad m, Monoid e) => ArrowPlus (Wire s e m) where
    (<+>) = (<|>)

instance (Monad m, Monoid e) => ArrowZero (Wire s e m) where
    zeroArrow = empty

instance (Monad m) => Category (Wire s e m) where
    id = WId

    w2' . w1' =
        WGen $ \ds mx0 -> do
            (mx1, w1) <- stepWire w1' ds mx0
            (mx2, w2) <- stepWire w2' ds mx1
            mx2 `seq` return (mx2, w2 . w1)

instance (Monad m, Monoid e) => Choice (Wire s e m) where
  left' = left
  right' = right

instance (Monad m, Floating b) => Floating (Wire s e m a b) where
    (**) = liftA2 (**)
    acos = fmap acos
    acosh = fmap acosh
    asin = fmap asin
    asinh = fmap asinh
    atan = fmap atan
    atanh = fmap atanh
    cos = fmap cos
    cosh = fmap cosh
    exp = fmap exp
    log = fmap log
    logBase = liftA2 logBase
    pi = pure pi
    sin = fmap sin
    sinh = fmap sinh
    sqrt = fmap sqrt
    tan = fmap tan
    tanh = fmap tanh

instance (Monad m, Fractional b) => Fractional (Wire s e m a b) where
    (/)   = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational

instance (Monad m) => Functor (Wire s e m a) where
    fmap f (WArr g)    = WArr (fmap f . g)
    fmap f (WConst mx) = WConst (fmap f mx)
    fmap f (WGen g)    = WGen (\ds -> liftM (fmap f ***! fmap f) . g ds)
    fmap f WId         = WArr (fmap f)
    fmap f (WPure g)   = WPure (\ds -> (fmap f ***! fmap f) . g ds)

instance (Monad m, IsString b) => IsString (Wire s e m a b) where
    fromString = pure . fromString

instance (Monad m, Monoid b) => Monoid (Wire s e m a b) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Monad m, Num b) => Num (Wire s e m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs    = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (Monad m) => Profunctor (Wire s e m) where
    dimap f g (WArr h)    = WArr (fmap g . h . fmap f)
    dimap _ g (WConst mx) = WConst (fmap g mx)
    dimap f g (WGen h)    = WGen (\ds -> liftM (fmap g ***! dimap f g) . h ds . fmap f)
    dimap f g WId         = WArr (fmap (g . f))
    dimap f g (WPure h)   = WPure (\ds -> (fmap g ***! dimap f g) . h ds . fmap f)

    lmap f (WArr g)       = WArr (g . fmap f)
    lmap _ (WConst mx)    = WConst mx
    lmap f (WGen g)       = WGen (\ds -> liftM (fmap (lmap f)) . g ds . fmap f)
    lmap f WId            = WArr (fmap f)
    lmap f (WPure g)      = WPure (\ds -> fmap (lmap f) . g ds . fmap f)

    rmap = fmap

instance (Monad m, Sg.Semigroup b) => Sg.Semigroup (Wire s e m a b) where
    (<>) = liftA2 (Sg.<>)

instance (Monad m, Monoid e) => Strong (Wire s e m) where
  first' = first
  second' = second


-- | Left-strict version of '&&&' for functions.

(&&&!) :: (a -> b) -> (a -> c) -> (a -> (b, c))
(&&&!) f g x' =
    let (x, y) = (f x', g x')
    in x `seq` (x, y)


-- | Left-strict version of '***' for functions.

(***!) :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
(***!) f g (x', y') =
    let (x, y) = (f x', g y')
    in x `seq` (x, y)


-- | This wire delays its input signal by the smallest possible
-- (semantically infinitesimal) amount of time.  You can use it when you
-- want to use feedback ('ArrowLoop'):  If the user of the feedback
-- depends on /now/, delay the value before feeding it back.  The
-- argument value is the replacement signal at the beginning.
--
-- * Depends: before now.

delay :: a -> Wire s e m a a
delay x' = mkSFN $ \x -> (x', delay x)


-- | Evaluate the input signal using the given 'Strategy' here.  This
-- wire evaluates only produced values.
--
-- * Depends: now.

evalWith :: Strategy a -> Wire s e m a a
evalWith s =
    WArr $ \mx ->
        case mx of
          Right x -> (x `using` s) `seq` mx
          Left _  -> mx


-- | Force the input signal to WHNF here.  This wire forces both
-- produced values and inhibition values.
--
-- * Depends: now.

force :: Wire s e m a a
force =
    WArr $ \mx ->
        case mx of
          Right x -> x `seq` mx
          Left ex -> ex `seq` mx


-- | Force the input signal to NF here.  This wire forces only produced
-- values.
--
-- * Depends: now.

forceNF :: (NFData a) => Wire s e m a a
forceNF =
    WArr $ \mx ->
        case mx of
          Right x -> x `deepseq` mx
          Left _  -> mx


-- | Left-strict tuple.

lstrict :: (a, b) -> (a, b)
lstrict (x, y) = x `seq` (x, y)


-- | Apply the given function to the wire's inhibition value.

mapLeft :: (Monad m) => (e -> e) -> Wire s e m a b -> Wire s e m a b
mapLeft _ w1@WId = w1
mapLeft f' w = mapOutput f w
    where
    f (Left ex) = Left (f' ex)
    f (Right x) = Right x


-- | Apply the given function to the wire's output.

mapOutput :: (Monad m) => (Either e b' -> Either e b) -> Wire s e m a b' -> Wire s e m a b
mapOutput f (WArr g)    = WArr (f . g)
mapOutput f (WConst mx) = WConst (f mx)
mapOutput f (WGen g)    = WGen (\ds -> liftM (f *** mapOutput f) . g ds)
mapOutput f WId         = WArr f
mapOutput f (WPure g)   = WPure (\ds -> (f *** mapOutput f) . g ds)


-- | Apply the given monad morphism to the wire's underlying monad.

mapWire ::
    (Monad m', Monad m)
    => (forall a. m' a -> m a)
    -> Wire s e m' a b
    -> Wire s e m a b
mapWire _ (WArr g)    = WArr g
mapWire _ (WConst mx) = WConst mx
mapWire f (WGen g)    = WGen (\ds -> liftM (lstrict . second (mapWire f)) . f . g ds)
mapWire _ WId         = WId
mapWire f (WPure g)   = WPure (\ds -> lstrict . second (mapWire f) . g ds)


-- | Construct a stateless wire from the given signal mapping function.

mkConst :: Either e b -> Wire s e m a b
mkConst = WConst


-- | Construct the empty wire, which inhibits forever.

mkEmpty :: (Monoid e) => Wire s e m a b
mkEmpty = mkConst (Left mempty)


-- | Construct a stateful wire from the given transition function.

mkGen :: (Monad m, Monoid s) => (s -> a -> m (Either e b, Wire s e m a b)) -> Wire s e m a b
mkGen f = loop mempty
    where
    loop s' =
        WGen $ \ds mx ->
            let s = s' <> ds in
            s `seq`
            case mx of
              Left ex  -> return (Left ex, loop s)
              Right x' -> liftM lstrict (f s x')


-- | Construct a stateless wire from the given transition function.

mkGen_ :: (Monad m) => (a -> m (Either e b)) -> Wire s e m a b
mkGen_ f = loop
    where
    loop =
        WGen $ \_ mx ->
            case mx of
              Left ex -> return (Left ex, loop)
              Right x -> liftM (lstrict . (, loop)) (f x)


-- | Construct a stateful wire from the given transition function.

mkGenN :: (Monad m) => (a -> m (Either e b, Wire s e m a b)) -> Wire s e m a b
mkGenN f = loop
    where
    loop =
        WGen $ \_ mx ->
            case mx of
              Left ex  -> return (Left ex, loop)
              Right x' -> liftM lstrict (f x')


-- | Construct the identity wire.

mkId :: Wire s e m a a
mkId = WId


-- | Construct a pure stateful wire from the given transition function.

mkPure :: (Monoid s) => (s -> a -> (Either e b, Wire s e m a b)) -> Wire s e m a b
mkPure f = loop mempty
    where
    loop s' =
        WPure $ \ds mx ->
            let s = s' <> ds in
            s `seq`
            case mx of
              Left ex  -> (Left ex, loop s)
              Right x' -> lstrict (f s x')


-- | Construct a pure stateless wire from the given transition function.

mkPure_ :: (a -> Either e b) -> Wire s e m a b
mkPure_ f = WArr $ (>>= f)


-- | Construct a pure stateful wire from the given transition function.

mkPureN :: (a -> (Either e b, Wire s e m a b)) -> Wire s e m a b
mkPureN f = loop
    where
    loop =
        WPure $ \_ mx ->
            case mx of
              Left ex  -> (Left ex, loop)
              Right x' -> lstrict (f x')


-- | Construct a pure stateful wire from the given signal function.

mkSF :: (Monoid s) => (s -> a -> (b, Wire s e m a b)) -> Wire s e m a b
mkSF f = mkPure (\ds -> lstrict . first (Right) . f ds)


-- | Construct a pure stateless wire from the given function.

mkSF_ :: (a -> b) -> Wire s e m a b
mkSF_ f = WArr (fmap f)


-- | Construct a pure stateful wire from the given signal function.

mkSFN :: (a -> (b, Wire s e m a b)) -> Wire s e m a b
mkSFN f = mkPureN (lstrict . first (Right) . f)


-- | Perform one step of the given wire.

stepWire :: (Monad m) => Wire s e m a b -> s -> Either e a -> m (Either e b, Wire s e m a b)
stepWire w@(WArr f)    _  mx' = return (f mx', w)
stepWire w@(WConst mx) _  mx' = return (mx' *> mx, w)
stepWire (WGen f)      ds mx' = f ds mx'
stepWire w@WId         _  mx' = return (mx', w)
stepWire (WPure f)     ds mx' = return (f ds mx')
