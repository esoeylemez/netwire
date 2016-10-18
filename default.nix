{ mkDerivation, base, containers, deepseq, parallel, profunctors
, random, semigroups, stdenv, time, transformers
}:
mkDerivation {
  pname = "netwire";
  version = "5.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq parallel profunctors random semigroups time
    transformers
  ];
  homepage = "https://github.com/esoeylemez/SKELETON";
  description = "Functional reactive programming library";
  license = stdenv.lib.licenses.bsd3;
}
