{ mkDerivation, base, containers, deepseq, parallel, profunctors
, random, semigroups, stdenv, time, transformers
}:
mkDerivation {
  pname = "netwire";
  version = "5.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq parallel profunctors random semigroups time
    transformers
  ];
  homepage = "https://github.com/esoeylemez/netwire";
  description = "Functional reactive programming library";
  license = stdenv.lib.licenses.bsd3;
}
