{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bmp, bytestring, stdenv, vector }:
      mkDerivation {
        pname = "JFA";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base vector ];
        executableHaskellDepends = [ base bmp bytestring vector ];
        jailbreak = true;
        doCheck = false;
        homepage = "https://github.com/timput/JFA";
        description = "Jump Flood Algorithm for discrete approximate Voronoi Diagrams and Euclidean Distance Fields";
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
