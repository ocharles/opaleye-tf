{
  nixpkgs ?
    let
      inherit (import <nixpkgs> {}) fetchgit;
    in
      import
        (fetchgit {
           url = "git://github.com/NixOS/nixpkgs.git";
           sha256 = "ccf2d010ad246e1bb1a7c8303174d9bac631981874c560b451468a841d0e24c4";
           rev = "7475728593a49f09c4b7b959b15513aee38ab4b4";
         }) {}
  , compiler ? "ghc801"
}:

let

  inherit (nixpkgs) pkgs;

  overrides = {
    overrides = self: super: {
      transformers-compat = pkgs.haskell.lib.doJailbreak super.transformers-compat_0_5_1_4;
      aeson = super.aeson_0_11_1_1;
      fail = pkgs.haskell.lib.dontHaddock super.fail;
      distributive = pkgs.haskell.lib.dontCheck super.distributive;
      comonad = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck super.comonad);
      bifunctors = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak super.bifunctors);
      product-profunctors = pkgs.haskell.lib.overrideCabal super.product-profunctors (drv: {
        src = pkgs.fetchgit {
          url = git://github.com/ocharles/product-profunctors;
          sha256 = "5ec18644e1e2166f7616bb28db3efe2bbee93883f3dfececc9da870f8c3cf734";
          rev = "5d9a8bd7ffcebab4083cc30d2b185a22d16d3dea";
        };
      });
      opaleye = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.overrideCabal super.opaleye (drv: {
        src = pkgs.fetchgit {
          url = git://github.com/ocharles/haskell-opaleye;
          sha256 = "e0150d106966c9bc34a2051d693f785acd549eb9f18bde2fcc076615ff3b55d8";
          rev = "0234a162b242254ff0c44b642654a250087d6195";
        };
      }));
    };
  };

  haskellPackages = pkgs.haskell.packages.${compiler}.override overrides;

  drv = haskellPackages.callPackage ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv
