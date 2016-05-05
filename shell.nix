{
  nixpkgs ?
    let
      inherit (import <nixpkgs> {}) fetchFromGitHub;
    in
      import
        (fetchFromGitHub {
          owner = "NixOS";
          repo = "nixpkgs";
          sha256 = "0camjjd9mnanmqfll08f9whzxxvvw8md0rwlq65xyvsd0cg7r3li";
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
        src = pkgs.fetchFromGitHub {
          owner = "ocharles";
          repo = "product-profunctors";
          sha256 = "0d7p7j60z1ysr7nfrpzkhcwfkgibzqzdna5v2rv6y5p2w528dhay";
          rev = "5d9a8bd7ffcebab4083cc30d2b185a22d16d3dea";
        };
      });
      opaleye = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.overrideCabal super.opaleye (drv: {
        src = pkgs.fetchFromGitHub {
          owner = "ocharles";
          repo = "haskell-opaleye";
          sha256 = "1hmynf24f55s56sb536jvwr0ldczplic9way31r4rfc1wqxqazbm";
          rev = "31474c58e0814520835437862240afd55c868bb9";
        };
      }));
    };
  };

  haskellPackages = pkgs.haskell.packages.${compiler}.override overrides;

  drv = haskellPackages.callPackage ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv
