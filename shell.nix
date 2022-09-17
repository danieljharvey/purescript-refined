{ pkgs, name }:

pkgs.stable.mkShell {
  inherit name;

  nativeBuildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Native Libraries:
    { }

    ###################################################
    # Languages:
    {
      inherit (pkgs.stable) dhall nodejs-16_x;
      inherit (pkgs.stable) purescript;
    }

    ###################################################
    # Code styles:
    {
      inherit (pkgs.stable)
        purs-tidy
        nixpkgs-fmt
        nix-linter;
      inherit (pkgs.stable.nodePackages) prettier;
    }

    ###################################################
    # Command line tools:
    { inherit (pkgs.stable) gitFull git-lfs; }

    ###################################################
    # Language servers:
    {
      inherit (pkgs.unstable.nodePackages)
        purescript-language-server;
    }

    ###################################################
    # Package managers:
    {
      inherit (pkgs.stable) spago;
      inherit (pkgs.stable.nodePackages) bower;
    }
  ];
}
