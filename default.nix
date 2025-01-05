{ pkgs ? import <nixpkgs> {} }:

let
  lib = import <nixpkgs/lib>;
  buildNodeJs = pkgs.callPackage "${<nixpkgs>}/pkgs/development/web/nodejs/nodejs.nix" {
    python = pkgs.python3;
  };

  nodejsVersion = lib.fileContents ./.nvmrc;

  nodejs = buildNodeJs {
    enableNpm = false;
    version = nodejsVersion;
    sha256 = "sha256-HgtvLyyk+wtGRKETYxadr0t8QvAOWlPSxlqf3EY+fYg=";
  };

  NPM_CONFIG_PREFIX = toString ./npm_config_prefix;

in pkgs.mkShell {
  packages = with pkgs; [
    nodejs
    nodePackages.npm
    nodePackages.parcel
    nodePackages.yarn
    elmPackages.elm
    elmPackages.elm-review
    elmPackages.elm-format
    elmPackages.elm-test
    elmPackages.elm-upgrade
    elmPackages.elm-language-server
  ];

  inherit NPM_CONFIG_PREFIX;

  shellHook = ''
    export PATH="${NPM_CONFIG_PREFIX}/bin:$PATH"
  '';
}
