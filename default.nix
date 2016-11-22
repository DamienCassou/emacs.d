/*

This is a nix expression to build Emacs and some Emacs packages I like
from source on any distro where nix is installed. This will install
all the dependencies from the nixpkgs repo and build the binary files
without interfering with the host distro.

http://nixos.org/nix/

To quickly install nix, you can run the following command:

$ curl -L http://git.io/nix-install.sh | bash

To initialize it:

$ source ~/.nix-profile/etc/profile.d/nix.sh

To build the project, type the following from the current directory:

$ nix-build

To run the newly compiled executable:

$ ./result/bin/emacs
*/
{ pkgs ? import <nixpkgs> {} }:

let
  baseEmacs = pkgs.emacs25;
  myEmacs = pkgs.lib.overrideDerivation (baseEmacs.override {
    withGTK3 = false;
    withGTK2 = true;
    # Make sure imagemagick is a dependency because I regularly
    # look at pictures from Emacs
    # imagemagick = pkgs.imagemagickBig;
  }) (attrs: {
    # I don't want emacs.desktop file because I only use
    # emacsclient.
    postInstall = attrs.postInstall + ''
      rm $out/share/applications/emacs.desktop
    '';
  });
  myEmacsPackagesNg = pkgs.emacsPackagesNgGen myEmacs;
in
with myEmacsPackagesNg;
emacsWithPackages []
