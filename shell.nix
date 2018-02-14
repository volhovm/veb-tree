with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc822.ghc;
   name = "veb-tree";
   buildInputs = [ ];
   LANG = "en_US.UTF-8"; # the only thing shell.nix is needed for
}

