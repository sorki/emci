let emci = ./emci.dhall

let mkRepo = λ(name : Text) → λ(loc : Text) → emci.mkRepo name loc

in  emci.mkProj
      "emci-meta"
      [ emci.mkRepo "emci" "https://github.com/sorki/emci"
      , emci.mkRepo
          "git-post-receive"
          "https://github.com/sorki/git-post-receive"
      ]
