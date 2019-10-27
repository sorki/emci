let emci = ./emci.dhall

let emciMeta = ./meta.dhall

in  emciMeta ⫽ { projConf = emci.mkProjConfAtPrefix "/tmp/emci" }
