let Flag
    : Type
    = < CI | Deployment | Generated >

let Repo
    : Type
    = { repoName : Text
      , repoLocation : Text
      , repoBranch : Optional Text
      , repoFlags : List Flag
      }

let defaultProjConf =
      { cfgBackupsPath = "~/ci/backup"
      , cfgMirrorsPath = "~/ci/mirror"
      , cfgUpstreamsPath = "~/ci/upstream"
      , cfgSnapshotsPath = "~/ci/snapshot"
      , cfgCheckoutsPath = "~/git/"
      }

let mkProjConfAtPrefix =
      λ(prefix : Text) →
        { cfgBackupsPath   = prefix ++ "/backup"
        , cfgMirrorsPath   = prefix ++ "/mirror"
        , cfgUpstreamsPath = prefix ++ "/upstream"
        , cfgSnapshotsPath = prefix ++ "/snapshot"
        , cfgCheckoutsPath = prefix ++ "/checkouts"
        }

let mkProj =
      λ(name : Text) →
      λ(repos : List Repo) →
        { projName = name, projConf = defaultProjConf, projRepos = repos }

let mkRepo =
      λ(name : Text) →
      λ(loc : Text) →
        { repoName = name
        , repoLocation = loc
        , repoBranch = None Text
        , repoFlags = [] : List Flag
        }

in  { Flag, Repo, defaultProjConf, mkProjConfAtPrefix, mkProj, mkRepo }
