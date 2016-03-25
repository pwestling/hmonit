# TO BUILD

Be warned, compiling for the first time will take a lot of time and processor power. Subsequent compilations will be much much faster.

```
brew install haskell-stack
git clone git@github.com:pwestling/hmonit.git
cd hmonit
stack setup
stack build
stack exec hmonit-exe <config file>
```


# CONFIG FORMAT

    {
    "ssh" : {
        "hosts" : [
             {"host" : "om-jobs02",       "remoteport" : 12812},
             {"host" : "om-importers02",  "remoteport" : 12812},
             {"host" : "om-fppe01",       "remoteport" : 12812},
             {"host" : "om-fppe02",       "remoteport" : 12812},
             {"host" : "om-jobs",         "remoteport" : 12812},
             {"host" : "om-abs03" ,       "remoteport" : 12812}],
        "username" : "pwestling"
      }
    }

Support for non-ssh monits over normal http coming soon
