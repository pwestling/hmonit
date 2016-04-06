# TO BUILD AND RUN

Be warned, compiling for the first time will take a lot of time and processor power. Subsequent compilations will be much much faster.

```
brew install haskell-stack
git clone git@github.com:pwestling/hmonit.git
cd hmonit
stack setup
stack build
stack exec hmonit-exe <host-name> <port> <config file>
```

Then open `<hostname>:<port>` in your browser


# CONFIG FORMAT

    {
    "ssh" : {
        "hosts" : [
             {"host" : "service-host02",    "remoteport" : 12812},
             {"host" : "executor-host02",   "remoteport" : 12812},
             {"host" : "service-host01",    "remoteport" : 12812},
             {"host" : "executor-host02",   "remoteport" : 12812},
             {"host" : "misc-host",         "remoteport" : 12812},
             {"host" : "misc-host03" ,      "remoteport" : 12812}],
        "username" : "pwestling"
      }
    }

Support for non-ssh monits over normal http coming soon
