# TO BUILD

brew install haskell-stack
git clone git@github.com:pwestling/hmonit.git
cd hmonit
stack setup
stack build
stack exec hmonit-exe <config file>


# CONFIG FORMAT

    {
    "ssh" : {
        "hosts" : [
             {"host" : "om-jobs02", "localport" : 20345, "remoteport" : 12812},
             {"host" : "om-importers02", "localport" : 20347, "remoteport" : 12812},
             {"host" : "om-fppe01", "localport" : 20348, "remoteport" : 12812},
             {"host" : "om-fppe02", "localport" : 20349, "remoteport" : 12812},
             {"host" : "om-jobs",   "localport" : 20350, "remoteport" : 12812},
             {"host" : "om-abs03" , "localport" : 20351, "remoteport" : 12812}],
        "username" : "pwestling"
      }
    }

Support for non-ssh monits over normal http coming soon
