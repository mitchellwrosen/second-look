GHC_FLAGS="-v0 -Wall -hidir obj -odir obj"

redo-ifchange SecondLook.hs GithubPayload.hs Email.hs Text/Extras.hs

ghc $GHC_FLAGS -o $3 SecondLook.hs
