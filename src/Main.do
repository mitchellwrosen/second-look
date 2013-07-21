GHC_FLAGS="-v0 -Wall -hidir obj -odir obj"
DEPS="SecondLook.hs GithubPayload.hs Email.hs Data/Text/Encoding/Extras.hs"
TARGET="Main.hs"

redo-ifchange $TARGET $DEPS

ghc $GHC_FLAGS -o $3 $TARGET
