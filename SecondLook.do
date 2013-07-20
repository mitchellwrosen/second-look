GHC_FLAGS="-v0 -Wall -Werror -hidir obj -odir obj"

redo-ifchange SecondLook.hs

ghc $GHC_FLAGS -o $3 SecondLook.hs
