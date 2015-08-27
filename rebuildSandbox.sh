cabal sandbox delete 
cabal sandbox init

cabal sandbox add-source non-hackage-dependencies/ghcjs-ffiqq/
cabal sandbox add-source non-hackage-dependencies/ghcjs-jquery/
cabal sandbox add-source non-hackage-dependencies/ghcjs-canvas/
cabal sandbox add-source non-hackage-dependencies/ghcjs-vdom/
cabal sandbox add-source non-hackage-dependencies/shakespeare-dynamic/ghcjs-shakespeare-dynamic/
cabal sandbox add-source non-hackage-dependencies/shakespeare-dynamic/vdom-adapter/
cabal sandbox add-source non-hackage-dependencies/stm-notify/

cabal install --ghcjs
cabal configure --ghcjs