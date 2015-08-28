cabal sandbox delete 
cabal sandbox init

cabal sandbox add-source non-hackage-dependencies/ghcjs-ffiqq/
cabal sandbox add-source non-hackage-dependencies/ghcjs-jquery/
cabal sandbox add-source non-hackage-dependencies/ghcjs-canvas/
cabal sandbox add-source non-hackage-dependencies/ghcjs-vdom/
cabal sandbox add-source non-hackage-dependencies/stm-notify/
cabal sandbox add-source non-hackage-dependencies/valentine/
cabal sandbox add-source non-hackage-dependencies/valentine/non-hackage-dependencies/live-vdom



cabal install --ghcjs
cabal configure --ghcjs
