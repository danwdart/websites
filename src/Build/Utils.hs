{-# LANGUAGE UnicodeSyntax #-}
module Build.Utils (mkdirp, make) where

import qualified Data.ByteString.Lazy.Char8    as BSL
import           System.Directory
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5              as H hiding (main)

mkdirp ∷ String → IO ()
mkdirp = createDirectoryIfMissing True

make ∷ String → Html → IO ()
make name page = do
    copyDir "static/common" $ ".sites/" ++ name
    copyDir ("static/" ++ name) (".sites/" ++ name)
    BSL.writeFile (".sites/" ++ name ++ "/index.html") $ renderHtml page
    putStrLn $ name ++ " compiled."
