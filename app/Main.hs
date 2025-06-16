module Main (main) where


import Text.Pandoc
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.DocTemplates
import Lib

mdToPres :: Text -> Template Text -> Text -> IO Text
mdToPres txt template dzcore =
    let readerOptions = def 
            { readerExtensions = pandocExtensions
            , readerStandalone = True
            }
        writerOptions = def
            { writerTemplate = Just template
            , writerReferenceLinks = True
            , writerSlideLevel = Just 1
            , writerVariables = Context (M.singleton "dzslides-core" (toVal dzcore))
            }
    in runIOorExplode $ 
        readMarkdown readerOptions txt 
            >>= writeDZSlides writerOptions


main :: IO ()
main = do 
    txt <- T.readFile "Presentation.md" 
    templateTxt <- T.readFile "template.html" 
    dzcoreTxt <- T.readFile "dz-core.html" 
    template <- either error id <$> compileTemplate "." templateTxt
    out <- mdToPres txt template dzcoreTxt
    T.writeFile "slides.html" out
    putStrLn "done"

