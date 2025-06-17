module Main (main) where


import Text.Pandoc
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.DocTemplates
import Timeline (addTimeline)

parseInput :: Text -> IO Pandoc
parseInput txt =
    let readerOptions = def 
            { readerExtensions = pandocExtensions
            , readerStandalone = True
            }
    in runIOorExplode $ 
        readMarkdown readerOptions txt 

printSlides :: Pandoc -> Template Text -> Text -> IO Text
printSlides doc template dzcore = 
    let
        writerOptions = def
            { writerTemplate = Just template
            , writerReferenceLinks = True
            , writerSlideLevel = Just 1
            , writerVariables = Context (M.singleton "dzslides-core" (toVal dzcore))
            }
    in runIOorExplode $ 
            writeDZSlides writerOptions doc

main :: IO ()
main = do 
    doc <- addTimeline =<< parseInput =<< T.readFile "Presentation.md"
    templateTxt <- T.readFile "template.html" 
    dzcoreTxt <- T.readFile "dz-core.html" 
    template <- either error id <$> compileTemplate "." templateTxt
    T.writeFile "slides.html" =<< printSlides doc template dzcoreTxt
    putStrLn "done"

