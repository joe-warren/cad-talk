module AddSlideDiv 
(addSlideDiv
) where

import Text.Pandoc
import Data.List (partition)
import Data.List.Split (splitOn)

isNote :: Block -> Bool
isNote (Div (_, classes, _) _) | "notes" `elem` classes = True
isNote _ = False

addSlideDiv :: Pandoc -> Pandoc
addSlideDiv (Pandoc meta blocks) = 
    let blocks' = splitOn [HorizontalRule] blocks
        contentAttr = ("", ["slide"], [])
        slideAttr = ("", ["pair"], [])
        noteAttr = ("", ["noteGroup"], [])
        makeGroup bs = 
            let (notes, regular) = partition isNote bs
            in Div slideAttr [Div contentAttr regular, Div noteAttr notes]  
        blocks'' = makeGroup <$> blocks'
        in Pandoc meta blocks''

