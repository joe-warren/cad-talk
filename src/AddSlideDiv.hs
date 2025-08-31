module AddSlideDiv 
(addSlideDiv
) where

import Text.Pandoc
import Text.Pandoc.Slides (prepSlides, getSlideLevel)
import Data.List (intersperse)
import Data.List.Split (splitOn)

addSlideDiv :: Pandoc -> Pandoc
addSlideDiv (Pandoc meta blocks) = 
    let sl = getSlideLevel blocks
        --blocks' = prepSlides sl blocks
        blocks' = splitOn [HorizontalRule] blocks
        contentAttr = ("", ["slide"], [])
        slideAttr = ("", ["pair"], [])
        blocks'' = ((Div slideAttr . pure . Div contentAttr) <$> blocks')
        in Pandoc meta blocks''

