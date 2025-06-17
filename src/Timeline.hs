module Timeline
( addTimeline 
) where

import Text.Pandoc
import Text.Pandoc.Walk (query , walkM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P 
import qualified Text.Megaparsec.Char.Lexer as PL
import Data.Foldable (toList)
import Data.Void (Void)
import Control.Applicative (empty)
import qualified Graphics.Svg as Svg 
import Codec.Picture (PixelRGBA8 (..))
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import qualified Data.Map as M 

type Parser = P.Parsec Void Text

decimal :: Parser Int
decimal = PL.decimal
parseInt :: Text -> Maybe Int
parseInt = P.parseMaybe decimal

data TimelineElement = TimelineElement
    { year :: Int
    , text :: Text
    , imageUrl :: Text
    } deriving (Eq, Ord, Show)

doBlock :: Block -> IO Block
doBlock (Header 1 attr inlines) = pure $ Header 1 attr inlines
doBlock x = pure x

foldText :: [Inline] -> Text
foldText = 
    let go (Str t) = t
        go (Space) = " "
        go _ = ""
    in T.strip . query go

getImageUrl :: [Inline] -> Text
getImageUrl = 
    let go (Image _ _ (link, _)) = First . Just $ link
        go _ = mempty
    in fromMaybe "assets/images/fallback.png" . getFirst . query go

collectTimelineElements :: Block -> [TimelineElement]
collectTimelineElements (Header 1 _attr ((Str num):Space:(Str ":"):tokens)) =
    case parseInt num of
        Nothing -> []
        Just n -> pure $ TimelineElement n (foldText tokens) (getImageUrl tokens)
collectTimelineElements _ = []

timelineBounds :: [TimelineElement] -> (Int, Int)
timelineBounds elements = (minimum $ year <$> elements, maximum $ year <$> elements)

timelineSvg :: [TimelineElement] -> Svg.Document
timelineSvg elements =
    let (lo, hi) = timelineBounds elements
        w = Svg.Px 600
        h = Svg.Px 400
        border = 60
        r = 8
        attrs = Svg.defaultSvg 
            & Svg.strokeWidth .~ pure (Svg.Px 2)
            & Svg.strokeColor .~ pure (Svg.ColorRef (PixelRGBA8 128 0 0 0))

        textAttrs = Svg.defaultSvg 
            & Svg.fillColor .~ pure (Svg.ColorRef (PixelRGBA8 0 0 0 0))
            & Svg.fontFamily .~ pure ["Roboto", "sans-serif"]
            & Svg.fontSize .~ pure (Svg.Px 7)

        line = Svg.LineTree $ Svg.Line attrs (Svg.Px border, Svg.Px 350)  (Svg.Px (600-border), Svg.Px 350)
        timelineElements e = 
            let frac = fromIntegral (year e - lo) / fromIntegral (hi - lo)
                pX = (border + (600 - border * 2) * frac)
                p = (Svg.Px pX , Svg.Px 350)

                circleAttrs = attrs 
                    & Svg.fillColor .~ (pure $ Svg.TextureRef (show (year e)))
                circle = Svg.CircleTree $ Svg.Circle circleAttrs p (Svg.Px r)
                dateText = Svg.TextTree Nothing $ 
                    Svg.Text Svg.TextAdjustSpacing (
                        Svg.defaultSvg 
                            & Svg.spanInfo . Svg.textInfoX .~ [Svg.Px pX]
                            & Svg.spanInfo . Svg.textInfoY .~ [Svg.Px 375]
                            & Svg.spanContent .~ [Svg.SpanText . T.pack . show . year $ e]
                    ) 
                        & Svg.drawAttr .~ textAttrs
                        & Svg.drawAttr . Svg.textAnchor .~ pure Svg.TextAnchorMiddle
                labelText = Svg.TextTree Nothing $ 
                    Svg.Text Svg.TextAdjustSpacing (
                        Svg.defaultSvg 
                            & Svg.spanContent .~ [Svg.SpanText . text $ e]
                    ) 
                        & Svg.drawAttr .~ textAttrs
                        & Svg.drawAttr . Svg.textAnchor .~ pure Svg.TextAnchorStart
                        & Svg.drawAttr . Svg.transform .~ pure 
                            [ Svg.Translate (pX) 350
                            , Svg.Rotate (-45) Nothing
                            , Svg.Translate (r + 3) (1.5)
                            ]
            in [circle, dateText, labelText]

        

        tree = [line] <> (timelineElements =<< elements)

        timelinePatterns e = 
            let name = show . year $ e
                diam = Svg.Px 16
                image = Svg.defaultSvg 
                    & Svg.imageWidth .~ diam
                    & Svg.imageHeight .~ diam
                    & Svg.imageHref .~ ("../" <> T.unpack (imageUrl e))
                pat = Svg.defaultSvg
                    & Svg.patternWidth .~ diam
                    & Svg.patternHeight .~ diam
                    & Svg.patternElements .~ [Svg.ImageTree image]
                element = Svg.ElementPattern pat

            in (name, element)
        defs = M.fromList (timelinePatterns <$> elements)
     in Svg.Document Nothing (Just w) (Just h) tree defs mempty mempty mempty

addTimeline :: Pandoc -> IO Pandoc
addTimeline doc = do
    let elements = query collectTimelineElements doc
    print elements
    Svg.saveXmlFile "generated/timeline.svg" (timelineSvg elements)
    walkM doBlock doc