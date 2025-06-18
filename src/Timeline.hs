module Timeline
( addTimeline 
) where

import Text.Pandoc
import Text.Pandoc.Walk (query , walk)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P 
import qualified Text.Megaparsec.Char.Lexer as PL
import Data.Foldable (toList)
import Data.Void (Void)
import Control.Monad (forM)
import Control.Applicative (empty)
import qualified Graphics.Svg as Svg 
import Codec.Picture (PixelRGBA8 (..))
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import qualified Data.Map as M 
import Data.List (inits)
import Linear (V2 (..))
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )

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

matchBlock :: Block -> Maybe TimelineElement
matchBlock (Header 1 _attr ((Str num):Space:(Str ":"):tokens)) =
    case parseInt num of
        Nothing -> Nothing
        Just n -> pure $ TimelineElement n (foldText tokens) (getImageUrl tokens)
matchBlock _ = Nothing

filterImages :: [Inline] -> [Inline]
filterImages = 
    let go (Image _ _ (link, _)) = []
        go x = pure x
    in query go

doBlock :: Block -> [Block]
doBlock block@(Header l attrs tokens)  = case matchBlock block of 
    Just e -> 
        [ Header l attrs (filterImages tokens)
        -- , RawBlock (Format "html") (svgToText $ timelineSvg bounds (Just e) (e : takeWhile (/= e) elems))
        , RawBlock (Format "html") 
            ("<embed type=\"image/svg+xml\" class=\"timeline horizontally-centered\" src=\"generated/timeline-" <> T.pack (show (year e)) <> ".svg\">")
        -- , Plain [Image nullAttr [] ("generated/timeline-" <> T.pack (show (year e)) <> ".svg", text e)]
        ]   
    Nothing -> pure block
doBlock block = pure block

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

timelineSvg :: (Int, Int) -> Maybe TimelineElement -> [TimelineElement] -> Svg.Document
timelineSvg (lo, hi) highlightElement elements =
    let w = Svg.Px 600
        h = Svg.Px 400
        border = 60
        r = 8
        attrs = Svg.defaultSvg 
            & Svg.strokeWidth .~ pure (Svg.Px 2)
            & Svg.strokeColor .~ pure (Svg.ColorRef (PixelRGBA8 128 0 0 0))
            & Svg.fillColor .~ pure (Svg.FillNone)

        textAttrs = Svg.defaultSvg 
            & Svg.fillColor .~ pure (Svg.ColorRef (PixelRGBA8 0 0 0 0))
            & Svg.fontFamily .~ pure ["Roboto", "sans-serif"]
            & Svg.fontSize .~ pure (Svg.Px 7)

        xPosition e = 
            let frac = fromIntegral (year e - lo) / fromIntegral (hi - lo)
             in border + (600 - border * 2) * frac

        line = Svg.LineTree $ Svg.Line attrs (Svg.Px border, Svg.Px 350)  (Svg.Px (600-border), Svg.Px 350)

        highlightElements e = 
            let pX = xPosition e
                center = (Svg.Px 300 , Svg.Px 130)
                circleAttrs = attrs 
                    & Svg.fillColor .~ (pure $ Svg.TextureRef "largePat")
                circle = Svg.CircleTree $ Svg.Circle circleAttrs center (Svg.Px 125)
                
                connector = Svg.PolyLineTree $ Svg.PolyLine attrs
                                [ V2 pX 350
                                , V2 pX 300
                                , V2 300 130
                                ]
            
            in [connector, circle]

        timelineElements e = 
            let pX = xPosition e                
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

        tree = [line] <> (highlightElements =<< toList highlightElement) <> (timelineElements =<< elements)

        mkPattern name diam e =
            let image = Svg.defaultSvg 
                    & Svg.imageWidth .~ diam
                    & Svg.imageHeight .~ diam
                    & Svg.imageHref .~ ("../" <> T.unpack (imageUrl e))
                pat = Svg.defaultSvg
                    & Svg.patternWidth .~ (Svg.Num 1)
                    & Svg.patternHeight .~ (Svg.Num 1)
                    & Svg.patternElements .~ [Svg.ImageTree image]
                element = Svg.ElementPattern pat
            in (name, element)

        timelinePatterns e = mkPattern (show . year $ e) (Svg.Px 16) e
        defs = M.fromList (timelinePatterns <$> elements)
                <> M.fromList (mkPattern "largePat" (Svg.Px 250) <$> toList highlightElement)
     in Svg.Document Nothing (Just w) (Just h) tree defs mempty mempty mempty

svgToText :: Svg.Document -> Text
svgToText = T.pack . ppcTopElement prettyConfigPP . Svg.xmlOfDocument

addTimeline :: Pandoc -> IO Pandoc
addTimeline doc = do
    let elements = query collectTimelineElements doc
    let bounds = timelineBounds elements
    forM (tail $ inits elements) $ \l -> 
        let e = last l
            path = "generated/timeline-" <> show (year e) <> ".svg"
        in Svg.saveXmlFile path (timelineSvg bounds (Just e) l)

    Svg.saveXmlFile "generated/timeline.svg" (timelineSvg bounds Nothing elements)
    pure $ walk (doBlock =<<) doc