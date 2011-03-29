{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax #-}
module EduGame.Utils where
import Data.Char
import Data.List (partition)
import Data.Text (Text, unpack)
import System.Environment.FindBin
import Text.InterpolatedString.Perl6

type X = Float
type Y = Float
type Threshold = Int

-- Constants
cardWidth, cardHeight, paperWidth, paperHeight :: Float
cardWidth = 180
cardHeight = 252
paperWidth = 540
paperHeight = 700
-- paperWidth = 1600
-- paperHeight = 2300

say = putStrLn
_Left_ :: X
_Left_ = 9.6
_Top_ :: Y
_Top_ = 13.65
_DarkRed_ = Color 0.6 0.3 0.3
_Brown_ = Color 0.5 0.25 0
_LightGreen_ = Color 0.9 1 0.9
_Green_ = Color 0.1 0.6 0.1
_Blue_ = Color 0.3 0.3 0.5
_GrayBlue_ = Color 0.5 0.5 0.6

data Shape = Shape
    { left            :: X
    , top             :: Y
    , width           :: X
    , height          :: Y
    , cornerRadius    :: Int
    , verticalPadding :: Int
    , fill            :: Fill
    , stroke          :: Stroke
    , shadow          :: Shadow
    , body            :: Body
    , picture         :: Picture
    } | PageBreak | RuleShape1 { left :: X, top :: Y } | RuleShape2 { left :: X, top :: Y } | FaceShape { left :: X, top :: Y, shapeColor :: Color } | SerialShape { left :: X, top :: Y, serialColor :: Color, serialNumber :: Int } | Power { left :: X, top :: Y, strength :: Strength } | Threshold { left :: X, top :: Y, threshold :: Threshold }

data Fill = FillWhite | FillLinear Color | FillRadial Color | FillRadialOut Color | FillColor Color | FillNonTopic | FillNone

instance ShowQ Fill where
    showQ FillWhite = ""
    showQ (FillLinear color) = [qq|gradient $color fill color:\{0.95, 0.95, 0.95}, fill: linear fill,|]
    showQ (FillRadial color) = [qq|fill $color gradient color:\{0.95, 0.95, 0.95}, fill: radial fill,|]
    showQ (FillRadialOut color) = [qq|gradient $color fill: radial fill,|]
    showQ (FillColor color) = [qq|fill $color|]
    showQ FillNonTopic = showQ $ FillRadialOut (Color 0.7 0.7 0.7)
    showQ FillNone = "fill: no fill,"


instance ShowQ Stroke where
    showQ StrokeNone = "draws stroke:false,"
    showQ StrokeParalyzed = "stroke color: {0.5, 0.2, 0.2}, "
    showQ StrokeDotted = "stroke color: {0.5, 0.5, 0.5}, stroke pattern: 24,"
    showQ StrokeWhite = "stroke color: {1, 1, 1},"
    showQ StrokeBlack = "stroke color: {0, 0, 0},"
    showQ StrokeBlackThick = "stroke color: {0, 0, 0}, thickness: 10,"
    showQ (StrokeThick color) = [qq|stroke $color thickness:15,|]
    showQ (StrokeDouble color) = [qq|stroke $color thickness:5, double stroke:true,|]
    showQ (StrokeSingle color) = [qq|stroke $color|]
    showQ (StrokeDoubleDotted color) = [qq|stroke $color thickness:5, double stroke:true, stroke pattern: 24, |]

instance ShowQ Shadow where
    showQ ShadowBottom = "shadow vector: {0, 1}, shadow fuzziness: 4,"
    showQ ShadowNone = "draws shadow: false, "
    showQ ShadowMiddle = "shadow vector: {0, 0}, shadow fuzziness: 4,"

data Picture = PictureRelative FilePath | PictureNone

instance ShowQ Picture where
    showQ (PictureRelative path) = [qq|image sizing: stretched, image: "$__Bin__/$path",|]
    showQ PictureNone = ""

data Color = Color { red :: Float, green :: Float, blue :: Float } deriving (Show, Eq)

instance ShowQ Color where
    showQ Color{..} = [qq|color: \{$red, $green, $blue}, |]

data Stroke = StrokeWhite | StrokeBlack | StrokeBlackThick | StrokeParalyzed | StrokeDotted | StrokeDouble Color | StrokeDoubleDotted Color | StrokeSingle Color | StrokeThick Color | StrokeNone deriving Eq
data Shadow = ShadowBottom | ShadowMiddle | ShadowNone
data Placement = PlacementTop | PlacementMiddle | PlacementBottom

instance ShowQ Placement where
    showQ PlacementTop = "text placement: top,"
    showQ PlacementBottom = "text placement: bottom,"
    showQ _ = ""

data Body   = Body
    { text  :: Text
    , color :: Color
    , font  :: Text
    , size  :: Int
    , placement :: Placement
    } | BodyNone

data Strength = MkStrength
    { strInterested :: Int
    , strUninterested :: Int
    }

instance ShowQ Shape where
    showQ shape = case shape of
        Power{..} -> [qq|
make new shape at end of graphics with properties \{draws shadow: false, corner radius: 15, size: \{28, 29}, side padding: 0, vertical padding: 0, origin: \{{left + 44}, {top + 195}}, text: \{text: "{maybeNil strInterested strength}", font: "BookmanOldStyle-Bold", size: 22, alignment: center}{maybeVoid strInterested strength}}
make new shape at end of graphics with properties \{draws shadow: false, corner radius: 15, size: \{28, 29}, side padding: 0, vertical padding: 0, origin: \{{left + 107}, {top + 195}}, text: \{text: "{maybeNil strUninterested strength}", font: "BookmanOldStyle", size: 18, alignment: center}{maybeVoid strUninterested strength}, fill: radial fill}
|]
        SerialShape{..} -> [qq|make new shape at end of graphics with properties \{textPosition: \{0, 0.25}, text placement: bottom, draws shadow: false, corner radius: 2, size: \{18, 17}, side padding: 1, flipped vertically: true, stroke {serialColor} name: "HorizontalTriangle", vertical padding: 0, origin: \{{left + 163}, {top + 232}}, fill color: \{0.5, 0.5, 0.5}, textSize: \{0.875, 0.5}, text: \{text: "{if serialNumber < 10 then " " else ""}{ serialNumber }", font: "AmericanTypewriter-Condensed", size: 8, color: \{1, 1, 1}}, gradient color: \{0.5, 0.5, 0.5}}
|]
        RuleShape1{..} -> [qq|$_begin $_origin size: \{ $cardWidth, $cardHeight } $_rule1
|]
        RuleShape2{..} -> [qq|$_begin $_origin size: \{ $cardWidth, $cardHeight } $_rule2
|]
        FaceShape{..} -> [qq|make new line at end of graphics with properties \{stroke color: \{0.5, 0.2, 0.2}, line type: bezier, thickness: 18, bezier point list: \{\{{left+27.25}, {top+176.35}}, \{{left+27.25}, {top+176.35}}, \{{left+49.25}, {top+190}}, \{{left+92.25}, {top+190}}, \{{left+135.25}, {top+190}}, \{{left+154.75}, {top+176}}, \{{left+154.75}, {top+176}}}}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{147., 19}, origin: \{{left + 17.5}, {top + 20.5}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 211.4}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 211.4}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 135}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 135}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 97}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 97}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 58.5}}, fill color: \{0.2, 0.2, 0.7}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 58.5}}, fill color: \{0.2, 0.2, 0.7}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
        |] ++ [qq|
            $_begin $_origin size: \{ $cardWidth, $cardHeight }, fill: linear fill, draws shadow: false, corner radius: 15, side padding: 0, gradient $shapeColor vertical padding: 0}
        |]
        Shape{..} -> let {
            _size   = [qq|size: \{$width, $height}, |];
            _end    = [qq|corner radius: $cornerRadius, vertical padding: $verticalPadding, side padding: 0 }|]
        } in [qq|$_begin $stroke $shadow $body $fill $picture $_origin $_size $_end
|]
        PageBreak  -> [q|
            end tell
            make new canvas
            set count_canvas to count of canvases
            set canvas_no to count_canvas
            tell canvas canvas_no
        |]
        Threshold{..} -> [qq|
make new shape at end of graphics with properties \{draws shadow: false, corner radius: 15, size: \{108, 29}, side padding: 0, vertical padding: 0, origin: \{{left + 36}, {top + 195}}, text: \{text: "$threshold", font: "BookmanOldStyle-Bold", size: 22, alignment: center}, fill: radial fill}
|]
        where
        _rule1 = [q|, fill: linear fill, gradient color: {0.9,0.9,0.9}, draws shadow: false, corner radius: 15, side padding: 15, vertical padding: 0, text: {{text: "回合：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande"}, {text: "➩", font: "ZapfDingbatsITC"}, {text: " 抽 1 張牌", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "➩  ", font: "ZapfDingbatsITC"}, {text: "2 次行動", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "➩  ", font: "ZapfDingbatsITC"}, {text: "手牌多於 4 張則棄牌

", font: "MicrosoftJhengHeiRegular"}, {text: "行動：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：抽 1 張牌", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：換任意張牌", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：出教學卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "  (需擲親和骰 3 以上)", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：出助教卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 2 動：出環境卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 2 動：出技藝卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "  (若已有技藝卡, 則需棄牌)", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• ? 動：出特殊卡", font: "MicrosoftJhengHeiRegular"}}}|]
        _rule2 = [q|, fill: linear fill, gradient color: {0.9,0.9,0.9}, draws shadow: false, corner radius: 15, side padding: 15, vertical padding: 0, text: {{text: "教學失效因素：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande"}, {text: "• 麻痺未解", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 無一風格相符", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 修正後力道 0 以下

", font: "MicrosoftJhengHeiRegular"}, {text: "力道修正因素：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande-Bold"}, {text: "• 風格障礙：每項 -2", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 風格完全覆蓋：+2

", font: "MicrosoftJhengHeiRegular"}, {text: "計分：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande-Bold"}, {text: "• 失敗：出無效教學者 -1", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 成功：最高+2 其他+1", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 特別成功：分數加倍", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 解麻痺獎分： +1", font: "MicrosoftJhengHeiRegular"}}}|]
        _begin = "make new shape at end of graphics with properties {"
        _origin = [qq|origin: \{{ left shape }, { top shape }}, |];

maybeNil :: (a -> Int) -> a -> String
maybeNil f x = case f x of
    -999 -> ""
    val  -> show val

maybeVoid :: (a -> Int) -> a -> String
maybeVoid f x = case f x of
    -999 -> ", gradient color: {0, 0, 0}, fill: radial fill"
    _    -> ""

instance ShowQ Text where
    showQ = unpack

instance ShowQ Body where
    showQ BodyNone = ""
    showQ Body{..} = [qq|text: \{ $color text: "$text", font: "$font", alignment: center $_size }, $placement |]
        where
        _size = case size of
            0 -> ""
            _ -> [qq|, size: $size|]

instance ShowQ [Shape] where
    showQ = concatMap showQ

