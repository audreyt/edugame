{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP #-}
import Data.Char
import Data.List (partition)
import System.Environment.FindBin
import qualified System.IO.UTF8 as UTF8
import Text.InterpolatedString.Perl6

rules = [ RuleCard1, RuleCard2 ]
_Cards_ = students -- concat [ topicCards, rules, students, lessons, actions, skills, environments ]

-- 「學習風格」：在牌的四角，有VARK四種：V代表視覺型、A代表聽覺型、R代表閱讀型、K代表操作型。
data Style = V | A | R | K | Anti Style deriving Show

-- 「學門」：遊戲有「數學」、「中文」、「英文」、「自然」、「社會」、「藝術」、「體育」七個學門。
data Topic = Mat | Chi | Eng | Nat | Soc | Art | Phy deriving (Eq, Show, Enum, Bounded)

topicName Mat = "數學"
topicName Chi = "中文"
topicName Eng = "英文"
topicName Nat = "自然"
topicName Soc = "社會"
topicName Art = "藝術"
topicName Phy = "體育"

---- 學門簡寫
c,e,m,n,s,a,p :: Topic
c = Chi; e = Eng; m = Mat; n = Nat; s = Soc; a = Art; p = Phy

---- 不限學門
anything :: [Topic]
anything = []

-- 「特殊」：有些牌有特殊能力，如解麻痺、引發興趣、觀察學生等。
data Ability = Unparalyze | Inspire | Look deriving Show

---- 特殊能力簡寫
u, i, l :: Ability
u = Unparalyze ; i = Inspire ; l = Look

data Card
    = Student -- 學生
        { serial            :: Int          -- 序號
        , name              :: String       -- 名稱
        , styles            :: [Style]      -- 學習風格
        , interested        :: Int          -- 蒙昧值(有興趣時)
        , uninterested      :: Int          -- 蒙昧值(無興趣時)
        , topics            :: [Topic]      -- 有興趣之學門
        , paralyzed         :: [Topic]      -- 有麻痺之學門
        , flavor            :: String       -- 斜體字
        }
    | Lesson -- 教學
        { serial            :: Int          -- 序號
        , name              :: String       -- 名稱
        , styles            :: [Style]      -- 學習風格
        , interested        :: Int          -- 成就點數(有興趣時)
        , uninterested      :: Int          -- 成就點數(無興趣時)
        , topics            :: [Topic]      -- 學門
        , abilities         :: [Ability]    -- 特殊能力
        , flavor            :: String       -- 斜體字
        }
    | Action -- 行動
        { serial            :: Int          -- 序號
        , name              :: String       -- 名稱
        , turns             :: Int          -- 所需回合
        , effect            :: String       -- 效果
        , flavor            :: String       -- 斜體字
        }
    | Skill -- 技藝
        { serial            :: Int          -- 序號
        , name              :: String       -- 名稱
        , effect            :: String       -- 效果
        , flavor            :: String       -- 斜體字
        }
    | Environment -- 環境
        { serial            :: Int          -- 序號
        , name              :: String       -- 名稱
        , effect            :: String       -- 效果
        , flavor            :: String       -- 斜體字
        }
    | Assistant -- 助教
        { serial            :: Int          -- 序號
        , name              :: String       -- 名稱
        , styles            :: [Style]      -- 額外風格
        , topics            :: [Topic]      -- 解麻痺學科
        , abilities         :: [Ability]    -- 特殊能力
        , cost              :: Int          -- 啟動所需力道
        , flavor            :: String       -- 斜體字
        }
    | TopicCard { topic :: Topic }
    | RuleCard1
    | RuleCard2
    | FaceCard { cardColor :: Color }
    | EmptyStudent
    deriving Show

--------------------------

say = UTF8.putStrLn

instance ShowQ [Shape] where
    showQ = concatMap showQ

_Left_ = 9.6
_Top_ = 13.65

main = do
    say [$qq|

tell application "OmniGraffle Professional 5"
    tell document of front window
        set count_canvas to count of canvases
        set canvas_no to count_canvas
        tell canvas canvas_no
{ renderCards _Left_ _Top_ _Cards_ }
        end tell
    end tell
end tell
    |]

-- { renderCards _Left_ _Top_ (concat $ replicate 3 (students ++ [EmptyStudent])) }
-- { renderCards _Left_ _Top_ sheet1Cards }
faceColors :: [Color]
faceColors =
    [ Color 1 1 0.75
    , Color 1 0.75 1
    , Color 1 1 1
    , Color 1 0.75 0.75
    , Color 0.75 0.75 1
    , Color 0.75 1 0.75
    ]

sheet1Cards = topicCards ++ playerCards
topicCards = concatMap (replicate 4 . TopicCard) [minBound..maxBound] 
playerCards = concat [ [RuleCard1, RuleCard2, FaceCard c] | c <- faceColors ]

renderCards :: X -> Y -> [Card] -> [Shape]
renderCards _  _  []     = []
renderCards xo yo (c:cs) = map adjustOffset (renderCard c) ++ maybePageBreak ++ renderCards xo' yo' cs
    where
    adjustOffset s = s{ left = left s + xo, top = top s + yo }
    (xo', yo', maybePageBreak)
        | moveRight <- xo + cardWidth
        , moveRight < paperWidth
        = (moveRight, yo, [])
        | moveDown <- yo + cardHeight
        , moveDown < paperHeight
        = (_Left_, moveDown, [])
        | otherwise
        = (_Left_, _Top_, [PageBreak])

-- Constants
cardWidth, cardHeight, paperWidth :: Float
cardWidth = 180
cardHeight = 252
-- paperWidth = 1600
-- paperHeight = 2300
paperWidth = 540
paperHeight = 700

type X = Float
type Y = Float

data Stroke = StrokeWhite | StrokeBlack | StrokeBlackThick | StrokeParalyzed | StrokeDotted | StrokeDouble Color | StrokeDoubleDotted Color | StrokeSingle Color | StrokeThick Color | StrokeNone deriving Eq
data Shadow = ShadowBottom | ShadowMiddle | ShadowNone
data Placement = PlacementTop | PlacementMiddle | PlacementBottom

instance ShowQ Placement where
    showQ PlacementTop = "text placement: top,"
    showQ PlacementBottom = "text placement: bottom,"
    showQ _ = ""

data Text   = Text
    { txt   :: String
    , color :: Color
    , font  :: String
    , size  :: Int
    , placement :: Placement
    } | TextNone

mkText = Text
    { txt       = ""
    , color     = Color 0 0 0
    , font      = "ArialUnicodeMS"
    , size      = 0
    , placement = PlacementMiddle
    }

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
    , text            :: Text
    , picture         :: Picture
    } | PageBreak | RuleShape1 { left :: X, top :: Y } | RuleShape2 { left :: X, top :: Y } | FaceShape { left :: X, top :: Y, shapeColor :: Color } | SerialShape { left :: X, top :: Y, serialColor :: Color, serialNumber :: Int } | Power { left :: X, top :: Y, strength :: Strength }
    
data Strength = MkStrength
    { strInterested :: Int
    , strUninterested :: Int
    }

maybeNil :: (a -> Int) -> a -> String
maybeNil f x = case f x of
    -999 -> ""
    val  -> show val

maybeVoid :: (a -> Int) -> a -> String
maybeVoid f x = case f x of
    -999 -> ", gradient color: {0, 0, 0}, fill: radial fill"
    _    -> ""

instance ShowQ Shape where
    showQ shape = case shape of
        Power{..} -> [$qq|
make new shape at end of graphics with properties \{draws shadow: false, corner radius: 15, size: \{28, 29}, side padding: 0, vertical padding: 0, origin: \{{left + 44}, {top + 195}}, text: \{text: "{maybeNil strInterested strength}", font: "BookmanOldStyle-Bold", size: 22, alignment: center}{maybeVoid strInterested strength}}
make new shape at end of graphics with properties \{draws shadow: false, corner radius: 15, size: \{28, 29}, side padding: 0, vertical padding: 0, origin: \{{left + 107}, {top + 195}}, text: \{text: "{maybeNil strUninterested strength}", font: "BookmanOldStyle", size: 18, alignment: center}{maybeVoid strUninterested strength}, fill: radial fill}
|]
        SerialShape{..} -> [$qq|make new shape at end of graphics with properties \{textPosition: \{0, 0.25}, text placement: bottom, draws shadow: false, corner radius: 2, size: \{21, 17}, side padding: 1, flipped vertically: true, stroke {serialColor} name: "HorizontalTriangle", vertical padding: 0, origin: \{{left + 160}, {top + 232}}, fill color: \{0, 0, 0}, textSize: \{0.875, 0.5}, text: \{text: "{if serialNumber < 10 then " " else ""}{ serialNumber }", font: "AmericanTypewriter-Condensed", size: 9, color: \{1, 1, 1}}, gradient color: \{0.25, 0.25, 0.25}}
|]
        RuleShape1{..} -> [$qq|$_begin $_origin size: \{ $cardWidth, $cardHeight } $_rule1
|]
        RuleShape2{..} -> [$qq|$_begin $_origin size: \{ $cardWidth, $cardHeight } $_rule2
|]
        FaceShape{..} -> [$qq|make new line at end of graphics with properties \{stroke color: \{0.5, 0.2, 0.2}, line type: bezier, thickness: 18, bezier point list: \{\{{left+27.25}, {top+176.35}}, \{{left+27.25}, {top+176.35}}, \{{left+49.25}, {top+190}}, \{{left+92.25}, {top+190}}, \{{left+135.25}, {top+190}}, \{{left+154.75}, {top+176}}, \{{left+154.75}, {top+176}}}}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{147., 19}, origin: \{{left + 17.5}, {top + 20.5}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 211.4}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 211.4}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 135}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 135}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 97}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 97}}, fill color: \{0.5, 0.5, 0.5}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 105.7}, {top + 58.5}}, fill color: \{0.2, 0.2, 0.7}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
            make new shape at end of graphics with properties \{draws shadow: false, corner radius: 5, size: \{58.8, 19}, origin: \{{left + 17.5}, {top + 58.5}}, fill color: \{0.2, 0.2, 0.7}, gradient color: \{0.5, 0.2, 0.2}, draws stroke: false}
        |] ++ [$qq|
            $_begin $_origin size: \{ $cardWidth, $cardHeight }, fill: linear fill, draws shadow: false, corner radius: 15, side padding: 0, gradient $shapeColor vertical padding: 0}
        |]
        Shape{..} -> let {
            _size   = [$qq|size: \{$width, $height}, |];
            _end    = [$qq|corner radius: $cornerRadius, vertical padding: $verticalPadding, side padding: 0 }|]
        } in [$qq|$_begin $stroke $shadow $text $fill $picture $_origin $_size $_end
|]
        PageBreak  -> [$q|
            end tell
            make new canvas
            set count_canvas to count of canvases
            set canvas_no to count_canvas
            tell canvas canvas_no
        |]
        where
        _rule1 = [$q|, fill: linear fill, gradient color: {0.9,0.9,0.9}, draws shadow: false, corner radius: 15, side padding: 15, vertical padding: 0, text: {{text: "回合：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande"}, {text: "➩", font: "ZapfDingbatsITC"}, {text: " 抽 1 張牌", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "➩  ", font: "ZapfDingbatsITC"}, {text: "2 次行動", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "➩  ", font: "ZapfDingbatsITC"}, {text: "手牌多於 4 張則棄牌

", font: "MicrosoftJhengHeiRegular"}, {text: "行動：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：抽 1 張牌", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：換任意張牌", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：出教學卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "  (需擲親和骰 3 以上)", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 1 動：出助教卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 2 動：出環境卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 2 動：出技藝卡", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "  (若已有技藝卡, 則需棄牌)", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• ? 動：出特殊卡", font: "MicrosoftJhengHeiRegular"}}}|]
        _rule2 = [$q|, fill: linear fill, gradient color: {0.9,0.9,0.9}, draws shadow: false, corner radius: 15, side padding: 15, vertical padding: 0, text: {{text: "教學失效因素：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande"}, {text: "• 麻痺未解", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 無一風格相符", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 修正後力道 0 以下

", font: "MicrosoftJhengHeiRegular"}, {text: "力道修正因素：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande-Bold"}, {text: "• 風格障礙：每項 -2", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 風格完全覆蓋：+2

", font: "MicrosoftJhengHeiRegular"}, {text: "計分：", font: "MicrosoftJhengHeiBold"}, {text: " ", font: "LucidaGrande-Bold"}, {text: "• 失敗：出無效教學者 -1", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 成功：最高+2 其他+1", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 特別成功：分數加倍", font: "MicrosoftJhengHeiRegular"}, {text: " ", font: "LucidaGrande"}, {text: "• 解麻痺獎分： +1", font: "MicrosoftJhengHeiRegular"}}}|]
        _begin = "make new shape at end of graphics with properties {"
        _origin = [$qq|origin: \{{ left shape }, { top shape }}, |];

data Fill = FillWhite | FillLinear Color | FillRadial Color | FillRadialOut Color | FillColor Color | FillNonTopic | FillNone

instance ShowQ Fill where
    showQ FillWhite = ""
    showQ (FillLinear color) = [$qq|gradient $color fill color:\{0.95, 0.95, 0.95}, fill: linear fill,|]
    showQ (FillRadial color) = [$qq|fill $color gradient color:\{0.95, 0.95, 0.95}, fill: radial fill,|]
    showQ (FillRadialOut color) = [$qq|gradient $color fill: radial fill,|]
    showQ (FillColor color) = [$qq|fill $color|]
    showQ FillNonTopic = showQ $ FillRadialOut (Color 0.7 0.7 0.7)
    showQ FillNone = "fill: no fill,"


instance ShowQ Stroke where
    showQ StrokeNone = "draws stroke:false,"
    showQ StrokeParalyzed = "stroke color: {0.5, 0.2, 0.2}, "
    showQ StrokeDotted = "stroke color: {0.5, 0.5, 0.5}, stroke pattern: 24,"
    showQ StrokeWhite = "stroke color: {1, 1, 1},"
    showQ StrokeBlack = "stroke color: {0, 0, 0},"
    showQ StrokeBlackThick = "stroke color: {0, 0, 0}, thickness: 10,"
    showQ (StrokeThick color) = [$qq|stroke $color thickness:15,|]
    showQ (StrokeDouble color) = [$qq|stroke $color thickness:5, double stroke:true,|]
    showQ (StrokeSingle color) = [$qq|stroke $color|]
    showQ (StrokeDoubleDotted color) = [$qq|stroke $color thickness:5, double stroke:true, stroke pattern: 24, |]

instance ShowQ Shadow where
    showQ ShadowBottom = "shadow vector: {0, 1}, shadow fuzziness: 4,"
    showQ ShadowNone = "draws shadow: false, "
    showQ ShadowMiddle = "shadow vector: {0, 0}, shadow fuzziness: 4,"

instance ShowQ Text where
    showQ TextNone = ""
    showQ Text{..} = [$qq|text: \{ $color text: "$txt", font: "$font", alignment: center $_size }, $placement |]
        where
        _size = case size of
            0 -> ""
            _ -> [$qq|, size: $size|]

data Picture = PictureRelative FilePath | PictureNone

instance ShowQ Picture where
    showQ (PictureRelative path) = [$qq|image sizing: stretched, image: "$__Bin__/$path",|]
    showQ PictureNone = ""

data Color = Color { red :: Float, green :: Float, blue :: Float } deriving (Show, Eq)

instance ShowQ Color where
    showQ Color{..} = [$qq|color: \{$red, $green, $blue}, |]

mkIconText ch r g b f = mkText
    { txt   = [ch]
    , color = Color r g b
    , font  = f
    , size  = 14
    }

styleIcon :: Style -> Shape
styleIcon V = mkShape
    { width   = 23
    , height  = 14
    , left    = 5
    , top     = 10
    , picture = PictureRelative "images/v.png"
    }
styleIcon A = mkShape
    { width   = 23
    , height  = 33
    , left    = 5
    , top     = 195
    , picture = PictureRelative "images/a.png"
    }
styleIcon R = mkShape
    { width   = 23
    , height  = 17
    , left    = 152
    , top     = 10
    , picture = PictureRelative "images/r.png"
    }
styleIcon K = mkShape
    { width   = 23
    , height  = 33
    , left    = 152
    , top     = 195
    , picture = PictureRelative "images/k.png"
    }
styleIcon (Anti (Anti x)) = styleIcon x
styleIcon (Anti x) = (styleIcon x)
    { stroke       = StrokeParalyzed
    , cornerRadius = 5
    , fill         = FillNonTopic
    }

abilityText :: Ability -> Text
abilityText Look = mkIconText '✆' 0.2 0.7 0.2 "ArialUnicodeMS"
abilityText Inspire = mkIconText '♥' 0.2 0.2 0.7 "ArialUnicodeMS"
abilityText Unparalyze = mkIconText '✙' 0.7 0.2 0.2 "ArialUnicodeMS"

abilityIcon :: Ability -> Shape
abilityIcon topic = mkShape
    { left            = 154
    , top             = 101.5
    , width           = 16
    , height          = 21
    , cornerRadius    = 5
    , verticalPadding = 12
    , fill            = FillWhite
    , stroke          = StrokeWhite
    , shadow          = ShadowBottom
    , text            = abilityText topic
    }

topicText :: Topic -> Text
topicText Art = mkIconText '♪' 0.6 0.4 0.4 "Helvetica"
topicText Chi = mkIconText '文' 0.4 0.6 0.7 "AR-PL-New-Kai"
topicText Eng = mkIconText 'A' 0.6 0.6 0.7 "AmericanTypewriter"
topicText Mat = mkIconText 'π' 0.4 0.5 0.4 "TrajanPro-Regular"
topicText Nat = mkIconText '☀' 0.5 0.7 0.4 "AR-PL-New-Kai"
topicText Phy = mkIconText '➶' 0.6 0.6 0.3 "ZapfDingbatsITC"
topicText Soc = mkIconText '☯' 0.7 0.5 0.7 "ArialUnicodeMS"

topicIcon :: Topic -> Shape
topicIcon topic = mkShape
    { left            = 8
    , top             = 101.5
    , width           = 16
    , height          = 21
    , cornerRadius    = 5
    , verticalPadding = 12
    , fill            = FillWhite
    , stroke          = StrokeWhite
    , shadow          = ShadowBottom
    , text            = topicText topic
    }

renderAllTopics = (topicIcon Chi)
    { text = mkIconText '∞' 0 0 0 "AmericanTypewriter"
    , fill = FillRadial (Color 0.8 0.8 0.9)
    }

renderAbility :: Ability -> Float -> Shape
renderAbility ability n = icon{ top = top + n * (height + 5) }
    where
    icon@Shape{..} = abilityIcon ability

renderTopic :: Topic -> Float -> Shape
renderTopic topic n = icon{ top = top + n * (height + 5) }
    where
    icon@Shape{..} = topicIcon topic

renderNonTopic :: Topic -> Float -> Shape
renderNonTopic topic n = icon
    { top    = top + n * (height + 5)
    , left   = 154
    , stroke = StrokeNone
    , fill   = FillNonTopic
    }
    where
    icon@Shape{..} = topicIcon topic

renderFlavor :: String -> Shape
renderFlavor flavor = mkShape
    { left            = 10
    , top             = 233
    , width           = 160
    , height          = 9
    , text            = mkText
        { txt   = flavor
        , font  = "AR-PL-New-Kai"
        , size  = 8
        , color = Color 0.1 0.1 0.1
        }
    }

mkShape = Shape
    { left            = 0
    , top             = 0
    , width           = error "missing width"
    , height          = error "missing height"
    , cornerRadius    = 0
    , verticalPadding = 0
    , fill            = FillNone
    , stroke          = StrokeNone
    , shadow          = ShadowNone
    , text            = TextNone
    , picture         = PictureNone
    }

renderTopicLarge :: Topic -> Shape
renderTopicLarge topic = mkShape
    { left            = 0
    , top             = 0
    , width           = cardWidth
    , height          = cardHeight -- - 60
    , cornerRadius    = 0
    , text            = (topicText topic){ size  = 144 }
    }

renderTopicLabel :: Topic -> Shape
renderTopicLabel topic = mkShape
    { left            = 0
    , top             = cardHeight - 70
    , width           = cardWidth
    , height          = 50
    , cornerRadius    = 3
    , text            = mkText
        { txt   = topicName topic
        , font  = "cwTeXKai"
        , size  = 48
        }
    }

renderTopicBorder :: Topic -> Shape
renderTopicBorder topic = mkShape
    { width        = cardWidth - 13
    , height       = cardHeight - 13
    , top          = 6.5
    , left         = 6.5
    , stroke       = StrokeThick $ color (topicText topic)
    , cornerRadius = 15
    }

renderStudentName :: String -> String -> Shape
renderStudentName name fontName = mkShape
    { left            = 4
    , top             = 5
    , width           = 168
    , height          = 25
    , cornerRadius    = 0
    , verticalPadding = 2
    , fill            = FillNone
    , stroke          = StrokeNone
    , shadow          = ShadowNone
    , text            = mkText
        { txt   = name
        , font  = fontName
        , size  = 22
        }
    }


renderName :: String -> String -> Shape
renderName name fontName = mkShape
    { left            = 76
    , top             = 43
    , width           = 28
    , height          = 138
    , cornerRadius    = 0
    , verticalPadding = 2
    , fill            = FillNone
    , stroke          = StrokeNone
    , shadow          = ShadowNone
    , text            = mkText
        { txt   = name
        , font  = fontName
        , size  = 18
        }
    }

renderPower :: Int -> Int -> Shape -- -> Color -> Color -> Shape
renderPower i u = Power
    { left            = 0
    , top             = 0
    , strength        = MkStrength i u
    }

renderEffect :: String -> Color -> Color -> Shape
renderEffect effect strokeColor fillColor = mkShape
    { left            = 4
    , top             = 197
    , width           = 168
    , height          = 25
    , stroke          = StrokeSingle strokeColor
    , shadow          = ShadowBottom
    , fill            = FillColor fillColor
    , cornerRadius    = 3
    , text            = mkText
        { txt   = effect
        , font  = "LiGothicMed"
        , size  = 10
        }
    }

renderTurns :: Int -> Shape
renderTurns turns = mkShape
    { left            = 69.5
    , top             = 164
    , width           = 40
    , height          = 22
    , cornerRadius    = 3
    , stroke          = StrokeSingle (Color 0.6 0.3 0.3)
    , fill            = FillWhite
    , verticalPadding = 4
    , text            = mkText
        { txt   = show turns ++ " ⏎"
        , font  = "Gentium"
        , size  = 14
        , placement = PlacementTop
        }
    }

renderSerial :: Color -> Int -> Shape
renderSerial = SerialShape 0 0

_DarkRed_ = Color 0.6 0.3 0.3
_Brown_ = Color 0.5 0.25 0
_LightGreen_ = Color 0.9 1 0.9
_Green_ = Color 0.1 0.6 0.1
_Blue_ = Color 0.3 0.3 0.5
_GrayBlue_ = Color 0.5 0.5 0.6

renderCard :: Card -> [Shape]
renderCard EmptyStudent =
    [ renderPower (-1) (-1) -- _Brown_ (Color 1 0.95 0.9)
    , renderSerial _Brown_ 0
    , innerRect _Brown_ (Color 0.9 0.85 0.8)
    , outerRect
    ]
renderCard RuleCard1 = [ RuleShape1 0 0 ]
renderCard RuleCard2 = [ RuleShape2 0 0 ]
renderCard FaceCard{..} = [ FaceShape 0 0 cardColor ]
renderCard TopicCard{..} =
    [ renderTopicLarge topic
--    , renderTopicLabel topic
    , renderTopicBorder topic
    ]
renderCard Environment{..} =
    [ renderFlavor flavor
    , renderName name "LiGothicMed"
    , (renderEffect effect (Color 0.1 0.6 0.1) _LightGreen_){ stroke = StrokeDotted }
    , renderSerial _LightGreen_ serial
    , outerRect{ fill = FillRadialOut (Color 0.5 0.7 0.4) }
    ]
renderCard Action{..} =
    [ renderFlavor flavor
    , renderName name "LiGothicMed"
    , renderTurns turns
    , renderEffect effect _DarkRed_ (Color 1 0.9 0.9)
    , renderSerial _DarkRed_ serial
    , innerRect _DarkRed_ (Color 0.6 0.5 0.5)
    , outerRect
    ]
renderCard Skill{..} =
    [ renderFlavor flavor
    , renderName name "LiGothicMed"
    , renderEffect effect _Green_ (Color 0.9 1 0.9)
    , renderSerial _Green_ serial
    , innerRect _Green_ (Color 0.5 0.6 0.4)
    , outerRect
    ]

renderCard Student{..} = topicsShapes ++ nonTopicShapes  ++ styleShapes ++
    [ renderFlavor flavor
    , renderName "" "cwTeXYen"
    , renderStudentName name "cwTeXYen"
    , renderPower interested uninterested -- _Brown_ (Color 1 0.95 0.9)
    , renderSerial _Brown_ serial
    , (innerRect _Brown_ (Color 0.9 0.85 0.8))
        { picture = PictureRelative ("images/students/" ++ show serial ++ ".jpg") }
    , outerRect
    ]
    where
    orderParalyzed xs = nps ++ ps
        where
        (ps, nps) = partition (`elem` paralyzed) xs
    styleShapes = map styleIcon styles
    nonTopicShapes =
        [ let shape = renderNonTopic t n in
            if t `elem` paralyzed
                then shape{ stroke = StrokeParalyzed }
                else shape
        | t <- orderParalyzed nonTopics
        | n <- [((1 - toEnum (length nonTopics)) / 2)..]
        ]
    nonTopics = filter (`notElem` topics) [minBound..maxBound]
    topicsShapes =
        [ let shape = renderTopic t n in
            if t `elem` paralyzed
                then shape{ stroke = StrokeParalyzed }
                else shape
        | t <- orderParalyzed topics
        | n <- [((1 - toEnum (length topics)) / 2)..]
        ]
renderCard Lesson{..} = topicsShapes ++ abilityShapes ++ styleShapes ++
    [ renderFlavor flavor
    , renderName name "cwTeXHeiBold"
    , renderPower interested uninterested -- power _Blue_ (Color 0.9 0.9 1)
    , renderSerial _Blue_ serial
    , innerRect _Blue_ _GrayBlue_
    , outerRect
    ]
    where
    {-
    power | interested == uninterested = interested'
          | otherwise                  = [$qq|$interested' / $uninterested'|]
    interested' = maybeNil interested
    uninterested' = maybeNil uninterested
    maybeNil n = case n of
        100 -> "✘"
        _ -> show n
    -}
    styleShapes = map styleIcon styles
    abilityShapes = [ renderAbility t n | t <- abilities | n <- [((1 - toEnum (length abilities)) / 2)..] ]
    topicsShapes = case topics of
        [] -> [ renderAllTopics ]
        _  -> [ renderTopic t n | t <- topics | n <- [((1 - toEnum (length topics)) / 2)..] ]
renderCard Assistant{..} = topicsShapes ++ abilityShapes ++ styleShapes ++
    [ renderFlavor flavor
    , renderName name "cwTeXHeiBold"
    , (renderPower (negate cost) (negate cost)) -- _Blue_ (Color 0.9 0.9 1))
        { cornerRadius = 3
        }
    , renderSerial _GrayBlue_ serial
    , (innerRect undefined undefined)
        { fill = FillRadialOut _GrayBlue_
        , stroke = StrokeDoubleDotted _Blue_
        }
    , outerRect
    ]
    where
    styleShapes = map styleIcon styles
    abilityShapes = [ renderAbility t n | t <- abilities | n <- [((1 - toEnum (length abilities)) / 2)..] ]
    topicsShapes = case topics of
        [] -> [ renderAllTopics ]
        _  -> [ renderTopic t n | t <- topics | n <- [((1 - toEnum (length topics)) / 2)..] ]

innerRect strokeColor fillColor = mkShape
    { width        = 108
    , height       = 154
    , left         = 36
    , top          = 35
    , cornerRadius = 15
    , stroke       = StrokeDouble strokeColor
    , fill         = FillLinear fillColor
    }

outerRect = mkShape
    { width        = cardWidth - 1
    , height       = cardHeight - 1
    , top          = 0.5
    , left         = 0.5
    , stroke       = StrokeBlack
    , cornerRadius = 15
    }

allCards = concat [ students, lessons, actions, skills, environments, assistants ]

#include "data.hs"


