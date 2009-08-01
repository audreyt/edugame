{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP #-}
import Data.Char
import System.Environment.FindBin
import qualified System.IO.UTF8 as UTF8
import Text.InterpolatedString.Perl6

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
        { name              :: String       -- 名稱
        , styles            :: [Style]      -- 學習風格
        , interested        :: Int          -- 蒙昧值(有興趣時)
        , uninterested      :: Int          -- 蒙昧值(無興趣時)
        , topics            :: [Topic]      -- 有興趣之學門
        , paralyzed         :: [Topic]      -- 有麻痺之學門
        , flavor            :: String       -- 斜體字
        }
    | Lesson -- 教學
        { name              :: String       -- 名稱
        , styles            :: [Style]      -- 學習風格
        , interested        :: Int          -- 成就點數(有興趣時)
        , uninterested      :: Int          -- 成就點數(無興趣時)
        , topics            :: [Topic]      -- 學門
        , abilities         :: [Ability]    -- 特殊能力
        , flavor            :: String       -- 斜體字
        }
    | Action -- 行動
        { name              :: String       -- 名稱
        , turns             :: Int          -- 所需回合
        , effect            :: String       -- 效果
        , flavor            :: String       -- 斜體字
        }
    | Skill -- 技藝
        { name              :: String       -- 名稱
        , effect            :: String       -- 效果
        , flavor            :: String       -- 斜體字
        }
    | Environment -- 環境
        { name              :: String       -- 名稱
        , effect            :: String       -- 效果
        , flavor            :: String       -- 斜體字
        }
    | Assistant -- 助教
        { name              :: String       -- 名稱
        , styles            :: [Style]      -- 額外風格
        , topics            :: [Topic]      -- 解麻痺學科
        , abilities         :: [Ability]    -- 特殊能力
        , cost              :: Int          -- 啟動所需力道
        , flavor            :: String       -- 斜體字
        }
    | TopicCard { topic :: Topic }
    deriving Show

--------------------------

say = UTF8.putStrLn

instance ShowQ [Shape] where
    showQ = concatMap showQ

_Left_ = 5
_Top_ = 5

main = do
    say [$qq|

tell application "OmniGraffle Professional 5"
    tell document of front window
        set count_canvas to count of canvases
        set canvas_no to count_canvas
        tell canvas canvas_no
{ renderCards _Left_ _Top_ allCards }
        end tell
    end tell
end tell
    |]

-- (concat . replicate 3 $ map TopicCard [minBound..maxBound])


renderCards :: X -> Y -> [Card] -> [Shape]
renderCards _  _  []     = []
renderCards xo yo (c:cs) = map adjustOffset (renderCard c) ++ maybePageBreak ++ renderCards xo' yo' cs
    where
    adjustOffset s@Shape{..} = s{ left = left + xo, top = top + yo }
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
paperWidth = 500
paperHeight = 600

type X = Float
type Y = Float

data Stroke = StrokeWhite | StrokeBlack | StrokeParalyzed | StrokeDotted | StrokeDouble Color | StrokeDoubleDotted Color | StrokeSingle Color | StrokeNone
data Shadow = ShadowBottom | ShadowMiddle | ShadowNone
data Placement = PlacementTop | PlacementMiddle

instance ShowQ Placement where
    showQ PlacementTop = "text placement: top,"
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
    } | PageBreak

instance ShowQ Shape where
    showQ PageBreak = [$q|
        end tell
        make new canvas
        set count_canvas to count of canvases
        set canvas_no to count_canvas
        tell canvas canvas_no
    |]
    showQ Shape{..} = [$qq|$_begin $stroke $shadow $text $fill $_origin $_size $_end
$picture
|]
        where
        _begin = "make new shape at end of graphics with properties {"
        _end   = [$qq|corner radius: $cornerRadius, vertical padding: $verticalPadding, side padding: 0 }|]
        _size   = [$qq|size: \{$width, $height}, |]
        _origin = [$qq|origin: \{$left, $top}, |]

data Fill = FillWhite | FillLinear Color | FillRadial Color | FillRadialOut Color | FillColor Color | FillParalyzed | FillNone

instance ShowQ Fill where
    showQ FillWhite = ""
    showQ (FillLinear color) = [$qq|gradient $color fill color:\{0.95, 0.95, 0.95}, fill: linear fill,|]
    showQ (FillRadial color) = [$qq|fill $color gradient color:\{0.95, 0.95, 0.95}, fill: radial fill,|]
    showQ (FillRadialOut color) = [$qq|gradient $color fill: radial fill,|]
    showQ (FillColor color) = [$qq|fill $color|]
    showQ FillParalyzed = showQ $ FillRadialOut (Color 0.7 0.7 0.7)
    showQ FillNone = "fill: no fill,"


instance ShowQ Stroke where
    showQ StrokeNone = "draws stroke:false,"
    showQ StrokeParalyzed = "stroke color: {0.5, 0.2, 0.2}, stroke pattern: 3,"
    showQ StrokeDotted = "stroke color: {0.5, 0.5, 0.5}, stroke pattern: 24,"
    showQ StrokeWhite = "stroke color: {1, 1, 1},"
    showQ StrokeBlack = "stroke color: {0, 0, 0},"
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
    showQ (PictureRelative path) = '\n':[$qq|set image of result to "$__Bin__/$path"|]
    showQ PictureNone = ""

data Color = Color { red :: Float, green :: Float, blue :: Float }

instance ShowQ Color where
    showQ Color{..} = [$qq|color: \{$red, $green, $blue}, |]

mkIconText ch r g b f = mkText
    { txt   = [ch]
    , color = Color r g b
    , font  = f
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
    { stroke = StrokeParalyzed
    , cornerRadius = 10
    , fill = FillParalyzed
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

renderParalyzed :: Topic -> Float -> Shape
renderParalyzed topic n = icon
    { top    = top + n * (height + 5)
    , left   = 154
    , stroke = StrokeParalyzed
    , fill   = FillParalyzed
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
    , height          = cardHeight - 60
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

renderPower :: String -> Color -> Color -> Shape
renderPower power strokeColor fillColor = mkShape
    { left            = 64
    , top             = 200
    , width           = 52
    , height          = 19
    , cornerRadius    = 20
    , verticalPadding = 2
    , stroke          = StrokeSingle strokeColor
    , fill            = FillColor fillColor
    , shadow          = ShadowBottom
    , text            = mkText
        { txt   = power
        , font  = "DroidSerif"
        , size  = 10
        }
    }

renderEffect :: String -> Color -> Color -> Shape
renderEffect effect strokeColor fillColor = mkShape
    { left            = 18
    , top             = 197
    , width           = 144
    , height          = 25
    , stroke          = StrokeSingle strokeColor
    , shadow          = ShadowBottom
    , fill            = FillColor fillColor
    , cornerRadius    = 3
    , text            = mkText
        { txt   = effect
        , font  = "LiGothicMed"
        , size  = 8
        }
    }

renderTurns :: Int -> Shape
renderTurns turns = mkShape
    { left            = 77
    , top             = 171
    , width           = 25
    , height          = 16
    , stroke          = StrokeSingle (Color 0.6 0.3 0.3)
    , fill            = FillWhite
    , verticalPadding = 4
    , text            = mkText
        { txt   = show turns ++ " ⏎"
        , font  = "Gentium"
        , size  = 8
        , placement = PlacementTop
        }
    }

_DarkRed_ = Color 0.6 0.3 0.3

renderCard :: Card -> [Shape]
renderCard TopicCard{..} =
    [ renderTopicLarge topic
    , renderTopicLabel topic
    , outerRect
    ]
renderCard Environment{..} =
    [ renderFlavor flavor
    , renderName name "LiGothicMed"
    , (renderEffect effect (Color 0.1 0.6 0.1) (Color 0.9 1 0.9)){ stroke = StrokeDotted }
    , outerRect{ fill = FillRadialOut (Color 0.5 0.7 0.4) }
    ]
renderCard Action{..} =
    [ renderFlavor flavor
    , renderName name "LiGothicMed"
    , renderTurns turns
    , renderEffect effect _DarkRed_ (Color 1 0.9 0.9)
    , innerRect _DarkRed_ (Color 0.6 0.5 0.5)
    , outerRect
    ]
renderCard Skill{..} =
    [ renderFlavor flavor
    , renderName name "LiGothicMed"
    , renderEffect effect (Color 0.1 0.6 0.1) (Color 0.9 1 0.9)
    , innerRect (Color 0.1 0.6 0.1) (Color 0.5 0.6 0.4)
    , outerRect
    ]

renderCard Student{..} = topicsShapes ++ paralyzedShapes ++ styleShapes ++
    [ renderFlavor flavor
    , renderName name "cwTeXYen"
    , renderPower [$qq|$interested / $uninterested|] (Color 0.5 0.25 0) (Color 1 0.95 0.9)
    , innerRect (Color 0.5 0.25 0) (Color 0.9 0.85 0.8)
    , outerRect
    ]
    where
    styleShapes = map styleIcon styles
    paralyzedShapes = [ renderParalyzed t n | t <- paralyzed' | n <- [((1 - toEnum (length paralyzed')) / 2)..] ]
    paralyzed' = filter (`notElem` topics) paralyzed
    topicsShapes =
        [ let shape = renderTopic t n in
            if t `elem` paralyzed
                then shape{ stroke = StrokeParalyzed, fill = FillParalyzed }
                else shape
        | t <- topics
        | n <- [((1 - toEnum (length topics)) / 2)..]
        ]
renderCard Lesson{..} = topicsShapes ++ abilityShapes ++ styleShapes ++
    [ renderFlavor flavor
    , renderName name "cwTeXHeiBold"
    , renderPower power (Color 0.3 0.3 0.5) (Color 0.9 0.9 1)
    , innerRect (Color 0.3 0.3 0.5) (Color 0.5 0.5 0.6)
    , outerRect
    ]
    where
    power | interested == uninterested = interested'
          | otherwise                  = [$qq|$interested' / $uninterested'|]
    interested' = maybeNil interested
    uninterested' = maybeNil uninterested
    maybeNil n = case n of
        0 -> "✘"
        _ -> show n
    styleShapes = map styleIcon styles
    abilityShapes = [ renderAbility t n | t <- abilities | n <- [((1 - toEnum (length abilities)) / 2)..] ]
    topicsShapes = case topics of
        [] -> [ renderAllTopics ]
        _  -> [ renderTopic t n | t <- topics | n <- [((1 - toEnum (length topics)) / 2)..] ]
renderCard Assistant{..} = topicsShapes ++ abilityShapes ++ styleShapes ++
    [ renderFlavor flavor
    , renderName name "cwTeXHeiBold"
    , renderPower (show (negate cost)) (Color 0.3 0.3 0.5) (Color 0.9 0.9 1)
    , (innerRect undefined undefined)
        { fill = FillRadialOut (Color 0.5 0.5 0.6)
        , stroke = StrokeDoubleDotted (Color 0.3 0.3 0.5)
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
    { width  = cardWidth
    , height = cardHeight
    , stroke = StrokeBlack
    , cornerRadius = 15
    }

allCards = concat [ students, lessons, actions, skills, environments, assistants ]

#include "data.hs"
