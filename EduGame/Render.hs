{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax, OverloadedStrings #-}
module EduGame.Render where
import Data.Text (Text, singleton)
import EduGame.Types
import EduGame.Utils

innerRect strokeColor fillColor = mkShape
    { width        = 108
    , height       = 154
    , left         = 36
    , top          = 35
    , cornerRadius = 15
    , stroke       = StrokeNone -- StrokeDouble strokeColor
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
    , body            = BodyNone
    , picture         = PictureNone
    }

renderPower :: Int -> Int -> Shape -- -> Color -> Color -> Shape
renderPower i u = Power
    { left            = 0
    , top             = 0
    , strength        = MkStrength i u
    }

renderThreshold :: Threshold -> Shape -- -> Color -> Color -> Shape
renderThreshold t = Threshold
    { left            = 0
    , top             = 0
    , threshold        = t
    }

renderSerial :: Color -> Int -> Shape
renderSerial = SerialShape 0 0

styleIcon :: Style -> Shape
styleIcon V = mkShape
    { width   = 23
    , height  = 14
    , left    = 5
    , top     = 10
    , picture = Picture "v" 0
    }
styleIcon A = mkShape
    { width   = 23
    , height  = 33
    , left    = 5
    , top     = 195
    , picture = Picture "a" 0
    }
styleIcon R = mkShape
    { width   = 23
    , height  = 17
    , left    = 152
    , top     = 10
    , picture = Picture "r" 0
    }
styleIcon K = mkShape
    { width   = 23
    , height  = 33
    , left    = 152
    , top     = 195
    , picture = Picture "k" 0
    }
styleIcon (Anti (Anti x)) = styleIcon x
styleIcon (Anti x) = (styleIcon x)
    { stroke       = StrokeParalyzed
    , cornerRadius = 5
    , fill         = FillNonTopic
    }

renderFlavor :: Text -> Shape
renderFlavor flavor = mkShape
    { left            = 15
    , top             = 233
    , width           = 150
    , height          = 9
    , body            = mkBody
        { text  = flavor
        , font  = "STHeitiTC-Light"
        , size  = 8
        , color = Color 0.1 0.1 0.1
        }
    }

type Name = Text
type Font = Text
renderTitle :: Name -> Shape
renderTitle = (`renderStudentName` "cwTeXYen")

renderName :: Name -> Font -> Shape
renderName name fontName = mkShape
    { left            = 76
    , top             = 35 -- 43
    , width           = 28
    , height          = 138
    , cornerRadius    = 0
    , verticalPadding = 2
    , fill            = FillNone
    , stroke          = StrokeNone
    , shadow          = ShadowNone
    , body            = mkBody
        { text  = name
        , font  = fontName
        , size  = 18
        }
    }

mkBody = Body
    { text      = ""
    , color     = Color 0 0 0
    , font      = "ArialUnicodeMS"
    , size      = 0
    , placement = PlacementMiddle
    }

renderStudentName :: Name -> Font -> Shape
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
    , body            = mkBody
        { text  = name
        , font  = fontName
        , size  = 22
        }
    }

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

topicText :: Topic -> Body
topicText Art = mkIcon '♪' 0.6 0.4 0.4 "Helvetica"
topicText Chi = mkIcon '文' 0.4 0.6 0.7 "AR-PL-New-Kai"
topicText Eng = mkIcon 'A' 0.6 0.6 0.7 "AmericanTypewriter"
topicText Mat = mkIcon 'π' 0.4 0.5 0.4 "TrajanPro-Regular"
topicText Nat = mkIcon '☀' 0.5 0.7 0.4 "AR-PL-New-Kai"
topicText Phy = mkIcon '➶' 0.6 0.6 0.3 "ZapfDingbatsITC"
topicText Soc = mkIcon '☯' 0.7 0.5 0.7 "ArialUnicodeMS"

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
    , body            = topicText topic
    }

mkIcon ch r g b f = mkBody
    { text  = singleton ch
    , color = Color r g b
    , font  = f
    , size  = 14
    }

