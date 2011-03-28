{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax #-}
module EduGame.Render where
import EduGame.Types
import EduGame.Utils

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
    , picture = PictureRelative "v2/images/v.png"
    }
styleIcon A = mkShape
    { width   = 23
    , height  = 33
    , left    = 5
    , top     = 195
    , picture = PictureRelative "v2/images/a.png"
    }
styleIcon R = mkShape
    { width   = 23
    , height  = 17
    , left    = 152
    , top     = 10
    , picture = PictureRelative "v2/images/r.png"
    }
styleIcon K = mkShape
    { width   = 23
    , height  = 33
    , left    = 152
    , top     = 195
    , picture = PictureRelative "v2/images/k.png"
    }
styleIcon (Anti (Anti x)) = styleIcon x
styleIcon (Anti x) = (styleIcon x)
    { stroke       = StrokeParalyzed
    , cornerRadius = 5
    , fill         = FillNonTopic
    }
