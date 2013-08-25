{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax, OverloadedStrings, TypeSynonymInstances #-}
import EduGame.Types
import EduGame.Utils
import EduGame.Render
import EduGame.Parse
import Text.InterpolatedString.Perl6
import Data.List (partition)
import qualified Data.Text as T

main = putStrLn =<< output

run = writeFile "/Library/Scripts/_OmniGraffle/1.scpt" =<< output

output = do
    students     <- parseStudent `from` "students"
    actions      <- parseAction `from` "actions"
    environments <- parseEnvironment `from` "environments"
    skills       <- parseSkill `from` "skills"
    courses      <- parseCourse `from` "courses"
    let _Cards_ = students ++ actions ++ environments ++ skills ++ courses
    return [qq|

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

renderCard :: Card -> [Shape]
renderCard EmptyStudent = map styleIcon [V,A,R,K] ++
    [ renderThreshold 0
    , renderSerial _Brown_ 0
    -- , innerRect _Brown_ (Color 0.9 0.85 0.8)
    , outerRect
    ]
renderCard Environment{..} =
    [ renderFlavor flavor
    , renderName name "LiGothicMed"
    , (renderEffect effect (Color 0.1 0.6 0.1) _LightGreen_){ stroke = StrokeDotted }
    , renderSerial _LightGreen_ serial
    , outerRect
        { fill = FillRadialOut (Color 0.5 0.7 0.4)
        , picture = Picture "environments" serial }
    ]
renderCard Action{..} =
    [ renderFlavor flavor
    , renderTitle name
    , renderTurns turns
    , renderEffect effect _DarkRed_ (Color 1 0.9 0.9)
    , renderSerial _DarkRed_ serial
    , (innerRect _DarkRed_ (Color 0.6 0.5 0.5))
        { picture = Picture "actions" serial }
    , outerRect { fill = FillColor (Color 1 0.95 0.95) }
    ]
renderCard Student{..} = topicsShapes ++ nonTopicShapes ++ styleShapes ++
    [ renderFlavor flavor
    , renderTitle name
    , renderThreshold threshold
    , renderEffect effect _DarkRed_ (Color 1 0.9 0.9)
    , renderSerial _Brown_ serial
    , (innerRect _Brown_ (Color 0.9 0.85 0.8))
        { picture = Picture "students" serial }
    , outerRect
    ]
    where
    orderParalyzed xs = nps ++ ps
        where
        (ps, nps) = partition (`elem` paralyses) xs
    styleShapes = map styleIcon styles
    nonTopicShapes =
        [ let shape = renderNonTopic t n in
            if t `elem` paralyses
                then shape{ stroke = StrokeParalyzed }
                else shape
        | t <- orderParalyzed nonTopics
        | n <- [((1 - toEnum (length nonTopics)) / 2)..]
        ]
    nonTopics = filter (`notElem` interests) [minBound..maxBound]
    topicsShapes =
        [ let shape = renderTopic t n in
            if t `elem` paralyses
                then shape{ stroke = StrokeParalyzed }
                else shape
        | t <- orderParalyzed interests
        | n <- [((1 - toEnum (length interests)) / 2)..]
        ]
renderCard Skill{..} =
    [ renderFlavor flavor
    , renderTitle name
    , renderEffect effect _Green_ (Color 0.9 1 0.9)
    , renderSerial _Green_ serial
    , (innerRect _Green_ (Color 0.5 0.6 0.4))
        { picture = Picture "skills" serial }
    , outerRect { fill = FillColor (Color 0.95 1 0.95) }
    ]
renderCard Course{..} = topicsShapes ++ abilityShapes ++ styleShapes ++
    [ renderFlavor flavor
    , renderTitle name
    , renderPower interested uninterested
    , renderSerial _Blue_ serial
    , (innerRect _Blue_ _GrayBlue_)
        { picture = Picture "courses" serial }
    , outerRect
    ]
    where
    styleShapes = map styleIcon styles
    abilityShapes = [ renderAbility t n | t <- abilities | n <- [((1 - toEnum (length abilities)) / 2)..] ]
    topicsShapes = case topics of
        [] -> [ renderAllTopics ]
        _  -> [ renderTopic t n | t <- topics | n <- [((1 - toEnum (length topics)) / 2)..] ]


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

renderTurns :: Int -> Shape
renderTurns turns = mkShape
    { left            = 127
    , top             = 100
    , width           = 40
    , height          = 22
    , cornerRadius    = 3
    , stroke          = StrokeMedium (Color 0.6 0.3 0.3)
    , fill            = FillWhite
    , verticalPadding = 4
    , body            = mkBody
        { text  = ("０１２３４５６７８９" `T.index` turns) `T.cons` "⏎"
        , font  = "LiGothicMed" -- "BookmanOldStyle-Bold" -- Gentium
        , size  = 15
        , placement = PlacementTop
        }
    }

renderEffect :: Text -> Color -> Color -> Shape
renderEffect effect strokeColor fillColor = mkShape
    { left            = 6
    , top             = 197
    , width           = 168
    , height          = 25
    , stroke          = StrokeSingle strokeColor
    , shadow          = ShadowBottom
    , fill            = FillColor fillColor
    , cornerRadius    = 3
    , body            = mkBody
        { text  = effect
        , font  = "LiGothicMed"
        , size  = 10
        }
    }

renderAllTopics = (topicIcon Chi)
    { body = mkIcon '∞' 0 0 0 "AmericanTypewriter"
    , fill = FillRadial (Color 0.8 0.8 0.9)
    }

renderAbility :: Ability -> Float -> Shape
renderAbility ability n = icon{ top = top + n * (height + 5) }
    where
    icon@Shape{..} = abilityIcon ability

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
    , body            = abilityText topic
    }

abilityText :: Ability -> Body
-- abilityText Look = mkIcon '✆' 0.2 0.7 0.2 "ArialUnicodeMS"
abilityText Inspire = mkIcon '♥' 0.2 0.2 0.7 "ArialUnicodeMS"
abilityText Unparalyze = mkIcon '✙' 0.7 0.2 0.2 "ArialUnicodeMS"
