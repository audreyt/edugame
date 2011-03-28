{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax, OverloadedStrings, TypeSynonymInstances #-}
import EduGame.Types
import EduGame.Utils
import EduGame.Render
import EduGame.Parse
import Text.InterpolatedString.Perl6

main = do
    students     <- parseStudent `from` "students"
    actions      <- parseAction `from` "actions"
    environments <- parseEnvironment `from` "environments"
    skills       <- parseSkill `from` "skills"
    lessons      <- parseLesson `from` "lessons"
    -- let _Cards_ = students ++ actions ++ environments ++ skills ++ lessons in print _Cards_
    let _Cards_ = [EmptyStudent]
    say [qq|

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
    , innerRect _Brown_ (Color 0.9 0.85 0.8)
    , outerRect
    ]

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


