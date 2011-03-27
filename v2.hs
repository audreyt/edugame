{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax, OverloadedStrings, TypeSynonymInstances #-}
import Data.Char
import Data.List (partition, intercalate)
import System.Environment.FindBin
import Text.InterpolatedString.Perl6
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Str = T.Text

_Cards_ = [] --students

data Style = V | A | R | K | Anti Style deriving Show

-- 「學門」：遊戲有「數學」、「中文」、「英文」、「自然」、「社會」、「藝術」、「健體」七個學門。
data Topic = Mat | Chi | Eng | Nat | Soc | Art | Phy deriving (Eq, Show, Enum, Bounded)

---- 學門簡寫
c,e,m,n,s,a,p :: Topic
c = Chi; e = Eng; m = Mat; n = Nat; s = Soc; a = Art; p = Phy

data Card
    = Student -- 學生
        { serial            :: Int          -- 序號
        , name              :: String       -- 名稱
        , styles            :: [Style]      -- 學習風格
        , threshold         :: Int          -- 蒙昧值
        , interested        :: [Topic]      -- 有興趣之學門
        , paralyzed         :: [Topic]      -- 有麻痺之學門
        , flavor            :: String       -- 斜體字
        }
    deriving Show

say = putStrLn

-- instance ShowQ [Shape] where
--     showQ = concatMap showQ

_Left_ = 9.6
_Top_ = 13.65

_main = do
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


type X = Float
type Y = Float
renderCards :: X -> Y -> [Card] -> String
renderCards = undefined

parseTable :: Str -> [[(Str, Str)]]
parseTable s = [hs `zip` proc b | b <- tbody]
    where
    (thead:tbody) = T.lines s
    proc = tail . map T.strip . T.splitOn "|"
    hs = proc thead

instance ShowQ Str where
    showQ = T.unpack

instance ShowQ (Str, Str) where
    showQ (x, y) = "(" ++ showQ x ++ ", " ++ showQ y ++ ")"

instance ShowQ [(Str, Str)] where
    showQ xs = "[" ++ (intercalate ", " $ map showQ xs) ++ "]"

instance ShowQ [[(Str, Str)]] where
    showQ xs = "[" ++ (intercalate ", " $ map showQ xs) ++ "]"

main = do
    s <- T.readFile $ __Bin__ ++ "/v2/students.txt"
    say [qq| { head $ parseTable s } |]

