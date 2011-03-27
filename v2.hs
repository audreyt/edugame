{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax, OverloadedStrings, TypeSynonymInstances #-}
import Data.Char
import Data.Maybe
import Data.List (partition, intercalate)
import System.Environment.FindBin
import Text.InterpolatedString.Perl6
import Data.Attoparsec.Text
import Data.Text (Text)
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P


type Power = Maybe Int
data Style = V | A | R | K | Anti Style deriving Show

-- 「學門」：遊戲有「數學」、「中文」、「英文」、「自然」、「社會」、「藝術」、「健體」七個學門。
data Topic = Mat | Chi | Eng | Nat | Soc | Art | Phy deriving (Eq, Show, Enum, Bounded)

---- 學門簡寫
c,e,m,n,s,a,p :: Topic
c = Chi; e = Eng; m = Mat; n = Nat; s = Soc; a = Art; p = Phy

data Card
    = Student -- 學生
        { serial            :: Int        -- 序號
        , name              :: Text       -- 名稱
        , styles            :: [Style]    -- 學習風格
        , threshold         :: Int        -- 蒙昧值
        , interests         :: [Topic]    -- 有興趣之學門
        , paralyses         :: [Topic]    -- 有麻痺之學門
        , flavor            :: Text       -- 斜體字
        }
    | Action -- 行動
        { serial            :: Int        -- 序號
        , name              :: Text       -- 名稱
        , turns             :: Int        -- 所需回合
        , effect            :: Text       -- 效果
        , flavor            :: Text       -- 斜體字
        }
    | Environment -- 環境
        { serial            :: Int        -- 序號
        , name              :: Text       -- 名稱
        , effect            :: Text       -- 效果
        , flavor            :: Text       -- 斜體字
        }
    | Skill -- 技藝
        { serial            :: Int        -- 序號
        , name              :: Text       -- 名稱
        , effect            :: Text       -- 效果
        , flavor            :: Text       -- 斜體字
        }
    | Lesson -- 教學
        { serial            :: Int        -- 序號
        , name              :: Text       -- 名稱
        , styles            :: [Style]    -- 學習風格
        , interested        :: Maybe Int  -- 成就點數(有興趣時)
        , uninterested      :: Maybe Int  -- 成就點數(無興趣時)
        , topics            :: [Topic]    -- 學門
        , abilities         :: [Ability]  -- 特殊能力
        , flavor            :: Text       -- 斜體字
        }
    deriving Show

say = putStrLn

-- instance ShowQ [Shape] where
--     showQ = concatMap showQ

_Left_ = 9.6
_Top_ = 13.65

{-
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
-}


type X = Float
type Y = Float
renderCards :: X -> Y -> [Card] -> String
renderCards = undefined

-- 「特殊」：有些牌有特殊能力，如解麻痺、引發興趣等。
data Ability = Unparalyze | Inspire deriving Show

parseTable :: Text -> [[(Text, Text)]]
parseTable s = [hs `zip` proc b | b <- tbody, not (T.null b) ]
    where
    (thead:tbody) = map T.strip $ T.lines s
    proc = tail . map T.strip . T.splitOn "|"
    hs = proc thead


type Row = [(Text, Text)]
instance ShowQ Text where
    showQ = T.unpack

instance ShowQ (Text, Text) where
    showQ (x, y) = "(" ++ showQ x ++ ", " ++ showQ y ++ ")"

instance ShowQ Row where
    showQ xs = "[" ++ (intercalate ", " $ map showQ xs) ++ "]"

instance ShowQ [Row] where
    showQ xs = "[" ++ (intercalate ", " $ map showQ xs) ++ "]"

-- say [qq| { head $ parseTable s } |]
main = do
    students     <- parseStudent `from` "students"
    actions      <- parseAction `from` "actions"
    environments <- parseEnvironment `from` "environments"
    skills       <- parseSkill `from` "skills"
    lessons      <- parseLesson `from` "lessons"
    let _Cards_ = students ++ actions ++ environments ++ skills ++ lessons in print _Cards_

parser `from` table = do
    f <- T.readFile $ __Bin__ ++ "/v2/" ++ table ++ ".txt"
    return $ map parser $ parseTable f

(<<<) :: Grok a => Row -> Text -> a
row <<< label = case lookup label row of
    Just text -> grok (T.strip text)
    _         -> error $ show (row, label)

class Grok a where
    grok :: Text -> a
    grok = runParser parser
    parser :: Parser a
    parser = error "Parser undefined"

instance Grok String where parser = fmap T.unpack $ P.takeWhile (const True)
instance Grok Text where parser = P.takeWhile (const True)
instance Grok Int where parser = decimal
instance Grok (Maybe Int) where parser = fmap Just decimal <|> pure Nothing
instance Grok (Style -> Maybe Style) where
    parser = (char 'a' *> pure Just)
         <|> (char 'o' *> pure Just)
         <|> (char 'x' *> pure (Just . Anti))
         <|> (pure $ const Nothing)

instance Grok [Ability] where
    grok text = catMaybes $ map parsePair abilityMap
        where
        parsePair (ability, ch) = fmap (const ability) $ T.find (== ch) text


parseAction :: Row -> Card
parseAction r = Action
    { serial     = r <<< "編號"
    , name       = r <<< "名稱"
    , turns      = r <<< "所需行動"
    , effect     = r <<< "效果"
    , flavor     = r <<< "斜體字"
    }

parseSkill :: Row -> Card
parseSkill r = Skill
    { serial     = r <<< "編號"
    , name       = r <<< "名稱"
    , effect     = r <<< "效果"
    , flavor     = r <<< "斜體字"
    }

parseEnvironment :: Row -> Card
parseEnvironment r = case parseSkill r of
    Skill s n e f  -> Environment s n e f
    _              -> undefined

parseStudent :: Row -> Card
parseStudent r = Student
    { serial     = r <<< "編號"
    , name       = r <<< "名稱"
    , threshold  = r <<< "蒙昧值"
    , flavor     = r <<< "斜體字"
    , styles     = parseStyles r styleMap
    , interests  = parseTopics r 'a' topicMap
    , paralyses  = parseTopics r 'x' topicMap
    }

-- | 編號 | 名稱 | V | A | R | K | 教學風格 | 興趣力道 | 無興趣力道 | 學科 | 文學 | 外語 | 數學 | 自然 | 社會 | 藝術 | 健體 | 特殊效果 | 斜體字 |
parseLesson :: Row -> Card
parseLesson r = Lesson
    { serial       = r <<< "編號"
    , name         = r <<< "名稱"
    , flavor       = r <<< "斜體字"
    , styles       = parseStyles r styleMap
    , topics       = parseTopics r 'o' topicMap
    , interested   = r <<< "興趣力道"
    , uninterested = r <<< "無興趣力道"
    , abilities    = r <<< "特殊效果"
    }

styleMap = [(V, "視"), (A, "聽"), (R, "讀"), (K, "作")]
topicMap = [(c, "文學"), (e, "外語"), (m, "數學"), (n, "自然"), (s, "社會"), (a, "藝術"), (p, "健體")]
abilityMap = [(Inspire, 'I'), (Unparalyze, 'U')]

parseStyles r pairs = catMaybes $ map parsePair pairs
    where
    parsePair (style, label) = (r <<< label) style

parseTopics r ch pairs = catMaybes $ map parsePair pairs
    where
    parsePair (topic, label) = case lookup label r of
        Just text -> fmap (const topic) $ T.find (== ch) text
        _         -> error $ show (r, label)

runParser :: Parser a -> Text -> a
runParser parser text = case parse parser text of
    Done _ x -> x
    Fail _ _ e -> error e
    Partial f -> case f T.empty of
        Done _ x -> x
        Fail _ _ e -> error e
        Partial {} -> error "Unterminated parse"
        
