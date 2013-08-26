{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax, OverloadedStrings, TypeSynonymInstances #-}
module EduGame.Parse where
import EduGame.Types
import Data.Char
import Data.Maybe
import Data.List (partition, intercalate, sort)
import System.Environment.FindBin
import Text.InterpolatedString.Perl6
import Data.Attoparsec.Text
import Data.Text (Text)
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P

parseTable :: Text -> [[(Text, Text)]]
parseTable s = [hs `zip` proc b | b <- tbody, not (T.null b) ]
    where
    (thead:tbody) = map T.strip . reverse . foldl multiLines [] $ T.lines s
    proc = tail . map T.strip . T.splitOn "|"
    hs = proc thead

multiLines :: [Text] -> Text -> [Text]
multiLines [] row = [row]
multiLines rows@(r:rs) row
    | T.null row = rows
    | '|' <- T.head row = (row:rows)
    | otherwise = (T.concat [r, "\n", row] : rs)


type Row = [(Text, Text)]
instance ShowQ Text where
    showQ = T.unpack

instance ShowQ (Text, Text) where
    showQ (x, y) = "(" ++ showQ x ++ ", " ++ showQ y ++ ")"

instance ShowQ Row where
    showQ xs = "[" ++ (intercalate ", " $ map showQ xs) ++ "]"

instance ShowQ [Row] where
    showQ xs = "[" ++ (intercalate ", " $ map showQ xs) ++ "]"

parser `from` table = do
    f <- T.readFile $ __Bin__ ++ "/data/" ++ table ++ ".txt"
    return $ map parser $ parseTable f

(<<<) :: Grok a => Row -> Text -> a
row <<< label = case lookup label row of
    Just text -> grok (T.strip text)
    _         -> error $ "Cannot find " ++ showQ label ++ " from " ++ showQ row

class Grok a where
    grok :: Text -> a
    grok = runParser parser
    parser :: Parser a
    parser = error "Parser undefined"

instance Grok String where parser = fmap T.unpack $ P.takeWhile (const True)
instance Grok Text where parser = P.takeWhile (const True)
instance Grok Int where parser = decimal <|> pure 0
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
    , effect     = r <<< "效果"
    , styles     = parseStyles r styleMap
    , interests  = sort (parseTopics r 'a' topicMap)
    , paralyses  = sort (parseTopics r 'x' topicMap)
    }

parseCourse :: Row -> Card
parseCourse r = Course
    { serial       = r <<< "編號"
    , name         = r <<< "名稱"
    , flavor       = r <<< "斜體字"
    , styles       = parseStyles r styleMap
    , topics       = parseTopics r 'o' topicMap
    , interested   = r <<< "興趣力道"
    , uninterested = r <<< "無興趣力道"
    , abilities    = sort (r <<< "特殊效果")
    }
    
parseGoal :: Row -> Card
parseGoal r = Goal
    { serial     = r <<< "編號"
    , name       = r <<< "名稱"
    , effect     = r <<< "效果"
    , flavor     = r <<< "斜體字"
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
        _         -> error $ showQ (r, label)

runParser :: Parser a -> Text -> a
runParser parser text = case parse parser text of
    Done _ x -> x
    Fail _ _ e -> error e
    Partial f -> case f T.empty of
        Done _ x -> x
        Fail _ _ e -> error e
        Partial {} -> error "Unterminated parse"
