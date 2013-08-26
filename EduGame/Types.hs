{-# LANGUAGE QuasiQuotes, NamedFieldPuns, RecordWildCards, ParallelListComp, FlexibleInstances, PatternGuards, CPP, UnicodeSyntax #-}
module EduGame.Types (module EduGame.Types, module Data.Text) where
import Data.Char
import Data.Maybe
import Data.List (partition, intercalate)
import System.Environment.FindBin
import Text.InterpolatedString.Perl6
import Data.Attoparsec.Text
import Data.Text (Text, pack, unpack)
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P

-- 「特殊」：有些牌有特殊能力，如解麻痺、引發興趣等。
data Ability = Unparalyze | Inspire deriving (Show, Eq, Ord)

data Style = V | A | R | K | Anti Style deriving Show

-- 「學門」：遊戲有「文史哲」、「數理」、「社會」、「藝能」四個學門。
data Topic = Chi | Nat | Soc | Art deriving (Eq, Ord, Show, Enum, Bounded)


{-
-- 「學門」：遊戲有「數學」、「中文」、「英文」、「自然」、「社會」、「藝術」、「健體」七個學門。
data Topic = Mat | Chi | Eng | Nat | Soc | Art | Phy deriving (Eq, Ord, Show, Enum, Bounded)
-}

type Power = Maybe Int


---- 學門簡寫
c,n,s,a :: Topic
c = Chi; n = Nat; s = Soc; a = Art

{-
---- 學門簡寫
c,e,m,n,s,a,p :: Topic
c = Chi; e = Eng; m = Mat; n = Nat; s = Soc; a = Art; p = Phy
-}

type Serial = Int
type Threshold = Int
type Negative = Int
data Card
    = EmptyStudent
    | Student -- 學生
        { serial            :: Serial     -- 序號
        , name              :: Text       -- 名稱
        , styles            :: [Style]    -- 學習風格
        , threshold         :: Threshold  -- 需求
        , negative			:: Negative   -- 負向
        , interests         :: [Topic]    -- 有興趣之學門
        , paralyses         :: [Topic]    -- 有麻痺之學門
        , effect            :: Text       -- 效果
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
    | Course -- 教學
        { serial            :: Int        -- 序號
        , name              :: Text       -- 名稱
        , styles            :: [Style]    -- 學習風格
        , interested        :: Power      -- 成就點數(有興趣時)
        , uninterested      :: Power      -- 成就點數(無興趣時)
        , topics            :: [Topic]    -- 學門
        , abilities         :: [Ability]  -- 特殊能力
        , flavor            :: Text       -- 斜體字
        }
    | Goal -- 勝利目標
        { serial            :: Int        -- 序號
        , name              :: Text       -- 名稱
        , effect            :: Text       -- 效果
        , flavor            :: Text       -- 斜體字
        }
    deriving Show
