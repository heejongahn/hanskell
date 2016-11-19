module Hangul
  ( HangulString
  , HangulChar
  , Choseong
  , Jungseong
  , Jongseong
  ) where

import Data.Maybe
import Data.String (IsString (fromString))

type HangulString = [HangulChar]

data HangulChar = HangulChar
  { choSeong :: Choseong
  , jungSeong :: Jungseong
  , jongSeong :: Jongseong
  }

data Choseong
  = ChG     -- ㄱ
  | ChKK    -- ㄲ
  | ChN     -- ㄴ
  | ChD     -- ㄷ
  | ChTT    -- ㄸ
  | ChL     -- ㄹ
  | ChM     -- ㅁ
  | ChB     -- ㅂ
  | ChBB    -- ㅃ
  | ChS     -- ㅅ
  | ChSS    -- ㅆ
  | ChNULL  -- ㅇ
  | ChJ     -- ㅈ
  | ChJJ    -- ㅉ
  | ChCH    -- ㅊ
  | ChK     -- ㅋ
  | ChT     -- ㅌ
  | ChP     -- ㅍ
  | ChH     -- ㅎ
  deriving (Eq, Show)

fromCharToChoseong :: Char -> Maybe Choseong
fromCharToChoseong c =
  case c of
    'ㄱ' -> Just ChG
    'ㄲ' -> Just ChKK
    'ㄴ' -> Just ChN
    'ㄷ' -> Just ChD
    'ㄸ' -> Just ChTT
    'ㄹ' -> Just ChL
    'ㅁ' -> Just ChM
    'ㅂ' -> Just ChB
    'ㅃ' -> Just ChBB
    'ㅅ' -> Just ChS
    'ㅆ' -> Just ChSS
    'ㅇ' -> Just ChNULL
    'ㅈ' -> Just ChJ
    'ㅉ' -> Just ChJJ
    'ㅊ' -> Just ChCH
    'ㅋ' -> Just ChK
    'ㅌ' -> Just ChT
    'ㅍ' -> Just ChP
    'ㅎ' -> Just ChH
    _    -> Nothing

instance IsString (Maybe Choseong) where
  fromString [c] = fromCharToChoseong c
  fromString _ = Nothing

data Jungseong
  = A   -- ㅏ
  | AE  -- ㅐ
  | YA  -- ㅑ
  | YAE -- ㅒ
  | EO  -- ㅓ
  | E   -- ㅔ
  | YEO -- ㅕ
  | YE  -- ㅖ
  | O   -- ㅗ
  | WA  -- ㅘ
  | WAE -- ㅙ
  | OE  -- ㅚ
  | U   -- ㅜ
  | WO  -- ㅝ
  | WE  -- ㅞ
  | WI  -- ㅟ
  | EU  -- ㅡ
  | UI  -- ㅢ
  | I   -- ㅣ
  deriving (Eq, Show)

fromCharToJungseong :: Char -> Maybe Jungseong
fromCharToJungseong c =
  case c of
    'ㅏ' -> Just A
    'ㅐ' -> Just AE
    'ㅑ' -> Just YA
    'ㅒ' -> Just YAE
    'ㅓ' -> Just EO
    'ㅔ' -> Just E
    'ㅕ' -> Just YEO
    'ㅖ' -> Just YE
    'ㅗ' -> Just O
    'ㅘ' -> Just WA
    'ㅙ' -> Just WAE
    'ㅚ' -> Just OE
    'ㅜ' -> Just U
    'ㅝ' -> Just WO
    'ㅞ' -> Just WE
    'ㅟ' -> Just WI
    'ㅡ' -> Just EU
    'ㅢ' -> Just UI
    'ㅣ' -> Just I
    _    -> Nothing

instance IsString (Maybe Jungseong) where
  fromString [c] = fromCharToJungseong c
  fromString _   = Nothing

data Jongseong
  = JG   -- ㄱ
  | JKK  -- ㄲ
  | JGS  -- ㄳ
  | JN   -- ㄴ
  | JNJ  -- ㄵ
  | JNH  -- ㄶ
  | JD   -- ㄷ
  | JL   -- ㄹ
  | JLG  -- ㄺ
  | JLM  -- ㄻ
  | JLB  -- ㄼ
  | JLS  -- ㄽ
  | JLT  -- ㄾ
  | JLP  -- ㄿ
  | JLH  -- ㅀ
  | JM   -- ㅁ
  | JB   -- ㅂ
  | JBS  -- ㅄ
  | JS   -- ㅅ
  | JSS  -- ㅆ
  | JNG  -- ㅇ
  | JJ   -- ㅈ
  | JCH  -- ㅊ
  | JK   -- ㅋ
  | JT   -- ㅌ
  | JP   -- ㅍ
  | JH   -- ㅎ
  deriving (Eq, Show)

fromCharToJongseong :: Char -> Maybe Jongseong
fromCharToJongseong c =
  case c of
    'ㄱ' -> Just JG
    'ㄲ' -> Just JKK
    'ㄳ' -> Just JGS
    'ㄴ' -> Just JN
    'ㄵ' -> Just JNJ
    'ㄶ' -> Just JNH
    'ㄷ' -> Just JD
    'ㄹ' -> Just JL
    'ㄺ' -> Just JLG
    'ㄻ' -> Just JLM
    'ㄼ' -> Just JLB
    'ㄽ' -> Just JLS
    'ㄾ' -> Just JLT
    'ㄿ' -> Just JLP
    'ㅀ' -> Just JLH
    'ㅁ' -> Just JM
    'ㅂ' -> Just JB
    'ㅄ' -> Just JBS
    'ㅅ' -> Just JS
    'ㅆ' -> Just JSS
    'ㅇ' -> Just JNG
    'ㅈ' -> Just JJ
    'ㅊ' -> Just JCH
    'ㅋ' -> Just JK
    'ㅌ' -> Just JT
    'ㅍ' -> Just JP
    'ㅎ' -> Just JH
    _    -> Nothing

instance IsString (Maybe Jongseong) where
  fromString [c] = fromCharToJongseong c
  fromString _ = Nothing
