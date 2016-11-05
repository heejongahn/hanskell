module Hangul
  ( HangulString
  , HangulChar
  , ChoSeong
  , JungSeong
  , JongSeong
  ) where

import Data.Maybe
import Data.String (IsString)

data HangulString = [HangulChar]
data HangulChar = HangulChar
  { choSeong :: ChoSeong
  , jungSeong :: JungSeong
  , jongSeong :: JongSeong
  } deriving (Show)

data Choseong
  = G     -- ㄱ
  | KK    -- ㄲ
  | N     -- ㄴ
  | D     -- ㄷ
  | TT    -- ㄸ
  | L     -- ㄹ
  | M     -- ㅁ
  | B     -- ㅂ
  | BB    -- ㅃ
  | S     -- ㅅ
  | SS    -- ㅆ
  | NULL  -- ㅇ
  | J     -- ㅈ
  | JJ    -- ㅉ
  | CH    -- ㅊ
  | K     -- ㅋ
  | T     -- ㅌ
  | P     -- ㅍ
  | H     -- ㅎ

instance IsString (Maybe Choseong) where
  fromString s =
    case s of
      "ㄱ" -> Just G
      "ㄲ" -> Just KK
      "ㄴ" -> Just N
      "ㄷ" -> Just D
      "ㄸ" -> Just TT
      "ㄹ" -> Just L
      "ㅁ" -> Just M
      "ㅂ" -> Just B
      "ㅃ" -> Just BB
      "ㅅ" -> Just S
      "ㅆ" -> Just SS
      "ㅇ" -> Just NULL
      "ㅈ" -> Just J
      "ㅉ" -> Just JJ
      "ㅊ" -> Just CH
      "ㅋ" -> Just K
      "ㅌ" -> Just T
      "ㅍ" -> Just P
      "ㅎ" -> Just H
      _    -> Nothing

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

instance IsString (Maybe Jungseong) where
  fromString s =
    case s of
      "ㅏ" -> Just A
      "ㅐ" -> Just AE
      "ㅑ" -> Just YA
      "ㅒ" -> Just YAE
      "ㅓ" -> Just EO
      "ㅔ" -> Just E
      "ㅕ" -> Just YEO
      "ㅖ" -> Just YE
      "ㅗ" -> Just O
      "ㅘ" -> Just WA
      "ㅙ" -> Just WAE
      "ㅚ" -> Just OE
      "ㅜ" -> Just U
      "ㅝ" -> Just WO
      "ㅞ" -> Just WE
      "ㅟ" -> Just WI
      "ㅡ" -> Just EU
      "ㅢ" -> Just UI
      "ㅣ" -> Just I
      _    -> Nothing

data Jongseong
  = G   -- ㄱ
  | KK  -- ㄲ
  | GS  -- ㄳ
  | N   -- ㄴ
  | NJ  -- ㄵ
  | NH  -- ㄶ
  | D   -- ㄷ
  | L   -- ㄹ
  | LG  -- ㄺ
  | LM  -- ㄻ
  | LB  -- ㄼ
  | LS  -- ㄽ
  | LT  -- ㄾ
  | LP  -- ㄿ
  | LH  -- ㅀ
  | M   -- ㅁ
  | B   -- ㅂ
  | BS  -- ㅄ
  | S   -- ㅅ
  | SS  -- ㅆ
  | NG  -- ㅇ
  | J   -- ㅈ
  | CH  -- ㅊ
  | K   -- ㅋ
  | T   -- ㅌ
  | P   -- ㅍ
  | H   -- ㅎ

instance IsString (Maybe Jongseong) where
  fromString s =
    case s of
      "ㄱ" -> Just G
      "ㄲ" -> Just KK
      "ㄳ" -> Just GS
      "ㄴ" -> Just N
      "ㄵ" -> Just NJ
      "ㄶ" -> Just NH
      "ㄷ" -> Just D
      "ㄹ" -> Just L
      "ㄺ" -> Just LG
      "ㄻ" -> Just LM
      "ㄼ" -> Just LB
      "ㄽ" -> Just LS
      "ㄾ" -> Just LT
      "ㄿ" -> Just LP
      "ㅀ" -> Just LH
      "ㅁ" -> Just M
      "ㅂ" -> Just B
      "ㅄ" -> Just BS
      "ㅅ" -> Just S
      "ㅆ" -> Just SS
      "ㅇ" -> Just NG
      "ㅈ" -> Just J
      "ㅊ" -> Just CH
      "ㅋ" -> Just K
      "ㅌ" -> Just T
      "ㅍ" -> Just P
      "ㅎ" -> Just H
      _    -> Nothing
