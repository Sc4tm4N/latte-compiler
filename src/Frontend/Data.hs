module Frontend.Data (ParserFunction) where

import Frontend.Grammar.LexLatte (Token)
import Frontend.Grammar.ErrM (Err)

type ParserFunction a = [Token] -> Err a