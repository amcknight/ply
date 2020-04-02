module QueryBuilder
  ( build
  , table
  ) where

import qualified Tokenizer as T
import Control.Exception.Base (Exception, throw)

newtype QueryException = ParseException String deriving Show
instance Exception QueryException

-- TODO: This needs to work with:
-- Multiple From Strings or Join
-- include JOIN, GROUP BY, HAVING, LIMIT, ORDER BY
-- Multiple primitives rather than just String and Int (preferably as a param)
data Col = ICol String | SCol String
data EqClause = EqCC Col Col | EqCI Col Int | EdCS Col String | EqIC Int Col | EqSC String Col
newtype Select = Select [Col]
data From = From String [Col]
newtype Where = Where [EqClause]
data Query = SelectFromWhere Select From Where

-- TODO: Will need the headers for matching Cols
build :: String -> Query
--build str = SelectFromWhere (Select [SCol "first_name", ICol "age"]) (From "people_no_header" [SCol "first_name", SCol "last_name", ICol "age"]) (Where [EqCI (ICol "age") 35])
build str = parse $ T.tokenize str

parse :: [T.Token] -> Query
parse tokens = case tokens of
  [T.Select, T.Name, T.Name, T.From, T.Name] -> SelectFromWhere (Select [SCol "first_name", ICol "age"]) (From "people_no_header" [SCol "first_name", SCol "last_name", ICol "age"]) (Where [])
  _ -> throw $ ParseException $ "Could not parse tokens: " ++ show tokens

table :: Query -> String
table (SelectFromWhere _ (From t _) _) = t
