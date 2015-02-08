module Main where

getThing :: String -> IO Int
-- getThing "foo" = IO 2  This wont work because IO is a restricted constructor
getThing "foo" = return 2
getThing _ = return 1  -- like a switch, this is the "default"

--symetricMagic :: (a,a)
--intTuple :: (Int,Int)
--mixedTuple :: (Int,String)
-- a-m => types x,y,z "values"
tupleMagic :: (a,b) -> (b,a)
tupleMagic (x,y) = (y,x)

-- /foo/
-- Matcher can be either
--   Dot :: Matcher
--   Sequence :: Matcher
--   Matcher :: *
data Matcher = Dot | Sequence String
   deriving (Show)
-- Regex can either be
--   Match :: Matcher -> String -> Regex or two args become "it"
--   Unmatched :: String -> Regex
--   Regex :: *
data Regex = Match Matcher String | Unmatched String
   deriving (Show)

doesMatch :: [Regex] -> Maybe [Regex]
doesMatch [Unmatched _] = Nothing
doesMatch rss@(r:rs) = case r of
  Unmatched _ -> fmap ([r] ++) (doesMatch rs)
  Match _ _ -> Just rss


findMatch :: Matcher -> String ->  [Regex]
findMatch (Sequence pattern) matchedString =
   go ( []) pattern matchedString

   where

   buildMatch :: Char -> Char -> Regex
  --  buildMatch a b = Match (Sequence [a]) [b]
   buildMatch = (. return) . Match . Sequence . return

   addNewMatch :: [Regex] -> String -> String ->  [Regex]
   addNewMatch rs (p:ps) (s:ss) = go ( (rs ++ [buildMatch p s] )) ss ps


   addToUnmatched :: [Regex] -> String -> String ->  [Regex]
   addToUnmatched rs (s:ss) us = go (init rs ++ [Unmatched $ us ++ [s]]) pattern ss


   go :: [Regex] -> String -> String ->  [Regex]
   -- first
   go [] (p:ps) (s:ss)  = if s == p

     then go ( [buildMatch p s]) ps ss
     else go ([Unmatched [s]]) pattern ss

   go rs [p] [s] = let begin = (init rs ++) in case last rs of

     Unmatched us -> if s == p
       then begin [Match (Sequence [p]) [s]]
       else begin [Unmatched [s]]

     Match (Sequence up) us -> if s == p
       then begin [Match (Sequence $ up ++ [p]) $ us ++ [s]]
       else begin [Unmatched $ us ++ [s]]

   go rs (ps) [s] = case last rs of

     Unmatched us ->          init rs ++ [Unmatched $ us ++ [s]]
     Match (Sequence _) us -> init rs ++ [Unmatched $ us ++ [s]]

   go rs pp@[p] sss@(s:ss) = case last rs of

     Unmatched us -> if s == p
       then addNewMatch    rs pp sss
       else addToUnmatched rs sss us

     Match (Sequence up) us -> if s == p
       then init rs
         ++ [Match (Sequence $ up ++ [p]) (us ++ [s])]
         ++ [Unmatched ss]
       else addToUnmatched rs sss us

   go rs pp@(p:ps) sss@(s:ss) = case last rs of

     Unmatched us -> if s == p
       then addNewMatch rs pp sss
       else go ( init rs ++ [Unmatched (us ++ [s])] ) pattern ss

     Match (Sequence up) us -> if s == p
       then go (init rs ++ [Match (Sequence $ up ++ [p]) $ us ++ [s]]) ps ss
       else go (init rs ++ [Unmatched [s]] ) pattern ss

---- doesMatch :: [] Matchers -> (String -> [] Regex)
-- doesMatch :: [Matcher] -> String -> [Regex]
-- doesMatch matchers astring = foldr go [] matchers
--
--    where
--    go :: Matcher -> [Regex] -> [Regex]
--    go (Sequence seq) regexes =
--      if
---- Sequence foo
---- go Dot regexes =

main :: IO ()
main = do
  print $ findMatch (Sequence "foo") "foobar"
  print $ findMatch (Sequence "foo") "barfoo"
  print $ doesMatch $ findMatch (Sequence "foo") "foobar"
  print $ doesMatch $ findMatch (Sequence "foo") "barfoo"
  print $ findMatch (Sequence "foo") "nobar"
  print $ doesMatch $ findMatch (Sequence "foo") "nobar"


--main = getThing "foo" >>= \f ->
--  print "hello world" >>= \_ ->
--    putStrLn $ "strings only!!!" ++ show f

-- >>= is a monadic bind, only operates on arguments that are of the same
-- print "" IS an IO (), so is printStrLn so "getThing" must be an IO _

-- () is a "thing" --> empty tuple, (1) -> 1 tuple, (1,2,3) -> triple
