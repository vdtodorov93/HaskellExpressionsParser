import System.IO
import Data.Maybe
import Data.List
import Control.Monad
import System.IO.Unsafe
import qualified Data.Map as Map
data RegTree a = EmptyTree | Node a (RegTree a) (RegTree a) deriving (Show, Read)

--calcValue :: (RegTree a) => a -> Double
singleTon a = (Node a EmptyTree EmptyTree)
calcValue EmptyTree = 0.0
calcValue (Node a EmptyTree EmptyTree) = read a :: Double
calcValue (Node a l r)
	|a == "+" = (calcValue l) + (calcValue r)
	|a == "-" = (calcValue l) - (calcValue r)
	|a == "*" = (calcValue l) * (calcValue r)
	|a == "/" = ((calcValue l) / (calcValue r))
	|a == "^" = (calcValue l) ^ (floor (calcValue r))

isOp :: [Char] -> Bool
isOp x =
	x == "+" || x == "-" || x == "*" || x == "/"

isNumber :: String -> Bool
isNumber (x:xs)
	|(x >= '0' && x <= '9') = True
	|otherwise = False

stringToList :: String -> [String]
stringToList x = words x

parseTree :: [String] -> (RegTree String, [String])
parseTree (x:xs)
	|x == [] = (EmptyTree,[])
	|x == "(" = 
		let	left = (parseTree xs)
			op = head (snd left)
			right = (parseTree (tail $ snd left))
		in (Node op (fst left) (fst right), tail (snd right))
		--in (Node (head (snd left)) (fst left) (fst (parseTree (snd left))), (snd right))
	|otherwise = (singleTon x, xs)
--	|isNumber x = (singleTon x, xs)

--	|otherwise = (EmptyTree, [])

parse :: [String] -> (RegTree String)
parse x = fst $ parseTree x

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v): xs) = 
	if key == k
		then Just v
		else findKey key xs

takeValuesFromArgs :: Eq k => RegTree k -> [(k, k)] -> RegTree k
takeValuesFromArgs EmptyTree args = EmptyTree
takeValuesFromArgs (Node a left right) args = if (findKey a args == Nothing) 
	then (Node a (takeValuesFromArgs left args) (takeValuesFromArgs right args)) 
	else (Node (fromJust $ findKey a args) (takeValuesFromArgs left args) (takeValuesFromArgs right args))


calculate :: RegTree String -> [(String, String)] -> Double
calculate expr args = calcValue $ takeValuesFromArgs expr args

getContent path = do
	handle <- openFile path ReadMode
	contents <- hGetContents handle
	return contents

loadExpr path =
	parse (stringToList (unsafeDupablePerformIO (getContent path)))
{-
NOT WORKING !!!
takeValuesFromMap (x:str) m result
	|x == [] = result
	|findKey x m == Nothing = takeValuesFromMap str m (result++[x])
	|otherwise = takeValuesFromMap str m (result++[fromJust (findKey x m)])
-}
{-
takeValuesFromMap :: Eq k => [k] -> [(k, k)] -> [k] -> [k]
takeValuesFromMap str m result
	|str == [] = result
	|findKey (head str) m == Nothing = takeValuesFromMap (tail str) m (result++[head str])
	|otherwise = takeValuesFromMap (tail str) m (result++[fromJust (findKey (head str) m)])
-}
-- |otherwise = let v = findKey x m
--			in if v == Nothing then takeValuesFromMap str m (result:x) else takeValuesFromMap
--takeKvAndChange :: [String] -> (String,String) -> [String]
--takeKvAndChange (x:expr) (k,v) result
--	|x == [] = result
--	|x == k = takeKvAndChange expr (k,v) (result++[(snd v)])
--	|otherwise = takeKvAndChange expr (k,v) (result++[x])


{-
calculate expr args
	|args == [] = parse $ stringToList expr
	|
-}	
--parseTree String -> RegTree a
{-
 FAIL
parseTree (x:y:xs)
	|x == [] = EmptyTree
	|x == ")" = parseTree (y:xs)
	|x == "(" = parseTree (y:xs)
	|isOp y = (Node y (singleTon x) (parseTree xs) )
	|isNumber x = (Node x EmptyTree EmptyTree)
	|otherwise = EmptyTree
-}
--do contents <- readFile "expression.txt"
--evgeni = contents
--	|a == ['^'] = (calcValue l) ^ (calcValue r)

--let ivo = singleTon "5"
--let evgeni = (Node ['*'] (Node ['+'] (singleTon "5") (singleTon "6") ) (Node ['-'] (singleTon "9") (singleTon "3") ) )