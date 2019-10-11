import Data.List
import System.IO
import qualified Data.Map as Map


data Value =
  Numv  Float  |
  Boolv Bool   |
  Procv Env [Ast] Ast
  deriving (Eq)

instance Show Value where
  show (Numv x)  = show x
  show (Boolv x) = show x
  show (Procv _ _ _) = "#<procedure>"

instance Num Value where
  (Numv x) + (Numv y) = Numv $ x + y
  (Numv x) * (Numv y) = Numv $ x * y
  abs (Numv x)    = Numv $ abs x
  signum (Numv x) = Numv $ signum x
  fromInteger x   = Numv $ fromInteger x
  negate (Numv x) = Numv $ negate x

instance Fractional Value where
  (Numv x) / (Numv y) = Numv $ x / y
  fromRational x = Numv $ fromRational x


data Ast =
  Numa   Float   |
  Boola  Bool    |
  Ida    String  |
  Primv  String  |
  If       Ast Ast Ast      |
  Assume   [(Ast, Ast)] Ast |
  Function [Ast] Ast        |
  Apply    Ast [Ast]
  deriving (Eq, Read, Show)

type Env = Map.Map String Value

main = do
  putStr "functional: "
  hFlush stdout
  exp <- getLine
  if null exp
    then return ()
    else do
      putStrLn (show . run $ exp)
      main

run :: String -> Value
run = (eval $ Map.fromList def) . parse
  where def = map f ops
        f s = (s, Procv m fs $ Primv s)
        ops = ["+", "*", "-", "/", "=", "&", "|", "~", "zero?"]
        fs = [Ida "x", Ida "y"]
        m = Map.empty

eval :: Env -> Ast -> Value
eval _ (Numa  x) = Numv  x
eval _ (Boola x) = Boolv x
eval m (Ida x)   = get m x
eval m (Primv "+") = (get m "x") + (get m "y")
eval m (Primv "*") = (get m "x") * (get m "y")
eval m (Primv "-") = (get m "x") - (get m "y")
eval m (Primv "/") = (get m "x") / (get m "y")
eval m (Primv "=") = Boolv $ get m "x" == get m "y"
eval m (Primv "&") = Boolv $ get m "x" == Boolv True && get m "y" == Boolv True
eval m (Primv "|") = Boolv $ get m "x" == Boolv True || get m "y" == Boolv True
eval m (Primv "~") = Boolv $ if get m "x" == Boolv True then False else True
eval m (Primv "zero?") = Boolv $ get m "x" == Numv 0
eval m (If c t e)      = if eval m c == Boolv True then eval m t else eval m e
eval m (Assume bs x)   = eval m' x
  where m' = Map.union mb m
        mb = elaborate m bs
eval m (Function fs b) = Procv m fs b
eval m (Apply x ps)    = eval m' b
  where m' = Map.union mf ml
        mf = elaborate m $ zip fs ps
        (Procv ml fs b) = eval m x

elaborate :: Env -> [(Ast, Ast)] -> Env
elaborate m =  Map.fromList . map f
  where f (Ida x, e) = (x, eval m e)

get :: Env -> String -> Value
get m id = case v of
    (Just x) -> x
    Nothing  -> error $ "id " ++ id ++ " not set!"
  where v = Map.lookup id m


parse :: String -> Ast
parse s = (read . unwords . unpack . alter . Bnode "" . pack . words $ bpad) :: Ast
  where bpad = replace "(" " ( " . replace ")" " ) " . replace "[" "(" . replace "]" ")" $ s

parse' s = (unwords . unpack . alter . Bnode "" . pack . words $ bpad)
  where bpad = replace "(" " ( " . replace ")" " ) " . replace "[" "(" . replace "]" ")" $ s


alter :: Btree -> Btree
alter (Bnode _ (Bleaf "if":ns)) = (Bnode "(" (Bleaf "If":ns))
alter (Bnode _ (Bleaf "assume":ns)) = (Bnode "(" (Bleaf "Assume":ns'))
  where (Bnode _ bs):xs = ns
        ns' = (Bnode "[" bs'):xs'
        bs' = intersperse c . map pair $ bs
        pair (Bnode _ xv) = Bnode "(" . intersperse c . map alter $ xv
        xs' = map alter xs
        c = Bleaf ","
alter (Bnode _ (Bleaf "function":ns)) = (Bnode "(" (Bleaf "Function":ns'))
  where (Bnode _ fs):xs = ns
        ns' = (Bnode "[" fs'):xs'
        fs' = intersperse c . map alter $ fs
        xs' = map alter xs
        c = Bleaf ","
alter (Bnode _ (Bleaf "@":x:ps)) = (Bnode "(" (Bleaf "Apply":x':ps'))
  where x' = alter x
        ps' = [Bnode "[" $ intersperse c . map alter $ ps]
        c = Bleaf ","
alter (Bnode "(" ns) = alter $ Bnode "(" (Bleaf "@":ns)
alter (Bnode b ns) = Bnode b $ map alter ns
alter (Bleaf w) = Bleaf $ case w of
  w
    | isFloat w  -> "(Numa "  ++ w ++ ")"
    | isBool  w  -> "(Boola " ++ w ++ ")"
    | otherwise  -> "(Ida \""   ++ w ++ "\")"


data Btree =
  Bnode String [Btree] |
  Bleaf String
  deriving (Eq, Read, Show)

unpack :: Btree -> [String]
unpack (Bleaf w)  = [w]
unpack (Bnode b ns) = b : (foldr (++) [b'] $ map unpack ns)
  where b' = if b == "[" then "]" else (if b == "(" then ")" else "")

pack :: [String] -> [Btree]
pack [] = []
pack all@(w:ws)
  | isClose = []
  | isOpen  = node : pack ws'
  | otherwise = Bleaf w : pack ws
  where isOpen  = w == "[" || w == "("
        isClose = w == "]" || w == ")"
        node = Bnode w $ pack ws
        ws' = drop (area node) all
        win = pack ws

area :: Btree -> Int
area (Bleaf _) = 1
area (Bnode _ ns) = foldr (+) 2 $ map area ns


replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace from to all@(x:xs)
  | from `isPrefixOf` all = to ++ (replace from to . drop (length from) $ all)
  | otherwise             = x : replace from to xs

isFloat :: String -> Bool
isFloat s = case (reads s) :: [(Float, String)] of
  [(_, "")] -> True
  _         -> False

isBool :: String -> Bool
isBool s = case (reads s) :: [(Bool, String)] of
  [(_, "")] -> True
  _         -> False
