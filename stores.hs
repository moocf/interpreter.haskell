import Data.List
import System.IO
import Text.Printf
import qualified Data.Map as Map


data Value =
  Numv  Float  |
  Boolv Bool   |
  Procv Env [Ast] Ast |
  Recuv Env Env [Ast] Ast
  deriving (Eq)

instance Show Value where
  show (Numv x)  = show x
  show (Boolv x) = show x
  show (Procv _ _ _) = "#<procedure>"
  show (Recuv _ _ _ _) = "#<procedure>"

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
  If        Ast Ast Ast      |
  SetRef    Ast Ast          |
  DeRef     Ast              |
  NewRef    Ast              |
  Seq       [Ast]            |
  Assume    [(Ast, Ast)] Ast |
  Function  [Ast] Ast        |
  Recfun    [(Ast, [Ast], Ast)] Ast |
  Apply     Ast [Ast]
  deriving (Eq, Read, Show)

type Env   = Map.Map String Value
type Store = [Value]
type Ref   = Int

main = do
  putStr "stores: "
  hFlush stdout
  exp <- getLine
  if null exp
    then return ()
    else do
      putStrLn (show . run $ exp)
      main

run :: String -> (Store, Value)
run = (eval [] $ Map.fromList def) . parse
  where def = map f ops
        f s = (s, Procv m fs $ Primv s)
        ops = ["+", "*", "-", "/", "=", "&", "|", "~", "zero?"]
        fs = [Ida "x", Ida "y"]
        m = Map.empty

eval :: Store -> Env -> Ast -> (Store, Value)
eval s _ (Numa  x) = (s, Numv  x)
eval s _ (Boola x) = (s, Boolv x)
eval s m (Ida x)   = (s, get m x)
eval s m (Primv "+") = (s, (get m "x") + (get m "y"))
eval s m (Primv "*") = (s, (get m "x") * (get m "y"))
eval s m (Primv "-") = (s, (get m "x") - (get m "y"))
eval s m (Primv "/") = (s, (get m "x") / (get m "y"))
eval s m (Primv "=") = (s, Boolv $ get m "x" == get m "y")
eval s m (Primv "&") = (s, Boolv $ get m "x" == Boolv True && get m "y" == Boolv True)
eval s m (Primv "|") = (s, Boolv $ get m "x" == Boolv True || get m "y" == Boolv True)
eval s m (Primv "~") = (s, Boolv $ if get m "x" == Boolv True then False else True)
eval s m (Primv "zero?")  = (s, Boolv $ get m "x" == Numv 0)
eval s m (If c t e)       = if snd (eval s m c) == Boolv True then eval s m t else eval s m e
eval s m (SetRef n v)     = (setref s n' v', v')
  where n' = toRef $ snd $ eval s m n
        v' = snd $ eval s m v
eval s m (DeRef n)        = (s, deref s n')
  where n' = toRef $ snd $ eval s m n
eval s m (NewRef v)       = (s', n')
  where v' = snd $ eval s m v
        (s', n) = newref s v'
        n' = fromRef n
eval s m (Seq xs)         = foldl f (s, Numv 0) xs
  where f (s, _) x = eval s m x
eval s m (Assume bs x)    = eval s m' x
  where m' = Map.union mb m
        mb = elaborate s m bs
eval s m (Function fs b)  = (s, Procv m fs b)
eval s m (Recfun ps x) = eval s m' x
  where m' = Map.union mb m
        mb = recurse . elaborate s m . map f $ ps
        f (l, fs, b) = (l, Function fs b)
eval s m (Apply x ps)     = eval s m' b
  where m' = Map.union mf ml
        mf = elaborate s m $ zip fs ps
        (Procv ml fs b) = unrecurse $ snd $ eval s m x

unrecurse :: Value -> Value
unrecurse (Recuv m mb fs b) = Procv m' fs b
  where m' = Map.union (recurse mb) m
unrecurse v = v

recurse :: Env -> Env
recurse mb = Map.map f mb
  where f (Procv m fs b) = Recuv m mb fs b
        f x = x

elaborate :: Store -> Env -> [(Ast, Ast)] -> Env
elaborate s m =  Map.fromList . map f
  where f (Ida x, e) = (x, snd $ eval s m e)

setref :: Store -> Ref -> Value -> Store
setref s n v
  | n < l     = take n s ++ [v] ++ drop (n+1) s
  | otherwise = error $ printf "store:%d does not have address %d" l n
  where l = length s

deref :: Store -> Ref -> Value
deref s n
  | n < l     = s !! n
  | otherwise = error $ printf "store:%d does not have address %d" l n
  where l = length s

newref :: Store -> Value -> (Store, Ref)
newref s v = (s ++ [v], l)
  where l = length s

get :: Env -> String -> Value
get m id = case v of
    (Just x) -> x
    Nothing  -> error $ "id " ++ id ++ " not set!"
  where v = Map.lookup id m


parse :: String -> Ast
parse s = (read . unwords . unpack . alter . Bnode "" . pack . words $ bpad) :: Ast
  where bpad = replace "(" " ( " . replace ")" " ) " . replace "[" "(" . replace "]" ")". replace " . " " " $ s

alter :: Btree -> Btree
alter (Bnode _ (Bleaf "if":ns)) = (Bnode "(" (Bleaf "If":ns'))
  where ns' = map alter ns
alter (Bnode _ (Bleaf "setref":ns)) = (Bnode "(" (Bleaf "SetRef":ns'))
  where ns' = map alter ns
alter (Bnode _ (Bleaf "deref":ns)) = (Bnode "(" (Bleaf "DeRef":ns'))
  where ns' = map alter ns
alter (Bnode _ (Bleaf "newref":ns)) = (Bnode "(" (Bleaf "NewRef":ns'))
  where ns' = map alter ns
alter (Bnode _ (Bleaf "seq":Bnode _ xs:_)) = (Bnode "(" (Bleaf "Seq":Bnode "[" xs':[]))
  where xs' = intersperse c . map alter $ xs
        c = Bleaf ","
alter (Bnode _ (Bleaf "assume":Bnode _ bs:e)) = (Bnode "(" (Bleaf "Assume":Bnode "[" bs':e'))
  where e' = map alter e
        bs' = intersperse c . map pair $ bs
        pair (Bnode _ xv) = Bnode "(" . intersperse c . map alter $ xv
        c = Bleaf ","
alter (Bnode _ (Bleaf "function":Bnode _ fs:b)) = (Bnode "(" (Bleaf "Function":Bnode "[" fs':b'))
  where b' = map alter b
        fs' = intersperse c . map alter $ fs
        c = Bleaf ","
alter (Bnode _ (Bleaf "recfun":Bnode _ ps:e)) = (Bnode "(" (Bleaf "Recfun":Bnode "[" ps':e'))
  where e' = map alter e
        ps' = intersperse c . map proc $ ps
        proc (Bnode _ (l:Bnode _ fs:b)) = Bnode "(" . intersperse c $ l':(Bnode "[" fs'):b'
          where (l', b') = (alter l, map alter b)
                fs' = intersperse c . map alter $ fs
        c = Bleaf ","
alter (Bnode _ (Bleaf "@":e:ps)) = (Bnode "(" (Bleaf "Apply":e':ps'))
  where e' = alter e
        ps' = [Bnode "[" . intersperse c . map alter $ ps]
        c = Bleaf ","
alter (Bnode "(" ns) = alter $ Bnode "(" $ Bleaf "@":ns
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

toRef :: Value -> Ref
toRef (Numv n) = round n

fromRef :: Ref -> Value
fromRef n = Numv $ fromIntegral n

isFloat :: String -> Bool
isFloat s = case (reads s) :: [(Float, String)] of
  [(_, "")] -> True
  _         -> False

isBool :: String -> Bool
isBool s = case (reads s) :: [(Bool, String)] of
  [(_, "")] -> True
  _         -> False
