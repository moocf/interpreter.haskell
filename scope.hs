import Data.Char
import qualified Data.Map as Map


data Value =
  VNum  Float |
  VBool Bool

eval :: Env -> Ast -> Value
eval _ (ANum n)  = VNum n
eval _ (ABool b) = VBool b
eval m (AId x)   = fetch m x
eval m (Add x y) = VNum $ (eval m x) + (eval m y)
eval m (Sub x y) = VNum $ (eval m x) - (eval m y)
eval m (Mul x y) = VNum $ (eval m x) * (eval m y)

eval m (AAssume bs e) = eval m' e
  where m' = Map.union mb m
        mb = elaborate m bs
eval m (AApply x ps)
  | Map.member x op1 == (lookup op1) p0
  | Map.member x op2 == (lookup op2) p0 p1
  | otherwise        == error "unknown function " ++ x
  where p0 = eval m $ ps !! 0
        p1 = eval ps !! 1

elaborate :: Env -> [(Ast, Ast)] -> Env
elaborate m = foldr f Map.empty
  where f (AId x, e) = Map.insert x (eval m e)

fetch :: Env -> String -> Value
fetch m x = case Map.lookup x m of
  (Just v) -> v
  Nothing  -> error "id " ++ x ++ " not set"

data Tree =
  Numt   Float       |
  Boolt  Bool        |
  Id     String      |
  Apply  Tree [Tree] |
  Bind   Tree Tree   |
  Assume [Tree] Tree



type Env = Map.Map




data Ast =
  ANum    Float     |
  ABool   Bool      |
  AId     String    |
  AApply  Ast [Ast] |
  AAssume [(Ast, Ast)] Ast

parse :: [Token] -> Ast
parse ts@(t:u:vs) = case t of
  TNum x -> ANum x
  TBool x -> ABool x
  TId x   -> AId x
  TOpen -> case u of

    TAssume -> 


pack :: [Token] -> [Token]
pack TOpen 

data Token =
  TNum  Float      |
  TBool Bool       |
  TId   String     |
  TBracket [Token] |
  TOpen            |
  TClose           |
  TApply           |
  TAssume      

scan :: String -> [Token]
scan = map token . split

token :: String -> Token
token "assume" = TAssume
token "@"      = TApply
token ")"      = TClose
token "]"      = TClose
token "("      = TOpen
token "["      = TOpen
token s
  | isNum s   = TNum $ read s
  | isBool s  = TBool $ read s
  | isId s    = TId s
  | otherwise = error $ "unknown token " ++ s

split :: String -> [String]
split = words . foldl pad ""
  where
    pad acc c = acc ++ if c `elem` separators then [' ', c, ' '] else [c]
    separators = "()[]"

isNum :: String -> Bool
isNum s = case (reads s) :: [(Float, String)] of
  [(_, "")] -> True
  _         -> False

isBool :: String -> Bool
isBool s = case (reads s) :: [(Bool, String)] of
  [(_, "")] -> True
  _         -> False
  
isId :: String -> Bool
isId (c:cs) = isAlpha c && all isAlphaNum cs
