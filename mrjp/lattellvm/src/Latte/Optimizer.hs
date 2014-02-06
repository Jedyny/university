module Latte.Optimizer (optimize, mkIfElse) where

import qualified Control.Monad.Writer as Writer (tell)
import Control.Monad.Writer hiding (tell)
import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set

import Latte.ProgramTree

shouldOptimize :: Bool
shouldOptimize = True

optimize :: Module -> (Module, [OptimizerWarning])
optimize m = if shouldOptimize then runWriter $ evalStateT (optimizeModule m) mkEnv else (m, [])

optimizeModule :: Module -> Optimizer Module
optimizeModule m@(Module pos strs cs ms) = do
  setStrs strs
  newCs <- mapM optimizeClass cs
  newMs <- mapM optimizeMethod ms
  newStrs <- getStrs
  removeDeadCode $ Module pos newStrs newCs newMs
  
optimizeClass (Clazz pos n s ss fs ms) = do
  newMs <- mapM optimizeMethod ms
  return $ Clazz pos n s ss fs newMs
  
optimizeMethod (Method pos ret n as ss) = do
  newSs <- mapM optimizeStmt ss
  return $ Method pos ret n as newSs

optimizeStmt :: Stmt -> Optimizer Stmt
optimizeStmt (AssignS pos v e) = calculateExp e >>= (\x -> return $ AssignS pos v x)
optimizeStmt (BlockS pos ss) = mapM optimizeStmt ss >>= (\x -> return $ BlockS pos x)
optimizeStmt (CondS pos cond ifB mElseB) = do
  newCond <- calculateExp cond
  case (newCond, mElseB) of
    (BoolE _ True, _) -> optimizeStmt ifB 
    (BoolE _ False, Nothing) -> return EmptyS
    (BoolE _ False, Just s) -> optimizeStmt s
    (_, Nothing) -> optimizeStmt ifB >>= (\x -> return $ CondS pos newCond x Nothing)
    (_, Just s) -> do
      newIf <- optimizeStmt ifB
      newElse <- optimizeStmt s
      return $ CondS pos newCond newIf $ Just newElse
optimizeStmt (DecS pos t is) = mapM optimizeItem is >>= (\x -> return $ DecS pos t x)
  where 
    optimizeItem (n, Nothing) = return (n, Nothing)
    optimizeItem (n, Just e) = calculateExp e >>= (\x -> return $ (n, Just x))
optimizeStmt (ExpS pos e) = calculateExp e >>= (\x -> return $ ExpS pos x)
optimizeStmt (ForS pos t n v s) = optimizeStmt s >>= (\x -> return $ ForS pos t n v x)
optimizeStmt (ReturnS pos (Just e)) = calculateExp e >>= (\x -> return $ ReturnS pos $ Just x)
optimizeStmt (WhileS pos e s) = do
  newE <- calculateExp e
  case newE of
    (BoolE _ False) -> return EmptyS
    _ -> optimizeStmt s >>= (\x -> return $ WhileS pos newE x)
optimizeStmt s = return s
      
mkIfElse :: SourcePos -> Exp -> Stmt -> Maybe Stmt -> Stmt
mkIfElse _ (BoolE _ True) s _ = s
mkIfElse _ (BoolE _ False) _ (Just s) = s 
mkIfElse _ (BoolE _ False) _ Nothing = EmptyS
mkIfElse p e s s' = CondS p e s s'  


calculateExp :: Exp -> Optimizer Exp
calculateExp (UnaryE t pos "-" e) = do
  newE <- calculateExp e
  case newE of
    (IntE _ n) -> return $ IntE pos $ negate n
    _ -> return $ UnaryE t pos "-" newE
calculateExp (UnaryE t pos "!" e) = do
  newE <- calculateExp e
  case newE of
    (BoolE _ b) -> return $ BoolE pos $ not b
    _ -> return $ UnaryE t pos "!" newE
calculateExp (BinaryE StringT pos "+" lhs rhs) = do
  newLhs <- calculateExp lhs
  newRhs <- calculateExp rhs
  case (newLhs, newRhs) of
    ((StringE _ s1), (StringE _ s2)) -> concatStrs s1 s2 >> (return $ StringE pos $ s1 ++ s2)
    _ -> return $ BinaryE StringT pos "+" newLhs newRhs
calculateExp e@(BinaryE IntT _ "+" _ _) = calculateArith e (+) IntE
calculateExp e@(BinaryE _ _ "-" _ _) = calculateArith e (-) IntE
calculateExp e@(BinaryE _ _ "*" _ _) = calculateArith e (*) IntE
calculateExp e@(BinaryE _ _ "/" _ _) = calculateArith0 e div
calculateExp e@(BinaryE _ _ "%" _ _) = calculateArith0 e mod
calculateExp e@(BinaryE _ _ "<" _ _) = calculateArith e (<) BoolE
calculateExp e@(BinaryE _ _ "<=" _ _) = calculateArith e (<=) BoolE
calculateExp e@(BinaryE _ _ ">=" _ _) = calculateArith e (>=) BoolE
calculateExp e@(BinaryE _ _ ">" _ _) = calculateArith e (>) BoolE
calculateExp e@(BinaryE t pos "==" lhs rhs) = do
  newLhs <- calculateExp lhs
  newRhs <- calculateExp rhs 
  case (newLhs, newRhs) of
    ((BoolE _ b1), (BoolE _ b2)) -> return $ BoolE pos $ b1 == b2
    ((IntE _ n1), (IntE _ n2)) -> return $ BoolE pos $ n1 == n2
    ((StringE _ s1), (StringE _ s2)) -> return $ BoolE pos $ s1 == s2
    _ -> return $ BinaryE t pos "==" newLhs newRhs
calculateExp e@(BinaryE t pos "!=" lhs rhs) = do
  newLhs <- calculateExp lhs
  newRhs <- calculateExp rhs 
  case (newLhs, newRhs) of
    ((BoolE _ b1), (BoolE _ b2)) -> return $ BoolE pos $ b1 /= b2
    ((IntE _ n1), (IntE _ n2)) -> return $ BoolE pos $ n1 /= n2
    ((StringE _ s1), (StringE _ s2)) -> return $ BoolE pos $ s1 /= s2
    _ -> return $ BinaryE t pos "!=" newLhs newRhs
calculateExp e@(BinaryE t pos "&&" lhs rhs) = do
  newLhs <- calculateExp lhs
  newRhs <- calculateExp rhs
  case (newLhs, newRhs) of
    ((BoolE _ False), _) -> return $ BoolE pos False
    ((BoolE _ True), (BoolE _ True)) -> return $ BoolE pos True
    _ -> return $ BinaryE t pos "&&" newLhs newRhs  
calculateExp e@(BinaryE t pos "||" lhs rhs) = do
  newLhs <- calculateExp lhs
  newRhs <- calculateExp rhs
  case (newLhs, newRhs) of
    ((BoolE _ True), _) -> return $ BoolE pos True
    ((BoolE _ False), (BoolE _ False)) -> return $ BoolE pos False
    _ -> return $ BinaryE t pos "||" newLhs newRhs  
calculateExp (CallE t pos v es) = do
  newEs <- mapM calculateExp es
  return $ CallE t pos v newEs
calculateExp (CastE pos t e) = do
  newE <- calculateExp e
  return $ CastE pos t newE
calculateExp (NewArrE pos t e) = do
  newE <- calculateExp e
  return $ NewArrE pos t newE
calculateExp e = return e
    
calculateArith :: Exp -> (Int32 -> Int32 -> a) -> (SourcePos -> a -> Exp) -> Optimizer Exp
calculateArith (BinaryE t pos op lhs rhs) f ctor = do
  newLhs <- calculateExp lhs
  newRhs <- calculateExp rhs 
  case (newLhs, newRhs) of
    ((IntE _ n1), (IntE _ n2)) -> return $ ctor pos $ f n1 n2
    _ -> return $ BinaryE t pos op newLhs newRhs
    
calculateArith0 :: Exp -> (Int32 -> Int32 -> Int32) -> Optimizer Exp
calculateArith0 (BinaryE IntT pos op lhs rhs) f = do
  newLhs <- calculateExp lhs
  newRhs <- calculateExp rhs 
  case (newLhs, newRhs) of
    ((IntE _ n1), (IntE _ 0)) -> warn pos divideByZero >> (return $ BinaryE IntT pos op newLhs newRhs)
    ((IntE _ n1), (IntE _ n2)) -> return $ IntE pos $ f n1 n2
    _ -> return $ BinaryE IntT pos op newLhs newRhs   
    
---- remove dead code

removeDeadCode :: Module -> Optimizer Module
removeDeadCode (Module pos strs cs ms) = do
  newCs <- mapM removeDeadCodeClass cs
  newMs <- mapM removeDeadCodeMethod ms
  return $ Module pos strs newCs newMs

removeDeadCodeClass :: Clazz -> Optimizer Clazz
removeDeadCodeClass (Clazz pos n s ss fs ms) = do
  newMs <- mapM removeDeadCodeMethod ms
  return $ Clazz pos n s ss fs newMs
  
removeDeadCodeMethod :: Method -> Optimizer Method
removeDeadCodeMethod (Method pos ret n as ss) = do
  newSs <- removeDeadCodeStmts ss
  return $ Method pos ret n as newSs
    
removeDeadCodeBlock :: Stmt -> Optimizer Stmt
removeDeadCodeBlock (BlockS pos body) = removeDeadCodeStmts body >>= (\x -> return $ BlockS pos x)
removeDeadCodeBlock (CondS pos cond ifB Nothing) = do
  newIf <- removeDeadCodeBlock ifB
  return $ CondS pos cond newIf Nothing
removeDeadCodeBlock (CondS pos cond ifB (Just elseB)) = do
  newIf <- removeDeadCodeBlock ifB
  newElse <- removeDeadCodeBlock elseB
  return $ CondS pos cond newIf $ Just newElse
removeDeadCodeBlock (ForS pos t n v s) = removeDeadCodeBlock s >>= (\x -> return $ ForS pos t n v x)     
removeDeadCodeBlock (WhileS pos c s) = removeDeadCodeBlock s >>= (\x -> return $ WhileS pos c x)
removeDeadCodeBlock s = return s

removeDeadCodeStmts :: [Stmt] -> Optimizer [Stmt]
removeDeadCodeStmts ss = go ss []    
  where
    go [] xs = return $ reverse xs
    go (EmptyS:ss) xs = go ss xs
    go (s:ss) xs@((ReturnS _ _):_) = warn (getPosS s) unreachableCode >> go [] xs  
    go (s:ss) xs@((WhileS _ (BoolE _ True) _):_) = warn (getPosS s) unreachableCode >> go [] xs 
    go (s:ss) xs = go ss (s:xs)
    
{-- Environment --}

data Env = Env {
  envStrs :: Set String
}

mkEnv :: Env
mkEnv = Env Set.empty

env_setStrs :: [String] -> Env -> Env
env_setStrs strs env = env { envStrs = Set.fromList strs }

env_putStr :: String -> Env -> Env
env_putStr str env = env { envStrs = Set.insert str $ envStrs env }

env_removeStr :: String -> Env -> Env
env_removeStr str env = env { envStrs = Set.delete str $ envStrs env }
  
{- Optimizer -}  
  
setStrs :: [String] -> Optimizer ()
setStrs = modify . env_setStrs  
  
concatStrs :: String -> String -> Optimizer ()
concatStrs s1 s2 = do
  modify $ env_removeStr s1
  modify $ env_removeStr s2
  modify $ env_putStr (s1 ++ s2)
  
getStrs :: Optimizer [String]
getStrs = gets (Set.toList . envStrs)
    
{-- Messages --}

data OptimizerWarning = OptimizerWarning SourcePos String deriving (Eq, Ord)

instance Show OptimizerWarning where
  show (OptimizerWarning pos msg) = show pos ++ " " ++ msg
  
divideByZero :: String
divideByZero = "warning: divide by zero"

unreachableCode :: String
unreachableCode = "warning: unreachable code"

{- Utils -}

type Optimizer a = StateT Env (Writer [OptimizerWarning]) a

warn :: SourcePos -> String -> Optimizer ()
warn pos msg = lift $ Writer.tell [OptimizerWarning pos msg]
