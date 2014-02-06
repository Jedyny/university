module Latte.Verifier (verify) where

import qualified Control.Monad.Writer as Writer (tell)
import Control.Monad.Writer hiding (tell)
import Control.Monad.State

import Data.List ((\\), find, intercalate, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MMap
import Data.Maybe (listToMaybe, mapMaybe)

import Latte.Parser
import Latte.ProgramTree

verify :: Module -> (Module, [VerificationError])
verify = runWriter . ($mkEnv) . evalStateT . verifyModule

{-- Verifier --} 
verifyModule :: Module -> Verifier Module
verifyModule (Module pos _ cs ms) = do
  putPredefFuns
  checkMain pos ms
  newCs <- mkCtxTree cs
  mapM_ putFun ms 
  mapM_ verifyClass newCs
  newCs <- mapM verifyMethods newCs
  newMs <- mapM verifyMethod ms
  strs <- getStrs
  return (Module pos (nub strs) newCs newMs)

verifyClass :: Clazz -> Verifier ()
verifyClass (Clazz pos n s _ fs ms) = inCtx n $ do
  mapM_ putField fs 
  mapM_ putFun ms
  
verifyMethods :: Clazz -> Verifier Clazz
verifyMethods (Clazz pos n b _ fs ms) = inCtx n $ do
  newMs <- mapM verifyMethod ms
  cSupers <- getSupers n
  return (Clazz pos n b cSupers fs newMs)
  
verifyMethod :: Method -> Verifier Method
verifyMethod (Method pos ret name args body) = inCtx (show pos) $ do
  checkRetType pos ret
  mapM_ (checkType pos) $ map fst args
  mapM_ (putArg pos) args
  mustReturn ret
  newBody <- mapM verifyStmt body
  checkReturn pos name
  return (Method pos ret name args newBody)

verifyStmt :: Stmt -> Verifier Stmt

verifyStmt (AssignS pos ns e) = do
  newE <- verifyExp e
  (varT, newNs) <- verifyVar pos ns
  checkCast pos (getType newE) varT
  return (AssignS pos newNs newE)
  
verifyStmt (BlockS pos ss) = inCtx (show pos) $ mapM verifyStmt ss >>= return . BlockS pos

verifyStmt (CondS pos cond ifB elseB) = do
  newCond <- verifyExp cond
  checkCast pos (getType newCond) BoolT
  newIf <- verifyStmt ifB
  ifReturned <- isReturned
  ignoreReturn
  case elseB of 
    Just x -> do
      newElse <- verifyStmt x
      elseReturned <- isReturned
      unless (ifReturned && elseReturned) ignoreReturn
      return (CondS pos newCond newIf $ Just newElse)
    Nothing -> return (CondS pos newCond newIf elseB)
    
verifyStmt (DecS pos t is) = checkType pos t >> mapM processItem is >>= return . DecS pos t 
  where 
    processItem i@(n, Nothing) = do
      putVar pos n t
      return i
    processItem (n, Just e) = do
      newE <- verifyExp e
      checkCast pos (getType newE) t
      putVar pos n t
      return (n, Just newE) 

verifyStmt (ExpS pos e) = verifyExp e >>= (return . ExpS pos)

verifyStmt (ForS pos t i arr body) = inCtx (show pos) $ do
  (varT, newArr) <- verifyVar pos arr
  checkType pos t
  isElemOf pos t varT
  putVar pos i t 
  newBody <- verifyStmt body
  ignoreReturn
  return (ForS pos t i newArr newBody)
  
verifyStmt (PredS pos ns) = do
  (varT, newNs) <- verifyVar pos ns
  checkCast pos varT IntT 
  return (PredS pos newNs) 

verifyStmt (SuccS pos ns) = do
  (varT, newNs) <- verifyVar pos ns
  checkCast pos varT IntT 
  return (SuccS pos newNs)

verifyStmt s@(ReturnS pos Nothing) = markReturn pos VoidT >> return s
verifyStmt (ReturnS pos (Just e)) = do 
  newExp <- verifyExp e 
  markReturn pos $ getType newExp
  return (ReturnS pos $ Just newExp)
    
verifyStmt (WhileS pos cond body) = do
  newCond <- verifyExp cond
  checkCast pos (getType newCond) BoolT
  newBody <- verifyStmt body
  ignoreReturn 
  return (WhileS pos newCond newBody) 
  
verifyStmt (EmptyS) = return EmptyS

verifyExp :: Exp -> Verifier Exp
verifyExp (VarE _ pos ns) = do
  (varT, newNs) <- verifyVar pos ns
  return (VarE varT pos newNs)
verifyExp e@(IntE pos n) = return e
verifyExp e@(BoolE pos b) = return e
verifyExp e@(StringE pos s) = addStr s >> return e
verifyExp e@(NullE pos) = return e
verifyExp (UnaryE _ pos op e) = do
  newE <- verifyExp e
  opT <- getUnary pos op (getType newE)
  return (UnaryE opT pos op newE)
verifyExp (BinaryE _ pos op lhs rhs) = do
  newLhs <- verifyExp lhs
  newRhs <- verifyExp rhs
  opT <- getBinary pos op (getType newLhs) (getType newRhs)
  return (BinaryE opT pos op newLhs newRhs)
verifyExp (CallE _ pos ns as) = do
  newAs <- mapM verifyExp as
  (funT, newNs) <- verifyFun pos ns $ map getType newAs
  return (CallE funT pos newNs newAs)
verifyExp (CastE pos t e) = do
  newE <- verifyExp e
  checkType pos t
  checkCast pos (getType newE) t
  return (CastE pos t newE)
verifyExp (NewArrE pos t len) = do
  checkType pos t
  newLen <- verifyExp len 
  return (NewArrE pos t newLen)
verifyExp e@(NewObjE pos t) = do
  checkType pos t
  return e

verifyVar :: SourcePos -> [(String, Maybe Exp)] -> Verifier (Type, [(String, Maybe Exp)])
verifyVar pos ns = do
    newNs <- mapM (checkName pos) ns
    t <- getVar pos ns
    return (t, newNs)
  
verifyFun :: SourcePos -> [(String, Maybe Exp)] -> [Type] -> Verifier (Type, [(String, Maybe Exp)])  
verifyFun pos ns as = do
    newNs <- mapM (checkName pos) ns
    t <- getFun pos ns as
    return (t, newNs)
    
checkName pos (s, Just e) = do
  newE <- verifyExp e
  checkCast pos (getType newE) IntT
  return (s, Just newE)
checkName pos n = return n

type VarMap = Map String Type
type FunMap = Map (String, [Type]) Type

data Ctx = Ctx { 
  ctxVars :: VarMap, 
  ctxFuns :: FunMap 
}

mkCtx :: Ctx
mkCtx = Ctx Map.empty Map.empty

mkClassCtx :: String -> Ctx
mkClassCtx name = Ctx (Map.singleton "self" $ ClassT name) Map.empty

getCtxVar :: String -> Ctx -> Maybe Type
getCtxVar name = Map.lookup name . ctxVars 

getCtxFun :: String -> [Type] -> Ctx -> Maybe Type
getCtxFun name args = Map.lookup (name, args) . ctxFuns

memberCtxVar :: String -> Ctx -> Bool
memberCtxVar name = Map.member name . ctxVars

memberCtxFun :: String -> [Type] -> Ctx -> Bool
memberCtxFun name args = Map.member (name, args) . ctxFuns

putCtxVar :: String -> Type -> Ctx -> Ctx
putCtxVar name clazz ctx@(Ctx vars _) = ctx { ctxVars = Map.insert name clazz vars }

putCtxFun :: String -> [Type] -> Type -> Ctx -> Ctx
putCtxFun name args ret ctx@(Ctx _ funs) = ctx { ctxFuns = Map.insert (name, args) ret funs }

instance Show Ctx where
  show (Ctx vs fs) = (concatMap showVar $ Map.toList vs) ++ (concatMap showFun $ Map.toList fs)
    where
      showVar (n, t) = "  " ++ show t ++ " " ++ show n ++ "\n" 
      showFun ((n, as), t) = "  " ++ show t ++ " " ++ show n ++ show as ++ "\n"

----------
data Env = Env {
  envSupers :: Map String [String],
  envCtxs :: Map String Ctx,
  envCurrCtx :: String,
  envRet :: (Type, Bool),
  envStrs :: [String]
}

mkEnv :: Env
mkEnv = Env (Map.singleton "" []) (Map.singleton "" mkCtx) "" (AnyT, False) []

supers :: String -> Env -> [String]
supers name env = case Map.lookup name $ envSupers env of
  Just xs -> xs
  _ -> error $ "Latte.Verifier.supers: given context " ++ name ++ " does not exist\n" ++ show env

context :: String -> Env -> Ctx
context name env = case Map.lookup name $ envCtxs env of
  Just ctx -> ctx
  _ -> error $ "Latte.Verifier.context: given context " ++ name ++ " does not exist\n" ++ show env
  
superCtxs :: String -> Env -> [Ctx]
superCtxs name env = map (flip context env) $ supers name env
  
currentCtx :: Env -> Ctx
currentCtx env = context (envCurrCtx env) env

getEnvVar_ :: String -> String -> Env -> Maybe Type
getEnvVar_ name ctx env = listToMaybe $ mapMaybe (getCtxVar name) ctxs 
  where ctxs = (context ctx env) : (superCtxs ctx env)
  
getEnvVar :: [(String, Maybe Exp)] -> Env -> Maybe Type
getEnvVar [] _ = error "Latte.Verifier.getEnvVar: empty name list"
getEnvVar names env = foldM go (mkType $ envCurrCtx env) names
  where
    go (ArrayT t) ("length", Nothing) = Just IntT
    go (ClassT s) (e, Nothing) = getEnvVar_ e s env
    go (ClassT s) (e, Just _) = getEnvVar_ e s env >>= fromArr
    go _ _ = Nothing
    fromArr (ArrayT t) = Just t
    fromArr _ = Nothing

getEnvFun_ :: String -> [Type] -> String -> Env -> Maybe Type
getEnvFun_ name args ctx env = listToMaybe $ mapMaybe (getCtxFun name args) ctxs
  where ctxs = (context ctx env) : (superCtxs ctx env)
  
getEnvFun :: [(String, Maybe Exp)] -> [Type] -> Env -> Maybe Type
getEnvFun [] _ _ = error "Latte.Verifier.getEnvFun: empty name list"
getEnvFun [(name, Nothing)] args env = getEnvFun_ name args (envCurrCtx env) env
getEnvFun [(name, _)] _ _ = Nothing
getEnvFun names args env = do
  funCtx <- getEnvVar (init names) env
  case snd fnName of
    Nothing -> getEnvFun_ (fst fnName) args (show funCtx) env
    _ -> Nothing
  where fnName = last names
  
putEnvVar :: String -> Type -> Env -> Either String Env
putEnvVar name clazz env 
  | memberCtxVar name ctx = Left $ envCurrCtx env 
  | otherwise = Right $ putVar_ name clazz env
  where ctx = currentCtx env

putVar_ :: String -> Type -> Env -> Env
putVar_ name clazz env = 
  env { envCtxs = Map.adjust (putCtxVar name clazz) (envCurrCtx env) $ envCtxs env }

putEnvFun :: String -> [Type] -> Type -> Env -> Either String Env
putEnvFun name args ret env = do
  when (memberCtxFun name args $ currentCtx env) $ Left $ envCurrCtx env
  case find invalidRet $ supers (envCurrCtx env) env of
      Just ctx -> Left ctx
      _ -> Right $ putFun_ name args ret env 
  where 
    invalidRet n = (getCtxFun name args $ context n env) `notElem` [Nothing, Just ret]

putFun_ :: String -> [Type] -> Type -> Env -> Env
putFun_ name args ret env = 
  env { envCtxs = Map.adjust (putCtxFun name args ret) (envCurrCtx env) $ envCtxs env }

putEnvCtx :: String -> String -> Env -> Env
putEnvCtx super name env = env { 
  envCtxs = Map.insert name mkCtx $ envCtxs env,
  envSupers = Map.insert name (super : (supers super env)) $ envSupers env
} 

putEnvClassCtx :: String -> String -> Env -> Env
putEnvClassCtx super name env = env { 
  envCtxs = Map.insert name (mkClassCtx name) $ envCtxs env,
  envSupers = Map.insert name (super : (supers super env)) $ envSupers env
} 

enterCtx :: String -> Env -> Env
enterCtx ctx env@(Env _ ctxs curr _ _) = newEnv { envCurrCtx = ctx }
  where newEnv = if Map.member ctx ctxs then env else putEnvCtx curr ctx env

exitCtx :: Env -> Env
exitCtx env@(Env _ _ ctx _ _) = env { envCurrCtx = if null ss then "" else head ss } 
  where ss = supers ctx env

setEnvRet :: Type -> Bool -> Env -> Env
setEnvRet t b env = env { envRet = (t, b) }

putEnvStr :: String -> Env -> Env
putEnvStr s env = env { envStrs = s : (envStrs env) }

instance Show Env where
  show (Env supers ctxs curr ret strs) = "Current: " ++ showName curr ++ "; return: " ++ (show ret) ++ "\n"
    ++ (concatMap showSuper $ Map.toList supers)
    ++ (concatMap showCtx $ Map.toList ctxs)
    ++ show strs
    where
      showSuper (n, ss) = "super " ++ showName n ++ ": " ++ show ss ++ "\n"
      showCtx (n, c) = "def " ++ showName n ++ ":\n" ++ (show c) ++ "\n"
      showName "" = "__global__"
      showName n = n

----------
getVar :: SourcePos -> [(String, Maybe Exp)] -> Verifier Type
getVar pos names = do
  maybeClazz <- gets $ getEnvVar names
  case maybeClazz of
    Just x -> return x
    _ -> report pos (notVariableErr $ intercalate "." $ map prettyName names) >> return AnyT
  where 

getFun :: SourcePos -> [(String, Maybe Exp)] -> [Type] -> Verifier Type
getFun pos names args = do
  maybeClazz <- gets (getEnvFun names args)
  case maybeClazz of
    Just x -> return x
    _ -> report pos (functionUndefinedErr prettyFn) >> return AnyT
  where 
    prettyFn = (intercalate "." $ map prettyName names) ++ "(" ++ (intercalate "," $ map show args) ++ ")"
    
prettyName (s, x) = s ++ (maybe "" (\_ -> "[]") x)

putVar :: SourcePos -> String -> Type -> Verifier ()
putVar pos name clz = do
  errOrEnv <- gets $ putEnvVar name clz
  case errOrEnv of
    Right x -> put x
    Left ctx -> report pos $ duplicateVariableErr name

putField :: Field -> Verifier ()
putField (Field pos clz name) = putVar pos name clz

putArg :: SourcePos -> (Type, String) -> Verifier ()
putArg pos (clz, name) = putVar pos name clz

putFun :: Method -> Verifier ()
putFun (Method pos ret name args _) = do
  envOrErr <- gets $ putEnvFun name (map fst args) ret
  ctx <- gets envCurrCtx
  case envOrErr of
    Right x -> put x
    Left errCtx -> case (errCtx == ctx, ctx) of
      (True, "") -> report pos $ duplicateFunErr prettyFn
      (True, clz) -> report pos $ duplicateMethodErr prettyFn clz
      (False, "") -> report pos $ incompatibleReturnErr prettyFn
      (False, clz) -> report pos $ incompatibleReturnErr $ clz ++ "." ++ prettyFn
  where prettyFn = name ++ "(" ++ (intercalate "," $ map show args) ++ ")"
  
putPredefFuns :: Verifier () 
putPredefFuns = do
  modify $ putFun_ "error" [] VoidT
  modify $ putFun_ "readInt" [] IntT
  modify $ putFun_ "readString" [] StringT
  modify $ putFun_ "printInt" [IntT] VoidT
  modify $ putFun_ "printString" [StringT] VoidT

getUnary :: SourcePos -> String -> Type -> Verifier Type
getUnary _ "-" IntT = return IntT
getUnary _ "!" BoolT = return BoolT
getUnary pos op t = (report pos $ unknownUnaryErr op $ show t) >> return AnyT 

getBinary :: SourcePos -> String -> Type -> Type -> Verifier Type
getBinary _ "+" IntT IntT = return IntT
getBinary _ "+" StringT StringT = return StringT
getBinary _ "-" IntT IntT = return IntT
getBinary _ "*" IntT IntT = return IntT
getBinary _ "/" IntT IntT = return IntT
getBinary _ "%" IntT IntT = return IntT
getBinary _ "<" IntT IntT = return BoolT
getBinary _ "<=" IntT IntT = return BoolT
getBinary _ ">=" IntT IntT = return BoolT
getBinary _ ">" IntT IntT = return BoolT
getBinary pos "==" t1 t2
  | t1 == t2 = return BoolT
  | otherwise = (report pos $ unknownBinaryErr "==" (show t1) (show t2)) >> return AnyT
getBinary pos "!=" t1 t2
  | t1 == t2 = return BoolT
  | otherwise = (report pos $ unknownBinaryErr "!=" (show t1) (show t2)) >> return AnyT
getBinary _ "&&" BoolT BoolT = return BoolT
getBinary _ "||" BoolT BoolT = return BoolT
getBinary pos op lhs rhs = (report pos $ unknownBinaryErr op (show lhs) (show rhs)) >> return AnyT
 
inCtx :: String -> Verifier a -> Verifier a
inCtx ctx m = do
  modify $ enterCtx ctx
  result <- m
  modify exitCtx
  return result

checkMain :: SourcePos -> [Method] -> Verifier () 
checkMain pos ms = when (null $ filter isMain ms) $ report pos noMainErr

mkCtxTree :: [Clazz] -> Verifier [Clazz]
mkCtxTree cs = checkSupers cs >>= checkDuplicates >>= checkClassCycles

checkSupers :: [Clazz] -> Verifier [Clazz]
checkSupers cs = filterM hasValidSuper cs
  where 
    hasValidSuper (Clazz pos n s _ _ _) 
      | (s `elem`) $ ("":) $ map (\(Clazz _ n _ _ _ _) -> n) cs = return True 
      | otherwise = (report pos $ badSuperErr s) >> return False 
      
checkDuplicates :: [Clazz] -> Verifier [Clazz]
checkDuplicates cs = foldM go [] cs
  where
    go cs c@(Clazz pos n _ _ _ _) 
      | any (\(Clazz _ n' _ _ _ _) -> n' == n) cs = (report pos $ duplicateTypeErr n) >> return cs 
      | otherwise = return (c:cs)
  
checkClassCycles :: [Clazz] -> Verifier [Clazz]
checkClassCycles cs = run >> filterM hasValidHierarchy cs
  where
    go [] m = return ()
    go (c:cs) m = do
      vs <- gets (Map.keys . envCtxs)
      let neighbors = (MMap.lookup c m) \\ vs
      mapM_ (modify . putEnvClassCtx c) neighbors
      go (cs ++ neighbors) m
    run = go [""] $ MMap.fromList $ map (\(Clazz _ n s _ _ _) -> (s, n)) cs
    hasValidHierarchy (Clazz pos n _ _ _ _) = do
      visited <- gets envCtxs
      case (Map.member n visited) of
        True -> return True
        False -> (report pos $ badHierarchyErr n) >> return False

getSupers :: String -> Verifier [String]
getSupers = gets . supers

checkType_ :: [Type] -> SourcePos -> Type -> Verifier ()
checkType_ _ _ AnyT = return ()
checkType_ prim pos (ArrayT t) = checkType_ prim pos t
checkType_ _ pos (ClassT s) = do
  ctxMap <- gets envCtxs
  unless (Map.member s ctxMap) $ report pos $ unknownTypeErr s  
checkType_ prim pos t = unless (t `elem` prim) $ report pos $ unknownTypeErr $ show t  

checkType, checkRetType :: SourcePos -> Type -> Verifier ()
checkType = checkType_ [BoolT, IntT, StringT]
checkRetType = checkType_ [BoolT, IntT, StringT, VoidT]
  
isElemOf :: SourcePos -> Type -> Type -> Verifier ()
isElemOf _ _ AnyT = return ()
isElemOf pos t1 (ArrayT t2) = checkCast pos t2 t1
isElemOf pos t1 t2 = report pos badIterableErr

getElemOf :: SourcePos -> Type -> Verifier Type
getElemOf _ AnyT = return AnyT
getElemOf _ (ArrayT t) = return t
getElemOf pos t = (report pos badArrayErr) >> return AnyT
  
checkCast :: SourcePos -> Type -> Type -> Verifier ()
checkCast _ AnyT _ = return ()
checkCast _ _ AnyT = return ()
checkCast _ NullT (ClassT _) = return ()
checkCast pos t1@(ClassT c1) t2@(ClassT c2) = do
  bases <- gets $ supers c1 
  unless (c1 == c2 || c2 `elem` bases) $ report pos $ cannotConvertErr c1 c2 
checkCast pos t1 t2
  | t1 == t2 = return ()
  | otherwise = report pos $ cannotConvertErr (show t1) (show t2)

mustReturn :: Type -> Verifier ()
mustReturn ret = modify $ setEnvRet ret False 

ignoreReturn :: Verifier ()
ignoreReturn = do
  (t, _) <- gets envRet
  modify $ setEnvRet t False
  
markReturn :: SourcePos -> Type -> Verifier ()
markReturn pos ret = do
  (t, _) <- gets envRet
  checkCast pos ret t
  modify $ setEnvRet t True

checkReturn :: SourcePos -> String -> Verifier ()
checkReturn pos name = do
  (t, b) <- gets envRet
  unless (b || t == VoidT) $ report pos $ noReturnErr name $ show t
  
isReturned :: Verifier Bool
isReturned = gets (snd . envRet)

addStr :: String -> Verifier ()
addStr = modify . putEnvStr

getStrs :: Verifier [String]
getStrs = gets envStrs

{-- Errors --}

data VerificationError = VerificationError SourcePos String deriving (Eq, Ord)

instance Show VerificationError where
  show (VerificationError pos msg) = show pos ++ " " ++ msg

badArrayErr :: String
badArrayErr = "error: not an array"

badHierarchyErr :: String -> String
badHierarchyErr = format "error: the hierarchy of the type " " is inconsistent"

badIterableErr :: String
badIterableErr = "error: can only iterate over array"

badSuperErr :: String -> String
badSuperErr = format "error: cannot extend " ""

cannotConvertErr :: String -> String -> String
cannotConvertErr = format2 "error: cannot convert from " " to " ""

duplicateFieldErr :: String -> String
duplicateFieldErr = format "error: duplicate field " ""

duplicateFunErr :: String -> String
duplicateFunErr = format "error: duplicate function " ""

duplicateMethodErr :: String -> String -> String
duplicateMethodErr = format2 "error: duplicate method " " in type " ""

duplicateTypeErr :: String -> String
duplicateTypeErr = format "error: the type " " is already defined"

duplicateVariableErr :: String -> String
duplicateVariableErr = format "error: duplicate variable " ""

incompatibleReturnErr :: String -> String
incompatibleReturnErr = format "error: the return type is incompatible with " ""

functionUndefinedErr :: String -> String
functionUndefinedErr = format "error: no match for function " "" 

noMainErr :: String
noMainErr = "error: could not find main method"

noReturnErr :: String -> String -> String
noReturnErr = format2 "error: the method " " must return a result of type " ""

notVariableErr :: String -> String
notVariableErr = format "error: not a variable " ""

unknownBinaryErr :: String -> String -> String -> String
unknownBinaryErr = format3 "error: operator " " with arguments " ", " " does not exist"

unknownTypeErr :: String -> String
unknownTypeErr = format "error: " " cannot be resolved to a type"

unknownUnaryErr :: String -> String -> String
unknownUnaryErr = format2 "error: operator " " with arguments " " does not exist" 

format :: String -> String -> String -> String
format s1 s2 a = s1 ++ a ++ s2

format2 :: String -> String -> String -> String -> String -> String
format2 s1 s2 s3 a1 a2 = s1 ++ a1 ++  s2 ++  a2 ++  s3   

format3 :: String -> String -> String -> String -> String -> String -> String -> String
format3 s1 s2 s3 s4 a1 a2 a3 = s1 ++ a1 ++ s2 ++ a2 ++ s3 ++ a3 ++ s4

{-- Utils --}

type Verifier a = StateT Env (Writer [VerificationError]) a

report :: SourcePos -> String -> Verifier ()
report pos msg = lift $ Writer.tell [VerificationError pos msg]
