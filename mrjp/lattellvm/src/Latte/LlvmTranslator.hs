module Latte.LlvmTranslator (toLlvm, toLlvmToFile) where

import qualified Control.Monad.Writer as Writer (tell)
import Control.Monad.Writer hiding (tell)
import Control.Monad.State

import Data.List (find, foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe, mapMaybe)

import System.IO (writeFile)

import Latte.ProgramTree

toLlvmToFile :: Module -> FilePath -> IO ()
toLlvmToFile m f = writeFile f (toLlvm m)

toLlvm :: Module -> String
toLlvm = execWriter . ($mkEnv) . evalStateT . moduleToLlvm

moduleToLlvm :: Module -> Translator ()
moduleToLlvm (Module _ strs cs ms) = do
  putLib
  putStrs strs
  genClasses cs
  mapM_ classToLlvm cs
  mapM_ methodToLlvm ms
  
classToLlvm :: Clazz -> Translator ()  
classToLlvm (Clazz _ name super supers fs ms) = mapM_ (classMethodToLlvm name) ms

classMethodToLlvm :: String -> Method -> Translator ()
classMethodToLlvm clazz (Method pos ret name args body) =
  methodToLlvm (Method pos ret (clazz ++ "__" ++ name) ((ClassT clazz, "self"):args) body)

methodToLlvm :: Method -> Translator ()
methodToLlvm (Method _ rt name args body) = inMethod $ do
  tell $ "define " ++ (typeToLlvm rt) ++ " @" ++ name ++ "(" ++ (intercalate ", " argsL) ++ ") {\n"
  unless (rt == VoidT) $ newReg >> alloca (typeToLlvm rt) "%1"
  retLabel <- newLabel "ret"
  setRetLabel retLabel 
  mapM_ regArg args
  mapM_ stmtToLlvm body
  goto retLabel
  label retLabel
  case rt of
    VoidT -> ret "void" ""
    t -> do
      loadReg <- load (ptr $ typeToLlvm t) "%1"
      ret (typeToLlvm t) loadReg
  tell $ "}\n\n"
  where 
    argsL = map (\(t, n) -> typeToLlvm t ++ " %" ++ n) args
    regArg (t, n) = do
      argReg <- putArg (t, n)
      alloca (typeToLlvm t) argReg
      store (typeToLlvm t) ("%" ++ n) (ptr $ typeToLlvm t) argReg 
    
-------------------------------------
stmtToLlvm :: Stmt -> Translator ()
stmtToLlvm (AssignS _ ns e) = do
  fromReg <- expToLlvm e
  (toReg, latteVarT) <- varToLlvm_ ns
  let varT = typeToLlvm latteVarT
  if (llvmT /= varT) then do
    castReg <- bitcast llvmT fromReg varT
    store varT castReg (ptr varT) toReg 
  else store llvmT fromReg (ptr llvmT) toReg 
  where llvmT = typeToLlvm $ getType e
stmtToLlvm (BlockS _ ss) = inBlock $ mapM_ stmtToLlvm ss
stmtToLlvm (CondS _ cond ifB Nothing) = do
  [condReg, ifLabel, endLabel] <- sequence [expToLlvm cond, newLabel "if" , newLabel "end"]
  br "i1" condReg ifLabel endLabel
  label ifLabel
  stmtToLlvm ifB
  goto endLabel
  label endLabel
stmtToLlvm (CondS _ cond ifB (Just elseB)) = do
  [condReg, ifLabel, elseLabel, endLabel] <- sequence [expToLlvm cond, newLabel "if", newLabel "else", newLabel "end"]
  br "i1" condReg ifLabel elseLabel
  label ifLabel
  stmtToLlvm ifB
  goto endLabel
  label elseLabel
  stmtToLlvm elseB
  goto endLabel
  label endLabel
stmtToLlvm (DecS _ t is) = mapM_ itemToLlvm is
  where 
    llvmT = typeToLlvm t
    itemToLlvm (n, mx) = do
      case mx of
        Just x -> do
          eReg <- expToLlvm x
          varR <- putVar t n
          alloca llvmT varR
          let eT = typeToLlvm $ getType x
          if (llvmT /= eT) then do
            castReg <- bitcast eT eReg llvmT
            store llvmT castReg (ptr llvmT) varR  
          else store llvmT eReg (ptr llvmT) varR
        Nothing -> do
          varR <- putVar t n
          alloca llvmT varR
          store llvmT (defVal t) (ptr llvmT) varR
stmtToLlvm (ExpS _ e) = void $ expToLlvm e
stmtToLlvm (ForS _ t i arr body) = do 
  [ixReg, arrHolderReg, elemReg, condLabel, bodyLabel, incLabel, endLabel] <- 
    sequence [newReg, newReg, putVar t i, newLabel "cond", newLabel "body", newLabel "inc", newLabel "end"]
  let arrT = typeToLlvm $ ArrayT t
  let arrPtrT = ptr arrT
  alloca "i32" ixReg
  alloca arrT arrHolderReg
  alloca (typeToLlvm t) elemReg
  store "i32" "0" "i32*" ixReg
  (arrReg, _) <- varToLlvm arr
  store arrT arrReg arrPtrT arrHolderReg
  goto condLabel
  label condLabel
  condIxReg <- load "i32*" ixReg
  condArrReg <- load arrPtrT arrHolderReg
  lenPtrReg <- getelementptr [(arrT, condArrReg), ("i32", "0"), ("i32", "1")]
  lenReg <- load "i32*" lenPtrReg
  cmpReg <- icmp "slt" "i32" condIxReg lenReg
  br "i1" cmpReg bodyLabel endLabel
  label bodyLabel
  bodyIxReg <- load "i32*" ixReg
  bodyArrReg <- load arrPtrT arrHolderReg
  inArrPtrReg <- getelementptr [(arrT, bodyArrReg), ("i32", "0"), ("i32", "0")]
  inArrReg <- load (arrptr $ typeToLlvm t) inArrPtrReg
  elemPtrReg <- getelementptr [(ptr $ typeToLlvm t, inArrReg), ("i32", bodyIxReg)]
  elemOtherReg <- load (ptr $ typeToLlvm t) elemPtrReg
  store (typeToLlvm t) elemOtherReg (ptr $ typeToLlvm t) elemReg
  stmtToLlvm body
  goto incLabel
  label incLabel
  incIxReg <- load "i32*" ixReg
  addReg <- add "i32" incIxReg "1"
  store "i32" addReg "i32*" ixReg
  goto condLabel
  label endLabel                     
stmtToLlvm (PredS _ ns) = do
  (varReg, _) <- varToLlvm_ ns
  loadReg <- load "i32*" varReg
  subReg <- sub "i32" loadReg "1"
  store "i32" subReg "i32*" varReg
stmtToLlvm (SuccS _ ns) = do
  (varReg, _) <- varToLlvm_ ns
  loadReg <- load "i32*" varReg
  addReg <- add "i32" loadReg "1"
  store "i32" addReg "i32*" varReg
stmtToLlvm (ReturnS _ Nothing) = do
  getRetLabel >>= goto
  newLabel "unreachable" >>= label
stmtToLlvm (ReturnS _ (Just x)) = do 
  expReg <- expToLlvm x
  store llvmT expReg (ptr llvmT) "%1"
  getRetLabel >>= goto
  newLabel "unreachable" >>= label
  where llvmT = typeToLlvm $ getType x 
stmtToLlvm (WhileS _ cond body) = do
  condLabel <- newLabel "cond"
  goto condLabel
  label condLabel
  [condReg, bodyLabel, endLabel] <- sequence [expToLlvm cond, newLabel "body", newLabel "end"]
  br "i1" condReg bodyLabel endLabel
  label bodyLabel
  stmtToLlvm body
  goto condLabel
  label endLabel
stmtToLlvm (EmptyS) = return ()

expToLlvm :: Exp -> Translator String
expToLlvm (VarE _ pos ns) = do
  (v, _) <- varToLlvm ns
  return v
expToLlvm (IntE _ n) = return $ show n
expToLlvm (BoolE _ True) = return "1"
expToLlvm (BoolE _ False) = return "0"
expToLlvm (StringE pos s) = do
  strIdent <- getStr s
  bitcast ("[" ++ (show $ length s + 1) ++ " x i8]*") strIdent "i8*"
expToLlvm (NullE pos) = return "null"
expToLlvm (UnaryE _ _ "-" e) = unary (sub "i32" "0") e
expToLlvm (UnaryE _ _ "!" e) = unary (sub "i1" "1") e
expToLlvm (BinaryE StringT _ "+" lhs rhs) = do
  lhsReg <- expToLlvm lhs
  rhsReg <- expToLlvm rhs
  sumReg <- newReg
  alloca "i8*" sumReg
  lhsLen <- call "i64" "@strlen" [("i8*", lhsReg)]
  rhsLen <- call "i64" "@strlen" [("i8*", rhsReg)]
  tmpSum <- add "i64" lhsLen rhsLen
  sumLen <- add "i64" tmpSum "1"
  desReg <- call "i8*" "@_Znam" [("i64", sumLen)]
  store "i8*" desReg "i8**" sumReg 
  cpyReg <- call "i8*" "@strcpy" [("i8*", desReg), ("i8*", lhsReg)]
  call "i8*" "@strcat" [("i8*", desReg), ("i8*", rhsReg)]
expToLlvm (BinaryE IntT _ "+" lhs rhs) = binary (add "i32") lhs rhs
expToLlvm (BinaryE _ _ "-" lhs rhs) = binary (sub "i32") lhs rhs
expToLlvm (BinaryE _ _ "*" lhs rhs) = binary (mul "i32") lhs rhs
expToLlvm (BinaryE _ _ "/" lhs rhs) = binary (udiv "i32") lhs rhs
expToLlvm (BinaryE _ _ "%" lhs rhs) = binary (urem "i32") lhs rhs
expToLlvm (BinaryE _ _ "&&" lhs rhs) = do
  [lhsReg, rhsLabel, endLabel] <- sequence [expToLlvm lhs, newLabel "rhs", newLabel "end"]
  lhsLabel <- currLabel
  br "i1" lhsReg rhsLabel endLabel
  label rhsLabel
  rhsReg <- expToLlvm rhs
  realRhsLabel <- currLabel
  goto endLabel
  label endLabel
  phi "i1" [("false", lhsLabel), (rhsReg, realRhsLabel)]
expToLlvm (BinaryE _ _ "||" lhs rhs) = do  
  [lhsReg, rhsLabel, endLabel] <- sequence [expToLlvm lhs, newLabel "rhs", newLabel "end"]
  lhsLabel <- currLabel
  br "i1" lhsReg endLabel rhsLabel
  label rhsLabel
  rhsReg <- expToLlvm rhs
  realRhsLabel <- currLabel
  goto endLabel
  label endLabel
  phi "i1" [("true", lhsLabel), (rhsReg, realRhsLabel)]
expToLlvm (BinaryE _ _ "==" lhs rhs) = binary (icmp "eq" llvmT) lhs rhs
  where llvmT = typeToLlvm $ getType lhs
expToLlvm (BinaryE _ _ "!=" lhs rhs) = binary (icmp "ne" llvmT) lhs rhs
  where llvmT = typeToLlvm $ getType lhs
expToLlvm (BinaryE _ _ "<" lhs rhs) = binary (icmp "slt" "i32") lhs rhs
expToLlvm (BinaryE _ _ "<=" lhs rhs) = binary (icmp "sle" "i32") lhs rhs
expToLlvm (BinaryE _ _ ">=" lhs rhs) = binary (icmp "sge" "i32") lhs rhs
expToLlvm (BinaryE _ _ ">" lhs rhs) = binary (icmp "sgt" "i32") lhs rhs
expToLlvm (CallE t _ [(n, _)] as) = do 
  let asT = map getType as
  asRegs <- mapM expToLlvm as
  call (typeToLlvm t) ("@" ++ n) (zip (map typeToLlvm asT) asRegs)
expToLlvm (CallE t _ ns as) = do  
  let asT = map getType as
  let asLlvmT = map typeToLlvm asT
  let (obj, (fn, _)) = (init ns, last ns)
  asRegs <- mapM expToLlvm as
  (objReg, latteObjT) <- varToLlvm obj
  (fnPos, selfT) <- getFun latteObjT fn asT
  let objT = typeToLlvm latteObjT
  let fnT = (typeToLlvm t) ++ " (" ++ (intercalate "," $ selfT:asLlvmT) ++ ")*"
  castReg <- bitcast objT objReg (arrptr fnT)
  virtualReg <- load (arrptr fnT) castReg
  fnPtrReg <- getelementptr [(ptr fnT, virtualReg), ("i32", fnPos)]  
  fnReg <- load (ptr fnT) fnPtrReg
  if (objT /= selfT) then do
    castReg2 <- bitcast objT objReg selfT 
    call (typeToLlvm t) fnReg $ (selfT, castReg2) : (zip asLlvmT asRegs)
    else call (typeToLlvm t) fnReg $ (selfT, objReg) : (zip asLlvmT asRegs)
expToLlvm (CastE _ t e) = do
  expReg <- expToLlvm e
  case getType e of
    NullT -> return expReg
    t -> bitcast (typeToLlvm $ getType e) expReg $ typeToLlvm t    
expToLlvm (NewArrE _ t len) = do
  lenReg <- expToLlvm len
  callReg <- call "i8*" "@_Znwm" [("i64", "8")]
  castReg <- bitcast "i8*" callReg (typeToLlvm (ArrayT t))
  --call "void" "@llvm.memset.p0i8.i64" [("i8*", callReg), ("i8", "0"), ("i64", "8"), ("i32", "1"), ("i1", "false")]
  arrReg <- getelementptr [(typeToLlvm (ArrayT t), castReg), ("i32", "0"), ("i32", "0")]
  typeSize <- getClassSize t
  len64Reg <- sext "i32" lenReg "i64"
  sizeReg <- mul "i64" len64Reg (show typeSize)
  newReg <- call "i8*" "@_Znam" [("i64", sizeReg)] -- @_Znam == new[]
  arrCastReg <- bitcast "i8*" newReg $ ptr $ typeToLlvm t    
  store (ptr $ typeToLlvm t) arrCastReg (arrptr $ typeToLlvm t) arrReg
  lenStoreReg <- getelementptr [(typeToLlvm (ArrayT t), castReg), ("i32", "0"), ("i32", "1")]
  store "i32" lenReg "i32*" lenStoreReg
  return castReg
expToLlvm (NewObjE _ t@(ClassT s)) = do
  size <- getClassSize t
  callReg <- call "i8*" "@_Znwm" [("i64", show size)]
  castReg <- bitcast "i8*" callReg (typeToLlvm t)
  call "void" ("@__" ++ s ++ "__") [(typeToLlvm t, castReg)]
  --call "void" "@llvm.memset.p0i8.i64" [("i8*", callReg), ("i8", "0"), ("i64", show size), ("i32", "1"), ("i1", "false")]
  return castReg

unary :: (String -> Translator String) -> Exp -> Translator String
unary op e = expToLlvm e >>= op 
  
binary :: (String -> String -> Translator String) -> Exp -> Exp -> Translator String
binary op e1 e2 = do
  expReg1 <- expToLlvm e1
  expReg2 <- expToLlvm e2
  op expReg1 expReg2

typeToLlvm :: Type -> String
typeToLlvm BoolT = "i1"
typeToLlvm IntT = "i32"
typeToLlvm StringT = "i8*"
typeToLlvm (ArrayT BoolT) = "%class._arr_bool*"
typeToLlvm (ArrayT IntT) = "%class._arr_int*"
typeToLlvm (ArrayT StringT) = "%class._arr_strReting*"
typeToLlvm (ArrayT (ClassT t)) = "%class._arr_" ++ t ++ "*"
typeToLlvm VoidT = "void"
typeToLlvm (ClassT s) = "%class." ++ s ++ "*"
typeToLlvm t = error $ "Latte.LlvmTranslator.typeToLlvm: type cannot be translated: " ++ show t

varToLlvm_ :: [(String, Maybe Exp)] -> Translator (String, Type)
varToLlvm_ ns = mapM goItem ns >>= getVar >>= uncurry3 processStruct
  where 
    uncurry3 f (a, b, c) = f a b c
    goItem (s, Nothing) = return (s, Nothing)
    goItem (s, Just e) = expToLlvm e >>= (\x -> return (s, Just x))
    processStruct objReg finalT [] = return (objReg, finalT)
    processStruct objReg finalT [(objT, ixs)] = do
      objPtr <- load (ptr $ objT) objReg
      fieldPtr <- getelementptr $ (objT, objPtr) : map ((,) "i32") ixs
      return (fieldPtr, finalT)  
    processStruct objReg finalT ((objT, ixs):objs) = do
      objPtr <- load (ptr $ objT) objReg
      fieldPtr <- getelementptr $ (objT, objPtr) : map ((,) "i32") ixs 
      processStruct fieldPtr finalT objs
    
varToLlvm :: [(String, Maybe Exp)] -> Translator (String, Type)
varToLlvm ns = do
  (reg, t) <- varToLlvm_ ns
  loadReg <- load (ptr $ typeToLlvm t) reg
  return (loadReg, t) 
  

{-- Env --}
data Env = Env {
  envStrs :: Map String String,
  envClzs :: Map String (Int, String, Map String (Type, Int)),
  envVars :: [Map String (Type, String)],
  envFuns :: Map (String, String, [Type]) (Type, Int),
  envRegIx :: Int,
  envLabelIx :: Int,
  envRetLabel :: String,
  envCurrLabel :: String
} deriving Show

mkEnv :: Env
mkEnv = Env Map.empty Map.empty [] Map.empty 1 1 "" ""

putEnvStr :: (String, String) -> Env -> Env
putEnvStr (str, reg) env = env { envStrs = Map.insert str reg $ envStrs env } 

getEnvStr :: String -> Env -> String
getEnvStr str env = case Map.lookup str $ envStrs env of
  Just name -> name
  Nothing -> error $ "Latte.LlvmTranslator.getEnvStr: " ++ str ++ " does not exist"   

putEnvClass :: String -> String -> [(Type, String)] -> Env -> Env
putEnvClass name super fs env = env { envClzs = Map.insert name (classSize, super, classFields) $ envClzs env }
  where
    (fTypes, fNames) = unzip fs
    classSize = max 1 $ sum $ map typeSize fTypes
    classFields = Map.fromList $ zip fNames $ zip fTypes [1,2..]
  
getEnvClassInfo :: String -> Env -> (Int, String, Map String (Type, Int))
getEnvClassInfo clz env = case Map.lookup clz $ envClzs env of
  Just x -> x
  Nothing -> error $ "Latte.LlvmTranslator.getEnvClassInfo: class " ++ clz ++ " does not exist"  
    
getEnvClassSize :: String -> Env -> Int
getEnvClassSize clz env = let (size, _, _) = getEnvClassInfo clz env in size

putEnvVar :: String -> Type -> String -> Env -> Env
putEnvVar n t r env = case envVars env of 
  (vm:vms) -> env { envVars = Map.insert n (t, r) vm : vms }
  [] -> error "Latte.LlvmTranslator.putEnvVar: not in a block" 

getEnvVar :: String -> Env -> Maybe (Type, String)
getEnvVar var env = listToMaybe $ mapMaybe (Map.lookup var) $ envVars env

getElemPtr :: [(String, Maybe String)] -> Env -> (String, Type, [(String, [String])])
getElemPtr [] _ = error "Latte.LlvmTranslator.getFunPtr: empty name"
getElemPtr ((n, ix):xs) env = case getEnvVar n env of
  Nothing -> case getEnvVar "self" env of
    Nothing -> error "Latte.LlvmTranslator.getElemPtr: variable self does not exist"
    Just (t, reg) -> toTriple reg $ build t ((n, ix):xs)
  Just (t, reg) -> toTriple reg $ buildIx t ix xs
  where
    infixr 5 <:>
    a <:> (b, as) = (b, a:as)
    toTriple a (b, c) = (a, b, c)
    build t [] = (t, [])
    build t@(ArrayT _) [("length", Nothing)] = (typeToLlvm t, ["0", "1"]) <:> build IntT []
    build t@(ClassT c) ((n, ix):fs) = let (fieldT, ixs) = findField c n in 
      (typeToLlvm t, "0":ixs) <:> buildIx fieldT ix fs 
    build _ _ = error "LlvmTranslator.getElemPtr.build: invalid arguments"
    buildIx t@(ArrayT e) (Just ix) fs = 
      (typeToLlvm t, ["0", "0"]) <:> (ptr $ typeToLlvm e, [ix]) <:> build e fs
    buildIx t Nothing fs = build t fs
    buildIx _ _ _ = error "LlvmTranslator.getElemPtr.buildIx: invalid arguments"
    findField c n = let (_, superC, fMap) = getEnvClassInfo c env in
      case Map.lookup n fMap of
        Just (t, ix) -> (t, [show ix])
        Nothing -> "0" <:> findField superC n

putEnvFun :: String -> String -> [Type] -> Type -> Int -> Env -> Env
putEnvFun clz fn as self pos env = 
  env { envFuns = Map.insert (clz, fn, as) (self, pos) $ envFuns env }
  
getFunPtr :: Type -> String -> [Type] -> Env -> (Type, Int)
getFunPtr (ClassT c) fn as env = case Map.lookup (c, fn, as) $ envFuns env of
  Just x -> x
  Nothing -> error $ "Latte.LlvmTranslator.getFunPtr: function " ++ fn ++ show as ++ " does not exist in type " ++ c ++ ".\n" ++ show env
getFunPtr t fn _ _ = error $ "Latte.LlvmTranslator.getFunPtr: function " ++ fn ++ " does not exist in type " ++ show t
                           
clearEnvReg :: Env -> Env
clearEnvReg env = env { envRegIx = 1 }

incEnvReg :: Env -> Env
incEnvReg env = env { envRegIx = succ $ envRegIx env }

incEnvLabel :: Env -> Env
incEnvLabel env = env { envLabelIx = succ $ envLabelIx env }

enterEnvBlock :: Env -> Env
enterEnvBlock env = env { envVars = Map.empty : envVars env }

exitEnvBlock :: Env -> Env
exitEnvBlock env = case envVars env of
  (x:xs) -> env { envVars = xs }
  [] -> error "Latte.llvmTranslator.exitEnvBlock: not in a block" 

setEnvRetLabel :: String -> Env -> Env
setEnvRetLabel l env = env { envRetLabel = l }

setEnvCurrLabel :: String -> Env -> Env
setEnvCurrLabel l env = env { envCurrLabel = l }

typeSize :: Type -> Int
typeSize _ = 4

defVal BoolT = "0"
defVal IntT = "0"
defVal _ = "null"

----------------------
----------------------

putStrs :: [String] -> Translator ()
putStrs strs = do
  let strCs = zip strs $ map (("@.str"++) . show) [1,2..]
  mapM (modify . putEnvStr) strCs
  mapM_ (tell . showStr) strCs
  where 
    showStr (str, reg) = 
      reg ++ " = private constant [" ++ (show $ length str + 1) ++ " x i8] c\"" ++ escapeStr str ++ "\"\n" 

getStr :: String -> Translator String
getStr = gets . getEnvStr

putArg :: (Type, String) -> Translator String
putArg (clazz, name) = putVar clazz name

putVar :: Type -> String -> Translator String
putVar clazz name = do
  reg <- gets (("%"++) . show . envRegIx)
  modify incEnvReg
  modify $ putEnvVar name clazz reg
  return reg

getVar :: [(String, Maybe String)] -> Translator (String, Type, [(String, [String])])
getVar = gets . getElemPtr

putFun :: String -> String -> [Type] -> Type -> Int -> Translator ()
putFun clz name as sup pos = modify $ putEnvFun clz name as sup pos

getFun :: Type -> String -> [Type] -> Translator (String, String) 
getFun t n as = do 
  (t, pos) <- gets $ getFunPtr t n as
  return (show pos, typeToLlvm t)

getClassSize :: Type -> Translator Int
getClassSize (ClassT t) = gets $ getEnvClassSize t
getClassSize t = return $ typeSize t

newLabel :: String -> Translator String
newLabel s = do
  lIx <- gets envLabelIx
  modify incEnvLabel
  return $ s ++ show lIx
  
currLabel :: Translator String
currLabel = gets envCurrLabel

newReg :: Translator String
newReg = do
  regIx <- gets envRegIx
  modify incEnvReg
  return $ "%" ++ show regIx

inBlock :: Translator a -> Translator a
inBlock ma = do
  modify enterEnvBlock
  a <- ma
  modify exitEnvBlock
  return a
  
inMethod :: Translator a -> Translator a
inMethod ma = modify clearEnvReg >> modify (setEnvCurrLabel "0") >> inBlock ma
  
genClasses :: [Clazz] -> Translator ()
genClasses cs = do
  genArrs
  mapM_ genStruct cs
  genStrangeExternalVars
  mapM_ genNameVar cs
  mapM_ genInfoVar cs
  csWithVirtual <- mapM (genVirtualVar cs) cs
  mapM_ genCtor csWithVirtual
  
genArrs :: Translator ()
genArrs = do
  tell $ "%class._arr_bool = type { i1*, i32 }\n\
    \%class._arr_int = type { i32*, i32 }\n\
    \%class._arr_string = type { i8**, i32 }\n"
  modify $ putEnvClass "_arr_bool" "" [(ArrayT BoolT, "arr"), (IntT, "length")]
  modify $ putEnvClass "_arr_int" "" [(ArrayT IntT, "arr"), (IntT, "length")]
  modify $ putEnvClass "_arr_string" "" [(ArrayT StringT, "arr"), (IntT, "length")]

genStruct :: Clazz -> Translator ()
genStruct (Clazz _ n s _ fs _) = do
  let fields = map genField fs
  tell $ "%class." ++ n ++ " = type { "
    ++ (intercalate ", " $ (vtOrSuper s) : (map (typeToLlvm . fst) fields)) ++ " }\n\
    \%class._arr_" ++ n ++ " = type { %class." ++ n ++ "*, i32 }\n"
  modify $ putEnvClass n s fields
  modify $ putEnvClass ("_arr_" ++ n) "" [(ArrayT $ ClassT n, "arr"), (IntT, "length")]
  where 
    vtOrSuper "" = "i32 (...)*"
    vtOrSuper s = "%class." ++ s
    genField (Field _ t n) = (t, n)

genStrangeExternalVars :: Translator ()
genStrangeExternalVars = do
  tell $ "@_ZTVN10__cxxabiv120__si_class_type_infoE = external global i8*\n"
  tell $ "@_ZTVN10__cxxabiv117__class_type_infoE = external global i8*\n"

genNameVar :: Clazz -> Translator ()
genNameVar (Clazz _ n _ _ _ _) = 
  tell $ "@_Name" ++ n ++ " = constant [" ++ (show $ length n + 1) ++ " x i8] c\"" ++ (escapeStr n) ++ "\"\n"
  
genInfoVar :: Clazz -> Translator ()
genInfoVar (Clazz _ n "" _ _ _) = 
  tell $ "@_Info" ++ n ++ " = constant { i8*, i8* } { i8* bitcast (i8** getelementptr \
    \(i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr \
    \([" ++ (show $ length n + 1) ++ " x i8]* @_Name" ++ n ++ ", i32 0, i32 0) }\n"  
genInfoVar (Clazz _ n s ss _ _) = 
  tell $ "@_Info" ++ n ++ " = constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr \
    \(i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr \
    \([" ++ (show $ length n + 1) ++ " x i8]* @_Name" ++ n ++ ", i32 0, i32 0), i8* bitcast ("
    ++ superStruct ss ++ "* @_Info" ++ s ++ " to i8*) }\n"
  where 
    superStruct [s, ""] = "{ i8*, i8* }"
    superStruct _ = "{ i8*, i8*, i8* }" 

genVirtualVar :: [Clazz] -> Clazz -> Translator (Clazz, Int)  
genVirtualVar cs c@(Clazz _ n s ss _ ms) = do
  let kitties = reverse $ foldr putMethods [] $ c : (map (findClass cs) $ init ss)
  tell $ "@Virtual" ++ n ++ " = constant [" ++ (show $ length kitties + 2) ++ " x i8*] [i8* null, \
    \i8* bitcast (" ++ struct s ++ "* @_Info" ++ n ++ " to i8*)" ++ (if null kitties then "" else ", "  
    ++ (intercalate ", " $ map putKitty kitties)) ++ "]\n" 
  mapM_ putFun' $ zip [0, 1..] kitties
  return (c, length kitties + 2)
  where
    putMethods (Clazz _ n _ _ _ ms) l = foldr (\x l' -> putOrReplace (x, n) l') l $ map simpleMethod ms
    putOrReplace (k, v) l = case lookup k l of 
      Nothing -> (k, v):l
      Just _ -> map (replace (k, v)) l
    replace (k1, v1) (k2, v2) = if k1 == k2 then (k1, v1) else (k2, v2) 
    simpleMethod (Method _ ret n as _) = (ret, n, as)
    findClass cs s = fromJust $ find (\(Clazz _ n _ _ _ _) -> n == s) cs
    putKitty ((ret, n, as), c) = "i8* bitcast (" ++ typeToLlvm ret ++ " (%class." ++ c ++ "*" ++ 
      (if null as then "" else ", " ++ (intercalate ", " $ map (typeToLlvm . fst) as)) 
      ++ ")* @" ++ c ++ "__" ++ n ++ " to i8*)"
    putFun' (pos, ((_, fnN, as), s)) = putFun n fnN (map fst as) (ClassT s) pos 
    struct "" = "{ i8*, i8* }"
    struct s = "{ i8*, i8*, i8* }" 
       
genCtor :: (Clazz, Int) -> Translator ()
genCtor ((Clazz _ n "" _ fs ms), i) = do 
  tell $ "define void @__" ++ n ++ "__(%class." ++ n ++ "* %this) {\n\
  \  %1 = alloca %class." ++ n ++"*\n\
  \  store %class." ++ n ++ "* %this, %class." ++ n ++ "** %1\n\
  \  %2 = load %class." ++ n ++ "** %1\n\
  \  %3 = bitcast %class." ++ n ++ "* %2 to i8***\n\
  \  store i8** getelementptr ([" ++ show i ++ " x i8*]* @Virtual" ++ n ++ ", i64 0, i64 2), i8*** %3\n"
  mapM_ (genFieldDefaultVal 3 n) $ zip fs [1,2..]
  tell $ "  ret void\n\
  \}\n"
genCtor ((Clazz _ n s _ fs ms), i) = do
  tell $ "define void @__" ++ n ++ "__(%class." ++ n ++ "* %this) {\n\
  \  %1 = alloca %class." ++ n ++"*\n\
  \  store %class." ++ n ++ "* %this, %class." ++ n ++ "** %1\n\
  \  %2 = load %class." ++ n ++ "** %1\n\
  \  %3 = bitcast %class." ++ n ++ "* %2 to %class." ++ s ++ "*\n\
  \  call void @__" ++ s ++ "__(%class." ++ s ++ "* %3)\n\
  \  %4 = bitcast %class." ++ n ++ "* %2 to i8***\n\
  \  store i8** getelementptr ([" ++ show i ++ " x i8*]* @Virtual" ++ n ++ ", i64 0, i64 2), i8*** %4\n"
  mapM_ (genFieldDefaultVal 4 n) $ zip fs [1,2..]
  tell $ "  ret void\n\
  \}\n"
  
genFieldDefaultVal :: Int -> String -> (Field, Int) -> Translator ()
genFieldDefaultVal x n ((Field _ t _), ix) = do
  tell $ "  %" ++ show (ix + x) ++ " = getelementptr %class." ++ n ++ "* %2, i32 0, i32 " ++ show ix ++ "\n"
  tell $ "  store " ++ typeToLlvm t ++ " " ++ defVal t ++ ", " ++ (ptr $ typeToLlvm t) ++ " %" ++ (show (ix + x)) ++ "\n"  
      
putLib :: Translator ()
putLib = tell $
  "%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, \
  \%struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, \
  \i8*, i8*, i64, i32, [20 x i8] }\n\
  \%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }\n\
  \@stderr = external global %struct._IO_FILE*\n\
  \@stdin = external global %struct._IO_FILE*\n\
  \@.intPFormat = private constant [4 x i8] c\"%d\\0A\\00\"\n\
  \@.intSFormat = private constant [4 x i8] c\"%d\\0A\\00\"\n\
  \@.strFormat = private constant [4 x i8] c\"%s\\0A\\00\"\n\
  \@.runtimeErr = private constant [14 x i8] c\"runtime error\\00\"\n\
  \declare i8* @_Znam(i64)\n\
  \declare i8* @_Znwm(i64)\n\
  \declare i64 @getline(i8**, i64*, %struct._IO_FILE*)\n\
  \declare i32 @fprintf(%struct._IO_FILE*, i8*, ...)\n\
  \declare i32 @printf(i8*, ...)\n\
  \declare i32 @scanf(i8*, ...)\n\
  \declare i8* @strcat(i8*, i8*)\n\
  \declare i8* @strcpy(i8*, i8*)\n\
  \declare i64 @strlen(i8*)\n\
  \define void @printInt(i32 %i) {\n\
  \  %1 = alloca i32\n\
  \  store i32 %i, i32* %1\n\
  \  %2 = load i32* %1\n\
  \  %3 = call i32 (i8*, ...)* \
       \@printf(i8* getelementptr ([4 x i8]* @.intPFormat, i32 0, i32 0), i32 %2)\n\
  \  ret void\n\
  \}\n\
  \define void @printString(i8* %c) {\n\
  \  %1 = alloca i8*\n\
  \  store i8* %c, i8** %1\n\
  \  %2 = load i8** %1\n\
  \  %3 = call i32 (i8*, ...)* \
       \@printf(i8* getelementptr ([4 x i8]* @.strFormat, i32 0, i32 0), i8* %2)\n\
  \  ret void\n\
  \}\n\
  \define void @error() {\n\
  \  %1 = load %struct._IO_FILE** @stderr\n\
  \  %2 = call i32 (%struct._IO_FILE*, i8*, ...)* \
       \@fprintf(%struct._IO_FILE* %1, i8* getelementptr ([14 x i8]* @.runtimeErr, i32 0, i32 0))\n\
  \  ret void\n\
  \}\n\
  \define i32 @readInt() {\n\
  \  %i = alloca i32\n\
  \  %1 = call i32 (i8*, ...)* @scanf(i8* getelementptr ([4 x i8]* @.intSFormat, i32 0, i32 0), i32* %i)\n\
  \  %2 = load i32* %i\n\
  \  ret i32 %2\n\
  \}\n\
  \define i8* @readString() {\n\
  \  %str = alloca i8*\n\
  \  %i = alloca i64\n\
  \  store i8* null, i8** %str\n\
  \  %1 = load %struct._IO_FILE** @stdin\n\
  \  %2 = call i64 @getline(i8** %str, i64* %i, %struct._IO_FILE* %1)\n\
  \  %3 = sub i64 %2, 1\n\
  \  %4 = load i8** %str\n\
  \  %5 = getelementptr i8* %4, i64 %3\n\
  \  store i8 0, i8* %5\n\
  \  ret i8* %4\n\
  \}\n"
  
setRetLabel :: String -> Translator ()
setRetLabel = modify . setEnvRetLabel

getRetLabel :: Translator String
getRetLabel = gets envRetLabel

escapeStr :: String -> String
escapeStr s = (concatMap escapeChar s) ++ "\\00"
  where
    escapeChar '\"' = "\\22"
    escapeChar '\\' = "\\5C"
    escapeChar '\a' = "\\07"
    escapeChar '\b' = "\\08"
    escapeChar '\f' = "\\0C"
    escapeChar '\n' = "\\0A"
    escapeChar '\r' = "\\0D"
    escapeChar '\t' = "\\09"
    escapeChar '\v' = "\\0B"
    escapeChar c = [c]
      
{-- Llvm instructions builders --}

ins :: String -> Translator String
ins i = do 
  reg <- newReg
  tell $ "  " ++ reg ++ " = " ++ i
  return reg

add :: String -> String -> String -> Translator String
add = insBinary "add"

alloca :: String -> String -> Translator ()
alloca t r = tell $ "  " ++ r ++ " = alloca " ++ t ++ "\n"

bitcast :: String -> String -> String -> Translator String
bitcast t1 r t2 = ins $ "bitcast " ++ t1 ++ " " ++ r ++ " to " ++ t2 ++ "\n"

br :: String -> String -> String -> String -> Translator ()
br t r l1 l2 = tell $ "  br " ++ t ++ " " ++ r ++ ", label %" ++ l1 ++ ", label %" ++ l2 ++ "\n"

call :: String -> String -> [(String, String)] -> Translator String
call "void" n as = do
  tell $ "  call void " ++ n ++ "(" ++ (intercalate ", " $ map showPair as) ++ ")\n" 
  return ""
call t n as = ins $ "call " ++ t ++ " " ++ n ++ "(" ++ (intercalate ", " $ map showPair as) ++ ")\n"

getelementptr :: [(String, String)] -> Translator String
getelementptr ps = ins $ "getelementptr " ++ (intercalate ", " $ map showPair ps) ++ "\n"

goto :: String -> Translator ()
goto l = tell $ "  br label %" ++ l ++ "\n"

icmp :: String -> String -> String -> String -> Translator String
icmp cmp = insBinary $ "icmp " ++ cmp

label :: String -> Translator ()
label l = do
  modify $ setEnvCurrLabel l
  tell $ l ++ ":\n"

load :: String -> String -> Translator String
load t r = ins $ "load " ++ t ++ " " ++ r ++ "\n"

mul :: String -> String -> String -> Translator String
mul = insBinary "mul"

phi :: String -> [(String, String)] -> Translator String
phi t1 vs = ins $ "phi " ++ t1 ++ " " ++ (intercalate ", " $ map putPair vs) ++ "\n"
  where putPair (v, l) = "[ " ++ v ++ ", %" ++ l ++ " ]" 

ret :: String -> String -> Translator ()
ret t r = tell $ "  ret " ++ t ++ " " ++ r ++ "\n" 

sext :: String -> String -> String -> Translator String
sext t1 r t2 = ins $ "sext " ++ t1 ++ " " ++ r ++ " to " ++ t2 ++ "\n"

store :: String -> String -> String -> String -> Translator ()
store t1 r1 t2 r2 = tell $ unwords ["  store", t1, r1 ++ ",", t2, r2, "\n"]

sub :: String -> String -> String -> Translator String
sub = insBinary "sub"

udiv :: String -> String -> String -> Translator String
udiv = insBinary "udiv"

urem :: String -> String -> String -> Translator String
urem = insBinary "urem"

arrptr :: String -> String
arrptr = (++ "**")

ptr :: String -> String
ptr = (++ "*")

insBinary :: String -> String -> String -> String -> Translator String
insBinary op t r1 r2 = ins $ op ++ " " ++ t ++ " " ++ r1 ++ ", " ++ r2 ++ "\n"

showPair (a, b) = a ++ " " ++ b

{-- Utils --}

type Translator a = StateT Env (Writer String) a

tell :: String -> Translator ()
tell = lift . Writer.tell
