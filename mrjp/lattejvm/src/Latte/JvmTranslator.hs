module Latte.JvmTranslator (toJvm, toJvmToFile) where

import Control.Monad.State
import qualified Control.Monad.Writer as Writer (tell)
import Control.Monad.Writer hiding (tell)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)

import Latte.ProgramTree

toJvmToFile :: Module -> String -> FilePath -> IO ()
toJvmToFile m n f = writeFile f $ toJvm m n

toJvm :: Module -> String -> String
toJvm m n = execWriter $ ($(newEnv n)) $ evalStateT $ programToJvm m

programToJvm :: Module -> Translator ()
programToJvm (Module _ _ _ ms) = putLib >> mapM_ methodToJvm ms

methodToJvm :: Method -> Translator ()    
methodToJvm (Method _ ret name args body) = inMethod $ do
  tell $ ".method public static " ++ name ++ "(" ++ argsD ++ ")" ++ descriptor ret ++ "\n"
  when (locals > 0) $ tell $ "  .limit locals " ++ (show locals) ++ "\n"
  when (stack > 0) $ tell $ "  .limit stack " ++ (show stack) ++ "\n"
  mapM_ putVar $ map snd args
  mapM_ stmtToJvm body
  when (ret == VoidT) $ return_ Nothing
  tell "  nop\n" 
  tell ".end method\n\n"
  where 
    argsD = concatMap (descriptor . fst) args
    locals = length args + (foldl countLocals 0 body)
    stack = maximum $ 0 : map countStackS body
  
countLocals :: Int -> Stmt -> Int
countLocals n (BlockS _ ss) = n + (foldl countLocals 0 ss)
countLocals n (CondS _ _ ifB Nothing) = countLocals n ifB
countLocals n (CondS _ _ ifB (Just elseB)) = countLocals (countLocals n ifB) elseB
countLocals n (DecS _ _ is) = n + length is
countLocals n (WhileS _ _ s) = countLocals n s
countLocals n _ = n

countStackS :: Stmt -> Int
countStackS (AssignS _ _ e) = countStackE e
countStackS (BlockS _ ss) = maximum $ 0 : map countStackS ss
countStackS (CondS _ c ifB Nothing) = max (countStackE c) (countStackS ifB)
countStackS (CondS _ c ifB (Just elseB)) = maximum [countStackE c, countStackS ifB, countStackS elseB]
countStackS (DecS _ _ is) = maximum $ map countStackI is
  where 
    countStackI (n, Nothing) = 1
    countStackI (n, Just e) = countStackE e
countStackS (ExpS _ e) = countStackE e
countStackS (ReturnS _ (Just e)) = countStackE e
countStackS (WhileS _ c s) = max (countStackE c) (countStackS s)
countStackS _ = 0

countStackE (UnaryE _ _ "!" e) = countStackE e + 1
countStackE (UnaryE _ _ _ e) = countStackE e
countStackE (BinaryE _ _ "&&" e1 e2) = maximum [2, countStackE e1, countStackE e2]
countStackE (BinaryE _ _ "||" e1 e2) = maximum [2, countStackE e1, countStackE e2]
countStackE (BinaryE _ _ "<" e1 e2) = maximum [2, countStackE e1 + 1, countStackE e2 + 2]
countStackE (BinaryE _ _ "<=" e1 e2) = maximum [2, countStackE e1 + 1, countStackE e2 + 2]
countStackE (BinaryE _ _ ">" e1 e2) = maximum [2, countStackE e1 + 1, countStackE e2 + 2]
countStackE (BinaryE _ _ ">=" e1 e2) = maximum [2, countStackE e1 + 1, countStackE e2 + 2]
countStackE (BinaryE _ _ "==" e1 e2) = case getType e1 of
  StringT -> max (countStackE e1) (countStackE e2 + 1)
  _ -> maximum [2, countStackE e1 + 1, countStackE e2 + 2]
countStackE (BinaryE _ _ "!=" e1 e2) = case getType e1 of
  StringT -> max (countStackE e1) (countStackE e2 + 1)
  _ -> maximum [2, countStackE e1 + 1, countStackE e2 + 2]
countStackE (BinaryE _ _ _ e1 e2) = max (countStackE e1) (countStackE e2 + 1)
countStackE (CallE VoidT _ _ []) = 0
countStackE (CallE _ _ _ []) = 1
countStackE (CallE _ _ _ es) = maximum $ map (\(i, e) -> i + countStackE e) $ zip [0, 1..] es
countStackE _ = 1

  
stmtToJvm :: Stmt -> Translator ()
stmtToJvm (AssignS _ [(n, Nothing)] e) = expToJvm e >> getVar n >>= store (getType e) 
stmtToJvm (BlockS _ ss) = inBlock $ mapM_ stmtToJvm ss
stmtToJvm (CondS _ c ifB Nothing) = do
  endLabel <- newLabel
  expToJvm c
  ifeq endLabel
  stmtToJvm ifB
  label endLabel
stmtToJvm (CondS _ c ifB (Just elseB)) = do
  elseLabel <- newLabel
  endLabel <- newLabel
  expToJvm c
  ifeq elseLabel
  stmtToJvm ifB
  goto endLabel
  label elseLabel
  stmtToJvm elseB
  label endLabel 
stmtToJvm (DecS _ t is) = mapM_ putItem is
  where 
    putItem (n, Nothing) = case t of
      StringT -> tell "  aconst_null\n" >> putVar n >>= store t
      _ -> iconst 0 >> putVar n >>= store t
    putItem (n, Just e) = expToJvm e >> putVar n >>= store t
stmtToJvm (ExpS _ e@(CallE VoidT _ _ _)) = expToJvm e
stmtToJvm (ExpS _ e) = expToJvm e >> pop
stmtToJvm (PredS _ [(n, Nothing)]) = getVar n >>= (flip iinc) (-1)
stmtToJvm (SuccS _ [(n, Nothing)]) = getVar n >>= (flip iinc) 1
stmtToJvm (ReturnS _ Nothing) = return_ Nothing
stmtToJvm (ReturnS _ (Just e)) = expToJvm e >> return_ (Just $ getType e)
stmtToJvm (WhileS _ c s) = do
  condLabel <- newLabel
  endLabel <- newLabel
  label condLabel
  expToJvm c
  ifeq endLabel
  stmtToJvm s
  goto condLabel
  label endLabel
stmtToJvm s = error $ "JvmTranslator.stmtToJvm: unsupported statement " ++ show s

expToJvm :: Exp -> Translator ()
expToJvm (VarE t _ [(n, Nothing)]) = getVar n >>= load t
expToJvm (IntE _ n)
  | (-1) <= n && n <= 5 = iconst n
  | -128 <= n && n <= 127 = bipush n
  | otherwise = ldc $ show n
expToJvm (BoolE _ True) = iconst 1
expToJvm (BoolE _ False) = iconst 0
expToJvm (StringE _ s) = ldc ("\"" ++ s ++ "\"")
expToJvm (UnaryE _ _ "-" e) = expToJvm e >> ineg
expToJvm (UnaryE _ _ "!" e) = iconst 1 >> expToJvm e >> isub
expToJvm (BinaryE StringT _ "+" exp1 exp2) = do
  tell $ "  new java/lang/StringBuilder\n\
    \  dup\n\
    \  invokespecial java/lang/StringBuilder/<init>()V\n"
  expToJvm exp1
  tell "  invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;\n"
  expToJvm exp2
  tell "  invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;\n\
    \  invokevirtual java/lang/StringBuilder/toString()Ljava/lang/String;\n"  
expToJvm (BinaryE IntT _ "+" exp1 exp2) = binary iadd exp1 exp2
expToJvm (BinaryE _ _ "-" exp1 exp2) = binary isub exp1 exp2
expToJvm (BinaryE _ _"*" exp1 exp2) = binary imul exp1 exp2
expToJvm (BinaryE _ _ "/" exp1 exp2) = binary idiv exp1 exp2
expToJvm (BinaryE _ _ "%" exp1 exp2) = binary irem exp1 exp2
expToJvm (BinaryE _ _ "<" exp1 exp2) = comparison "if_icmpge" exp1 exp2
expToJvm (BinaryE _ _ "<=" exp1 exp2) = comparison "if_icmpgt" exp1 exp2
expToJvm (BinaryE _ _ ">" exp1 exp2) = comparison "if_icmple" exp1 exp2
expToJvm (BinaryE _ _ ">=" exp1 exp2) = comparison "if_icmplt" exp1 exp2
expToJvm (BinaryE StringT _ "==" exp1 exp2) = do
  expToJvm exp1
  expToJvm exp2
  tell $ "  invokevirtual java/lang/Object/equals(Ljava/lang/Object;)Z\n"
expToJvm (BinaryE StringT _ "!=" exp1 exp2) = do
  expToJvm exp1
  expToJvm exp2
  tell $ "  invokevirtual java/lang/Object/equals(Ljava/lang/Object;)Z\n"
  ineg
expToJvm (BinaryE _ _ "==" exp1 exp2) = comparison "if_icmpne" exp1 exp2
expToJvm (BinaryE _ _ "!=" exp1 exp2) = comparison "if_icmpeq" exp1 exp2
expToJvm (BinaryE _ _ "&&" exp1 exp2) = do
  endLabel <- newLabel
  expToJvm exp1
  dup
  ifeq endLabel
  pop
  expToJvm exp2
  label endLabel
expToJvm (BinaryE _ _ "||" exp1 exp2) = do
  endLabel <- newLabel
  expToJvm exp1
  dup
  ifne endLabel
  pop
  expToJvm exp2
  label endLabel
expToJvm (CallE t _ [(n, Nothing)] as) = do
  let dArgs = concatMap (descriptor . getType) as
  mapM expToJvm as
  fileName <- gets envClass
  tell $ "  invokestatic " ++ fileName ++ "/" ++ n ++ "(" ++ dArgs ++ ")" ++ descriptor t ++ "\n"
expToJvm e = error $ "JvmTranslator.expToJvm: unsupported expression " ++ show e

binary :: Translator () -> Exp -> Exp -> Translator ()
binary op lhs rhs = expToJvm lhs >> expToJvm rhs >> op

comparison :: String -> Exp -> Exp -> Translator ()
comparison cmp lhs rhs = do
  endLabel <- newLabel
  iconst 0
  expToJvm lhs
  expToJvm rhs
  tell $ "  " ++ cmp ++ " " ++ endLabel ++ "\n"
  iconst 1
  iadd
  label endLabel

descriptor :: Type -> String
descriptor BoolT = "Z"
descriptor IntT = "I"
descriptor StringT = "Ljava/lang/String;"
descriptor VoidT = "V"
descriptor t = error $ "Latte.JvmTranslator.descriptor: unsupported type " ++ show t

prefix :: Type -> String
prefix BoolT = "i"
prefix IntT = "i"
prefix VoidT = ""
prefix StringT = "a"
prefix t = error $ "Latte.JvmTranslator.prefix: unsupported type " ++ show t

{- Environment -}

data Env = Env {
  envClass :: String,
  envVars :: [Map String Int],
  envVarIx :: Int,
  envLabel :: Int
}  

newEnv :: String -> Env
newEnv n = Env n [] 0 0

putEnvVar :: String -> Env -> (Env, Int)
putEnvVar n env = case envVars env of 
  (vm:vms) -> let ix = envVarIx env in (env { envVars = Map.insert n ix vm : vms, envVarIx = succ $ ix }, ix)
  [] -> error "Latte.JvmTranslator.putEnvVar: not in a block"  

getEnvVar :: String -> Env -> Int
getEnvVar n env = case envVars env of
  (vs:vms) -> case listToMaybe $ mapMaybe (Map.lookup n) (vs:vms) of
    Just n -> n
    Nothing -> error $ "Latte.JvmTranslator.getEnvVar: variable " ++ n ++ " does not exist"
  [] -> error "Latte.JvmTranslator.getEnvVar: not in the block"

enterEnvBlock :: Env -> Env
enterEnvBlock env = env { envVars = Map.empty : (envVars env) } 

exitEnvBlock :: Env -> Env
exitEnvBlock env = case envVars env of
  (vm:vms) -> env { envVars = vms }
  [] -> error "Latte.JvmTranslator.exitEnvBlock: not in a block"

resetEnvVarIx :: Env -> Env
resetEnvVarIx env = env { envVarIx = 0 }

incEnvLabelIx :: Env -> Env
incEnvLabelIx env = env { envLabel = succ $ envLabel env }

{- Translator -}

putVar :: String -> Translator Int
putVar var = do
  (newEnv, ix) <- gets $ putEnvVar var 
  put newEnv
  return ix

getVar :: String -> Translator Int
getVar = gets . getEnvVar 

inBlock :: Translator a -> Translator a
inBlock ma = do
  modify enterEnvBlock
  a <- ma
  modify exitEnvBlock
  return a 
  
inMethod :: Translator a -> Translator a
inMethod ma = modify resetEnvVarIx >> inBlock ma

newLabel :: Translator String
newLabel = do
  l <- gets envLabel
  modify incEnvLabelIx
  return $ "L" ++ show l

putLib :: Translator ()
putLib = do
  fileName <- gets envClass 
  tell $ ".class public " ++ fileName ++ "\n\
  \.super java/lang/Object\n\
  \.method public <init>()V\n\
  \  aload 0\n\
  \  invokespecial java/lang/Object/<init>()V\n\
  \  return\n\
  \.end method\n\n\
  \.method public static main([Ljava/lang/String;)V\n\
  \  invokestatic " ++ fileName ++ "/main()I\n\
  \  pop\n\
  \  return\n\
  \.end method\n\n\
  \.method public static printInt(I)V\n\
  \  .limit stack 2\n\
  \  getstatic java/lang/System/out Ljava/io/PrintStream;\n\
  \  iload_0\n\
  \  invokevirtual java/io/PrintStream/println(I)V\n\
  \  return\n\
  \.end method\n\n\
  \.method public static printString(Ljava/lang/String;)V\n\
  \  .limit stack 2\n\
  \  getstatic java/lang/System/out Ljava/io/PrintStream;\n\
  \  aload_0\n\
  \  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n\
  \  return\n\
  \.end method\n\n\
  \.method public static error()V\n\
  \  .limit stack 2\n\
  \  getstatic java/lang/System/err Ljava/io/PrintStream;\n\
  \  ldc \"runtime error\"\n\
  \  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n\
  \  return\n\
  \.end method\n\n\
  \.method public static readInt()I\n\
  \  .limit stack 3\n\
  \  new java/util/Scanner\n\
  \  dup\n\
  \  getstatic java/lang/System/in Ljava/io/InputStream;\n\
  \  invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n\
  \  invokevirtual java/util/Scanner/nextInt()I\n\
  \  ireturn\n\
  \.end method\n\n\
  \.method public static readString()Ljava/lang/String;\n\
  \  .limit stack 3\n\
  \  new java/util/Scanner\n\
  \  dup\n\
  \  getstatic java/lang/System/in Ljava/io/InputStream;\n\
  \  invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n\
  \  invokevirtual java/util/Scanner/nextLine()Ljava/lang/String;\n\
  \  areturn\n\
  \.end method\n\n"  
  
---------  

bipush :: Int32 -> Translator ()
bipush n = tell $ "  bipush " ++ show n ++ "\n"

dup :: Translator ()
dup = tell $ "  dup\n"

goto :: String -> Translator ()
goto l = tell $ "  goto " ++ l ++ "\n"

iconst :: Int32 -> Translator ()
iconst (-1) = tell $ "  iconst_m1\n"
iconst n = tell $ "  iconst_" ++ show n ++ "\n"

label :: String -> Translator ()
label l = tell $ l ++ ":\n"

ldc :: String -> Translator ()
ldc c = tell $ "  ldc " ++ c ++ "\n"
  
load :: Type -> Int -> Translator ()
load t var = tell $ withUnderscore ("  " ++ (prefix t) ++ "load") var 3

iinc :: Int -> Int -> Translator ()
iinc v n = tell $ "  iinc " ++ show v ++ " " ++ show n ++ "\n"   

iadd :: Translator ()
iadd = tell $ "  iadd\n"

idiv :: Translator ()
idiv = tell $ "  idiv\n"

ifeq :: String -> Translator ()
ifeq l = tell $ "  ifeq " ++ l ++ "\n"

ifne :: String -> Translator ()
ifne l = tell $ "  ifne " ++ l ++ "\n"

imul :: Translator ()
imul = tell $ "  imul\n"

ineg :: Translator ()
ineg = tell $ "  ineg\n"

irem :: Translator ()
irem = tell $ "  irem\n"

isub :: Translator ()
isub = tell $ "  isub\n"

pop :: Translator ()
pop = tell $ "  pop\n"

return_ :: Maybe Type -> Translator ()
return_ Nothing = tell "  return\n"
return_ (Just t) = tell $ "  " ++ prefix t ++ "return\n"

store :: Type -> Int -> Translator ()
store t var = tell $ withUnderscore ("  " ++ (prefix t) ++ "store") var 3
      
withUnderscore :: String -> Int -> Int -> String
withUnderscore ins var limit
  | var >= 0 && var <= limit = ins ++ "_" ++ show var ++ "\n"
  | otherwise = ins ++ " " ++ show var ++ "\n"
  
-- Utils

type Translator a = StateT Env (Writer String) a

tell = lift . Writer.tell 
