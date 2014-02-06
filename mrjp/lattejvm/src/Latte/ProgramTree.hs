{-# LANGUAGE ViewPatterns #-}

module Latte.ProgramTree (
  Module(..),
  Clazz(..),
  Field(..),
  Method(..),
  Stmt(..),
  Exp(..),
  Type(..),
  isMain,
  getType,
  mkType,
  Int32(..),
  SourcePos(..)
) where

import Data.Int (Int32(..))
import Data.List (isSuffixOf)

import Text.Parsec (SourcePos)

data Module = Module SourcePos [String] [Clazz] [Method] deriving (Show)

data Clazz = Clazz SourcePos Name Name [Name] [Field] [Method] deriving (Show)

data Field = Field SourcePos Type Name deriving (Show)

data Method = Method SourcePos Type Name [(Type, Name)] [Stmt] deriving (Show)

isMain :: Method -> Bool
isMain (Method _ IntT "main" [] _) = True
isMain _ = False

data Stmt = AssignS SourcePos Var Exp 
  | BlockS SourcePos [Stmt] 
  | CondS SourcePos Exp Stmt (Maybe Stmt) 
  | DecS SourcePos Type [(Name, Maybe Exp)] 
  | ExpS SourcePos Exp 
  | ForS SourcePos Type Name Var Stmt
  | PredS SourcePos Var 
  | SuccS SourcePos Var 
  | ReturnS SourcePos (Maybe Exp) 
  | WhileS SourcePos Exp Stmt
  | EmptyS deriving (Show)

data Exp = VarE Type SourcePos Var
  | IntE SourcePos Int32
  | BoolE SourcePos Bool 
  | StringE SourcePos String
  | NullE SourcePos 
  | UnaryE Type SourcePos Op Exp 
  | BinaryE Type SourcePos Op Exp Exp 
  | CallE Type SourcePos Var [Exp]
  | CastE SourcePos Type Exp
  | NewArrE SourcePos Type Exp 
  | NewObjE SourcePos Type deriving (Show)
  
getType :: Exp -> Type
getType (VarE t _ _) = t
getType (IntE _ _) = IntT
getType (BoolE _ _) = BoolT
getType (StringE _ _) = StringT
getType (NullE _) = NullT
getType (UnaryE t _ _ _) = t
getType (BinaryE t _ _ _ _) = t
getType (CallE t _ _ _) = t
getType (CastE _ t _) = t
getType (NewArrE _ t _) = ArrayT t
getType (NewObjE _ t) = t
  
type Var = [(Name, Maybe Exp)]
type Name = String
type Op = String
  
data Type = AnyT 
  | BoolT 
  | IntT 
  | StringT 
  | VoidT 
  | NullT
  | ArrayT Type 
  | ClassT String deriving (Ord)

mkType :: String -> Type
mkType s
  | "[]" `isSuffixOf` s = ArrayT $ mkType $ take (length s - 2) s 
  | otherwise = case s of
      "boolean" -> BoolT
      "int" -> IntT
      "string" -> StringT
      "void" -> VoidT
      custom -> ClassT custom

instance Eq Type where
  (==) AnyT _ = True
  (==) _ AnyT = True
  (==) BoolT BoolT = True
  (==) IntT IntT = True
  (==) StringT StringT = True
  (==) VoidT VoidT = True
  (==) (ArrayT t1) (ArrayT t2) = t1 == t2
  (==) (ClassT s1) (ClassT s2) = s1 == s2
  (==) _ _ = False

instance Show Type where
  show AnyT = "any"
  show BoolT = "boolean"
  show IntT = "int"
  show StringT = "string"
  show VoidT = "void"
  show NullT = "null"
  show (ArrayT t) = show t ++ "[]"
  show (ClassT s) = s 
