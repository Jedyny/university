{-# LANGUAGE RecordWildCards #-}

module Latte.Optimizer (mkIfElse) where

import Latte.ProgramTree

mkIfElse :: SourcePos -> Exp -> Stmt -> Maybe Stmt -> Stmt
mkIfElse _ (BoolE _ True) s _ = s
mkIfElse _ (BoolE _ False) _ (Just s) = s 
mkIfElse _ (BoolE _ False) _ Nothing = EmptyS
mkIfElse p e s s' = CondS p e s s'  
