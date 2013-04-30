{-# LANGUAGE FlexibleInstances #-}
module Complexity.MassiveAST where

import Language.Python.Common.AST
import Complexity.Massive

instance Massive ModuleSpan where
  mass coef (Module stmts) = mass coef stmts

instance Massive StatementSpan where
  mass coef (Import items _) = mass coef items
  mass coef (FromImport _ items _) = mass coef items

  mass coef (While cond body else_ _) = Simple coef : masses
    where masses = concatMass3 (coef + 0.1) cond body else_

  mass coef (For targets gen body else_ _) = Simple coef : masses
    where masses = concatMass4 (coef + 0.1) targets gen body else_

  mass coef (Fun (Ident name _) args annot body span) = [Func name masses span]
    where masses = concatMass3 coef args annot body

  mass coef (Class name args body _) = concatMass3 coef name args body
  mass coef (Conditional guards else_ _) = concatMass2 coef guards else_
  mass coef (Assign to expr _) = concatMass2 coef to expr
  mass coef (AugmentedAssign to _ expr _) = concatMass2 coef to expr
  mass coef (Decorated decorators def _) = concatMass2 coef decorators def
  mass coef (Return expr _) = mass coef expr
  mass coef (Try body excepts else_ finally _) = concatMass4 coef body excepts else_ finally
  mass coef (Raise expr _) = mass coef expr
  mass coef (With context body _) = concatMass2 coef context body
  mass coef (Delete exprs _) = mass coef exprs
  mass coef (StmtExpr expr _) = mass coef expr
  mass coef (Global idents _) = mass (coef + 0.1) idents
  mass coef (NonLocal idents _) = mass (coef + 0.1) idents
  mass coef (Assert exprs _) = mass coef exprs
  mass coef (Print _ exprs _ _) = mass coef exprs
  mass coef (Exec expr envs _) = concatMass2 coef expr envs

  mass coef stmt = []

instance Massive ImportItemSpan where
  mass coef (ImportItem name asname _) = concatMass2 coef name asname

instance Massive FromItemsSpan where
  mass coef (ImportEverything _) = [Simple coef]
  mass coef (FromItems items _) = mass coef items

instance Massive FromItemSpan where
  mass coef (FromItem name asname _) = concatMass2 coef name asname

instance Massive ExprSpan where
  mass coef (Call fun args _) = concatMass2 (coef + 0.2) fun args
  mass coef (Subscript subs expr _) = concatMass2 coef subs expr
  mass coef (SlicedExpr slicee slices _) = concatMass2 coef slicee slices
  mass coef (CondExpr true cond false _) = concatMass3 coef true cond false
  mass coef (BinaryOp op left right _) = concatMass3 coef op left right
  mass coef (UnaryOp op expr _) = concatMass2 coef op expr
  mass coef (Lambda args body _) = concatMass2 coef args body
  mass coef (Tuple exprs _) = mass coef exprs
  mass coef (Yield expr _) = mass coef expr
  mass coef (Generator comp _) = mass coef comp
  mass coef (ListComp comp _) = mass coef comp
  mass coef (List exprs _) = mass coef exprs
  mass coef (Dictionary mappings _) = mass coef mappings
  mass coef (DictComp comp _) = mass coef comp
  mass coef (Set exprs _) = mass coef exprs
  mass coef (SetComp comp _) = mass coef comp
  mass coef (Starred expr _) = mass coef expr
  mass coef (Paren expr _) = mass coef expr
  mass coef (StringConversion expr _) = mass coef expr
  mass coef expr = [Simple (coef * 0.25)]

instance Massive DecoratorSpan where
  mass coef (Decorator name args _) = concatMass2 coef name args

instance Massive HandlerSpan where
  mass coef (Handler clause suite _) = concatMass2 coef clause suite

instance Massive ExceptClauseSpan where
  mass coef (ExceptClause clause _) = mass coef clause

instance Massive RaiseExprSpan where
  mass coef (RaiseV3 raise) = mass coef raise
  mass coef (RaiseV2 raise) = mass coef raise

instance Massive ParameterSpan where
  mass coef (Param name annot dflt _) = concatMass3 coef name annot dflt
  mass coef (VarArgsPos name annot _) = concatMass2 coef name annot
  mass coef (VarArgsKeyword name annot _) = concatMass2 coef name annot
  mass coef (UnPackTuple tuple dflt _) = concatMass2 (coef + 0.1) tuple dflt
  mass coef (EndPositional _) = []

instance Massive (ComprehensionSpan ExprSpan) where
  mass coef (Comprehension expr for _) = concatMass2 coef expr for

instance Massive (ComprehensionSpan (ExprSpan, ExprSpan)) where
  mass coef (Comprehension exprs for _) = concatMass2 coef exprs for

instance Massive CompForSpan where
  mass coef (CompFor exprs expr iter _) = concatMass3 coef exprs expr iter

instance Massive CompIterSpan where
  mass coef (IterFor for _) = mass coef for
  mass coef (IterIf if_ _) = mass coef if_

instance Massive CompIfSpan where
  mass coef (CompIf expr iter _) = concatMass2 coef expr iter

instance Massive SliceSpan where
  mass coef (SliceProper lower upper stride _) = concatMass3 coef lower upper stride
  mass coef (SliceExpr expr _) = mass coef expr
  mass coef (SliceEllipsis _) = []

instance Massive ArgumentSpan where
  mass coef (ArgExpr expr _) = mass coef expr
  mass coef (ArgVarArgsPos expr _) = mass coef expr
  mass coef (ArgVarArgsKeyword expr _) = mass coef expr
  mass coef (ArgKeyword keyword expr _) = concatMass2 coef keyword expr

instance Massive ParamTupleSpan where
  mass coef (ParamTupleName name _) = mass coef name
  mass coef (ParamTuple tuple _) = mass (coef + 0.1) tuple

instance Massive OpSpan where
  mass coef op = [Simple (coef * 0.5)]

instance Massive IdentSpan where
  mass coef ident = [Simple coef]
