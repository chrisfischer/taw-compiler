module Ast where

import Data.Int

data Node a = Node { elt :: a, loc :: Int }

type Id = String

data Ty =
   TBool
 | TInt
  deriving (Show, Eq)

data Rty =
  RFun [Ty] Retty
  deriving (Show, Eq)

data Retty =
  RetVal Ty
  deriving (Show, Eq)

data Unop =
   Neg
 | Lognot
  deriving (Show, Eq, Enum)

data Binop =
    Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
  | IAnd
  | IOr
  | Shl
  | Shr
  | Sar
  | Mod
  deriving (Show, Eq, Enum)

data Exp =
    CBool Bool
  | CInt Int64
  | Id Id
  | Call (Node Exp) [Node Exp]
  | Bop Binop (Node Exp) (Node Exp)
  | Uop Unop (Node Exp)
  deriving (Show, Eq)

data Vdecl = VDecl Id (Node Exp) deriving (Show, Eq)

data Stmt =
    Assn (Node Exp) (Node Exp)
  | Decl Vdecl
  | Ret (Maybe (Node Exp))
  | SCall (Node Exp) [Node Exp]
  | If (Node Exp) [Node Stmt] [Node Stmt]
  | For [Vdecl] (Maybe (Node Exp)) (Maybe (Node Stmt)) [Node Stmt]
  | While (Node Exp) [Node Stmt]
  deriving (Show, Eq)

type Block = [Node Stmt]

data Fdecl = Fdecl {
    retty :: Retty
  , fname :: Id
  , args :: [(Ty, Id)]
  , body :: Block
  } deriving (Show, Eq)

newtype Decl =
  Gfdecl (Node Fdecl)
  deriving (Show, Eq)

type Prog = [Decl]

