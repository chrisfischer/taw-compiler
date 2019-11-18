module Ast where

import Data.Int

data Node a = Node { elt :: a, loc :: Int }

type Id = String

data Ty =
   TBool
 | TInt
-- | TRef of rty
-- | TNullRef of rty

data Rty =
-- | RString
-- | RStruct of id
-- | RArray of ty
  RFun [Ty] Retty

data Retty =
    RetVoid
  | RetVal Ty

data Unop =
   Neg
 | Lognot
 | Bitnot

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

-- add binops for floats

data Exp =
-- | CNull of rty
   CBool Bool
 | CInt Int64
-- | CStr of string
 | Id Id
-- | CArr of ty * exp node list
-- | NewArr of ty * exp node * id * exp node
-- | Index of exp node * exp node
-- | Length of exp node
-- | CStruct of id * (id * exp node) list
-- | Proj of exp node * id
 | Call (Node Exp) [Node Exp]
 | Bop Binop (Node Exp) (Node Exp)
 | Uop Unop (Node Exp)

-- type cfield = id * exp node

data Vdecl = VDecl Id (Node Exp)

data Stmt = 
   Assn (Node Exp) (Node Exp)
 | Decl Vdecl
 | Ret (Maybe (Node Exp))
 | SCall (Node Exp) [Node Exp]
 | If (Node Exp) [Node Stmt] [Node Stmt]
-- | Cast of rty * id * exp node * stmt node list * stmt node list
 | For [Vdecl] (Maybe (Node Exp)) (Maybe (Node Stmt)) [Node Stmt]
 | While (Node Exp) [Node Stmt]

type Block = [Node Stmt]

-- type gdecl = { name : id; init : exp node }

data Fdecl = Fdecl { retty :: Retty, fname :: Id, args :: [(Ty, Id)], body :: Block }

-- data Field = Field { fieldName :: Id, ftyp :: Ty }

-- type Tdecl = [(Id, Field)]

newtype Decl =
-- | Gvdecl of gdecl node
   Gfdecl (Node Fdecl)
-- | Gtdecl (Node Tdecl)

type Prog = [Decl]

