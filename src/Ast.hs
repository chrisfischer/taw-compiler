module Ast where

data Node a = Node { elt :: a, loc :: Loc } deriving (Show, Eq)

type Id = String

type Loc = Int

data Ty =
    TBool
  | TInt
  deriving (Show, Eq, Enum)

data Rty =
  RFun [Ty] Retty
  deriving (Show, Eq)

data Retty =
    RetVal Ty
  | RetVoid
  deriving (Show, Eq)

data ValueTy =
    VBool Bool
  | VInt Int
  | VFun Id
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
  | Mod
  deriving (Show, Eq, Enum)

data Exp =
    CBool Bool
  | CInt Int
  | Id Id
  | Call (Node Exp) [Node Exp]
  | Bop Binop (Node Exp) (Node Exp)
  | Uop Unop (Node Exp)
  deriving (Show, Eq)

data Vdecl = Vdecl Id (Node Exp) deriving (Show, Eq)

data Stmt =
    Assn (Node Exp) (Node Exp)
  | Decl Vdecl
  | Ret (Node Exp)
  | If (Node Exp) Block Block
  | For [Vdecl] (Maybe (Node Exp)) (Maybe (Node Stmt)) Block
  | While (Node Exp) Block
  | Nop
  deriving (Show, Eq)

type Block = [Node Stmt]

data Fdecl = Fdecl {
    retty :: Retty
  , fname :: Id
  , args :: [(Ty, Id)]
  , body :: Block
  } deriving (Show, Eq)

-- | External function declaration
data Fext = Fext {
    extRetty :: Retty
  , extFname :: Id
  , extArgs :: [(Ty, Id)]
  } deriving (Show, Eq)

data Decl =
    Gfdecl (Node Fdecl)
  | Gfext (Node Fext)
  deriving (Show, Eq)

type Prog = [Decl]

