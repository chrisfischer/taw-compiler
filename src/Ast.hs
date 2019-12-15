module Ast where

-- | Groups a value with a line number for better, more specific errors
data Node a = Node { elt :: a, loc :: Loc } deriving (Show, Eq)

-- | Helper method to create a Node with no location
noLoc :: a -> Node a
noLoc a = Node a 0

type Id = String

type Loc = Int

data Ty =
    TBool
  | TInt
  | TRef Rty
  deriving (Show, Eq)

instance Ord Ty where
  (<=) = \x y -> show x <= show y

data Rty =
  RFun [Ty] Retty
  deriving (Show, Eq)

data Retty =
    RetVal Ty
  | RetVoid
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
  | Ret (Maybe (Node Exp))
  | SCall (Node Exp) [Node Exp]
  | If (Node Exp) Block (Maybe Block)
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

-- | Name of the function the interpreter and compiler will start at
entryFunctionName :: Id
entryFunctionName = "main"

-- | Returns the function name from a global decl
nameFromDecl :: Decl -> String
nameFromDecl (Gfdecl (Node (Fdecl _ id _ _) _)) = id
nameFromDecl (Gfext (Node (Fext _ id _) _)) = id
