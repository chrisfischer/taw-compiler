module PrettyAst where

import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP

import Ast

-- PP Class and Instances

class PP a where
  pp :: a -> Doc

instance PP Ty where
  pp TBool    = PP.text "bool"
  pp TInt     = PP.text "int"
  pp (TRef r) = pp r

instance PP Rty where
  pp (RFun tys retty) = PP.hsep [
    PP.parens $ PP.hsep $ PP.punctuate PP.comma (map pp tys),
    PP.text "->",
    pp retty]

instance PP Retty where
  pp (RetVal ty) = pp ty
  pp RetVoid     = PP.text "void"

instance PP Unop where
  pp Neg = PP.char '-'
  pp Lognot = PP.char '!'

instance PP Binop where
  pp Add  = PP.char '+'
  pp Sub  = PP.char '-'
  pp Mul  = PP.char '*'
  pp Div  = PP.char '/'
  pp Eq   = PP.text "=="
  pp Neq  = PP.text "!="
  pp Lt   = PP.char '<'
  pp Lte  = PP.text "<="
  pp Gt   = PP.char '>'
  pp Gte  = PP.text ">="
  pp And  = PP.text "&&"
  pp Or   = PP.text "||"
  pp IAnd = PP.char '&'
  pp IOr  = PP.char '|'
  pp Shl  = PP.text "<<"
  pp Shr  = PP.text ">>"
  pp Mod  = PP.text "%"

instance PP Exp where
  pp (CBool True)      = PP.text "true"
  pp (CBool False)     = PP.text "false"
  pp (CInt  n)         = PP.int n
  pp (Id    id)        = PP.text id
  pp (Call (Node fn _) nexps) =
    let args = map (pp . elt) nexps in
    pp fn <> PP.parens (PP.hsep $ PP.punctuate PP.comma args)
  pp (Bop op (Node x _) (Node y _)) = PP.hsep [px, pp op, py] where
    px = if expLevel x < level op then PP.parens ppx else ppx
    py = if expLevel y < level op then PP.parens ppy else ppy
    ppx = pp x
    ppy = pp y
  pp (Uop op (Node e _)) = PP.hcat [pp op, p] where
    p = if expLevel e < ulevel op then PP.parens ppe else ppe
    ppe = pp e

instance PP Vdecl where
  pp (Vdecl id nexp) =
    PP.hsep [PP.text "var", PP.text id, PP.char '=', pp $ elt nexp]

instance PP Stmt where
  pp (Assn lhs rhs) =
    PP.hsep [pp (elt lhs), PP.char '=', pp (elt rhs) <> PP.semi]
  pp (Decl vdecl) = pp vdecl <> PP.semi
  pp (Ret e) = (PP.text "return" <+> ppe) <> PP.semi where
    ppe = maybe PP.empty (pp . elt) e
  pp (SCall nexp nexps) =
    let exp  = elt nexp
        exps = map (pp . elt) nexps in
    pp exp <> PP.parens (PP.hsep $ PP.punctuate PP.comma exps) <> PP.semi
  pp (If nexp b mb) =
    let topLine = PP.hsep [PP.text "if", PP.parens $ pp $ elt nexp, PP.lbrace]
        mid     = PP.nest 2 (printBlock b)
        end     = case mb of
                    Nothing -> [PP.rbrace]
                    Just b' ->
                      [ PP.rbrace <+> PP.text "else" <+> PP.lbrace,
                        PP.nest 2 (printBlock b'),
                        PP.rbrace ] in
    PP.vcat $ topLine : mid : end
  pp (For decls cond iter b) =
    let exp = maybe PP.empty (pp . elt) cond <> PP.semi
        smt = maybe PP.empty (pp . elt) iter
        decs = PP.hsep (PP.punctuate PP.comma (map pp decls))
        parenPart = (decs PP.<> PP.semi) <+> exp <+> smt in
    PP.vcat [
      PP.hsep [PP.text "for", PP.parens parenPart, PP.lbrace],
      PP.nest 2 (printBlock b),
      PP.rbrace ]
  pp (While nexp b) =
    PP.vcat [
      PP.hsep [PP.text "while", PP.parens $ pp (elt nexp), PP.lbrace],
      PP.nest 2 (printBlock b),
      PP.rbrace ]

instance PP Fdecl where
  pp (Fdecl r f as b) =
    let args = map (\(t,i) -> pp t <+> PP.text i) as in
    PP.vcat [
      PP.hsep [
        pp r,
        PP.text f <> PP.parens (PP.hsep $ PP.punctuate PP.comma args),
        PP.lbrace ],
      PP.nest 2 (printBlock b),
      PP.rbrace ]

instance PP Fext where
  pp (Fext r i as) =
    let args = map (\(t,i) -> pp t <+> PP.text i) as in
    PP.hsep [
      pp r,
      PP.text i <> PP.parens (PP.hsep $ PP.punctuate PP.comma args) <>
        PP.semi ]

instance PP Decl where
  pp (Gfdecl x) = pp (elt x)
  pp (Gfext x)  = pp (elt x)

-- Helpers

printBlock :: Block -> Doc
printBlock b = PP.vcat $ map (pp . elt) b

oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

indented :: PP a => a -> String
indented = PP.render . pp

expLevel :: Exp -> Int
expLevel (Bop bop _ _) = level bop
expLevel (Uop uop _) = ulevel uop
expLevel _ = 10

-- | Use the C++ precedence table
level :: Binop -> Int
level Mul = 7
level Div = 7
level Add = 5
level Sub = 5
level _   = 3

ulevel :: Unop -> Int
ulevel _ = 9

-- | Pretty print a Taw program
renderProg :: Prog -> String
renderProg (Prog prog) = PP.render . PP.vcat $ map ((<> PP.char '\n') . pp) prog

instance Show Prog where
  show = renderProg