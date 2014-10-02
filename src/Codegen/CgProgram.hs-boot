module CgProgram (codeGenThread) where
import Opts
import AstExpr
import AstComp
import CgMonad

codeGenThread :: DynFlags -> String -> Comp CTy Ty -> Cg ()
