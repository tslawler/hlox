module HLox.Data.Value (
    Value(..), LoxFun(..), LoxClass(..), ForeignFun(..), typeMatch, truthy, arity
) where

import HLox.Data.Token
import HLox.Data.Stmt
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty(..))
import Data.IORef

data ForeignFun = Clock deriving (Eq)

type Scope = M.Map String (IORef Value)
type Env = NonEmpty Scope

data LoxClass = LoxClass Token [FunDecl]
    deriving (Eq)
instance Show LoxClass where
    show (LoxClass t _) = _lexeme t

data LoxFun = 
    FFI ForeignFun
    | LoxFun { _name :: Token, _params :: [Token], _body :: [Stmt], _closure :: Env }
    deriving (Eq)

arity :: LoxFun -> Int
arity (FFI Clock) = 0
arity (LoxFun _ ps _ _) = length ps
instance Show LoxFun where
    show (FFI Clock) = "<native function clock>"
    show (LoxFun nm _ _ _) = "<function " ++ _lexeme nm ++ ">"

data Value
    = VStr !String
    | VNum !Double
    | VBool !Bool
    | VFun !LoxFun
    | VClass !LoxClass
    | VInstance !LoxClass Scope
    | VNil
    deriving (Eq)

instance (Show Value) where
    show (VStr str) = str
    show (VNum d) =
        let (n,k) = properFraction d
        in if k == 0.0 then show (n :: Integer) else show d
    show (VBool True) = "true"
    show (VBool False) = "false"
    show (VFun f) = show f
    show (VClass c) = show c
    show (VInstance c _) = "<" ++ show c ++ " instance>"
    show VNil = "nil"

typeMatch :: Value -> Value -> Bool
typeMatch (VStr _) (VStr _) = True
typeMatch (VNum _) (VNum _) = True
typeMatch (VBool _) (VBool _) = True
typeMatch VNil VNil = True
typeMatch _ _ = False

truthy :: Value -> Bool
truthy (VBool False) = False
truthy VNil = False
truthy _ = True