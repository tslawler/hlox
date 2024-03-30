module HLox.Data.Value (
    Value(..), typeMatch
) where

data Value
    = VStr !String
    | VNum !Double
    | VBool !Bool
    | VNil
    deriving (Eq)

instance (Show Value) where
    show (VStr str) = show str
    show (VNum d) =
        let (n,k) = properFraction d
        in if k == 0.0 then show (n :: Integer) else show d
    show (VBool True) = "true"
    show (VBool False) = "false"
    show VNil = "nil"

typeMatch :: Value -> Value -> Bool
typeMatch (VStr _) (VStr _) = True
typeMatch (VNum _) (VNum _) = True
typeMatch (VBool _) (VBool _) = True
typeMatch VNil VNil = True
typeMatch _ _ = False