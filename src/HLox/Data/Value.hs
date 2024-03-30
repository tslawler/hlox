module HLox.Data.Value (
    Value(..)
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