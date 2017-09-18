{-# LANGUAGE
    GeneralizedNewtypeDeriving
    #-}
module Language.Lispy.Syntax where

import Data.String
import Data.Vector
import Data.Text (Text)

data Program = Program !Expr

data Expr = Identifier !Name
          | Literal !Literal
          | SExpr !(Vector Expr)
          | Apply !Expr !(Vector Expr)
          | Let !Name !Expr !Expr
          | Lambda !(Vector Name) !Expr
          deriving (Show, Eq)

data Literal = LitIntegral !Integer
             | LitFloating !Double
             | LitString !Text
             | LitSymbol !Name
             deriving (Show, Eq)

instance IsString Literal where fromString = LitString . fromString

newtype Name = Name {nameBase :: Text} deriving (Show, Eq, Ord, IsString)