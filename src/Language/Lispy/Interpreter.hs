{-# LANGUAGE
    OverloadedStrings, OverloadedLists,
    RankNTypes,
    LambdaCase,
    FlexibleInstances
    #-}
module Language.Lispy.Interpreter where

import Control.Applicative (liftA2)

import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Bool

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.Reader
import Control.Monad.Except

import Language.Lispy.Syntax

data Val = ValInt !Integer
         | ValFloat !Double
         | ValString !Text
         | ValSymbol !Name
         | ValSExpr !(Vector Val)
         | ValFunction !(Vector Name) Expr
         | ValPartial !(Vector Name) !(Vector Val -> ExceptT Err (Reader Env) Val)
         | ValBuiltin !(Maybe Int) !(Vector Val -> Val)

class PrimVal a where
    toVal :: a -> Val
    fromVal :: Val -> Maybe a

instance PrimVal Integer where
    toVal = ValInt
    fromVal = \case {ValInt i -> Just i; _ -> Nothing}

instance PrimVal Double where
    toVal = ValFloat
    fromVal = \case {ValFloat f -> Just f; ValInt i -> Just (fromIntegral i); _ -> Nothing}

instance PrimVal Text where
    toVal = ValString
    fromVal = \case {ValString s -> Just s; _ -> Nothing}

instance PrimVal (Vector Val) where
    toVal = ValSExpr
    fromVal = \case {ValSExpr vec -> Just vec; _ -> Nothing}

instance PrimVal Bool where
    toVal = bool (ValSymbol "false") (ValSymbol "true")
    fromVal = \case 
        ValSymbol sym ->
            if sym == "true"
            then Just True
            else
                if sym == "false"
                then Just False
                else Nothing
        _ -> Nothing

instance Show Val where
    show (ValInt i) = show i
    show (ValFloat f) = show f
    show (ValString t) = show t
    show (ValSymbol n) = '#' : T.unpack (nameBase n)
    show (ValSExpr vals) = "'(" ++ V.foldr' (\val acc -> show val ++ ' ' : acc) ")" vals
    show (ValFunction args _) = "function" ++ show args
    show (ValPartial args _) = "function" ++ show args
    show (ValBuiltin arity _) = "builtin<" ++ show arity ++ ">"
         
type Env = Map Name Val

type Err = Text

emptyEnv :: Env
emptyEnv = M.empty

defaultEnv :: Env
defaultEnv = M.fromList [
    ("+", ValBuiltin Nothing addMany),
    ("-", ValBuiltin (Just 2) sub),
    ("*", ValBuiltin Nothing multMany),
    ("/", ValBuiltin (Just 2) div),
    ("show", ValBuiltin (Just 1) (ValString . T.pack . show . V.head)),
    ("==", ValBuiltin (Just 2) eq),
    ("typeof", ValBuiltin Nothing typeof),
    ("if", ValBuiltin (Just 3) cond),
    ("match", ValBuiltin Nothing match)
    ]
  where
    addMany = liftBinOpVariadic (+) 0
    sub = liftBinOp (-)
    multMany = liftBinOpVariadic (*) 1
    div = liftBinOpFl (/)
    eq vec = case (vec V.! 0, vec V.! 1) of
        (ValInt i1, ValInt i2) -> toVal $ i1 == i2
        (ValFloat f1, ValFloat f2) -> toVal $ f1 == f2
        (ValString t1, ValString t2) -> toVal $ t1 == t2
        (ValSymbol s1, ValSymbol s2) -> toVal $ s1 == s2
        (ValSExpr v1, ValSExpr v2) -> if V.length v1 == V.length v2
            then case V.foldl' (liftA2 (&&)) (Just True) $ V.zipWith (\x y -> fromVal (eq [x, y])) v1 v2 of
                Just True -> toVal True
                Just False -> toVal False
                Nothing -> nil
            else nil
        _ -> nil
    typeof :: Vector Val -> Val
    typeof vec = case V.length vec of
        0 -> nil
        1 -> type_ (vec V.! 0)
        _ -> ValSExpr $ V.map type_ vec
    type_ (ValInt _) = ValSymbol "int"
    type_ (ValFloat _) = ValSymbol "float"
    type_ (ValString _) = ValSymbol "string"
    type_ (ValSymbol _) = ValSymbol "symbol"
    type_ (ValSExpr vec) = typeof vec
    type_ (ValFunction args _) = ValSymbol . Name $ "function<" <> T.pack (show (V.length args)) <> ">"
    type_ (ValPartial args _) = ValSymbol . Name $ "function<" <> T.pack (show (V.length args)) <> ">"
    type_ (ValBuiltin Nothing _) = ValSymbol "builtin<variadic>"
    type_ (ValBuiltin (Just ar) _) = ValSymbol . Name $ "builtin<" <> T.pack (show ar) <> ">"
    cond vec = case fromVal (vec V.! 0) of
        Just False -> vec V.! 1
        Just True -> vec V.! 2
        _ -> nil
    match vec = if V.length vec == 0 then nil else
        case vec V.! 0 of
            ValSymbol sym -> findBy (critSym sym)
            _ -> nil
      where
        findBy crit = case V.find fst $ V.map crit $ V.tail vec of
            Just r -> snd r
            Nothing -> nil
        critSym sym (ValSymbol s) = (sym == s, ValSymbol s)
        critSym sym (ValSExpr vs) = case V.length vs of
            0 -> (False, nil)
            1 -> case V.head vs of
                ValSymbol s -> (sym == s, ValSymbol s)
                _ -> (False, nil)
            2 -> case V.head vs of
                ValSymbol s -> (sym == s, vs V.! 1)
                _ -> (False, nil)
            _ -> case V.head vs of
                ValSymbol s -> (sym == s, ValSExpr $ V.tail vs)
                _ -> (False, nil)
        critSym _ _ = (False, nil)

liftBinOp :: (forall a.Num a => a -> a -> a) -> Vector Val -> Val
liftBinOp op vec = case vec V.! 0 of
    ValInt i -> case vec V.! 1 of
        ValInt i2 -> ValInt (i `op` i2)
        ValFloat f -> ValFloat (fromIntegral i `op` f)
    ValFloat f -> case vec V.! 1 of
        ValInt i -> ValFloat (f `op` fromIntegral i)
        ValFloat f2 -> ValFloat (f `op` f2)

liftBinOpFl :: (Double -> Double -> Double) -> Vector Val -> Val
liftBinOpFl op vec = ValFloat $ case vec V.! 0 of
    ValInt i -> case vec V.! 1 of
        ValInt i2 -> fromIntegral i `op` fromIntegral i2
        ValFloat f -> fromIntegral i `op` f
    ValFloat f -> case vec V.! 1 of
        ValInt i -> f `op` fromIntegral i
        ValFloat f2 -> f `op` f2
        
liftBinOpVariadic :: (forall a.Num a => a -> a -> a) -> Integer -> Vector Val -> Val
liftBinOpVariadic op z = either ValFloat ValInt . V.foldr' (\arg acc ->
        case arg of
            ValInt i -> case acc of
                Left f -> Left (f `op` fromIntegral i)
                Right i2 -> Right (i `op` i2)
            ValFloat f -> case acc of
                Left f2 -> Left (f `op` f2)
                Right i -> Left (f `op` fromIntegral i)
            _ -> acc) (Right z)

nil :: Val
nil = ValSymbol "nil"

runLispy :: Expr -> ExceptT Err (Reader Env) Val
runLispy (Identifier name) = getVal name
runLispy (Literal lit) = pure $ case lit of
    LitIntegral i -> ValInt i
    LitFloating f -> ValFloat f
    LitString t -> ValString t
    LitSymbol n -> ValSymbol n
runLispy (SExpr exprs) = ValSExpr <$> V.mapM runLispy exprs
runLispy (Apply fe args) = do
    func <- runLispy fe
    case func of
        ValFunction fa body ->
            let numArgs = V.length args
            in case numArgs `compare` V.length fa of
                LT -> makePartial V.empty V.empty body
                EQ -> do
                    argVals <- V.mapM runLispy args
                    let binds = V.zip fa argVals
                        upd = \env -> V.foldl' (\e (k, v) -> M.insert k v e) env binds
                    local upd $ runLispy body
                GT -> throwError "function: too many arguments!"
        ValPartial _ _ -> throwError "partially-applied function: not implemented (how did you get here anyway?)"
        ValBuiltin Nothing f -> f <$> V.mapM runLispy args
        ValBuiltin (Just arity) f -> case V.length args `compare` arity of
            LT -> throwError "partially-applied builtin: not implemented"
            EQ -> f <$> V.mapM runLispy args
            GT -> throwError $ "builtin: too many arguments! (expecting " <> (T.pack $ show arity) <> ")"
        _ -> throwError "not a function"
runLispy (Let name valE body) = do
    val <- runLispy valE
    local (M.insert name val) (runLispy body)
runLispy (Lambda args body) = pure $ ValFunction args body

getVal :: Name -> ExceptT Err (Reader Env) Val
getVal name = do
    mbVal <- asks (M.lookup name)
    case mbVal of
        Nothing -> throwError ("Value " <> nameBase name <> " is not defined!")
        Just val -> pure val

makePartial :: Vector Name -> Vector Val -> Expr -> ExceptT Err (Reader Env) Val
makePartial formalArgs given body = throwError "partially-applied function: not implemented"