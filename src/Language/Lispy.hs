module Language.Lispy where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec

import Control.Monad.Except
import Control.Monad.Reader

import Language.Lispy.Syntax
import Language.Lispy.Parser
import Language.Lispy.Interpreter

parseLispy :: Text -> Either ParseError Expr
parseLispy = parse expr "<test>"

run :: Text -> Either (Either ParseError Err) Val
run input = case parseLispy input of
    Left err -> Left (Left err)
    Right program -> case runReader (runExceptT $ runLispy program) defaultEnv of
        Left err -> Left (Right err)
        Right val -> Right val

runIO :: IO ()
runIO = do
    input <- T.pack <$> getLine
    case run input of
        Left (Left err) -> putStr "Parse error: " >> print err
        Left (Right err) -> putStr "Lispy error: " >> print err
        Right val -> print val