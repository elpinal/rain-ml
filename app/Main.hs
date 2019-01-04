module Main where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B

import Options.Applicative

import Language.RainML.Asm as Asm
import Language.RainML.CodeGen
import qualified Language.RainML.Intermediate as I
import Language.RainML.Parser
import Language.RainML.Syntax
import Language.RainML.Version

main :: IO ()
main = run

data ParseException
  = ParseException SyntaxError
  | ExternalTypeError TypeError
  | IntermediateTypeError I.TypeError -- This kind of errors indicates bugs of this compiler.
  | TranslateError Asm.TranslateError

instance Show ParseException where
  show (ParseException e)        = "syntax error: " ++ show e
  show (ExternalTypeError e)     = "type error: " ++ display e
  show (IntermediateTypeError e) = "[bug] internal type error: " ++ show e
  show (TranslateError e)        = "translate error: " ++ show e

instance Exception ParseException

data Command
  = Build FilePath FilePath
  | Version
  deriving Show

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "build" $ info buildCommand $ progDesc "Build a program"
  , command "version" $ info versionCommand $ progDesc "Print Rain ML version"
  ]

buildCommand :: Parser Command
buildCommand = Build <$> argument str (metavar "INPUT") <*> argument str (metavar "OUTPUT")

versionCommand :: Parser Command
versionCommand = pure Version

run :: (MonadIO m, MonadThrow m) => m ()
run = do
  x <- liftIO $ customExecParser (prefs showHelpOnEmpty) $ info (commandParser <**> helper) $ fullDesc <> progDesc "A Rain ML compiler." <> header "Rain ML."
  case x of
    Version -> showVersion
    Build fp outfp -> compile fp outfp

compile :: (MonadIO m, MonadThrow m) => FilePath -> FilePath -> m ()
compile fp outfp = do
  content <- liftIO $ readFile fp
  tm <- either (throwM . ParseException) return $ parseString fp content
  either (throwM . ExternalTypeError) return $ typecheck tm
  let inter = I.translate $ fromPositional tm
  either (throwM . IntermediateTypeError) return $ I.typecheck inter
  asm <- either (throwM . TranslateError) return $ Asm.toAsm inter
  liftIO $ B.writeFile outfp $ codeGen asm

programName :: String
programName = "rain-ml"

showVersion :: MonadIO m => m ()
showVersion = liftIO $ putStrLn $ programName ++ " " ++ rainmlVersion
