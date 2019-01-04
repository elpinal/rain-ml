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

data CompileException
  = SyntaxError SyntaxError
  | ExternalTypeError TypeError
  | IntermediateTypeError I.TypeError -- This kind of errors indicates bugs of this compiler.
  | TranslateError Asm.TranslateError

instance Show CompileException where
  show (SyntaxError e)           = "syntax error: " ++ show e
  show (ExternalTypeError e)     = "type error: " ++ display e
  show (IntermediateTypeError e) = "[bug] internal type error: " ++ show e
  show (TranslateError e)        = "translate error: " ++ show e

instance Exception CompileException

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
    Build fp outfp -> compileFile fp outfp

orThrow ::  (MonadThrow m, Exception e) => (a1 -> e) -> Either a1 a2 -> m a2
orThrow c = either (throwM . c) return

compileFile :: (MonadIO m, MonadThrow m) => FilePath -> FilePath -> m ()
compileFile fp outfp = liftIO (readFile fp) >>= compile fp >>= liftIO . B.writeFile outfp

compile :: MonadThrow m => FilePath -> String -> m B.ByteString
compile fp content = do
  tm <- orThrow SyntaxError $ parseString fp content
  orThrow ExternalTypeError $ typecheck tm

  let inter = I.translate $ fromPositional tm
  orThrow IntermediateTypeError $ I.typecheck inter

  fmap codeGen $ orThrow TranslateError $ Asm.toAsm inter

programName :: String
programName = "rain-ml"

showVersion :: MonadIO m => m ()
showVersion = liftIO $ putStrLn $ programName ++ " " ++ rainmlVersion
