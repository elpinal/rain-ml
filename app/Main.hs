module Main where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B

import Options.Applicative

import Language.RainML.CodeGen
import qualified Language.RainML.Intermediate as I
import Language.RainML.Parser
import Language.RainML.Version

main :: IO ()
main = run

data ParseException = ParseException String

instance Show ParseException where
  show (ParseException s) = s

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
  liftIO $ B.writeFile outfp $ codeGen $ I.translate tm

programName :: String
programName = "rain-ml"

showVersion :: MonadIO m => m ()
showVersion = liftIO $ putStrLn $ programName ++ " " ++ rainmlVersion
