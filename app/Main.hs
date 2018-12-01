{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.ByteString as B

import Options.Generic

import Language.RainML.CodeGen
import Language.RainML.Parser
import Language.RainML.Version

main :: IO ()
main = run

data ParseException = ParseException FilePath
  deriving Show

instance Exception ParseException

data Command
  = Build FilePath FilePath
  | Version
  | Help
  deriving (Generic, Show)

instance ParseRecord Command

run :: (MonadIO m, MonadThrow m) => m ()
run = do
  (x, help) <- getWithHelp "Rain ML."
  case x of
    Help -> help
    Version -> showVersion
    Build fp outfp -> compile fp outfp

compile :: (MonadIO m, MonadThrow m) => FilePath -> FilePath -> m ()
compile fp outfp = do
  content <- liftIO $ readFile fp
  n <- maybe (throwM $ ParseException fp) return $ parse content
  liftIO $ B.writeFile outfp $ codeGen n

programName :: String
programName = "rain-ml"

showVersion :: MonadIO m => m ()
showVersion = liftIO $ putStrLn $ programName ++ " " ++ rainmlVersion
