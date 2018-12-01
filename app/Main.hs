module Main where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.ByteString as B

import Language.RainML.Parser
import Language.RainML.CodeGen

main :: IO ()
main = run

data ParseException = ParseException FilePath
  deriving Show

instance Exception ParseException

run :: (MonadIO m, MonadThrow m) => m ()
run = do
  let fp = "input.rml"
  let outfp = "output.rvm"
  compile fp outfp

compile :: (MonadIO m, MonadThrow m) => FilePath -> FilePath -> m ()
compile fp outfp = do
  content <- liftIO $ readFile fp
  n <- maybe (throwM $ ParseException fp) return $ parse content
  liftIO $ B.writeFile outfp $ codeGen n
