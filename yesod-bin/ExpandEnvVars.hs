{-# LANGUAGE RecordWildCards #-}

module ExpandEnvVars
(
    expandEnvVars
)
where

import Data.Char
import Data.Functor.Compose
import Data.Maybe
import System.Environment
import Text.Regex.Applicative

data EnvMatch = EnvMatch {
    envMatchName :: String
  , envMatchDefault :: String
}

envPattern :: RE Char EnvMatch
envPattern = EnvMatch
    <$ string "_env:"
    <*> token
    <* string ":"
    <*> token
    where
        token :: RE Char String
        token = many (psym $ \c -> isAlphaNum c || c == '_')

expandEnv :: EnvMatch -> IO String
expandEnv EnvMatch{..} = fmap (fromMaybe envMatchDefault) (lookupEnv envMatchName)

envTransform :: RE Char (IO String)
envTransform = getCompose . (concat <$>) . sequenceA . map Compose $
    [
        pure <$> many anySym
      , expandEnv <$> envPattern
      , pure <$> many anySym
    ]

runEnvTransform :: String -> IO (Maybe String)
runEnvTransform = sequenceA . match envTransform

expandEnvVars :: String -> IO String
expandEnvVars s = fmap fromJust (runEnvTransform s)

