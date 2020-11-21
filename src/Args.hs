module Arg where

import Options.Applicative

data Args = Args
  { argsVerbose :: Bool,
    argsInputFile :: FilePath,
    argsOutputFile :: FilePath
  }

argsParser :: Parser Args
argsParser =
  Args
    <$> switch (short 'v' <> long "verbose")
    <*> strArgument (metavar "INPUT")
    <*> strOption (short 'o' <> long "output" <> value "a.out")
