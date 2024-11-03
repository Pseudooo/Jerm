
module CommandArguments (Arguments, commandArguments, opts, parseCommandArgs) where

import Options.Applicative


data Arguments = Arguments {
	source :: String,
	output :: String 
} deriving (Show)

parseCommandArgs :: IO (Arguments)
parseCommandArgs = execParser opts

opts :: ParserInfo Arguments
opts = info (commandArguments <**> helper)
	(fullDesc
	  <> progDesc "Build Jerm sourcecode into Jerm byte code"
	  <> header "Jerm Compiler")

commandArguments :: Parser Arguments
commandArguments =
		Arguments <$> strOption (long "source" <> help "Source file to build")
		<*> strOption (long "output" <> help "Output build")

