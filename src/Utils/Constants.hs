module Utils.Constants (wrongUsage, parsingError, staticAnalysisError, okMsg, errorMsg) where

wrongUsage :: String
wrongUsage = "Usage: \"./latc_x86 filename\""

parsingError :: String
parsingError = "Parsing error:"

staticAnalysisError :: String
staticAnalysisError = "Static analysis error:"

okMsg :: String
okMsg = "OK"

errorMsg :: String
errorMsg = "ERROR"