module Backend.AssemblyGeneration where

import System.FilePath (replaceExtension, dropExtension)
import System.Process (callCommand)

import Backend.IntermediateCodeGeneration
import Parser.AbsLatte

generateAssembly :: ImmediateCode -> IO String
generateAssembly code = return $ unlines $ concatMap generateFunction (functions code)

generateFunction :: (Ident, [Instruction]) -> [String]
generateFunction (Ident s, ins) = (".globl " ++ s) : (s ++ ":") : map show ins

generateFile :: FilePath -> String -> IO ()
generateFile file = writeFile (replaceExtension file "s")

generateExecutable :: FilePath -> IO ()
generateExecutable file = callCommand $
  "gcc -m32 " ++ replaceExtension file "s" ++ " -o " ++ dropExtension file
