module Backend.AssemblyGeneration where

import System.FilePath (replaceExtension)

import Parser.AbsLatte
import Backend.IntermediateCodeGeneration

generateAssembly :: ImmediateCode -> IO String
generateAssembly code = return $ unlines $ concatMap generateFunction (functions code)

generateFunction :: (Ident, [Instruction]) -> [String]
generateFunction (Ident s, ins) = (".globl " ++ s) : (s ++ ":") : map show ins

generateFile :: FilePath -> String -> IO ()
generateFile file = writeFile (replaceExtension file "s")
