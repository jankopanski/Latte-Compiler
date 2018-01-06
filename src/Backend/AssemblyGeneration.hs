module Backend.AssemblyGeneration where

import System.FilePath (replaceExtension)

import Backend.IntermediateCodeGeneration
import Parser.AbsLatte

-- runAssemblyPrinter :: ImmediateCode -> IO ()
-- runAssemblyPrinter code = mapM_ printFunction (functions code)
--
-- printFunction :: (Ident, [Instruction]) -> IO ()
-- printFunction (Ident s, ins) = putStrLn (s ++ ":") >> mapM_ print ins >> putStrLn ""

generateAssembly :: ImmediateCode -> IO String
generateAssembly code = return $ unlines $ concatMap generateFunction (functions code)

generateFunction :: (Ident, [Instruction]) -> [String]
generateFunction (Ident s, ins) = (".globl " ++ s) : (s ++ ":") : map show ins

generateFile :: FilePath -> String -> IO ()
generateFile file = writeFile (replaceExtension file "s")
