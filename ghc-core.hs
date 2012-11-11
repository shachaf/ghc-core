{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Module    : ghc-core
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability: Needs a few libraries from hackage.
--
--------------------------------------------------------------------
--
-- Inspect the optimised core and assembly produce by GHC.
--
-- Examples:
--
-- > ghc-core zipwith
--
-- > ghc-core -fvia-C zipwith
--

------------------------------------------------------------------------

import Control.Applicative
import Control.Exception as E
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Text.Regex.PCRE.Light.Char8

-- BSD-licensed Haskell syntax highlighting, based on Programmatica
import Language.Haskell.Colorize

{-
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
-- 'Literate' is hscolour-1.11 only. 'Bool' was used in hscolour-1.10
import Language.Haskell.HsColour.Options (Literate(..))
-}

------------------------------------------------------------------------
--
-- Command line parsing
--

data Options = Options
  { optHelp   :: Bool
--  , optFormat :: Output
  , optGhcExe :: String
  , optAsm    :: Bool
  , optSyntax :: Bool
  , optCast   :: Bool
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { optHelp    = False
--  , optFormat  = TTY
  , optGhcExe  = "ghc"
  , optAsm     = True
  , optSyntax  = True
  , optCast    = True
  }

-- formats :: [(String, Output)]
-- formats = [("css", CSS), ("html", HTML), ("tty", TTY)]

options :: [OptDescr (Options -> Options)]
options =
    [ --  Option ['f'] ["format"]
      --      (ReqArg (\x opts -> opts { optFormat = fromString x }) "FORMAT")
      --      ("Output format " ++ formats' ++ ".")
         Option ['h'] ["help"]
            (NoArg (\opts -> opts { optHelp = True }))
            "Print this help message."
        ,Option [] ["with-ghc"]
            (ReqArg (\x opts -> opts { optGhcExe = x }) "PROGRAM")
            "Ghc executable to use."
        ,Option [] ["no-asm"]
            (NoArg (\opts -> opts { optAsm = False }))
            "Don't output generated assembly code."
        ,Option [] ["no-syntax"]
            (NoArg (\opts -> opts { optSyntax = False }))
            "Don't colorize generated code."
        ,Option [] ["no-cast"]
            (NoArg (\opts -> opts { optCast = False }))
            "Don't output calls to cast in generated code."
    ]
    where
{-      fromString  f = fromMaybe (formatError f) (lookup f formats)
        formatError f = error $
                            "invalid format `" ++ f ++ "'"
                            ++ ", must be one of " ++ formats' ++ "."
        formats' = "(" ++ concat (intersperse ", " (map fst formats)) ++ ")"
-}

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
    case getOpt RequireOrder options argv of
        (o, n, []) -> let o' = foldl (flip ($)) defaultOptions o in
                        if optHelp o'
                            then do hPutStr stderr (usageInfo header options)
                                    exitWith ExitSuccess
                            else return (o', n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: ghc-core [OPTION...] [--] [GHC_OPTION...] [files...]"

isExtCoreFile :: FilePath -> Bool
isExtCoreFile = (== ".hcr") . takeExtension
------------------------------------------------------------------------

main :: IO ()
main = do
    -- Parse command line
    (opts, args) <- getArgs >>= parseOptions

    -- Read colors from .hscolour
--    colourPrefs <- readColourPrefs

    mv <- getEnvMaybe "PAGER"
    let less = case mv of Just s -> case s of "less" -> "less -f" ; _ -> s
                          _      -> "less -f"

    (strs, tmps) <- case args of
                      [fp] | isExtCoreFile fp -> do
                                contents <- readFile fp
                                return (contents, Nothing)
                      _ -> do
                        strs1 <- compileWithCore (optGhcExe opts)
                                 args (optAsm opts) (not (optCast opts))
                        let strs2 = polish strs1
                    -- TODO this is a bit lazy on my part...
                        x <- readProcessWithExitCode "sh" ["-c","ls /tmp/ghc*/*.s | head -1"] []
                        case x of
                          (ExitFailure _, _, _) -> return (strs2, Nothing)
                          (ExitSuccess  , s, _) -> if "-fvia-C" `elem` args || "-fllvm" `elem` args
                                       then do asm <- readFile (init s)
                                               return ((strs2 ++ asm), Just $ takeDirectory s)
                                       else return (strs2, Just $ takeDirectory s)

{-
    -- If we replace the 'NoLit' constructor with 'False' (and
    -- remove the include for the Literate type), then this will
    -- work with older hscolour-1.10.* versions.
    let nice = hscolour
                (optFormat opts)
                colourPrefs False True NoLit [] strs
-}
    let nice | optSyntax opts = render ansiLight strs []
             | otherwise      = strs

    bracket
        (openTempFile "/tmp" "ghc-core-XXXX.hcr")
        (\(f,h) -> do hClose h
                      removeFile f
                      case tmps of
                        Just g -> system ("rm -rf " ++ g) >> return ()
                        _      -> return ()
        )
        (\(f,h) -> do
            hPutStrLn h nice >> hFlush h
            e <- system $ less ++ " -r " ++ f
            exitWith e)

--
-- Clean up the output with some regular expressions.
--
polish :: String -> String
polish = unlines . dups . map polish' . lines
    where
        polish' [] = []
        polish' s
            | Just [_,a,b] <- match name  s [] = polish' (a ++ b)
            | Just [_,a,b] <- match local s [] = polish' (a ++ b)
            | isJunk s                         = ""
            | otherwise                        = s

        -- simplify some qualified names
        name  = compile
                 "^(.*)GHC\\.[^\\.]*\\.(.*)$"
                 [ungreedy]

        local = compile
                 "^(.*)Main\\.(.*)$"
                 [ungreedy]

        isJunk s = any (\r -> isJust (match r s [])) junks

        junks  = map (\r -> compile r [])
                    [ "^.GlobalId"
                    , "^.Arity .*"
                    , "^Rec {|^end Rec"
                    , "DmdType"
                    , "NoCafRefs"
                    , "^\\[\\]$"
                    ]

        -- remove duplicate blank lines
        dups []         = []
        dups ([]:[]:xs) = dups ([]:xs)
        dups (x:xs) = x : dups xs

------------------------------------------------------------------------

compileWithCore :: String -> [String] -> Bool -> Bool -> IO String
compileWithCore ghc opts asm suppressCasts = do
    let args = words "-O2 -keep-tmp-files -ddump-simpl -ddump-simpl-stats -fforce-recomp --make"
                ++ (if asm then ["-ddump-asm"] else [])
                ++ (if suppressCasts then ["-dsuppress-coercions"] else [])

    x <- readProcessWithExitCode ghc (args ++ opts) []
    case x of
         (err@(ExitFailure _),str,std) -> do
            mapM_ putStrLn (lines str)
            mapM_ putStrLn (lines std)
            hPutStrLn stderr ("GHC failed to compile " ++ show err)
            exitWith (ExitFailure 1) -- fatal

         (ExitSuccess, str, _)      -> return str

------------------------------------------------------------------------

-- Safe wrapper for getEnv
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe name = handle (\(_::SomeException) -> return Nothing) (Just <$> getEnv name)
