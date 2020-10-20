{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
module Main where

import           Prelude                                          hiding ( readFile)

import           Data.Bifunctor                                          ( first)
import           Data.Void
import qualified Data.Set                                   as Set
import           Data.List                                               ( isPrefixOf)
import           Data.Text                                               ( Text)
import           Data.Foldable                                           ( foldrM)
import qualified Data.Text                                  as T
import           Data.Text.IO                                            ( readFile)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Control.Monad                                           (zipWithM_)
import           Control.Monad.State.Strict
import           Control.Monad.Except

import qualified Options.Applicative                        as O
import           Options.Applicative                                     ( Parser
                                                                         , str
                                                                         , execParser
                                                                         , command
                                                                         , metavar
                                                                         , subparser
                                                                         , info
                                                                         , argument
                                                                         , progDesc
                                                                         , fullDesc
                                                                         , helper
                                                                         , (<**>))

import           Lens.Micro.Platform

import           Text.Megaparsec                                         ( parse
                                                                         , errorBundlePretty
                                                                         , many
                                                                         , eof
                                                                         , Parsec)

import           System.Environment                                      ( getArgs)
import           System.Console.Repline                          hiding  (command)

import           AbstractSyntaxTree
import           Parser                                                  ( parseProgram
                                                                         , exprDefs
                                                                         , typeExprDefs
                                                                         , defDuctives
                                                                         , constructorDefs
                                                                         , destructorDefs
                                                                         , parseDecl
                                                                         , emptyState
                                                                         , parseExpr
                                                                         , parseTypeExpr
                                                                         , ParserState)
import           TypeChecker                                             ( checkProgram
                                                                         , checkProgramPTI
                                                                         , runPTI
                                                                         , runTI
                                                                         , evalInTI
                                                                         , emptyCtx
                                                                         , defCtx
                                                                         , inferTerm
                                                                         , inferType)
import           Eval                                                    ( evalType
                                                                         , evalCtx)
import           PrettyPrinter                                           ( prettyType
                                                                         , prettyKind)


data Opts = Run [String] | Repl [String]

cmdOpts :: Parser Opts
cmdOpts = subparser $
     (command "run" $ Run <$> info (O.many $ argument str (metavar "FILES.."))
                                   (progDesc "Run FILES"))
  <> (command "repl" $ Repl <$> info (O.many $ argument str (metavar "FILES.."))
                                     (progDesc "Open repl where FILES are loaded"))

main :: IO ()
main = execParser (info (cmdOpts <**> helper)
         ( fullDesc
           <> progDesc "Interpreter for depended inductive and coinductive types" ))
       >>= \case
        (Run files) -> evalStateT (mapM_ evalProgram files) []
        (Repl files) -> repl files

type Repl a = HaskelineT (StateT [Decl] IO) a

cmd :: String -> Repl ()
cmd input = do
  decls <- get
  let (resOrError, newDecls) =
        runPTI (liftEither ((:[]) <$> parsePrettyError (evalStateT (parseDecl <* eof)
                                                                   (defsToParserState decls))
                                                        "" (T.pack input))
                >>= (errorNewLn . checkProgramPTI)) decls
  case resOrError of
    Left err -> liftIO $ putDoc err
    Right res -> put (newDecls++decls)
      >> case res of
           [TypedExpr expr _] -> liftIO (putDoc $ pretty expr <> "\n")
           _                  -> pure ()

clear :: [String] -> Repl ()
clear = const (put [])

getType :: [String] -> Repl ()
getType input = do
  decls <- get
  let resOrError =
        runTI (liftEither (parsePrettyError (evalStateT (parseExpr <* eof)
                                                        (defsToParserState decls))
                                            "" (T.pack $ concat input))
               >>= inferTerm >>= (errorNewLn . evalInTI . evalType))
               $ (defCtx .~ decls) emptyCtx
  case resOrError of
    Left err -> liftIO $ putDoc err
    Right res -> liftIO (putDoc $ prettyType res <> "\n")

kind :: [String] -> Repl ()
kind input = do
  decls <- get
  let resOrError =
        runTI (liftEither (parsePrettyError (evalStateT (parseTypeExpr <* eof)
                                                        (defsToParserState decls))
                                            "" (T.pack $ concat input))
               >>= inferType >>= (errorNewLn . evalInTI . evalCtx))
               $ (defCtx .~ decls) emptyCtx
  case resOrError of
    Left err -> liftIO . putDoc $ err
    Right res -> liftIO (putDoc $ prettyKind res <> "\n")


parsePrettyError :: Parsec Void Text a -> String -> Text -> Either (Doc ann) a
parsePrettyError parser file input = first (pretty . errorBundlePretty)
                                           (parse parser file input)

defsToParserState :: [Decl] -> ParserState
defsToParserState []                                    =
  emptyState
defsToParserState (ExprDef{..}               : decls)   =
  over exprDefs (Set.insert name) (defsToParserState decls)
defsToParserState (TypeDef od@ OpenDuctive{..} : decls) =
  let structors = Set.fromList $ map strName strDefs
  in (case inOrCoin of
       IsIn -> over constructorDefs (Set.union structors)
       IsCoin -> over destructorDefs (Set.union structors))
     (over typeExprDefs (Set.insert nameDuc)
      $ over defDuctives (od:) (defsToParserState decls))
defsToParserState (_                          : decls)  =
  defsToParserState  decls

getDefs :: [Decl] -> [Text]
getDefs []                                = []
getDefs (ExprDef{..}             : decls) = name : getDefs decls
getDefs (TypeDef OpenDuctive{..} : decls) = map strName strDefs
                                           ++ nameDuc : getDefs decls
getDefs (_                       : decls) = getDefs decls

-- Prefix tab completeter
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":l"    , fileCompleter)
  , (":load" , fileCompleter)
  ]

-- Default tab completer
byWord :: (Monad m, MonadState [Decl] m) => WordCompleter m
byWord n = gets $ filter (isPrefixOf n)
              . map T.unpack
              . (++ ["rec", "corec", "data", "codata", "to", "where"])
              . getDefs

-- Commands
load :: [String] -> Repl ()
load = mapM_ (dontCrash . lift . evalProgram)

evalProgram :: String -> StateT [Decl] IO ()
evalProgram file = do
  program <- liftIO $ readFile file
  decls <- get
  let (resOrError, newDecls) =
        runPTI (liftEither
                  (parsePrettyError (evalStateT (many parseDecl
                                                 <* eof)
                                                (defsToParserState decls))
                                     file program)
                   >>= (errorNewLn . checkProgramPTI)) decls
  either (liftIO . putDoc)
         (\res -> put (newDecls ++ decls)
                  >> liftIO (putDoc $ vsep (map pretty res) <> if null res
                                                               then ""
                                                               else "\n"))
         resOrError

errorNewLn :: MonadError (Doc ann) m => m a -> m a
errorNewLn  = flip catchError (throwError . (<> "\n"))

help :: [String] -> Repl ()
help _ = liftIO . putStr $ unlines
  [ "Type :h[elp] for help"
  , "Type :l[oad] [file..] to load a file"
  , "Type :t[ype] <expr> to get the type of a expression"
  , "Type :k[ind] <expr> to get the kind of a expression"
  , "Type :c[lear] to clear defintions"
  , "Type :q[uit] to close the repl" ]

opts :: [(String, [String] -> Repl ())]
opts = [
    ("help", help) -- :help
  , ("h", help) -- :h
  , ("load", load) -- :load
  , ("l", load) -- :l
  , ("type", getType) -- :type
  , ("t", getType) -- :t
  , ("kind", kind) -- :kind
  , ("k", kind) -- :k
  , ("clear", clear) -- :clear
  , ("c", clear) -- :c
  , ("quit", const abort) -- :quit
  , ("q", const abort) -- :q
  ]

ini :: [String] -> Repl ()
ini files = liftIO (putStrLn "Repl :h for help") >> load files

repl :: [String] -> IO ()
repl files = flip evalStateT []
             $ evalRepl (pure ">>> ") cmd opts (Just ':')
                        (Prefix (wordCompleter byWord) defaultMatcher)
                        (ini files)
