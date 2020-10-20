
-- | Dispatch for running all the CLI commands
module Cardano.Unlog.Run
  ( ClientCommand(..)
  , ClientCommandErrors
  , renderClientCommandError
  , runClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as Text

import           Cardano.Unlog.Commands (AnalysisCommand)
import           Cardano.Unlog.Analysis (AnalysisCmdError, renderAnalysisCmdError,
                     runAnalysisCommand)

import           Cardano.Config.Git.Rev (gitRev)
import           Data.Version (showVersion)
import           Paths_locli (version)

-- | Sub-commands of 'locli'.
data ClientCommand =

  -- | Analysis commands
    AnalysisCommand AnalysisCommand

  | DisplayVersion
  deriving Show

data ClientCommandErrors
  = AnalysisError AnalysisCommand AnalysisCmdError
  deriving Show

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand (AnalysisCommand c) = firstExceptT (AnalysisError c) $ runAnalysisCommand c
runClientCommand DisplayVersion = runDisplayVersion

renderClientCommandError :: ClientCommandErrors -> Text
renderClientCommandError (AnalysisError cmd err) =
  renderAnalysisCmdError cmd err

runDisplayVersion :: ExceptT ClientCommandErrors IO ()
runDisplayVersion = do
    liftIO . putTextLn $ mconcat
                [ "locli ", renderVersion version
                , ", git rev ", gitRev
                ]
  where
    renderVersion = Text.pack . showVersion
