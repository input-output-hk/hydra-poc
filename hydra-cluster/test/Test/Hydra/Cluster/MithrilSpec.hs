module Test.Hydra.Cluster.MithrilSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO)
import Control.Lens ((^?))
import Data.Aeson.Lens (key)
import Hydra.Cluster.Mithril (MithrilLog (..), downloadLatestSnapshotTo)
import Hydra.Logging (Envelope (..), Tracer, traceInTVar)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Test.Hydra.Cluster.Utils (forEachKnownNetwork)

spec :: Spec
spec = parallel $ do
  describe "downloadLatestSnapshotTo" $
    forEachKnownNetwork "invokes mithril-client correctly" $ \network -> do
      pendingWith "client is outdated"
      (tracer, getTraces) <- captureTracer "MithrilSpec"
      withTempDir ("mithril-download-" <> show network) $ \tmpDir -> do
        let dbPath = tmpDir </> "db"
        doesDirectoryExist dbPath `shouldReturn` False
        race_
          (downloadLatestSnapshotTo tracer network tmpDir)
          (waitForDownload getTraces)

-- | Wait for the 'StdErr' message to indicate it starts downloading.
waitForDownload :: HasCallStack => IO [Envelope MithrilLog] -> IO ()
waitForDownload getTraces = do
  traces <- getTraces
  unless (any isRightTrace traces) $ do
    threadDelay 1
    waitForDownload getTraces
 where
  isRightTrace = \case
    Envelope{message = StdErr{output}} ->
      isJust $ output ^? key "bytes_downloaded"
    _ -> False

-- | Create a tracer that captures all messages and a function to retrieve all
-- traces captured.
captureTracer :: Text -> IO (Tracer IO a, IO [Envelope a])
captureTracer namespace = do
  traces <- newTVarIO []
  let tracer = traceInTVar traces namespace
  pure (tracer, readTVarIO traces)
