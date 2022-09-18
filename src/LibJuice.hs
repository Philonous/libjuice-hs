{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module LibJuice
  ( module LibJuice
  , B.Config(..)
  , B.Callbacks(..)
  , B.JuiceError(..)
  , B.State(..)
  )
where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text

import qualified Bindings                 as B

-- Run all interaction with the Agent in a bound thread

data Connection =
  Connection { connectionAgent :: B.Agent
             }

withConnection :: Connection -> (B.Agent -> IO b) -> IO b
withConnection Connection{connectionAgent} f = f connectionAgent

connect :: B.Config -> B.Callbacks -> IO Connection
connect config callbacks = Connection <$> B.newAgent config callbacks

disconnect :: Connection -> IO ()
disconnect con = withConnection con $ \agent -> B.destroyAgent agent

gatherCandidates :: Connection -> IO ()
gatherCandidates con = withConnection con B.gatherCandidates

getLocalDescription :: Connection -> IO ByteString
getLocalDescription con = withConnection con B.getLocalDescription

setRemoteDescription :: Connection -> ByteString -> IO ()
setRemoteDescription con des =
  withConnection con (\agent -> B.setRemoteDescription agent des)

addRemoteCandidate :: Connection -> ByteString -> IO ()
addRemoteCandidate con cand =
  withConnection con (\agent -> B.addRemoteCandidate agent cand)

setRemoteGatheringDone :: Connection -> IO ()
setRemoteGatheringDone con =
  withConnection con B.setRemoteGatheringDone

send :: Connection -> ByteString -> IO ()
send con bs =
  withConnection con (\agent -> B.send agent bs)

sendDiffserv :: Connection -> ByteString -> Int -> IO ()
sendDiffserv con bs ds =
  withConnection con (\agent -> B.sendDiffserv agent bs ds)

getState :: Connection -> IO B.State
getState con = withConnection con B.getState

getSelectedCanidates :: Connection -> IO (ByteString, ByteString)
getSelectedCanidates con = withConnection con B.getSelectedCandidates

getSelectedAddresses :: Connection -> IO (ByteString, ByteString)
getSelectedAddresses con = withConnection con B.getSelectedAddresses
