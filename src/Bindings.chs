{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass  #-}
module Bindings where

import qualified Control.Monad.Catch as Ex
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Typeable
import           Data.Word
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr

#include <juice.h>

{# context lib="libjuice" prefix="juice" #}

{# enum define JuiceError
   { JUICE_ERR_SUCCESS   as ErrorSuccess
   , JUICE_ERR_INVALID   as ErrorInvalid
   , JUICE_ERR_FAILED    as ErrorFailed
   , JUICE_ERR_NOT_AVAIL as ErrorNotAvail
   }
  deriving (Show, Eq, Ord, Typeable)
#}

instance Ex.Exception JuiceError

maxAddressStringLen = {#const JUICE_MAX_ADDRESS_STRING_LEN #}
maxCandidateSdpStringLen = {#const JUICE_MAX_CANDIDATE_SDP_STRING_LEN #}
juiceMaxSdpStringLen = {#const JUICE_MAX_SDP_STRING_LEN #}

{# enum state as State {underscoreToCase} deriving (Show, Ord, Eq)#}

--------------------------------------------------------------------------------
-- Config And Agent ------------------------------------------------------------
--------------------------------------------------------------------------------

{# pointer * juice_agent_t as Agent foreign finalizer juice_destroy as destroyAgent newtype #}

foreign import ccall "wrapper"
  mkStateChangedCallback ::
       (Ptr Agent -> CInt -> Ptr () -> IO ())
    -> IO {#type juice_cb_state_changed_t#}

foreign import ccall "wrapper"
  mkCandidateCallback ::
       (Ptr Agent -> Ptr CChar -> Ptr () -> IO ())
    -> IO {#type juice_cb_candidate_t#}

foreign import ccall "wrapper"
  mkGatheringDoneCallback ::
       (Ptr Agent -> Ptr () -> IO ())
    -> IO {#type juice_cb_gathering_done_t#}

foreign import ccall "wrapper"
  mkRecvCallback ::
       (Ptr Agent -> Ptr CChar -> {#type size_t#} -> Ptr () -> IO ())
    -> IO {#type juice_cb_recv_t#}


data Callbacks =
  Callbacks
  { onStateChanged  :: State -> IO ()
  , onCandidate     :: ByteString -> IO ()
  , onGatheringDone :: IO ()
  , onRecv          :: Ptr CChar -> Int -> IO ()
  }

data Config =
  Config
  { configHost :: Text
  , configPort :: Word16
  -- [TurnServe elided]
  , configLocalPortRange :: (Word16, Word16)
  -- , config
  }

newAgent :: Config -> Callbacks -> IO Agent
newAgent Config{..} Callbacks{..} =
  BS.useAsCString (Text.encodeUtf8 configHost) $ \hostBytes ->
  callocaBytes {#sizeof juice_config#} $ \configPtr -> do
    {#set juice_config.stun_server_host #} configPtr hostBytes
    {#set juice_config.stun_server_port#} configPtr (port configPort)
    let (portMin, portMax) = configLocalPortRange
    {#set juice_config.local_port_range_begin#} configPtr (port portMin)
    {#set juice_config.local_port_range_end#} configPtr (port portMax)

    {#set juice_config.cb_state_changed#} configPtr =<< mkStateChanged
    {#set juice_config.cb_candidate#} configPtr =<< mkCandidate
    {#set juice_config.cb_gathering_done#} configPtr =<< mkGatheringDone
    {#set juice_config.cb_recv#} configPtr =<< mkRecv

    agentPtr <- {#call juice_create as ^ #} configPtr
    Agent <$> newForeignPtr destroyAgent agentPtr
  where
    mkStateChanged =
      mkStateChangedCallback $
        \_agent stateOrd _userPtr -> onStateChanged (enum stateOrd)
    mkCandidate =
      mkCandidateCallback $ \_agent cstr _ptr -> do
        bs <- BS.packCString cstr
        onCandidate bs
    mkGatheringDone =
      mkGatheringDoneCallback $ \_agent _userPtr -> onGatheringDone
    mkRecv =
      mkRecvCallback $ \_agent ptr len _userPtr ->
        onRecv (castPtr ptr) (fromIntegral len)

    callocaBytes b = Ex.bracket (callocBytes b) free
    enum = toEnum . fromIntegral
    port :: Word16 -> CUShort
    port = fromIntegral

checkError :: IO CInt -> IO ()
checkError f = do
  res <- f
  if (res /= 0)
    then Ex.throwM (toEnum $ fromIntegral res :: JuiceError)
    else return ()

gatherCandidates :: Agent -> IO ()
gatherCandidates agent = withAgent agent $ \agentPtr ->
  checkError $ {#call juice_gather_candidates#} agentPtr

getLocalDescription :: Agent -> IO ByteString
getLocalDescription agent = fmap snd $
  withAgent agent $ \agentPtr ->
  bsOut maxCandidateSdpStringLen $ \buffer ->
    checkError $ {#call juice_get_local_description #} agentPtr buffer
    (fromIntegral maxCandidateSdpStringLen)

setRemoteDescription :: Agent -> ByteString -> IO ()
setRemoteDescription agent bs =
  withAgent agent $ \agentPtr ->
  BS.useAsCString bs $ \ptr ->
  checkError $ {#call juice_set_remote_description#} agentPtr ptr

addRemoteCandidate :: Agent -> ByteString -> IO ()
addRemoteCandidate agent bs =
  withAgent agent $ \agentPtr ->
  BS.useAsCString bs $ \ptr ->
  checkError $ {#call juice_set_remote_description#} agentPtr ptr

setRemoteGatheringDone :: Agent -> IO ()
setRemoteGatheringDone agent = withAgent agent $ \agentPtr ->
  checkError $ {#call juice_set_remote_gathering_done#} agentPtr

send :: Agent -> ByteString -> IO ()
send agent bs = withAgent agent $ \agentPtr ->
  BS.useAsCStringLen bs $ \(dataPtr, len) -> do
  checkError $ {#call juice_send #} agentPtr dataPtr (fromIntegral len)

sendDiffserv :: Agent -> ByteString -> Int -> IO ()
sendDiffserv agent bs ds =
  withAgent agent $ \agentPtr ->
  BS.useAsCStringLen bs $ \(dataPtr, len) -> do
  checkError $ {#call juice_send_diffserv#} agentPtr dataPtr (fromIntegral len)
               (fromIntegral ds)

getState :: Agent -> IO State
getState agent = withAgent agent $ \agentPtr ->
  (toEnum . fromIntegral) <$> {#call juice_get_state#} agentPtr

getSelectedCanidates :: Agent -> IO (ByteString, ByteString)
getSelectedCanidates agent = fmap (\(((), remote), local) -> (local, remote)) $
  withAgent agent $ \agentPtr ->
  bsOut 4096 $ \local ->
  bsOut 4096 $ \remote ->
    checkError $ {#call juice_get_selected_candidates#} agentPtr
       local 4096 remote 4096

getSelectedAddresses :: Agent -> IO (ByteString, ByteString)
getSelectedAddresses agent = fmap (\(((), remote), local) -> (local, remote)) $
  withAgent agent $ \agentPtr ->
  bsOut 4096 $ \local ->
  bsOut 4096 $ \remote ->
    checkError $ {#call juice_get_selected_addresses#} agentPtr
       local 4096 remote 4096


bsOut bufsize f =
  allocaBytes bufsize $ \ptr -> do
    res <- f ptr
    bs <- BS.packCString ptr
    return (res, bs)
