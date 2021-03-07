{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE LambdaCase      #-}

module Run
    ( runMain
    ) where

import           Bindings
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- import Bindings

conf =
  Config
  { configHost = "stun.stunprotocol.org"
  , configPort = 3478
  , configLocalPortRange = (0,0)
  }

cbs = Callbacks
  { onStateChanged = \state -> putStrLn $ "State changed: " ++ show state
  , onCandidate = \candidate -> putStrLn $ "New Candidate: " ++ show candidate
  , onGatheringDone = putStrLn "Gathering done"
  , onRecv = \ptr len -> BS.packCStringLen (ptr, len) >>=
                           \bs -> putStrLn $ ("Receiving: " ++ show bs)
  }

runMain :: IO ()
runMain = do
  agent <- newAgent conf cbs
  gatherCandidates agent
  getLocalDescription agent >>= \desc -> putStrLn ("local description: " ++ show desc)
  getState agent >>= print
