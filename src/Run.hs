{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE LambdaCase      #-}

module Run
    ( runMain
    ) where

import           Control.Lens


runMain :: IO ()
runMain = putStrLn "libjuice-hs!"
