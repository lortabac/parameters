{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin Param.Plugin #-}

module Param.Fx.Time
  ( HasTime,
    Time (..),
    runTime,
    currentTime,
    monotonicTime,
    defaultTime,
  )
where

import Data.Time
import GHC.Clock (getMonotonicTime)
import Param
import Param.Fx

-- | Time effect
data Time = Time
  { _currentTime :: IO UTCTime,
    _monotonicTime :: IO Double
  }

-- | Time parameter (not exported)
data TimeParam

type HasTime = HasParam TimeParam Time

-- | Run a 'Time' effect with the provided handler
runTime :: Time -> ((HasTime) => r) -> r
runTime = runParam @TimeParam

-- | Get the current time
currentTime :: (HasTime) => Fx UTCTime
currentTime = fx @TimeParam (_currentTime (paramAsk @TimeParam))

-- | Get the monotonic time
monotonicTime :: (HasTime) => Fx Double
monotonicTime = fx @TimeParam (_monotonicTime (paramAsk @TimeParam))

-- | Default handler for the 'Time' effect
defaultTime :: Time
defaultTime =
  Time
    { _currentTime = getCurrentTime,
      _monotonicTime = getMonotonicTime
    }
