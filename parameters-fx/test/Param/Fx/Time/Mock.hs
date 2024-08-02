{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Param.Fx.Time.Mock
  ( staticTime,
    runStaticTime,
  )
where

import Data.Time
import Param.Fx
import Param.Fx.Time

staticTime :: UTCTime -> Double -> Time
staticTime current monotonic =
  Time
    { _currentTime = pure current,
      _monotonicTime = pure monotonic
    }

runStaticTime :: UTCTime -> Double -> ((RunTime) => Fx r) -> Fx r
runStaticTime current monotonic = runTime $ staticTime current monotonic
