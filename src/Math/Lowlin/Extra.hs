-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Lowlin.Extra where

import Math.Lowlin.Types

-- | cross product of two 3-dimensional vectors
cross :: Num a => V3 a -> V3 a -> V3 a
cross (x1,x2,x3) (y1,y2,y3) = (x2*y3-x3*y2, x3*y1-x1*y3, x1*y2-x2*y1)
{-# INLINE cross #-}

-- Remark: Since it's not possible to cmap realToFrac, functions for the individual types are implemented here

-- | realToFrac for a 2-vector
realToFracV2 :: (Real a, Fractional b) => V2 a -> V2 b
realToFracV2 (x1,x2) = (realToFrac x1, realToFrac x2)

-- | realToFrac for a 3-vector
realToFracV3 :: (Real a, Fractional b) => V3 a -> V3 b
realToFracV3 (x1,x2,x3) = (realToFrac x1, realToFrac x2, realToFrac x3)

-- | realToFrac for a 4-vector
realToFracV4 :: (Real a, Fractional b) => V4 a -> V4 b
realToFracV4 (x1,x2,x3,x4) = (realToFrac x1, realToFrac x2, realToFrac x3, realToFrac x4)

-- | realToFrac for a 2x2-matrix
realToFracM22 :: (Real a, Fractional b) => M22 a -> M22 b
realToFracM22 (m1,m2) = (realToFracV2 m1, realToFracV2 m2)

-- | realToFrac for a 3x3-matrix
realToFracM33 :: (Real a, Fractional b) => M33 a -> M33 b
realToFracM33 (m1,m2,m3) = (realToFracV3 m1, realToFracV3 m2, realToFracV3 m3)

-- | realToFrac for a 4x4-matrix
realToFracM44 :: (Real a, Fractional b) => M44 a -> M44 b
realToFracM44 (m1,m2,m3,m4) = (realToFracV4 m1, realToFracV4 m2, realToFracV4 m3, realToFracV4 m4)
