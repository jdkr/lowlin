{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>


module Math.Lowlin.Types where

import Prelude hiding (zipWith, sum)
import Foreign.Storable (Storable(..), sizeOf)
import Foreign.Ptr (Ptr, castPtr)

import Math.Lowlin.Classes

-- | 2-vector
type V2 a  = (a, a)
-- | 3-vector
type V3 a  = (a, a, a)
-- | 4-vector
type V4 a  = (a, a, a, a)
-- | 2,2-matrix
type M22 a = V2 (V2 a)
-- | 3,2-matrix
type M32 a = V3 (V2 a)
-- | 4,2-matrix
type M42 a = V4 (V2 a)
-- | 2,3-matrix
type M23 a = V2 (V3 a)
-- | 3,3-matrix
type M33 a = V3 (V3 a)
-- | 4,3-matrix
type M43 a = V4 (V3 a)
-- | 2,4-matrix
type M24 a = V2 (V4 a)
-- | 3,4-matrix
type M34 a = V3 (V4 a)
-- | 4,4-matrix
type M44 a = V4 (V4 a)




-- Remark: Only the instances for vectors need to be implemented. The corresponding matrix-instances are then implicitly defined, since matrices are defined as vectors of vectors.
------------------------------------------------------
-- Storable Instances

instance Storable a => Storable (V2 a) where
    {-# INLINE sizeOf #-}
    sizeOf _ = 2 * sizeOf (undefined::a)
    {-# INLINE alignment #-}
    alignment _ = sizeOf (undefined::a)
    {-# INLINE peek #-}
    peek ptr1 = do
        let ptr2 = castPtr ptr1 :: Ptr a
            k = sizeOf (undefined::a)
        x1 <- peek        ptr2
        x2 <- peekByteOff ptr2 (k  )
        return (x1,x2)
    {-# INLINE poke #-}
    poke ptr1 (x1,x2) = do
        let ptr2 = castPtr ptr1 :: Ptr a
            k = sizeOf (undefined::a)
        poke        ptr2       x1
        pokeByteOff ptr2 (k  ) x2



instance Storable a => Storable (V3 a) where
    {-# INLINE sizeOf #-}
    sizeOf _ = 3 * sizeOf (undefined::a)
    {-# INLINE alignment #-}
    alignment _ = sizeOf (undefined::a)
    {-# INLINE peek #-}
    peek ptr1 = do
        let ptr2 = castPtr ptr1 :: Ptr a
            k = sizeOf (undefined::a)
        x1 <- peek        ptr2
        x2 <- peekByteOff ptr2 (k  )
        x3 <- peekByteOff ptr2 (k+k)
        return (x1,x2,x3)
    {-# INLINE poke #-}
    poke ptr1 (x1,x2,x3) = do
        let ptr2 = castPtr ptr1 :: Ptr a
            k = sizeOf (undefined::a)
        poke        ptr2       x1
        pokeByteOff ptr2 (k  ) x2
        pokeByteOff ptr2 (k+k) x3


instance Storable a => Storable (V4 a) where
    {-# INLINE sizeOf #-}
    sizeOf _ = 4 * sizeOf (undefined::a)
    {-# INLINE alignment #-}
    alignment _ = sizeOf (undefined::a)
    {-# INLINE peek #-}
    peek ptr1 = do
        let ptr2 = castPtr ptr1 :: Ptr a
            k = sizeOf (undefined::a)
        x1 <- peek        ptr2
        x2 <- peekByteOff ptr2 (k  )
        x3 <- peekByteOff ptr2 (k+k)
        x4 <- peekByteOff ptr2 (3*k)
        return (x1,x2,x3,x4)
    {-# INLINE poke #-}
    poke ptr1 (x1,x2,x3,x4) = do
        let ptr2 = castPtr ptr1 :: Ptr a
            k = sizeOf (undefined::a)
        poke        ptr2       x1
        pokeByteOff ptr2 (k  ) x2
        pokeByteOff ptr2 (k+k) x3
        pokeByteOff ptr2 (3*k) x4





------------------------------------------------------------------------------
-- Num Instances

instance (Num a) => Num (V2 a) where
    (+) = zipWith (+)
    {-# INLINE (+) #-}
    (*) = zipWith (*)
    {-# INLINE (*) #-}
    (-) = zipWith (-)
    {-# INLINE (-) #-}
    negate = cmap negate
    {-# INLINE negate #-}
    abs = cmap abs
    {-# INLINE abs #-}
    signum = cmap signum
    {-# INLINE signum #-}
    fromInteger = same . fromInteger
    {-# INLINE fromInteger #-}

instance (Num a) => Num (V3 a) where
    (+) = zipWith (+)
    {-# INLINE (+) #-}
    (*) = zipWith (*)
    {-# INLINE (*) #-}
    (-) = zipWith (-)
    {-# INLINE (-) #-}
    negate = cmap negate
    {-# INLINE negate #-}
    abs = cmap abs
    {-# INLINE abs #-}
    signum = cmap signum
    {-# INLINE signum #-}
    fromInteger = same . fromInteger
    {-# INLINE fromInteger #-}

instance (Num a) => Num (V4 a) where
    (+) = zipWith (+)
    {-# INLINE (+) #-}
    (*) = zipWith (*)
    {-# INLINE (*) #-}
    (-) = zipWith (-)
    {-# INLINE (-) #-}
    negate = cmap negate
    {-# INLINE negate #-}
    abs = cmap abs
    {-# INLINE abs #-}
    signum = cmap signum
    {-# INLINE signum #-}
    fromInteger = same . fromInteger
    {-# INLINE fromInteger #-}



------------------------------------------------------------------------------
-- Fractional Instances


instance (Fractional a) => Fractional (V2 a) where
    fromRational = same . fromRational
    {-# INLINE fromRational #-}
    (/) = zipWith (/)
    {-# INLINE (/) #-}

instance (Fractional a) => Fractional (V3 a) where
    fromRational = same . fromRational
    {-# INLINE fromRational #-}
    (/) = zipWith (/)
    {-# INLINE (/) #-}

instance (Fractional a) => Fractional (V4 a) where
    fromRational = same . fromRational
    {-# INLINE fromRational #-}
    (/) = zipWith (/)
    {-# INLINE (/) #-}


------------------------------------------------------------------------------
-- ContainerClass Instances


instance ContainerClass (V2 a) where
    type ElemC (V2 a) = a
    (!) (x1,x2) idx
        | idx == 0 = x1
        | idx == 1 = x2
        | otherwise = error $ "(!) : index out of range"
    {-# INLINE (!) #-}
    cmap f (x1,x2) = (f x1, f x2)
    {-# INLINE cmap #-}
    zipWith f (x1,x2) (y1,y2) = (f x1 y1, f x2 y2)
    {-# INLINE zipWith #-}
    same x = (x,x)
    {-# INLINE same #-}
    toList (x1,x2) = [x1,x2]
    {-# INLINE toList #-}

instance ContainerClass (V3 a) where
    type ElemC (V3 a) = a
    (!) (x1,x2,x3) idx
        | idx == 0 = x1
        | idx == 1 = x2
        | idx == 2 = x3
        | otherwise = error $ "(!) : index out of range"
    {-# INLINE (!) #-}
    cmap f (x1,x2,x3) = (f x1, f x2, f x3)
    {-# INLINE cmap #-}
    zipWith f (x1,x2,x3) (y1,y2,y3) = (f x1 y1, f x2 y2, f x3 y3)
    {-# INLINE zipWith #-}
    same x = (x,x,x)
    {-# INLINE same #-}
    toList (x1,x2,x3) = [x1,x2,x3]
    {-# INLINE toList #-}

instance ContainerClass (V4 a) where
    type ElemC (V4 a) = a
    (!) (x1,x2,x3,x4) idx
        | idx == 0 = x1
        | idx == 1 = x2
        | idx == 2 = x3
        | idx == 3 = x4
        | otherwise = error $ "(!) : index out of range"
    {-# INLINE (!) #-}
    cmap f (x1,x2,x3,x4) = (f x1, f x2, f x3, f x4)
    {-# INLINE cmap #-}
    zipWith f (x1,x2,x3,x4) (y1,y2,y3,y4) = (f x1 y1, f x2 y2, f x3 y3, f x4 y4)
    {-# INLINE zipWith #-}
    same x = (x,x,x,x)
    {-# INLINE same #-}
    toList (x1,x2,x3,x4) = [x1,x2,x3,x4]
    {-# INLINE toList #-}


------------------------------------------------------------------------------
-- VectorClass Instances

instance (Ord a, Floating a) => VectorClass (V2 a) where
    type ElemV (V2 a) = a
    (*\) s (x1,x2) = (s*x1,s*x2)
    {-# INLINE (*\) #-}
    (/*) v s = s *\ v
    {-# INLINE (/*) #-}
    (//) (x1,x2) s = (x1/s,x2/s)
    {-# INLINE (//) #-}
    norm (x1,x2) = sqrt $ x1*x1 + x2*x2
    {-# INLINE norm #-}
    normalize (x1,x2) = (x1/n,x2/n) where n = norm (x1,x2)
    {-# INLINE normalize #-}
    dot (x1,x2) (y1,y2) = x1*y1 + x2*y2
    {-# INLINE dot #-}
    maximum (x1,x2) = max x1 x2
    {-# INLINE maximum #-}
    minimum (x1,x2) = min x1 x2
    {-# INLINE minimum #-}
    sum (x1,x2) = x1+x2
    {-# INLINE sum #-}


instance (Ord a, Floating a) => VectorClass (V3 a) where
    type ElemV (V3 a) = a
    (*\) s (x1,x2,x3) = (s*x1,s*x2,s*x3)
    {-# INLINE (*\) #-}
    (/*) v s = s *\ v
    {-# INLINE (/*) #-}
    (//) (x1,x2,x3) s = (x1/s,x2/s,x3/s)
    {-# INLINE (//) #-}
    norm (x1,x2,x3) = sqrt $ x1*x1 + x2*x2 + x3*x3
    {-# INLINE norm #-}
    normalize (x1,x2,x3) = (x1/n,x2/n,x3/n) where n = norm (x1,x2,x3)
    {-# INLINE normalize #-}
    dot (x1,x2,x3) (y1,y2,y3) = x1*y1 + x2*y2 + x3*y3
    {-# INLINE dot #-}
    maximum (x1,x2,x3) = max x1 $ max x2 x3
    {-# INLINE maximum #-}
    minimum (x1,x2,x3) = min x1 $ min x2 x3
    {-# INLINE minimum #-}
    sum (x1,x2,x3) = x1+x2+x3
    {-# INLINE sum #-}


instance (Ord a, Floating a) => VectorClass (V4 a) where
    type ElemV (V4 a) = a
    (*\) s (x1,x2,x3,x4) = (s*x1,s*x2,s*x3,s*x4)
    {-# INLINE (*\) #-}
    (/*) v s = s *\ v
    {-# INLINE (/*) #-}
    (//) (x1,x2,x3,x4) s = (x1/s,x2/s,x3/s,x4/s)
    {-# INLINE (//) #-}
    norm (x1,x2,x3,x4) = sqrt $ x1*x1 + x2*x2 + x3*x3 + x4*x4
    {-# INLINE norm #-}
    normalize (x1,x2,x3,x4) = (x1/n,x2/n,x3/n,x4/n) where n = norm (x1,x2,x3,x4)
    {-# INLINE normalize #-}
    dot (x1,x2,x3,x4) (y1,y2,y3,y4) = x1*y1 + x2*y2 + x3*y3 + x4*y4
    {-# INLINE dot #-}
    maximum (x1,x2,x3,x4) = max x1 $ max x2 $ max x3 x4
    {-# INLINE maximum #-}
    minimum (x1,x2,x3,x4) = min x1 $ min x2 $ min x3 x4
    {-# INLINE minimum #-}
    sum (x1,x2,x3,x4) = x1+x2+x3+x4
    {-# INLINE sum #-}



------------------------------------------------------------------------------
-- MatrixSquareClass Instances



instance (Ord a, Floating a) => MatrixSquareClass (M22 a) where
    type ElemM (M22 a) = a
    type VecM (M22 a) = V2 a
    (|.|) ((x11,x12),(x21,x22)) ((y11,y12),(y21,y22)) =
        ((x11*y11+x12*y21,x11*y12+x12*y22),(x21*y11+x22*y21,x21*y12+x22*y22))
    {-# INLINE (|.|) #-}
    (|.\) ((x11,x12),(x21,x22)) (y1,y2) = (x11*y1+x12*y2,x21*y1+x22*y2)
    {-# INLINE (|.\) #-}
    fnorm (v1,v2) = sqrt $ dot v1 v1 + dot v2 v2
    {-# INLINE fnorm #-}
    transpose ((x11,x12),(x21,x22)) = ((x11,x21),(x12,x22))
    {-# INLINE transpose #-}
    diagonal ((x11,x12),(x21,x22)) = (x11,x22)
    {-# INLINE diagonal #-}
    trace = sum . diagonal
    {-# INLINE trace #-}
    adjugate ((x11,x12),(x21,x22)) = ((x22,-x12),(-x21,x11))
    {-# INLINE adjugate #-}
    det ((x11,x12),(x21,x22)) = x11*x22-x12*x21
    {-# INLINE det #-}
    inv m = cmap (//(det m)) (adjugate m)
    {-# INLINE inv #-}


instance (Ord a, Floating a) => MatrixSquareClass (M33 a) where
    type ElemM (M33 a) = a
    type VecM (M33 a) = V3 a
    (|.|) (v1,v2,v3) ((y11,y12,y13),(y21,y22,y23),(y31,y32,y33)) =
        ((dot v1 w1, dot v1 w2, dot v1 w3)
        ,(dot v2 w1, dot v2 w2, dot v2 w3)
        ,(dot v3 w1, dot v3 w2, dot v3 w3)) where
            w1=(y11,y21,y31); w2=(y12,y22,y32); w3=(y13,y23,y33)
    {-# INLINE (|.|) #-}
    (|.\) (v1,v2,v3) w = (dot v1 w, dot v2 w, dot v3 w)
    {-# INLINE (|.\) #-}
    fnorm (v1,v2,v3) = sqrt $ dot v1 v1 + dot v2 v2 + dot v3 v3
    {-# INLINE fnorm #-}
    transpose ((x11,x12,x13),(x21,x22,x23),(x31,x32,x33)) =
        ((x11,x21,x31),(x12,x22,x32),(x13,x23,x33))
    {-# INLINE transpose #-}
    diagonal ((x11,x12,x13),(x21,x22,x23),(x31,x32,x33)) = (x11,x22,x33)
    {-# INLINE diagonal #-}
    trace = sum . diagonal
    {-# INLINE trace #-}
    adjugate ((x11,x12,x13),(x21,x22,x23),(x31,x32,x33)) =
        ((y11,y12,y13),(y21,y22,y23),(y31,y32,y33)) where
            y11 =   det ((x22,x23),(x32,x33))
            y12 = - det ((x12,x13),(x32,x33))
            y13 =   det ((x12,x13),(x22,x23))
            y21 = - det ((x21,x23),(x31,x33))
            y22 =   det ((x11,x13),(x31,x33))
            y23 = - det ((x11,x13),(x21,x23))
            y31 =   det ((x21,x22),(x31,x32))
            y32 = - det ((x11,x12),(x31,x32))
            y33 =   det ((x11,x12),(x21,x22))
    {-# INLINE adjugate #-}
    det ((x11,x12,x13),(x21,x22,x23),(x31,x32,x33)) =
        x11*(det m11) - x12*(det m12) + x13*(det m13) where
            m11 = ((x22,x23),(x32,x33)); m12 = ((x21,x23),(x31,x33)); m13 = ((x21,x22),(x31,x32))
    {-# INLINE det #-}
    inv m = cmap (//(det m)) (adjugate m)
    {-# INLINE inv #-}


instance (Ord a, Floating a) => MatrixSquareClass (M44 a) where
    type ElemM (M44 a) = a
    type VecM (M44 a) = V4 a
    (|.|) (v1,v2,v3,v4) ((y11,y12,y13,y14),(y21,y22,y23,y24),(y31,y32,y33,y34),(y41,y42,y43,y44)) =
        ((dot v1 w1, dot v1 w2, dot v1 w3, dot v1 w4)
        ,(dot v2 w1, dot v2 w2, dot v2 w3, dot v2 w4)
        ,(dot v3 w1, dot v3 w2, dot v3 w3, dot v3 w4)
        ,(dot v4 w1, dot v4 w2, dot v4 w3, dot v4 w4)) where
            w1=(y11,y21,y31,y41); w2=(y12,y22,y32,y42); w3=(y13,y23,y33,y43); w4=(y14,y24,y34,y44)
    {-# INLINE (|.|) #-}
    (|.\) (v1,v2,v3,v4) w = (dot v1 w, dot v2 w, dot v3 w, dot v4 w)
    {-# INLINE (|.\) #-}
    fnorm (v1,v2,v3,v4) = sqrt $ dot v1 v1 + dot v2 v2 + dot v3 v3 + dot v4 v4
    {-# INLINE fnorm #-}
    transpose ((x11,x12,x13,x14),(x21,x22,x23,x24),(x31,x32,x33,x34),(x41,x42,x43,x44)) =
        ((x11,x21,x31,x41),(x12,x22,x32,x42),(x13,x23,x33,x43),(x14,x24,x34,x44))
    {-# INLINE transpose #-}
    diagonal ((x11,x12,x13,x14),(x21,x22,x23,x24),(x31,x32,x33,x34),(x41,x42,x43,x44)) =
        (x11,x22,x33,x44)
    {-# INLINE diagonal #-}
    trace = sum . diagonal
    {-# INLINE trace #-}
    adjugate ((x11,x12,x13,x14),(x21,x22,x23,x24),(x31,x32,x33,x34),(x41,x42,x43,x44)) =
        ((y11,y12,y13,y14),(y21,y22,y23,y24),(y31,y32,y33,y34),(y41,y42,y43,y44)) where
            y11 =   det ((x22,x23,x24),(x32,x33,x34),(x42,x43,x44))
            y12 = - det ((x12,x13,x14),(x32,x33,x34),(x42,x43,x44))
            y13 =   det ((x12,x13,x14),(x22,x23,x24),(x42,x43,x44))
            y14 = - det ((x12,x13,x14),(x22,x23,x24),(x32,x33,x34))
            y21 = - det ((x21,x23,x24),(x31,x33,x34),(x41,x43,x44))
            y22 =   det ((x11,x13,x14),(x31,x33,x34),(x41,x43,x44))
            y23 = - det ((x11,x13,x14),(x21,x23,x24),(x41,x43,x44))
            y24 =   det ((x11,x13,x14),(x21,x23,x24),(x31,x33,x34))
            y31 =   det ((x21,x22,x24),(x31,x32,x34),(x41,x42,x44))
            y32 = - det ((x11,x12,x14),(x31,x32,x34),(x41,x42,x44))
            y33 =   det ((x11,x12,x14),(x21,x22,x24),(x41,x42,x44))
            y34 = - det ((x11,x12,x14),(x21,x22,x24),(x31,x32,x34))
            y41 = - det ((x21,x22,x23),(x31,x32,x33),(x41,x42,x43))
            y42 =   det ((x11,x12,x13),(x31,x32,x33),(x41,x42,x43))
            y43 = - det ((x11,x12,x13),(x21,x22,x23),(x41,x42,x43))
            y44 =   det ((x11,x12,x13),(x21,x22,x23),(x31,x32,x33))
    {-# INLINE adjugate #-}
    det ((x11,x12,x13,x14),(x21,x22,x23,x24),(x31,x32,x33,x34),(x41,x42,x43,x44)) =
        x11*(det m11) - x12*(det m12) + x13*(det m13) - x14*(det m14) where
            m11 = ((x22,x23,x24),(x32,x33,x34),(x42,x43,x44))
            m12 = ((x21,x23,x24),(x31,x33,x34),(x41,x43,x44))
            m13 = ((x21,x22,x24),(x31,x32,x34),(x41,x42,x44))
            m14 = ((x21,x22,x23),(x31,x32,x33),(x41,x42,x43))
    {-# INLINE det #-}
    inv m = cmap (/same (det m)) (adjugate m)
    {-# INLINE inv #-}


