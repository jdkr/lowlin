{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>

module Math.Lowlin.Classes where



-- | this class is a workaround, since it's not possible to make a typesynonym tuple an instance of functor:
class ContainerClass c where
    -- | type of single container element
    type ElemC c
    infixl 9 !
    -- | indexing of a single container element
    (!) :: c -> Int -> ElemC c -- index function not allowed to be in vector class since then it would only be possible to index Floating values
    -- | map a function (acting on the container elements) over the container
    cmap :: (ElemC c -> ElemC c) -> c -> c
    -- | zipWith for the container
    zipWith :: (ElemC c -> ElemC c -> ElemC c) -> c -> c -> c
    -- | creates a container with all elements the same
    same :: ElemC c -> c
    -- | converts the container to a list
    toList :: c -> [ElemC c]

-- | Typeclass for a vector
class VectorClass v where
    -- | type of a single vector element
    type ElemV v
    infixl 7 *\, /*, //
    -- | vector multiplied by a scalar from the left
    (*\) :: ElemV v -> v -> v
    -- | vector multiplied by a scalar from the right
    (/*) :: v -> ElemV v -> v
    -- | vector divided by a scalar
    (//) :: v -> ElemV v -> v
    -- | euclidean norm
    norm :: v -> ElemV v
    -- | normalize to unit-vector in euclidean norm
    normalize :: v -> v
    -- | dot-product of two vectors
    dot :: v -> v -> ElemV v
    -- | maximum element of a vector
    maximum :: v -> ElemV v
    -- | minimum element of a vector
    minimum :: v -> ElemV v
    -- | sum of all vector elements
    sum :: v -> ElemV v


-- TODO: implement (*|), (|*), (|/)
-- | Typeclass for a square matrix
class MatrixSquareClass m where
    -- | type of a single matrix element
    type ElemM m
    -- | type of a vector a matrix can multiplied with
    type VecM m
    infixl 7 |.|, |.\
    -- | matrix product
    (|.|) :: m -> m -> m
    -- | matrix-vector product
    (|.\) :: m -> VecM m -> VecM m
    -- | frobenius norm
    fnorm :: m -> ElemM m
    -- | transposed matrix
    transpose :: m -> m
    -- | diagonal of a matrix as a vector
    diagonal :: m -> VecM m
    -- | trace of a matrix
    trace :: m -> ElemM m
    -- | adjugate of a matrix
    adjugate :: m -> m
    -- | determinant of a matrix
    det :: m -> ElemM m
    -- | inverse matrix
    inv :: m -> m





