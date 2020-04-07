-- |
-- Copyright   : (c) Johannes Kropp
-- License     : BSD 3-Clause
-- Maintainer  : Johannes Kropp <jodak932@gmail.com>


module Math.LowlinSpec where

import Prelude hiding (sum, minimum, maximum)
import Test (Spec(..), (=~=))
import Math.Lowlin
import Math.LowlinData


specs =
    [Spec "*\\" $ 0 =~= norm (s1*\v2 - (207,477))
    ,Spec "/*"  $ 0 =~= norm (v2/*s1 - (207,477))
    ,Spec "//"  $ 0 =~= norm (v2//s1 - (2.55555556, 5.88888889))
    ,Spec "norm 2" $ 0 =~= (norm v2 - 57.77542730261716)
    ,Spec "normalize 2" $ 0 =~= (norm (normalize v2 - (0.39809312, 0.91734501)))
    ,Spec "dot 2" $ 0 =~= (dot v2 w2 - 4299)
    ,Spec "maximum 2" $ maximum v2 == 53
    ,Spec "minimum 2" $ minimum v2 == 23
    ,Spec "sum 2" $ sum v2 == 76
    ,Spec "|.| 2x2" $ 0 =~= (fnorm $ m22 |.| n22 - ((5312, 7991), (5060, 7747)))
    ,Spec "|.\\ 2x2" $ 0 =~= (norm $ m22 |.\ v2 - (4434, 4928))
    ,Spec "fnorm 2x2" $ fnorm m22 =~= 116.15506876585283
    ,Spec "transpose 2x2" $ transpose m22 == ((43, 23), (65, 83))
    ,Spec "diagonal 2x2" $ diagonal m22 == (43, 83)
    ,Spec "trace 2x2" $ trace m22 == 126
    ,Spec "det 2x2" $ det m22 =~= 2074.0000000000005
    ,Spec "inv 2x2" $ 0 =~= (fnorm $ inv m22 |.| m22 - ((1,0),(0,1)))

    ,Spec "*\\" $ 0 =~= norm (s1*\v3 - (837, 855, 306))
    ,Spec "/*"  $ 0 =~= norm (v3/*s1 - (837, 855, 306))
    ,Spec "//"  $ 0 =~= norm (v3//s1 - (10.33333333, 10.55555556,  3.77777778))
    ,Spec "norm 3" $ 0 =~= (norm v3 - 137.22244714331543)
    ,Spec "normalize 3" $ 0 =~= (norm (normalize v3 - (0.67773168, 0.69230656, 0.24777287)))
    ,Spec "dot 3" $ 0 =~= (dot v3 w3 - 11088)
    ,Spec "maximum 3" $ maximum v3 == 95
    ,Spec "minimum 3" $ minimum v3 == 34
    ,Spec "sum 3" $ sum v3 == 222
    ,Spec "|.| 3x3" $ 0 =~= (fnorm $ m33 |.| n33 - ((4877, 6091, 7010), (5636, 6051, 9649), (4718, 6632, 6985)))
    ,Spec "|.\\ 3x3" $ 0 =~= (norm $ m33 |.\ v3 - ( 8299, 11860,  9182))
    ,Spec "fnorm 3x3" $ fnorm m33 =~= 152.18081350814234
    ,Spec "transpose 3x3" $ transpose m33 == ((43, 23, 49), (22, 83, 29), (65, 54, 55))
    ,Spec "diagonal 3x3" $ diagonal m33 == (43, 83, 55)
    ,Spec "trace 3x3" $ trace m33 == 181
    ,Spec "det 3x3" $ det m33 =~= (-61660.999999999985)
    ,Spec "inv 3x3" $ 0 =~= (fnorm $ inv m33 |.| m33 - ((1,0,0),(0,1,0),(0,0,1)))

    ,Spec "*\\" $ 0 =~= norm (s1*\v4 - (846, 207, 387, 270))
    ,Spec "/*"  $ 0 =~= norm (v4/*s1 - (846, 207, 387, 270))
    ,Spec "//"  $ 0 =~= norm (v4//s1 - (10.44444444,  2.55555556,  4.77777778,  3.33333333))
    ,Spec "norm 4" $ 0 =~= (norm v4 - 110.06361796706484)
    ,Spec "normalize 4" $ 0 =~= (norm (normalize v4 - (0.85405152, 0.20897005, 0.39068314, 0.27256963)))
    ,Spec "dot 4" $ 0 =~= (dot v4 w4 - 11781)
    ,Spec "maximum 4" $ maximum v4 == 94
    ,Spec "minimum 4" $ minimum v4 == 23
    ,Spec "sum 4" $ sum v4 == 190
    ,Spec "|.| 4x4" $ 0 =~= (fnorm $ m44 |.| n44 - ((12079, 13154, 10460, 11240), ( 8940, 10240,  7782,  7285), ( 8111, 14020,  8239, 11922), ( 8779, 12382,  8169,  9399)))
    ,Spec "|.\\ 4x4" $ 0 =~= (norm $ m44 |.\ v4 - (13040,  8420,  6749,  7496))
    ,Spec "fnorm 4x4" $ fnorm m44 =~= 207.74262923146034
    ,Spec "transpose 4x4" $ transpose m44 == ((88, 55, 11, 33), (25, 50, 73, 69), (71, 30, 52, 29), (38, 27, 60, 52))
    ,Spec "diagonal 4x4" $ diagonal m44 == (88, 50, 52, 52)
    ,Spec "trace 4x4" $ trace m44 == 242
    ,Spec "det 4x4" $ det m44 =~= 3009259.0000000014
    ,Spec "inv 4x4" $ 0 =~= (fnorm $ inv m44 |.| m44 - ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1)))
    ]



