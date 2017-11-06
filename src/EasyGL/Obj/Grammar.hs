{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module EasyGL.Obj.Grammar where
import qualified EasyGL.Obj.Tokens as L
import EasyGL.Obj.ObjData
import Graphics.Rendering.OpenGL
import Data.Sequence
import Data.Foldable (toList)
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: t5 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: t7 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: t10 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: t11 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: t12 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: t13 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: t14 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: t15 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: t16 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: t17 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: t18 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: t19 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: t20 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: t21 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: t22 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> t22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyInTok :: (L.Token) -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22) -> (L.Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x10\x00\x83\x00\x0b\x00\x68\x00\x0b\x00\x00\x00\x1c\x00\x00\x00\x5f\x00\x89\x00\x37\x00\x37\x00\x37\x00\x00\x00\x00\x00\x00\x00\x37\x00\x1c\x00\x00\x00\x00\x00\x65\x00\x00\x00\x65\x00\x37\x00\x37\x00\x00\x00\x22\x00\x80\x00\x1b\x00\x00\x00\x80\x00\x53\x00\x82\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x7e\x00\x5a\x00\x00\x00\x81\x00\x7f\x00\x7d\x00\x5a\x00\x00\x00\x7c\x00\x79\x00\x7b\x00\x7b\x00\x37\x00\x37\x00\x7a\x00\x00\x00\x00\x00\x78\x00\x00\x00\x37\x00\x37\x00\x77\x00\x37\x00\x37\x00\x37\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x75\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x31\x00\x00\x00\x3a\x00\x00\x00\x3f\x00\x00\x00\x11\x00\x00\x00\x41\x00\x52\x00\x74\x00\x73\x00\x72\x00\x00\x00\x00\x00\x00\x00\x71\x00\x02\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x2a\x00\x70\x00\x6f\x00\x00\x00\x57\x00\x44\x00\x00\x00\x00\x00\x5d\x00\x67\x00\x36\x00\x66\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x64\x00\x00\x00\x23\x00\x00\x00\x00\x00\x63\x00\x00\x00\x00\x00\x00\x00\x3b\x00\xf9\xff\x59\x00\x50\x00\x2e\x00\x00\x00\x00\x00\x21\x00\x00\x00\x27\x00\x24\x00\x49\x00\x1a\x00\x17\x00\x0f\x00\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xf9\xff\x00\x00\xd8\xff\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xfb\xff\xfa\xff\x00\x00\x00\x00\xd7\xff\xd4\xff\xe0\xff\xd1\xff\xdf\xff\x00\x00\x00\x00\xf5\xff\xf3\xff\x00\x00\xf0\xff\xc8\xff\xf4\xff\xee\xff\x00\x00\xef\xff\xdc\xff\xdb\xff\xf8\xff\xfe\xff\xe8\xff\xea\xff\xca\xff\xcb\xff\xcf\xff\xed\xff\xec\xff\xc7\xff\xeb\xff\xe9\xff\xf2\xff\x00\x00\x00\x00\x00\x00\xdd\xff\xd0\xff\xd3\xff\xde\xff\xf7\xff\x00\x00\x00\x00\x00\x00\xda\xff\xd6\xff\x00\x00\xf1\xff\xe3\xff\xe5\xff\xe6\xff\xe7\xff\x00\x00\xc9\xff\xe4\xff\xe2\xff\xce\xff\x00\x00\xd2\xff\xd5\xff\xd9\xff\x00\x00\xf6\xff\xcc\xff\x00\x00\xcd\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x04\x00\x05\x00\x06\x00\x10\x00\x08\x00\x04\x00\x05\x00\x06\x00\x10\x00\x08\x00\x12\x00\x01\x00\x10\x00\x03\x00\x12\x00\x01\x00\x01\x00\x10\x00\x03\x00\x12\x00\x04\x00\x05\x00\x06\x00\x01\x00\x08\x00\x0f\x00\x01\x00\x0c\x00\x02\x00\x02\x00\x0f\x00\x02\x00\x10\x00\x06\x00\x12\x00\x02\x00\x01\x00\x0b\x00\x0b\x00\x01\x00\x0d\x00\x0e\x00\x0d\x00\x0e\x00\x0b\x00\x0d\x00\x0d\x00\x0e\x00\x00\x00\x0f\x00\x02\x00\x03\x00\x0b\x00\x0c\x00\x0d\x00\x07\x00\x0b\x00\x09\x00\x0a\x00\x02\x00\x03\x00\x07\x00\x08\x00\x09\x00\x07\x00\x03\x00\x09\x00\x0a\x00\x0f\x00\x07\x00\x11\x00\x09\x00\x0a\x00\x09\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x07\x00\x01\x00\x09\x00\x0a\x00\x10\x00\x0b\x00\x12\x00\x0d\x00\x0e\x00\x07\x00\x01\x00\x09\x00\x0a\x00\x06\x00\x0b\x00\x08\x00\x0d\x00\x0e\x00\x03\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x0d\x00\x0e\x00\x04\x00\x05\x00\x08\x00\x08\x00\x10\x00\x08\x00\x08\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x09\x00\x09\x00\x10\x00\xff\xff\x03\x00\x02\x00\x06\x00\x05\x00\x04\x00\x0a\x00\x02\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\x06\x00\x0b\x00\x0b\x00\x0a\x00\x09\x00\x09\x00\x03\x00\xff\xff\xff\xff\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x4e\x00\x1a\x00\x1b\x00\x2d\x00\x1c\x00\x38\x00\x1a\x00\x1b\x00\x1d\x00\x1c\x00\x3f\x00\x0a\x00\x1d\x00\x0b\x00\x1e\x00\x4a\x00\x0a\x00\x1d\x00\x0b\x00\x1e\x00\x19\x00\x1a\x00\x1b\x00\x4b\x00\x1c\x00\x0c\x00\x4c\x00\x03\x00\x2f\x00\x20\x00\x0c\x00\x28\x00\x1d\x00\x21\x00\x1e\x00\x20\x00\x3b\x00\x30\x00\x22\x00\x3c\x00\x23\x00\x24\x00\x23\x00\x24\x00\x22\x00\x35\x00\x23\x00\x24\x00\x03\x00\x45\x00\x04\x00\x05\x00\x13\x00\x34\x00\x35\x00\x06\x00\x36\x00\x07\x00\x08\x00\x25\x00\x05\x00\x0e\x00\x0f\x00\x10\x00\x06\x00\x24\x00\x07\x00\x08\x00\x28\x00\x06\x00\x29\x00\x07\x00\x08\x00\x12\x00\x2d\x00\x13\x00\x14\x00\x15\x00\x16\x00\x4d\x00\x3d\x00\x07\x00\x08\x00\x1d\x00\x36\x00\x30\x00\x15\x00\x37\x00\x11\x00\x3e\x00\x07\x00\x08\x00\x31\x00\x2d\x00\x1c\x00\x23\x00\x24\x00\x0b\x00\x18\x00\x19\x00\x49\x00\x4a\x00\x23\x00\x24\x00\x18\x00\x19\x00\x42\x00\x46\x00\x2d\x00\x26\x00\x2b\x00\x32\x00\x33\x00\x39\x00\x3a\x00\x0c\x00\x10\x00\x52\x00\x50\x00\xff\xff\x00\x00\x0b\x00\x41\x00\x21\x00\x19\x00\x18\x00\x51\x00\x48\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x42\x00\x44\x00\x45\x00\x2b\x00\x2b\x00\x0b\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 56) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56)
	]

happy_n_terms = 17 :: Int
happy_n_nonterms = 19 :: Int

happyReduce_1 = happySpecReduce_2  0# happyReduction_1
happyReduction_1 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Mtllib happy_var_1) -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Obj Nothing (Just happy_var_1) (toList happy_var_2)
	)}}

happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (Obj Nothing Nothing (toList happy_var_1)
	)}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.TFloat happy_var_1) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.TInt happy_var_1) -> 
	happyIn5
		 (fromIntegral happy_var_1 :: GLfloat
	)}

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.TUInt happy_var_1) -> 
	happyIn5
		 (fromIntegral happy_var_1 :: GLfloat
	)}

happyReduce_6 = happySpecReduce_1  2# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (singleton happy_var_1
	)}

happyReduce_7 = happySpecReduce_2  2# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (happy_var_1 |> happy_var_2
	)}}

happyReduce_8 = happySpecReduce_3  3# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Object happy_var_1) -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 ((\(v,tc,n) -> Group (Just happy_var_1) v tc n happy_var_3 ) happy_var_2
	)}}}

happyReduce_9 = happyReduce 6# 3# happyReduction_9
happyReduction_9 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (L.GroupV happy_var_1) -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOut11 happy_x_5 of { happy_var_5 -> 
	case happyOut8 happy_x_6 of { happy_var_6 -> 
	happyIn7
		 ((\(v,tc,n) -> Group (Just happy_var_1) ((Vertex3 happy_var_2 happy_var_3 happy_var_4) <| v) tc n happy_var_6 ) happy_var_5
	) `HappyStk` happyRest}}}}}}

happyReduce_10 = happySpecReduce_2  3# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 ((\(v,tc,n) -> Group Nothing v tc n happy_var_2 ) happy_var_1
	)}}

happyReduce_11 = happySpecReduce_1  4# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (singleton (IndexBlock Nothing Nothing Nothing happy_var_1)
	)}

happyReduce_12 = happySpecReduce_1  4# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_13 = happySpecReduce_2  5# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (singleton ((\(name,shad,mat) -> IndexBlock name shad mat happy_var_2 ) happy_var_1)
	)}}

happyReduce_14 = happySpecReduce_3  5# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (happy_var_1 |> ((\(name,shad,mat) -> IndexBlock name shad mat happy_var_3 ) happy_var_2)
	)}}}

happyReduce_15 = happySpecReduce_1  6# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((Nothing,happy_var_1,Nothing)
	)}

happyReduce_16 = happySpecReduce_1  6# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Usemtl happy_var_1) -> 
	happyIn10
		 ((Nothing,Nothing,Just happy_var_1)
	)}

happyReduce_17 = happySpecReduce_1  6# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Group happy_var_1) -> 
	happyIn10
		 ((Just happy_var_1,Nothing,Nothing)
	)}

happyReduce_18 = happySpecReduce_2  6# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Group happy_var_1) -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 ((Just happy_var_1,happy_var_2,Nothing)
	)}}

happyReduce_19 = happySpecReduce_2  6# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Group happy_var_1) -> 
	case happyOutTok happy_x_2 of { (L.Usemtl happy_var_2) -> 
	happyIn10
		 ((Just happy_var_1,Nothing,Just happy_var_2)
	)}}

happyReduce_20 = happySpecReduce_2  6# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L.Group happy_var_2) -> 
	happyIn10
		 ((Just happy_var_2,happy_var_1,Nothing)
	)}}

happyReduce_21 = happySpecReduce_2  6# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Usemtl happy_var_1) -> 
	case happyOutTok happy_x_2 of { (L.Group happy_var_2) -> 
	happyIn10
		 ((Just happy_var_2,Nothing,Just happy_var_1)
	)}}

happyReduce_22 = happySpecReduce_2  6# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L.Usemtl happy_var_2) -> 
	happyIn10
		 ((Nothing,happy_var_1,Just happy_var_2)
	)}}

happyReduce_23 = happySpecReduce_2  6# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Usemtl happy_var_1) -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 ((Nothing,happy_var_2,Just happy_var_1)
	)}}

happyReduce_24 = happySpecReduce_3  6# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Group happy_var_1) -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (L.Usemtl happy_var_3) -> 
	happyIn10
		 ((Just happy_var_1,happy_var_2,Just happy_var_3)
	)}}}

happyReduce_25 = happySpecReduce_3  6# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Group happy_var_1) -> 
	case happyOutTok happy_x_2 of { (L.Usemtl happy_var_2) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((Just happy_var_1,happy_var_3,Just happy_var_2)
	)}}}

happyReduce_26 = happySpecReduce_3  6# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L.Group happy_var_2) -> 
	case happyOutTok happy_x_3 of { (L.Usemtl happy_var_3) -> 
	happyIn10
		 ((Just happy_var_2,happy_var_1,Just happy_var_3)
	)}}}

happyReduce_27 = happySpecReduce_3  6# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Usemtl happy_var_1) -> 
	case happyOutTok happy_x_2 of { (L.Group happy_var_2) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((Just happy_var_2,happy_var_3,Just happy_var_1)
	)}}}

happyReduce_28 = happySpecReduce_3  6# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L.Usemtl happy_var_2) -> 
	case happyOutTok happy_x_3 of { (L.Group happy_var_3) -> 
	happyIn10
		 ((Just happy_var_3,happy_var_1,Just happy_var_2)
	)}}}

happyReduce_29 = happySpecReduce_3  6# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Usemtl happy_var_1) -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (L.Group happy_var_3) -> 
	happyIn10
		 ((Just happy_var_3,happy_var_2,Just happy_var_1)
	)}}}

happyReduce_30 = happySpecReduce_1  7# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ((happy_var_1,Nothing,Nothing)
	)}

happyReduce_31 = happySpecReduce_2  7# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 ((happy_var_1,Just happy_var_2,Nothing)
	)}}

happyReduce_32 = happySpecReduce_2  7# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 ((happy_var_1,Nothing,Just happy_var_2)
	)}}

happyReduce_33 = happySpecReduce_3  7# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((happy_var_1,Just happy_var_2,Just happy_var_3)
	)}}}

happyReduce_34 = happySpecReduce_3  7# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((happy_var_1,Just happy_var_3,Just happy_var_2)
	)}}}

happyReduce_35 = happySpecReduce_1  8# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn12
		 (Nothing
	)

happyReduce_36 = happySpecReduce_1  8# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Shading happy_var_1) -> 
	happyIn12
		 (Just happy_var_1
	)}

happyReduce_37 = happyReduce 4# 9# happyReduction_37
happyReduction_37 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (Vertex3 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_38 = happyReduce 5# 9# happyReduction_38
happyReduction_38 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (Vertex3 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_39 = happySpecReduce_1  10# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (singleton happy_var_1
	)}

happyReduce_40 = happySpecReduce_2  10# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (happy_var_1 |> happy_var_2
	)}}

happyReduce_41 = happySpecReduce_3  11# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (Vector2 happy_var_2 happy_var_3
	)}}

happyReduce_42 = happyReduce 4# 11# happyReduction_42
happyReduction_42 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (Vector2 happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_43 = happySpecReduce_1  12# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (singleton happy_var_1
	)}

happyReduce_44 = happySpecReduce_2  12# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (happy_var_1 |> happy_var_2
	)}}

happyReduce_45 = happyReduce 4# 13# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 (Vector3 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_46 = happySpecReduce_1  14# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (singleton happy_var_1
	)}

happyReduce_47 = happySpecReduce_2  14# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (happy_var_1 |> happy_var_2
	)}}

happyReduce_48 = happySpecReduce_1  15# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.TUInt happy_var_1) -> 
	happyIn19
		 (FaceNode (happy_var_1-1) Nothing Nothing
	)}

happyReduce_49 = happySpecReduce_3  15# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.TUInt happy_var_1) -> 
	case happyOutTok happy_x_3 of { (L.TUInt happy_var_3) -> 
	happyIn19
		 (FaceNode (happy_var_1-1) (Just (happy_var_3-1)) Nothing
	)}}

happyReduce_50 = happyReduce 5# 15# happyReduction_50
happyReduction_50 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (L.TUInt happy_var_1) -> 
	case happyOutTok happy_x_3 of { (L.TUInt happy_var_3) -> 
	case happyOutTok happy_x_5 of { (L.TUInt happy_var_5) -> 
	happyIn19
		 (FaceNode (happy_var_1-1) (Just (happy_var_3-1)) (Just (happy_var_5-1))
	) `HappyStk` happyRest}}}

happyReduce_51 = happyReduce 4# 15# happyReduction_51
happyReduction_51 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (L.TUInt happy_var_1) -> 
	case happyOutTok happy_x_4 of { (L.TUInt happy_var_4) -> 
	happyIn19
		 (FaceNode (happy_var_1-1) Nothing (Just (happy_var_4-1))
	) `HappyStk` happyRest}}

happyReduce_52 = happySpecReduce_2  16# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (happy_var_2
	)}

happyReduce_53 = happySpecReduce_1  17# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (singleton happy_var_1
	)}

happyReduce_54 = happySpecReduce_2  17# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (happy_var_1 |> happy_var_2
	)}}

happyReduce_55 = happySpecReduce_1  18# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (singleton happy_var_1
	)}

happyReduce_56 = happySpecReduce_2  18# happyReduction_56
happyReduction_56 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (happy_var_1 |> happy_var_2
	)}}

happyNewToken action sts stk [] =
	happyDoAction 16# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	L.Object happy_dollar_dollar -> cont 1#;
	L.Group happy_dollar_dollar -> cont 2#;
	L.Vertex -> cont 3#;
	L.VertexTexture -> cont 4#;
	L.VertexNormal -> cont 5#;
	L.Face -> cont 6#;
	L.TFloat happy_dollar_dollar -> cont 7#;
	L.TInt happy_dollar_dollar -> cont 8#;
	L.TUInt happy_dollar_dollar -> cont 9#;
	L.TDiv -> cont 10#;
	L.Usemtl happy_dollar_dollar -> cont 11#;
	L.Mtllib happy_dollar_dollar -> cont 12#;
	L.ShadingOff -> cont 13#;
	L.Shading happy_dollar_dollar -> cont 14#;
	L.GroupV happy_dollar_dollar -> cont 15#;
	_ -> happyError' (tk:tks)
	}

happyError_ 16# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(L.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseObj tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


--errStrPut = hPutStrLn stderr

--printError = lift.errStrPut

--sprint = lift.putStrLn

parseError :: [L.Token] -> b
parseError t = error ("Parse error: " ++ show t)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc5020_0\\ghc_2.h" #-}
















































































































































{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates\\GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
