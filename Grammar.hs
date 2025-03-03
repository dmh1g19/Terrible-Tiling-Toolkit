{-# OPTIONS_GHC -w #-}
module Grammar where 
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,341) ([2560,24576,24576,32792,2,24,1560,2048,0,0,0,0,0,0,10,96,6240,512,0,0,32768,0,0,0,32,64512,24191,0,0,0,16,2,65472,1511,64,0,0,0,0,0,0,128,0,0,640,6144,6144,36870,8192,65520,377,0,0,0,2048,0,40959,23,2,0,0,128,0,0,8192,0,0,0,8,0,0,512,0,0,32768,0,0,0,32,0,0,2048,0,0,0,2,0,0,128,0,0,8192,0,0,0,8,0,0,512,0,0,32768,0,0,0,32,0,0,2048,0,0,0,2,0,0,0,0,0,0,0,0,64,0,0,0,33344,137,0,36868,8800,0,256,64,0,0,2560,24576,24576,24,0,0,0,0,0,0,4096,32640,0,0,6160,0,0,0,0,0,36864,8800,0,256,0,0,0,0,0,0,16384,35202,0,1024,144,32,0,1,0,0,0,6160,0,0,0,0,0,32768,0,65520,377,32,64512,24191,2048,0,40959,23,2,65472,1511,128,61440,31231,8193,0,32764,94,8,65280,6047,512,49152,59391,32773,0,65520,377,32,64512,24191,2048,0,40959,23,2,65472,1511,128,61440,31231,8193,0,32764,94,8,65280,6047,512,49152,59391,32773,0,65520,377,64,0,0,0,7936,0,0,0,0,0,144,61472,31231,1,0,0,0,16,31,0,576,128,0,36868,8192,0,256,36,8,16384,2304,512,0,16400,32770,0,1024,0,0,0,0,4,0,0,256,0,0,16384,0,0,0,16,0,0,64,0,0,4096,0,0,0,4,0,0,256,0,0,0,4,0,0,16,0,0,1024,0,0,0,1,0,0,64,0,0,0,1,0,0,64,0,0,256,0,0,0,4,0,0,32,0,0,33344,137,0,36868,8800,0,256,0,0,0,2304,512,0,16,0,0,0,320,510,0,16384,96,0,0,32,0,0,256,0,0,16384,0,0,0,52,8,16384,3328,512,0,16,49152,7,0,144,32,0,1,31744,0,0,9,2,4096,576,128,0,36868,8192,0,256,40,384,24960,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,96,6240,576,128,0,4,0,0,0,32,64512,24191,2304,512,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,576,128,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,144,32,0,9217,2048,0,64,8,65280,6047,512,49152,59391,5,0,0,0,0,64,0,0,4096,0,0,0,7,0,0,448,0,16384,0,0,0,16,0,0,1024,1984,0,0,61441,1,0,64,124,0,0,7937,0,0,4,0,0,256,496,0,0,1,0,0,64,0,0,0,1984,0,0,61440,1,0,0,124,0,0,7936,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,576,128,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31748,0,0,32,0,0,640,6144,6144,36870,8192,0,256,1024,124,0,16384,0,0,0,0,0,0,144,32,0,16385,31744,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","MultiExp","Exp","ExpInt","ExpBool","ExpTile","int","doNothing","'='","'('","')'","'{'","'}'","';'","','","true","false","'&&'","'||'","'!'","'<'","'>'","'+'","'-'","'*'","'/'","'^'","if","while","else","combineTilesRight","combineTilesDown","duplicateTileRight","duplicateTileDown","rotateTile90Degrees","rotateTile180Degrees","rotateTile270Degrees","squareRotateTile","scaleTile","createBlankTile","reflectTileX","reflectTileY","reflectTileXY","print","getTileFile","createSubTile","conjunctTiles","negateTile","removeTop","return","tileVar","%eof"]
        bit_start = st Prelude.* 54
        bit_end = (st Prelude.+ 1) Prelude.* 54
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..53]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (10) = happyShift action_3
action_0 (12) = happyShift action_4
action_0 (30) = happyShift action_5
action_0 (31) = happyShift action_6
action_0 (46) = happyShift action_7
action_0 (47) = happyShift action_8
action_0 (52) = happyShift action_9
action_0 (53) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_12
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_3
action_1 (12) = happyShift action_4
action_1 (30) = happyShift action_5
action_1 (31) = happyShift action_6
action_1 (46) = happyShift action_7
action_1 (47) = happyShift action_8
action_1 (52) = happyShift action_9
action_1 (53) = happyShift action_10
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (16) = happyShift action_40
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_9

action_4 (10) = happyShift action_3
action_4 (12) = happyShift action_4
action_4 (30) = happyShift action_5
action_4 (31) = happyShift action_6
action_4 (46) = happyShift action_7
action_4 (47) = happyShift action_8
action_4 (52) = happyShift action_9
action_4 (53) = happyShift action_10
action_4 (5) = happyGoto action_39
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (12) = happyShift action_38
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (12) = happyShift action_37
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (12) = happyShift action_16
action_7 (33) = happyShift action_17
action_7 (34) = happyShift action_18
action_7 (35) = happyShift action_19
action_7 (36) = happyShift action_20
action_7 (37) = happyShift action_21
action_7 (38) = happyShift action_22
action_7 (39) = happyShift action_23
action_7 (40) = happyShift action_24
action_7 (41) = happyShift action_25
action_7 (42) = happyShift action_26
action_7 (43) = happyShift action_27
action_7 (44) = happyShift action_28
action_7 (45) = happyShift action_29
action_7 (48) = happyShift action_30
action_7 (49) = happyShift action_31
action_7 (50) = happyShift action_32
action_7 (51) = happyShift action_33
action_7 (53) = happyShift action_34
action_7 (8) = happyGoto action_36
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (53) = happyShift action_35
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (12) = happyShift action_16
action_9 (33) = happyShift action_17
action_9 (34) = happyShift action_18
action_9 (35) = happyShift action_19
action_9 (36) = happyShift action_20
action_9 (37) = happyShift action_21
action_9 (38) = happyShift action_22
action_9 (39) = happyShift action_23
action_9 (40) = happyShift action_24
action_9 (41) = happyShift action_25
action_9 (42) = happyShift action_26
action_9 (43) = happyShift action_27
action_9 (44) = happyShift action_28
action_9 (45) = happyShift action_29
action_9 (48) = happyShift action_30
action_9 (49) = happyShift action_31
action_9 (50) = happyShift action_32
action_9 (51) = happyShift action_33
action_9 (53) = happyShift action_34
action_9 (8) = happyGoto action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (11) = happyShift action_14
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (54) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (16) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (10) = happyShift action_3
action_13 (12) = happyShift action_4
action_13 (30) = happyShift action_5
action_13 (31) = happyShift action_6
action_13 (46) = happyShift action_7
action_13 (47) = happyShift action_8
action_13 (52) = happyShift action_9
action_13 (53) = happyShift action_10
action_13 (4) = happyGoto action_41
action_13 (5) = happyGoto action_12
action_13 _ = happyReduce_2

action_14 (9) = happyShift action_45
action_14 (12) = happyShift action_74
action_14 (26) = happyShift action_50
action_14 (33) = happyShift action_17
action_14 (34) = happyShift action_18
action_14 (35) = happyShift action_19
action_14 (36) = happyShift action_20
action_14 (37) = happyShift action_21
action_14 (38) = happyShift action_22
action_14 (39) = happyShift action_23
action_14 (40) = happyShift action_24
action_14 (41) = happyShift action_25
action_14 (42) = happyShift action_26
action_14 (43) = happyShift action_27
action_14 (44) = happyShift action_28
action_14 (45) = happyShift action_29
action_14 (48) = happyShift action_30
action_14 (49) = happyShift action_31
action_14 (50) = happyShift action_32
action_14 (51) = happyShift action_33
action_14 (53) = happyShift action_75
action_14 (6) = happyGoto action_72
action_14 (8) = happyGoto action_73
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_11

action_16 (12) = happyShift action_16
action_16 (33) = happyShift action_17
action_16 (34) = happyShift action_18
action_16 (35) = happyShift action_19
action_16 (36) = happyShift action_20
action_16 (37) = happyShift action_21
action_16 (38) = happyShift action_22
action_16 (39) = happyShift action_23
action_16 (40) = happyShift action_24
action_16 (41) = happyShift action_25
action_16 (42) = happyShift action_26
action_16 (43) = happyShift action_27
action_16 (44) = happyShift action_28
action_16 (45) = happyShift action_29
action_16 (48) = happyShift action_30
action_16 (49) = happyShift action_31
action_16 (50) = happyShift action_32
action_16 (51) = happyShift action_33
action_16 (53) = happyShift action_34
action_16 (8) = happyGoto action_71
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (12) = happyShift action_70
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (12) = happyShift action_69
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (12) = happyShift action_68
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (12) = happyShift action_67
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (12) = happyShift action_66
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (12) = happyShift action_65
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (12) = happyShift action_64
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (12) = happyShift action_63
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (12) = happyShift action_62
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (12) = happyShift action_61
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (12) = happyShift action_60
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (12) = happyShift action_59
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (12) = happyShift action_58
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (12) = happyShift action_57
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (12) = happyShift action_56
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (12) = happyShift action_55
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (12) = happyShift action_54
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_33

action_35 (53) = happyShift action_53
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_8

action_37 (9) = happyShift action_45
action_37 (12) = happyShift action_46
action_37 (18) = happyShift action_47
action_37 (19) = happyShift action_48
action_37 (22) = happyShift action_49
action_37 (26) = happyShift action_50
action_37 (53) = happyShift action_51
action_37 (6) = happyGoto action_43
action_37 (7) = happyGoto action_52
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (9) = happyShift action_45
action_38 (12) = happyShift action_46
action_38 (18) = happyShift action_47
action_38 (19) = happyShift action_48
action_38 (22) = happyShift action_49
action_38 (26) = happyShift action_50
action_38 (53) = happyShift action_51
action_38 (6) = happyGoto action_43
action_38 (7) = happyGoto action_44
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (13) = happyShift action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (10) = happyShift action_3
action_40 (12) = happyShift action_4
action_40 (30) = happyShift action_5
action_40 (31) = happyShift action_6
action_40 (46) = happyShift action_7
action_40 (47) = happyShift action_8
action_40 (52) = happyShift action_9
action_40 (53) = happyShift action_10
action_40 (4) = happyGoto action_41
action_40 (5) = happyGoto action_12
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_1

action_42 _ = happyReduce_3

action_43 (11) = happyShift action_109
action_43 (22) = happyShift action_110
action_43 (23) = happyShift action_111
action_43 (24) = happyShift action_112
action_43 (25) = happyShift action_77
action_43 (26) = happyShift action_78
action_43 (27) = happyShift action_79
action_43 (28) = happyShift action_80
action_43 (29) = happyShift action_81
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (13) = happyShift action_108
action_44 (20) = happyShift action_101
action_44 (21) = happyShift action_102
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_12

action_46 (9) = happyShift action_45
action_46 (12) = happyShift action_46
action_46 (18) = happyShift action_47
action_46 (19) = happyShift action_48
action_46 (22) = happyShift action_49
action_46 (26) = happyShift action_50
action_46 (53) = happyShift action_51
action_46 (6) = happyGoto action_106
action_46 (7) = happyGoto action_107
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_21

action_48 _ = happyReduce_22

action_49 (9) = happyShift action_45
action_49 (12) = happyShift action_46
action_49 (18) = happyShift action_47
action_49 (19) = happyShift action_48
action_49 (22) = happyShift action_49
action_49 (26) = happyShift action_50
action_49 (53) = happyShift action_51
action_49 (6) = happyGoto action_43
action_49 (7) = happyGoto action_105
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (9) = happyShift action_45
action_50 (12) = happyShift action_104
action_50 (26) = happyShift action_50
action_50 (53) = happyShift action_51
action_50 (6) = happyGoto action_103
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_13

action_52 (13) = happyShift action_100
action_52 (20) = happyShift action_101
action_52 (21) = happyShift action_102
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_10

action_54 (12) = happyShift action_16
action_54 (33) = happyShift action_17
action_54 (34) = happyShift action_18
action_54 (35) = happyShift action_19
action_54 (36) = happyShift action_20
action_54 (37) = happyShift action_21
action_54 (38) = happyShift action_22
action_54 (39) = happyShift action_23
action_54 (40) = happyShift action_24
action_54 (41) = happyShift action_25
action_54 (42) = happyShift action_26
action_54 (43) = happyShift action_27
action_54 (44) = happyShift action_28
action_54 (45) = happyShift action_29
action_54 (48) = happyShift action_30
action_54 (49) = happyShift action_31
action_54 (50) = happyShift action_32
action_54 (51) = happyShift action_33
action_54 (53) = happyShift action_34
action_54 (8) = happyGoto action_99
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (12) = happyShift action_16
action_55 (33) = happyShift action_17
action_55 (34) = happyShift action_18
action_55 (35) = happyShift action_19
action_55 (36) = happyShift action_20
action_55 (37) = happyShift action_21
action_55 (38) = happyShift action_22
action_55 (39) = happyShift action_23
action_55 (40) = happyShift action_24
action_55 (41) = happyShift action_25
action_55 (42) = happyShift action_26
action_55 (43) = happyShift action_27
action_55 (44) = happyShift action_28
action_55 (45) = happyShift action_29
action_55 (48) = happyShift action_30
action_55 (49) = happyShift action_31
action_55 (50) = happyShift action_32
action_55 (51) = happyShift action_33
action_55 (53) = happyShift action_34
action_55 (8) = happyGoto action_98
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (12) = happyShift action_16
action_56 (33) = happyShift action_17
action_56 (34) = happyShift action_18
action_56 (35) = happyShift action_19
action_56 (36) = happyShift action_20
action_56 (37) = happyShift action_21
action_56 (38) = happyShift action_22
action_56 (39) = happyShift action_23
action_56 (40) = happyShift action_24
action_56 (41) = happyShift action_25
action_56 (42) = happyShift action_26
action_56 (43) = happyShift action_27
action_56 (44) = happyShift action_28
action_56 (45) = happyShift action_29
action_56 (48) = happyShift action_30
action_56 (49) = happyShift action_31
action_56 (50) = happyShift action_32
action_56 (51) = happyShift action_33
action_56 (53) = happyShift action_34
action_56 (8) = happyGoto action_97
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (12) = happyShift action_16
action_57 (33) = happyShift action_17
action_57 (34) = happyShift action_18
action_57 (35) = happyShift action_19
action_57 (36) = happyShift action_20
action_57 (37) = happyShift action_21
action_57 (38) = happyShift action_22
action_57 (39) = happyShift action_23
action_57 (40) = happyShift action_24
action_57 (41) = happyShift action_25
action_57 (42) = happyShift action_26
action_57 (43) = happyShift action_27
action_57 (44) = happyShift action_28
action_57 (45) = happyShift action_29
action_57 (48) = happyShift action_30
action_57 (49) = happyShift action_31
action_57 (50) = happyShift action_32
action_57 (51) = happyShift action_33
action_57 (53) = happyShift action_34
action_57 (8) = happyGoto action_96
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (12) = happyShift action_16
action_58 (33) = happyShift action_17
action_58 (34) = happyShift action_18
action_58 (35) = happyShift action_19
action_58 (36) = happyShift action_20
action_58 (37) = happyShift action_21
action_58 (38) = happyShift action_22
action_58 (39) = happyShift action_23
action_58 (40) = happyShift action_24
action_58 (41) = happyShift action_25
action_58 (42) = happyShift action_26
action_58 (43) = happyShift action_27
action_58 (44) = happyShift action_28
action_58 (45) = happyShift action_29
action_58 (48) = happyShift action_30
action_58 (49) = happyShift action_31
action_58 (50) = happyShift action_32
action_58 (51) = happyShift action_33
action_58 (53) = happyShift action_34
action_58 (8) = happyGoto action_95
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (12) = happyShift action_16
action_59 (33) = happyShift action_17
action_59 (34) = happyShift action_18
action_59 (35) = happyShift action_19
action_59 (36) = happyShift action_20
action_59 (37) = happyShift action_21
action_59 (38) = happyShift action_22
action_59 (39) = happyShift action_23
action_59 (40) = happyShift action_24
action_59 (41) = happyShift action_25
action_59 (42) = happyShift action_26
action_59 (43) = happyShift action_27
action_59 (44) = happyShift action_28
action_59 (45) = happyShift action_29
action_59 (48) = happyShift action_30
action_59 (49) = happyShift action_31
action_59 (50) = happyShift action_32
action_59 (51) = happyShift action_33
action_59 (53) = happyShift action_34
action_59 (8) = happyGoto action_94
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (12) = happyShift action_16
action_60 (33) = happyShift action_17
action_60 (34) = happyShift action_18
action_60 (35) = happyShift action_19
action_60 (36) = happyShift action_20
action_60 (37) = happyShift action_21
action_60 (38) = happyShift action_22
action_60 (39) = happyShift action_23
action_60 (40) = happyShift action_24
action_60 (41) = happyShift action_25
action_60 (42) = happyShift action_26
action_60 (43) = happyShift action_27
action_60 (44) = happyShift action_28
action_60 (45) = happyShift action_29
action_60 (48) = happyShift action_30
action_60 (49) = happyShift action_31
action_60 (50) = happyShift action_32
action_60 (51) = happyShift action_33
action_60 (53) = happyShift action_34
action_60 (8) = happyGoto action_93
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (12) = happyShift action_16
action_61 (33) = happyShift action_17
action_61 (34) = happyShift action_18
action_61 (35) = happyShift action_19
action_61 (36) = happyShift action_20
action_61 (37) = happyShift action_21
action_61 (38) = happyShift action_22
action_61 (39) = happyShift action_23
action_61 (40) = happyShift action_24
action_61 (41) = happyShift action_25
action_61 (42) = happyShift action_26
action_61 (43) = happyShift action_27
action_61 (44) = happyShift action_28
action_61 (45) = happyShift action_29
action_61 (48) = happyShift action_30
action_61 (49) = happyShift action_31
action_61 (50) = happyShift action_32
action_61 (51) = happyShift action_33
action_61 (53) = happyShift action_34
action_61 (8) = happyGoto action_92
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (12) = happyShift action_16
action_62 (33) = happyShift action_17
action_62 (34) = happyShift action_18
action_62 (35) = happyShift action_19
action_62 (36) = happyShift action_20
action_62 (37) = happyShift action_21
action_62 (38) = happyShift action_22
action_62 (39) = happyShift action_23
action_62 (40) = happyShift action_24
action_62 (41) = happyShift action_25
action_62 (42) = happyShift action_26
action_62 (43) = happyShift action_27
action_62 (44) = happyShift action_28
action_62 (45) = happyShift action_29
action_62 (48) = happyShift action_30
action_62 (49) = happyShift action_31
action_62 (50) = happyShift action_32
action_62 (51) = happyShift action_33
action_62 (53) = happyShift action_34
action_62 (8) = happyGoto action_91
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (12) = happyShift action_16
action_63 (33) = happyShift action_17
action_63 (34) = happyShift action_18
action_63 (35) = happyShift action_19
action_63 (36) = happyShift action_20
action_63 (37) = happyShift action_21
action_63 (38) = happyShift action_22
action_63 (39) = happyShift action_23
action_63 (40) = happyShift action_24
action_63 (41) = happyShift action_25
action_63 (42) = happyShift action_26
action_63 (43) = happyShift action_27
action_63 (44) = happyShift action_28
action_63 (45) = happyShift action_29
action_63 (48) = happyShift action_30
action_63 (49) = happyShift action_31
action_63 (50) = happyShift action_32
action_63 (51) = happyShift action_33
action_63 (53) = happyShift action_34
action_63 (8) = happyGoto action_90
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (12) = happyShift action_16
action_64 (33) = happyShift action_17
action_64 (34) = happyShift action_18
action_64 (35) = happyShift action_19
action_64 (36) = happyShift action_20
action_64 (37) = happyShift action_21
action_64 (38) = happyShift action_22
action_64 (39) = happyShift action_23
action_64 (40) = happyShift action_24
action_64 (41) = happyShift action_25
action_64 (42) = happyShift action_26
action_64 (43) = happyShift action_27
action_64 (44) = happyShift action_28
action_64 (45) = happyShift action_29
action_64 (48) = happyShift action_30
action_64 (49) = happyShift action_31
action_64 (50) = happyShift action_32
action_64 (51) = happyShift action_33
action_64 (53) = happyShift action_34
action_64 (8) = happyGoto action_89
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (12) = happyShift action_16
action_65 (33) = happyShift action_17
action_65 (34) = happyShift action_18
action_65 (35) = happyShift action_19
action_65 (36) = happyShift action_20
action_65 (37) = happyShift action_21
action_65 (38) = happyShift action_22
action_65 (39) = happyShift action_23
action_65 (40) = happyShift action_24
action_65 (41) = happyShift action_25
action_65 (42) = happyShift action_26
action_65 (43) = happyShift action_27
action_65 (44) = happyShift action_28
action_65 (45) = happyShift action_29
action_65 (48) = happyShift action_30
action_65 (49) = happyShift action_31
action_65 (50) = happyShift action_32
action_65 (51) = happyShift action_33
action_65 (53) = happyShift action_34
action_65 (8) = happyGoto action_88
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (12) = happyShift action_16
action_66 (33) = happyShift action_17
action_66 (34) = happyShift action_18
action_66 (35) = happyShift action_19
action_66 (36) = happyShift action_20
action_66 (37) = happyShift action_21
action_66 (38) = happyShift action_22
action_66 (39) = happyShift action_23
action_66 (40) = happyShift action_24
action_66 (41) = happyShift action_25
action_66 (42) = happyShift action_26
action_66 (43) = happyShift action_27
action_66 (44) = happyShift action_28
action_66 (45) = happyShift action_29
action_66 (48) = happyShift action_30
action_66 (49) = happyShift action_31
action_66 (50) = happyShift action_32
action_66 (51) = happyShift action_33
action_66 (53) = happyShift action_34
action_66 (8) = happyGoto action_87
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (12) = happyShift action_16
action_67 (33) = happyShift action_17
action_67 (34) = happyShift action_18
action_67 (35) = happyShift action_19
action_67 (36) = happyShift action_20
action_67 (37) = happyShift action_21
action_67 (38) = happyShift action_22
action_67 (39) = happyShift action_23
action_67 (40) = happyShift action_24
action_67 (41) = happyShift action_25
action_67 (42) = happyShift action_26
action_67 (43) = happyShift action_27
action_67 (44) = happyShift action_28
action_67 (45) = happyShift action_29
action_67 (48) = happyShift action_30
action_67 (49) = happyShift action_31
action_67 (50) = happyShift action_32
action_67 (51) = happyShift action_33
action_67 (53) = happyShift action_34
action_67 (8) = happyGoto action_86
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (12) = happyShift action_16
action_68 (33) = happyShift action_17
action_68 (34) = happyShift action_18
action_68 (35) = happyShift action_19
action_68 (36) = happyShift action_20
action_68 (37) = happyShift action_21
action_68 (38) = happyShift action_22
action_68 (39) = happyShift action_23
action_68 (40) = happyShift action_24
action_68 (41) = happyShift action_25
action_68 (42) = happyShift action_26
action_68 (43) = happyShift action_27
action_68 (44) = happyShift action_28
action_68 (45) = happyShift action_29
action_68 (48) = happyShift action_30
action_68 (49) = happyShift action_31
action_68 (50) = happyShift action_32
action_68 (51) = happyShift action_33
action_68 (53) = happyShift action_34
action_68 (8) = happyGoto action_85
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (12) = happyShift action_16
action_69 (33) = happyShift action_17
action_69 (34) = happyShift action_18
action_69 (35) = happyShift action_19
action_69 (36) = happyShift action_20
action_69 (37) = happyShift action_21
action_69 (38) = happyShift action_22
action_69 (39) = happyShift action_23
action_69 (40) = happyShift action_24
action_69 (41) = happyShift action_25
action_69 (42) = happyShift action_26
action_69 (43) = happyShift action_27
action_69 (44) = happyShift action_28
action_69 (45) = happyShift action_29
action_69 (48) = happyShift action_30
action_69 (49) = happyShift action_31
action_69 (50) = happyShift action_32
action_69 (51) = happyShift action_33
action_69 (53) = happyShift action_34
action_69 (8) = happyGoto action_84
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (12) = happyShift action_16
action_70 (33) = happyShift action_17
action_70 (34) = happyShift action_18
action_70 (35) = happyShift action_19
action_70 (36) = happyShift action_20
action_70 (37) = happyShift action_21
action_70 (38) = happyShift action_22
action_70 (39) = happyShift action_23
action_70 (40) = happyShift action_24
action_70 (41) = happyShift action_25
action_70 (42) = happyShift action_26
action_70 (43) = happyShift action_27
action_70 (44) = happyShift action_28
action_70 (45) = happyShift action_29
action_70 (48) = happyShift action_30
action_70 (49) = happyShift action_31
action_70 (50) = happyShift action_32
action_70 (51) = happyShift action_33
action_70 (53) = happyShift action_34
action_70 (8) = happyGoto action_83
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (13) = happyShift action_82
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (25) = happyShift action_77
action_72 (26) = happyShift action_78
action_72 (27) = happyShift action_79
action_72 (28) = happyShift action_80
action_72 (29) = happyShift action_81
action_72 _ = happyReduce_7

action_73 _ = happyReduce_6

action_74 (9) = happyShift action_45
action_74 (12) = happyShift action_74
action_74 (26) = happyShift action_50
action_74 (33) = happyShift action_17
action_74 (34) = happyShift action_18
action_74 (35) = happyShift action_19
action_74 (36) = happyShift action_20
action_74 (37) = happyShift action_21
action_74 (38) = happyShift action_22
action_74 (39) = happyShift action_23
action_74 (40) = happyShift action_24
action_74 (41) = happyShift action_25
action_74 (42) = happyShift action_26
action_74 (43) = happyShift action_27
action_74 (44) = happyShift action_28
action_74 (45) = happyShift action_29
action_74 (48) = happyShift action_30
action_74 (49) = happyShift action_31
action_74 (50) = happyShift action_32
action_74 (51) = happyShift action_33
action_74 (53) = happyShift action_75
action_74 (6) = happyGoto action_76
action_74 (8) = happyGoto action_71
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (13) = happyReduce_13
action_75 (16) = happyReduce_13
action_75 _ = happyReduce_13

action_76 (13) = happyShift action_121
action_76 (25) = happyShift action_77
action_76 (26) = happyShift action_78
action_76 (27) = happyShift action_79
action_76 (28) = happyShift action_80
action_76 (29) = happyShift action_81
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (9) = happyShift action_45
action_77 (12) = happyShift action_104
action_77 (26) = happyShift action_50
action_77 (53) = happyShift action_51
action_77 (6) = happyGoto action_146
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (9) = happyShift action_45
action_78 (12) = happyShift action_104
action_78 (26) = happyShift action_50
action_78 (53) = happyShift action_51
action_78 (6) = happyGoto action_145
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (9) = happyShift action_45
action_79 (12) = happyShift action_104
action_79 (26) = happyShift action_50
action_79 (53) = happyShift action_51
action_79 (6) = happyGoto action_144
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (9) = happyShift action_45
action_80 (12) = happyShift action_104
action_80 (26) = happyShift action_50
action_80 (53) = happyShift action_51
action_80 (6) = happyGoto action_143
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (9) = happyShift action_45
action_81 (12) = happyShift action_104
action_81 (26) = happyShift action_50
action_81 (53) = happyShift action_51
action_81 (6) = happyGoto action_142
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_47

action_83 (17) = happyShift action_141
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (17) = happyShift action_140
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (17) = happyShift action_139
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (17) = happyShift action_138
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (13) = happyShift action_137
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (13) = happyShift action_136
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (13) = happyShift action_135
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (13) = happyShift action_134
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (17) = happyShift action_133
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (13) = happyShift action_132
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (13) = happyShift action_131
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (13) = happyShift action_130
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (13) = happyShift action_129
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (17) = happyShift action_128
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (17) = happyShift action_127
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (13) = happyShift action_126
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (17) = happyShift action_125
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (14) = happyShift action_124
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (9) = happyShift action_45
action_101 (12) = happyShift action_46
action_101 (18) = happyShift action_47
action_101 (19) = happyShift action_48
action_101 (22) = happyShift action_49
action_101 (26) = happyShift action_50
action_101 (53) = happyShift action_51
action_101 (6) = happyGoto action_43
action_101 (7) = happyGoto action_123
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (9) = happyShift action_45
action_102 (12) = happyShift action_46
action_102 (18) = happyShift action_47
action_102 (19) = happyShift action_48
action_102 (22) = happyShift action_49
action_102 (26) = happyShift action_50
action_102 (53) = happyShift action_51
action_102 (6) = happyGoto action_43
action_102 (7) = happyGoto action_122
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_14

action_104 (9) = happyShift action_45
action_104 (12) = happyShift action_104
action_104 (26) = happyShift action_50
action_104 (53) = happyShift action_51
action_104 (6) = happyGoto action_76
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_25

action_106 (11) = happyShift action_109
action_106 (13) = happyShift action_121
action_106 (22) = happyShift action_110
action_106 (23) = happyShift action_111
action_106 (24) = happyShift action_112
action_106 (25) = happyShift action_77
action_106 (26) = happyShift action_78
action_106 (27) = happyShift action_79
action_106 (28) = happyShift action_80
action_106 (29) = happyShift action_81
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (13) = happyShift action_120
action_107 (20) = happyShift action_101
action_107 (21) = happyShift action_102
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (14) = happyShift action_119
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (11) = happyShift action_118
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (11) = happyShift action_117
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (9) = happyShift action_45
action_111 (11) = happyShift action_116
action_111 (12) = happyShift action_104
action_111 (26) = happyShift action_50
action_111 (53) = happyShift action_51
action_111 (6) = happyGoto action_115
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (9) = happyShift action_45
action_112 (11) = happyShift action_114
action_112 (12) = happyShift action_104
action_112 (26) = happyShift action_50
action_112 (53) = happyShift action_51
action_112 (6) = happyGoto action_113
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (25) = happyShift action_77
action_113 (26) = happyShift action_78
action_113 (27) = happyShift action_79
action_113 (28) = happyShift action_80
action_113 (29) = happyShift action_81
action_113 _ = happyReduce_28

action_114 (9) = happyShift action_45
action_114 (12) = happyShift action_104
action_114 (26) = happyShift action_50
action_114 (53) = happyShift action_51
action_114 (6) = happyGoto action_160
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (25) = happyShift action_77
action_115 (26) = happyShift action_78
action_115 (27) = happyShift action_79
action_115 (28) = happyShift action_80
action_115 (29) = happyShift action_81
action_115 _ = happyReduce_26

action_116 (9) = happyShift action_45
action_116 (12) = happyShift action_104
action_116 (26) = happyShift action_50
action_116 (53) = happyShift action_51
action_116 (6) = happyGoto action_159
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (9) = happyShift action_45
action_117 (12) = happyShift action_104
action_117 (26) = happyShift action_50
action_117 (53) = happyShift action_51
action_117 (6) = happyGoto action_158
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (9) = happyShift action_45
action_118 (12) = happyShift action_104
action_118 (26) = happyShift action_50
action_118 (53) = happyShift action_51
action_118 (6) = happyGoto action_157
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (10) = happyShift action_3
action_119 (12) = happyShift action_4
action_119 (30) = happyShift action_5
action_119 (31) = happyShift action_6
action_119 (46) = happyShift action_7
action_119 (47) = happyShift action_8
action_119 (52) = happyShift action_9
action_119 (53) = happyShift action_10
action_119 (4) = happyGoto action_156
action_119 (5) = happyGoto action_12
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_32

action_121 _ = happyReduce_20

action_122 _ = happyReduce_24

action_123 _ = happyReduce_23

action_124 (10) = happyShift action_3
action_124 (12) = happyShift action_4
action_124 (30) = happyShift action_5
action_124 (31) = happyShift action_6
action_124 (46) = happyShift action_7
action_124 (47) = happyShift action_8
action_124 (52) = happyShift action_9
action_124 (53) = happyShift action_10
action_124 (4) = happyGoto action_155
action_124 (5) = happyGoto action_12
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (9) = happyShift action_45
action_125 (12) = happyShift action_104
action_125 (26) = happyShift action_50
action_125 (53) = happyShift action_51
action_125 (6) = happyGoto action_154
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_50

action_127 (12) = happyShift action_16
action_127 (33) = happyShift action_17
action_127 (34) = happyShift action_18
action_127 (35) = happyShift action_19
action_127 (36) = happyShift action_20
action_127 (37) = happyShift action_21
action_127 (38) = happyShift action_22
action_127 (39) = happyShift action_23
action_127 (40) = happyShift action_24
action_127 (41) = happyShift action_25
action_127 (42) = happyShift action_26
action_127 (43) = happyShift action_27
action_127 (44) = happyShift action_28
action_127 (45) = happyShift action_29
action_127 (48) = happyShift action_30
action_127 (49) = happyShift action_31
action_127 (50) = happyShift action_32
action_127 (51) = happyShift action_33
action_127 (53) = happyShift action_34
action_127 (8) = happyGoto action_153
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (9) = happyShift action_45
action_128 (12) = happyShift action_104
action_128 (26) = happyShift action_50
action_128 (53) = happyShift action_51
action_128 (6) = happyGoto action_152
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_46

action_130 _ = happyReduce_45

action_131 _ = happyReduce_44

action_132 _ = happyReduce_43

action_133 (9) = happyShift action_45
action_133 (12) = happyShift action_104
action_133 (26) = happyShift action_50
action_133 (53) = happyShift action_51
action_133 (6) = happyGoto action_151
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_41

action_135 _ = happyReduce_40

action_136 _ = happyReduce_39

action_137 _ = happyReduce_38

action_138 (9) = happyShift action_45
action_138 (12) = happyShift action_104
action_138 (26) = happyShift action_50
action_138 (53) = happyShift action_51
action_138 (6) = happyGoto action_150
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (9) = happyShift action_45
action_139 (12) = happyShift action_104
action_139 (26) = happyShift action_50
action_139 (53) = happyShift action_51
action_139 (6) = happyGoto action_149
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (12) = happyShift action_16
action_140 (33) = happyShift action_17
action_140 (34) = happyShift action_18
action_140 (35) = happyShift action_19
action_140 (36) = happyShift action_20
action_140 (37) = happyShift action_21
action_140 (38) = happyShift action_22
action_140 (39) = happyShift action_23
action_140 (40) = happyShift action_24
action_140 (41) = happyShift action_25
action_140 (42) = happyShift action_26
action_140 (43) = happyShift action_27
action_140 (44) = happyShift action_28
action_140 (45) = happyShift action_29
action_140 (48) = happyShift action_30
action_140 (49) = happyShift action_31
action_140 (50) = happyShift action_32
action_140 (51) = happyShift action_33
action_140 (53) = happyShift action_34
action_140 (8) = happyGoto action_148
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (12) = happyShift action_16
action_141 (33) = happyShift action_17
action_141 (34) = happyShift action_18
action_141 (35) = happyShift action_19
action_141 (36) = happyShift action_20
action_141 (37) = happyShift action_21
action_141 (38) = happyShift action_22
action_141 (39) = happyShift action_23
action_141 (40) = happyShift action_24
action_141 (41) = happyShift action_25
action_141 (42) = happyShift action_26
action_141 (43) = happyShift action_27
action_141 (44) = happyShift action_28
action_141 (45) = happyShift action_29
action_141 (48) = happyShift action_30
action_141 (49) = happyShift action_31
action_141 (50) = happyShift action_32
action_141 (51) = happyShift action_33
action_141 (53) = happyShift action_34
action_141 (8) = happyGoto action_147
action_141 _ = happyFail (happyExpListPerState 141)

action_142 _ = happyReduce_19

action_143 (29) = happyShift action_81
action_143 _ = happyReduce_18

action_144 (29) = happyShift action_81
action_144 _ = happyReduce_17

action_145 (27) = happyShift action_79
action_145 (28) = happyShift action_80
action_145 (29) = happyShift action_81
action_145 _ = happyReduce_16

action_146 (27) = happyShift action_79
action_146 (28) = happyShift action_80
action_146 (29) = happyShift action_81
action_146 _ = happyReduce_15

action_147 (13) = happyShift action_170
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (13) = happyShift action_169
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (13) = happyShift action_168
action_149 (25) = happyShift action_77
action_149 (26) = happyShift action_78
action_149 (27) = happyShift action_79
action_149 (28) = happyShift action_80
action_149 (29) = happyShift action_81
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (13) = happyShift action_167
action_150 (25) = happyShift action_77
action_150 (26) = happyShift action_78
action_150 (27) = happyShift action_79
action_150 (28) = happyShift action_80
action_150 (29) = happyShift action_81
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (13) = happyShift action_166
action_151 (25) = happyShift action_77
action_151 (26) = happyShift action_78
action_151 (27) = happyShift action_79
action_151 (28) = happyShift action_80
action_151 (29) = happyShift action_81
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (17) = happyShift action_165
action_152 (25) = happyShift action_77
action_152 (26) = happyShift action_78
action_152 (27) = happyShift action_79
action_152 (28) = happyShift action_80
action_152 (29) = happyShift action_81
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (13) = happyShift action_164
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (13) = happyShift action_163
action_154 (25) = happyShift action_77
action_154 (26) = happyShift action_78
action_154 (27) = happyShift action_79
action_154 (28) = happyShift action_80
action_154 (29) = happyShift action_81
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (15) = happyShift action_162
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (15) = happyShift action_161
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (25) = happyShift action_77
action_157 (26) = happyShift action_78
action_157 (27) = happyShift action_79
action_157 (28) = happyShift action_80
action_157 (29) = happyShift action_81
action_157 _ = happyReduce_30

action_158 (25) = happyShift action_77
action_158 (26) = happyShift action_78
action_158 (27) = happyShift action_79
action_158 (28) = happyShift action_80
action_158 (29) = happyShift action_81
action_158 _ = happyReduce_31

action_159 (25) = happyShift action_77
action_159 (26) = happyShift action_78
action_159 (27) = happyShift action_79
action_159 (28) = happyShift action_80
action_159 (29) = happyShift action_81
action_159 _ = happyReduce_27

action_160 (25) = happyShift action_77
action_160 (26) = happyShift action_78
action_160 (27) = happyShift action_79
action_160 (28) = happyShift action_80
action_160 (29) = happyShift action_81
action_160 _ = happyReduce_29

action_161 (32) = happyShift action_172
action_161 _ = happyFail (happyExpListPerState 161)

action_162 _ = happyReduce_5

action_163 _ = happyReduce_51

action_164 _ = happyReduce_49

action_165 (9) = happyShift action_45
action_165 (12) = happyShift action_104
action_165 (26) = happyShift action_50
action_165 (53) = happyShift action_51
action_165 (6) = happyGoto action_171
action_165 _ = happyFail (happyExpListPerState 165)

action_166 _ = happyReduce_42

action_167 _ = happyReduce_37

action_168 _ = happyReduce_36

action_169 _ = happyReduce_35

action_170 _ = happyReduce_34

action_171 (17) = happyShift action_174
action_171 (25) = happyShift action_77
action_171 (26) = happyShift action_78
action_171 (27) = happyShift action_79
action_171 (28) = happyShift action_80
action_171 (29) = happyShift action_81
action_171 _ = happyFail (happyExpListPerState 171)

action_172 (14) = happyShift action_173
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (10) = happyShift action_3
action_173 (12) = happyShift action_4
action_173 (30) = happyShift action_5
action_173 (31) = happyShift action_6
action_173 (46) = happyShift action_7
action_173 (47) = happyShift action_8
action_173 (52) = happyShift action_9
action_173 (53) = happyShift action_10
action_173 (4) = happyGoto action_176
action_173 (5) = happyGoto action_12
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (9) = happyShift action_45
action_174 (12) = happyShift action_104
action_174 (26) = happyShift action_50
action_174 (53) = happyShift action_51
action_174 (6) = happyGoto action_175
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (17) = happyShift action_178
action_175 (25) = happyShift action_77
action_175 (26) = happyShift action_78
action_175 (27) = happyShift action_79
action_175 (28) = happyShift action_80
action_175 (29) = happyShift action_81
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (15) = happyShift action_177
action_176 _ = happyFail (happyExpListPerState 176)

action_177 _ = happyReduce_4

action_178 (9) = happyShift action_45
action_178 (12) = happyShift action_104
action_178 (26) = happyShift action_50
action_178 (53) = happyShift action_51
action_178 (6) = happyGoto action_179
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (13) = happyShift action_180
action_179 (25) = happyShift action_77
action_179 (26) = happyShift action_78
action_179 (27) = happyShift action_79
action_179 (28) = happyShift action_80
action_179 (29) = happyShift action_81
action_179 _ = happyFail (happyExpListPerState 179)

action_180 _ = happyReduce_48

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (MultiExpr happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 11 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExpIf happy_var_3 happy_var_6 happy_var_10
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 7 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExpWhile happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TokenTileVar _ happy_var_1))
	 =  HappyAbsSyn5
		 (ExpSetTileVar happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenTileVar _ happy_var_1))
	 =  HappyAbsSyn5
		 (ExpSetIntVar happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ExpPrint happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn5
		 (ExpDoNothing
	)

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyTerminal (TokenTileVar _ happy_var_3))
	(HappyTerminal (TokenTileVar _ happy_var_2))
	_
	 =  HappyAbsSyn5
		 (ExpGetTileFile happy_var_2 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  5 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ExpReturn happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyTerminal (TokenInt _ happy_var_1))
	 =  HappyAbsSyn6
		 (IntVal happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyTerminal (TokenTileVar _ happy_var_1))
	 =  HappyAbsSyn6
		 (IntVar happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (IntNegate happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (IntPlus happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (IntMinus happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (IntTimes happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (IntDivide happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (IntExponential happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  7 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn7
		 (BoolTrue
	)

happyReduce_22 = happySpecReduce_1  7 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn7
		 (BoolFalse
	)

happyReduce_23 = happySpecReduce_3  7 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BoolAnd happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  7 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BoolOr happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  7 happyReduction_25
happyReduction_25 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (BoolNot happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  7 happyReduction_26
happyReduction_26 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (BoolLessThan happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 7 happyReduction_27
happyReduction_27 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BoolLessEqualThan happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  7 happyReduction_28
happyReduction_28 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (BoolMoreThan happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 7 happyReduction_29
happyReduction_29 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BoolMoreEqualThan happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 7 happyReduction_30
happyReduction_30 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BoolEqual happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 7 happyReduction_31
happyReduction_31 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BoolNotEqual happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  7 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  8 happyReduction_33
happyReduction_33 (HappyTerminal (TokenTileVar _ happy_var_1))
	 =  HappyAbsSyn8
		 (TileVar happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happyReduce 6 8 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileCTR happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 6 8 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileCTD happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 6 8 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileDTR happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 6 8 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileDTD happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 4 8 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileRT90 happy_var_3
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 8 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileRT180 happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 8 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileRT270 happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 4 8 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileSRT happy_var_3
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 6 8 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileST happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 4 8 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileCBT happy_var_3
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 4 8 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileRTX happy_var_3
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 4 8 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileRTY happy_var_3
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 4 8 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileRTXY happy_var_3
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_3  8 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 12 8 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileSub happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 6 8 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileConjunct happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 8 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileNegate happy_var_3
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 6 8 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TileRemoveTop happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 54 54 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt _ happy_dollar_dollar -> cont 9;
	TokenDoNothing _ -> cont 10;
	TokenEq _ -> cont 11;
	TokenLParen _ -> cont 12;
	TokenRParen _ -> cont 13;
	TokenLSquig _ -> cont 14;
	TokenRSquig _ -> cont 15;
	TokenSemiColon _ -> cont 16;
	TokenComma _ -> cont 17;
	TokenTrue _ -> cont 18;
	TokenFalse _ -> cont 19;
	TokenAnd _ -> cont 20;
	TokenOr _ -> cont 21;
	TokenNot _ -> cont 22;
	TokenLessThan _ -> cont 23;
	TokenMoreThan _ -> cont 24;
	TokenPlus _ -> cont 25;
	TokenMinus _ -> cont 26;
	TokenTimes _ -> cont 27;
	TokenDiv _ -> cont 28;
	TokenExponential _ -> cont 29;
	TokenIf _ -> cont 30;
	TokenWhile _ -> cont 31;
	TokenElse _ -> cont 32;
	TokenCTR _ -> cont 33;
	TokenCTD _ -> cont 34;
	TokenDTR _ -> cont 35;
	TokenDTD _ -> cont 36;
	TokenRT90 _ -> cont 37;
	TokenRT180 _ -> cont 38;
	TokenRT270 _ -> cont 39;
	TokenSR _ -> cont 40;
	TokenST _ -> cont 41;
	TokenBlank _ -> cont 42;
	TokenRTX _ -> cont 43;
	TokenRTY _ -> cont 44;
	TokenRTXY _ -> cont 45;
	TokenPrint _ -> cont 46;
	TokenTileFile _ -> cont 47;
	TokenSubTile _ -> cont 48;
	TokenConjunct _ -> cont 49;
	TokenNegate _ -> cont 50;
	TokenRemoveTop _ -> cont 51;
	TokenReturnTile _ -> cont 52;
	TokenTileVar _ happy_dollar_dollar -> cont 53;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 54 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Exp = ExpSetTileVar String ExpTile
         | MultiExpr Exp Exp
         | ExpPrint ExpTile
         | ExpIf ExpBool Exp Exp
         | ExpWhile ExpBool Exp
         | ExpDoNothing
         | ExpGetTileFile String String
         | ExpSetIntVar String ExpInt
         | ExpReturn ExpTile
    deriving (Show,Eq)

data ExpBool = BoolTrue
             | BoolFalse
             | BoolAnd ExpBool ExpBool
             | BoolOr ExpBool ExpBool
             | BoolNot ExpBool
             | BoolLessThan ExpInt ExpInt
             | BoolLessEqualThan ExpInt ExpInt
             | BoolMoreThan ExpInt ExpInt
             | BoolMoreEqualThan ExpInt ExpInt
             | BoolEqual ExpInt ExpInt
             | BoolNotEqual ExpInt ExpInt
    deriving (Show,Eq)

data ExpInt = IntVal Int
            | IntVar String
            | IntNegate ExpInt
            | IntPlus ExpInt ExpInt
            | IntMinus ExpInt ExpInt
            | IntTimes ExpInt ExpInt
            | IntDivide ExpInt ExpInt
            | IntExponential ExpInt ExpInt
    deriving (Show,Eq)

data ExpTile = TileVar String
             | TileCTR ExpTile ExpTile
             | TileCTD ExpTile ExpTile 
             | TileDTR ExpTile ExpInt 
             | TileDTD ExpTile ExpInt
             | TileRT90 ExpTile
             | TileRT180 ExpTile
             | TileRT270 ExpTile
             | TileSRT ExpTile
             | TileST ExpTile ExpInt
             | TileCBT ExpTile
             | TileRTX ExpTile
             | TileRTY ExpTile
             | TileRTXY ExpTile
             | TileSub ExpTile ExpInt ExpInt ExpInt ExpInt
             | TileConjunct ExpTile ExpTile
             | TileNegate ExpTile
             | TileRemoveTop ExpTile ExpInt
    deriving (Show,Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
