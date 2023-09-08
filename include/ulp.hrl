%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%
-define(OPCODE_WR_REG, 1).

-define(OPCODE_RD_REG, 2).

-define(RD_REG_PERIPH_RTC_CNTL, 0).
-define(RD_REG_PERIPH_RTC_IO, 1).
-define(RD_REG_PERIPH_SENS, 2).
-define(RD_REG_PERIPH_RTC_I2C, 3).

-define(OPCODE_I2C, 3).

-define(OPCODE_DELAY, 4).

-define(OPCODE_ADC, 5).

-define(OPCODE_ST, 6).
-define(SUB_OPCODE_ST, 4).

-define(OPCODE_ALU, 7).
-define(SUB_OPCODE_ALU_REG, 0).
-define(SUB_OPCODE_ALU_IMM, 1).
-define(SUB_OPCODE_ALU_CNT, 2).
-define(ALU_SEL_ADD, 0).
-define(ALU_SEL_SUB, 1).
-define(ALU_SEL_AND, 2).
-define(ALU_SEL_OR, 3).
-define(ALU_SEL_MOV, 4).
-define(ALU_SEL_LSH, 5).
-define(ALU_SEL_RSH, 6).
-define(ALU_SEL_SINC, 0).
-define(ALU_SEL_SDEC, 1).
-define(ALU_SEL_SRST, 2).

-define(OPCODE_BRANCH, 8).
-define(SUB_OPCODE_BX, 0).
-define(SUB_OPCODE_BR, 1).
-define(SUB_OPCODE_BS, 2).
-define(BX_JUMP_TYPE_DIRECT, 0).
-define(BX_JUMP_TYPE_ZERO, 1).
-define(BX_JUMP_TYPE_OVF, 2).
-define(SUB_OPCODE_B, 1).
-define(B_CMP_L, 0).
-define(B_CMP_GE, 1).
-define(JUMPS_LT, 0).
-define(JUMPS_GE, 1).
-define(JUMPS_LE, 2).

-define(OPCODE_END, 9).
-define(SUB_OPCODE_END, 0).
-define(SUB_OPCODE_SLEEP, 1).

-define(OPCODE_TSENS, 10).

-define(OPCODE_HALT, 11).

-define(OPCODE_LD, 13).

% Register addresses are in words
-define(RTC_GPIO_OUT_REG, 0).
-define(RTC_GPIO_OUT_W1TS_REG, 1).
-define(RTC_GPIO_OUT_W1TC_REG, 2).
-define(RTC_GPIO_OUT_SHIFT, 14).
-define(RTC_GPIO_ENABLE_REG, 3).
-define(RTC_GPIO_ENABLE_SHIFT, 14).
-define(RTC_GPIO_IN_REG, 9).
-define(RTC_GPIO_IN_REG_SHIFT, 14).

-define(SENS_SAR_START_FORCE_REG, 11).
-define(SENS_PC_INIT_LOW, 11).
-define(SENS_PC_INIT_HIGH, 21).

-define(RTC_CNTL_STATE0_REG, 6).
-define(RTC_CNTL_ULP_CP_SLP_TIMER_EN, 24).

% ULP instructions are stored in little endian in RTC Memory
% We're using the same name of macros in the legacy macro interface here.
% Please note, however, that there are few differences, including with the assembler.

% 30.4.10 WAIT – Wait for a Number of Cycles
-define(I_DELAY(Cycles),
    <<Cycles:16/little, 0:8, ?OPCODE_DELAY:4, 0:4>>
).

% 30.4.7 HALT – End the Program
-define(I_HALT,
    <<0:24, ?OPCODE_HALT:4, 0:4>>
).

% 30.4.13 REG_RD – Read from Peripheral Register
-define(I_RD_REG(RegAddr, Peripheral, LowBit, HighBit),
    <<RegAddr, 0:6, Peripheral:2, (HighBit band 1):1, LowBit:5, 0:2, ?OPCODE_RD_REG:4,
        (HighBit bsr 1):4>>
).

-define(I_RD_REG_RTC_CNTL(RegAddr, LowBit, HighBit),
    ?I_RD_REG(RegAddr, ?RD_REG_PERIPH_RTC_CNTL, LowBit, HighBit)
).
-define(I_RD_REG_RTC_IO(RegAddr, LowBit, HighBit),
    ?I_RD_REG(RegAddr, ?RD_REG_PERIPH_RTC_IO, LowBit, HighBit)
).
-define(I_RD_REG_SENS(RegAddr, LowBit, HighBit),
    ?I_RD_REG(RegAddr, ?RD_REG_PERIPH_SENS, LowBit, HighBit)
).
-define(I_RD_REG_RTC_I2C(RegAddr, LowBit, HighBit),
    ?I_RD_REG(RegAddr, ?RD_REG_PERIPH_RTC_I2C, LowBit, HighBit)
).

-define(I_RD_RTC_GPIO(RTC_GPIO),
    ?I_RD_REG_RTC_IO(
        ?RTC_GPIO_IN_REG, (RTC_GPIO + ?RTC_GPIO_IN_REG_SHIFT), (RTC_GPIO + ?RTC_GPIO_IN_REG_SHIFT)
    )
).

% 30.4.14 REG_WR – Write to Peripheral Register
-define(I_WR_REG(RegAddr, Peripheral, LowBit, HighBit, Val),
    <<RegAddr, (Val band 2#111111):6, Peripheral:2, (HighBit band 1):1, LowBit:5, (Val bsr 6):2,
        ?OPCODE_WR_REG:4, (HighBit bsr 1):4>>
).

-define(I_WR_REG_RTC_CNTL(RegAddr, LowBit, HighBit, Val),
    ?I_WR_REG(RegAddr, ?RD_REG_PERIPH_RTC_CNTL, LowBit, HighBit, Val)
).
-define(I_WR_REG_RTC_IO(RegAddr, LowBit, HighBit, Val),
    ?I_WR_REG(RegAddr, ?RD_REG_PERIPH_RTC_IO, LowBit, HighBit, Val)
).
-define(I_WR_REG_SENS(RegAddr, LowBit, HighBit, Val),
    ?I_WR_REG(RegAddr, ?RD_REG_PERIPH_SENS, LowBit, HighBit, Val)
).
-define(I_WR_REG_RTC_I2C(RegAddr, LowBit, HighBit, Val),
    ?I_WR_REG(RegAddr, ?RD_REG_PERIPH_RTC_I2C, LowBit, HighBit, Val)
).

-define(I_WR_RTC_GPIO(RTC_GPIO, Level),
    ?I_WR_REG_RTC_IO(
        ?RTC_GPIO_OUT_REG, (RTC_GPIO + ?RTC_GPIO_OUT_SHIFT), (RTC_GPIO + ?RTC_GPIO_OUT_SHIFT), Level
    )
).
-define(I_WR_SENS_PC_INIT_LOW(PC),
    ?I_WR_REG_SENS(
        ?SENS_SAR_START_FORCE_REG, ?SENS_PC_INIT_LOW, ?SENS_PC_INIT_LOW + 7, (PC band 16#FF)
    )
).
-define(I_WR_SENS_PC_INIT_HIGH(PC),
    ?I_WR_REG_SENS(?SENS_SAR_START_FORCE_REG, ?SENS_PC_INIT_LOW + 8, ?SENS_PC_INIT_HIGH, (PC bsr 8))
).
-define(I_WR_RTC_CNTL_ULP_CP_SLP_TIMER_EN(Enabled),
    ?I_WR_REG_RTC_CNTL(
        ?RTC_CNTL_STATE0_REG, ?RTC_CNTL_ULP_CP_SLP_TIMER_EN, ?RTC_CNTL_ULP_CP_SLP_TIMER_EN, Enabled
    )
).

% 30.4.8 WAKE – Wake up the Chip
-define(I_WAKE, <<1, 0, 0, ?OPCODE_END:4, ?SUB_OPCODE_END:3, 0:1>>).

% 30.4.2 ST – Store Data in Memory
% Offset is in words (not bytes as in the assembler)
-define(I_ST(RegVal, RegAddr, Offset),
    <<0:4, RegAddr:2, RegVal:2, (Offset band 2#111111):6, 0:2, 0:3, (Offset bsr 6):5, ?OPCODE_ST:4,
        ?SUB_OPCODE_ST:3, 0:1>>
).

% 30.4.3 LD – Load Data from Memory
% Offset is in words (not bytes as in the assembler)
-define(I_LD(RegDest, RegAddr, Offset),
    <<0:4, RegAddr:2, RegDest:2, (Offset band 2#111111):6, 0:2, 0:3, (Offset bsr 6):5, ?OPCODE_LD:4,
        0:4>>
).

% 30.4.5 JUMPR – Jump to a Relative Offset (Conditional upon R0)
% PC offset is expressed in 32 bits words (following legacy macros but unlike
% assembler which parses it as number of bytes)
-define(I_JUMPR(PCOffset, Imm, CompType),
    <<Imm:16/little, (abs(PCOffset)):7, CompType:1, ?OPCODE_BRANCH:4, ?SUB_OPCODE_BR:3,
        ((PCOffset bsr 7) band 1):1>>
).
-define(I_JUMPR_LT(PCOffset, Imm), ?I_JUMPR(PCOffset, Imm, ?B_CMP_L)).
-define(I_JUMPR_GE(PCOffset, Imm), ?I_JUMPR(PCOffset, Imm, ?B_CMP_GE)).

-define(I_BL(PCOffset, Imm), ?I_JUMPR_LT(PCOffset, Imm)).
-define(I_BGE(PCOffset, Imm), ?I_JUMPR_GE(PCOffset, Imm)).

% 30.4.1.1 Operations Among Registers
-define(I_ALU_REG(Sel, RegDest, RegSrc1, RegSrc2), <<
    0:2,
    RegSrc2:2,
    RegSrc1:2,
    RegDest:2,
    0:8,
    (Sel band 2#111):3,
    0:1,
    0:4,
    ?OPCODE_ALU:4,
    ?SUB_OPCODE_ALU_REG:3,
    (Sel bsr 3):1
>>).

-define(I_ADDR(RegDest, RegSrc1, RegSrc2),
    ?I_ALU_REG(?ALU_SEL_ADD, RegDest, RegSrc1, RegSrc2)
).
-define(I_SUBR(RegDest, RegSrc1, RegSrc2),
    ?I_ALU_REG(?ALU_SEL_SUB, RegDest, RegSrc1, RegSrc2)
).
-define(I_ANDR(RegDest, RegSrc1, RegSrc2),
    ?I_ALU_REG(?ALU_SEL_AND, RegDest, RegSrc1, RegSrc2)
).
-define(I_ORR(RegDest, RegSrc1, RegSrc2),
    ?I_ALU_REG(?ALU_SEL_OR, RegDest, RegSrc1, RegSrc2)
).
% Replicate bug or intended behavior of gas
% https://github.com/espressif/binutils-esp32ulp/issues/26
-define(I_MOVR(RegDest, RegSrc),
    ?I_ALU_REG(?ALU_SEL_MOV, RegDest, RegSrc, RegSrc)
).
-define(I_LSHR(RegDest, RegSrc1, RegSrc2),
    ?I_ALU_REG(?ALU_SEL_LSH, RegDest, RegSrc1, RegSrc2)
).
-define(I_RSHR(RegDest, RegSrc1, RegSrc2),
    ?I_ALU_REG(?ALU_SEL_RSH, RegDest, RegSrc1, RegSrc2)
).

% 30.4.1.2 Operations with Immediate Value
-define(I_ALU_IMM(Sel, RegDest, RegSrc1, Imm), <<
    (Imm band 2#1111):4,
    RegSrc1:2,
    RegDest:2,
    ((Imm bsr 4) band 16#FF),
    (Sel band 2#111):3,
    0:1,
    (Imm bsr 12):4,
    ?OPCODE_ALU:4,
    ?SUB_OPCODE_ALU_IMM:3,
    (Sel bsr 3):1
>>).

-define(I_ADDI(RegDest, RegSrc, Imm),
    ?I_ALU_IMM(?ALU_SEL_ADD, RegDest, RegSrc, Imm)
).
-define(I_SUBI(RegDest, RegSrc, Imm),
    ?I_ALU_IMM(?ALU_SEL_SUB, RegDest, RegSrc, Imm)
).
-define(I_ANDI(RegDest, RegSrc, Imm),
    ?I_ALU_IMM(?ALU_SEL_AND, RegDest, RegSrc, Imm)
).
-define(I_ORI(RegDest, RegSrc, Imm),
    ?I_ALU_IMM(?ALU_SEL_OR, RegDest, RegSrc, Imm)
).
-define(I_MOVI(RegDest, Imm),
    ?I_ALU_IMM(?ALU_SEL_MOV, RegDest, 0, Imm)
).
-define(I_LSHI(RegDest, RegSrc, Imm),
    ?I_ALU_IMM(?ALU_SEL_LSH, RegDest, RegSrc, Imm)
).
-define(I_RSHI(RegDest, RegSrc, Imm),
    ?I_ALU_IMM(?ALU_SEL_RSH, RegDest, RegSrc, Imm)
).

% 30.4.1.3 Operations with Stage Count Register
-define(I_STAGE_RST,
    <<0, 0, (?ALU_SEL_SRST band 2#111):3, 0:1, 0:4, ?OPCODE_ALU:4, ?SUB_OPCODE_ALU_CNT:3,
        (?ALU_SEL_SRST bsr 3):1>>
).

-define(I_STAGE_INC(Imm), <<
    (Imm band 2#1111):4,
    0:4,
    0:5,
    ((Imm bsr 4) band 2#111):3,
    (?ALU_SEL_SINC band 2#111):3,
    0:1,
    0:4,
    ?OPCODE_ALU:4,
    ?SUB_OPCODE_ALU_CNT:3,
    (?ALU_SEL_SINC bsr 3):1
>>).

-define(I_STAGE_DEC(Imm), <<
    (Imm band 2#1111):4,
    0:4,
    0:5,
    ((Imm bsr 4) band 2#111):3,
    (?ALU_SEL_SDEC band 2#111):3,
    0:1,
    0:4,
    ?OPCODE_ALU:4,
    ?SUB_OPCODE_ALU_CNT:3,
    (?ALU_SEL_SDEC bsr 3):1
>>).

% 30.4.6 JUMPS – Jump to a Relative Address (Conditional upon Stage Count Regis­ter)
-define(I_JUMPS(PCOffset, Imm, CompType),
    <<Imm, (CompType band 1):1, 0:7, (abs(PCOffset)):7, (CompType bsr 1):1, ?OPCODE_BRANCH:4,
        ?SUB_OPCODE_BS:3, ((PCOffset bsr 7) band 1):1>>
).
-define(I_JUMPS_LT(PCOffset, Imm),
    ?I_JUMPS(PCOffset, Imm, ?JUMPS_LT)
).
-define(I_JUMPS_GE(PCOffset, Imm),
    ?I_JUMPS(PCOffset, Imm, ?JUMPS_GE)
).
-define(I_JUMPS_LE(PCOffset, Imm),
    ?I_JUMPS(PCOffset, Imm, ?JUMPS_LE)
).
-define(I_JUMPS_GT(PCOffset, Imm),
    ?I_JUMPS(PCOffset, (Imm + 1), ?JUMPS_GE)
).
