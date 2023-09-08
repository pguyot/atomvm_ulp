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
-module(ulp_compile_test).

-include_lib("eunit/include/eunit.hrl").

-include("ulp.hrl").

instruction_test_() ->
    [
        ?_assertEqual(<<0, 0, 0, 176>>, ?I_HALT),
        ?_assertEqual(<<1, 0, 0, 144>>, ?I_WAKE),
        ?_assertEqual(<<4, 0, 0, 104>>, ?I_ST(0, 1, 0)),
        ?_assertEqual(<<13, 236, 31, 104>>, ?I_ST(1, 3, -5)),
        ?_assertEqual(<<4, 0, 0, 208>>, ?I_LD(0, 1, 0)),
        ?_assertEqual(<<6, 20, 0, 208>>, ?I_LD(2, 1, 5)),
        ?_assertEqual(<<225, 1, 128, 114>>, ?I_MOVI(1, 30)),
        ?_assertEqual(<<160, 2, 128, 114>>, ?I_MOVI(0, 42)),
        ?_assertEqual(<<0, 0, 64, 116>>, ?I_STAGE_RST),
        ?_assertEqual(<<160, 0, 0, 116>>, ?I_STAGE_INC(10)),
        ?_assertEqual(<<160, 0, 32, 116>>, ?I_STAGE_DEC(10)),
        ?_assertEqual(<<16, 0, 32, 116>>, ?I_STAGE_DEC(1)),
        ?_assertEqual(<<255, 255, 3, 130>>, ?I_BGE(1, -1)),
        ?_assertEqual(<<22, 0, 128, 112>>, ?I_MOVR(2, 1)),
        ?_assertEqual(<<20, 0, 9, 130>>, ?I_JUMPR_GE(4, 20)),
        ?_assertEqual(<<20, 128, 8, 132>>, ?I_JUMPS_GE(4, 20)),
        ?_assertEqual(<<57, 0, 0, 114>>, ?I_ADDI(1, 2, 3)),
        ?_assertEqual(<<57, 0, 32, 114>>, ?I_SUBI(1, 2, 3)),
        ?_assertEqual(<<57, 0, 64, 114>>, ?I_ANDI(1, 2, 3)),
        ?_assertEqual(<<57, 0, 96, 114>>, ?I_ORI(1, 2, 3)),
        ?_assertEqual(<<57, 0, 160, 114>>, ?I_LSHI(1, 2, 3)),
        ?_assertEqual(<<57, 0, 192, 114>>, ?I_RSHI(1, 2, 3)),

        ?_assertEqual(<<73, 35, 1, 114>>, ?I_ADDI(1, 2, 16#1234)),
        ?_assertEqual(<<73, 35, 33, 114>>, ?I_SUBI(1, 2, 16#1234)),
        ?_assertEqual(<<73, 35, 65, 114>>, ?I_ANDI(1, 2, 16#1234)),
        ?_assertEqual(<<73, 35, 97, 114>>, ?I_ORI(1, 2, 16#1234)),
        ?_assertEqual(<<73, 35, 161, 114>>, ?I_LSHI(1, 2, 16#1234)),
        ?_assertEqual(<<73, 35, 193, 114>>, ?I_RSHI(1, 2, 16#1234)),

        ?_assertEqual(<<201, 220, 14, 114>>, ?I_ADDI(1, 2, -16#1234)),
        ?_assertEqual(<<201, 220, 46, 114>>, ?I_SUBI(1, 2, -16#1234)),
        ?_assertEqual(<<201, 220, 78, 114>>, ?I_ANDI(1, 2, -16#1234)),
        ?_assertEqual(<<201, 220, 110, 114>>, ?I_ORI(1, 2, -16#1234)),
        ?_assertEqual(<<201, 220, 174, 114>>, ?I_LSHI(1, 2, -16#1234)),
        ?_assertEqual(<<201, 220, 206, 114>>, ?I_RSHI(1, 2, -16#1234)),

        ?_assertEqual(<<57, 0, 0, 112>>, ?I_ADDR(1, 2, 3)),
        ?_assertEqual(<<57, 0, 32, 112>>, ?I_SUBR(1, 2, 3)),
        ?_assertEqual(<<57, 0, 64, 112>>, ?I_ANDR(1, 2, 3)),
        ?_assertEqual(<<57, 0, 96, 112>>, ?I_ORR(1, 2, 3)),
        ?_assertEqual(<<57, 0, 160, 112>>, ?I_LSHR(1, 2, 3)),
        ?_assertEqual(<<57, 0, 192, 112>>, ?I_RSHR(1, 2, 3)),
        ?_assertEqual(<<0, 1, 220, 27>>, ?I_WR_RTC_GPIO(9, 0)),
        ?_assertEqual(<<9, 1, 88, 43>>, ?I_RD_RTC_GPIO(8))
    ].
