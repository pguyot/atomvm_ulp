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
-module(sht20).
-export([start/0, test/0]).

-include_lib("atomvm_ulp/include/ulp.hrl").

-define(GPIO_SHT20_I2C_SCL, 26).
-define(GPIO_SHT20_I2C_SDA, 0).
-define(RTC_GPIO_SHT20_I2C_SCL, 7).
-define(RTC_GPIO_SHT20_I2C_SDA, 11).

start() ->
    % Determine if we were woken by ulp.
    InstallULP =
        case esp:reset_reason() of
            esp_rst_deepsleep ->
                case esp:sleep_get_wakeup_cause() of
                    sleep_wakeup_ulp ->
                        io:format("Woken up by ULP\n"),
                        false;
                    OtherSleepWakeup ->
                        io:format("Unexpected sleep wakeup ~s\n", [OtherSleepWakeup]),
                        true
                end;
            OtherResetReason ->
                io:format("Reset reason is not deep sleep but ~s\n", [OtherResetReason]),
                true
        end,
    if
        InstallULP ->
            io:format("Installing ULP program\n"),
            setup(),
            ok;
        true ->
            read_temperatures(),
            ok
    end,

    io:format("Going to deep sleep\n"),
    esp:sleep_ulp_wakeup(),
    esp:deep_sleep().

setup() ->
    {ULPBinary, Labels} = ulp:compile(
        [
            {label, read_counter},
            % counter
            <<0:32>>,
            {label, read_values}
        ] ++ lists:duplicate(32, <<0:32>>) ++
            [
                {label, start},

                % Let SDA be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 0),
                % Wait for SDA to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SDA),
                ?I_BL(-1, 1),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),

                {movi, 2, next0},
                {bxi, i2c_start},
                {label, next0},

                % Write address

                % byte to write
                ?I_MOVI(1, 2#10000000),

                {movi, 2, next1},
                {bxi, write_byte},
                {label, next1},

                % Abort if NACK
                ?I_JUMPR_LT(3, 1),
                ?I_MOVI(1, 16#FFFE),
                {bxi, error_x},

                % Write command
                ?I_MOVI(1, 2#11100011),

                {movi, 2, next2},
                {bxi, write_byte},
                {label, next2},

                % Abort if NACK
                ?I_JUMPR_LT(3, 1),
                ?I_MOVI(1, 16#FFFD),
                {bxi, error_x},

                {movi, 2, next3},
                {bxi, i2c_stop},
                {label, next3},

                {movi, 2, next4},
                {bxi, i2c_start},
                {label, next4},

                % Write address
                ?I_MOVI(1, 2#10000001),

                {movi, 2, next5},
                {bxi, write_byte},
                {label, next5},

                ?I_MOVI(1, 0),
                % ACK
                ?I_MOVI(3, 0),

                % Read byte
                {movi, 2, next6},
                {bxi, read_byte},
                {label, next6},

                % Read byte
                {movi, 2, next7},
                {bxi, read_byte},
                {label, next7},

                {movi, 3, read_counter},
                ?I_LD(2, 3, 0),
                {movi, 3, read_values},
                ?I_ADDR(3, 2, 3),
                ?I_ADDR(3, 2, 3),
                ?I_ST(1, 3, 0),

                ?I_MOVI(1, 0),
                % NACK
                ?I_MOVI(3, 1),

                % Read byte
                {movi, 2, next8},
                {bxi, read_byte},
                {label, next8},

                {movi, 3, read_counter},
                ?I_LD(2, 3, 0),
                {movi, 3, read_values},
                ?I_ADDR(3, 2, 3),
                ?I_ADDR(3, 2, 3),
                ?I_ST(1, 3, 1),
                ?I_ADDI(2, 2, 1),
                % Use a circular buffer to avoid overwriting code
                ?I_ANDI(2, 2, 15),
                {movi, 3, read_counter},
                ?I_ST(2, 3, 0),
                ?I_MOVR(3, 2),

                {movi, 2, next9},
                {bxi, i2c_stop},
                {label, next9},

                ?I_MOVR(0, 3),
                % Wake up every 16 measures
                ?I_JUMPR_GE(2, 1),
                ?I_WAKE,
                ?I_HALT,

                % I2C Start
                % Returns to address set by R2.
                % On exit, SCL and SDA are driven low
                {label, i2c_start},
                % Drive SDA low
                ?I_WR_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SDA, 0),
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 1),
                % Drive SCL low
                ?I_WR_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL, 0),
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 1),
                % Return
                ?I_BXR(2),

                % I2C Stop
                % Returns to address set by R2.
                {label, i2c_stop},
                % Drive SDA low
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 1),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),
                % Let SDA be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 0),
                % Ensure SDA is high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SDA),
                {jumpr_lt, arbitration_lost, 1},
                % Return
                {label, i2c_stop_exit},
                ?I_BXR(2),

                % Write a byte
                % Returns to address set by R2.
                % Byte to write is in R1
                % Stage counter and R0 are modified
                % On exit, SCL is high
                {label, write_byte},
                ?I_STAGE_RST,

                {label, write_byte_loop},
                ?I_ANDI(0, 1, 16#80),
                ?I_STAGE_INC(1),
                ?I_LSHI(1, 1, 1),

                {jumpr_ge, write_bit_high, 1},
                % Drive SDA low
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 1),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),
                {bxi, write_byte_loop_continue},

                {label, write_bit_high},
                % Let SDA be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 0),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),
                % Ensure SDA is high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SDA),
                {jumpr_lt, arbitration_lost, 1},

                {label, write_byte_loop_continue},
                % Drive SCL low
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 1),
                {jumps_lt, write_byte_loop, 8},

                % Let SDA be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 0),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),
                % Read ACK/NACK
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SDA),
                % Drive SCL low
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 1),
                % Return
                ?I_BXR(2),

                % Read a byte
                % R3 determines if it's ACK or NACK
                % Byte is put into R1 which is shifted by 8 bits
                % R0 is used
                % Return address is R2.
                {label, read_byte},
                ?I_STAGE_RST,
                {label, read_byte_loop},
                % Let SDA be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 0),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),
                % Read SDA
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SDA),
                ?I_LSHI(1, 1, 1),
                ?I_ORR(1, 1, 0),
                % Drive SCL low
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 1),
                ?I_STAGE_INC(1),
                {jumps_lt, read_byte_loop, 8},

                % Write ACK or NACK (R3)
                ?I_MOVR(0, 3),
                {jumpr_ge, read_byte_nack, 1},
                % Write ACK
                % Drive SDA low
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 1),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),
                {bxi, read_byte_exit},

                {label, read_byte_nack},
                % Let SDA be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 0),
                % Let SCL be driven by pull up
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                % Wait for SCL to be high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SCL),
                ?I_BL(-1, 1),
                % Ensure SDA is high
                ?I_RD_RTC_GPIO(?RTC_GPIO_SHT20_I2C_SDA),
                {jumpr_ge, read_byte_exit, 1},
                {bxi, arbitration_lost},

                {label, read_byte_exit},
                % Drive SCL low
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 1),
                % Return
                ?I_BXR(2),

                {label, arbitration_lost},
                ?I_MOVI(1, 16#FFFF),
                {label, error_x},
                {movi, 3, read_counter},
                ?I_LD(2, 3, 0),
                {movi, 3, read_values},
                ?I_ADDR(3, 2, 3),
                ?I_ADDR(3, 2, 3),
                ?I_ST(1, 3, 0),
                ?I_ADDI(2, 2, 1),
                {movi, 3, read_counter},
                ?I_ST(2, 3, 0),
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SDA, 0),
                ?I_WR_RTC_GPIO_ENABLE(?RTC_GPIO_SHT20_I2C_SCL, 0),
                ?I_WAKE,
                ?I_HALT
            ]
    ),
    io:format("Program size = ~B\n", [byte_size(ULPBinary)]),
    ulp:load_binary(ULPBinary),
    ?RTC_GPIO_SHT20_I2C_SCL = rtc_gpio:gpio_to_rtc_gpio(?GPIO_SHT20_I2C_SCL),
    ?RTC_GPIO_SHT20_I2C_SDA = rtc_gpio:gpio_to_rtc_gpio(?GPIO_SHT20_I2C_SDA),
    % Measure every 5 secs
    ulp:set_wakeup_period(0, 5000000),
    % configure pins
    rtc_gpio:init(?GPIO_SHT20_I2C_SCL),
    rtc_gpio:set_direction(?GPIO_SHT20_I2C_SCL, input_output_od),
    rtc_gpio:pulldown_dis(?GPIO_SHT20_I2C_SCL),
    rtc_gpio:pullup_en(?GPIO_SHT20_I2C_SCL),
    rtc_gpio:init(?GPIO_SHT20_I2C_SDA),
    rtc_gpio:set_direction(?GPIO_SHT20_I2C_SDA, input_output_od),
    rtc_gpio:pulldown_dis(?GPIO_SHT20_I2C_SDA),
    rtc_gpio:pullup_en(?GPIO_SHT20_I2C_SDA),
    ulp:run(maps:get(start, Labels)).

read_temperatures() ->
    Mem0 = ulp:read_memory(0),
    Cursor = Mem0 band 16#FFFF,
    io:format("Cursor = ~B\n", [Cursor]),
    read_temperature0(0, 16).

read_temperature0(Ix, Ix) ->
    ok;
read_temperature0(Ix, Count) ->
    Mem0 = ulp:read_memory((2 * Ix) + 1),
    Mem1 = ulp:read_memory((2 * Ix) + 2),
    Val0 = Mem0 band 16#FFFF,
    Val1 = Mem1 band 16#FFFF,
    % Verify checksum
    case crc8(Val0, 2#100110001) of
        0 when Mem0 =:= 0 andalso Mem1 =:= 0 ->
            io:format("~B: (unset)\n", [Ix]);
        Val1 ->
            % Apply formula
            TemperatureC = Val0 * 175.72 / (1 bsl 16) - 46.85,
            io:format("~B: ~f\n", [Ix, TemperatureC]);
        _ ->
            io:format("~B: error (~8.16.0B-~8.16.0B)\n", [Ix, Mem0, Mem1])
    end,
    read_temperature0(Ix + 1, Count).

crc8(Val, Poly) ->
    ValBits = count_bits(Val, 0),
    PolyBits = count_bits(Poly, 0),
    crc8_0(Val bsl 8, ValBits + 8, Poly, PolyBits).

count_bits(0, N) -> N;
count_bits(X, N) -> count_bits(X bsr 1, N + 1).

crc8_0(Val, _ValBits, Poly, _PolyBits) when Val < Poly -> Val;
crc8_0(Val, ValBits, Poly, PolyBits) ->
    NewVal = Val bxor (Poly bsl (ValBits - PolyBits)),
    NewValBits = count_bits(NewVal, 0),
    crc8_0(NewVal, NewValBits, Poly, PolyBits).

test() ->
    16#1C = crc8(16#6C94, 305),
    16#93 = crc8(16#737C, 305),
    ok.
