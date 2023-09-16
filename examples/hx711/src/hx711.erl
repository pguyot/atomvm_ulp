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
-module(hx711).
-export([start/0]).

-include_lib("atomvm_ulp/include/ulp.hrl").

-define(GPIO_HX711_CLK, 32).
-define(GPIO_HX711_DAT, 33).
-define(RTC_GPIO_HX711_CLK, 9).
-define(RTC_GPIO_HX711_DAT, 8).

start() ->
    % Determine if we were woken by ulp.
    RunSetup =
        case esp:reset_reason() of
            esp_rst_deepsleep ->
                case esp:sleep_get_wakeup_cause() of
                    sleep_wakeup_ulp ->
                        io:format("Woken up by ULP, object was detected!\n"),
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
        RunSetup ->
            setup();
        true ->
            io:format("Remove the object (waiting for 5 secs)\n"),
            timer:sleep(5000),
            do_deep_sleep()
    end.

setup() ->
    io:format("Running setup\n"),
    {ULPBinary, Labels} = ulp:compile([
        % read value: high bits
        {label, read_high},
        <<0:32>>,
        % read value: low bits
        {label, read_low},
        <<0:32>>,
        % threshold value: high bits
        {label, threshold_high},
        <<0:32>>,
        % threshold value: low bits
        {label, threshold_low},
        <<0:32>>,
        {label, start},
        ?I_MOVI(2, 0),
        % wait for the hx711 to be ready
        ?I_WR_RTC_GPIO(?RTC_GPIO_HX711_CLK, 0),
        ?I_RD_RTC_GPIO(?RTC_GPIO_HX711_DAT),
        ?I_BGE(-1, 1),
        ?I_STAGE_RST,
        % Loop to read first 8 bits
        ?I_STAGE_INC(8),
        % @loop1
        ?I_MOVI(1, 0),
        % @loop2
        ?I_WR_RTC_GPIO(?RTC_GPIO_HX711_CLK, 1),
        ?I_WR_RTC_GPIO(?RTC_GPIO_HX711_CLK, 0),
        ?I_RD_RTC_GPIO(?RTC_GPIO_HX711_DAT),
        ?I_LSHI(1, 1, 1),
        ?I_ORR(1, 1, 0),
        ?I_STAGE_DEC(1),
        % Jump to loop2 until stage counter reaches 0
        ?I_JUMPS_GT(-6, 0),
        ?I_ST(1, 2, 0),
        % If r2 >= 1, we're done and jump to @exit
        ?I_MOVR(0, 2),
        ?I_JUMPR_GE(4, 1),
        ?I_ADDI(2, 2, 1),
        % Loop to read next 16 bits
        ?I_STAGE_INC(16),
        % always jump at @loop1
        ?I_JUMPS_GT(-13, 0),
        % @exit
        % Pulse once more
        ?I_WR_RTC_GPIO(?RTC_GPIO_HX711_CLK, 1),
        ?I_WR_RTC_GPIO(?RTC_GPIO_HX711_CLK, 0),
        % And power it down with clock set to high
        ?I_WR_RTC_GPIO(?RTC_GPIO_HX711_CLK, 1),
        % Determine if read value is higher than threshold
        ?I_MOVI(0, 0),
        ?I_LD(1, 0, 0),
        ?I_LD(2, 0, 2),
        ?I_SUBR(1, 1, 2),
        % threshold high > measured high, sleep
        ?I_BXFI(34),
        % threshold high = measured high, compare
        ?I_BXZI(35),
        % threshold high < measured high, wake
        ?I_WAKE,
        ?I_HALT,
        ?I_LD(1, 0, 1),
        ?I_LD(2, 0, 3),
        ?I_SUBR(1, 1, 2),
        % threshold low > measured low, sleep
        ?I_BXFI(34),
        ?I_WAKE,
        ?I_HALT
    ]),
    ulp:load_binary(ULPBinary),
    ?RTC_GPIO_HX711_CLK = rtc_gpio:gpio_to_rtc_gpio(?GPIO_HX711_CLK),
    ?RTC_GPIO_HX711_DAT = rtc_gpio:gpio_to_rtc_gpio(?GPIO_HX711_DAT),
    % configure pins
    rtc_gpio:init(?GPIO_HX711_CLK),
    rtc_gpio:set_direction(?GPIO_HX711_CLK, output_only),
    rtc_gpio:set_level(?GPIO_HX711_CLK, low),
    rtc_gpio:init(?GPIO_HX711_DAT),
    rtc_gpio:set_direction(?GPIO_HX711_DAT, input_only),
    rtc_gpio:set_level(?GPIO_HX711_DAT, low),
    io:format("Make sure nothing is on the scale\n"),
    #{
        read_high := ReadHighAddr,
        read_low := ReadLowAddr,
        threshold_high := ThresholdHighAddr,
        threshold_low := ThresholdLowAddr,
        start := Start
    } = Labels,
    Tare = measure_avg_loop(5, ReadHighAddr, ReadLowAddr),
    io:format("Tare: ~B\n", [Tare]),
    io:format("Put an object on the scale now (waiting for 5 secs)\n"),
    timer:sleep(5000),
    MeasureObj = measure_avg_loop(5, ReadHighAddr, ReadLowAddr),
    io:format("Object: ~B\n", [MeasureObj]),
    io:format("Remove the object (waiting for 5 secs)\n"),
    timer:sleep(5000),
    Threshold = (Tare + MeasureObj) div 2,
    io:format("Treshold = ~B\n", [Threshold]),
    ulp:write_memory(ThresholdHighAddr, Threshold bsr 16),
    ulp:write_memory(ThresholdLowAddr, Threshold band 16#FFFF),
    % Measure every 200ms
    ulp:set_wakeup_period(0, 200000),
    ulp:run(Start),
    do_deep_sleep().

do_deep_sleep() ->
    esp:sleep_ulp_wakeup(),
    io:format("Deep sleeping until object is put on scale using threshold\n"),
    esp:deep_sleep().

measure_avg_loop(N, ReadHighAddr, ReadLowAddr) ->
    measure_avg_loop(N, 0, 0, ReadHighAddr, ReadLowAddr).

measure_avg_loop(0, Count, Sum, _ReadHighAddr, _ReadLowAddr) ->
    round(Sum / Count);
measure_avg_loop(Remaining, Count, Sum, ReadHighAddr, ReadLowAddr) ->
    Value = measure(ReadHighAddr, ReadLowAddr),
    measure_avg_loop(Remaining - 1, Count + 1, Sum + Value).

measure(ReadHighAddr, ReadLowAddr) ->
    {ok, {Handler, Ref}} = ulp:isr_register(),
    ulp:run(4),
    receive
        {ulp, Ref} ->
            true = ulp:isr_deregister(Handler),
            Mem0 = ulp:read_memory(ReadHighAddr),
            Val0 = Mem0 band 16#FFFF,
            Mem1 = ulp:read_memory(ReadLowAddr),
            Val1 = Mem1 band 16#FFFF,
            Val = Val0 bsl 16 + Val1,
            Val
    after 1000 ->
        exit(timeout)
    end.
