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
-module(blink).
-export([start/0]).

-include_lib("atomvm_ulp/include/ulp.hrl").

-define(GPIO_LED, 0).
-define(RTC_GPIO_LED, 11).

start() ->
    ULPBinary = ulp:compile([
        % @loop
        % Turn led on
        ?I_WR_RTC_GPIO(?RTC_GPIO_LED, 1),
        % Delay for about 1 second
        ?I_MOVI(0, 1000),
        % 8000 cycles - 6 for this instruction
        ?I_DELAY(7984),
        % - 6 cycles
        ?I_SUBI(0, 0, 1),
        % - 4 cycles
        ?I_BGE(-2, 1),
        % Turn led off
        ?I_WR_RTC_GPIO(?RTC_GPIO_LED, 0),
        ?I_MOVI(0, 1000),
        ?I_DELAY(7984),
        ?I_SUBI(0, 0, 1),
        ?I_BGE(-2, 1),
        % always jump to @loop
        ?I_BGE(-10, 0)
    ]),
    ulp:load_binary(ULPBinary),
    % Verify the GPIO number
    ?RTC_GPIO_LED = rtc_gpio:gpio_to_rtc_gpio(?GPIO_LED),
    io:format("Configure pin\n"),
    rtc_gpio:init(?GPIO_LED),
    rtc_gpio:set_direction(?GPIO_LED, output_only),
    rtc_gpio:set_level(?GPIO_LED, low),
    io:format("Start ULP program\n"),
    ulp:run(),
    io:format("Enter deep sleep\n"),
    esp:deep_sleep().
