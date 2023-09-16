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
-module(interrupt).
-export([start/0]).

-include_lib("atomvm_ulp/include/ulp.hrl").

-define(GPIO_BUTTON, 0).
-define(RTC_GPIO_BUTTON, 11).

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
            {ULPBinary, _Labels} = ulp:compile([
                % Read button
                ?I_RD_RTC_GPIO(?RTC_GPIO_BUTTON),
                % pin is pulled-up, halt without interrupt/wake if it's not down
                ?I_BGE(2, 1),
                % interrupt
                ?I_WAKE,
                ?I_HALT
            ]),
            ulp:load_binary(ULPBinary),
            % Verify the GPIO number
            ?RTC_GPIO_BUTTON = rtc_gpio:gpio_to_rtc_gpio(?GPIO_BUTTON),
            ok;
        true ->
            ok
    end,
    % Configure pin as we're doing an awaken wait now.
    io:format("Configure pin\n"),
    rtc_gpio:init(?GPIO_BUTTON),
    rtc_gpio:set_direction(?GPIO_BUTTON, input_only),
    rtc_gpio:pulldown_dis(?GPIO_BUTTON),
    rtc_gpio:pullup_en(?GPIO_BUTTON),
    % Also register for interrupts
    {ok, {HandlerResource, HandlerRef}} = ulp:isr_register(),
    if
        InstallULP ->
            io:format("Configure ULP timer\n"),
            ulp:set_wakeup_period(0, 200000),
            io:format("Start ULP program\n"),
            ulp:run();
        true ->
            ok
    end,
    io:format("Waiting for interrupt\n"),
    receive
        {ulp, HandlerRef} -> ok
    end,
    io:format("Got an interrupt\n"),
    true = ulp:isr_deregister(HandlerResource),
    esp:sleep_ulp_wakeup(),
    io:format("Deep sleep (next interrupt should wake us up)\n"),
    esp:deep_sleep().
