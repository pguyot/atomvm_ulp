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
-module(rtc_gpio).

-export([
    is_valid_gpio/1,
    gpio_to_rtc_gpio/1,
    init/1,
    deinit/1,
    get_level/1,
    set_level/2,
    set_direction/2,
    set_direction_in_sleep/2,
    pullup_en/1,
    pulldown_en/1,
    pullup_dis/1,
    pulldown_dis/1
]).

-type gpio_num() :: 0..48.
-type rtc_gpio_num() :: 0..21.
-type level() :: 0 | low | 1 | high.
-type direction() ::
    input_only | output_only | input_output | disabled | output_od | input_output_od.

%% @doc Determine if a GPIO Pin is also an RTC pin.
%% @param GPIOPin pin to test
%% @return true if the pin can be used by RTC
-spec is_valid_gpio(GPIOPin :: gpio_num()) -> boolean().
is_valid_gpio(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Determine RTC pin number from GPIO Pin number.
%% @param GPIOPin pin to convert
%% @return the RTC pin number that can be used by ULP programs
-spec gpio_to_rtc_gpio(GPIOPin :: gpio_num()) -> rtc_gpio_num().
gpio_to_rtc_gpio(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Initialize a GPIO pin to be used by RTC
%% @param GPIOPin pin to initialize
%% @return `ok'
-spec init(GPIOPin :: gpio_num()) -> ok.
init(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Deinitialize a GPIO pin so it is no longer to be used by RTC
%% @param GPIOPin pin to deinitialize
%% @return `ok'
-spec deinit(GPIOPin :: gpio_num()) -> ok.
deinit(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Get the level of a GPIO pin.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to get the level of
%% @return 0 if the pin is low, 1 if it's high.
-spec get_level(GPIOPin :: gpio_num()) -> 0 | 1.
get_level(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Set the level of a GPIO pin.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to set the level of
%% @param Level level to set
%% @return `ok'
-spec set_level(GPIOPin :: gpio_num(), Level :: level()) -> ok.
set_level(_GPIOPin, _Level) ->
    erlang:nif_error(undefined).

%% @doc Set the direction of a GPIO pin.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to set the direction of
%% @param Direction direction to set
%% @return `ok'
-spec set_direction(GPIOPin :: gpio_num(), Direction :: direction()) -> ok.
set_direction(_GPIOPin, _Direction) ->
    erlang:nif_error(undefined).

%% @doc Set the direction of a GPIO pin in deep sleep mode.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to set the direction of
%% @param Direction direction to set
%% @return `ok'
-spec set_direction_in_sleep(GPIOPin :: gpio_num(), Direction :: direction()) -> ok.
set_direction_in_sleep(_GPIOPin, _Direction) ->
    erlang:nif_error(undefined).

%% @doc Enable pull-up of a GPIO pin.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to enable pull-up for
%% @return `ok'
-spec pullup_en(GPIOPin :: gpio_num()) -> ok.
pullup_en(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Enable pull-down of a GPIO pin.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to enable pull-down for
%% @return `ok'
-spec pulldown_en(GPIOPin :: gpio_num()) -> ok.
pulldown_en(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Disable pull-up of a GPIO pin.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to disable pull-up for
%% @return `ok'
-spec pullup_dis(GPIOPin :: gpio_num()) -> ok.
pullup_dis(_GPIOPin) ->
    erlang:nif_error(undefined).

%% @doc Disable pull-down of a GPIO pin.
%% This function raises badarg if the pin is not also an RTC GPIO pin.
%% @param GPIOPin pin to disable pull-down for
%% @return `ok'
-spec pulldown_dis(GPIOPin :: gpio_num()) -> ok.
pulldown_dis(_GPIOPin) ->
    erlang:nif_error(undefined).
