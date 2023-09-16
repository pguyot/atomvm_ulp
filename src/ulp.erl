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
-module(ulp).

-export([
    load_binary/1,
    load_binary/2,
    run/0,
    run/1,
    read_memory/1,
    write_memory/2,
    isr_register/0,
    isr_deregister/1,
    set_wakeup_period/2,
    compile/1,
    link/3
]).

-export_type([
    offset/0,
    instruction/0,
    isr_handler/0
]).

-include_lib("atomvm_ulp/include/ulp.hrl").

-opaque isr_handler() :: binary().

%% Offset must be within CONFIG_ULP_COPROC_RESERVE_MEM which is at most 8KB
%% It is expressed in words
-type offset() :: 0..2047.

-type register() :: 0..3.

%% Instructions are 32 bits binaries (generated from macros) or labels/
%% references to labels
-type instruction() ::
    <<_:32>>
    | {label, atom()}
    | {bxi, Address :: offset() | atom()}
    | {bxzi, Address :: offset() | atom()}
    | {bxfi, Address :: offset() | atom()}
    | {movi, register(), Value :: offset() | atom()}
    | {jumps_lt, RelativeAddress :: offset() | atom(), Imm :: 0..255}
    | {jumpr_ge, RelativeAddress :: offset() | atom(), Imm :: 0..65535}
    | {jumpr_lt, RelativeAddress :: offset() | atom(), Imm :: 0..65535}.

%% Magic beginning of ULP binaries
-define(MAGIC, 16#00706c75).

%% @equiv load_binary(0, Code)
%% @param Code binary to load
%% @returns ok
-spec load_binary(Code :: binary) -> ok.
load_binary(_Code) ->
    erlang:nif_error(undefined).

%% @doc Load a binary.
%% The binary should start with the ULP header. It can be generated with the
%% ulp toolchain or using the macros from `ulp.hrl'.
%% Several binaries can be loaded using Offset to ensure they do not overlap.
%% @param LoadAddr offset to load the binary (in words)
%% @param Code binary to load
-spec load_binary(LoadAddr :: offset(), Code :: binary) -> ok.
load_binary(_LoadAddr, _Code) ->
    erlang:nif_error(undefined).

%% @equiv run(0)
%% @returns ok
-spec run() -> ok.
run() ->
    erlang:nif_error(undefined).

%% @doc Start ULP and run program at a given offset
%% @param EntryPoint offset to the entry point (in words)
%% @returns ok
-spec run(EntryPoint :: offset()) -> ok.
run(_EntryPoint) ->
    erlang:nif_error(undefined).

%% @doc Read memory at a given offset.
%% The whole 32 bits word is returned. On ESP32, ULP can only write the low
%% 16 bits, the high half encodes the PC of the store operation.
%% @param Addr offset (in words) to read from
%% @returns the value read
-spec read_memory(Addr :: offset()) -> non_neg_integer().
read_memory(_Addr) ->
    erlang:nif_error(undefined).

%% @doc Write memory at a given offset.
%% On ESP32, ULP can only read the low 16 bits. However, this can also be used
%% to write a program if needed.
%% @param Addr offset (in words) to write to
%% @param Value value to write
%% @returns ok
-spec write_memory(Addr :: offset(), Value :: non_neg_integer()) -> ok.
write_memory(_Addr, _Value) ->
    erlang:nif_error(undefined).

%% @doc Register interrupt handler.
%% After this function is called, the current process may receive messages
%% when ULP program raises an interrupt (using WAKE instruction).
%% These messages are:
%% `{ulp, Reference}'
%% where Reference is the returned reference.
%% @returns a tuple with the handler and the reference.
-spec isr_register() -> {ok, {isr_handler(), reference()}}.
isr_register() ->
    erlang:nif_error(undefined).

%% @doc Unregister interrupt handler.
%% Unregister for interruptions.
%% @param ISRHandler    handler returned by `isr_register/0'
%% @returns `true'
-spec isr_deregister(isr_handler()) -> true.
isr_deregister(_ISRHandler) ->
    erlang:nif_error(undefined).

%% @doc Set a wakeup period.
%% Four wakeup periods are available on ESP32, default is 0.
%% Duration is expressed in usec. The time between HALT and restart of the ULP
%% program is 133us longer (20 cycles of RC_SLOW).
%% @param PeriodIndex   index of the period to set
%% @param PeriodUS      duration of the period in usecs
%% @returns ok
-spec set_wakeup_period(PeriodIndex :: 0..3, PeriodUS :: non_neg_integer()) -> ok.
set_wakeup_period(_PeriodIndex, _PeriodUS) ->
    erlang:nif_error(undefined).

%% @doc Compile and link instructions to a binary suitable for `load_binary/1,2'
%% @param Instructions instructions to compile, a list of binaries generated
%%        with macros or labels/references to labels.
%% @returns A compiled binary and a map with labels.
-spec compile([instruction()]) -> binary().
compile(Instructions) ->
    Labels = parse_labels(Instructions, 0, #{}),
    TextBin = compile0(Instructions, 0, [], Labels),
    % For now we don't determine which address are accessed and should be
    % declared as data/bss
    LinkedBinary = link(TextBin, <<>>, 0),
    {LinkedBinary, Labels}.

parse_labels([], _PC, AccLabels) ->
    AccLabels;
parse_labels([{label, Label} | Tail], PC, AccLabels) ->
    NewAccLabels = AccLabels#{Label => PC},
    parse_labels(Tail, PC, NewAccLabels);
parse_labels([_Instruction | Tail], PC, AccLabels) ->
    parse_labels(Tail, PC + 1, AccLabels).

compile0([], _PC, Acc, _Labels) ->
    list_to_binary(lists:reverse(Acc));
compile0([{label, _} | Tail], PC, Acc, Labels) ->
    compile0(Tail, PC, Acc, Labels);
compile0([Instruction | Tail], PC, Acc, Labels) when is_binary(Instruction) ->
    compile0(Tail, PC + 1, [Instruction | Acc], Labels);
compile0([InstructionTuple | Tail], PC, Acc, Labels) when is_tuple(InstructionTuple) ->
    Instruction = compile_instruction(InstructionTuple, PC, Labels),
    compile0(Tail, PC + 1, [Instruction | Acc], Labels).

compile_instruction({bxi, Target}, _PC, Labels) ->
    TargetVal =
        if
            is_integer(Target) -> Target;
            is_atom(Target) -> maps:get(Target, Labels)
        end,
    ?I_BXI(TargetVal);
compile_instruction({bxzi, Target}, _PC, Labels) ->
    TargetVal =
        if
            is_integer(Target) -> Target;
            is_atom(Target) -> maps:get(Target, Labels)
        end,
    ?I_BXZI(TargetVal);
compile_instruction({bxfi, Target}, _PC, Labels) ->
    TargetVal =
        if
            is_integer(Target) -> Target;
            is_atom(Target) -> maps:get(Target, Labels)
        end,
    ?I_BXFI(TargetVal);
compile_instruction({movi, Register, Value}, _PC, Labels) ->
    ValueVal =
        if
            is_integer(Value) -> Value;
            is_atom(Value) -> maps:get(Value, Labels)
        end,
    ?I_MOVI(Register, ValueVal);
compile_instruction({jumps_lt, Relative, Imm}, PC, Labels) ->
    RelativeVal =
        if
            is_integer(Relative) -> Relative;
            is_atom(Relative) -> maps:get(Relative, Labels) - PC
        end,
    ?I_JUMPS_LT(RelativeVal, Imm);
compile_instruction({jumpr_ge, Relative, Imm}, PC, Labels) ->
    RelativeVal =
        if
            is_integer(Relative) -> Relative;
            is_atom(Relative) -> maps:get(Relative, Labels) - PC
        end,
    ?I_JUMPR_GE(RelativeVal, Imm);
compile_instruction({jumpr_lt, Relative, Imm}, PC, Labels) ->
    RelativeVal =
        if
            is_integer(Relative) -> Relative;
            is_atom(Relative) -> maps:get(Relative, Labels) - PC
        end,
    ?I_JUMPR_LT(RelativeVal, Imm).

%% @doc Link a binary.
%% Append the header required by `load_binary/1,2'
%% @param TextBin binary of the code
%% @param DataBin binary of initialized data
%% @param BSSSize size of uninitialized data (loader will zero them)
%% @returns A compiled binary
-spec link(TextBin :: binary(), DataBin :: binary(), BSSSize :: non_neg_integer()) -> binary().
link(TextBin, DataBin, BSSSize) ->
    TextSize = byte_size(TextBin),
    DataSize = byte_size(DataBin),
    <<
        ?MAGIC:32/little,
        % text offset
        12:16/little,
        TextSize:16/little,
        DataSize:16/little,
        BSSSize:16/little,
        TextBin/binary,
        DataBin/binary
    >>.
