-module(mgnr_clock).

-export([read/0]).
-export([format/1]).

-type t() :: non_neg_integer().

-export_type([t/0]).

-spec read() -> t().
read() ->
    Epoch = epoch(),
    erlang:monotonic_time() - Epoch.

epoch() ->
    case persistent_term:get(Key = {?MODULE, epoch}, undefined) of
        Epoch when is_integer(Epoch) ->
            Epoch;
        undefined ->
            Epoch = erlang:monotonic_time(),
            ok = persistent_term:put(Key, Epoch),
            Epoch
    end.

-spec format(t()) -> binary().
format(Clock) ->
    Micros = erlang:convert_time_unit(Clock, native, microsecond),
    genlib_format:format_decimal(Micros, 6).
