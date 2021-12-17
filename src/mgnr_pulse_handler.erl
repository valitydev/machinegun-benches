-module(mgnr_pulse_handler).

-include_lib("machinegun_core/include/pulse.hrl").

-export([child_spec/1]).
-export([start_link/1]).
-export([log/2]).
-export([trace/4]).

-behaviour(mg_core_pulse).
-export([handle_beat/2]).

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

%%

-spec child_spec(string() | undefined) -> supervisor:child_spec().
child_spec(ID) ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, [ID]},
        restart => permanent
    }.

-spec start_link(string()) -> {ok, pid()}.
start_link(ID) ->
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        ID,
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ).

-spec log(_Format :: string(), _Args :: list()) -> ok.
log(Format, Args) ->
    gen_server:cast(?MODULE, {log, mgnr_clock:read(), Format, Args}).

-spec trace(_Class :: atom(), _ID :: binary(), _Event :: iodata(), _Data) -> ok.
trace(Class, ID, Event, Data) ->
    gen_server:cast(?MODULE, {trace, mgnr_clock:read(), Class, ID, Event, Data}).

%%

-spec handle_beat(undefined, mg_core_pulse:beat()) -> ok.
handle_beat(_Options, Beat) ->
    gen_server:cast(?MODULE, {beat, mgnr_clock:read(), self(), Beat}).

%%

-type state() :: {string(), file:io_device()}.

-spec init(string() | undefined) -> {ok, state()}.
init(ID) ->
    FD =
        case ID of
            undefined ->
                standard_error;
            _ ->
                {ok, R} = file:open(mk_log_filename(ID), [write, exclusive]),
                R
        end,
    _ = erlang:process_flag(priority, low),
    {ok, {ID, FD}}.

mk_log_filename(ID) ->
    Datetime = calendar:system_time_to_rfc3339(erlang:system_time(second), [{offset, "Z"}]),
    ID ++ "." ++ string:replace(Datetime, ":", "-", all) ++ ".log".

-spec handle_call(_Call, _From, state()) -> {noreply, state()}.
handle_call(_Call, _From, State) ->
    {noreply, State}.

-type cast() :: {log, mgnr_clock:microseconds(), _Format :: string(), _Args :: list()}.

-spec handle_cast(cast(), state()) -> {noreply, state()}.
handle_cast({log, Clock, Format, Args}, St) ->
    ok = write_log(Clock, Format, Args, St),
    {noreply, St};
handle_cast({trace, Clock, Class, ID, Event, Data}, St) ->
    ok = write_trace(Clock, Class, ID, Event, Data, St),
    {noreply, St};
handle_cast({beat, Clock, From, Beat}, St) ->
    MachineID =
        case get_machine_id(Beat) of
            ID when is_binary(ID) ->
                _ = try_save_machine_id(From, Beat),
                ID;
            undefined ->
                get_saved_machine_id(From)
        end,
    case MachineID /= undefined of
        true ->
            case get_event_name(Beat) of
                Name when is_binary(Name) ->
                    ok = write_trace(Clock, beat, MachineID, Name, Beat, St);
                undefined ->
                    ok
            end;
        false ->
            ok
    end,
    {noreply, St}.

try_save_machine_id(From, #mg_core_machine_process_started{machine_id = ID}) ->
    % Try to associate ID w/ pid, to augment ID-less events later.
    _ = erlang:put(From, ID);
try_save_machine_id(From, #mg_core_machine_lifecycle_unloaded{}) ->
    erlang:erase(From);
try_save_machine_id(From, #mg_core_machine_lifecycle_failed{}) ->
    erlang:erase(From);
try_save_machine_id(_From, _) ->
    ok.

get_saved_machine_id(From) ->
    erlang:get(From).

write_trace(Clock, Class, ID, Event, Data, St) ->
    write_log(Clock, "~p\t~s\t~s\t~0p", [Class, ID, Event, Data], St).

write_log(Clock, Format, Args, {ID, FD}) ->
    io:format(FD, "~s\t~s\t" ++ Format ++ "~n", [ID, mgnr_clock:format(Clock) | Args]).

%%

get_machine_id(#mg_core_worker_call_attempt{machine_id = ID}) -> ID;
get_machine_id(#mg_core_worker_start_attempt{machine_id = ID}) -> ID;
get_machine_id(#mg_core_machine_lifecycle_loaded{machine_id = ID}) -> ID;
get_machine_id(#mg_core_machine_lifecycle_created{machine_id = ID}) -> ID;
get_machine_id(#mg_core_machine_lifecycle_unloaded{machine_id = ID}) -> ID;
get_machine_id(#mg_core_machine_lifecycle_failed{machine_id = ID}) -> ID;
get_machine_id(#mg_core_machine_process_started{machine_id = ID}) -> ID;
get_machine_id(#mg_core_machine_process_finished{machine_id = ID}) -> ID;
get_machine_id(_) -> undefined.

get_event_name(#mg_core_worker_call_attempt{}) ->
    <<"worker call">>;
get_event_name(#mg_core_worker_start_attempt{}) ->
    <<"worker start">>;
get_event_name(#mg_core_machine_lifecycle_loaded{}) ->
    <<"machine loaded">>;
get_event_name(#mg_core_machine_lifecycle_created{}) ->
    <<"machine created">>;
get_event_name(#mg_core_machine_lifecycle_unloaded{}) ->
    <<"machine unloaded">>;
get_event_name(#mg_core_machine_lifecycle_failed{}) ->
    <<"machine failed">>;
get_event_name(#mg_core_machine_process_started{processor_impact = {init, _}}) ->
    <<"process init started">>;
get_event_name(#mg_core_machine_process_finished{processor_impact = {init, _}}) ->
    <<"process init finished">>;
get_event_name(#mg_core_machine_process_started{processor_impact = {call, _}}) ->
    <<"process call started">>;
get_event_name(#mg_core_machine_process_finished{processor_impact = {call, _}}) ->
    <<"process call finished">>;
get_event_name(#mg_core_machine_process_started{processor_impact = timeout}) ->
    <<"process timer started">>;
get_event_name(#mg_core_machine_process_finished{processor_impact = timeout}) ->
    <<"process timer finished">>;
get_event_name(#mg_core_machine_process_started{processor_impact = continuation}) ->
    <<"process continuation started">>;
get_event_name(#mg_core_machine_process_finished{processor_impact = continuation}) ->
    <<"process continuation finished">>;
get_event_name(#mg_core_storage_get_start{}) ->
    <<"storage get started">>;
get_event_name(#mg_core_storage_get_finish{}) ->
    <<"storage get finished">>;
get_event_name(#mg_core_storage_put_start{}) ->
    <<"storage put started">>;
get_event_name(#mg_core_storage_put_finish{}) ->
    <<"storage put finished">>;
get_event_name(_) ->
    undefined.
