-module(mgnr_counter).

-export([configure/2]).
-export([init/1]).
-export([read/1]).
-export([increment/1]).

%% mg_core_events_machine handler
-behaviour(mg_core_events_machine).
-export([process_signal/4]).
-export([process_call/4]).
-export([process_repair/4]).

-define(INCREMENT, <<"increment">>).

-type id() :: binary().
-type options() :: #{}.

%%

-spec configure(mg_core_storage:options(), [mg_core_events_sink:handler()]) -> mg_core_events_machine:options().
configure(StorageOpts, EventSinks) ->
    NS = <<"counter">>,
    Pulse = mgnr_pulse_handler,
    Processor = {mgnr_counter, #{}},
    Worker = #{
        registry => mg_core_procreg_gproc
    },
    % SchedulerOpts = #{
    %     min_scan_delay => 5000
    % },
    Schedulers = #{
        timers => disable,
        overseer => disable,
        timers_retries => disable
    },
    Options = #{
        namespace => NS,
        processor => Processor,
        machines => #{
            namespace => NS,
            storage => mgnr_config:postfix_storage(<<"m">>, StorageOpts),
            worker => Worker,
            pulse => Pulse,
            schedulers => Schedulers
        },
        tagging => #{
            namespace => genlib_string:cat(NS, <<"_tags">>),
            storage => StorageOpts,
            worker => Worker,
            pulse => Pulse,
            retries => #{}
        },
        events_storage => mgnr_config:postfix_storage(<<"e">>, StorageOpts),
        event_stash_size => 5,
        event_sinks => EventSinks,
        pulse => Pulse
    },
    _ = persistent_term:put(?MODULE, Options),
    Options.

-spec init(id()) -> ok.
init(ID) ->
    ReqCtx = mk_req_ctx(),
    _ = mgnr_pulse_handler:trace(client, ID, <<"init started">>, {}),
    try
        mg_core_events_machine:start(options(), ID, null, ReqCtx, undefined)
    after
        mgnr_pulse_handler:trace(client, ID, <<"init finished">>, {})
    end.

-spec read(id()) -> non_neg_integer().
read(ID) ->
    Range = mk_whole_range(),
    _ = mgnr_pulse_handler:trace(client, ID, <<"read started">>, {}),
    try
        Machine = mg_core_events_machine:get_machine(options(), {id, ID}, Range),
        collapse_machine(Machine)
    after
        mgnr_pulse_handler:trace(client, ID, <<"read finished">>, {})
    end.

-spec increment(id()) -> ok.
increment(ID) ->
    ReqCtx = mk_req_ctx(),
    Range = mk_whole_range(),
    _ = mgnr_pulse_handler:trace(client, ID, <<"increment started">>, {}),
    try
        mg_core_events_machine:call(options(), {id, ID}, ?INCREMENT, Range, ReqCtx, undefined)
    after
        mgnr_pulse_handler:trace(client, ID, <<"increment finished">>, {})
    end.

options() ->
    persistent_term:get(?MODULE).

mk_whole_range() ->
    {undefined, undefined, forward}.

mk_req_ctx() ->
    mgnr_clock:format(mgnr_clock:read()).

%%

-type request_ctx() :: mg_core:request_context().

-type deadline() :: mg_core_deadline:deadline().
-type signal_result() :: mg_core_events_machine:signal_result().
-type call_result() :: mg_core_events_machine:call_result().
-type repair_result() :: mg_core_events_machine:repair_result().

-spec process_signal(options(), request_ctx(), deadline(), mg_core_events_machine:signal_args()) -> signal_result().
process_signal(_Options, _ReqCtx, _Deadline, {{init, _Args}, Machine}) ->
    State = collapse_machine(Machine),
    Change = {mk_content(mk_aux_state()), [mk_content(mk_event(increment, State))]},
    Action = #{},
    {Change, Action}.

-spec process_call(options(), request_ctx(), deadline(), mg_core_events_machine:call_args()) -> call_result().
process_call(_Options, _ReqCtx, _Deadline, {?INCREMENT, Machine}) ->
    State = collapse_machine(Machine),
    Change = {mk_content(mk_aux_state()), [mk_content(mk_event(increment, State))]},
    Action = #{},
    {ok, Change, Action}.

-spec process_repair(options(), request_ctx(), deadline(), mg_core_events_machine:repair_args()) -> repair_result().
process_repair(_Options, _ReqCtx, _Deadline, {_Args, _Machine}) ->
    {error, {failed, noimpl}}.

collapse_machine(#{history := History}) ->
    collapse_history(History).

collapse_history(History) ->
    lists:foldl(
        fun(Event, State) ->
            merge_event(decode_event(Event), State)
        end,
        init_state(),
        History
    ).

decode_event(#{id := _ID, body := {_Metadata, Event}}) ->
    Event.

init_state() ->
    0.

merge_event(N, _) when is_integer(N) ->
    N.

mk_event(increment, N) ->
    N + 1.

mk_content(Opaque) ->
    {#{format_version => 1}, Opaque}.

mk_aux_state() ->
    null.
