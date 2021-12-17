-module(machinegunner).

%% API
-export([start/0]).
-export([start/1]).
-export([stop/0]).

%% application callbacks
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start() -> {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

-spec start([_Run :: atom()]) -> {ok, _}.
start([Run]) ->
    ok = application:set_env(?MODULE, run, Run),
    application:ensure_all_started(?MODULE).

-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    Run = application:get_env(?MODULE, run, undefined),
    StorageOpts = mk_storage_options(),
    {
        EventSinkChildSpec,
        EventSink
    } = mk_event_sink_child_spec(StorageOpts),
    ChildSpecs = [
        mgnr_pulse_handler:child_spec(Run),
        EventSinkChildSpec,
        mk_counter_child_spec(StorageOpts, EventSink)
    ],
    genlib_adhoc_supervisor:start_link(
        {local, ?MODULE},
        #{strategy => one_for_one},
        ChildSpecs
    ).

-type storage_opts() :: mg_core_storage:options().
-type event_sink_opts() :: mg_core_events_sink_machine:options().

-spec mk_storage_options() -> storage_opts().
mk_storage_options() ->
    {mg_core_storage_riak, #{
        host => "riak.compose",
        port => 8087,
        bucket => <<>>,
        pool_options => #{
            init_count => 4,
            max_count => 100
        }
    }}.

mk_counter_child_spec(StorageOpts, _EventSink) ->
    mg_core_events_machine:child_spec(
        mgnr_counter:configure(StorageOpts, []),
        counter
    ).

mk_event_sink_child_spec(StorageOpts) ->
    Options = mk_event_sink_options(StorageOpts),
    ChildSpec = mg_core_events_sink_machine:child_spec(Options, evsink),
    {ChildSpec, {mg_core_events_sink_machine, Options}}.

-spec mk_event_sink_options(storage_opts()) -> event_sink_opts().
mk_event_sink_options(StorageOpts) ->
    Worker = #{
        registry => mg_core_procreg_gproc
    },
    Pulse = mgnr_pulse_handler,
    #{
        namespace => <<"es">>,
        machine_id => <<"primary">>,
        storage => mgnr_config:postfix_storage(<<"es">>, StorageOpts),
        events_storage => mgnr_config:postfix_storage(<<"es">>, StorageOpts),
        worker => Worker,
        pulse => Pulse
    }.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
