-module(mgnr_loadgen).

-export([run/4]).

-export([uniquize/1]).

%%

-type id() :: binary().
-type seconds() :: pos_integer().
-type fraction() :: float().
-type rate() :: number() | {number(), _Jitter :: fraction()}.
-type work() :: workfun() | {uniquize, workfun()}.
-type workfun() :: fun((id()) -> _).

-spec run(_Runtime :: seconds(), _Concurrency :: pos_integer(), rate(), work()) -> _.
run(Runtime, Concurrency, Rate, Work) when is_number(Rate) ->
    run(Runtime, Concurrency, {Rate, 0.0}, Work);
run(Runtime, Concurrency, Rate, Work) ->
    _ = warmup(Runtime, Rate, Work),
    Runners = [
        start_runner(mk_id(Nw), Nw, Concurrency, Runtime, Rate, Work)
     || Nw <- lists:seq(1, Concurrency)
    ],
    wait_completion(Runners).

warmup(Runtime, {Rate, _Jitter}, Work) ->
    WarmupRuntime = min(5, ceil(Runtime / 10)),
    WarmupRate = min(5, ceil(Rate / 10)),
    Runner = start_runner(<<"warmup">>, 1, 1, WarmupRuntime, {WarmupRate, 0.0}, Work),
    wait_completion([Runner]).

start_runner(ID, Nw, Concurrency, Runtime, Rate, Work) ->
    Delay = compute_initial_delay(Nw, Concurrency, Rate),
    {Pid, MRef} = erlang:spawn_monitor(fun() -> runner(ID, Delay, Rate, Work) end),
    StopAfter = erlang:convert_time_unit(Runtime, seconds, millisecond),
    _ = erlang:send_after(StopAfter, Pid, enough),
    {ok, _TRef} = timer:kill_after(StopAfter + 5000, Pid),
    ok = mgnr_pulse_handler:trace(loadgen, ID, <<"started">>, {}),
    {ID, Pid, MRef}.

runner(ID, Delay, Rate, Work) ->
    ok = delay(Delay),
    loop(ID, 1, mk_rate_limiter(Rate), Work).

compute_initial_delay(Nw, Concurrency, {Rate, _Jitter}) ->
    ceil((Nw - 1) * 1000 / Concurrency / Rate).

loop(ID, N, RateLim, Work) ->
    receive
        enough -> exit(normal)
    after 0 -> ok
    end,
    _ = work(Work, mk_id(ID, N)),
    {Estimate, RateLim1} = limit_rate(RateLim),
    ok = mgnr_pulse_handler:trace(loadgen, ID, <<"rate">>, Estimate),
    loop(ID, N + 1, RateLim1, Work).

work({uniquize, Work}, ID) ->
    work(Work, uniquize(ID));
work(Fun, ID) when is_function(Fun, 1) ->
    Fun(ID).

mk_id(N) ->
    integer_to_binary(N).

mk_id(ID, N) ->
    <<ID/binary, "/", (integer_to_binary(N))/binary>>.

wait_completion([{ID, Pid, MRef} | Rest]) ->
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            ok = mgnr_pulse_handler:trace(loadgen, ID, <<"finished">>, Reason),
            normal = Reason,
            wait_completion(Rest)
    end;
wait_completion([]) ->
    ok.

%%

-spec uniquize(id()) -> id().
uniquize(ID) ->
    %% 2021-01-01T00:00:00Z
    Epoch = 1609459200000,
    Prefix = genlib_format:format_int_base(erlang:system_time(millisecond) - Epoch, 62),
    <<Prefix/binary, "/", ID/binary>>.

%%

-record(lim, {
    rate :: number(),
    jitter :: fraction(),
    t0 :: integer(),
    n :: non_neg_integer()
}).

mk_rate_limiter({Rate, Jitter}) ->
    #lim{
        rate = Rate,
        jitter = Jitter,
        t0 = erlang:monotonic_time(millisecond),
        n = 0
    }.

limit_rate(Lim = #lim{rate = Rate, jitter = Jitter, t0 = T0, n = N}) ->
    J = (rand:uniform() * 2.0 - 1.0) * Jitter,
    Te = T0 + floor(1000 * (N + 1 + J) / Rate),
    Tn = erlang:monotonic_time(millisecond),
    Lim1 = Lim#lim{n = N + 1},
    case Te - Tn of
        Dt when Dt > 0 ->
            delay(Dt);
        _Dt ->
            ok
    end,
    {estimate_rate(Lim1), Lim1}.

estimate_rate(#lim{t0 = T0, n = N}) when N > 0 ->
    Tn = erlang:monotonic_time(millisecond),
    1000 / (max(Tn - T0, 1) / N).

delay(Dt) ->
    receive
    after Dt -> ok
    end.
