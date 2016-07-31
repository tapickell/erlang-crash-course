-module(my_server).
-export([ call/2
        , cast/2
        , reply/2
        , start/2
        , start_link/2
        ]).

%% Public API
start(Module, InitialSate) ->
  spawn(fun() -> init(Module, InitialSate) end).

start_link(Module, InitialSate) ->
  spawn_link(fun() -> init(Module, InitialSate) end).

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

cast(Pid, Msg) ->
  Pid ! {async, Msg},
  ok.

reply({Pid, Ref}, Reply) ->
  Pid ! {Ref, Reply}.

%% Private
init(Module, InitialSate) ->
  loop(Module, Module:init(InitialSate)).

loop(Module, State) ->
  receive
    {async, Msg} ->
      loop(Module, Module:handle_cast(Msg, State));
    {sync, Pid, Ref, Msg} ->
      loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
  end.

