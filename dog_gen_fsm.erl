-module(dog_gen_fsm).
-behaviour(gen_fsm).
-compile(export_all).

init([]) -> {ok, []}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

%% Async Events
handle_event(Event, StateName, StateData) ->
  {ok}.

%% Synch Events
handle_sync_event(_One, _Two, _Three, _Four) ->
  {ok}.

handle_info(_One, _Two, _Three) ->
  {ok}.

terminate(_One, _Two, _Three) ->
  {ok}.

%% Async Events
state_name(Event, StateData) ->
  {ok}.

%% Synch Events
state_name(Event, From, StateData) ->
   {ok}.
