-module(trade_fsm).
-behaviour(gen_fsm).

%% public api
-export([ start/1
        , start_link/1
        , trade/2
        , accept_trade/1
        , make_offer/2
        , retract_offer/2
        , ready/1
        , cancel/1
        ]).

%% gen_fsm callbacks
-export([ init/1
        , handle_event/3
        , handle_sync_event/4
        , handle_info/3
        , terminate/3
        , code_change/4
        ]).

%% custom state names
-export([ idle/2
        , idle/3
        , idle_wait/2
        , idle_wait/3
        , negotiate/2
        , negotiate/3
        , wait/2
        , ready/2
        , ready/3
        ]).

%% public api
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%% async negotiation functions
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

%% synch negotiation functions
ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

%% all state event
notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).

%% fsm callbacks
-record(state, { name=""
               , other
               , ownitems=[]
               , otheritems=[]
               , monitor
               , from
               }).

init(Name) ->
  {ok, idle, #state{name=Name}}.

notice(#state{name=N}, Str, Args) ->
  io:format("~s: "++Str++"~n", [N|Args]).

unexpected(Msg, State) ->
  io:format("~p received unkown event ~p while in state ~p~n"
            , [self(), Msg, State]).

add(Item, Items) ->
  [Item | Items].

remove(Item, Items) ->
  Items -- [Item].

idle({ask_negotiate, OtherPid}, S=#state{}) ->
  Ref = monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting negotiation", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

