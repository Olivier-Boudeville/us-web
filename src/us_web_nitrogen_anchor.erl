-module(us_web_nitrogen_anchor).

-moduledoc """
This is the US-Web module in charge of **handling requests aimed at
Nitrogen-based virtual hosts**.

Directly adapted from simple_bridge's
src/cowboy_bridge_modules/cowboy_simple_bridge_anchor.erl module.
""".


-export([
		init/2,
		handle/2,
		terminate/2,
		websocket_init/1,
		websocket_handle/2,
		websocket_info/2,
		websocket_terminate/2 ]).


-record( ws_state,
		 {handler, keepalive_interval, bridge, state, init_req} ).


% State is BinContentRoot here:
init( Req, State ) ->

	trace_utils:debug_fmt( "[~w] Initialising US-Web Nitrogen anchor with:~n"
		" - Req = ~p~n - State = ~p", [ self(), Req, State ] ),

	% Lighter:
	%trace_utils:debug_fmt( "Initialising US-Web Nitrogen anchor with "
	%                       "State = ~p (Req not traced)", [ State ] ),

	% Typically set by class_USWebConfigServer:get_nitrogen_dispatch_for/6:
	BinContentRoot = State,

	%basic_utils:crash(),

	Upgrade = cowboy_req:header( <<"upgrade">>, Req ),

	Upgrade2 = case Upgrade of

		undefined ->
			undefined;

		Other ->
			simple_bridge_util:binarize_header( Other )

	end,

	case Upgrade2 of

		<<"websocket">> ->
			% Keepalive stuff:
			{ KAInterval, KATimeout } =
				simple_bridge_util:get_websocket_keepalive_interval_timeout(
					cowboy),

			CowboyTimeout =
				simple_bridge_websocket:keepalive_timeout( KAInterval,
														   KATimeout ),

			% START YOUR ENGINES, FOLKS!
			WSState = #ws_state{ init_req = Req,
								 keepalive_interval=KAInterval },

			{ cowboy_websocket, Req, WSState,
			  #{ idle_timeout => CowboyTimeout } };

		_ ->
			%DocRoot = simple_bridge_util:get_env(document_root),
			DocRoot = BinContentRoot,

			Handler = simple_bridge_util:get_env( handler ),
			Bridge = simple_bridge:make( cowboy, { Req, DocRoot } ),

			trace_utils:debug_fmt( "Anchor init with DocRoot = ~p, "
				"Handler = ~p and~n Bridge = ~p.",
				[ DocRoot, Handler, Bridge ] ),

			% Typically nitrogen:run/1:
			{ ok, NewReq } = Handler:run( Bridge ),

			trace_utils:debug_fmt( "NewReq = ~p", [ NewReq ] ),

			{ ok, NewReq, State }

		end.


handle( Req, State ) ->
	trace_utils:debug_fmt( "[~w] Handling Req = ~p (state: ~p).",
						   [ self(), Req, State ] ),
	{ ok, Req, State }.


terminate( Reason, State ) ->
	trace_utils:debug_fmt( "[~w] Terminating, with Reason = ~p (state: ~p).",
						   [ self(), Reason, State ] ),
	ok.



websocket_init( State ) ->

	%trace_utils:debug_fmt( "[~w] Initialising websockets (state: ~p).",
	%                       [ self(), State ] ),

	trace_utils:debug_fmt( "[~w] Initialising websockets.", [ self() ] ),

	{ ok, Handler } = application:get_env( simple_bridge, handler ),

	Bridge = simple_bridge:make( cowboy, { State#ws_state.init_req, "" } ),

	UserState = simple_bridge_websocket:call_init( Handler, Bridge ),

	simple_bridge_websocket:schedule_keepalive_msg(
		State#ws_state.keepalive_interval ),

	% Start your engines, folks!
	WSState = State#ws_state{ state=UserState,
							  init_req=undefined,
							  bridge=Bridge,
							  handler=Handler },

	{ ok, WSState }.



websocket_handle( {ping, _Data}, WSState ) ->
	% We don't need to pong, cowboy does that automatically. So just carry on!
	{ok, WSState};

websocket_handle({pong, _PongMsg}, WSState) ->
	{ok, WSState};

websocket_handle(Data, WSState) ->
	#ws_state{handler=Handler, bridge=Bridge, state=State} = WSState,
	Result = Handler:ws_message(Data, Bridge, State),
	massage_reply(Result, WSState).


websocket_info( simple_bridge_send_ping,
				WSState=#ws_state{keepalive_interval=KAInterval}) ->
	simple_bridge_websocket:schedule_keepalive_msg(KAInterval),
	{reply, {ping, <<"Simple Bridge Ping">>}, WSState};

websocket_info( Data, WSState ) ->
	#ws_state{ handler=Handler, bridge=Bridge, state=State } = WSState,
	Result = Handler:ws_info( Data, Bridge, State ),
	massage_reply( Result, WSState ).


websocket_terminate( Reason,
					 #ws_state{bridge=Bridge, handler=Handler, state=State} ) ->
	ok = Handler:ws_terminate( Reason, Bridge, State ).


-doc """
Reformats a simple_bridge return value into something that can
be handled by cowboy.
""".
massage_reply( {reply, {Type, Data}, NewState }, WSState )
		when Type==binary orelse Type==text ->
	{reply, {Type, iolist_to_binary(Data)}, WSState#ws_state{state=NewState}};

massage_reply({reply, List, NewState}, WSState) ->
	FixedList = [{Type, iolist_to_binary(Data)} || {Type, Data} <- List],
	{reply, FixedList, WSState#ws_state{state=NewState}};

massage_reply({reply, Reply}, WSState) ->
	massage_reply({reply, Reply, WSState#ws_state.state}, WSState);

massage_reply({noreply, NewState}, WSState) ->
	{ok, WSState#ws_state{state=NewState}};

massage_reply({remote, _Reason}, WSState) ->
	{stop, WSState};

massage_reply(stop, WSState) ->
	{stop, WSState}.
