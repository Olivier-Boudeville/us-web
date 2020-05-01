% Copyright (C) 2019-2020 Olivier Boudeville
%
% This file belongs to the US-Web project, a part of the Universal Server
% framework.
%
% This program is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.
%
% You should have received a copy of the GNU Affero General Public License along
% with this program. If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Wednesday, December 25, 2019.


% The main entry point of the us-web layer.
-module(us_web_app).

-behaviour(application).

-export([ start/0, start/2, stop/1 ]).


% Implementation notes:
%
% Calls to io:format/{1,2} shall not be replaced typically by trace_utils ones,
% in order to better diagnose problems with dependencies (typically should
% Myriad not be found).



% Typically if called manually, from a shell:
start() ->
	io:format( "Starting us_web application with default settings...~n" ),
	application:start( us_web ).



% Typically called by running '[...]/bin/us_web start' (see the 'start' make
% target):
%
start( StartType, StartArgs ) ->

	io:format( "Starting us_web application "
			   "(start type: ~p, arguments: ~p)...~n",
			   [ StartType, StartArgs ] ),

	% To debug any dependency-related 'undef' problem, or to ensure
	% concurrently-emitted messages can be seen (otherwise many outputs may be
	% lost):
	%
	%io:format( "Current code path:~n~p~n", [ code:get_path() ] ),
	%timer:sleep( 2000 ),

	start_prerequisites(),

	%basic_utils:display( "Prerequisites started; loaded applications:~n~p~n",
	%		   [ application:loaded_applications() ] ),

	us_web_sup:start_link().



stop( _State ) ->
	io:format( "Stopping us_web application.~n" ),
	ok.



% Internal functions:

list_required_applications() ->

	% Actually the applications listed in the relx release section of
	% rebar.config are automatically started (with default settings), so they
	% should not be listed here (otherwise will detect that they are already
	% launched).

	%[ crypto, ranch, cowboy, nprocreg, simple_bridge, simple_cache, qdate,
	%  nitrogen_core, sync ].
	%[simple_bridge].
	%[ ranch, cowlib, cowboy ].
	[].


start_prerequisites() ->

	PreReqs = list_required_applications(),

	io:format( "Starting prerequisites ~p...~n", [ PreReqs ] ),

	[ start_application( App ) || App <- PreReqs ].



% Startings required applications:
start_application( simple_bridge ) ->

	io:format( "- starting 'simple_bridge'~n" ),

	case simple_bridge:start( _Backend=cowboy, _Handler=us_web ) of

		ok ->
			io:format( "(simple_bridge started)~n" );

		Other ->
			io:format( "simple_bridge failed to start :~n~p~n", [ Other ] ),
			throw( { app_start_failed, simple_bridge, Other } )

	end;


start_application( App ) ->

	io:format( "- starting application '~s'~n", [ App ] ),

	case application:start( App ) of

		ok ->
			io:format( "(application '~s' started)~n", [ App ] );

		{ error, { already_started, myriad } } ->
			io:format( "Error, application '~s' was already started.~n",
					   [ App ] ),
			throw( { app_already_started, App } );


		Other ->
			io:format( "Application '~s' failed to start :~n~p~n",
					   [ App, Other ] ),
			throw( { app_start_failed, App, Other } )

	end.
