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


% The main entry point of the US-Web application.
%
% Typically triggered:
%  - through OTP/rebar3, by ebin/us_web.app (as obtained from
%  conf/us_web.app.src; see start/2)
%  - directly, with the help of Myriad's otp_utils (see exec/0)
%
-module(us_web_app).

-behaviour(application).

-export([ exec/0, get_ordered_otp_prerequisites/0, start/0, start/2, stop/1 ]).


% Implementation notes:
%
% Calls to io:format/{1,2} shall not be replaced typically by trace_utils ones,
% in order to better diagnose problems with dependencies (typically should
% Myriad not be found).


% Shorthands:
-type application_name() :: otp_utils:application_name().


% Runs US-Web, directly (ex: as 'make us_web_exec') rather than as a release.
-spec exec() -> void().
exec() ->

	% Myriad may not be already available:
	io:format( "Starting the US-Web application directly "
			   "(not as a release).~n" ),

	% Not in an OTP context here, yet we need OTP applications (Cowboy, LEEC,
	% etc.) to be available (ex: w.r.t. their .app being found, etc.):

	OrderedAppNames = get_ordered_otp_prerequisites(),

	% Build root directory from which prerequisite applications may be found:
	BuildRootDir = "..",

	% Updating ebin paths so that the corresponding *.app files are found:
	%
	% (not adding us_web as the purpose of this function is to build/run US-Web
	% without rebar3)
	%
	case otp_utils:prepare_for_execution( OrderedAppNames, BuildRootDir ) of

		ready ->
			ok;

		{ lacking_app, AppName } ->
			throw( { lacking_prerequisite_app, AppName } )

	end,

	start().



% Returns an (ordered) list of the US-Web prerequisite OTP applications, to be
% started in that order.
%
% Notes:
% - not listed here (not relevant for that use case): elli, getopt, yamerl,
% erlang_color
% - jsx preferred over jiffy; yet neither needs to be initialized as an
% application
% - no need to start myriad either
%
-spec get_ordered_otp_prerequisites() -> [ application_name() ].
get_ordered_otp_prerequisites() ->
	[ ranch, cowlib, cowboy, myriad, wooper, traces ].



% Typically if called manually, from a shell:
start() ->

	trace_utils:info( "Starting the US-Web application "
					  "with default settings..." ),

	{ ok, AppNames } = application:ensure_all_started( us_web ),

	trace_bridge:debug_fmt( "Applications started: ~p.", [ AppNames ] ),

	%application:start( us_web ).
	ok.


% Typically called when executed as a release, by running '[...]/bin/us_web
% start' (see the 'start' make target):
%
start( StartType, StartArgs ) ->

	io:format( "Starting us_web application "
		"(start type: ~p, arguments: ~p)...~n", [ StartType, StartArgs ] ),

	% To debug any dependency-related 'undef' problem, or to ensure
	% concurrently-emitted messages can be seen (otherwise many outputs may be
	% lost):
	%
	%io:format( "Current code path:~n~p~n", [ code:get_path() ] ),
	%timer:sleep( 2000 ),

	start_prerequisites(),

	%basic_utils:display( "Prerequisites started; loaded applications:~n~p~n",
	%		   [ application:loaded_applications() ] ),

	% See http://erlang.org/doc/design_principles/applications.html:
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

	case list_required_applications() of

		[] ->
			ok;

		PreReqs ->
			trace_utils:info_fmt( "Starting prerequisites ~p...", [ PreReqs ] ),
			[ start_application( App ) || App <- PreReqs ]

	end.


% Startings required applications:
start_application( simple_bridge ) ->

	trace_utils:info_fmt( "Starting 'simple_bridge'..." ),

	case simple_bridge:start( _Backend=cowboy, _Handler=us_web ) of

		ok ->
			trace_utils:debug_fmt( "(simple_bridge started)" );

		Other ->
			trace_utils:error_fmt( "simple_bridge failed to start :~n~p", [ Other ] ),
			throw( { app_start_failed, simple_bridge, Other } )

	end;


start_application( App ) ->

	trace_utils:info_fmt( "Starting application '~s'.", [ App ] ),

	case application:start( App ) of

		ok ->
			trace_utils:debug_fmt( "(application '~s' started)", [ App ] );

		{ error, { already_started, myriad } } ->
			trace_utils:error_fmt(
			  "Error, application '~s' was already started.",  [ App ] ),
			throw( { app_already_started, App } );


		Other ->
			trace_utils:error_fmt( "Application '~s' failed to start :~n~p",
								   [ App, Other ] ),
			throw( { app_start_failed, App, Other } )

	end.
