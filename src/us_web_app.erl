% Copyright (C) 2019-2021 Olivier Boudeville
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


% The main entry point of the US-Web active OTP application.
%
% Typically triggered:
%  - through OTP/rebar3, by ebin/us_web.app (as obtained from
%  conf/us_web.app.src; see start/2)
%  - directly, with the help of Myriad's otp_utils (see exec/0)
%
-module(us_web_app).

-behaviour(application).

-export([ exec/0, start/2, stop/1 ]).

% Shorthands:
-type application_name() :: otp_utils:application_name().


% Implementation notes:
%
% We define two starting procedures here:
%  - the direct, native one (yet OTP-compliant for prerequisites), through our
%  Ceylan make system
%  - the OTP release-based one (generally based on rebar3)

% Calls to io:format/{1,2} shall not be replaced typically by trace_bridge ones,
% in order to better diagnose problems with dependencies (typically should
% Myriad not be found).


% Silencing:
-export([ start_application/1 ]).



% Runs US-Web, directly (ex: as 'make us_web_exec') rather than as an OTP
% release.
%
-spec exec() -> void().
exec() ->

	% Expecting Myriad to be already available in this branch:
	trace_bridge:info(
	  "Starting the US-Web application natively (not as a release)." ),

	cond_utils:if_defined( us_web_debug_execution,
		trace_bridge:debug_fmt( "Initially, the ~s",
							   [ code_utils:get_code_path_as_string() ] ) ),

	% Not in an OTP context here, yet we need OTP applications (Cowboy, LEEC,
	% etc.) to be available (ex: w.r.t. their .app and BEAMs being found, their
	% starting to be done, etc.); we just not want US-Web to be launched the
	% same way:

	% Base build root directory from which prerequisite applications may be
	% found:
	%
	BuildRootDir = "..",

	% For all (direct and indirect) OTP prerequisites: updating ebin paths so
	% that the corresponding *.app and BEAM files are found, checking that
	% applications are compiled and preparing their starting.
	%
	% We used to blacklist notably shotgun, as for this use case of LEEC we do
	% not use it (the same applies for elli), moreover shotgun implies (its own
	% version of) cowlib, which may clash with the one needed by cowboy; however
	% as long as shotgun is listed in the 'applications' key of LEEC, having OTP
	% start LEEC afterwards results in checking shotgun (and failing then); so
	% now LEEC's .app file does not list shotgun (or elli) anymore, they are to
	% be started explicitly only if needed.
	%
	%BlacklistedApps = [ shotgun, elli ],
	BlacklistedApps = [],

	OrderedAppNames = otp_utils:prepare_for_execution( _ThisApp=us_web,
											BuildRootDir, BlacklistedApps ),

	% Retain all applications but US-Web itself, so that we can run US-Web as we
	% want:
	%
	{ us_web, PrereqAppNames } =
		list_utils:extract_last_element( OrderedAppNames ),

	trace_bridge:info_fmt( "Resulting prerequisite applications to start, "
						   "in order: ~w.", [ OrderedAppNames ] ),

	otp_utils:start_applications( PrereqAppNames, _RestartType=temporary,
								  BlacklistedApps ),

	% So here the us_web application is not regarded as specifically started
	% (start/2 below never called):
	%
	us_web_sup:start_link( as_native ),

	trace_bridge:debug( "US-Web started (as native)." ).



% Called when US-Web itself is started as an OTP application (as opposed to
% natively, "manually", see exec/0).
%
% The setup and dependency management shall have been done already by the OTP
% release system. So here no ebin path to set or prerequisite applications to
% start for applications listed in US-Web's .app file, we focus only on the
% applications not listed whereas possibly useful in this context (shotgun,
% elli) and on us_web itself.
%
% Note that it may easier/more reliable to add these applications directly in
%
start( StartType, StartArgs ) ->

	% Myriad may be already available in this branch as well, though:
	io:format( "Starting us_web application "
		"(start type: ~p, arguments: ~p)...~n", [ StartType, StartArgs ] ),

	% Location to check:
	BuildRootDir = "..",

	% They will be started by LEEC afterwards:
	_OrderedAppNames = otp_utils:prepare_for_execution( [ shotgun, elli ],
													   BuildRootDir ),

	% To debug any dependency-related 'undef' problem, or to ensure
	% concurrently-emitted messages can be seen (otherwise many outputs may be
	% lost):
	%
	%io:format( "Current code path:~n~p~n", [ code:get_path() ] ),
	%io:format( "undef interpretation: ~s~n",
	%  [ code_utils:interpret_undef_exception( M, F, A ) ] ),
	%
	%timer:sleep( 2000 ),

	%basic_utils:display( "Prerequisites started; loaded applications:~n~p~n",
	%		   [ application:loaded_applications() ] ),

	% See http://erlang.org/doc/design_principles/applications.html:
	us_web_sup:start_link( as_otp_release ).


stop( _State ) ->
	trace_bridge:info( "Stopping us_web application." ),

	% In native context, explicit stopping should be done, with the same
	% blacklisting.

	ok.



% Internal functions:


% Startings required applications (not used currently):
-spec start_application( application_name() ) -> void().
start_application( simple_bridge ) ->

	trace_bridge:info_fmt( "Starting 'simple_bridge'..." ),

	case simple_bridge:start( _Backend=cowboy, _Handler=us_web ) of

		ok ->
			trace_bridge:debug_fmt( "(simple_bridge started)" );

		Other ->
			trace_bridge:error_fmt( "simple_bridge failed to start:~n~p",
									[ Other ] ),
			throw( { app_start_failed, simple_bridge, Other } )

	end;

start_application( App ) ->
	otp_utils:start_application( App ).
