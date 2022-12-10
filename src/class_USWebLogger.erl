% Copyright (C) 2019-2022 Olivier Boudeville
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
% Creation date: Tuesday, December 31, 2019.


% @doc Class providing <b>web logging</b> (accesses and errors) for the US-Web
% framework.
%
-module(class_USWebLogger).


-define( class_description, "Server in charge of the web logging "
		 "(accesses and errors) for the US-Web framework." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).



% Logging service for the US-Web activities.
%
% This includes:
% - access logging, typically for web analyzer tools such as awstats
% - error logging, for administration purposes
% - US-integrated optional advanced logging, for centralized monitoring in link
% with the Universal Server
%
% A web logger may or may not be a singleton (generally not); in general, to
% favor parallelism:
%
% - one instance thereof is created per (virtual) host
%
% - an overall instance thereof may also possibly be created (as a singleton),
% however its uses may overlap with the Traces file (typically in
% traces_via_otp.traces)

% Note that a web logger may be driven (typically for log rotation) either
% directly by a scheduler or by a task ring (for proper inter-logger
% synchronisation).
%
% A logger, as a task, may (actively) register to its scheduler, and
% symmetrically will thus have to unregister from it when relevant (hence the
% PID of that scheduler shall be kept); for a task ring, the logger is
% registered by a third party (that by default it does not know - it does not
% need to know the ring either, thus it does not store its PID), and is to be
% unregistered the same way.


% Using Awstats, two different, mostly unrelated actions shall be considered:
%
% - updating the Awstats database: it shall be done when rotating (access) logs
% (typically to be best done by being scheduled regularly for that),
% sequentially, per virtual host, with up to one instance running at any time
% (to avoid concurrent accesses to the Awstats database), thus typically based
% on a task ring; note that the rotation count starts at 1 and is incremented at
% each rotation step (which is common to the access and error log); note also
% that if a log file is empty, it will not be rotated, and as a result there may
% be gaps in the rotation counts of archive filenames ; finally, if at US-Web
% start-up log files already exist, they will be immediately rotated (hence even
% after a stop/crash of the server, no written log should be lost)
%
% - generating reports: either done statically/periodically (ex: if inserted in
% the task ring among access rotations, to avoid possible read/write concurrent
% accesses) or when explicitly requested by the user (ex: through a dedicated
% user console command (see generate-us-web-log-report.sh /
% us_web_generate_report_app.erl) or through an interactive, control page,
% possibly Nitrogen-based)



-type server_pid() :: class_UniversalServer:server_pid().

-type log_line() :: text_utils:bin_string().
% A log line entry.


-export_type([ server_pid/0, log_line/0 ]).


% To silence unused warnings:
-export([ generate_other_report_pages/3 ]).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type file_name() :: file_utils:file_name().
-type bin_directory_path() :: file_utils:bin_directory_path().
-type bin_file_name() :: file_utils:bin_file_name().

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type file() :: file_utils:file().

-type domain_id() :: class_USWebConfigServer:domain_id().
-type vhost_id() :: class_USWebConfigServer:vhost_id().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
-type user_period() :: class_USScheduler:user_period().

-type log_analysis_tool() :: class_USWebConfigServer:log_analysis_tool().

-include("class_USWebConfigServer.hrl").

-type web_analysis_info() :: class_USWebConfigServer:web_analysis_info().



% The class-specific attributes:
-define( class_attributes, [

	{ vhost_id, vhost_id(),
	  "the identifier of the corresponding virtual host" },

	{ domain_id, domain_id(),
	  "the domain identifier of the corresponding virtual host" },

	{ log_dir, bin_directory_path(),
	  "the directory where (basic, technical) logs shall be written" },

	% Currently not strictly needed:
	{ conf_dir, bin_directory_path(),
	  "the US-Web configuration directory, where notably the per-host "
	  "configuration files for log analysis tools are defined" },

	% Not used anymore, as having to rely on Awstats lookup scheme:
	%{ analysis_conf_path, text_utils:bin_string(),
	%  "the path to the configuration file (typically for Awstats) for the "
	%  "corresponding host" },

	{ access_log_file_path, bin_file_path(),
	  "the (absolute) path of the (current) access log file being written" },

	{ access_log_file, file(),
	  "the handle of the actual access file being written" },


	{ error_log_file_path, bin_file_name(),
	  "the (absolute) path of the (current) error log file being written" },

	{ error_log_file, file(),
	  "the handle of the actual error file being written" },


	{ scheduler_pid, maybe( scheduler_pid() ),
	  "the PID of any scheduler used by this logger; otherwise that logger is "
	  "expected to be driven by a task ring" },


	{ log_task_id, maybe( task_id() ),
	  "the identifier of this task for log rotation, as assigned by the "
	  "scheduler (if any)" },

	{ rotation_count, count(), "the number of the upcoming rotation to "
	  "take place (useful to keep track of a series of rotated files)" },

	{ web_analysis_info, maybe( web_analysis_info() ),
	  "settings about web analysis (if any)" },

	{ log_analysis_update_cmd, maybe( ustring() ), "the prebuilt command to "
	  "run in order to update the log analysis database" },

	{ log_analysis_report_gen_cmd, maybe( ustring() ),
	  "the prebuilt command to run in order to generate the log analysis "
	  "reports" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Web.Logging" ).


-define( registration_name, us_web_logger ).

-define( registration_scope, global_only ).



% The default DHMS period at which log rotation will be requested by a
% given logger instance.

% Every 4 minutes (for testing):
%-define( default_dhms_log_rotation_period, { 0, 0, 4, 0 } ).

% Every 2 days:
-define( default_dhms_log_rotation_period, { 2, 0, 0, 0 } ).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% @doc Constructs the US-Web logger for the specified host.
%
% A web analytics tool will be used iff MaybeBinLogAnalysisToolPath is defined
% (i.e. not set to 'undefined').
%
-spec construct( wooper:state(), vhost_id(), domain_id(), bin_directory_path(),
	maybe( scheduler_pid() ), maybe( web_analysis_info() ) ) -> wooper:state().
construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
		   MaybeWebAnalysisInfo ) ->
	construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
		?default_dhms_log_rotation_period, MaybeWebAnalysisInfo,
		_IsSingleton=false ).



% @doc Constructs the US-Web logger (possibly a singleton) for the specified
% host in the specified domain, using specified directory to write access and
% error log, and any specified scheduler and period for log rotation.
%
% A web analytics tool will be used iff MaybeBinLogAnalysisToolPath is defined
% (i.e. not set to 'undefined').
%
-spec construct( wooper:state(), vhost_id(), domain_id(), bin_directory_path(),
				 maybe( scheduler_pid() ), user_period(),
				 maybe( web_analysis_info() ), boolean() ) -> wooper:state().
construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
		   UserPeriod, MaybeWebAnalysisInfo, _IsSingleton=true ) ->

	% Relies first on the next, main constructor:
	InitState = construct( State, BinHostId, DomainId, BinLogDir,
		MaybeSchedulerPid, UserPeriod, MaybeWebAnalysisInfo, false ),

	% Then self-registering:
	RegName = ?registration_name,
	RegScope = ?registration_scope,

	naming_utils:register_as( RegName, RegScope ),

	setAttributes( InitState, [ { registration_name, RegName },
								{ registration_scope, RegScope } ] );


construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
		   UserPeriod, MaybeWebAnalysisInfo, _IsSingleton=false ) ->

	% Auto-registering if a scheduler has been specified (otherwise a task ring
	% is expected to declare a registration); go for maximum interleaving:
	%
	case MaybeSchedulerPid of

		undefined ->
			ok;

		SchedPid ->
			SchedPid ! { registerTask, [ _TaskCmd=rotateLogs,
				_StartTime=flexible, UserPeriod, _Count=unlimited,
				_ActPid=self() ], self() }

	end,

	ServerName = text_utils:format( "Logger for ~ts",
					[ get_host_description( BinHostId, DomainId ) ] ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State,
							?trace_categorize(ServerName), _TrapExits=true ),

	{ BinAccessLogFilename, BinErrorLogFilename } =
		get_log_paths( BinHostId, DomainId ),

	{ BinAccessLogFilePath, BinErrorLogFilePath } =
		{ file_utils:join( BinLogDir, BinAccessLogFilename ),
		  file_utils:join( BinLogDir, BinErrorLogFilename ) },

	{ MaybeLogUpdateCmd, MaybeLogReportCmd } = case MaybeWebAnalysisInfo of

		undefined ->
			{ undefined, undefined };

		% Note that both tool_path and helper_path deal with the generation of
		% HTML reports (reflecting the current state of the Awstats database),
		% not the update of that database. So currently this database is *never*
		% updated (fixme).
		%
		#web_analysis_info{ tool=ToolName=awstats,

							% Typically XXX/awstats_buildstaticpages.pl:
							update_tool_path=BinUpdateToolPath,

							% Typically YYY/awstats.pl:
							report_tool_path=BinReportToolPath,

							conf_dir=BinConfDir,
							web_content_dir=BinWbContDir } ->

			% Checks that the targeted configuration file already exists:
			ConfPath = file_utils:join( BinConfDir,
					get_conf_filename_for( DomainId, BinHostId, ToolName ) ),

			file_utils:is_existing_file_or_link( ConfPath ) orelse
				% Expected to have been generated beforehand (typically by the
				% US-Web configuration server):
				%
				throw( { log_analysis_conf_file_not_found, ConfPath,
						 ToolName } ),

			% Let's prepare once for all the command to run for log analysis:

			% Niceness values range from -20 (most favorable to the process) to
			% 19 (least favorable to the process), 0 being the default; so here
			% we go for a quite low - yet not absolute lowest - priority:
			%
			Niceness = 15,

			% To designate the same filename as get_conf_filename_for/3:
			HostCfgDesc = get_host_description_for( BinHostId, DomainId,
													ToolName ),

			% Returning said command to (only) update database:

			% Additional possible commands:
			ExtraOpts = "-showsteps -showcorrupted -showdropped "
				"-showunknownorigin -showdirectorigin ",

			% Unsilence if debugging:
			RedirectCmd = cond_utils:if_defined( us_web_debug_log_analysis,
								"", " 1>/dev/null" ),

			UpdateCmd = text_utils:format( "nice -n ~B ~ts " ++ ExtraOpts
				++ "-config=~ts" ++ RedirectCmd,
				[ Niceness, BinUpdateToolPath, HostCfgDesc ] ),

			% Command to generate reports:
			%
			% (normal outputs such as "Build alldomains page: [...] and al of no
			% real interest, thus not wanted)
			%
			% Note that this command does not include '-update' anymore, it just
			% generates (all) reports:
			%
			ReportCmd = text_utils:format( "nice -n ~B ~ts -config=~ts "
				"-dir=~ts -awstatsprog=~ts" ++ RedirectCmd,
				[ Niceness, BinReportToolPath, HostCfgDesc, BinWbContDir,
				  BinUpdateToolPath ] ),

			{ UpdateCmd, ReportCmd }

	end,

	% Some needed now:
	ToolState = setAttributes( TraceState, [
		{ vhost_id, BinHostId },
		{ domain_id, DomainId },
		{ log_dir, BinLogDir },
		{ access_log_file_path, BinAccessLogFilePath },
		{ error_log_file_path, BinErrorLogFilePath },
		{ scheduler_pid, MaybeSchedulerPid },
		{ log_task_id, undefined },
		{ rotation_count, 1 },
		{ web_analysis_info, MaybeWebAnalysisInfo },
		{ log_analysis_update_cmd, MaybeLogUpdateCmd },
		{ log_analysis_report_gen_cmd, MaybeLogReportCmd } ] ),

	InitialRotationCount = 1,

	% Now taking into account any past, not yet rotated log file, even though it
	% may create a delay and a resource spike at start-up (as web loggers are
	% created mostly in parallel, as asynchronously - spawning them
	% synchronously would result in too long start-up delays).
	%
	FirstRotCount = case file_utils:is_existing_file_or_link(
							BinAccessLogFilePath ) of

		true ->
			% Processes it and then removes it:
			rotate_access_log_file( InitialRotationCount, ToolState ),
			InitialRotationCount+1;

		false ->
			InitialRotationCount

	end,

	AccessFile = create_log_file( BinAccessLogFilePath, ToolState ),


	SecondRotCount = case file_utils:is_existing_file_or_link(
							BinErrorLogFilePath ) of

		true ->
			% Processes it and then removes it:
			rotate_basic_log_file( BinErrorLogFilePath, InitialRotationCount,
								   ToolState ),
			InitialRotationCount+1;

		false ->
			FirstRotCount

	end,

	ErrorFile = create_log_file( BinErrorLogFilePath, ToolState ),


	MaybeLogTaskId = case MaybeSchedulerPid of

		undefined ->
			undefined;

		_ ->
			% In answer to the call to any registerTask/6 request:
			receive

				{ wooper_result, { task_registered, TId } } ->
					TId

			end

	end,

	ReadyState = setAttributes( ToolState, [
		{ access_log_file, AccessFile },
		{ error_log_file, ErrorFile },
		{ log_task_id, MaybeLogTaskId },
		{ rotation_count, SecondRotCount } ] ),

	?send_info( ReadyState, "Constructed: " ++ to_string( ReadyState ) ),

	ReadyState.



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Unregistering only from any scheduler, not from any task ring, as we
	% registered to any former and have been registered to any latter.

	MaybeSchedPid = ?getAttr(scheduler_pid),

	LogTaskId = ?getAttr(log_task_id),

	case MaybeSchedPid of

		undefined ->
			?debug( "Being destructed, performing a last rotation of the "
					"log files." );

		_ ->
			?debug( "Being destructed, unregistering from scheduler and "
					"performing a last rotation of the log files." ),

			% Any extra schedule trigger sent will be lost; not a problem as it
			% is a oneway:
			%
			MaybeSchedPid ! { unregisterTask, [ LogTaskId ], self() }

	end,

	% Last rotation before closing (first close these files, which will not be
	% reopened; prefer avoiding throwing from destructor):
	%
	[ file_utils:close( ?getAttr(FAttr), _FailureMode=overcome_failure )
		|| FAttr <- [ access_log_file, error_log_file ] ],

	ClosedState = setAttributes( State, [ { access_log_file, undefined },
										  { error_log_file, undefined } ] ),

	rotate_log_files( ClosedState ),

	MaybeSchedPid =:= undefined orelse
		receive

			task_unregistered ->
				ok;

			{ task_unregistration_failed, Error } ->
				?error_fmt( "Unregistration of task #~B failed "
							"at deletion: ~p.", [ LogTaskId, Error ] )

		end,

	?info( "Deleted." ),

	ClosedState.




% Method section.


% @doc Reports an access to a web content; typically sent by a web handler when
% sending content back to a browser having emitted a request.
%
-spec reportAccess( wooper:state(), log_line() ) -> const_oneway_return().
reportAccess( State, BinLogLine ) ->

	cond_utils:if_defined( us_web_debug_log_analysis,
		?debug_fmt( "Storing access log '~ts'.", [ BinLogLine ] ) ),

	file_utils:write( ?getAttr(access_log_file), BinLogLine ),

	wooper:const_return().



% @doc Reports a web error while accessing to a web content; typically sent by a
% web handler dealing with an issue related to a browser having emitted a
% request.
%
-spec reportError( wooper:state(), log_line() ) -> const_oneway_return().
reportError( State, BinLogLine ) ->

	?error( text_utils:binary_to_string( BinLogLine ) ),

	file_utils:write( ?getAttr(error_log_file), BinLogLine ),

	wooper:const_return().



% @doc Rotates asynchronously the managed web logs.
%
% Defined to be triggered as an asynchronous task, typically by a scheduler.
-spec rotateLogs( wooper:state() ) -> oneway_return().
rotateLogs( State ) ->

	RotateState = rotate_logs( State ),

	wooper:return_state( RotateState ).


% @doc Rotates synchronously the managed web logs, as a request.
%
% Defined to be triggered synchronously.
-spec rotateLogsSync( wooper:state() ) ->
						request_return( fallible( 'log_rotated' ) ).
rotateLogsSync( State ) ->

	RotateState = rotate_logs( State ),

	wooper:return_state_result( RotateState, log_rotated ).


% @doc Rotates synchronously the managed web logs, as a oneway call.
%
% Defined to be triggered as a synchronous task, typically by a task ring.
%
% Not synchronised as a request, but alternatively by sending back a oneway
% call.
%
-spec rotateLogsAltSync( wooper:state(), pid() ) -> oneway_return().
rotateLogsAltSync( State, CalledPid ) ->

	RotateState = rotate_logs( State ),

	% Oneway callback:
	CalledPid ! { notifyTaskDone, self() },

	wooper:return_state( RotateState ).



% @doc enerates the report for the managed host, based on the current database
% state (asynchronous version).
%
-spec generateReport( wooper:state() ) -> const_oneway_return().
generateReport( State ) ->

	_Res = generate_report( State ),

	wooper:const_return().



% @doc Generates the report for the managed host, based on the current database
% state (synchronous version).
%
-spec generateReportSync( wooper:state() ) ->
					const_request_return( fallible( 'report_generated' ) ).
generateReportSync( State ) ->

	Res = generate_report( State ),

	wooper:const_return_result( Res ).



% @doc Rotates the logs then generates the corresponding report.
%
% Both intentionally serialised to ensure that no concurrent access can
% interfere.
%
-spec rotateThenGenerateReportSync( wooper:state() ) ->
					const_request_return( fallible( 'report_generated' ) ).
rotateThenGenerateReportSync( State ) ->

	RotateState = rotate_logs( State ),

	Res = generate_report( RotateState ),

	wooper:const_return_result( Res ).



% (helper, for re-use)
-spec generate_report( wooper:state() ) -> fallible( 'report_generated' ).
generate_report( State ) ->

	case ?getAttr(log_analysis_report_gen_cmd) of

		undefined ->
			?error( "Generation of report request, yet no corresponding "
					"command was defined." ),
			{ error, no_report_command };

		% Supposing Awstats here:
		LogReportGenCmd ->

			?debug_fmt( "Generating HTML Awstats access report based on "
						"command '~ts'.", [ LogReportGenCmd ] ),

			%?debug_fmt( "Generating HTML access report in '~ts' with Awstats.",
			%			[ HTMLReportPath ] ),

			%file_utils:remove_file_if_existing( HTMLReportPath ),

			% We trigger the generation of the corresponding HTML pages.
			%
			% First, the main HTML report:

			%CfgDesc = get_host_description_for( ?getAttr(vhost_id),
			%                                    ?getAttr(domain_id), awstats ),

			% Note: awstats_buildstaticpages.pl is now preferred to the
			% following, less efficient, multi-exec approach.

			% No more '-update':
			%Cmd = text_utils:format(
			%  "nice -n ~B ~ts -config=~ts -output -staticlinks > ~ts",
			%  [ Niceness, BinToolPath, CfgDesc, HTMLReportPath ] ),
			%
			%case system_utils:run_command( Cmd ) of
			%
			%	{ _ReturnCode=0, _CmdOutput="" } ->
			%		ok;
			%
			%	{ _ReturnCode=0, CmdOutput } ->
			%		?warning_fmt( "Awstats main report generation succeeded, "
			%           "yet returned following message: '~ts'.",
			%           [ CmdOutput ] );
			%
			%	% An error here shall not kill this logger as a whole:
			%	{ ReturnCode, CmdOutput } ->
			%		?error_fmt( "Awstats main report generation failed "
			%           "(return code: ~w), and returned following message: "
			%           "'~ts' (command was: '~ts').",
			%			[ ReturnCode, CmdOutput, Cmd ] )
			%
			%end,

			% Then create also the other report pages:
			% (list via /usr/share/webapps/awstats/cgi-bin/awstats.pl -h)
			%
			% (not retaining mail logs and matching filters)
			%
			%ReportTypes = [ "alldomains", "allhosts", "lasthosts", "unknownip",
			%                "alllogins", "lastlogins", "allrobots",
			%                "lastrobots", "urldetail", "urlentry", "urlexit",
			%                "osdetail", "browserdetail", "unknownbrowser",
			%                "unknownos", "refererse", "refererpages",
			%                "keyphrases", "keywords", "errors404" ],
			%
			% Targeting ultimately for example
			% "awstats.foo.bar.org.browserdetail.html":
			%
			%BaseGenFile = file_utils:join( ?getAttr(web_content_dir),
			%                               "awstats" ),
			%
			% No more '-update':
			%CmdFormatString = text_utils:format( "nice -n ~B ~ts -config=~ts "
			%   "-output=~~ts -staticlinks > ~ts.~ts.~~ts.html",
			%   [ Niceness, BinToolPath, CfgDesc, BaseGenFile, CfgDesc ] ),
			%
			%generate_other_report_pages( ReportTypes, CmdFormatString, State )

			% Newer approach builds all report pages in one go:
			case system_utils:run_command( LogReportGenCmd ) of

				{ _ReturnCode=0, _CmdOutput="" } ->
					report_generated;

				{ _ReturnCode=0, CmdOutput } ->
					?warning_fmt( "Awstats HTML report generation "
						"succeeded, yet returned following message: '~ts'.",
						[ CmdOutput ] ),
					report_generated;

				% An error here shall not kill this logger as a whole:
				{ ReturnCode, CmdOutput } ->
					?error_fmt( "Awstats HTML report generation failed "
						"(return code: ~w), and returned following "
						"message: '~ts' (command was: '~ts').",
						[ ReturnCode, CmdOutput, LogReportGenCmd ] ),
					{ error, CmdOutput }

			end

	end.





% @doc Callback triggered whenever a linked process stops.
-spec onWOOPERExitReceived( wooper:state(), pid(),
						basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, _StopPid, _ExitType=normal ) ->

	% Not even a trace sent for that, as running a log report tool will trigger
	% a normal exit for three ports (such as #Port<0.119>), probably one input,
	% one normal output and one error output channel.
	%
	%?notice_fmt( "Ignoring normal exit from process ~w.", [ StopPid ] ),

	wooper:const_return();

onWOOPERExitReceived( State, CrashedPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%   {wooper_oneway_failed,<0.44.0>,class_XXX,
	%      FunName,Arity,Args,AtomCause}}, [...]}"

	?error_fmt( "Received and ignored an exit message from ~w:~n  ~p",
				[ CrashedPid, ExitType ] ),

	wooper:const_return().





% Helper section.


% @doc Creates the relevant log files, supposed not already opened nor even
% existing.
%
-spec create_log_files( wooper:state() ) -> wooper:state().
create_log_files( State ) ->

	[ basic_utils:check_undefined( ?getAttr(FAttr) )
		|| FAttr <- [ access_log_file, error_log_file ] ],

	AccessFile = create_log_file( ?getAttr(access_log_file_path), State ),
	ErrorFile = create_log_file( ?getAttr(error_log_file_path), State ),

	setAttributes( State, [ { access_log_file, AccessFile },
							{ error_log_file, ErrorFile } ] ).


% (generic helper)
-spec create_log_file( bin_file_path(), wooper:state() ) -> file().
create_log_file( BinLogFilePath, State ) ->

	% Not supposed to be still there in nominal conditions:
	file_utils:is_existing_file_or_link( BinLogFilePath ) andalso
		begin
			% Used to be deemed better than rotating an unknown/unexpected file:
			%
			% (would deserve a warning, yet almost always happens at start-up)
			%
			?info_fmt( "Removing a prior '~ts' log file.", [ BinLogFilePath ] ),

			file_utils:remove_file( BinLogFilePath )
		end,

	%?debug_fmt( "Creating log file '~ts'.", [ BinLogFilePath ] ),

	file_utils:open( BinLogFilePath,
					 _Opts=[ write, exclusive, raw, delayed_write ] ).



% @doc Performs the log rotation.
%
% Overall helper introduced for code sharing between synchronous and
% asynchronous ones.
%
-spec rotate_logs( wooper:state() ) -> wooper:state().
rotate_logs( State ) ->

	%?debug( "Log rotation just triggered." ),

	ClosedState = close_log_files( State ),

	{ MaybeAccessCompressedFilePath, MaybeErrorCompressedFilePath, RotState } =
		rotate_log_files( ClosedState ),

	?info_fmt( "Log rotation: ~ts, ~ts.", [
		case MaybeAccessCompressedFilePath of

			undefined ->
				"no access log to archive";

			_ ->
				text_utils:format( "access logs archived in '~ts'",
								   [ MaybeAccessCompressedFilePath ] )

		end,
		case MaybeErrorCompressedFilePath of

			undefined ->
				"no error log to archive";

			_ ->
				text_utils:format( "error logs archived in '~ts'",
								   [ MaybeErrorCompressedFilePath ] )

		end ] ),

	% Reopening them now:
	create_log_files( RotState ).



% @doc Rotates the relevant log files, supposed already closed (and does not
% reopen them, as this is not wanted in all cases).
%
-spec rotate_log_files( wooper:state() ) ->
			{ maybe( file_path() ), maybe( file_path() ), wooper:state() }.
rotate_log_files( State ) ->

	[ basic_utils:check_undefined( ?getAttr(FAttr) )
			|| FAttr <- [ access_log_file, error_log_file ] ],

	RotCount = ?getAttr(rotation_count),

	MaybeAccessArchivePath = rotate_access_log_file( RotCount, State ),

	BinErrorLogFilePath = ?getAttr(error_log_file_path),

	MaybeErrorArchivePath = case file_utils:get_size( BinErrorLogFilePath ) of

		0 ->
			?debug_fmt( "Not rotating empty error log file '~ts'.",
						[ BinErrorLogFilePath ] ),
			undefined;

		_ ->
			?debug_fmt( "Rotating error log file '~ts'.",
						[ BinErrorLogFilePath ] ),
			rotate_basic_log_file( BinErrorLogFilePath, RotCount, State )

	end,

	RotState = setAttribute( State, rotation_count, RotCount+1 ),

	{ MaybeAccessArchivePath, MaybeErrorArchivePath, RotState }.



% @doc Closes the relevant log files, supposed already opened.
-spec close_log_files( wooper:state() ) -> wooper:state().
close_log_files( State ) ->

	% To avoid delayed errors such as {einval,
	% {file_descriptor,raw_file_io_delayed, etc.:
	%
	[ file_utils:close( ?getAttr(FAttr), _FailureMode=overcome_failure )
		|| FAttr <- [ access_log_file, error_log_file ] ],

	setAttributes( State, [ { access_log_file, undefined },
							{ error_log_file, undefined } ] ).



% @doc Returns a human-readable description of designated host.
-spec get_host_description( vhost_id(), domain_id() ) -> ustring().
get_host_description( _BinHostId=default_vhost_catch_all,
					  _DomainId=default_domain_catch_all ) ->
	"*.*";

get_host_description( _BinHostId=default_vhost_catch_all, DomainId ) ->
	text_utils:format( "*.~ts", [ DomainId ] );

get_host_description( BinHostname, _DomainId=default_domain_catch_all ) ->
	text_utils:format( "~ts.*", [ BinHostname ] );

get_host_description( BinHostname, DomainId ) ->
	text_utils:format( "~ts.~ts", [ BinHostname, DomainId ] ).



% @doc Returns a description of designated host for specified tool.
-spec get_host_description_for( vhost_id(), domain_id(),
								log_analysis_tool() ) -> ustring().
get_host_description_for( BinHostId, DomainId, _Tool=awstats ) ->
	% Includes default_vhost_catch_all and/or default_domain_catch_all:
	text_utils:format( "~ts.~ts", [ BinHostId, DomainId ] ).



% Static section.


% @doc Returns the default period of log rotation.
-spec get_default_log_rotation_period() ->
							static_return( time_utils:dhms_duration() ).
get_default_log_rotation_period() ->
	wooper:return_static( ?default_dhms_log_rotation_period ).



% @doc Returns the actual access and error filenames corresponding to the
% specified host.
%
-spec get_log_paths( vhost_id(), domain_id() ) ->
					static_return( { bin_file_name(), bin_file_name() } ).
get_log_paths( _BinHostId=default_vhost_catch_all, DomainId ) ->

	DomainString = get_domain_description( DomainId ),

	BinAccess = text_utils:bin_format( "access-vhost-catchall-for-~ts.log",
									   [ DomainString ] ),

	BinError = text_utils:bin_format( "error-vhost-catchall-for-~ts.log",
									  [ DomainString ] ),

	wooper:return_static( { BinAccess, BinError } );


get_log_paths( BinHostId, DomainId ) ->

	DomainString = get_domain_description( DomainId ),

	BinAccess = text_utils:bin_format( "access-for-~ts.~ts.log",
									   [ BinHostId, DomainString ] ),

	BinError = text_utils:bin_format( "error-for-~ts.~ts.log",
									  [ BinHostId, DomainString ] ),

	wooper:return_static( { BinAccess, BinError } ).



% @doc Returns a description about specified domain.
-spec get_domain_description( domain_id() ) -> ustring().
get_domain_description( _DomainId=default_domain_catch_all ) ->
	"domain-catchall";

get_domain_description( DomainName ) ->
	DomainName.



% @doc Rotates the access file, which should exist in all cases yet not be
% currently open (it should have been closed and its handle forgotten beforehand
% if necessary); it will not be reopened here.
%
% Notes: operates only on access logs, not error ones, as web analyzers do not
% care about the latters, whose processing is thus more direct.
%
-spec rotate_access_log_file( count(), wooper:state() ) -> maybe( file_path() ).
rotate_access_log_file( RotCount, State ) ->

	BinFilePath = ?getAttr(access_log_file_path),

	case file_utils:get_size( BinFilePath ) of

		0 ->
			?debug_fmt( "Not rotating empty access log file '~ts'.",
						[ BinFilePath ] ),
			undefined;

		_ ->
			?debug_fmt( "Rotating access log file '~ts'.", [ BinFilePath ] ),

			case ?getAttr(log_analysis_update_cmd) of

				undefined ->
					ok;

				% Supposing Awstats here:
				LogAnalysisUpdateCmd ->

					?debug_fmt( "Updating Awstats database now based "
								"on command '~ts'.", [ LogAnalysisUpdateCmd ] ),

					case system_utils:run_command( LogAnalysisUpdateCmd ) of

						{ _ReturnCode=0, _CmdOutput="" } ->
							ok;

						{ _ReturnCode=0, CmdOutput } ->
							?warning_fmt( "Awstats database update prior to "
								"log rotation succeeded for main report, "
								"yet returned following message: '~ts'.",
								[ CmdOutput ] );

						% An error here shall not kill this logger as a whole:
						{ ReturnCode, CmdOutput } ->
							?error_fmt( "Awstats database update prior to "
								"log rotation failed (return code: ~w), and "
								"returned following message: '~ts' (command "
								"was: '~ts').", [ ReturnCode, CmdOutput,
												  LogAnalysisUpdateCmd ] )

					end

			end,
			rotate_basic_log_file( BinFilePath, RotCount, State )

	end.



% @doc Generates the Awstats auxiliary (non-main, yet referenced by it) HTML
% report pages.
%
-spec generate_other_report_pages( [ ustring() ], ustring(),
								   wooper:state() ) -> void().
generate_other_report_pages( _ReportTypes=[], _CmdFormatString, _State ) ->
	ok;

generate_other_report_pages( _ReportTypes=[ ReportType | T ], CmdFormatString,
							 State ) ->

	Cmd = text_utils:format( CmdFormatString, [ ReportType, ReportType ] ),

	case system_utils:run_command( Cmd ) of

		{ _ReturnCode=0, _CmdOutput="" } ->
			?debug_fmt( "Generated report type '~ts' (command: '~ts').",
						[ ReportType, Cmd ] ),
			ok;

		{ _ReturnCode=0, CmdOutput } ->
			?warning_fmt( "Awstats execution prior to log rotation "
				"succeeded for report type '~ts', yet returned following "
				"message: '~ts'.", [ ReportType, CmdOutput ] );

		% An error here shall not kill this logger as a whole:
		{ ReturnCode, CmdOutput } ->
			?error_fmt( "Awstats execution prior to log rotation failed "
				"(return code: ~w) for report type '~ts', and returned "
				"following message: '~ts' (command was: '~ts').",
				[ ReturnCode, ReportType, CmdOutput, Cmd ] )

	end,

	generate_other_report_pages( T, CmdFormatString, State ).



% @doc Rotates any basic log file (typically the access or error one), with no
% specific post-processing.
%
% Returns the path of the resulting archive (ex:
% "/tmp/access-for-baz.foobar.org.log.41.2021-1-20-at-19h-53m-18s.xz").
%
% This file should not be currently open (it should have been closed and its
% handle forgotten beforehand if necessary); and it will not be reopened here.
%
-spec rotate_basic_log_file( file_utils:any_file_path(), count(),
							 wooper:state() ) -> file_path().
rotate_basic_log_file( FilePath, RotCount, _State ) ->

	ArchiveFilePath = text_utils:format( "~ts.~B.~ts", [ FilePath, RotCount,
		time_utils:get_textual_timestamp_for_path() ] ),

	%?debug_fmt( "Rotating '~ts' to a compressed version: '~ts'.",
	%            [ FilePath, ArchiveFilePath ] ),

	file_utils:move_file( FilePath, ArchiveFilePath ),

	CompressedFilePath = file_utils:compress( ArchiveFilePath ),

	file_utils:remove_file( ArchiveFilePath ),

	%?debug_fmt( "Log rotation finished: archive '~ts' generated "
	%   "(original '~ts' removed).", [ CompressedFilePath, FilePath ] ),

	CompressedFilePath.



% @doc Returns the conventional base filename for specified virtual host, in the
% context of specified tool.
%
% Useful both for input files (ex: configuration ones) and (possibly) output
% ones (ex: HTML generated ones).
%
% Much like get_log_paths/2.
%
-spec get_file_prefix_for( domain_id(), vhost_id(), log_analysis_tool() ) ->
								static_return( file_name() ).
get_file_prefix_for( DomainId, VHostId, _Tool=awstats ) ->

	% Works in all cases, including default_vhost_catch_all and/or
	% default_domain_catch_all:
	%
	Filename = text_utils:format( "awstats.~ts.~ts", [ VHostId, DomainId ] ),
	wooper:return_static( Filename );

get_file_prefix_for( _DomainId, _VHostId, Tool ) ->
	throw( { unsupported_web_analysis_tool, Tool } ).



% @doc Returns the name for the configuration file for specified web analysis
% tool, regarding specified virtual host.
%
% Had to be finally special-case for Awstats since version 7.8: no more
% arbitrary absolute paths for Awstats configuration files can be used, relying
% now on a fixed name in a fixed directory.
%
% We follow our conventions, resulting in having potentially
% default_domain_catch_all and/or default_vhost_catch_all in returned filename,
% as we need anyway a marker to denote wildcards ('*.foobar.org' could result in
% awstats.foobar.org.conf, yet we prefer '*' not to result inunclear, ambiguous
% awstats.conf .
%
-spec get_conf_filename_for( domain_id(), vhost_id(), log_analysis_tool() ) ->
								static_return( file_name() ).
get_conf_filename_for( DomainId, VHostId, _Tool=awstats ) ->

	Filename = text_utils:format( "awstats.~ts.conf",
				[ get_host_description_for( VHostId, DomainId, awstats ) ] ),

	wooper:return_static( Filename );

get_conf_filename_for( _DomainId, _VHostId, Tool ) ->
	throw( { unsupported_web_analysis_tool, Tool } ).



% @doc Returns a textual description of this server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	HostDesc = get_host_description( ?getAttr(vhost_id), ?getAttr(domain_id) ),

	ConfString = case ?getAttr(web_analysis_info) of

		undefined ->
			"no web analysis enabled";

		#web_analysis_info{ tool=ToolName,
							update_tool_path=BinUpdateToolPath,
							report_tool_path=BinReportToolPath,
							conf_dir=BinConfDir,
							web_content_dir=BinWbContDir } ->
			text_utils:format( "web analysis enabled, based on '~ts' "
				"(update path: '~ts', report generation path: '~ts'), with, as "
				"configuration directory '~ts', and as web content "
				"directory '~ts'",
				[ ToolName, BinUpdateToolPath, BinReportToolPath, BinConfDir,
				  BinWbContDir ] )

	end,

	text_utils:format( "US-Web logger for '~ts', using log directory '~ts' "
		"(writing there '~ts' and '~ts'); ~ts", [ HostDesc, ?getAttr(log_dir),
		?getAttr(access_log_file_path), ?getAttr(error_log_file_path),
		ConfString ] ).
