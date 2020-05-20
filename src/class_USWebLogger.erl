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
% Creation date: Tuesday, December 31, 2019.


-module(class_USWebLogger).


-define( class_description,
		 "Server in charge of the logging for the US-web framework." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).



% Logging service for the US-web activities.
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
% us_web/_build/default/rel/us_web/traces_via_otp.traces)

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


-type server_pid() :: class_UniversalServer:server_pid().

% A log line entry:
-type log_line() :: text_utils:bin_string().


-export_type([ server_pid/0, log_line/0 ]).


% To silence unused warnings:
-export([ generate_other_report_pages/3 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type bin_directory_path() :: file_utils:bin_directory_path().
-type bin_file_name() :: file_utils:bin_file_name().

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type file() :: file_utils:file().

-type domain_id() :: class_USWebConfigServer:domain_id().
-type vhost_id() :: class_USWebConfigServer:vhost_id().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
%-type task_id() :: c2149lass_USScheduler:task_id().
-type user_periodicity() :: class_USScheduler:user_periodicity().

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
	  "the US-web configuration directory, where notably the per-host "
	  "configuration files for log analysis tools are defined" },

	% Not used anymore, as having to rely on Awstats lookup scheme:
	%{ analysis_conf_path, text_utils:bin_string(),
	%  "the path to the configuration file (typically for Awstats) for the "
	%  "corresponding host" },

	{ access_log_file_path, bin_file_path(),
	  "the (absolute) path of the (current) access log file being written" },

	{ access_log_file, file_utils:file(),
	  "the handle of the actual access file being written" },


	{ error_log_file_path, bin_file_name(),
	  "the (absolute) path of the (current) error log file being written" },

	{ error_log_file, file_utils:file(),
	  "the handle of the actual error file being written" },


	{ scheduler_pid, maybe( scheduler_pid() ),
	  "the PID of any scheduler used by this logger; otherwise that logger is "
	  "expected to be driven by a task ring" },


	{ log_task_id, maybe( task_id() ),
	  "the identifier of this task for log rotation, as assigned by the "
	  "scheduler (if any)" },

	% In log_analysis_command:
	%{ analysis_tool_path, maybe( bin_executable_path() ),
	%  "a path (if any) to the web log analysis tool (expected to have been "
	%  "validated beforehand)" },

	{ web_analysis_info, maybe( web_analysis_info() ),
	  "settings about web analysis (if any)" },

	{ log_analysis_command, maybe( ustring() ), "the prebuilt command to "
	  "run in order to generate the log analysis report" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Configuration.USWebLogger" ).


-define( registration_name, us_web_logger ).

-define( registration_scope, global_only ).



% The default DHMS periodicity at which log rotation will be requested by a
% given logger instance.

% Every 4 minutes (for testing):
%-define( default_dhms_log_rotation_periodicity, { 0, 0, 4, 0 } ).

% Every 2 days:
-define( default_dhms_log_rotation_periodicity, { 2, 0, 0, 0 } ).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Constructs the US web logger for the specified host.
%
% A web analytics tool will be used iff MaybeBinLogAnalysisToolPath is defined
% (i.e. not set to 'undefined').
%
-spec construct( wooper:state(), vhost_id(), domain_id(), bin_directory_path(),
				 maybe( scheduler_pid() ), maybe( web_analysis_info() ) ) ->
					   wooper:state().
construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
		   MaybeWebAnalysisInfo ) ->
	construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
			   ?default_dhms_log_rotation_periodicity, MaybeWebAnalysisInfo,
			   _IsSingleton=false ).



% Constructs the US web logger (possibly a singleton) for the specified host in
% the specified domain, using specified directory to write access and error log,
% and any specified scheduler and periodicity for log rotation.
%
% A web analytics tool will be used iff MaybeBinLogAnalysisToolPath is defined
% (i.e. not set to 'undefined').
%
-spec construct( wooper:state(), vhost_id(), domain_id(), bin_directory_path(),
				 maybe( scheduler_pid() ), user_periodicity(),
				 maybe( web_analysis_info() ), boolean() ) -> wooper:state().
construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
		   UserPeriodicity, MaybeWebAnalysisInfo, _IsSingleton=true ) ->

	InitState = construct( State, BinHostId, DomainId, BinLogDir,
		   MaybeSchedulerPid, UserPeriodicity, MaybeWebAnalysisInfo, false ),

	% Then self-registering:
	RegName = ?registration_name,
	RegScope = ?registration_scope,

	naming_utils:register_as( RegName, RegScope ),

	setAttributes( InitState, [ { registration_name, RegName },
								{ registration_scope, RegScope } ] );


construct( State, BinHostId, DomainId, BinLogDir, MaybeSchedulerPid,
		   UserPeriodicity, MaybeWebAnalysisInfo, _IsSingleton=false ) ->

	% Auto-registering if a scheduler has been specified (otherwise a task ring
	% is expected to declare a registration); go for maximum interleaving:
	%
	case MaybeSchedulerPid of

		undefined ->
			ok;

		SchedPid ->
			 SchedPid ! { registerTask, [ _TaskCmd=rotateLogs,
				_StartTime=flexible, UserPeriodicity, _Count=unlimited,
				_ActPid=self() ], self() }

	end,

	ServerName = text_utils:format( "Logger for ~s",
					[ get_host_description( BinHostId, DomainId ) ] ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State,
										   ?trace_categorize(ServerName) ),

	%FilePair = { BinAccessLogFilename, BinErrorLogFilename } =
	%	get_log_paths( BinHostId, DomainId, BinLogDir ),

	{ BinAccessLogFilename, BinErrorLogFilename } =
		get_log_paths( BinHostId, DomainId ),

	{ BinAccessLogFilePath, BinErrorLogFilePath } =
		{ file_utils:join( BinLogDir, BinAccessLogFilename ),
		  file_utils:join( BinLogDir, BinErrorLogFilename ) },

	MaybeLogAnalysisCmd = case MaybeWebAnalysisInfo of

		undefined ->
			undefined;

		#web_analysis_info{ tool=ToolName=awstats,
							tool_path=BinToolPath,
							helper_path=BinHelperPath,
							conf_dir=BinConfDir,
							web_content_dir=BinWbContDir } ->

			% Checks that the targeted configuration file already exists:
			ConfPath = file_utils:join( BinConfDir,
					get_conf_filename_for( DomainId, BinHostId, ToolName ) ),

			case file_utils:is_existing_file_or_link( ConfPath ) of

				true ->
					ok;

				false ->
					% Expected to have been generated beforehand (typically by
					% the US-web configuration server):
					%
					throw( { log_analysis_conf_file_not_found, ConfPath,
							 ToolName } )

			end,

			% Let's prepare once for all the command to run for log analysis:

			% Niceness values range from -20 (most favorable to the process) to
			% 19 (least favorable to the process), 0 being the default; so here
			% we go for a quite low - yet not absolute lowest - priority:
			%
			Niceness = 15,

			% To designate the same filename as get_conf_filename_for/3:
			HostCfgDesc = get_host_description_for( BinHostId, DomainId,
													ToolName ),

			% Returning said command:
			%
			% (normal outputs such as "Build alldomains page: [...] and al of no
			% real interest, thus not wanted)
			%
			text_utils:format( "nice -n ~B ~s -update -config=~s -dir=~s "
							   "-awstatsprog=~s 1>/dev/null",
							   [ Niceness, BinToolPath, HostCfgDesc,
									 BinWbContDir, BinHelperPath ] )

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
		{ web_analysis_info, MaybeWebAnalysisInfo },
		{ log_analysis_command, MaybeLogAnalysisCmd } ] ),


	case file_utils:is_existing_file_or_link( BinAccessLogFilePath ) of

		true ->
			% Processes it and then removes it:
			rotate_access_log_file( ToolState );

		false ->
			ok

	end,

	AccessFile = create_log_file( BinAccessLogFilePath, ToolState ),


	case file_utils:is_existing_file_or_link( BinErrorLogFilePath ) of

		true ->
			% Processes it and then removes it:
			rotate_basic_log_file( BinErrorLogFilePath, ToolState );

		false ->
			ok

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
		{ log_task_id, MaybeLogTaskId } ] ),

	?send_info( ReadyState, "Just created: " ++ to_string( ReadyState ) ),

	ReadyState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Unregistering only from any scheduler, not from any task ring, as we
	% registered to any former and have been registered to any latter.

	MaybeSchedPid = ?getAttr(scheduler_pid),

	LogTaskId = ?getAttr(log_task_id),

	case MaybeSchedPid of

		undefined ->
			?trace( "Being destructed, performing a last rotation of the "
					"log files." );

		_ ->
			?trace( "Being destructed, unregistering from scheduler and "
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

	case MaybeSchedPid of

		undefined ->
			ok;

		_ ->
			receive

				task_unregistered ->
					ok;

				{ task_unregistration_failed, Error } ->
					?error_fmt( "Unregistration of task #~B failed "
								"at deletion: ~p.", [ LogTaskId, Error ] )

			end

	end,

	?info( "Deleted." ),

	ClosedState.



% Method section.


% Reports access to a web content; typically sent by a web handler when sending
% content back to a browser having sent a request.
%
-spec reportAccess( wooper:state(), log_line() ) -> const_oneway_return().
reportAccess( State, BinLogLine ) ->

	%?debug_fmt( "Storing access log '~s'.", [ BinLogLine ] ),

	file_utils:write( ?getAttr(access_log_file), BinLogLine ),

	wooper:const_return().



% Reports a web error while accessing to a web content; typically sent by a web
% handler dealing with an issue related to a browser having sent a request.
%
-spec reportError( wooper:state(), log_line() ) -> const_oneway_return().
reportError( State, BinLogLine ) ->

	?error( text_utils:binary_to_string( BinLogLine ) ),

	file_utils:write( ?getAttr(error_log_file), BinLogLine ),

	wooper:const_return().



% Defined to be triggered as an asynchronous task, typically by a scheduler.
-spec rotateLogs( wooper:state() ) -> oneway_return().
rotateLogs( State ) ->

	RotateState = rotate_logs( State ),

	wooper:return_state( RotateState ).



% Defined to be triggered as a synchronous task, typically by a task ring.
%
% Not synchronised as a request, but by sending back a oneway call.
%
-spec rotateLogsSynch( wooper:state(), pid() ) -> oneway_return().
rotateLogsSynch( State, CalledPid ) ->

	RotateState = rotate_logs( State ),

	% Oneway callback:
	CalledPid ! { notifyTaskDone, self() },

	wooper:return_state( RotateState ).



% Helper section.


% Creates the relevant log files, supposed not already opened nor even existing.
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
	case file_utils:is_existing_file_or_link( BinLogFilePath ) of

		true ->
			% Used to be deemed better than rotating an unknown/unexpected file:
			%
			% (would deserve a warning, yet almost always happens at start-up)
			%
			?info_fmt( "Removing a prior '~s' log file.",
					   [ BinLogFilePath ] ),
			file_utils:remove_file( BinLogFilePath );

		false ->
			ok

	end,

	%?debug_fmt( "Creating log file '~s'.", [ BinLogFilePath ] ),

	file_utils:open( BinLogFilePath,
					 _Opts=[ write, exclusive, raw, delayed_write ] ).



% Overall helper introduced for code sharing between synchronous and
% asynchronous ones.
%
-spec rotate_logs( wooper:state() ) -> wooper:state().
rotate_logs( State ) ->

	%?debug( "Log rotation just triggered." ),

	ClosedState = close_log_files( State ),

	{ AccessCompressedFilePath, ErrorCompressedFilePath } =
		rotate_log_files( ClosedState ),

	?info_fmt( "Logs rotated, accesses archived in '~s', errors in '~s'.",
			   [ AccessCompressedFilePath, ErrorCompressedFilePath ] ),

	% Reopening them now:
	create_log_files( ClosedState ).



% Rotates the relevant log files, supposed already closed (and does not reopen
% them, as this is not wanted in all cases).
%
-spec rotate_log_files( wooper:state() ) -> { file_path(), file_path() }.
rotate_log_files( State ) ->

	[ basic_utils:check_undefined( ?getAttr(FAttr) )
	  || FAttr <- [ access_log_file, error_log_file ] ],

	AccessArchivePath = rotate_access_log_file( State ),

	ErrorArchivePath = rotate_basic_log_file( ?getAttr(error_log_file_path),
											   State ),

	{ AccessArchivePath, ErrorArchivePath }.



% Closes the relevant log files, supposed already opened.
-spec close_log_files( wooper:state() ) -> wooper:state().
close_log_files( State ) ->

	[ file_utils:close( ?getAttr(FAttr) )
	  || FAttr <- [ access_log_file, error_log_file ] ],

	setAttributes( State, [ { access_log_file, undefined },
							{ error_log_file, undefined } ] ).



% Returns a human-readable description of designated host.
-spec get_host_description( vhost_id(), domain_id() ) -> ustring().
get_host_description( _BinHostId=default_vhost_catch_all,
					  _DomainId=default_domain_catch_all ) ->
	"*.*";

get_host_description( _BinHostId=default_vhost_catch_all, DomainId ) ->
	text_utils:format( "*.~s", [ DomainId ] );

get_host_description( BinHostname, _DomainId=default_domain_catch_all ) ->
	text_utils:format( "~s.*", [ BinHostname ] );

get_host_description( BinHostname, DomainId ) ->
	text_utils:format( "~s.~s", [ BinHostname, DomainId ] ).



% Returns a description of designated host for specified tool.
-spec get_host_description_for( vhost_id(), domain_id(),
								log_analysis_tool() ) -> ustring().
get_host_description_for( BinHostId, DomainId, _Tool=awstats ) ->
	% Includes default_vhost_catch_all and/or default_domain_catch_all:
	text_utils:format( "~s.~s", [ BinHostId, DomainId ] ).



% Static section.


% Returns the default periodicity of log rotation.
-spec get_default_log_rotation_periodicity() ->
							 static_return( time_utils:dhms_duration() ).
get_default_log_rotation_periodicity() ->
	wooper:return_static( ?default_dhms_log_rotation_periodicity ).



% Returns the actual access and error filenames corresponding to the specified
% host.
%
-spec get_log_paths( vhost_id(), domain_id() ) ->
					static_return( { bin_file_name(), bin_file_name() } ).
get_log_paths( _BinHostId=default_vhost_catch_all, DomainId ) ->

	DomainString = get_domain_description( DomainId ),

	BinAccess = text_utils:bin_format( "access-vhost-catchall-for-~s.log",
									   [ DomainString ] ),

	BinError = text_utils:bin_format( "error-vhost-catchall-for-~s.log",
									  [ DomainString ] ),

	wooper:return_static( { BinAccess, BinError } );


get_log_paths( BinHostId, DomainId ) ->

	DomainString = get_domain_description( DomainId ),

	BinAccess = text_utils:bin_format( "access-for-~s.~s.log",
									   [ BinHostId, DomainString ] ),

	BinError = text_utils:bin_format( "error-for-~s.~s.log",
									  [ BinHostId, DomainString ] ),

	wooper:return_static( { BinAccess, BinError } ).



% Returns a description about specified domain.
-spec get_domain_description( domain_id() ) -> ustring().
get_domain_description( _DomainId=default_domain_catch_all ) ->
	"domain-catchall";

get_domain_description( DomainName ) ->
	DomainName.



% Rotates the access file, which should exist in all cases yet not be currently
% open (it should have been closed and its handle forgotten beforehand if
% necessary); it will not be reopened here.
%
% Notes: operates only on access logs, not error ones, as web analyzers do not
% care about the latters.
%
-spec rotate_access_log_file( wooper:state() ) -> bin_file_path().
rotate_access_log_file( State ) ->

	BinFilePath = ?getAttr(access_log_file_path),

	%?debug_fmt( "Rotating access log file '~s'.", [ BinFilePath ] ),

	case ?getAttr(log_analysis_command) of

		undefined ->
			ok;

		% Supposing Awstats here:
		LogAnalysisCmd ->

			%?debug( "Generating HTML access report with Awstats." ),

			?debug_fmt( "Generating HTML Awstats access report based on "
						"command '~s'.", [ LogAnalysisCmd ] ),

			%?debug_fmt( "Generating HTML access report in '~s' with Awstats.",
			%			[ HTMLReportPath ] ),

			%file_utils:remove_file_if_existing( HTMLReportPath ),


			% We trigger the reading of the access logs and the generation of
			% the corresponding HTML page at the same time.
			%
			% First, the main HTML report:

			%CfgDesc = get_host_description_for( ?getAttr(vhost_id),
			%									?getAttr(domain_id), awstats ),

			% Note: awstats_buildstaticpages.pl is preferred to the following,
			% less efficient, multi-exec approach.

			%Cmd = text_utils:format(
			%  "nice -n ~B ~s -config=~s -update -output -staticlinks > ~s",
			%  [ Niceness, BinToolPath, CfgDesc, HTMLReportPath ] ),
			%
			%case system_utils:run_executable( Cmd ) of
			%
			%	{ _ReturnCode=0, _CmdOutput="" } ->
			%		ok;
			%
			%	{ _ReturnCode=0, CmdOutput } ->
			%		?warning_fmt( "Awstats execution prior to log rotation "
			%			"succeeded for main report, yet returned following "
			%			"message: '~s'.",
			%			[ CmdOutput ] );
			%
			%	% An error here shall not kill this logger as a whole:
			%	{ ReturnCode, CmdOutput } ->
			%		?error_fmt( "Awstats execution prior to log rotation "
			%			"failed (return code: ~w) for main report, and "
			%			"returned following message: '~s' "
			%			"(command was: '~s').",
			%			[ ReturnCode, CmdOutput, Cmd ] )
			%
			%end,

			% Then create also the other report pages:
			% (list via /usr/share/webapps/awstats/cgi-bin/awstats.pl -h)
			%
			% (not retaining mail logs and matching filters)
			%
			%ReportTypes = [ "alldomains", "allhosts", "lasthosts", "unknownip",
			%				"alllogins", "lastlogins", "allrobots",
			%				"lastrobots", "urldetail", "urlentry", "urlexit",
			%				"osdetail", "browserdetail", "unknownbrowser",
			%				"unknownos", "refererse", "refererpages",
			%				"keyphrases", "keywords", "errors404" ],
			%
			% Targeting ultimately for example
			% "awstats.foo.bar.org.browserdetail.html":
			%
			%BaseGenFile = file_utils:join( ?getAttr(web_content_dir),
			%							   "awstats" ),
			%
			%CmdFormatString = text_utils:format( "nice -n ~B ~s -config=~s "
			%	"-update -output=~~s -staticlinks > ~s.~s.~~s.html",
			%	[ Niceness, BinToolPath, CfgDesc, BaseGenFile, CfgDesc ] ),
			%
			%generate_other_report_pages( ReportTypes, CmdFormatString, State )

			% Newer approach builds all report pages in one go:
			case system_utils:run_executable( LogAnalysisCmd ) of

				{ _ReturnCode=0, _CmdOutput="" } ->
					ok;

				{ _ReturnCode=0, CmdOutput } ->
					?warning_fmt( "Awstats execution prior to log rotation "
						"succeeded, yet returned following message: '~s'.",
						[ CmdOutput ] );

				% An error here shall not kill this logger as a whole:
				{ ReturnCode, CmdOutput } ->
					?error_fmt( "Awstats execution prior to log rotation "
						"failed (return code: ~w), and returned following "
						"message: '~s' (command was: '~s').",
						[ ReturnCode, CmdOutput, LogAnalysisCmd ] )

			end

	end,

	rotate_basic_log_file( BinFilePath, State ).



% Generates the Awstats auxiliary (non-main, yet referenced by it) HTLM report
% pages.
%
-spec generate_other_report_pages( [ ustring() ],  ustring(),
								   wooper:state() ) -> void().
generate_other_report_pages( _ReportTypes=[], _CmdFormatString, _State ) ->
	ok;

generate_other_report_pages( _ReportTypes=[ ReportType | T ], CmdFormatString,
							 State ) ->

	Cmd = text_utils:format( CmdFormatString, [ ReportType, ReportType ] ),

	case system_utils:run_executable( Cmd ) of

		{ _ReturnCode=0, _CmdOutput="" } ->
			?debug_fmt( "Generated report type '~s' (command: '~s').",
						[ ReportType, Cmd ] ),
			ok;

		{ _ReturnCode=0, CmdOutput } ->
			?warning_fmt( "Awstats execution prior to log rotation "
						  "succeeded for report type '~s', yet returned following "
						  "message: '~s'.",
						  [ ReportType, CmdOutput ] );

		% An error here shall not kill this logger as a whole:
		{ ReturnCode, CmdOutput } ->
			?error_fmt( "Awstats execution prior to log rotation failed "
						"(return code: ~w) for report type '~s', and returned "
						"following message: '~s' (command was: '~s').",
						[ ReturnCode, ReportType, CmdOutput, Cmd ] )

	end,

	generate_other_report_pages( T, CmdFormatString, State ).



% Rotates any basic file (typically the error log one), with no specific
% post-processing.
%
% This file should not be currently open (it should have been closed and its
% handle forgotten beforehand if necessary); and it will not be reopened here.
%
-spec rotate_basic_log_file( file_utils:any_file_path(), wooper:state() ) ->
								   bin_file_path().
rotate_basic_log_file( FilePath, _State ) ->

	ArchiveFilePath = text_utils:format( "~s.~s",
		  [ FilePath, time_utils:get_textual_timestamp_for_path() ] ),

	%?trace_fmt( "Rotating '~s' to a compressed version of '~s'.",
	%			[ FilePath, ArchiveFilePath ] ),

	file_utils:move_file( FilePath, ArchiveFilePath ),

	CompressedFilePath = file_utils:compress( ArchiveFilePath ),

	file_utils:remove_file( ArchiveFilePath ),

	%?debug_fmt( "Log rotation finished: archive '~s' generated "
	%			"(original '~s' removed).", [ CompressedFilePath, FilePath ] ),

	CompressedFilePath.



% Returns the conventional base filename for specified virtual host, in the
% context of specified tool.
%
% Useful both for input files (ex: configuration ones) and (possibly) output
% ones (ex: HTML generated ones).
%
% Much like get_log_paths/2.
%
-spec get_file_prefix_for( domain_id(), vhost_id(), log_analysis_tool() ) ->
								 static_return( file_utils:file_name() ).
get_file_prefix_for( DomainId, VHostId, _Tool=awstats ) ->

	% Works in all cases, including default_vhost_catch_all and/or
	% default_domain_catch_all:
	%
	Filename = text_utils:format( "awstats.~s.~s",
								  [ VHostId, DomainId ] ),
	wooper:return_static( Filename );

get_file_prefix_for( _DomainId, _VHostId, Tool ) ->
	throw( { unsupported_web_analysis_tool, Tool } ).



% Return the name for the configuration file for specified web analysis tool,
% regarding specified virtual host.
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
								   static_return( file_utils:file_name() ).
get_conf_filename_for( DomainId, VHostId, _Tool=awstats ) ->
	Filename = text_utils:format( "awstats.~s.conf",
				   [ get_host_description_for( VHostId, DomainId, awstats ) ] ),
	wooper:return_static( Filename );

get_conf_filename_for( _DomainId, _VHostId, Tool ) ->
	throw( { unsupported_web_analysis_tool, Tool } ).



% Returns a textual description of this server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	HostDesc = get_host_description( ?getAttr(vhost_id), ?getAttr(domain_id) ),

	ConfString = case ?getAttr(web_analysis_info) of

		undefined ->
			"no web analysis enabled";

		#web_analysis_info{ tool=ToolName,
							tool_path=BinToolPath,
							helper_path=BinHelperPath,
							conf_dir=BinConfDir,
							web_content_dir=BinWbContDir } ->
			text_utils:format( "web analysis enabled, based on '~s' "
				"(path: '~s', helper: '~s'), with as configuration directory "
				"'~s' and as web content directory '~s'",
				[ ToolName, BinToolPath, BinHelperPath, BinConfDir,
				  BinWbContDir ] )

	end,

	text_utils:format( "US web logger for '~s', using log directory '~s' "
		"(writing there '~s' and '~s'); ~s", [ HostDesc, ?getAttr(log_dir),
		?getAttr(access_log_file_path), ?getAttr(error_log_file_path),
		ConfString ] ).
