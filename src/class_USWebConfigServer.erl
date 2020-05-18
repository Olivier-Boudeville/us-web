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


-module(class_USWebConfigServer).


-define( class_description,
		 "Singleton server holding the configuration information of the "
		 "US-web framework." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).



% The various kinds of websites:
% - static: a basic, static website to be served (the default)
% - nitrogen: a Nitrogen-based, dynamic website (see
% http://nitrogenproject.com/)
%
-type web_kind() :: 'static' | 'nitrogen'.


% For example <<"bar">>:
-type vhost_id() :: bin_host_name() | 'default_vhost_catch_all'.


% Ex: 'awstats'.
-type log_analysis_tool_name() :: atom().


% For the vhost_config_entry and web_analysis_info records:
-include("class_USWebConfigServer.hrl").


-type web_analysis_info() :: #web_analysis_info{}.


-export_type([ web_kind/0, vhost_id/0, web_analysis_info/0,
			   log_analysis_tool_name/0 ]).



% The default (binary) filename for information about virtual hosts, to be found
% from the overall US configuration directory:
%
-define( default_us_web_cfg_filename, <<"us-web.config">> ).


% The default registration name of the US-web server:
-define( us_web_config_server_registration_name_key,
		 us_web_config_server_registration_name ).

% The default registration name of the US-web scheduler:
-define( us_web_scheduler_registration_name_key,
		 us_web_scheduler_registration_name ).

-define( us_web_username_key, us_web_username ).
-define( us_web_app_base_dir_key, us_web_app_base_dir ).
-define( us_web_data_dir_key, us_web_data_dir ).
-define( us_web_log_dir_key, us_web_log_dir ).
-define( http_tcp_port_key, http_tcp_port ).
-define( default_web_root_key, default_web_root ).
-define( log_analysis_key, log_analysis ).
-define( routes_key, routes ).



% All known, licit keys for the US-web configuration file:
-define( known_config_keys, [ ?us_web_config_server_registration_name_key,
			?us_web_scheduler_registration_name_key,
			?us_web_username_key, ?us_web_app_base_dir_key,
			?us_web_data_dir_key, ?us_web_log_dir_key,
			?http_tcp_port_key, ?default_web_root_key, ?log_analysis_key,
			?routes_key ] ).


% The last-resort environment variable:
-define( us_web_app_env_variable, "US_WEB_APP_BASE_DIR" ).


-define( default_data_base_dir, "/var/local/us-web/data" ).
-define( default_log_base_dir, "/var/log" ).


% The DHMS periodicity at which TLS certificate renewal will be requested for
% each actual domain.
%
% Let’s Encrypt certificate lifetime is 90 days (cf. duration
% https://letsencrypt.org/docs/faq/), we trigger a renewal with some margin:
%
-define( default_dhms_certificate_renewal_periodicity, { 75, 0, 0, 0 } ).



% Design notes:
%
% This US-web configuration server will ensure that an overall US configuration
% is running, either by fetching its PID if already existing, otherwise by
% launching it accordingly.
%
% In both cases the PID of the overall server will be known, but no link will be
% created between these two, as their life-cycles are mostly independant.
%
% The filename of the virtual host configuration file will be obtained from the
% overall US configuration table (see the vhost_filename_key define), either as
% an absolute path or, most preferably, thanks to one relative to the overall US
% configuration directory.
%
% The Cowboy webserver (see https://github.com/ninenines/cowboy) is used
% internally.

% A task scheduler will be created, at least in order to rotate logs. For
% framework isolation, it will be dedicated to US-web, and by default not be
% shared with the Universal Server, so that they can operate independently.

% Note that we chose that the analysis of log web access logs (typically based
% on Awstats) can be enabled only if the 'meta' website is itself enabled (but
% meta can exist with no log analysis).

% We used to define web logger instances that were autonomous with regard to
% their scheduling for log rotation (and thus possibly log analysis
% processing). However at least some tools (ex: Awstats) may not be "reentrant",
% i.e. may not support concurrent accesses that could derive from unsynchronised
% loggers.
%
% As a result, now these web loggers all belong to the same task ring, in charge
% of pacing them uniformly and of ensuring they cannot overlap.



% Implementation notes:
%
% The static routes, logger instances and generation of configuration files for
% any log analysis tool are finely interleaved, as defining a route requires its
% logger PID, which requires the corresponding configuration file.
%
% The US-web configuration server creates a scheduler, for its own use (ex: for
% certificate renewal and the task ring regarding web loggers), and possibly for
% other related services.



% To identify a domain name (or a catch-all for them):
-type domain_id() :: bin_domain_name() | 'default_domain_catch_all'.


% A table, associating to each domain name (ex: <<"foo.org">>), the
% configuration table regarding its virtual hosts (ex: regarding "bar.foo.org",
% a <<"bar">> key would correspond, associated to its vhost_config() value):
%
-type domain_config_table() :: table( domain_id(), vhost_config_table() ).

-type domain_pair() :: { domain_id(), vhost_config_table() }.


% A table, associating, to each virtual host identifier, its settings:
-type vhost_config_table() :: table( vhost_id(), vhost_config_entry() ).


-type vhost_config_entry() :: #vhost_config_entry{}.

-type meta_web_settings() ::
		maybe( { domain_id(), vhost_id(), bin_directory_path() } ).


% Notably for us_web_meta:
-export_type([ domain_id/0, domain_config_table/0, vhost_config_table/0,
			   vhost_config_entry/0, meta_web_settings/0 ]).


% Not exported:
%-type route_rule() :: cowboy_router:route_rule().
-type route_rule() :: term().

-type dispatch_routes() :: cowboy_router:dispatch_routes().
-type dispatch_rules() :: cowboy_router:dispatch_rules().


% A table holding US-web configuration information:
-type us_web_config_table() :: table( atom(), term() ).


% Settings about any web access log analysis, i.e. our canonical name for the
% analysis tool, its (main) root directory and the directory of this helper (if
% any were specified): {ToolName, MaybeAnalysisToolRoot,
% MaybeAnalysisHelperRoot}.
%
-type log_analysis_settings() :: { log_analysis_tool_name(),
								   maybe( file_utils:bin_directory_path() ),
								   maybe( file_utils:bin_directory_path() ) }.


% To silence attribute-only types:
-export_type([ log_analysis_settings/0 ]).


% Shorthands:

-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: file_utils:bin_directory_path().

-type bin_file_path() :: file_utils:bin_file_path().

-type domain_name() :: net_utils:domain_name().
-type bin_domain_name() :: net_utils:bin_domain_name().

%-type bin_subdomain() :: net_utils:bin_subdomain().

-type bin_host_name() :: net_utils:bin_host_name().

%-type server_pid() :: class_UniversalServer:server_pid().

-type logger_pid() :: class_USWebLogger:logger_pid().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
-type task_id() :: class_USScheduler:task_id().



% The class-specific attributes:
-define( class_attributes, [

	{ execution_context, basic_utils:execution_context(),
	  "tells whether this server is to run in development or production mode" },

	{ domain_config_table, domain_config_table(),
	  "a table containing all configuration information related to virtual "
	  "hosts, per-domain" },

	{ default_web_root, maybe( bin_directory_path() ),
	  "the default root (if any) for the website trees" },

	{ meta_web_settings, meta_web_settings(),
	  "the domain, virtual host identifiers and web root of the auto-generated "
	  "meta website (if any)" },

	{ http_tcp_port, net_utils:tcp_port(),
	  "the TCP port at which the webserver is to listen for the http scheme" },

	{ us_server_pid, maybe( server_pid() ),
	  "the PID of the associated US server (if any)" },

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" },

	{ us_web_scheduler_pid, scheduler_pid(),
	  "the PID of the US-web dedicated scheduler (at least for certificate
	  renewal, for task ring or possibly directly for web loggers)" },

	{ logger_task_ring, class_USTaskRing:ring_pid(),
	  "the PID of the task ring in charge of sequencing the webloggers" },

	{ us_web_username, basic_utils:user_name(),
	  "the user (if any) who shall launch the US web application" },

	{ us_web_supervisor_pid, otp_utils:supervisor_pid(),
	  "the PID of the OTP supervisor of US-web, as defined in us_web_sup" },

	{ dispatch_rules, dispatch_rules(),
	  "the Cowboy dispatch rules corresponding to the configuration" },

	{ config_base_directory, bin_directory_path(),
	  "the base directory where all US configuration is to be found "
	   "(not the us_web/priv/conf internal directory)"},

	{ app_base_directory, bin_directory_path(),
	  "the base directory of the US-web application (the root from whence "
	  "src, priv, ebin, etc. can be found" },

	{ conf_directory, bin_directory_path(),
	  "the US-web internal configuration directory, 'us_web/priv/conf'" },

	{ data_directory, bin_directory_path(),
	  "the directory where working data (ex: the database state of a log tool) "
	  "is to be stored" },

	{ log_directory, bin_directory_path(),
	  "the directory where (basic, technical) US-web logs shall be written, "
	  "notably access and error logs for websites" },

	{ scheduler_registration_name, naming_utils:registration_name(),
	  "the name under which the dedicated scheduler is registered" },

	{ start_timestamp, time_utils:timestamp(),
	  "the start timestamp of this server" },

	{ log_analysis_settings, maybe( log_analysis_settings() ),
	  "the settings to use for any analysis of web access logs" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Configuration.USWebConfigServer" ).


-define( default_us_web_config_server_registration_name, us_web_config_server ).
-define( default_us_web_scheduler_registration_name, us_web_scheduler ).

-define( default_registration_scope, global_only ).


-define( app_subdir, us-web ).


% For unused functions:
-export([ unregister_domain_tasks/2 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Constructs the US-web configuration server.
-spec construct( wooper:state(), otp_utils:supervisor_pid() ) -> wooper:state().
construct( State, SupervisorPid ) ->

	StartTimestamp = time_utils:get_timestamp(),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State,
										   ?trace_categorize("USWebServer") ),

	?send_info( TraceState, "Creating a US-web configuration server." ),

	% Has been useful to debug a crashing start-up not letting outputs
	% displayed:
	%
	%io:format( "code: ~s", [ code_utils:get_code_path_as_string() ] ),
	%timer:sleep( 1000 ),

	% Same logic as the overall US configuration server, notably to obtain the
	% same registration name for it:
	%
	BinCfgDir = case class_USConfigServer:get_us_config_directory() of

		{ undefined, CfgMsg } ->
			?send_error_fmt( TraceState, "Unable to determine the US "
							 "configuration directory: ~s.", [ CfgMsg ] ),
			throw( us_configuration_directory_not_found );

		{ FoundCfgDir, CfgMsg } ->
			?send_info( TraceState, CfgMsg ),
			FoundCfgDir

	end,

	SupState = setAttributes( TraceState, [
					{ us_web_supervisor_pid, SupervisorPid },
					{ start_timestamp, StartTimestamp } ] ),

	CfgState = load_and_apply_configuration( BinCfgDir, SupState ),


	?send_info( CfgState, to_string( CfgState ) ),

	% Done rather late on purpose, so that the existence of that file can be
	% seen as a sign that the initialisation went well (used by start-us-web.sh)
	%
	% Now that the log directory is known, we can properly redirect the traces.
	% Already a trace emitter:

	NewBinTraceFilePath = text_utils:string_to_binary(
			file_utils:join( getAttribute( CfgState, log_directory ),
							 "us_web.traces" ) ),

	getAttribute( CfgState, trace_aggregator_pid) !
		{ renameTraceFile, NewBinTraceFilePath },

	CfgState.



% Method section.


% Returns the web configuration settings (typically for the us_web supervisor).
-spec getWebConfigSettings( wooper:state() ) ->
		   const_request_return( { dispatch_rules(), net_utils:tcp_port() } ).
getWebConfigSettings( State ) ->
	wooper:const_return_result( { ?getAttr(dispatch_rules),
								  ?getAttr(http_tcp_port) } ).




% Helper section.


% Loads and applies the relevant configuration settings first from the overall
% US configuration file, then from the more web/vhost specific one.
%
-spec load_and_apply_configuration( bin_directory_path(), wooper:state() ) ->
		  wooper:state().
load_and_apply_configuration( BinCfgDir, State ) ->

	% Static settings regarding the overall US configuration server:
	{ USCfgFilename, USCfgRegNameKey, USCfgDefRegName, USCfgRegScope } =
		class_USConfigServer:get_default_settings(),

	USCfgFilePath = file_utils:join( BinCfgDir, USCfgFilename ),

	% Should, by design, never happen:
	case file_utils:is_existing_file_or_link( USCfgFilePath ) of

		true ->
			ok;

		false ->
			?error_fmt( "The overall US configuration file ('~s') "
						"could not be found.", [ USCfgFilePath ] ),
			% Must have disappeared then:
			throw( { us_config_file_not_found, USCfgFilePath } )

	end,

	?info_fmt( "Reading the Universal Server configuration from '~s'.",
			   [ USCfgFilePath ] ),

	% Ensures as well that all top-level terms are only pairs:
	ConfigTable = table:new_from_unique_entries(
					file_utils:read_terms( USCfgFilePath ) ),

	?info_fmt( "Read US configuration ~s", [ table:to_string( ConfigTable ) ] ),

	% We check whether a proper US configuration server already exists (and then
	% we use it) or if it shall be created; for that we just extract its
	% expected registration name:
	%
	% (important side-effects, such as updating the VM cookie)

	USCfgRegName = case table:lookup_entry( USCfgRegNameKey, ConfigTable ) of

		key_not_found ->
			?info_fmt( "No user-configured registration name to locate the "
			   "overall US configuration server, using default name '~s'.",
			   [ USCfgDefRegName ] ),
			USCfgDefRegName;

		{ value, UserRegName } when is_atom( UserRegName ) ->
			?info_fmt( "To locate the overall US configuration server, will "
					   "rely on the user-configured registration name '~s'.",
					   [ UserRegName ] ),
			UserRegName;

		{ value, InvalidRegName } ->
			?error_fmt( "Read invalid user-configured registration name to "
						"locate the overall US configuration server: '~p'.",
						[ InvalidRegName ] ),
			throw( { invalid_us_config_server_registration_name, InvalidRegName,
					 USCfgRegNameKey } )

	end,

	% Now we are able to look it up; either the overall US configuration server
	% already exists, or it shall be created:
	%
	CfgServerPid = case naming_utils:is_registered( USCfgRegName,
													USCfgRegScope ) of

		not_registered ->

			% Second try, if ever there were concurrent start-ups:
			timer:sleep( 500 ),

			case naming_utils:is_registered( USCfgRegName, USCfgRegScope ) of

				not_registered ->
					?info_fmt( "There is no ~s registration of '~s'; creating "
							   "thus a new overall US configuration server.",
							   [ USCfgRegScope, USCfgRegName ] ),
					% Not linked to have an uniform semantics:
					class_USConfigServer:new();

				DelayedCfgPid ->
					?info_fmt( "Found (after some delay) an already running "
							   "overall US configuration server, using it: "
							   "~s registration look-up for '~s' returned ~w.",
							   [ USCfgRegScope, USCfgRegName, DelayedCfgPid ] ),
					DelayedCfgPid

				end;

		CfgPid ->
			?info_fmt( "Found an already running overall US configuration "
					   "server, using it: ~s registration look-up for '~s' "
					   "returned ~w.",
					   [ USCfgRegScope, USCfgRegName, CfgPid ] ),
			CfgPid

	end,

	% This web configuration server is not supposed to read more the US
	% configuration file; it should request it to the overall configuration
	% server, about all the extra information it needs, to avoid duplicated,
	% possibly inconsistent reading/interpretation (and in order to declare
	% itself in the same move):
	%
	CfgServerPid ! { getWebRuntimeSettings, [], self() },

	% No possible interleaving:
	receive

		{ wooper_result, { RecvBinCfgDir, ExecContext, MaybeWebCfgFilename,
						   MaybeUSServerPid } } ->

			% Check:
			case RecvBinCfgDir of

				BinCfgDir ->
					ok;

				_ ->
					?error_fmt( "Mismatching US configuration directories: "
								"had '~s', yet received from US configuration "
								"server: '~s'.", [ BinCfgDir, RecvBinCfgDir ] ),

					throw( { us_config_dir_mismatch,
							 {  BinCfgDir, RecvBinCfgDir } } )

			end,

			StoreState = setAttributes( State, [
				{ execution_context, ExecContext },
				{ meta_web_settings, undefined },
				{ config_base_directory, BinCfgDir },
				{ us_server_pid, MaybeUSServerPid },
				{ us_config_server_pid, CfgServerPid },
				{ log_analysis_settings, undefined } ] ),

			load_web_config( BinCfgDir, MaybeWebCfgFilename, StoreState )

	end.



% Loads web configuration information, notably about virtual hosts.
-spec load_web_config( bin_directory_path(), maybe( bin_file_path() ),
		   wooper:state() ) -> { dispatch_routes(), wooper:state() }.
load_web_config( BinCfgBaseDir, _MaybeBinWebCfgFilename=undefined, State ) ->

	DefaultBinWebCfgFilename = ?default_us_web_cfg_filename,

	?info_fmt( "No configuration filename known of the overall US configuration"
			   " server (i.e. none defined in its own configuration file), "
			   "hence defaulting to '~s'.", [ DefaultBinWebCfgFilename ] ),

	load_web_config( BinCfgBaseDir, DefaultBinWebCfgFilename, State );


load_web_config( BinCfgBaseDir, BinWebCfgFilename, State ) ->

	WebCfgFilePath = file_utils:ensure_path_is_absolute( BinWebCfgFilename,
												_BasePath=BinCfgBaseDir ),

	case file_utils:is_existing_file_or_link( WebCfgFilePath ) of

		true ->
			?info_fmt( "Reading web configuration file, found as '~s'.",
					   [ WebCfgFilePath ] );

		false ->
			% Possibly user/group permission issue:
			?error_fmt( "No web configuration file found or accessible "
						"(ex: symbolic link to an inaccessible file); "
						"tried '~s'.", [ WebCfgFilePath ] ),
			throw( { us_web_config_file_not_found,
					 text_utils:binary_to_string( WebCfgFilePath ) } )

	end,

	% Checks that only pairs are found:
	WebCfgTable = table:new_from_unique_entries(
					file_utils:read_terms( WebCfgFilePath ) ),

	?debug_fmt( "Read web configuration ~s",
				[ table:to_string( WebCfgTable ) ] ),

	RegState = manage_registrations( WebCfgTable, State ),

	UserState = manage_os_user( WebCfgTable, RegState ),

	AppState = manage_app_base_directories( WebCfgTable, UserState ),

	DataState = manage_data_directory( WebCfgTable, AppState ),

	LogState = manage_log_directory( WebCfgTable,DataState  ),

	PortState = manage_port( WebCfgTable, LogState ),

	RootState = manage_web_root( WebCfgTable, PortState ),

	PreMetaState = manage_pre_meta( WebCfgTable, RootState ),

	RouteState = manage_routes( WebCfgTable, PreMetaState ),

	PostMetaState = manage_post_meta( RouteState ),

	LicitKeys = ?known_config_keys,

	case list_utils:difference( table:keys( WebCfgTable ), LicitKeys ) of

		[] ->
			PostMetaState;

		UnexpectedKeys ->
			?error_fmt( "Unknown key(s) in '~s': ~s~nLicit keys: ~s",
						[ WebCfgFilePath,
						  text_utils:terms_to_string( UnexpectedKeys ),
						  text_utils:terms_to_string( LicitKeys ) ] ),
			throw( { invalid_configuration_keys, UnexpectedKeys,
					 text_utils:binary_to_string( WebCfgFilePath ) } )

	end.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Now web loggers are not registered directly to the scheduler, so we just
	% have to take care of the task ring:

	% {DomainId,VHostCfgTable} pairs:
	%DomPairs = table:enumerate( ?getAttr(domain_config_table) ),

	% Result not kept here:.
	%unregister_domain_tasks( DomPairs, State ),

	% No synchronicity needed:
	?getAttr(logger_task_ring) ! delete,

	[ LPid ! delete || LPid <- get_all_logger_pids( State ) ],

	% Unregisters regarding certificate renewal:


	?getAttr(us_web_scheduler_pid) ! delete,

	?info( "Deleted." ),
	State.



% Unregisters the scheduling tasks associated to each vhost of each specified
% domain, returning updated domain pairs (State only used for traces).
%
-spec unregister_domain_tasks( [ domain_pair() ], wooper:state() ) ->
									[ domain_pair() ].
unregister_domain_tasks( DomPairs, State ) ->
	unregister_domain_tasks( DomPairs, ?getAttr(us_web_scheduler_pid), State,
							 _Acc=[] ).


% (helper)
unregister_domain_tasks( _DomPairs=[], _SchedulerPid, _State, Acc ) ->
	Acc;

unregister_domain_tasks( _DomPairs=[ { DomainId, VHostCfgTable } | T ],
						 SchedulerPid, State, Acc ) ->

	NewVHostCfgTable = unregister_domain_task( DomainId, VHostCfgTable,
											   SchedulerPid, State ),

	unregister_domain_tasks( T, SchedulerPid, State,
							 [ { DomainId, NewVHostCfgTable } | Acc ] ).



% Unregisters the scheduling tasks associated to specified domain.
unregister_domain_task( DomainId, VHostCfgTable, SchedulerPid, State ) ->

	{ AllTaskIds, NewVHostCfgTable } = extract_all_task_ids( VHostCfgTable ),

	SchedulerPid ! { unregisterTasks, [ AllTaskIds ], self() },

	receive

		{ wooper_result, Outcomes } ->
			% All tasks are unlimited, hence none shall be done;
			case lists_utils:delete_all_in( _Elem=task_unregistered,
											Outcomes ) of

				[] ->
					?debug_fmt( "For domain '~s', all ~B tasks successfully "
						"unregistered.", [ DomainId, length( Outcomes ) ] ),
					NewVHostCfgTable;

				_ ->
					Pairs = lists:zip( AllTaskIds, Outcomes ),
					UnexpectedPairs = [ P || P={ _Id, Outcome } <- Pairs,
											 Outcome =/= task_unregistered ],
					?error_fmt( "For domain '~s', following tasks could not be "
								"unregistered for following reasons: ~p",
								[ DomainId, UnexpectedPairs ] ),
					NewVHostCfgTable

			end

	end.



% Extracts all task identifiers from specified vhost table, returns them and the
% table obtained once they have been set to undefined.
%
-spec extract_all_task_ids( vhost_config_table() ) ->
								  { [ task_id() ], vhost_config_table() }.
extract_all_task_ids( VHostCfgTable ) ->
	VhostPairs = table:enumerate( VHostCfgTable ),
	extract_task_ids_from( VhostPairs, _AccIds=[], _AccPairs=[] ).



% (helper)
extract_task_ids_from( _VhostPairs=[], AccIds, AccPairs ) ->
	{ AccIds, table:new( AccPairs ) };

extract_task_ids_from( _VhostPairs=[ { VHostId, VHostEntry } | T ],
					   AccIds, AccPairs ) ->
	{ NewIds, NewVHostEntry } = extract_task_ids_from_entry( VHostEntry ),
	extract_task_ids_from( T, NewIds ++ AccIds,
						   [ { VHostId, NewVHostEntry } | AccPairs ] ).



% At least currently, only up to one:
extract_task_ids_from_entry(
  VCE=#vhost_config_entry{ cert_task_id=undefined } ) ->
	 { _NewIds=[], VCE };

extract_task_ids_from_entry(
  VCE=#vhost_config_entry{ cert_task_id=CertTaskId } ) ->
	 { _NewIds=[ CertTaskId ],
	   VCE#vhost_config_entry{ cert_task_id=undefined } }.



% Method section.


% Requests the TLS certificate of specified virtual host to be renewed.
-spec renewCertificate( wooper:state(), domain_id(), vhost_id() ) ->
							const_oneway_return().
renewCertificate( State, DomainId, VHostId ) ->

	?info_fmt( "Renewal of TLS certification for virtual host '~s' of "
			   "domain '~s' requested.", [ VHostId, DomainId ] ),

	wooper:const_return().



% Helper section.


% Returns a domain table and dispatch routes corresponding to the specified
% domain information list, and a possibly updated state.
%
-spec process_domain_info( [ { domain_name(), [ term() ] } ],
		bin_directory_path(), maybe( bin_directory_path() ),
		maybe( web_analysis_info() ), scheduler_pid(), wooper:state() ) ->
			{ domain_config_table(), dispatch_routes(), wooper:state() }.
process_domain_info( UserRoutes, BinLogDir, MaybeBinDefaultWebRoot,
					 MaybeLogAnalysisSettings, SchedulerPid, State ) ->

	%trace_utils:debug_fmt( "Domain list: ~p", [ UserRoutes ] ),

	MaybeWebAnalysisInfo = prepare_web_analysis( MaybeLogAnalysisSettings,
							UserRoutes, MaybeBinDefaultWebRoot, State ),

	% Any host catch-all must come last:
	process_domain_routes( lists:reverse( UserRoutes ), BinLogDir,
		MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, _AccVTable=table:new(),
		_AccRoutes=[], SchedulerPid, State ).




% Returns the web analysis information, if needed, to be able to generate on the
% fly the web analysis configuration files afterwards.
%
-spec prepare_web_analysis( maybe( log_analysis_settings() ), list(),
							maybe( bin_directory_path() ), wooper:state() ) ->
								  maybe( web_analysis_info() ).
prepare_web_analysis( _MaybeLogAnalysisSettings=undefined, _UserRoutes,
					  _MaybeBinDefaultWebRoot, _State ) ->
	undefined;

prepare_web_analysis(
  _MaybeLogAnalysisSettings={ _ToolName=awstats, BinAnalysisToolRoot,
							  BinAnalysisHelperRoot },
  UserRoutes, MaybeBinDefaultWebRoot, State ) ->

	% First verifications done in manage_pre_meta/2.

	% When web analysis is enabled like here, the logger instances must be
	% created while already knowing in which directory the HTML reports shall be
	% generated, i.e. the meta web root must be determined beforehand:
	%
	BinMetaWebRoot =
		determine_meta_web_root( UserRoutes, MaybeBinDefaultWebRoot, State ),

	% The directory in which the source template for the web log tool (Awstats
	% here) already lies:
	%
	BinAwConfDir = ?getAttr(conf_directory),

	ConfTemplatePath = file_utils:join( BinAwConfDir, "awstats.template.conf" ),

	case file_utils:is_existing_file_or_link( ConfTemplatePath ) of

		true ->
			ok;

		false ->
			throw( { awstats_conf_template_not_found, ConfTemplatePath } )

	end,

	% Quite similar to a collective file_utils:update_with_keywords/3:

	TemplateBaseContent = file_utils:read_whole( ConfTemplatePath ),

	% Common to all virtual hosts:
	BaseTranslationTable = table:new(
		[ { "US_WEB_LOG_ANALYSIS_DATA_DIR", ?getAttr(data_directory) } ] ),

	% Done once, for all virtual hosts:
	TemplateContent = text_utils:update_with_keywords( TemplateBaseContent,
													   BaseTranslationTable ),

	% Validates the target Awstats script. Previously the main tool was
	% "awstats.pl" and no helper was used, now building all report pages in one
	% go with (using the former as its helper):

	ToolPath = file_utils:join( [ BinAnalysisToolRoot,
								  "awstats_buildstaticpages.pl" ] ),

	BinToolPath = case file_utils:is_executable( ToolPath ) of

		true ->
			text_utils:string_to_binary( ToolPath );

		false ->
			throw( { awstats_main_executable_not_found, ToolPath } )

	end,

	HelperPath =
		file_utils:join( [ BinAnalysisHelperRoot, "cgi-bin", "awstats.pl" ] ),

	BinHelperPath = case file_utils:is_executable( HelperPath ) of

		true ->
			text_utils:string_to_binary( HelperPath );

		false ->
			throw( { awstats_helper_executable_not_found, HelperPath } )

	end,


	% The directory in which the per-vhost configuration files for the web log
	% tool (Awstats here) will be generated:
	%
	% It was previously in a directory of our own, yet since version 7.8
	% Awstasts insists on having these configuration files lie in a standard
	% one:

	%GenAwCfgDir = file_utils:join( ?getAttr(data_directory),
	%							   "awstats-vhost-configs" ),

	%file_utils:create_directory_if_not_existing( GenAwCfgDir ),

	GenAwCfgDir = "/usr/local/etc/awstats",

	% Let's attempt any creation, even if it may fail because of user
	% permissions:
	%
	try

		file_utils:create_directory_if_not_existing( GenAwCfgDir,
													 create_parents )

	catch

		_AnyClass:Exception ->
			?error_fmt( "The attempt to create the '~s' directory (and "
						"possibly its parents) as user '~s' failed: ~p.",
						[ GenAwCfgDir, system_utils:get_user_name_safe(),
						  Exception ] )

	end,

	% Expanded from log_analysis_settings and returned;
	#web_analysis_info{ tool=awstats,
						tool_path=BinToolPath,
						helper_path=BinHelperPath,
						template_content=TemplateContent,

						% Where per-vhost configuration files shall be
						% generated:
						%
						conf_dir=text_utils:string_to_binary( GenAwCfgDir ),

						web_content_dir=BinMetaWebRoot }.



% Determines the (absolute) meta web root to use.
%
% Returns the first found, supposing only one is defined (build_vhost_table/8
% will perform an exhaustive search thereof).
%
determine_meta_web_root( _UserRoutes=[], _MaybeBinDefaultWebRoot, _State ) ->
	throw( no_meta_web_root_found );

% Per-domain first:
determine_meta_web_root( _UserRoutes=[ { DomainId, VHostInfos } | T ],
						 MaybeBinDefaultWebRoot, State ) ->
	% Meta web root found for current domain?
	case determine_meta_web_root_for( VHostInfos, DomainId,
									  MaybeBinDefaultWebRoot, State ) of

		false ->
			determine_meta_web_root( T, MaybeBinDefaultWebRoot, State );

		BinMetaWebRoot ->
			BinMetaWebRoot

	end.


% Per-vhost now:
% (helper)
determine_meta_web_root_for( _VHostInfos=[], _DomainId, _MaybeBinDefaultWebRoot,
							 _State ) ->
	false;

% Meta found:
determine_meta_web_root_for(
  _VHostInfos=[ { VHostId, ContentRoot, _WebKind=meta } | _T ],
  DomainId, MaybeBinDefaultWebRoot, State ) ->
	ensure_meta_content_root_exists( ContentRoot, MaybeBinDefaultWebRoot,
									 VHostId, DomainId, State );

determine_meta_web_root_for( _VHostInfos=[ _ | T ], DomainId,
							 MaybeBinDefaultWebRoot, State ) ->
	determine_meta_web_root_for( T, DomainId, MaybeBinDefaultWebRoot, State ).



% (helper)
process_domain_routes( _UserRoutes=[], _BinLogDir, _MaybeBinDefaultWebRoot,
		_MaybeWebAnalysisInfo, AccVTable, AccRoutes, _SchedulerPid, State ) ->
	%trace_utils:debug_fmt( "Resulting routes:~n~p", [ AccRoutes ] ),
	{ AccVTable, AccRoutes, State };


% Explicit domain:
process_domain_routes( _UserRoutes=[ { DomainName, VHostInfo } | T ], BinLogDir,
		MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable, AccRoutes,
		SchedulerPid, State ) when is_list( DomainName ) ->

	BinDomainName = text_utils:string_to_binary( DomainName ),

	{ VHostTable, VHostRoutes, BuildState } = build_vhost_table( BinDomainName,
		VHostInfo, BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo,
		_AccVtable=table:new(), _AccRoutes=[], SchedulerPid, State ),

	NewAccVTable = table:add_new_entry( _K=BinDomainName, _V=VHostTable,
									   AccVTable ),

	% Order matters:
	NewAccRoutes = VHostRoutes ++ AccRoutes,

	process_domain_routes( T, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, NewAccVTable, NewAccRoutes, SchedulerPid,
		BuildState );


% Domain catch-all:
process_domain_routes(
  _UserRoutes=[ { DomainId=default_domain_catch_all, VHostInfo } | T ],
  BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable,
  AccRoutes, SchedulerPid, State ) ->

	{ VHostTable, VHostRoutes, BuildState } = build_vhost_table( DomainId,
		VHostInfo, BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo,
		_AccVtable=table:new(), _AccRoutes=[], SchedulerPid, State ),

	NewAccVTable = table:add_new_entry( _K=DomainId, _V=VHostTable, AccVTable ),

	% Order matters:
	NewAccRoutes = VHostRoutes ++ AccRoutes,

	process_domain_routes( T, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, NewAccVTable, NewAccRoutes, SchedulerPid,
		BuildState );


process_domain_routes( _UserRoutes=[ InvalidEntry | _T ], _BinLogDir,
		_MaybeBinDefaultWebRoot, _MaybeWebAnalysisInfo, _AccVTable, _AccRoutes,
		_SchedulerPid, State ) ->

	?error_fmt( "Invalid entry in virtual host configuration:~n~p",
				[ InvalidEntry ] ),

	throw( { invalid_vhost_entry, InvalidEntry } ).




% Builds a vhost table corresponding to specified domain identifier.
%
% Multiple operations have to be done in one pass as they are quite interlinked.
%
% (helper)
-spec build_vhost_table( domain_id(), [ term() ], bin_directory_path(),
			maybe( bin_directory_path() ), maybe( web_analysis_info() ),
			list(), dispatch_routes(), scheduler_pid(), wooper:state() ) ->
		  { vhost_config_table(), dispatch_routes(), wooper:state() }.
build_vhost_table( _DomainId, _VHostInfos=[], _BinLogDir,
		_MaybeBinDefaultWebRoot, _MaybeWebAnalysisInfo, AccVTable, AccRoutes,
		_SchedulerPid, State ) ->
	% Restores user-defined order (ex: default route to come last):
	{ AccVTable, lists:reverse( AccRoutes ), State };


% No kind specified, upgrading to static:
build_vhost_table( DomainId, _VHostInfos=[ { VHostId, ContentRoot } | T ],
		BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable,
		AccRoutes, SchedulerPid, State ) ->
	build_vhost_table( DomainId,
		_FullVHostInfos=[ { VHostId, ContentRoot, _DefaultKind=static } | T ],
		BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable,
		AccRoutes, SchedulerPid, State );


% Static - actual or catch-all - vhost specified here:
build_vhost_table( DomainId,
		_VHostInfos=[ { VHostId, ContentRoot, WebKind=static } | T ],
		BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable,
		AccRoutes, SchedulerPid, State ) ->

	BinContentRoot = get_content_root( ContentRoot, MaybeBinDefaultWebRoot,
									   VHostId, DomainId, State ),

	ActualKind = check_kind( WebKind, VHostId, DomainId, State ),

	BinVHostId = case VHostId of

		default_vhost_catch_all ->
			default_vhost_catch_all;

		_ ->
			text_utils:string_to_binary( VHostId )

	end,

	{ VHostEntry, VHostRoute } = manage_vhost( BinContentRoot, ActualKind,
		DomainId, BinVHostId, BinLogDir, SchedulerPid, MaybeWebAnalysisInfo ),

	NewAccVTable = table:add_entry( _K=BinVHostId, VHostEntry, AccVTable ),

	build_vhost_table( DomainId, T, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, NewAccVTable, [ VHostRoute | AccRoutes ],
		SchedulerPid, State );


% Meta kind here:
build_vhost_table( DomainId,
	   _VHostInfos=[ { VHost, _ContentRoot, WebKind=meta } | T ],
	   BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable,
	   AccRoutes, SchedulerPid, State ) ->

	% Quite similar to 'static', even if a logger and a web analysis
	% organisation are not that useful:

	% Not expected to be 'default_vhost_catch_all':
	BinVHost = text_utils:string_to_binary( VHost ),

	BinMetaContentRoot = case MaybeWebAnalysisInfo of

		undefined ->
			?error_fmt( "A meta website (vhost '~s' for domain '~s') was "
						"requested, yet no web analysis information is "
						"available. No 'log_analysis' entry defined in "
						"US-web configuration file?",
						[ VHost, DomainId ] ),
			throw( { no_web_analysis_info, VHost, DomainId } );

		#web_analysis_info{ web_content_dir=BinWebContentDir } ->
			% Full path, unlike ContentRoot:
			BinWebContentDir

	end,

	% Check, as only up to one meta wanted:
	undefined = ?getAttr(meta_web_settings),

	SetState = setAttribute( State, meta_web_settings,
					  { DomainId, BinVHost, BinMetaContentRoot } ),

	{ VHostEntry, VHostRoute } = manage_vhost( BinMetaContentRoot, WebKind,
			DomainId, BinVHost, BinLogDir, SchedulerPid, MaybeWebAnalysisInfo ),

	NewAccVTable = table:add_entry( _K=BinVHost, VHostEntry, AccVTable ),

	build_vhost_table( DomainId, T, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, NewAccVTable, [ VHostRoute | AccRoutes ],
		SchedulerPid, SetState );


% Nitrogen kind here:
build_vhost_table( _DomainId,
	   _VHostInfos=[ { _VHost, _ContentRoot, _WebKind=nitrogen } | _T ],
		_BinLogDir, _MaybeBinDefaultWebRoot, _MaybeWebAnalysisInfo, _AccVTable,
		_AccRoutes, _SchedulerPid, _State ) ->
	throw( not_implemented_yet );


% Unknown kind:
build_vhost_table( _DomainId,
	   _VHostInfos=[ { _VHost, _ContentRoot, UnknownWebKind } | _T ],
	   _BinLogDir, _MaybeBinDefaultWebRoot, _MaybeWebAnalysisInfo, _AccVTable,
	   _AccRoutes, _SchedulerPid, _State ) ->
	throw( { unknown_web_kind, UnknownWebKind } );

build_vhost_table( DomainId, _VHostInfos=[ InvalidVHostConfig | _T ],
	   _BinLogDir, _MaybeBinDefaultWebRoot, _MaybeWebAnalysisInfo, _AccVTable,
	   _AccRoutes, _SchedulerPid, _State ) ->
	throw( { invalid_virtual_host_config, InvalidVHostConfig, DomainId } ).




% Returns the corresponding {VHostEntry,VHostRoute} pair.
%
% (centralising helper)
%
% Here no web analysis is requested (yet log rotation is still useful):
manage_vhost( BinContentRoot, ActualKind, DomainId, VHostId, BinLogDir,
			  SchedulerPid, _MaybeWebAnalysisInfo=undefined ) ->

	% Go for maximum interleaving:

	TaskCmd = { renewCertificate, [ DomainId, VHostId ] },

	SchedulerPid ! { registerTask, [ TaskCmd, _StartTime=flexible,
					   ?default_dhms_certificate_renewal_periodicity,
					   _Count=unlimited, _ActPid=self() ], self() },

	% Now, we do not use anymore a scheduler for web looging, we prefer a task
	% ring in order to avoid any possible concurrent runs of any analysis tool:
	%
	LoggerPid = class_USWebLogger:new_link( VHostId, DomainId, BinLogDir,
			_MaybeSchedulerPid=undefined, _MaybeWebGenSettings=undefined ),

	VHostEntry = #vhost_config_entry{ virtual_host=VHostId,
									  parent_host=DomainId,
									  kind=ActualKind,
									  content_root=BinContentRoot,
									  logger_pid=LoggerPid },

	VHostRoute =
		get_static_dispatch_for( VHostId, DomainId, BinContentRoot, LoggerPid ),

	% In answer to the call to the registerTask/6 request:
	receive

		{ wooper_result, { task_registered, TaskId } } ->
			{ VHostEntry#vhost_config_entry{ cert_task_id=TaskId }, VHostRoute }

	end;


% Here web analysis is requested:
manage_vhost( BinContentRoot, ActualKind, DomainId, VHostId, BinLogDir,
		SchedulerPid,
		WebAnalysisInfo=#web_analysis_info{ tool=LogAnalysisTool,
											template_content=TemplateContent,
											conf_dir=BinConfDir } ) ->

	TaskCmd = { renewCertificate, [ DomainId, VHostId ] },

	SchedulerPid ! { registerTask, [ TaskCmd, _StartTime=flexible,
					   ?default_dhms_certificate_renewal_periodicity,
					   _Count=unlimited, _ActPid=self() ], self() },

	% Preparing for the requested web analysis: before spawning a corresponding
	% logger, generating its related configuration file.

	CfgDomainStr = case DomainId of

		default_domain_catch_all ->
			% Just use the "intranet" naming then (see SiteDomain comments in
			% the awstats template):
			%
			net_utils:localhost();

		_ ->
			text_utils:binary_to_string( DomainId )

	end,

	{ BinAccessLogFilename, _BinErrorLogFilename } =
		class_USWebLogger:get_log_paths( VHostId, DomainId ),

	AccessLogFilePath = file_utils:join( BinLogDir, BinAccessLogFilename ),

	BaseAlias = "localhost 127.0.0.1",

	{ CfgFullVHostStr, AliasStr } = case VHostId of

		default_vhost_catch_all ->

			% No relevant "X.foobar.org" to specify here, so just relying on
			% "foobar.org" for the domain, and a relevant regex for the alias to
			% catch all:
			%
			RegexStr = text_utils:format( " REGEX[~s$]", [
				% So that "baz.foobar.org" becomes "baz\.foobar\.org":
				text_utils:escape_with( CfgDomainStr, _CharsToEscape=[ $. ],
										_EscapingChar=$\ ) ] ),

			{ CfgDomainStr, BaseAlias ++ RegexStr };

		_ ->
			{ text_utils:binary_to_string( VHostId ) ++ [ $. | CfgDomainStr ],
			  BaseAlias }

	end,

	TranslationTable = table:new( [
		{ "US_WEB_VHOST_LOG_FILE", AccessLogFilePath },
		{ "US_WEB_VHOST_DOMAIN", CfgFullVHostStr },
		{ "US_WEB_HOST_ALIASES", AliasStr } ] ),

	VHostContent =
		text_utils:update_with_keywords( TemplateContent, TranslationTable ),

	%LogConfFilename = class_USWebLogger:get_file_prefix_for( DomainId, VHostId,
	%											LogAnalysisTool ) ++ ".conf",

	LogConfFilename = class_USWebLogger:get_conf_filename_for( DomainId,
							VHostId, LogAnalysisTool ),

	TargetConfFilePath = file_utils:join( BinConfDir, LogConfFilename ),

	file_utils:remove_file_if_existing( TargetConfFilePath ),

	file_utils:write_whole( TargetConfFilePath, VHostContent ),

	%trace_utils:debug_fmt( "Wrote '~s', creating corresponding logger.",
	%					   [ TargetConfFilePath ] ),

	% As we must create that logger *after* the configuration file it relies on:
	% (see previous clause about scheduler)
	LoggerPid = class_USWebLogger:new_link( VHostId, DomainId, BinLogDir,
					_MaybeSchedulerPid=undefined, WebAnalysisInfo ),

	VHostRoute = get_static_dispatch_for( VHostId, DomainId, BinContentRoot,
										  LoggerPid ),

	% In answer to the call to the registerTask/6 request:
	CertTaskId = receive

		{ wooper_result, { task_registered, TId } } ->
			TId

	end,

	VHostEntry = #vhost_config_entry{ virtual_host=VHostId,
									  parent_host=DomainId,
									  kind=ActualKind,
									  content_root=BinContentRoot,
									  logger_pid=LoggerPid,
									  cert_task_id=CertTaskId },

	{ VHostEntry, VHostRoute }.



% Checks and returns an absolute version of the specified content root.
-spec get_content_root( directory_path(), maybe( bin_directory_path() ),
		vhost_id(), domain_id(), wooper:state() ) -> bin_directory_path().
get_content_root( ContentRoot, MaybeBinDefaultWebRoot, VHostId, DomainId,
				  State ) ->

	ActualContentRoot = case file_utils:is_absolute_path( ContentRoot ) of

		true ->
			ContentRoot;

		false ->
			% Must be relative to the default web root then:
			case MaybeBinDefaultWebRoot of

				undefined ->
					?error_fmt( "For the ~s, a relative content root was "
								"specified ('~s') whereas no default web root "
								"was defined.",
						[ describe_host( VHostId, DomainId ), ContentRoot ] ),
					throw( { relative_to_undefined_web_root, ContentRoot,
							 { DomainId, VHostId } } );

				_ ->
					file_utils:join( MaybeBinDefaultWebRoot, ContentRoot )

			end

	end,

	NormContentRoot = file_utils:normalise_path( ActualContentRoot ),

	case file_utils:is_existing_directory_or_link( NormContentRoot ) of

		true ->
			text_utils:string_to_binary( NormContentRoot );

		false ->
			HostDesc = describe_host( VHostId, DomainId ),
			?error_fmt( "For the ~s, the specified content root ('~s') "
						"does not exist.", [ HostDesc, NormContentRoot ] ),
			throw( { non_existing_content_root, NormContentRoot,
					 { DomainId, VHostId } } )

	end.



% Returns an absolute version of the corresponding meta content root, creating
% it if necessary.
%
-spec ensure_meta_content_root_exists( directory_path(),
	maybe( bin_directory_path() ), vhost_id(), domain_id(), wooper:state() ) ->
											 bin_directory_path().
ensure_meta_content_root_exists( ContentRoot, MaybeBinDefaultWebRoot, VHostId,
								 DomainId, State ) ->

	ActualContentRoot = case file_utils:is_absolute_path( ContentRoot ) of

		true ->
			ContentRoot;

		false ->
			% Must be relative to the default web root then:
			case MaybeBinDefaultWebRoot of

				undefined ->
					?error_fmt( "For the meta ~s, a relative content root was "
								"specified ('~s') whereas no default web root "
								"was defined.",
						[ describe_host( VHostId, DomainId ), ContentRoot ] ),
					throw( { meta_relative_to_undefined_web_root, ContentRoot,
							 { DomainId, VHostId } } );

				_ ->
					file_utils:join( MaybeBinDefaultWebRoot, ContentRoot )

			end

	end,

	NormContentRoot = file_utils:normalise_path( ActualContentRoot ),

	case file_utils:is_existing_directory_or_link( NormContentRoot ) of

		true ->
			ok;

		false ->
			HostDesc = describe_host( VHostId, DomainId ),
			?warning_fmt( "For the ~s, the specified meta content root ('~s') "
						  "does not exist, creating it.",
						  [ HostDesc, NormContentRoot ] ),
			file_utils:create_directory( NormContentRoot )

	end,

	text_utils:string_to_binary( NormContentRoot ).



% Returns a description of specified vhost spec.
describe_host( _VHostId=default_vhost_catch_all, DomainId ) ->

	DomainString = case DomainId of

		default_domain_catch_all ->
			"the domain catch-all";

		BinDomainName ->
			text_utils:format( "domain '~s'", [ BinDomainName ] )

	end,

	text_utils:format( "catch-all for all virtual hosts under ~s",
					   [ DomainString ] );

describe_host( VHostId, _DomainId=default_domain_catch_all ) ->
	text_utils:format( "virtual host '~s' for the domain catch-all",
					   [ VHostId ] );

describe_host( VHostId, BinDomainName ) ->
	text_utils:format( "virtual host '~s.~s'", [ VHostId, BinDomainName ] ).



% Returns a static dispatch route corresponding to the specified virtual host
% identifier, domain identifier and content root.
%
% See https://ninenines.eu/docs/en/cowboy/2.7/guide/routing/ for more details.
%
-spec get_static_dispatch_for( vhost_id(), domain_id(), bin_directory_path(),
							   logger_pid() ) -> route_rule().
get_static_dispatch_for( VHostId, DomainId, BinContentRoot, LoggerPid ) ->

	% We prepare, once for all, all settings for a given (virtual) host.

	% Refer to https://ninenines.eu/docs/en/cowboy/2.7/manual/cowboy_static/ and
	% https://ninenines.eu/docs/en/cowboy/2.7/guide/static_files/:

	% Plain string wanted, if not wildcard:
	HostMatch = case DomainId of

		default_domain_catch_all ->

			case VHostId of

				default_vhost_catch_all ->
					'_';

				BinVHostName when is_binary( BinVHostName ) ->
					% Ex: if BinVHostName="baz", "baz.foobar.org" is matching
					% (i.e. "baz.*.*"):
					%
					text_utils:format( "~s.:_.:_", [ BinVHostName ] )

			end;


		BinDomainName when is_binary( BinDomainName ) ->

			case VHostId of

				default_vhost_catch_all ->
					% text_utils:format( ":_.~s", [ BinDomainName ] );
					%
					% A little better (more general) than above (if
					% BinDomainName="foobar.org", matches only "*.foobar.org"),
					% as below is matching any number of subdomains (ex:
					% "*.*.*.foobar.org"):
					%
					text_utils:format( "[...].~s", [ BinDomainName ] );

				BinVHostName when is_binary( BinVHostName ) ->
					text_utils:format( "~s.~s",
									   [ BinVHostName, BinDomainName ] )

			end

	end,

	CssPath = file_utils:join( [ BinContentRoot, "common", "default.css" ] ),

	MaybeBinCssFile = case file_utils:is_existing_file_or_link( CssPath ) of

		true ->
			text_utils:string_to_binary( "common/default.css" );

		false ->
			undefined

	end,


	IconPath =
		file_utils:join( [ BinContentRoot, "images", "default-icon.png" ] ),

	MaybeBinIconFile = case file_utils:is_existing_file_or_link( IconPath ) of

		true ->
			text_utils:string_to_binary( "images/default-icon.png" );

		false ->
			undefined

	end,

	Image404Path = file_utils:join( [ BinContentRoot, "images", "404.png" ] ),

	MaybeBin404 = case file_utils:is_existing_file_or_link( Image404Path ) of

		true ->
			text_utils:string_to_binary( "images/404.png" );

		false ->
			undefined

	end,

	% No charset or etag enforced here, just the default:
	CowboyOpts = [ { mimetypes, cow_mimetypes, web } ],

	% Full map-based state, as rich as needed:
	InitialState = #{ content_root => BinContentRoot,
					  css_path => MaybeBinCssFile,
					  icon_path => MaybeBinIconFile,
					  image_404 => MaybeBin404,
					  cowboy_opts => CowboyOpts,
					  logger_pid => LoggerPid },

	BinIndex = text_utils:string_to_binary(
				 text_utils:format( "~s/index.html", [ BinContentRoot ] ) ),

	{ HostMatch, [

	  % Allows to expand a requested 'http://foobar.org' into
	  % 'http://foobar.org/index.html':
	  %
	  { "/", us_web_static, InitialState#{ type => file,
										   path => BinIndex } },

	  % Allows to serve all files (ex: HTML, images, CSS, etc.) from the
	  % specified tree:
	  %
	  { "/[...]", us_web_static, InitialState#{ type => directory,
												path => BinContentRoot } }

	] }.



% Checks specified web kind.
check_kind( _WebKind=static, _VHost, _DomainId, _State ) ->
	static;

check_kind( _WebKind=nitrogen, _VHost, _DomainId, _State ) ->
	nitrogen;

check_kind( WebKind, VHost, DomainId, State ) when is_atom( WebKind ) ->
	?error_fmt( "Unknown web kind '~s' specified for virtual host '~s' for "
				"domain '~s'.", [ WebKind, VHost, DomainId ] ),
	throw( { unknown_web_kind, WebKind, { VHost, DomainId } } );

check_kind( WebKind, VHost, DomainId, State ) ->
	?error_fmt( "Invalid web kind '~p' specified for virtual host '~s' for "
				"domain '~s'.", [ WebKind, VHost, DomainId ] ),
	throw( { invalid_web_kind_term, WebKind, { VHost, DomainId } } ).




% Manages any user-configured registration names for this instance, for the
% US-web server and their related services, which may be created here.
%
-spec manage_registrations( us_web_config_table(), wooper:state() ) ->
									  wooper:state().
manage_registrations( ConfigTable, State ) ->

	Scope = ?default_registration_scope,

	WebCfgSrvRegName = case table:lookup_entry(
				   ?us_web_config_server_registration_name_key, ConfigTable ) of

		key_not_found ->
			DefCfgRegName = ?default_us_web_config_server_registration_name,
			?info_fmt( "No user-configured registration name for the US-web "
					   "configuration server, defaulting to '~s'.",
					   [ DefCfgRegName ] ),
			DefCfgRegName;

		{ value, CfgRegName } when is_atom( CfgRegName ) ->
			?info_fmt( "Using user-configured registration name for the "
					   "US-web configuration server, '~s'.", [CfgRegName  ] ),
			CfgRegName;

		{ value, InvalidCfgRegName } ->
			?error_fmt( "Invalid user-specified registration name for the "
						"US-web configuration server: '~p'.",
						[ InvalidCfgRegName ] ),
			throw( { invalid_web_config_registration_name, InvalidCfgRegName,
					 ?us_web_config_server_registration_name_key } )

	end,

	naming_utils:register_as( WebCfgSrvRegName, Scope ),


	SchedRegName = case table:lookup_entry(
					 ?us_web_scheduler_registration_name_key, ConfigTable ) of

		key_not_found ->
			DefScRegName = ?default_us_web_scheduler_registration_name,
			?info_fmt( "No user-configured registration name for the US-web "
					   "scheduler, defaulting to '~s'.", [ DefScRegName ] ),
			DefScRegName;

		{ value, ScRegName } when is_atom( ScRegName ) ->
			?info_fmt( "Using user-configured registration name for the "
					   "US-web scheduler, '~s'.", [ ScRegName ] ),
			ScRegName;

		{ value, InvalidScRegName } ->
			?error_fmt( "Invalid user-specified registration name for the "
						"US-web scheduler: '~p'.", [ InvalidScRegName ] ),
			throw( { invalid_web_scheduler_registration_name, InvalidScRegName,
					 ?us_web_config_server_registration_name_key } )

	end,

	% Relatively private to this node:
	SchedPid = class_USScheduler:new_link( "USWebScheduler", SchedRegName,
										   _SchedRegScope=local_only ),

	?info_fmt( "This US-web configuration server was registered as '~s', while "
	   "its scheduler (~w) was registered as '~s' (scope in all cases: ~s).",
	   [ WebCfgSrvRegName, SchedPid, SchedRegName, Scope ] ),

	setAttributes( State, [
			% Inherited:
			{ registration_name, WebCfgSrvRegName },
			{ registration_scope, Scope },

			{ scheduler_registration_name, SchedRegName },
			{ us_web_scheduler_pid, SchedPid } ] ).



% Manages any user-configured specification regarding the (operating-system
% level) US user.
%
-spec manage_os_user( us_web_config_table(), wooper:state() ) -> wooper:state().
manage_os_user( ConfigTable, State ) ->

	% Mostly used by start/stop scripts:
	WebUsername = case table:lookup_entry( ?us_web_username_key,
										   ConfigTable ) of

		key_not_found ->
			ActualUsername = system_utils:get_user_name(),
			?info_fmt( "No user-configured US-web operating-system username "
					   "set for this server; runtime-detected: '~s'.",
					   [ ActualUsername ] ),
			ActualUsername;

		{ value, Username } when is_list( Username ) ->
			case system_utils:get_user_name() of

				Username ->
					?info_fmt( "Using user-configured US-web operating-system "
							   "username '~s' for this server, which matches "
							   "the current runtime user.", [ Username ] ),
					Username;

				OtherUsername ->
					?error_fmt( "The user-configured US-web operating-system "
								"username '~s' for this server does not match "
								"the current runtime user, '~s'.",
								[ Username, OtherUsername ] ),
					throw( { inconsistent_os_us_web_user, OtherUsername,
							 Username, ?us_web_username_key } )

			end

	end,

	setAttribute( State, ?us_web_username_key,
				  text_utils:string_to_binary( WebUsername ) ).



% Manages any user-configured application base directory, and set related
% directories.
%
-spec manage_app_base_directories( us_web_config_table(), wooper:state() ) ->
									   wooper:state().
manage_app_base_directories( ConfigTable, State ) ->

	% As opposed to, say, start/stop scripts, it may not be strictly necessary,
	% depending on the context (the US framework being also launchable thanks
	% to, for example, 'make debug'), for this Erlang part to know the
	% application base directory (see app_base_directory); so lacking it is not
	% a blocking error, even if it would be better to know it.

	RawBaseDir = case table:lookup_entry( ?us_web_app_base_dir_key,
										  ConfigTable ) of

		key_not_found ->

			case system_utils:get_environment_variable(
				   ?us_web_app_env_variable ) of

				false ->
					% Guessing then, typically current directory is:
					% [...]/us_web/_build/default/rel/us_web, and we want the
					% first us_web, so:
					%
					GuessedDir = file_utils:normalise_path( file_utils:join( [
						file_utils:get_current_directory(), "..", "..", "..",
						".." ] ) ),

					?warning_fmt( "No user-configured US-web application "
								  "base directory set (neither in-file nor "
								  "through the '~s' environment variable), "
								  "hence trying to guess it as '~s'.",
								  [ ?us_web_app_env_variable, GuessedDir ] ),
					GuessedDir;

				EnvDir ->
					?info_fmt( "No user-configured US-web application base "
							   "directory set in configuration file, using "
							   "the value of the '~s' environment variable: "
							   "'~s'.", [ ?us_web_app_env_variable, EnvDir ] ),
					EnvDir

			end;

		{ value, D } when is_list( D ) ->
			D;

		{ value, D } when is_binary( D ) ->
			file_utils:binary_to_string( D );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured US-web application base "
						"directory: '~p'.", [ InvalidDir ] ),
			throw( { invalid_us_web_app_base_directory, InvalidDir,
					 ?us_web_app_base_dir_key } )

	end,

	BaseDir = file_utils:ensure_path_is_absolute( RawBaseDir ),

	MaybeBaseBinDir =
			case file_utils:is_existing_directory_or_link( BaseDir ) of

		true ->
			% As it may end with a version (ex: "us_web-0.0.1") or as a
			% "us_web-latest" symlink thereof:
			%
			case filename:basename( BaseDir ) of

				"us_web" ++ _ ->
					?info_fmt( "US-web application base directory set to '~s'.",
							   [ BaseDir ] ),
					text_utils:string_to_binary( BaseDir );

				_Other ->
					?warning_fmt( "The US-web application base directory '~s' "
						"does not seem legit (it should end with 'us_web'), "
						"thus considering knowing none.", [ BaseDir ] ),
					%throw( { incorrect_us_web_app_base_directory, BaseDir,
					%		 ?us_web_app_base_dir_key } )
					undefined

			end;

		false ->
			?warning_fmt( "The US-web application base directory '~s' does not "
						  "exist, thus considering knowing none.",
						  [ BaseDir ] ),
			%throw( { non_existing_us_web_app_base_directory, BaseDir,
			%		 ?us_web_app_base_dir_key } )
			undefined

	end,

	% The internal US-web directory (see conf_directory) used to be derived from
	% the app base one (as a 'conf' subdirectory thereof), yet because of that
	% it was not included in releases. So instead this 'conf' directory is a
	% subdirectory of 'priv':
	%
	% (for some reason, using this module, although it is listed in us_web.app,
	% results with code:priv_dir/1 in a bad_name exception)
	%
	%TargetMod = ?MODULE,
	%TargetMod = us_web_app,
	TargetMod = us_web_sup,

	ConfBinDir = text_utils:string_to_binary( file_utils:join(
					 otp_utils:get_priv_root( TargetMod, _BeSilent=true ), "conf" ) ),

	% Set in all cases:
	setAttributes( State, [ { app_base_directory, MaybeBaseBinDir },
							{ conf_directory, ConfBinDir } ] ).



% Manages any user-configured data directory to rely on, creating it if
% necessary.
%
-spec manage_data_directory( us_web_config_table(), wooper:state() ) ->
								   wooper:state().
manage_data_directory( ConfigTable, State ) ->

	BaseDir = case table:lookup_entry( ?us_web_data_dir_key, ConfigTable ) of

		key_not_found ->
			?default_data_base_dir;

		{ value, D } when is_list( D ) ->
			file_utils:ensure_path_is_absolute( D,
												?getAttr(app_base_directory) );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured data directory: '~p'.",
						[ InvalidDir ] ),
			throw( { invalid_data_directory, InvalidDir,
					 ?us_web_data_dir_key } )

	end,

	case file_utils:is_existing_directory( BaseDir ) of

		true ->
			ok;

		false ->
			?warning_fmt( "The base data directory '~s' does not exist, "
						  "creating it.", [ BaseDir ] )

	end,

	% Would lead to inconvenient paths, at least if defined as relative:
	%DataDir = file_utils:join( BaseDir, ?app_subdir ),
	DataDir = BaseDir,

	try

		file_utils:create_directory_if_not_existing( DataDir, create_parents )

	catch

		{ create_directory_failed, _DataDir, eacces } ->

			% Clearer than system_utils:get_user_name_string/0:
			Username = system_utils:get_user_name(),

			?error_fmt( "Unable to create the directory for working data '~s': "
				"please ensure its parent directory can be written by user "
				"'~s', or set it to different path thanks to the '~s' key.",
				[ DataDir, Username, ?us_web_data_dir_key ] ),

			throw( { data_directory_creation_failed, DataDir, eacces,
					 Username } );

		E ->
			throw( { data_directory_creation_failed, DataDir, E } )

	end,

	% Enforce security in all cases ("chmod 700"); if it fails here, the
	% combined path/user configuration must be incorrect; however we might not
	% be the owner of that directory (ex: if the us-web user is different from
	% the us one)
	%
	% So:
	%
	CurrentUserId = system_utils:get_user_id(),

	case file_utils:get_owner_of( DataDir ) of

		CurrentUserId ->
			file_utils:change_permissions( DataDir,
			  [ owner_read, owner_write, owner_execute,
				group_read, group_write, group_execute ] );

		% Not owned, do nothing:
		_OtherId ->
			ok

	end,

	BinDataDir = text_utils:string_to_binary( DataDir ),

	setAttribute( State, data_directory, BinDataDir ).



% Manages any user-configured log directory to rely on, creating it if
% necessary.
%
-spec manage_log_directory( us_web_config_table(), wooper:state() ) ->
							   wooper:state().
manage_log_directory( ConfigTable, State ) ->

	BaseDir = case table:lookup_entry( ?us_web_log_dir_key, ConfigTable ) of

		key_not_found ->
			?default_log_base_dir;

		{ value, D } when is_list( D ) ->
			file_utils:ensure_path_is_absolute( D,
												?getAttr(app_base_directory) );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured log directory: '~p'.",
						[ InvalidDir ] ),
			throw( { invalid_log_directory, InvalidDir, ?us_web_log_dir_key } )

	end,

	case file_utils:is_existing_directory( BaseDir ) of

		true ->
			ok;

		false ->
			?error_fmt( "The base log directory '~s' does not exist.",
						[ BaseDir ] ),
			throw( { non_existing_base_log_directory, BaseDir } )

	end,

	% Would lead to inconvenient paths, at least if defined as relative:
	%LogDir = file_utils:join( BaseDir, ?app_subdir ),
	LogDir = BaseDir,

	file_utils:create_directory_if_not_existing( LogDir ),

	% Enforce security in all cases ("chmod 700"); if it fails here, the
	% combined path/user configuration must be incorrect; however we might not
	% be the owner of that directory (ex: if the us-web user is different from
	% the us one)
	%
	% So:
	%
	CurrentUserId = system_utils:get_user_id(),

	case file_utils:get_owner_of( LogDir ) of

		CurrentUserId ->
			file_utils:change_permissions( LogDir,
			  [ owner_read, owner_write, owner_execute,
				group_read, group_write, group_execute ] );

		% Not owned, do nothing:
		_OtherId ->
			ok

	end,

	BinLogDir = text_utils:string_to_binary( LogDir ),

	setAttribute( State, log_directory, BinLogDir ).



% Manages any user-configured default web root.
-spec manage_web_root( us_web_config_table(), wooper:state() ) ->
		  wooper:state().
manage_web_root( ConfigTable, State ) ->

	MaybeBinDefWebRoot = case table:lookup_entry( ?default_web_root_key,
												  ConfigTable ) of

		key_not_found ->
			?info( "No default web root defined." ),
			undefined;

		{ value, DefWebRoot } ->
			AbsDefaultWebRoot =
					file_utils:ensure_path_is_absolute( DefWebRoot ),

			case file_utils:is_existing_directory( AbsDefaultWebRoot ) of

				true ->
					?info_fmt( "Default web root set to '~s'.",
							   [ AbsDefaultWebRoot ] ),

					text_utils:string_to_binary( AbsDefaultWebRoot );

				false ->
					?error_fmt( "The user-specified default web root "
								"(obtained from the 'default_web_root' key), "
								"'~s', is not an existing directory.",
								[ AbsDefaultWebRoot ] ),

					throw( { non_existing_default_web_root,
							 AbsDefaultWebRoot, default_web_root } )

			end

	end,

	setAttribute( State, default_web_root, MaybeBinDefWebRoot ).




% Manages any user-configured TCP port.
-spec manage_port( us_web_config_table(), wooper:state() ) -> wooper:state().
manage_port( ConfigTable, State ) ->

	TCPPort = case table:lookup_entry( ?http_tcp_port_key, ConfigTable ) of

		key_not_found ->
			DefaultHttpPort = 80,
			?info_fmt( "No user-specified HTTP TCP port, defaulting to #~B.",
					   [ DefaultHttpPort ] ),
			DefaultHttpPort;

		{ value, Port } when is_integer( Port ) ->
			?info_fmt( "The user-specified HTTP TCP port is #~B.",
					   [ Port ] ),
			Port;

		{ value, InvalidPort } ->
			?error_fmt( "Invalid user-specified HTTP TCP port: '~p'.",
						[ InvalidPort ] ),
			throw( { invalid_http_port, InvalidPort, http_tcp_port } )

	end,

	setAttribute( State, http_tcp_port, TCPPort ).



% Prepares the meta support.
-spec manage_pre_meta( us_web_config_table(), wooper:state() ) ->
							 wooper:state().
manage_pre_meta( ConfigTable, State ) ->

	case table:lookup_entry( ?log_analysis_key, ConfigTable ) of

		key_not_found ->
			?info( "No analysis of web access logs requested." ),
			State;

		{ value, LogSettings } ->
			% To have the 'log_analysis_settings' attribute ready:
			set_log_tool_settings( LogSettings, State )

	end.



% Determines the settings of the web log analysis tool.
set_log_tool_settings( undefined, State ) ->
	set_log_tool_settings( _DefaultToolName=awstats, State );

set_log_tool_settings( ToolName, State ) when is_atom( ToolName ) ->
	set_log_tool_settings( { ToolName, _DefaultAnalysisToolRoot=undefined,
							 _DefaultAnalysisHelperRoot=undefined },
						   State );

% At least Arch Linux defaults:
set_log_tool_settings( { awstats, _MaybeAnalysisToolRoot=undefined,
					   _MaybeAnalysisHelperRoot=undefined }, State ) ->
	set_log_tool_settings( { awstats, _ToolRoot="/usr/share/awstats/tools",
					 _HelperRoot="/usr/share/webapps/awstats" }, State );

% Supposing both are defined or not:
set_log_tool_settings( { ToolName, AnalysisToolRoot, AnalysisHelperRoot },
					   State ) when is_list( AnalysisToolRoot )
									andalso  is_list( AnalysisHelperRoot ) ->

	BinAnToolRoot =
			case file_utils:is_existing_directory_or_link( AnalysisToolRoot ) of

		true ->
			text_utils:string_to_binary( AnalysisToolRoot );

		false ->
			?error_fmt( "Root directory '~s' for web log analysis tool ('~s') "
						"not found.", [ AnalysisToolRoot, ToolName ] ),
			throw( { root_of_log_tool_not_found, ToolName, AnalysisToolRoot } )

	end,

	BinAnHelperRoot =
			case file_utils:is_existing_directory_or_link( AnalysisHelperRoot ) of

		true ->
			text_utils:string_to_binary( AnalysisHelperRoot );

		false ->
			?error_fmt( "Root directory '~s' for web log analysis helper "
				"('~s') not found.", [ AnalysisHelperRoot, ToolName ] ),
			throw( { root_of_log_helper_not_found, ToolName, AnalysisHelperRoot } )

	end,

	setAttribute( State, log_analysis_settings,
				  { ToolName, BinAnToolRoot, BinAnHelperRoot } );


set_log_tool_settings( Unexpected, State ) ->
	?error_fmt( "Unexpected settings for the web log analysis tool: ~p.",
				[ Unexpected ] ),
	throw( { unexpected_log_tool_settings, Unexpected } ).



% Manages user-configured web dispatch routes.
-spec manage_routes( us_web_config_table(), wooper:state() ) -> wooper:state().
manage_routes( ConfigTable, State ) ->

	BinLogDir = ?getAttr(log_directory),

	UserRoutes = case table:lookup_entry( ?routes_key, ConfigTable ) of

		key_not_found ->
			?warning( "No user-specified dispatch route, defaulting to "
					  "none specific (probably useless then)." ),
			[];

		{ value, Routes } when is_list( Routes ) ->
			?info_fmt( "Using user-specified dispatch routes:~n~p",
					   [ Routes ] ),
			Routes;

		{ value, InvalidRoutes } ->
			?error_fmt( "Invalid (non-list) user-specified dispatch routes:"
						"~n~p", [ InvalidRoutes ] ),
			throw( { invalid_dispatch_routes, InvalidRoutes, routes } )

	end,

	SchedulerPid = ?getAttr(us_web_scheduler_pid),

	% Only routes expected to remain (so we check that no extraneous entry
	% remains):
	%
	{ DomainTable, DispatchRoutes, ProcessState } = process_domain_info(
		UserRoutes, BinLogDir, ?getAttr(default_web_root),
		?getAttr(log_analysis_settings), SchedulerPid, State ),

	% Now that all loggers are created, we gather their PID so that they are
	% managed by a task ring, in order to synchronise them properly:
	%
	AllLoggerPids = get_all_logger_pids_from( DomainTable ),

	basic_utils:check_all_defined( AllLoggerPids ),

	TaskPeriodicity = class_USWebLogger:get_default_log_rotation_periodicity(),

	TaskRingPid = class_USTaskRing:new_link( _RingName="USWebLoggerRing",
		_Actuators=AllLoggerPids, _TaskRequestName=rotateLogsSynch,
		_TaskRequestArgs=[], TaskPeriodicity, _ScheduleCount=unlimited,
		SchedulerPid ),

	DispatchRules = cowboy_router:compile( DispatchRoutes ),

	setAttributes( ProcessState, [ { domain_config_table, DomainTable },
								   { dispatch_rules, DispatchRules },
								   { logger_task_ring, TaskRingPid } ] ).



% Applies the meta support.
-spec manage_post_meta( wooper:state() ) -> wooper:state().
manage_post_meta( State ) ->

	% Is meta enabled?
	case ?getAttr(meta_web_settings) of

		undefined ->
			State;

		MetaWebSettings={ _DomainId, _BinVHost, BinMetaContentRoot } ->

			% Is web analysis enabled?
			LogAnalysisEnabled = case ?getAttr(log_analysis_settings) of

				undefined ->
					false;

				{ _ToolName=awstats, _BinAnalysisToolRoot,
				  BinAnalysisHelperRoot } ->

					% Copies first, in meta web root (now known), the icons to
					% be used by the generated pages:
					%
					IconDir = file_utils:join( BinAnalysisHelperRoot, "icon" ),

					true = file_utils:is_existing_directory_or_link( IconDir ),

					% BinMetaContentRoot already known to exist:
					try

						file_utils:copy_tree( IconDir, BinMetaContentRoot )

					catch

						E={ copy_tree_failed, _PathPair, _ExitCode=1,
							ErrorOutput } ->
							trace_utils:error_fmt(
							  "Unable to copy Awstats icons: "
							  "one may try \"chmod -R +r '~s'\" to fix "
							  "permissions.~nError message was: ~s.",
							  [ IconDir, ErrorOutput ] ),
							throw( E )

					end,
					true

			end,

			generate_meta( MetaWebSettings, LogAnalysisEnabled,
				?getAttr(http_tcp_port), ?getAttr(domain_config_table),
				State )

	end.



% Generates the meta website.
-spec generate_meta( meta_web_settings(), boolean(), net_utils:tcp_port(),
					 domain_config_table(), wooper:state() ) -> wooper:state().
generate_meta( MetaWebSettings={ _DomainId, _BinVHost, BinMetaContentRoot },
			   LogAnalysisEnabled, Port, DomainCfgTable, State ) ->

	?trace_fmt( "Generating meta website in '~s'.", [ BinMetaContentRoot ] ),

	IndexPath = file_utils:join( BinMetaContentRoot, "index.html" ),

	IndexContent = us_web_meta:get_page_header()
		++ us_web_meta:get_page_body( Port, DomainCfgTable,
			?getAttr(start_timestamp), MetaWebSettings, LogAnalysisEnabled )
		++ us_web_meta:get_page_footer(),

	file_utils:write_whole( IndexPath, IndexContent ),

	State.



% Returns a textual description of this domain table.
-spec domain_table_to_string( domain_config_table() ) -> text_utils:ustring().
domain_table_to_string( DomainTable ) ->
	"domain configuration " ++ table:to_string( DomainTable ).


% Returns a list of all the PIDs of the webloggers.
-spec get_all_logger_pids( wooper:state() ) -> [ logger_pid() ].
get_all_logger_pids( State ) ->
	get_all_logger_pids_from( ?getAttr(domain_config_table) ).



% (helper)
get_all_logger_pids_from( DomainCfgTable ) ->
	list_utils:flatten_once( [ [ VHCfgE#vhost_config_entry.logger_pid
							   || VHCfgE <- table:values( VHCfgTable ) ]
						 || VHCfgTable <- table:values( DomainCfgTable ) ] ).



% Returns a textual description of this server.
-spec to_string( wooper:state() ) -> text_utils:ustring().
to_string( State ) ->

	WebRootString = case ?getAttr(default_web_root) of

		undefined ->
			"not using a default web root";

		WebRoot ->
			text_utils:format( "using default web root '~s'", [ WebRoot ] )

	end,

	SrvString = case ?getAttr(us_server_pid) of

		undefined ->
			"no US server";

		SrvPid ->
			text_utils:format( "US server ~w", [ SrvPid ] )

	end,

	text_utils:format( "US-web configuration ~s, using for the http "
		"scheme the TCP port #~B, ~s, running in the ~s execution context, "
		"knowing: ~s, US overall configuration server ~w and "
		"OTP supervisor ~w, relying on the '~s' configuration directory and "
		"on the '~s' log directory.~n~nUsing ~s~n~n"
		"Corresponding dispatch rules:~n~p",
		[ class_USServer:to_string( State ), ?getAttr(http_tcp_port),
		  WebRootString, ?getAttr(execution_context), SrvString,
		  ?getAttr(us_config_server_pid), ?getAttr(us_web_supervisor_pid),
		  ?getAttr(config_base_directory), ?getAttr(log_directory),
		  domain_table_to_string( ?getAttr(domain_config_table) ),
		  ?getAttr(dispatch_rules) ] ).