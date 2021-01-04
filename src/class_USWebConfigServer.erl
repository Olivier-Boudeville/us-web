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


-module(class_USWebConfigServer).


-define( class_description,
		 "Singleton server holding the configuration information of the "
		 "US-Web framework." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).



% The various kinds of websites:
% - static: a basic, static website to be served (the default)
% - meta: a website generated, if requested, by US-Web, to browse conveniently
% all the other hosted websites
% - nitrogen: a Nitrogen-based, dynamic website (see
% http://nitrogenproject.com/)
%
-type web_kind() :: 'static' | 'meta' | 'nitrogen'.


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


% The default registration name of the US-Web server:
-define( us_web_config_server_registration_name_key,
		 us_web_config_server_registration_name ).

% The default registration name of the US-Web scheduler:
-define( us_web_scheduler_registration_name_key,
		 us_web_scheduler_registration_name ).

-define( us_web_username_key, us_web_username ).
-define( us_web_app_base_dir_key, us_web_app_base_dir ).
-define( us_web_data_dir_key, us_web_data_dir ).
-define( us_web_log_dir_key, us_web_log_dir ).
-define( http_tcp_port_key, http_tcp_port ).
-define( https_tcp_port_key, https_tcp_port ).
-define( default_web_root_key, default_web_root ).
-define( log_analysis_key, log_analysis ).
-define( certificate_support_key, certificate_support ).
-define( certificate_mode_key, certificate_mode ).
-define( routes_key, routes ).



% All known, licit keys for the US-Web configuration file:
-define( known_config_keys, [ ?us_web_config_server_registration_name_key,
			?us_web_scheduler_registration_name_key,
			?us_web_username_key, ?us_web_app_base_dir_key,
			?us_web_data_dir_key, ?us_web_log_dir_key,
			?http_tcp_port_key, ?https_tcp_port_key,
			?default_web_root_key, ?log_analysis_key,
			?certificate_support_key, ?certificate_mode_key, ?routes_key ] ).


% The last-resort environment variable:
-define( us_web_app_env_variable, "US_WEB_APP_BASE_DIR" ).


% Preferring a default local directory to an absolute one requiring privileges:
%-define( default_data_base_dir, "/var/local/us-web/data" ).
-define( default_data_base_dir, "us-web-data" ).

% The subdirectory of the US-Web application directory in the one designated by
% us_web_data_dir_key:
%
-define( app_subdir, "us-web" ).


-define( default_log_base_dir, "/var/log/universal-server/us-web" ).


% The subdirectory of the US-Web log directory storing all web logs (access and
% error logs notably).
%
-define( web_log_subdir, "web-logs" ).


% The default, single, filename of the TLS private key file (stored in
% cert_directory) to be used by all LEEC agents (through certicate managers):
%
-define( leec_key_filename, "us-web-leec-agent-private.key" ).



% Design notes:
%
% This US-Web configuration server will ensure that an overall US configuration
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
% framework isolation, it will be dedicated to US-Web, and by default not be
% shared with the Universal Server, so that they can operate independently.

% Note that we chose that the analysis of log web access logs (typically based
% on Awstats) can be enabled only if the 'meta' website is itself enabled (but
% meta can exist with no log analysis).

% We used to define web logger instances that were autonomous with regard to
% their scheduling for log rotation (and thus possibly log analysis
% processing). However it may result in too large spikes of resource uses (as by
% default all web loggers are created roughly at the same time and thus would
% stay mostly in sync all through their periodical scheduling), and moreover at
% least some tools (ex: Awstats) may not be "reentrant", i.e. may not support
% concurrent accesses that could derive from unsynchronised loggers (however the
% Awstate state files, typically in /var/local/us-web/data, such as
% awstats052020.baz.foo.bar.org.txt suggest a per-vhost state file, hence
% possibly no problem).
%
% As a result, now these web loggers all belong to the same task ring, in charge
% of pacing them uniformly and of ensuring they cannot overlap.
%
% Two separate actions are to be considered, at least for Awstats:
%
% - updating its database based on access logs: done when rotating them, using
% awstats.pl (and not requesting the generation of a report)
%
% - generating HTML reports from the current state of the database: now done
% only on request, as a whole, through awstats_buildstaticpages.pl (no database
% update requested then)



% Implementation notes:
%
% The static routes, logger instances and generation of configuration files for
% any log analysis tool are finely interleaved, as defining a route requires its
% logger PID, which requires the corresponding configuration file.
%
% The US-Web configuration server creates a scheduler, for its own use (ex: for
% certificate renewal and the task ring regarding web loggers), and possibly for
% other related services.



% To identify a domain name (or a catch-all for them):
-type domain_id() :: bin_domain_name() | 'default_domain_catch_all'.

% The domain-specific information kept by this server:
-type domain_info() :: {

				   % The identifier of this domain (intentional duplicate):
				   domain_id(),

				   % The certificate manager (if any) corresponding to this
				   % domain:
				   %
				   maybe( cert_manager_pid() ),

				   %  The table storing the configuration of the virtual hosts
				   %  within this domain:
				   %
				   vhost_config_table() }.


% A table, associating to each domain name (ex: <<"foo.org">>), the
% configuration table regarding its virtual hosts (ex: regarding "bar.foo.org",
% a <<"bar">> key would correspond, associated to its vhost_config() value):
%
-type domain_config_table() :: table( domain_id(), domain_info() ).


% A table, associating, to each virtual host identifier, its settings:
-type vhost_config_table() :: table( vhost_id(), vhost_config_entry() ).


-type vhost_config_entry() :: #vhost_config_entry{}.

-type meta_web_settings() ::
		maybe( { domain_id(), vhost_id(), bin_directory_path() } ).


% General web settings, typically useful for the us_web supervisor:
-type general_web_settings() :: { dispatch_rules(), maybe( tcp_port() ),
		maybe( tcp_port() ), cert_support(), maybe( sni_info() ),
		maybe( bin_file_path() ), maybe( bin_file_path() ) }.


% Notably for us_web_meta:
-export_type([ domain_id/0, domain_config_table/0, vhost_config_table/0,
			   vhost_config_entry/0, meta_web_settings/0 ]).


-type path_match() :: text_utils:any_string().

% Not exported:
%-type route_rule() :: cowboy_router:route_rule().
-type route_rule() :: term().

-type dispatch_routes() :: cowboy_router:dispatch_routes().


% See https://ninenines.eu/docs/en/cowboy/2.8/guide/routing/:
-type dispatch_rules() :: cowboy_router:dispatch_rules().


% A table holding US-Web configuration information:
-type us_web_config_table() :: table( atom(), term() ).


% Settings about any web access log analysis, i.e. our canonical name for the
% analysis tool, its (main) root directory and the directory of this helper (if
% any were specified): {ToolName, MaybeAnalysisToolRoot,
% MaybeAnalysisHelperRoot}.
%
-type log_analysis_settings() :: { log_analysis_tool_name(),
			maybe( bin_directory_path() ), maybe( bin_directory_path() ) }.


% Tells whether certificates shall be used, and how:
-type cert_support() :: 'no_certificates' % Hence no https
					  | 'use_existing_certificates' % Hence use, but no renewal
					  | 'renew_certificates'. % Hence use, generate and renew


% Tells whether certificate generation is in testing/staging mode or not:
-type cert_mode() :: 'development' | 'production'.


% To silence attribute-only types:
-export_type([ dispatch_rules/0, log_analysis_settings/0,
			   cert_support/0, cert_mode/0 ]).


% User-specified tuple for a virtual host in the context of a domain:
-type vhost_info() :: { vhost_id(), directory_path() }
					| { vhost_id(), directory_path(), web_kind() }.


% Shorthands:

-type ustring() :: text_utils:ustring().

-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type bin_file_path() :: file_utils:bin_file_path().

-type domain_name() :: net_utils:domain_name().
-type bin_domain_name() :: net_utils:bin_domain_name().


%-type bin_subdomain() :: net_utils:bin_subdomain().
-type bin_host_name() :: net_utils:bin_host_name().
-type tcp_port() :: net_utils:tcp_port().

-type supervisor_pid() :: otp_utils:supervisor_pid().
-type application_run_context() :: otp_utils:application_run_context().


%-type server_pid() :: class_UniversalServer:server_pid().

-type logger_pid() :: class_USWebLogger:logger_pid().

-type cert_manager_pid() :: class_USCertificateManager:manager_pid().
-type sni_info() :: class_USCertificateManager:sni_info().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().

-type bin_san() :: letsencrypt:bin_san().



% The class-specific attributes:
-define( class_attributes, [

	{ execution_context, basic_utils:execution_context(),
	  "tells whether this server is to run in development or production mode" },

	% As it impacts at least various paths:
	{ app_run_context, application_run_context(),
	  "tells how US-Web is run, natively (using the Ceylan build/run system) "
	  "or as an OTP release" },

	{ domain_config_table, domain_config_table(),
	  "a table containing all configuration information related to a given "
	  "domain" },

	{ default_web_root, maybe( bin_directory_path() ),
	  "the default root (if any) for the website trees" },

	{ meta_web_settings, meta_web_settings(),
	  "the domain, virtual host identifiers and web root of the auto-generated "
	  "meta website (if any)" },

	{ http_tcp_port, maybe( tcp_port() ),
	  "the TCP port (if any) at which the webserver is to listen for the http "
	  "scheme" },

	{ https_tcp_port, maybe( tcp_port() ),
	  "the TCP port (if any) at which the webserver is to listen for the https "
	  "scheme" },

	{ us_server_pid, maybe( server_pid() ),
	  "the PID of the associated US server (if any)" },

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" },

	{ us_web_scheduler_pid, scheduler_pid(),
	  "the PID of the US-Web dedicated scheduler (at least for certificate
	  renewal, for task ring or possibly directly for web loggers)" },

	{ logger_task_ring, class_USTaskRing:ring_pid(),
	  "the PID of the task ring in charge of sequencing the webloggers" },

	{ us_web_username, basic_utils:user_name(),
	  "the user (if any) who shall launch the US web application" },

	{ us_web_supervisor_pid, supervisor_pid(),
	  "the PID of the OTP supervisor of US-Web, as defined in us_web_sup" },

	{ dispatch_rules, dispatch_rules(),
	  "the Cowboy dispatch rules corresponding to the configuration" },

	{ config_base_directory, bin_directory_path(),
	  "the base directory where all US configuration is to be found "
	  "(not the us_web/priv/conf internal directory)"},

	{ app_base_directory, bin_directory_path(),
	  "the base directory of the US-Web application (the root from whence "
	  "src, priv, ebin, etc. can be found" },

	{ conf_directory, bin_directory_path(),
	  "the US-Web internal configuration directory, 'us_web/priv/conf'" },

	{ data_directory, bin_directory_path(),
	  "the directory where working data (ex: the database state of a log tool, "
	  "or temporary TLS keys) is to be stored" },

	{ log_directory, bin_directory_path(), "the directory where (non-VM) US-Web
	  logs shall be written, notably access and error logs for websites (in its
	  ?web_log_subdir directory); traces are stored there as well" },

	{ cert_support, cert_support(),
	  "tells whether the use and possibly generation/renewal of X.509 "
	  "certificates is requested" },

	{ cert_mode, maybe( cert_mode() ),
	  "tells whether certificates (if any) are in staging or production mode" },

	{ cert_directory, maybe( bin_directory_path() ),
	  "the directory where the certificate information will be written" },

	{ leec_agents_key_path, maybe( bin_file_path() ),
	  "the absolute path to the common TLS private key file (if any) bound to "
	  "the common ACME account to be used by all the LEEC agents driven by "
	  "the certificate managers" },

	{ dh_key_path, maybe( bin_file_path() ),
	  "the absolute path to the Diffie-Helman key file (if any) to ensure "
	  "a very secure key exchange with Forward Secrecy" },

	{ ca_cert_key_path, maybe( bin_file_path() ),
	  "the absolute path to the certificate authority PEM file (if any) "
	  "for the chain of trust" },

	{ sni_info, maybe( sni_info() ),
	  "information regarding Server Name Indication, so that virtual hosts "
	  "can be supported with https" },

	{ scheduler_registration_name, naming_utils:registration_name(),
	  "the name under which the dedicated scheduler is registered" },

	{ start_timestamp, time_utils:timestamp(),
	  "the start timestamp of this server" },

	{ log_analysis_settings, maybe( log_analysis_settings() ),
	  "the settings to use for any analysis of web access logs" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Web.Configuration" ).


-define( default_us_web_config_server_registration_name, us_web_config_server ).
-define( default_us_web_scheduler_registration_name, us_web_scheduler ).

-define( default_registration_scope, global_only ).



% Exported helpers:
-export([ get_execution_target/0 ]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").

% For the tls_private_key record:
-include_lib("leec/include/letsencrypt.hrl").



% Constructs the US-Web configuration server.
%
% SupervisorPid is the PID of the main US-Web OTP supervisor, and AppRunContext
% tells how US-Web is being run.
%
-spec construct( wooper:state(), supervisor_pid(),
				 application_run_context() ) -> wooper:state().
construct( State, SupervisorPid, AppRunContext ) ->

	StartTimestamp = time_utils:get_timestamp(),

	TraceCateg = ?trace_categorize("Configuration Server"),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State, TraceCateg, _TrapExits=true ),

	% Allows functions provided by lower-level libraries (ex: LEEC) called
	% directly from this instance process to plug to the same (trace aggregator)
	% bridge, with the same settings:
	%
	class_TraceEmitter:register_bridge( TraceState ),

	?send_info_fmt( TraceState, "Creating a US-Web configuration server, "
		"running ~s.",
		[ otp_utils:application_run_context_to_string( AppRunContext ) ] ),

	?send_debug_fmt( TraceState, "Running Erlang ~s, whose ~s",
		[ system_utils:get_interpreter_version(),
		  code_utils:get_code_path_as_string() ] ),

	?send_debug_fmt( TraceState, "System description: ~s",
		[ system_utils:get_system_description() ] ),

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
							 "configuration directory: ~s", [ CfgMsg ] ),
			throw( us_configuration_directory_not_found );

		{ FoundCfgDir, CfgMsg } ->
			?send_info( TraceState, CfgMsg ),
			FoundCfgDir

	end,

	% Other attributes set by the next function:
	SupState = setAttributes( TraceState, [
					{ app_run_context, AppRunContext },
					{ us_web_supervisor_pid, SupervisorPid },
					{ start_timestamp, StartTimestamp },
					{ cert_directory, undefined },
					{ leec_agents_key_path, undefined },
					{ dh_key_path, undefined },
					{ ca_cert_key_path, undefined } ] ),

	CfgState = load_and_apply_configuration( BinCfgDir, SupState ),

	?send_info( CfgState, "Constructed: " ++ to_string( CfgState ) ),

	% Done rather late on purpose, so that the existence of that file can be
	% seen as a sign that the initialisation went well (used by start-us-web.sh)
	%
	% Now that the log directory is known, we can properly redirect the traces.
	% Already a trace emitter:

	NewBinTraceFilePath = text_utils:string_to_binary(
			file_utils:join( getAttribute( CfgState, log_directory ),
							 "us_web.traces" ) ),

	?send_debug_fmt( CfgState, "Requesting the renaming of trace file to '~s'.",
					 [ NewBinTraceFilePath ] ),

	getAttribute( CfgState, trace_aggregator_pid ) !
		{ renameTraceFile, NewBinTraceFilePath },

	CfgState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug( "Deletion initiated." ),

	% Now web loggers are not registered directly to the scheduler, so we just
	% have to take care of the task ring:
	%
	% (no synchronicity needed)
	%
	?getAttr(logger_task_ring) ! delete,

	[ LPid ! delete || LPid <- get_all_logger_pids( State ) ],

	% Includes the LEEC FSM shutdown:
	[ CMPid ! delete
	  || CMPid <- get_all_certificate_manager_pids( State ) ],

	?info( "Deleted." ),
	State.




% Method section.


% Returns basic, general web configuration settings (typically for the us_web
% supervisor).
%
-spec getWebConfigSettings( wooper:state() ) ->
								const_request_return( general_web_settings() ).
getWebConfigSettings( State ) ->

	CertSupport = ?getAttr(cert_support),

	% HTTPS port only returned if https enabled:
	MaybeHTTPSTCPPort = case CertSupport of

		no_certificates ->
			undefined;

		_ ->
			% Maybe still undefined:
			?getAttr(https_tcp_port)

	end,

	GenWebSettings = { ?getAttr(dispatch_rules), ?getAttr(http_tcp_port),
		MaybeHTTPSTCPPort, CertSupport, ?getAttr(sni_info),
		?getAttr(dh_key_path), ?getAttr(ca_cert_key_path) },

	?debug_fmt( "Returning the general web configuration settings:~n  ~p",
				[ GenWebSettings ] ),

	wooper:const_return_result( GenWebSettings ).



% Helper section.


% Loads and applies the relevant configuration settings first from the overall
% US configuration file, then from the more web/vhost specific one.
%
% As a result, the US configuration file is not fully checked as such (ex: no
% extracting and check that no entry remains), we just select the relevant
% information from it.
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
	CfgServerPid =
			case naming_utils:is_registered( USCfgRegName, USCfgRegScope ) of

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
			  "server, using it: ~s registration look-up for '~s' returned ~w.",
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
					?error_fmt( "Mismatching US configuration directories: had "
					  "'~s', yet received from US configuration server: '~s'.",
					  [ BinCfgDir, RecvBinCfgDir ] ),

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



% Loads web configuration information (i.e. the US-Web configuration file, as
% identified from the US one), notably about virtual hosts.
%
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
				"(ex: symbolic link to an inaccessible file); tried '~s'.",
				[ WebCfgFilePath ] ),
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

	PortState = manage_ports( WebCfgTable, LogState ),

	RootState = manage_web_root( WebCfgTable, PortState ),

	PreMetaState = manage_pre_meta( WebCfgTable, RootState ),

	CertState = manage_certificates( WebCfgTable, PreMetaState ),

	RouteState = manage_routes( WebCfgTable, CertState ),

	PostMetaState = manage_post_meta( RouteState ),

	LicitKeys = ?known_config_keys,

	case list_utils:difference( table:keys( WebCfgTable ), LicitKeys ) of

		[] ->
			PostMetaState;

		UnexpectedKeys ->
			?error_fmt( "Unknown key(s) in '~s': ~s~nLicit keys: ~s",
				[ WebCfgFilePath, text_utils:terms_to_string( UnexpectedKeys ),
				  text_utils:terms_to_string( LicitKeys ) ] ),
			throw( { invalid_configuration_keys, UnexpectedKeys,
					 text_utils:binary_to_string( WebCfgFilePath ) } )

	end.



% Creates a certificate manager for the specified domain (ex: foobar.org),
% including for all its virtual hosts (ex: baz.foobar.org), as SANs (Subject
% Alternative Names).
%
% We used to create one single, standalone certificate per virtual host, yet the
% Let's Encrypt rate limits (see https://letsencrypt.org/docs/rate-limits/)
% could quite easily be hit (ex: if having a total of more than 50 virtual hosts
% and/or domains).
%
% So now we create only certificates at the domain level - hence a certificate
% manager per domain, which lists all its corresponding virtual hosts as SANs;
% however a per-virtual host dedicated dispatch route must still be created
% (pointing to this certificate manager), as the ACME server will validate each
% of these SANs with a dedicated challenge: the corresponding, upcoming ACME
% http access must be intercepted and the corresponding token will have then to
% be returned.
%
-spec handle_certificate_manager_for( bin_domain_name(), [ bin_san() ],
		cert_support(), cert_mode(), bin_directory_path(),
		maybe( bin_file_path() ), scheduler_pid() ) ->
											maybe( cert_manager_pid() ).
% localhost of course invisible from outside the LAN:
handle_certificate_manager_for( _BinDomainName= <<"localhost">>, _BinSans,
		_CertSupport, _CertMode, _BinCertDir, _MaybeBinAgentKeyPath,
		_SchedulerPid ) ->
	undefined;

handle_certificate_manager_for( BinDomainName, BinSans,
		_CertSupport=renew_certificates, CertMode, BinCertDir,
		MaybeBinAgentKeyPath, SchedulerPid ) ->

	BinAgentKeyPath = case MaybeBinAgentKeyPath of

		undefined ->
			throw( no_leec_agents_key_file );

		KP ->
			KP

	end,

	% Initially, the certification generation was asynchronous (thus expected to
	% trigger a onCertificateReady/2 method - counterpart of the on_complete/1
	% function of Let's Encrypt, when obtained), but now that concurrent FSMs
	% can be managed thanks to our LEEC fork, it can be fully synchronous, which
	% is better.
	%
	% A synchronous creation is now used, otherwise even these parallel
	% creations could lead (through the initialisation of the LEEC FSM already
	% interacting with the ACME server) in hitting rate limits:
	%
	class_USCertificateManager:synchronous_new_link( BinDomainName, BinSans,
		CertMode, BinCertDir, BinAgentKeyPath, SchedulerPid,
		_IsSingleton=false );


% For CertSupport, either no_certificates or use_existing_certificates here, so
% no certificate manager to create:
%
handle_certificate_manager_for( _BinDomainName, _BinSans, _OtherCertSupport,
		_CertMode, _BinCertDir, _MaybeBinAgentKeyPath, _SchedulerPid ) ->
	undefined.



% Method section.


% Triggers and waits for a parallel certificate renewal from all known
% certificate managers (may be done for example at server startup).
%
-spec renewCertificates( wooper:state() ) ->
					const_request_return( 'certificate_renewals_over' ).
renewCertificates( State ) ->

	CertManagers = get_all_certificate_manager_pids( State ),

	CertManagerCount = length( CertManagers ),

	% 1  minute and 30 seconds per manager:
	MaxDurationInMs = 90*1000,

	?info_fmt( "Renewing all certificates, through their ~B managers (~w); "
		"setting a (large) ~s for each of them.", [ CertManagerCount,
		CertManagers, time_utils:time_out_to_string( MaxDurationInMs ) ] ),

	% We should not trigger all managers in parallel for a certificate renewal,
	% lest we hit another Letsencrypt rate limit (see in
	% https://letsencrypt.org/docs/rate-limits/ the 'Overall Requests limit').
	%
	% So we iterate on certificate managers and for each of them in turn we send
	% the following oneway and wait for its synchronisation ack before
	% proceeding to the next one:
	%
	case wooper:send_acknowledged_oneway_in_turn(
	  _OnwName=renewCertificateSynchronisable, _OnwArgs=[ self() ],
	  _TargetInstancePIDs=CertManagers, MaxDurationInMs,
	  _AckAtom=certificate_renewal_over ) of

		[] ->
			?debug_fmt( "All ~B certificates ready.", [ CertManagerCount ] );

		FailedCertPids ->
			?error_fmt( "~B certificate(s) (on a total of ~B) could not be "
				"obtained by their respective managers: ~w.",
				[ length( FailedCertPids ), CertManagerCount,
				  FailedCertPids ] )

	end,

	% Note the plural in atom:
	wooper:const_return_result( certificate_renewals_over ).



% Requests the TLS certificate of specified virtual host to be renewed.
-spec renewCertificate( wooper:state(), domain_id(), vhost_id() ) ->
							const_oneway_return().
renewCertificate( State, DomainId, VHostId ) ->

	?info_fmt( "Renewal of TLS certification for virtual host '~s' of "
			   "domain '~s' requested.", [ VHostId, DomainId ] ),

	?error( "Not implemented yet." ),

	wooper:const_return().



% Callback triggered whenever a linked process stops.
-spec onWOOPERExitReceived( wooper:state(), pid(),
						basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, _StopPid, _ExitType=normal ) ->

	% Not even a trace sent for that, as too many of them.
	%
	%?notice_fmt( "Ignoring normal exit from process ~w.", [ StopPid ] ),

	wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%						{wooper_oneway_failed,<0.44.0>,class_XXX,
	%							FunName,Arity,Args,AtomCause}}, [...]}"

	?error_fmt( "Received and ignored an exit message '~p' from ~w.",
				[ ExitType, CrashPid ] ),

	wooper:const_return().



% Helper section.


% Returns a domain table and dispatch routes corresponding to the specified
% domain information list, and a possibly updated state.
%
-spec process_domain_info( [ { domain_name(), [ vhost_info() ] } ],
	bin_directory_path(), maybe( bin_directory_path() ),
	maybe( web_analysis_info() ), cert_support(), cert_mode(),
	bin_directory_path(), maybe( bin_file_path() ), scheduler_pid(),
	wooper:state() ) ->
		{ domain_config_table(), dispatch_routes(), wooper:state() }.
process_domain_info( UserRoutes, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeLogAnalysisSettings, CertSupport, CertMode, BinCertDir,
		MaybeBinAgentKeyPath, SchedulerPid, State ) ->

	%trace_utils:debug_fmt( "Domain list: ~p", [ UserRoutes ] ),

	MaybeWebAnalysisInfo = prepare_web_analysis( MaybeLogAnalysisSettings,
							UserRoutes, MaybeBinDefaultWebRoot, State ),

	% The user-specified route order is respected, and any host catch-all must
	% come last:
	%
	process_domain_routes( lists:reverse( UserRoutes ), BinLogDir,
		MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, _AccVTable=table:new(),
		_AccRoutes=[], CertSupport, CertMode, BinCertDir, MaybeBinAgentKeyPath,
		SchedulerPid, State ).




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
  _MaybeLogAnalysisSettings={ _ToolName=awstats, BinAnalysisUpdateToolRoot,
							  BinAnalysisReportToolRoot },
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

	LogAnalysisStateDir = file_utils:join( ?getAttr(data_directory),
										   "log-analysis-state" ),

	case file_utils:is_existing_directory_or_link( LogAnalysisStateDir ) of

		true ->
			ok;

		false ->
			?debug_fmt( "Creating the directory to store the state of "
				"the log analysis tool, '~s'.", [ LogAnalysisStateDir ] ),
			file_utils:create_directory( LogAnalysisStateDir )

	end,

	% Common to all virtual hosts:
	BaseTranslationTable = table:new(
		[ { "US_WEB_LOG_ANALYSIS_DATA_DIR", LogAnalysisStateDir } ] ),

	% Quite similar to a collective file_utils:update_with_keywords/3:

	TemplateBaseContent = file_utils:read_whole( ConfTemplatePath ),

	% Done once, for all virtual hosts:
	TemplateContent = text_utils:update_with_keywords( TemplateBaseContent,
													   BaseTranslationTable ),

	% Validates the target Awstats scripts:

	UpdateToolPath =
	  file_utils:join( [ BinAnalysisUpdateToolRoot, "cgi-bin", "awstats.pl" ] ),

	BinUpdateToolPath = case file_utils:is_executable( UpdateToolPath ) of

		true ->
			text_utils:string_to_binary( UpdateToolPath );

		false ->
			throw( { awstats_update_executable_not_found, UpdateToolPath } )

	end,


	% Previously the main tool was "awstats.pl" and no helper was used, now
	% building all report pages in one go with (using the former as its helper):

	ReportToolPath = file_utils:join(
		[ BinAnalysisReportToolRoot, "awstats_buildstaticpages.pl" ] ),

	BinReportToolPath = case file_utils:is_executable( ReportToolPath ) of

		true ->
			text_utils:string_to_binary( ReportToolPath );

		false ->
			throw( { awstats_report_executable_not_found, ReportToolPath } )

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
				[ GenAwCfgDir, system_utils:get_user_name_safe(), Exception ] )

	end,

	% Expanded from log_analysis_settings and returned;
	#web_analysis_info{ tool=awstats,
						update_tool_path=BinUpdateToolPath,
						report_tool_path=BinReportToolPath,
						template_content=TemplateContent,

						% Where per-vhost configuration files shall be
						% generated:
						%
						conf_dir=text_utils:string_to_binary( GenAwCfgDir ),
						state_dir=text_utils:string_to_binary(
									LogAnalysisStateDir ),
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



% Here we go through domains then through virtual hosts, notably to prepare
% routes and any certificate management needed.
%
% (helper)
%
-spec process_domain_routes( [ { domain_name(), [ vhost_info() ] } ],
		bin_directory_path(), maybe( bin_directory_path() ),
		maybe( web_analysis_info() ), domain_config_table(), dispatch_routes(),
		cert_support(), cert_mode(), bin_directory_path(),
		maybe( bin_file_path() ), scheduler_pid(), wooper:state() ) ->
			{ domain_config_table(), dispatch_routes(), wooper:state() }.
process_domain_routes( _UserRoutes=[], _BinLogDir, _MaybeBinDefaultWebRoot,
		_MaybeWebAnalysisInfo, AccVTable, AccRoutes, _CertSupport,
		_CertMode, _BinCertDir, _MaybeBinAgentKeyPath, _SchedulerPid, State ) ->
	%trace_utils:debug_fmt( "Resulting routes:~n~p", [ AccRoutes ] ),
	{ AccVTable, AccRoutes, State };


% Explicit domain (not a catch-all); we create now a domain-level certificate
% (not anymore a virtual host-level one), which lists all its virtual hosts
% thanks to SAN. This is the recommended approach, and anyway the Let's Encrypt
% rate limits would be too easily reached otherwise.
%
process_domain_routes( _UserRoutes=[ { DomainName, VHostInfos } | T ],
		BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable,
		AccRoutes, CertSupport, CertMode, BinCertDir, MaybeBinAgentKeyPath,
		SchedulerPid, State ) when is_list( DomainName ) ->

	BinDomainName = text_utils:string_to_binary( DomainName ),

	BinSans = get_san_list( VHostInfos, DomainName ),

	MaybeCertManagerPid = handle_certificate_manager_for( BinDomainName,
		BinSans, CertSupport, CertMode, BinCertDir, MaybeBinAgentKeyPath,
		SchedulerPid ),

	{ VHostTable, VHostRoutes, BuildState } = build_vhost_table( BinDomainName,
		VHostInfos, MaybeCertManagerPid, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, _AccVtable=table:new(), _AccRoutes=[],
		CertSupport, State ),

	DomainInfo = { BinDomainName, MaybeCertManagerPid, VHostTable },

	NewAccVTable = table:add_new_entry( _K=BinDomainName, _V=DomainInfo,
										AccVTable ),

	% Order matters:
	NewAccRoutes = VHostRoutes ++ AccRoutes,

	process_domain_routes( T, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, NewAccVTable, NewAccRoutes, CertSupport,
		CertMode, BinCertDir, MaybeBinAgentKeyPath, SchedulerPid, BuildState );


% Domain catch-all:
process_domain_routes(
  _UserRoutes=[ { CatchAllDomainId=default_domain_catch_all, VHostInfos } | T ],
  BinLogDir, MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, AccVTable,
  AccRoutes, CertSupport, CertMode, BinCertDir, MaybeBinAgentKeyPath,
  SchedulerPid, State ) ->

	% We cannot create a wildcard certificate for any domain, so here no https
	% to expect, and no certificate manager.

	{ VHostTable, VHostRoutes, BuildState } = build_vhost_table(
		CatchAllDomainId, VHostInfos, _MaybeCertManagerPid=undefined, BinLogDir,
		MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, _AccVtable=table:new(),
		_AccRoutes=[], CertSupport, State ),

	DomainInfo = { CatchAllDomainId, _MaybeCertManagerPid=undefined,
				   VHostTable },

	NewAccVTable = table:add_new_entry( _K=CatchAllDomainId, _V=DomainInfo,
										AccVTable ),

	% Order matters:
	NewAccRoutes = VHostRoutes ++ AccRoutes,

	process_domain_routes( T, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, NewAccVTable, NewAccRoutes, CertSupport,
		CertMode, BinCertDir, MaybeBinAgentKeyPath, SchedulerPid, BuildState );


process_domain_routes( _UserRoutes=[ InvalidEntry | _T ], _BinLogDir,
		_MaybeBinDefaultWebRoot, _MaybeWebAnalysisInfo, _AccVTable, _AccRoutes,
		_CertSupport, _CertMode, _BinCertDir, _MaybeBinAgentKeyPath,
		_SchedulerPid, State ) ->

	?error_fmt( "Invalid entry in virtual host configuration:~n~p",
				[ InvalidEntry ] ),

	throw( { invalid_vhost_entry, InvalidEntry } ).




% Builds a vhost table corresponding to specified domain identifier.
%
% Multiple operations have to be done in one pass as they are quite interlinked.
%
% (helper)
%
-spec build_vhost_table( bin_domain_name(), [ vhost_info() ],
	maybe( cert_manager_pid() ), bin_directory_path(),
	maybe( bin_directory_path() ), maybe( web_analysis_info() ),
	vhost_config_table(), dispatch_routes(), cert_support(), wooper:state() ) ->
		  { vhost_config_table(), dispatch_routes(), wooper:state() }.
build_vhost_table( _DomainId, _VHostInfos=[], _MaybeCertManagerPid, _BinLogDir,
		_MaybeBinDefaultWebRoot, _MaybeWebAnalysisInfo, AccVTable, AccRoutes,
		_CertSupport, State ) ->

	% Restores user-defined order (ex: default route to come last):
	{ AccVTable, lists:reverse( AccRoutes ), State };


% No kind specified, upgrading to the default: static.
build_vhost_table( DomainId, _VHostInfos=[ { VHostId, ContentRoot } | T ],
		MaybeCertManagerPid, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, AccVTable, AccRoutes, CertSupport, State ) ->

	build_vhost_table( DomainId,
		_FullVHostInfos=[ { VHostId, ContentRoot, _DefaultKind=static } | T ],
		MaybeCertManagerPid, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, AccVTable, AccRoutes, CertSupport, State );


% Static - actual or catch-all - vhost specified here:
build_vhost_table( DomainId,
		_VHostInfos=[ { VHostId, ContentRoot, WebKind=static } | T ],
		MaybeCertManagerPid, BinLogDir, MaybeBinDefaultWebRoot,
		MaybeWebAnalysisInfo, AccVTable, AccRoutes, CertSupport, State ) ->

	?debug_fmt( "Managing static configuration for virtual host '~s' in "
				"domain '~s'.", [ VHostId, DomainId ] ),

	BinContentRoot = get_content_root( ContentRoot, MaybeBinDefaultWebRoot,
									   VHostId, DomainId, State ),

	ActualKind = check_kind( WebKind, VHostId, DomainId, State ),

	BinVHostId = case VHostId of

		default_vhost_catch_all ->
			default_vhost_catch_all;

		_ ->
			text_utils:string_to_binary( VHostId )

	end,

	% We do not generate certificates for individual virtual hosts anymore (too
	% many of them), we rely on SANs instead; so managing here only any web
	% logger, and static dispatch:
	%
	{ VHostEntry, VHostRoute } = manage_vhost( BinContentRoot, ActualKind,
		DomainId, BinVHostId, MaybeCertManagerPid, BinLogDir, CertSupport,
		MaybeWebAnalysisInfo ),

	NewAccVTable = table:add_entry( _K=BinVHostId, VHostEntry, AccVTable ),

	build_vhost_table( DomainId, T, MaybeCertManagerPid, BinLogDir,
		MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, NewAccVTable,
		[ VHostRoute | AccRoutes ], CertSupport, State );


% Meta kind here:
build_vhost_table( DomainId,
	   _VHostInfos=[ { VHost, _ContentRoot, WebKind=meta } | T ],
	   MaybeCertManagerPid, BinLogDir, MaybeBinDefaultWebRoot,
	   MaybeWebAnalysisInfo, AccVTable, AccRoutes, CertSupport,
	   State ) ->

	% Quite similar to 'static', even if a logger and a web analysis
	% organisation are not that useful:

	% Not expected to be 'default_vhost_catch_all':
	BinVHost = text_utils:string_to_binary( VHost ),

	BinMetaContentRoot = case MaybeWebAnalysisInfo of

		undefined ->
			?error_fmt( "A meta website (vhost '~s' for domain '~s') was "
				"requested, yet no web analysis information is available. "
				"No 'log_analysis' entry defined in US-Web configuration file?",
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
			DomainId, BinVHost, MaybeCertManagerPid, BinLogDir, CertSupport,
			MaybeWebAnalysisInfo ),

	NewAccVTable = table:add_entry( _K=BinVHost, VHostEntry, AccVTable ),

	% Certificate setting used here as well:
	build_vhost_table( DomainId, T, MaybeCertManagerPid, BinLogDir,
		MaybeBinDefaultWebRoot, MaybeWebAnalysisInfo, NewAccVTable,
		[ VHostRoute | AccRoutes ], CertSupport, SetState );


% Nitrogen kind here:
build_vhost_table( _DomainId,
		_VHostInfos=[ { _VHost, _ContentRoot, _WebKind=nitrogen } | _T ],
		_MaybeCertManagerPid, _BinLogDir, _MaybeBinDefaultWebRoot,
		_MaybeWebAnalysisInfo, _AccVTable, _AccRoutes, _CertSupport, _State ) ->
	throw( not_implemented_yet );


% Unknown kind:
build_vhost_table( _DomainId,
		_VHostInfos=[ { _VHost, _ContentRoot, UnknownWebKind } | _T ],
		_MaybeCertManagerPid, _BinLogDir, _MaybeBinDefaultWebRoot,
		_MaybeWebAnalysisInfo, _AccVTable, _AccRoutes, _CertSupport, _State ) ->
	throw( { unknown_web_kind, UnknownWebKind } );

build_vhost_table( DomainId, _VHostInfos=[ InvalidVHostConfig | _T ],
	   _MaybeCertManagerPid, _BinLogDir, _MaybeBinDefaultWebRoot,
	   _MaybeWebAnalysisInfo, _AccVTable, _AccRoutes, _CertSupport, _State ) ->
	throw( { invalid_virtual_host_config, InvalidVHostConfig, DomainId } ).




% Returns the corresponding {VHostEntry, VHostRoute} pair.
%
% (centralising helper)
%
% Here no web analysis is requested (yet log rotation is still useful):
manage_vhost( BinContentRoot, ActualKind, DomainId, VHostId,
		MaybeCertManagerPid, BinLogDir, CertSupport,
		_MaybeWebAnalysisInfo=undefined ) ->

	BinWebLogDir = get_web_log_dir( BinLogDir ),

	% Now, we do not use anymore a scheduler for web logging, we prefer a task
	% ring, as it is safer in terms of concurrent accesses to the database
	% maintained by any analysis tool, and it balances better the load.
	%
	LoggerPid = class_USWebLogger:new_link( VHostId, DomainId, BinWebLogDir,
			_MaybeSchedulerPid=undefined, _MaybeWebGenSettings=undefined ),

	VHostEntry = #vhost_config_entry{ virtual_host=VHostId,
									  parent_host=DomainId,
									  kind=ActualKind,
									  content_root=BinContentRoot,
									  logger_pid=LoggerPid,
									  cert_manager_pid=MaybeCertManagerPid },

	VHostRoute = get_static_dispatch_for( VHostId, DomainId, BinContentRoot,
						LoggerPid, CertSupport, MaybeCertManagerPid ),

	{ VHostEntry, VHostRoute };


% The same, except that here web analysis is requested:
manage_vhost( BinContentRoot, ActualKind, DomainId, VHostId,
		MaybeCertManagerPid, BinLogDir, CertSupport,
		WebAnalysisInfo=#web_analysis_info{ tool=LogAnalysisTool,
											template_content=TemplateContent,
											conf_dir=BinConfDir } ) ->

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

	BinWebLogDir = get_web_log_dir( BinLogDir ),

	{ BinAccessLogFilename, _BinErrorLogFilename } =
		class_USWebLogger:get_log_paths( VHostId, DomainId ),

	AccessLogFilePath = file_utils:join( BinWebLogDir, BinAccessLogFilename ),

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
										_EscapingChar=$\\ ) ] ),

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
	LoggerPid = class_USWebLogger:new_link( VHostId, DomainId, BinWebLogDir,
					_MaybeSchedulerPid=undefined, WebAnalysisInfo ),

	VHostEntry = #vhost_config_entry{ virtual_host=VHostId,
									  parent_host=DomainId,
									  kind=ActualKind,
									  content_root=BinContentRoot,
									  logger_pid=LoggerPid,
									  cert_manager_pid=MaybeCertManagerPid },

	VHostRoute = get_static_dispatch_for( VHostId, DomainId, BinContentRoot,
								LoggerPid, CertSupport, MaybeCertManagerPid ),

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
				"does not exist, creating it.", [ HostDesc, NormContentRoot ] ),
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
% See https://ninenines.eu/docs/en/cowboy/2.9/guide/routing/ for more details.
%
-spec get_static_dispatch_for( vhost_id(), domain_id(), bin_directory_path(),
		logger_pid(), cert_support(), maybe( cert_manager_pid() ) ) ->
									route_rule().
get_static_dispatch_for( VHostId, DomainId, BinContentRoot, LoggerPid,
						 CertSupport, MaybeCertManagerPid ) ->

	% We prepare, once for all, all settings for a given (virtual) host.

	% Refer to https://ninenines.eu/docs/en/cowboy/2.8/manual/cowboy_static/ and
	% https://ninenines.eu/docs/en/cowboy/2.8/guide/static_files/:

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
					% A little better (more general) than above (which, if
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

	% Allows to expand a requested 'http://foobar.org' into
	% 'http://foobar.org/index.html':
	%
	NoPagePathMatch = { "/", us_web_static, InitialState#{ type => file,
														   path => BinIndex } },

	% Allows to serve all files (ex: HTML, images, CSS, etc.) from the
	% specified tree:
	%
	OtherPathsMatch ={ "/[...]", us_web_static,
					   InitialState#{ type => directory,
									  path => BinContentRoot } },

	% MaybeCertManagerPid is undefined in the case of a catch-all:
	PathMatches = case CertSupport =:= renew_certificates
						andalso MaybeCertManagerPid =/= undefined of

		true ->
			% Be able to answer Let's Encrypt ACME challenges:
			[ NoPagePathMatch, get_challenge_path_match( MaybeCertManagerPid ),
			  OtherPathsMatch ];

		false ->
			[ NoPagePathMatch, OtherPathsMatch ]

	end,

	{ HostMatch, PathMatches }.



% Returns the path match that shall be used in order to answer ACME challenges
% from Let's Encrypt from a LEEC FSM.
%
% See https://github.com/Olivier-Boudeville/letsencrypt-erlang#as-slave
%
-spec get_challenge_path_match( cert_manager_pid() ) -> path_match().
get_challenge_path_match( CertManagerPid ) when is_pid( CertManagerPid ) ->
	%{ <<"/.well-known/acme-challenge/:token">>, us_web_letsencrypt_handler,
	{ "/.well-known/acme-challenge/:token", us_web_letsencrypt_handler,
	  _InitialState=CertManagerPid };

get_challenge_path_match( Unexpected ) ->
	throw( { invalid_certificate_manager, Unexpected } ).



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
% US-Web server and their related services, which may be created here.
%
-spec manage_registrations( us_web_config_table(), wooper:state() ) ->
									wooper:state().
manage_registrations( ConfigTable, State ) ->

	Scope = ?default_registration_scope,

	WebCfgSrvRegName = case table:lookup_entry(
			?us_web_config_server_registration_name_key, ConfigTable ) of

		key_not_found ->
			DefCfgRegName = ?default_us_web_config_server_registration_name,
			?info_fmt( "No user-configured registration name for the US-Web "
					   "configuration server, defaulting to '~s'.",
					   [ DefCfgRegName ] ),
			DefCfgRegName;

		{ value, CfgRegName } when is_atom( CfgRegName ) ->
			?info_fmt( "Using user-configured registration name for the "
					   "US-Web configuration server, '~s'.", [ CfgRegName ] ),
			CfgRegName;

		{ value, InvalidCfgRegName } ->
			?error_fmt( "Invalid user-specified registration name for the "
						"US-Web configuration server: '~p'.",
						[ InvalidCfgRegName ] ),
			throw( { invalid_web_config_registration_name, InvalidCfgRegName,
					 ?us_web_config_server_registration_name_key } )

	end,

	naming_utils:register_as( WebCfgSrvRegName, Scope ),


	SchedRegName = case table:lookup_entry(
					 ?us_web_scheduler_registration_name_key, ConfigTable ) of

		key_not_found ->
			DefScRegName = ?default_us_web_scheduler_registration_name,
			?info_fmt( "No user-configured registration name for the US-Web "
					   "scheduler, defaulting to '~s'.", [ DefScRegName ] ),
			DefScRegName;

		{ value, ScRegName } when is_atom( ScRegName ) ->
			?info_fmt( "Using user-configured registration name for the "
					   "US-Web scheduler, '~s'.", [ ScRegName ] ),
			ScRegName;

		{ value, InvalidScRegName } ->
			?error_fmt( "Invalid user-specified registration name for the "
						"US-Web scheduler: '~p'.", [ InvalidScRegName ] ),
			throw( { invalid_web_scheduler_registration_name, InvalidScRegName,
					 ?us_web_config_server_registration_name_key } )

	end,

	% Relatively private to this node:
	SchedPid = class_USScheduler:new_link( "US-Web Scheduler", SchedRegName,
										   _SchedRegScope=local_only ),

	?info_fmt( "This US-Web configuration server was registered as '~s' "
		"(scope: ~s), an will be using scheduler ~w.",
		[ WebCfgSrvRegName, Scope, SchedPid ] ),

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
			?info_fmt( "No user-configured US-Web operating-system username "
					   "set for this server; runtime-detected: '~s'.",
					   [ ActualUsername ] ),
			ActualUsername;

		{ value, Username } when is_list( Username ) ->
			case system_utils:get_user_name() of

				Username ->
					?info_fmt( "Using user-configured US-Web operating-system "
						"username '~s' for this server, which matches "
						"the current runtime user.", [ Username ] ),
					Username;

				OtherUsername ->
					?error_fmt( "The user-configured US-Web operating-system "
						"username '~s' for this server does not match "
						"the current runtime user, '~s'.",
						[ Username, OtherUsername ] ),
					throw( { inconsistent_os_us_web_user, OtherUsername,
							 Username, ?us_web_username_key } )

			end

	end,

	setAttribute( State, ?us_web_username_key,
				  text_utils:string_to_binary( WebUsername ) ).



% Manages any user-configured application base directory, and sets related
% directories.
%
-spec manage_app_base_directories( us_web_config_table(), wooper:state() ) ->
									   wooper:state().
manage_app_base_directories( ConfigTable, State ) ->

	% As opposed to, say, start/stop script, the Erlang code does not care so
	% much about these directories, so warnings, not errors, were issued if
	% not found (the US framework being also launchable thanks to, for example,
	% 'make debug'). We finally opted for a stricter policy, as errors could be
	% induced afterwards.

	AppRunContext = ?getAttr(app_run_context),

	MaybeConfBaseDir = case table:lookup_entry( ?us_web_app_base_dir_key,
											   ConfigTable ) of

		key_not_found ->
			undefined;


		{ value, D } when is_list( D ) ->
			?info_fmt( "User-configured US-Web application base directory "
					   "is '~s'.", [ D ] ),
			D;

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured US-Web application base "
						"directory: '~p'.", [ InvalidDir ] ),
			throw( { invalid_us_web_app_base_directory, InvalidDir,
					 ?us_web_app_base_dir_key, AppRunContext } )

	end,

	MaybeBaseDir = case MaybeConfBaseDir of

		undefined ->
			case system_utils:get_environment_variable(
				   ?us_web_app_env_variable ) of

				false ->
					undefined;

				% Might be set, yet to an empty string, typically because of
				% US_WEB_APP_BASE_DIR="${US_WEB_APP_BASE_DIR}":
				%
				"" ->
					undefined;

				EnvDir ->
					?info_fmt( "No user-configured US-Web application base "
						"directory set in configuration file, using the value "
						"of the '~s' environment variable: '~s'.",
						[ ?us_web_app_env_variable, EnvDir ] ),
					EnvDir

			end;

		_ ->
			MaybeConfBaseDir

	end,

	RawBaseDir = case MaybeBaseDir of

		undefined ->
			guess_app_dir( AppRunContext, State );

		_ ->
			MaybeBaseDir

	end,

	BaseDir = file_utils:ensure_path_is_absolute( RawBaseDir ),

	% We check not only that this candidate app directory exists, but also it is
	% a right one, expecting to have a 'priv' direct subdirectory then:

	MaybeBaseBinDir =
			case file_utils:is_existing_directory_or_link( BaseDir ) of

		true ->
			BinBaseDir = text_utils:string_to_binary( BaseDir ),
			case AppRunContext of

				as_otp_release ->
					% As, if run as a release, it may end with a version (ex:
					% "us_web-0.0.1") or as a "us_web-latest" symlink thereof:
					%
					case filename:basename( BaseDir ) of

						"us_web" ++ _ ->
							?info_fmt( "US-Web (release) application base "
							  "directory set to '~s'.", [ BaseDir ] ),
							BinBaseDir;

						_Other ->
							%?warning_fmt( "The US-Web application base "
							%  "directory '~s' does not seem legit (it "
							%  "should end with 'us_web'), thus considering "
							%   "knowing none.", [ BaseDir ] ),
							%undefined
							throw( { incorrect_us_web_app_base_directory,
									 BaseDir, ?us_web_app_base_dir_key,
									 AppRunContext } )

					end;

				as_native ->
					case file_utils:get_last_path_element( BaseDir ) of

						"us_web" ->
							?info_fmt( "US-Web (native) application base "
							  "directory set to '~s'.", [ BaseDir ] ),
							BinBaseDir;

						_Other ->
							throw( { incorrect_us_web_app_base_directory,
									 BaseDir, ?us_web_app_base_dir_key,
									 AppRunContext } )

					end

			end,

			% Final paranoid check:
			PrivDir = file_utils:join( BinBaseDir, "priv" ),
			case file_utils:is_existing_directory_or_link( PrivDir ) of

				true ->
					BinBaseDir;

				false ->
					?error_fmt( "The determined US-Web application base "
						"directory '~s' does not have a 'priv' subdirectory.",
						[ BinBaseDir ] ),
					throw( { no_priv_us_web_app_base_directory,
							 BaseDir, ?us_web_app_base_dir_key } )

			end;


		false ->
			%?warning_fmt( "The US-Web application base directory '~s' does "
			%			  "not exist, thus considering knowing none.",
			%			  [ BaseDir ] ),
			%undefined
			throw( { non_existing_us_web_app_base_directory, BaseDir,
					 ?us_web_app_base_dir_key } )


	end,

	% The internal US-Web directory (see conf_directory) used to be derived from
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



% Tries to guess the US-Web application directory.
guess_app_dir( AppRunContext, State ) ->
	GuessedDir = case AppRunContext of

		as_otp_release ->
			% [...]/us_web/_build/default/rel/us_web, and we want the first
			% us_web, so:
			%
			file_utils:normalise_path( file_utils:join( [
				file_utils:get_current_directory(), "..",
				"..", "..", ".." ] ) );

		as_native ->
			% In the case of a native build, running from us_web/src, so:
			file_utils:get_base_path( file_utils:get_current_directory() )

	end,

	% Was a warning:
	?info_fmt( "No user-configured US-Web application base directory set "
		"(neither in configuration file nor through the '~s' environment "
		"variable), hence trying to guess it, in a ~s context, as '~s'.",
		[ ?us_web_app_env_variable, AppRunContext, GuessedDir ] ),

	GuessedDir.



% Manages any user-configured data directory to rely on, creating it if
% necessary.
%
-spec manage_data_directory( us_web_config_table(), wooper:state() ) ->
									wooper:state().
manage_data_directory( ConfigTable, State ) ->

	BaseDir = case table:lookup_entry( ?us_web_data_dir_key, ConfigTable ) of

		key_not_found ->
			file_utils:ensure_path_is_absolute( ?default_data_base_dir,
									?getAttr(app_base_directory) );

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
	% the us one). So:
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

	% Longer paths if defined as relative, yet finally preferred as
	% '/var/log/universal-server/us-web' (rather than
	% '/var/log/universal-server') allows to separate US-Web from any other US-*
	% services:
	%
	LogDir = case table:lookup_entry( ?us_web_log_dir_key, ConfigTable ) of

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

	case file_utils:is_existing_directory( LogDir ) of

		true ->
			ok;

		false ->

			%throw( { non_existing_base_us_web_log_directory, LogDir } )

			?warning_fmt( "The base US-Web log directory '~s' does not exist, "
						  "creating it.", [ LogDir ] ),

			% As for example the default path would require to create
			% /var/log/universal-server/us-web:
			%
			file_utils:create_directory_if_not_existing( LogDir,
														 create_parents )

	end,

	% In addition to this US-Web log directory, we create a subdirectory thereof
	% to store all web logs (access and error logs):
	%
	BinWebLogDir = get_web_log_dir( LogDir ),
	file_utils:create_directory_if_not_existing( BinWebLogDir ),

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

			Perms = [ owner_read, owner_write, owner_execute,
					  group_read, group_write, group_execute ],

			file_utils:change_permissions( LogDir, Perms ),
			file_utils:change_permissions( BinWebLogDir, Perms );

		% Not owned, do nothing:
		_OtherId ->
			ok

	end,

	BinLogDir = text_utils:string_to_binary( LogDir ),

	setAttribute( State, log_directory, BinLogDir ).


% To centralise the definition of the directory where to store all web logs:
-spec get_web_log_dir( any_directory_path() ) -> bin_directory_path().
get_web_log_dir( LogDir ) ->
	text_utils:string_to_binary( file_utils:join( LogDir, ?web_log_subdir ) ).



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

			AbsDefaultWebRoot = file_utils:ensure_path_is_absolute(
				DefWebRoot,
				otp_utils:get_priv_root( ?MODULE, _BeSilent=true ) ),

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
							 AbsDefaultWebRoot } )

			end

	end,

	setAttribute( State, default_web_root, MaybeBinDefWebRoot ).




% Manages any user-configured TCP ports.
-spec manage_ports( us_web_config_table(), wooper:state() ) -> wooper:state().
manage_ports( ConfigTable, State ) ->

	HttpPort = case table:lookup_entry( ?http_tcp_port_key, ConfigTable ) of

		key_not_found ->
			DefaultHttpPort = 80,
			?info_fmt( "No user-specified HTTP TCP port, defaulting to #~B.",
					   [ DefaultHttpPort ] ),
			DefaultHttpPort;

		{ value, ClearPort } when is_integer( ClearPort ) ->
			?info_fmt( "The user-specified HTTP TCP port is #~B.",
					   [ ClearPort ] ),
			ClearPort;

		{ value, undefined } ->
			?info( "Use of any HTTP TCP port explicitly disabled "
				   "by the user." ),
			undefined;

		{ value, InvalidClearPort } ->
			?error_fmt( "Invalid user-specified HTTP TCP port: '~p'.",
						[ InvalidClearPort ] ),
			throw( { invalid_http_port, InvalidClearPort, http_tcp_port } )

	end,

	HttpsPort = case table:lookup_entry( ?https_tcp_port_key, ConfigTable ) of

		key_not_found ->
			DefaultHttpsPort = 443,
			?info_fmt( "No user-specified HTTPS TCP port, defaulting to #~B.",
					   [ DefaultHttpsPort ] ),
			DefaultHttpsPort;

		{ value, TLSPort } when is_integer( TLSPort ) ->
			?info_fmt( "The user-specified HTTPS TCP port is #~B.",
					   [ TLSPort ] ),
			TLSPort;

		{ value, undefined } ->
			?info( "Use of any HTTPS TCP port explicitly disabled "
				   "by the user." ),
			undefined;

		{ value, InvalidTLSPort } ->
			?error_fmt( "Invalid user-specified HTTPS TCP port: '~p'.",
						[ InvalidTLSPort ] ),
			throw( { invalid_https_port, InvalidTLSPort, https_tcp_port } )

	end,

	setAttributes( State, [ { http_tcp_port, HttpPort },
							{ https_tcp_port, HttpsPort } ] ).



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
	set_log_tool_settings( { ToolName, _DefaultAnalysisUpdateToolRoot=undefined,
							 _DefaultAnalysisReportToolRoot=undefined },
						   State );

% At least Arch Linux defaults:
set_log_tool_settings( { awstats, _MaybeAnalysisUpdateToolRoot=undefined,
					   _MaybeAnalysisReportToolRoot=undefined }, State ) ->
	set_log_tool_settings( { awstats,
		_UpdateToolRoot="/usr/share/webapps/awstats",
		_ReportToolRoot="/usr/share/awstats/tools" }, State );

% Supposing both are defined or neither:
set_log_tool_settings( { ToolName, AnalysisUpdateToolRoot,
						 AnalysisReportToolRoot }, State )
  when is_list( AnalysisUpdateToolRoot )
	   andalso is_list( AnalysisReportToolRoot ) ->

	BinUpdateToolRoot =
			case file_utils:is_existing_directory_or_link(
				   AnalysisUpdateToolRoot ) of

		true ->
			text_utils:string_to_binary( AnalysisUpdateToolRoot );

		false ->
			?error_fmt( "Root directory '~s' for web log analysis update "
				"tool ('~s') not found.",
				[ AnalysisUpdateToolRoot, ToolName ] ),
			throw( { root_of_log_update_tool_not_found, ToolName,
					 AnalysisUpdateToolRoot } )

	end,

	BinReportToolRoot = case file_utils:is_existing_directory_or_link(
							   AnalysisReportToolRoot ) of

		true ->
			text_utils:string_to_binary( AnalysisReportToolRoot );

		false ->
			?error_fmt( "Root directory '~s' for web log analysis report "
				"tool ('~s') not found.",
				[ AnalysisReportToolRoot, ToolName ] ),
			throw( { root_of_log_report_tool_not_found, ToolName,
					 AnalysisReportToolRoot } )

	end,

	setAttribute( State, log_analysis_settings,
				  { ToolName, BinUpdateToolRoot, BinReportToolRoot } );


set_log_tool_settings( Unexpected, State ) ->
	?error_fmt( "Unexpected settings for the web log analysis tool: ~p.",
				[ Unexpected ] ),
	throw( { unexpected_log_tool_settings, Unexpected } ).



% Manages how X.509 certificates shall be handled.
-spec manage_certificates( us_web_config_table(), wooper:state() ) ->
									wooper:state().
manage_certificates( ConfigTable, State ) ->

	CertSupport = case table:lookup_entry( ?certificate_support_key,
										   ConfigTable ) of

		key_not_found ->
			?info( "No certificate support specified, defaulting to none." ),
			no_certificates;

		{ value, no_certificates } ->
			?info( "Certificate support disabled." ),
			no_certificates;

		{ value, use_existing_certificates } ->
			?info( "Certificate support enabled, based only on existing ones "
				   "(no renewal)." ),
			use_existing_certificates;

		{ value, renew_certificates } ->
			?info( "Certificate generation, use and renewal enabled." ),
			renew_certificates;

		{ value, OtherSupport } ->
			?error_fmt( "Invalid certificate support setting: '~p'.",
						[ OtherSupport ] ),
			throw( { invalid_certificate_mode, OtherSupport } )

	end,

	CertDir = file_utils:join( ?getAttr(data_directory), "certificates" ),

	CertMode = case table:lookup_entry( ?certificate_mode_key, ConfigTable ) of

		key_not_found ->
			% Default:
			?info( "Certificate mode set by default to production." ),
			production;

		{ value, development } ->
			?info( "Certificate mode set to development." ),
			development;

		{ value, production } ->
			?info( "Certificate mode set to production." ),
			production

	end,

	{ MaybeBinKeyPath, MaybeDHKeyPath, MaybeBinCaKeyPath } = case CertSupport of

		renew_certificates ->


			file_utils:create_directory_if_not_existing( CertDir,
				_ParentCreation=create_parents ),

			?debug_fmt( "Certificates are to be generated in '~s'.",
						[ CertDir ] ),

			% A LEEC instance will be started by each (independent) certificate
			% manager, yet to avoid hitting the Let's Encrypt they will all rely
			% on a single ACME account, whose TLS private key is created once
			% for all - unless it already exists:

			TargetKeyPath = file_utils:join( CertDir, ?leec_key_filename ),

			BinKeyPath = case file_utils:is_existing_file_or_link(
								 TargetKeyPath ) of

				true ->
					?debug_fmt( "A pre-existing TLS private key for the US-Web "
						"LEEC agent has been found (as '~s'), it will be "
						"re-used.", [ TargetKeyPath ] ),
					text_utils:string_to_binary( TargetKeyPath );

				false ->

					?debug_fmt( "No pre-existing TLS private key for the US-Web"
						" LEEC agent has been found (searched for '~s'), "
						"generating it now.", [ TargetKeyPath ] ),

					PrivKey = letsencrypt_tls:obtain_private_key(
								{ new, ?leec_key_filename }, CertDir ),

					PrivKey#tls_private_key.file_path

			end,

			BinDHKeyPath = letsencrypt_tls:obtain_dh_key( CertDir ),

			BinCAKeyPath =
				letsencrypt_tls:obtain_ca_cert_file( CertDir ),

			?debug_fmt( "DH (Diffie-Helman) key path is '~s', "
				"CA (Certificate Authority) key path is '~s'.",
				[ BinDHKeyPath, BinCAKeyPath ] ),

			{ BinKeyPath, BinDHKeyPath, BinCAKeyPath };


		use_existing_certificates ->

			% A DH file is still needed, for key exchanges:
			BinDHKeyPath = letsencrypt_tls:obtain_dh_key( CertDir ),

			% Not sure relevant here:
			BinCAKeyPath =
				letsencrypt_tls:obtain_ca_cert_file( CertDir ),

			?debug_fmt( "DH (Diffie-Helman) key path is '~s', "
				"CA (Certificate Authority) key path is '~s'.",
				[ BinDHKeyPath, BinCAKeyPath ] ),

			{ undefined, BinDHKeyPath, BinCAKeyPath };


		no_certificates ->
			{ undefined, undefined, undefined }

	end,

	setAttributes( State, [ { cert_support, CertSupport },
							{ cert_mode, CertMode },
							{ cert_directory, CertDir },
							{ leec_agents_key_path, MaybeBinKeyPath },
							{ dh_key_path, MaybeDHKeyPath },
							{ ca_cert_key_path, MaybeBinCaKeyPath } ] ).



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

	CertSupport = ?getAttr(cert_support),

	CertDir = ?getAttr(cert_directory),

	% Only routes expected to remain (so we check that no extraneous entry
	% remains, see end of load_web_config/3):
	%
	{ DomainCfgTable, DispatchRoutes, ProcessState } = process_domain_info(
		UserRoutes, BinLogDir, ?getAttr(default_web_root),
		?getAttr(log_analysis_settings), CertSupport,
		?getAttr(cert_mode), CertDir, ?getAttr(leec_agents_key_path),
		SchedulerPid, State ),

	% TLS configuration for any https support:
	MaybeSNIInfo = case CertSupport of

		no_certificates ->
			undefined;

		_ ->
			SNIInfo = class_USCertificateManager:get_sni_info( UserRoutes,
															   CertDir ),

			?debug_fmt( "SNI information are:~n  ~p.", [ SNIInfo ] ),

			SNIInfo

	end,

	% Now that all loggers are created, we gather their PID so that they are
	% managed by a task ring, in order to synchronise them properly:
	%
	AllLoggerPids = get_all_logger_pids_from( DomainCfgTable ),

	basic_utils:check_all_defined( AllLoggerPids ),

	TaskPeriod = class_USWebLogger:get_default_log_rotation_period(),

	TaskRingPid = class_USTaskRing:new_link( _RingName="US-Web Logger Ring",
		_Actuators=AllLoggerPids, _TaskRequestName=rotateLogsSynch,
		_TaskRequestArgs=[], TaskPeriod, _ScheduleCount=unlimited,
		SchedulerPid ),

	DispatchRules = cowboy_router:compile( DispatchRoutes ),

	?debug_fmt( "The US-Web specified dispatch routes are:~n  ~p~n"
	  "They correspond, once compiled, to the following dispatch rules:~n  ~p",
	  [ DispatchRoutes, DispatchRules ] ),

	setAttributes( ProcessState, [ { domain_config_table, DomainCfgTable },
								   { dispatch_rules, DispatchRules },
								   { logger_task_ring, TaskRingPid },
								   { sni_info, MaybeSNIInfo } ] ).



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

				{ _ToolName=awstats, BinAnalysisToolRoot,
				  _BinAnalysisHelperRoot } ->

					% Copies first, in meta web root (now known), the icons to
					% be used by the generated pages:
					%
					IconDir = file_utils:join( BinAnalysisToolRoot, "icon" ),

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
-spec generate_meta( meta_web_settings(), boolean(), tcp_port(),
					 domain_config_table(), wooper:state() ) -> wooper:state().
generate_meta( MetaWebSettings={ _DomainId, _BinVHost, BinMetaContentRoot },
			   LogAnalysisEnabled, Port, DomainCfgTable, State ) ->

	?debug_fmt( "Generating meta website in '~s'.", [ BinMetaContentRoot ] ),

	IndexPath = file_utils:join( BinMetaContentRoot, "index.html" ),

	IndexContent = us_web_meta:get_page_header()
		++ us_web_meta:get_page_body( Port, DomainCfgTable,
			?getAttr(start_timestamp), MetaWebSettings, LogAnalysisEnabled )
		++ us_web_meta:get_page_footer(),

	file_utils:write_whole( IndexPath, IndexContent ),

	State.



% Returns a list of the Subject Alternative Names for specified domain, based on
% the specified information regarding its virtual hosts.
%
% Note that a per-virtual host list for SAN is preferred to a wildcard
% certificate, as, at least currently, the latter can only be obtained from
% Let's Encrypt based on a DNS challenge, which is not supported by LEEC.
%
-spec get_san_list( [ vhost_info() ], domain_name() ) -> [ bin_san() ].
get_san_list( VHostInfos, DomainName ) ->
	% Now we pre-collect the names of all virtual hosts (ex: the *.foobar.org),
	% so that the certificate for the corresponding domain (the vhost catch-all,
	% ex: for foobar.org) can list them as SANs (Subject Alternative Names):
	%
	get_san_list( VHostInfos, DomainName, _Acc=[] ).


% (helper)
get_san_list( _VHostInfos=[], _DomainName, Acc ) ->
	% Preferred order:
	lists:reverse( Acc );

get_san_list( _VHostInfos=[ VHInfo | T ], DomainName, Acc ) ->

	% All virtual hosts are tuples whose first element is their identifier,
	% either a plain string or default_vhost_catch_all:
	%
	VHostId = element( 1, VHInfo ),

	case VHostId of

		default_vhost_catch_all ->
			% We used to include the root domain by itself among the SANs as
			% others do, although it shall be the reference name of the
			% certificate, hence not a SAN; anyway if this operation is useful,
			% it shall be done directly in LEEC, not here, so:
			%
			%BinSan = text_utils:string_to_binary( DomainName ),
			%get_san_list( T, DomainName, [ BinSan | Acc ] );
			get_san_list( T, DomainName, Acc );

		VHostname ->
			BinSan = text_utils:bin_format( "~s.~s",
											[ VHostname, DomainName ] ),
			get_san_list( T, DomainName, [ BinSan | Acc ] )

	end.



% Returns a textual description of this domain table.
-spec domain_table_to_string( domain_config_table() ) -> ustring().
domain_table_to_string( DomainTable ) ->
	% No ellipsing wanted:
	"domain configuration "
		++ table:to_string( DomainTable, _DescriptionType=full ).


% Returns a list of all the PIDs of the webloggers.
-spec get_all_logger_pids( wooper:state() ) -> [ logger_pid() ].
get_all_logger_pids( State ) ->
	get_all_logger_pids_from( ?getAttr(domain_config_table) ).


% (helper)
get_all_logger_pids_from( DomainCfgTable ) ->
	MaybePids = list_utils:flatten_once( [
			[ VHCfgE#vhost_config_entry.logger_pid
			  || VHCfgE <- table:values( VHCfgTable ) ]
					   || { _DomainId, _MaybeCertManagerPid, VHCfgTable }
								<- table:values( DomainCfgTable ) ] ),
	list_utils:filter_out_undefined( MaybePids ).


% Returns a list of all the PIDs of the certificate managers.
-spec get_all_certificate_manager_pids( wooper:state() ) ->
		  [ cert_manager_pid() ].
get_all_certificate_manager_pids( State ) ->
	get_all_certificate_manager_pids_from( ?getAttr(domain_config_table) ).



% (helper)
get_all_certificate_manager_pids_from( DomainCfgTable ) ->
	MaybePids = [ MaybeCertManagerPid
				  || { _DomainId, MaybeCertManagerPid, _VHCfgTable }
								<- table:values( DomainCfgTable ) ],
	list_utils:filter_out_undefined( MaybePids ).



% Returns a textual description of this server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	HttpString = case ?getAttr(http_tcp_port) of

		undefined ->
			"with no http scheme enabled";

		HttpTCPPort ->
			text_utils:format( "using for the http scheme "
				"the TCP port #~B", [ HttpTCPPort ] )

	end,

	HttpsString = case ?getAttr(https_tcp_port) of

		undefined ->
			"with no https scheme enabled";

		HttpsTCPPort ->
			text_utils:format( "using for the https scheme "
				"the TCP port #~B", [ HttpsTCPPort ] )

	end,

	WebRootString = case ?getAttr(default_web_root) of

		undefined ->
			"not using a default web root";

		WebRoot ->
			text_utils:format( "using default web root '~s'", [ WebRoot ] )

	end,

	CertString = case ?getAttr(cert_support) of

		no_certificates ->
			"with no certificate management enabled";

		use_existing_certificates ->
			"using existing certificates";

		renew_certificates ->
			text_utils:format( "relying on auto-renewed certificates "
				"(mode: ~s)", [ ?getAttr(cert_mode) ] )

	end,

	SrvString = case ?getAttr(us_server_pid) of

		undefined ->
			"no US server";

		SrvPid ->
			text_utils:format( "US server ~w", [ SrvPid ] )

	end,

	text_utils:format( "US-Web configuration ~s, running ~s, ~s, ~s, ~s, "
		"running in the ~s execution context, ~s, knowing: ~s, "
		"US overall configuration server ~w and "
		"OTP supervisor ~w, relying on the '~s' configuration directory and "
		"on the '~s' log directory.~n~nIn terms of routes, using ~s~n~n"
		"Corresponding dispatch rules:~n~p",
		[ class_USServer:to_string( State ),
		  otp_utils:application_run_context_to_string(
			?getAttr(app_run_context ) ),
		  HttpString, HttpsString,
		  WebRootString, ?getAttr(execution_context), CertString, SrvString,
		  ?getAttr(us_config_server_pid), ?getAttr(us_web_supervisor_pid),
		  ?getAttr(config_base_directory), ?getAttr(log_directory),
		  domain_table_to_string( ?getAttr(domain_config_table) ),
		  ?getAttr(dispatch_rules) ] ).
