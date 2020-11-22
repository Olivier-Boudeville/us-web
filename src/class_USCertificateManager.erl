% Copyright (C) 2020-2020 Olivier Boudeville
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
% Creation date: Thursday, August 6, 2020.


-module(class_USCertificateManager).


-define( class_description,
		 "Class in charge of managing the generation and renewal of X.509 "
		 "certificates for a given domain." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).



% Managing the generation and renewal of X.509 certificates on behalf of the US
% framework, for a given need (typically a specific FQDN).
%
% This includes:
% - requesting and obtaining such certificates, typically for such a FQDN
% - renewing them on time
%
% Certificates are obtained thanks to Let's Encrypt, more precisely thanks to
% LEEC ("Let's Encrypt Erlang with Ceylan", our fork of letsencrypt-erlang),
% namely https://github.com/Olivier-Boudeville/letsencrypt-erlang).
%
% Each instance of this class is dedicated to the certificates for a given FQDN.
% This allows multiplexing requests possibly for several FQDNs in parallel,
% whereas the async callback of letsencrypt-erlang does not seem to tell for
% which source it was obtained.
%
% This allows also handling more easily and gracefully errors and time-outs.
%
% Certificates can be renewed no sooner than 30 days before their expiration
% date.
%
% A certificate filename is like 'mydomain.tld.crt', whereas a key filename is
% like 'mydomain.tld.key'; both are written in the certificate directory (which
% of course shall be writable by this Erlang VM). A 'mydomain.tld.csr'
% Certificate Signing Request is also created.
%
% In addition to TCP for http (generally port 80), the TCP for https port
% (generally 443) must be opened.

-type manager_pid() :: class_UniversalServer:server_pid().


% Per-virtual-host SNI (SSL-related) option:
-type sni_option() :: ssl:server_option() | ssl:common_option().

% Virtual-host pair storing SNI-related certificate information:
-type sni_host_info() :: { net_utils:string_host_name(), [ sni_option() ] }.


% Information regarding Server Name Indication: certificate path for the default
% hostname, and per-virtual host information.
%
-type sni_info() :: { BinCertDefaultHostname :: bin_file_path(),
					  [ sni_host_info() ] }.

-export_type([ manager_pid/0, sni_option/0, sni_host_info/0, sni_info/0 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type bin_fqdn() :: net_utils:bin_fqdn().

-type bin_directory_path() :: file_utils:bin_directory_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
%-type task_id() :: class_USScheduler:task_id().

-type dispatch_routes() :: class_USWebConfigServer:dispatch_routes().
-type cert_mode() :: class_USWebConfigServer:cert_mode().

-type leec_pid() :: letsencrypt:fsm_pid().
-type thumbprint_map() :: letsencrypt:thumbprint_map().



% The class-specific attributes:
-define( class_attributes, [

	{ fqdn, bin_fqdn(), "the FQDN for which a certificate is to be managed" },

	{ cert_mode, cert_mode(), "tells whether certificate generation is in "
	  "staging or production mode" },

	{ cert_dir, bin_directory_path(),
	  "the directory where certificates shall be written" },

	{ cert_path, maybe( bin_file_path() ),
	  "the full path where the final certificate file (if any) is located" },

	{ cert_renewal_period, maybe( unit_utils:seconds() ),
	  "the base delay between two successful certificate renewals" },

	{ webroot_dir, bin_directory_path(),
	  "the base directory served by the webserver" },

	{ leec_pid, leec_pid(), "the PID of the LEEC FSM" },

	{ scheduler_pid, scheduler_pid(),
	  "the PID of the scheduler used by this manager" },

	{ task_id, maybe( task_id() ), "the identifier of the scheduler task "
	  "(if any) in charge of requesting the certificate renewals" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization,
		 "US.Configuration.USCertificateManager" ).


-define( registration_name, us_web_certificate_manager ).

-define( registration_scope, global_only ).



% The default DHMS periodicity at which the X.509 certificate renewal will be
% requested by a given manager instance for the FQDN it takes care of.
%
% Let’s Encrypt certificate lifetime is 90 days (cf. duration in
% https://letsencrypt.org/docs/faq/); we trigger a renewal with some margin:

% Every 4 minutes (for development):
-define( dhms_cert_renewal_period_development, { 0, 0, 4, 0 } ).

% Every 75 days (minimum being 60 days, as lasting for 90 days and only renewed
% in the last 30 days), for production:
%
-define( dhms_cert_renewal_period_production, { 75, 0, 0, 0 } ).


% The maximum additional (positive) jitter introduced to smooth the load when
% having multiple virtual hosts:

% 30-sec max jitter, for testing:
-define( max_dhms_cert_renewal_jitter_development, { 0, 0, 0, 30 } ).

% 1-day max jitter, for production:
-define( max_dhms_cert_renewal_jitter_production, { 0, 23, 59, 59 } ).


% 50-second delay, for testing:
-define( dhms_cert_renewal_delay_after_failure_development, { 0, 0, 0, 50 } ).

% A bit more than 26-hour delay, for production:
-define( dhms_cert_renewal_delay_after_failure_production, { 0, 26, 17, 45 } ).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Constructs a US certificate manager for the specified FQDN (host in specified
% domain), in production mode, using the specified directory to write
% certificate information, and the specified scheduler for automatic certificate
% renewal.
%
-spec construct( wooper:state(), bin_fqdn(), bin_directory_path(),
		 bin_directory_path(), maybe( scheduler_pid() ) ) -> wooper:state().
construct( State, BinFQDN, BinCertDir, BinWebrootDir, MaybeSchedulerPid ) ->
	construct( State, BinFQDN, _CertMode=production, BinCertDir,
			   BinWebrootDir, MaybeSchedulerPid, _IsSingleton=false ).



% Constructs a US certificate manager for the specified FQDN (host in specified
% domain), in specified certificate management mode, using specified directory
% to write certificate information, and the specified scheduler.
%
% (most complete constructor)
%
-spec construct( wooper:state(), bin_fqdn(), cert_mode(), bin_directory_path(),
				 bin_directory_path(), maybe( scheduler_pid() ), boolean() ) ->
					   wooper:state().
construct( State, BinFQDN, CertMode, BinCertDir, BinWebrootDir,
		   MaybeSchedulerPid, _IsSingleton=true ) ->

	% Relies first on the next, main constructor clause:
	InitState = construct( State, BinFQDN, CertMode, BinCertDir, BinWebrootDir,
						   MaybeSchedulerPid, _Sing=false ),

	% Then self-registering:
	RegName = ?registration_name,
	RegScope = ?registration_scope,

	naming_utils:register_as( RegName, RegScope ),

	setAttributes( InitState, [ { registration_name, RegName },
								{ registration_scope, RegScope } ] );


% No self-registering here:
construct( State, BinFQDN, CertMode, BinCertDir, BinWebrootDir,
		   MaybeSchedulerPid, _IsSingleton=false ) ->

	ServerName =
		text_utils:format( "Certificate manager for ~s", [ BinFQDN ] ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State,
										   ?trace_categorize(ServerName) ),

	{ LEECPid, RenewPeriodSecs } =
		init_leec( BinFQDN, CertMode, BinCertDir, BinWebrootDir, TraceState ),

	% Registration to scheduler to happen in next (first) requestCertificate/1
	% call.

	ReadyState = setAttributes( TraceState, [
		{ fqdn, BinFQDN },
		{ cert_mode, CertMode },
		{ cert_dir, BinCertDir },
		{ cert_path, undefined },
		{ cert_renewal_period, RenewPeriodSecs },
		{ webroot_dir, BinWebrootDir },
		{ leec_pid, LEECPid },
		{ scheduler_pid, MaybeSchedulerPid },
		{ task_id, undefined } ] ),

	?send_info( ReadyState, "Just created: " ++ to_string( ReadyState ) ),

	% Rather than relying on a periodic scheduling, each renewal will just plan
	% the next one, to better account for any failed attempts / delay introduced
	% (and certificates shall be created directly when starting up):
	%
	self() ! requestCertificate,

	ReadyState.



% Initializes our LEEC private instance.
-spec init_leec( bin_fqdn(), cert_mode(), bin_directory_path(),
				 bin_directory_path(), wooper:state() ) -> leec_pid().
init_leec( BinFQDN, CertMode, BinCertDir, BinWebrootDir, State ) ->

	case file_utils:is_existing_directory( BinCertDir ) of

		true ->
			ok;

		false ->
			throw( { non_existing_certificate_directory,
					 text_utils:binary_to_string( BinCertDir ) } )

	end,

	{ BaseRenewPeriodDHMS, JitterMaxDHMS } = case CertMode of

		% Fake, frequently renewed certificates in staging:
		development ->
			{ ?dhms_cert_renewal_period_development,
			  ?max_dhms_cert_renewal_jitter_development };

		% Renewal must be within ]60,90[ days:
		production ->
			{ ?dhms_cert_renewal_period_production,
			  ?max_dhms_cert_renewal_jitter_production }

	end,

	BaseRenewPeriodSecs = time_utils:dhms_to_seconds( BaseRenewPeriodDHMS ),

	JitterMaxSecs = time_utils:dhms_to_seconds( JitterMaxDHMS ),

	% Only positive (delaying) jitter, as random value in [0.0, 1.0[:
	RenewPeriodSecs = BaseRenewPeriodSecs
		+ round( JitterMaxSecs * random_utils:get_random_value() ),

	% Let's start LEEC now:

	% Refer to https://github.com/Olivier-Boudeville/letsencrypt-erlang#api.

	% Slave mode, as we control the webserver for the challenge:
	% (BinCertDir must be writable by this process)

	% No agent_key_file_path specified, a suitable agent key will be
	% auto-generated.
	%
	% No TCP port to be specified on slave mode.

	HttpQueryTimeoutMs = 30000,

	StartBaseOpts = [ { mode, slave },
					  { cert_dir_path, BinCertDir },
					  { webroot_dir_path, BinWebrootDir },
					  { http_timeout, HttpQueryTimeoutMs } ],

	StartOpts = case CertMode of

		development ->
			% For testing only, then based on fake certificates:
			[ staging | StartBaseOpts ];

		production ->
			% No 'prod' option supported by LEEC:
			StartBaseOpts

	end,

	% Enabling the integration of its traces:

	% Otherwise opens subcategories in traces, as an emitter:
	DotlessFQDN = text_utils:substitute( $., $:, BinFQDN ),
	TraceEmitterName = text_utils:format( "LEEC for '~s'", [ DotlessFQDN ] ),

	BridgeSpec = trace_bridge:get_bridge_spec( TraceEmitterName,
		?trace_emitter_categorization, ?getAttr(trace_aggregator_pid) ),


	LEECPid = case letsencrypt:start( StartOpts, BridgeSpec ) of

		{ ok, FsmPid } ->
			?trace_fmt( "LEEC initialized, using FSM of PID ~w, based on "
				"following start options:~n  ~p", [ FsmPid, StartOpts ] ),
			FsmPid;

		{ error, Reason } ->
			?error_fmt( "Initialization of LEEC failed: ~p; "
				"start options were:~n  ~p", [ Reason, StartOpts ] ),
			throw( { leec_initialization_failed, Reason } )

	end,

	{ LEECPid, RenewPeriodSecs }.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Unregistering only from any scheduler, not from any task ring, as we
	% registered to any former and have been registered to any latter.

	MaybeSchedPid = ?getAttr(scheduler_pid),

	CertTaskId = ?getAttr(cert_task_id),

	case MaybeSchedPid of

		undefined ->
			ok;

		_ ->
			?trace( "Being destructed, unregistering from schedule." ),

			% Any extra schedule trigger sent will be lost; not a problem as it
			% is a oneway:
			%
			MaybeSchedPid ! { unregisterTask, [ CertTaskId ], self() }

	end,

	case ?getAttr(leec_pid) of

		undefined ->
			ok;

		LeecPid ->
			?trace( "Shutting down LEEC." ),
			letsencrypt:stop( LeecPid )

	end,

	% End of interleaving:
	case MaybeSchedPid of

		undefined ->
			ok;

		_ ->
			receive

				task_unregistered ->
					ok;

				{ task_unregistration_failed, Error } ->
					?error_fmt( "Unregistration of task #~B failed "
								"at deletion: ~p.", [ CertTaskId, Error ] )

			end

	end,

	?info( "Deleted." ),

	State.




% Method section.


% Requests synchronously a certificate for the management hostname.
-spec requestCertificate( wooper:state() ) -> oneway_return().
requestCertificate( State ) ->

	% The task_id attribute may or may not be defined.

	FQDN = ?getAttr(fqdn),

	?trace_fmt( "Requesting certificate for '~s'.", [ FQDN ] ),

	% Synchronous (blocking) call (no callback used); this clause also returns
	% (after some delay, like 30 seconds) the delay (with some jitter) until the
	% next certificate renewal (quickly if having just failed, normally if
	% having succeeded):
	%
	{ MaybeRenewDelay, MaybeCertPath } =
			case letsencrypt:obtain_certificate_for( FQDN, ?getAttr(leec_pid),
											 _OptionMap=#{ async => false } ) of

		%{ ok, #{ cert := BinCertPath, key := BinCertKey } } ->
		%	?trace_fmt( "Certificate generation success for '~s': "
		%		"certificate file is '~s' and certificate key is '~s'.",
		%		[ FQDN, BinCertPath, BinCertKey ] ),

		{ certificate_ready, BinCertFilePath } ->
			?info_fmt( "Certificate generation success for '~s', "
				"certificate stored in '~s'.", [ FQDN, BinCertFilePath ] ),

			{ ?getAttr(cert_renewal_period), BinCertFilePath };


		{ error, Reason } ->

			?error_fmt( "Certificate generation failed for '~s': ~p.",
						[ Reason ] ),

			% Reasonable offset for next attempt:
			Dur = case ?getAttr(cert_mode) of

				development ->
					time_utils:dhms_to_seconds(
					  ?dhms_cert_renewal_delay_after_failure_development );

				production ->
					time_utils:dhms_to_seconds(
					  ?dhms_cert_renewal_delay_after_failure_production )

			end,
			{ Dur, undefined }

	end,

	NewState = case ?getAttr(scheduler_pid) of

		undefined ->
			?info( "No certificate renewal will be attempted "
				   "(no scheduler registered)." ),
			State;

		SchedPid ->
			case MaybeRenewDelay of

				undefined ->
					?info( "No certificate renewal will be attempted "
						   "(no periodicity defined)." ),
					State;


				RenewDelay ->

					% A bit of interleaving:
					SchedPid ! { registerOneshotTask, [ _Cmd=requestCertificate,
								_Delay=RenewDelay, _ActPid=self() ], self() },

					NextTimestamp = time_utils:offset_timestamp(
						time_utils:get_timestamp(), RenewDelay ),

					?debug_fmt( "Next attempt of certificate renewal to "
					  "take place in ~s, i.e. at ~s.",
					  [ time_utils:duration_to_string( RenewDelay ),
						time_utils:timestamp_to_string( NextTimestamp ) ] ),

					receive

						{ wooper_result, { task_registered, TaskId } } ->
							setAttribute( State, task_id, TaskId );

						% Quite unlikely, yet possible:
						{ wooper_result, task_done } ->
							State

					end

			end

	end,

	CertState = setAttribute( NewState, cert_path, MaybeCertPath ),

	wooper:return_state( CertState ).



% Requests this manager to return the current thumprint challenges.
%
% Typically called from a web handler whenever the ACME well-known URL is read
% by an ACME server.
%
-spec getChallenge( wooper:state() ) ->
						  const_request_return( thumbprint_map() ).
getChallenge( State ) ->

	FSMPid = ?getAttr(leec_pid),

	?trace_fmt( "Requested to return the current thumprint challenges from ~w.",
				[ FSMPid ] ),

	case letsencrypt:get_ongoing_challenges( FSMPid ) of

		error ->
			throw( { no_thumbprint_obtained_from, FSMPid } );

		Thumbprints ->
			wooper:const_return_result( Thumbprints )

	end.



% Callback triggered, as we trap exits, whenever a linked process stops
% (typically should the LEEC FSM crash).
%
-spec onWOOPERExitReceived( wooper:state(), pid(),
							basic_utils:exit_reason() ) -> oneway_return().
onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%						{wooper_oneway_failed,<0.44.0>,class_XXX,
	%							FunName,Arity,Args,AtomCause}}, [...]}"

	FQDN = ?getAttr(fqdn),

	% No need to overwhelm the ACME server in case of permacrash:
	WaitDurationMs = 15000,

	?error_fmt( "Received an exit message '~p' from ~w (for FQDN '~s'), "
		"starting a new LEEC instance after a delay of ~s.",
		[ ExitType, CrashPid, FQDN,
		  text_utils:duration_to_string( WaitDurationMs ) ] ),

	timer:sleep( WaitDurationMs ),

	{ LEECPid, _RenewPeriodSecs } =
		init_leec( FQDN, ?getAttr(cert_mode), ?getAttr(cert_dir),
				   ?getAttr(webroot_dir), State ),

	?info_fmt( "New LEEC instance started for '~s': ~w; requesting new "
			   "certificate.", [ FQDN, LEECPid ] ),

	% Immediately retries:
	self() ! requestCertificate,

	RestartState = setAttribute( State, leec_pid, LEECPid ),

	wooper:return_state( RestartState ).



% Static section.


% Returns SNI information suitable for https-enabled virtual hosts, i.e. the
% path to the PEM certificate for the main, default host (ex: foobar.org),
% together with SNI (Server Name Indication, see
% https://erlang.org/doc/man/ssl.html#type-sni_hosts) host information for the
% other (virtual) hosts (ex: baz.foobar.org, aa.buz.net), etc.
%
-spec get_sni_info( dispatch_routes(), bin_directory_path() ) ->
		  static_return( sni_info() ).
get_sni_info( _UserRoutes, _BinCertDir=undefined ) ->
	throw( no_certificate_directory_for_sni );

get_sni_info( _UserRoutes=[], _BinCertDir ) ->
	throw( no_hostname_for_sni );

get_sni_info( UserRoutes=[ { FirstHostname, _VirtualHosts } | _T ],
			  BinCertDir ) ->

	% For https with SNI, a default host is defined, distinct from the SNI ones;
	% by convention it is the first one found in the user-defined dispatch
	% routes (with no sub-domain/virtual host considered here, i.e. foobar.org,
	% not something.foobar.org); a plain string is required, apparently.
	%
	DefaultCertFilename = FirstHostname ++ ".pem",

	DefaultHostnameCertPath =
		file_utils:join( BinCertDir, DefaultCertFilename ),

	% Options to apply for the host that matches what the client requested with
	% Server Name Indication:
	%
	SNIHostInfos = list_utils:flatten_once(
				  [ get_virtual_host_sni_infos( H, VH, BinCertDir )
					|| { H, VH } <- UserRoutes ] ),

	trace_utils:debug_fmt( "SNI information: certificate path for the default "
		"hostname is '~s', virtual host options are:~n~p",
		[ DefaultHostnameCertPath, SNIHostInfos ] ),

	wooper:return_static( { DefaultHostnameCertPath, SNIHostInfos } ).




% Helper section.


% No domain-level wildcard certificate:
get_virtual_host_sni_infos( _Hostname=default_domain_catch_all,
							_VHostInfos, _BinCertDir ) ->
	[];

get_virtual_host_sni_infos( _Hostname="localhost", _VHostInfos, _BinCertDir ) ->
	[];

get_virtual_host_sni_infos( Hostname, VHostInfos, BinCertDir ) ->
	get_vh_sni_infos_for( Hostname, VHostInfos, BinCertDir, _Acc=[] ).



% (helper)
get_vh_sni_infos_for( _Hostname, _VHostInfos=[], _BinCertDir, Acc ) ->
	% Preferring to respect the order from the user rules:
	lists:reverse( Acc );

% No host-level wildcard certificate:
get_vh_sni_infos_for( Hostname,
		_VHostInfos=[ { _VH=default_vhost_catch_all, _ContentRoot } | T ],
		BinCertDir, Acc ) ->
	get_vh_sni_infos_for( Hostname, T, BinCertDir, Acc );

get_vh_sni_infos_for( Hostname,
		_VHostInfos=[ { VHostname, _ContentRoot } | T ], BinCertDir, Acc ) ->
	VHPair = get_vh_pair( Hostname, VHostname, BinCertDir ),
	get_vh_sni_infos_for( Hostname, T, BinCertDir, [ VHPair | Acc ] );

get_vh_sni_infos_for( Hostname,
		_VHostInfos=[ { VHostname, _ContentRoot, _WebKind } | T ], BinCertDir,
		Acc ) ->
	VHPair = get_vh_pair( Hostname, VHostname, BinCertDir ),
	get_vh_sni_infos_for( Hostname, T, BinCertDir, [ VHPair | Acc ] );

get_vh_sni_infos_for( Hostname, [ Unexpected | _T ], _BinCertDir, _Acc ) ->
	throw( { unexpected_vhost_info_for_snis, Unexpected, Hostname } ).


% (helper)
get_vh_pair( Hostname, VHostname, BinCertDir ) ->
	FQDN = VHostname ++ [ $. | Hostname ],
	CertFilename = FQDN ++ ".pem",
	CertFilePath = file_utils:join( BinCertDir, CertFilename ),
	{ FQDN, [ { certfile, CertFilePath } ] }.



% Returns a textual description of this server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	PeriodStr = case ?getAttr(cert_renewal_period) of

		undefined ->
			"no periodic renewal";

		PeriodSecs ->
			text_utils:format( "with a renewal every ~s",
				[ time_utils:duration_to_string( 1000 * PeriodSecs ) ] )

	end,

	FSMStr = case ?getAttr(leec_pid) of

		undefined ->
			"no LEEC FSM";

		LeecPid ->
			text_utils:format( "LEEC FSM of PID ~s", [ LeecPid ] )

	end,

	CertStr = case ?getAttr(cert_path) of

		undefined ->
			"no certificate generated";

		CertPath ->
			text_utils:format( "a certificate generated in '~s'", [ CertPath ] )

	end,

	text_utils:format( "US certificate manager for '~s', using "
		"certificate directory '~s', with ~s, using ~s, with ~s",
		[ ?getAttr(fqdn), ?getAttr(cert_dir), PeriodStr, FSMStr, CertStr ] ).
