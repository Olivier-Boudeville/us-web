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

% For certificate renewals, rather than relying on a periodic scheduling defined
% a priori, each renewal will just plan the next one, to better account for any
% failed attempts / delay introduced (and certificates shall be created directly
% when starting up).
%
% Previously for each certificate generation for each virtual host a throwaway
% Let's Encrypt account was created, yet it led to hitting quickly their rate
% limits. So now we generate first a (unique) TLS private key, create a (single)
% ACME account that all certificate managers will use to order their
% certificate. SAN information and wildcard certificates could also be used for
% that.
%
% Initially, the call to letsencrypt:obtain_certificate_for/3 below was
% synchronous (blocking), however then the corresponding LEEC web handler
% process that would be spawned whenever the ACME server requested the token
% would lead to this LEEC handler process requesting the challenge from the
% current certificate manager - whereas this one was blocked by design. So now
% certificate managers use non-blocking calls to obtain their certificate.


% Initially a certificate manager was not trapping exits, so that any crash was
% to propagate to the US-Web config server; now trapping them to detect for
% example the crash of the associated LEEC FSM, and manage it (resisting and
% relaunching it).


-type manager_pid() :: class_UniversalServer:server_pid().


% Per-virtual-host SNI (SSL-related) option (ranch_ssl:opts()):
-type ssl_option() :: ssl:server_option() | ssl:common_option().

% Virtual-host pair storing SNI-related certificate information:
-type sni_host_info() :: { net_utils:string_host_name(), [ ssl_option() ] }.


% Information regarding Server Name Indication: transport information for the
% main, default hostname, and per-virtual host information.
%
-type sni_info() :: { [ ssl_option() ], [ sni_host_info() ] }.


% Identifier of a cipher (ex: 'AES128-SHA').
-type cipher_name() :: atom().

-export_type([ manager_pid/0, ssl_option/0, sni_host_info/0, sni_info/0,
			   cipher_name/0 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type bin_fqdn() :: net_utils:bin_fqdn().

-type seconds() :: unit_utils:seconds().

-type bin_directory_path() :: file_utils:bin_directory_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
%-type task_id() :: class_USScheduler:task_id().

-type dispatch_routes() :: class_USWebConfigServer:dispatch_routes().
-type cert_mode() :: class_USWebConfigServer:cert_mode().

-type leec_pid() :: letsencrypt:fsm_pid().

-type bin_san() :: letsencrypt:bin_san().



% The class-specific attributes:
-define( class_attributes, [

	{ fqdn, bin_fqdn(), "the FQDN for which a certificate is to be managed" },

	{ cert_mode, cert_mode(), "tells whether certificate generation is in "
	  "staging or production mode" },

	{ cert_dir, bin_directory_path(),
	  "the directory where certificates shall be written" },

	{ cert_path, maybe( bin_file_path() ),
	  "the full path where the final certificate file (if any) is located" },

	{ sans, [ bin_san() ], "the Subject Alternative Names to be included "
	  "in the generated certificates" },

	{ private_key_path, bin_file_path(), "the (absolute) path to the TLS "
	  "private key to be used by the LEEC agent driven by this certificate "
	  "manager" },

	{ cert_renewal_period, maybe( seconds() ),
	  "the base delay between two successful certificate renewals" },

	{ renew_listener, maybe( pid() ),
	  "the PID of the process (if any) to notify once the next certificate is "
	  "obtained" },

	{ leec_pid, leec_pid(), "the PID of our private LEEC FSM" },

	{ scheduler_pid, scheduler_pid(),
	  "the PID of the scheduler used by this manager" },

	{ task_id, maybe( task_id() ), "the identifier of the scheduler task "
	  "(if any) in charge of requesting the certificate renewals" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Certificate Management" ).


-define( registration_name, us_web_certificate_manager ).

-define( registration_scope, global_only ).



% The default DHMS periodicity at which the X.509 certificate renewal will be
% requested by a given manager instance for the FQDN it takes care of.
%
% Let’s Encrypt certificate lifetime is 90 days (cf. duration in
% https://letsencrypt.org/docs/faq/); we trigger a renewal with some margin:

% Every 15 minutes (for development):
-define( dhms_cert_renewal_period_development, { 0, 0, 15, 0 } ).

% Every 75 days (minimum being 60 days, as lasting for 90 days and only renewed
% in the last 30 days), for production:
%
-define( dhms_cert_renewal_period_production, { 75, 0, 0, 0 } ).


% The maximum additional (positive) jitter introduced to smooth the load when
% having multiple virtual hosts:

% 30-sec max jitter, for testing:
-define( max_dhms_cert_renewal_jitter_development, { 0, 0, 0, 30 } ).

% 1-day max jitter, for production:
-define( max_dhms_cert_renewal_jitter_production, { 1, 0, 0, 0 } ).


% 50-second delay, for testing:
-define( dhms_cert_renewal_delay_after_failure_development, { 0, 0, 0, 50 } ).

% A bit more than 26-hour delay, for production:
-define( dhms_cert_renewal_delay_after_failure_production, { 0, 26, 17, 45 } ).


% Public (PEM) certificate extension (could have been ".pem"):
-define( cert_extension, ".crt" ).

% Private key extension:
-define( priv_key_extension, ".key" ).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Constructs a US certificate manager for the specified FQDN (host in specified
% domain), in production mode, using the specified directory to write
% certificate information, and the specified scheduler for automatic certificate
% renewal.
%
-spec construct( wooper:state(), bin_fqdn(), [ bin_san() ],
		bin_directory_path(), bin_file_path(), maybe( scheduler_pid() ) ) ->
					   wooper:state().
construct( State, BinFQDN, BinSans, BinCertDir, BinAgentKeyPath,
		   MaybeSchedulerPid ) ->
	construct( State, BinFQDN, BinSans, _CertMode=production, BinCertDir,
			   BinAgentKeyPath, MaybeSchedulerPid, _IsSingleton=false ).



% Constructs a US certificate manager for the specified FQDN (host in specified
% domain), in specified certificate management mode, using specified directory
% to write certificate information, and the specified scheduler.
%
% (most complete constructor)
%
-spec construct( wooper:state(), bin_fqdn(), [ bin_san() ], cert_mode(),
	bin_directory_path(), bin_file_path(), maybe( scheduler_pid() ),
	boolean() ) -> wooper:state().
construct( State, BinFQDN, BinSans, CertMode, BinCertDir, BinAgentKeyPath,
		   MaybeSchedulerPid, _IsSingleton=true ) ->

	% Relies first on the next, main constructor clause:
	InitState = construct( State, BinFQDN, BinSans, CertMode, BinCertDir,
						   BinAgentKeyPath, MaybeSchedulerPid, _Sing=false ),

	% Then self-registering:
	RegName = ?registration_name,
	RegScope = ?registration_scope,

	naming_utils:register_as( RegName, RegScope ),

	% Inherited attributes:
	setAttributes( InitState, [ { registration_name, RegName },
								{ registration_scope, RegScope } ] );


% Main constructor; no self-registering here:
construct( State, BinFQDN, BinSans, CertMode, BinCertDir, BinAgentKeyPath,
		   MaybeSchedulerPid, _IsSingleton=false ) ->

	ServerName =
		text_utils:format( "Certificate manager for ~s", [ BinFQDN ] ),

	% First the direct mother classes, then this class-specific actions:
	%
	% (trapping EXITs, as wanting to detect any crash of a LEEC FSM, calling
	% then onWOOPERExitReceived/3)
	%
	TraceState = class_USServer:construct( State,
						?trace_categorize(ServerName), _TrapExits=true ),

	% For example for any stateless helper:
	class_TraceEmitter:register_bridge( TraceState ),

	% Just a start thereof (LEEC plus its associated, linked, FSM; no
	% certificate request issued yet):
	%
	{ LEECFsmPid, RenewPeriodSecs } =
		init_leec( BinFQDN, CertMode, BinCertDir, BinAgentKeyPath, TraceState ),

	% Registration to scheduler to happen in next (first) renewCertificate/1
	% call.

	ReadyState = setAttributes( TraceState, [
		{ fqdn, BinFQDN },
		{ cert_mode, CertMode },
		{ cert_dir, BinCertDir },
		{ cert_path, undefined },
		{ sans, BinSans },
		{ private_key_path, BinAgentKeyPath },
		{ cert_renewal_period, RenewPeriodSecs },
		{ renew_listener, undefined },
		{ leec_pid, LEECFsmPid },
		{ scheduler_pid, MaybeSchedulerPid },
		{ task_id, undefined } ] ),

	?send_info_fmt( ReadyState, "Constructed: ~s.",
					[ to_string( ReadyState ) ] ),

	% Would be too early, as the HTTP webserver needed to validate the ACME
	% challenges is not launched yet:
	%
	% self() ! renewCertificate,

	ReadyState.



% Initializes our LEEC private instance.
-spec init_leec( bin_fqdn(), cert_mode(), bin_directory_path(), bin_file_path(),
				 wooper:state() ) -> leec_pid().
init_leec( BinFQDN, CertMode, BinCertDir, BinAgentKeyPath, State ) ->

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
					  { agent_key_file_path, BinAgentKeyPath },
					  { cert_dir_path, BinCertDir },
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
	TraceEmitterName = text_utils:format( "LEEC for ~s", [ DotlessFQDN ] ),

	BridgeSpec = trace_bridge:get_bridge_spec( TraceEmitterName,
		?trace_emitter_categorization, ?getAttr(trace_aggregator_pid) ),

	LEECFsmPid = case letsencrypt:start( StartOpts, BridgeSpec ) of

		{ ok, FsmPid } ->
			?debug_fmt( "LEEC initialized, using FSM of PID ~w, based on "
				"following start options:~n  ~p", [ FsmPid, StartOpts ] ),
			FsmPid;

		{ error, Reason } ->
			?error_fmt( "Initialization of LEEC failed: ~p; "
				"start options were:~n  ~p", [ Reason, StartOpts ] ),
			throw( { leec_initialization_failed, Reason } )

	end,

	{ LEECFsmPid, RenewPeriodSecs }.



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
			?debug( "Being destructed, unregistering from schedule." ),

			% Any extra schedule trigger sent will be lost; not a problem as it
			% is a oneway:
			%
			MaybeSchedPid ! { unregisterTask, [ CertTaskId ], self() }

	end,

	case ?getAttr(leec_pid) of

		undefined ->
			ok;

		LeecPid ->
			?debug( "Shutting down LEEC." ),
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


% Renews asynchronously a certificate for the managed hostname.
-spec renewCertificate( wooper:state() ) -> const_oneway_return().
renewCertificate( State ) ->

	?debug( "Requested to renew certificate asynchronously." ),

	request_certificate( State ),

	% onCertificateRequestOutcome/2 callback to be triggered soon.

	wooper:const_return().



% Renews a certificate for the managed hostname on a synchronisable manner.
%
% The specified listener is typically the US-Web configuration server, so that
% HTTPS support can be triggered only when certificates are ready.
%
-spec renewCertificateSynchronisable( wooper:state(), pid() ) ->
			oneway_return().
renewCertificateSynchronisable( State, ListenerPid ) ->

	% Note that these managers must not be frozen here in the waiting of a
	% onCertificateRequestOutcome message, as they have to remain responsive to
	% branch the US-Web Letsencrypt handler to the corresponding LEEC FSM, so
	% that the challenges can be returned to the ACME server for this procedure
	% to complete.

	?debug( "Requested to renew certificate in a synchronisable manner." ),

	% Also a check:
	{ ListenState, undefined } =
		swapInAttribute( State, renew_listener, ListenerPid ),

	request_certificate( ListenState ),

	% onCertificateRequestOutcome/2 callback to be triggered soon.

	wooper:return_state( ListenState ).



% (helper)
-spec request_certificate( wooper:state() ) -> void().
request_certificate( State ) ->

	% The task_id attribute may or may not be defined.

	FQDN = ?getAttr(fqdn),

	% We used to rely on a synchronous (blocking) call (no callback was used),
	% which was a mistake (see implementation notes). Now using an asynchronous
	% call instead.

	% Closure:
	Self = self(),

	Callback = fun( CertCreationOutcome ) ->
				Self ! { onCertificateRequestOutcome, [ CertCreationOutcome ] }
			   end,

	ActualSans = case ?getAttr(sans) of

		undefined ->
			[];

		S ->
			S

	end,

	?debug_fmt( "Requesting certificate for '~s', with SAN information ~p.",
				[ FQDN, ActualSans ] ),

	async = letsencrypt:obtain_certificate_for( FQDN, ?getAttr(leec_pid),
		_CertReqOptionMap=#{ async => true,
							 callback => Callback,
							 sans => ActualSans } ),

	?debug( "Certificate creation request initiated." ).



% Method to be called back whenever a certificate was requested.
-spec onCertificateRequestOutcome( wooper:state(),
			letsencrypt:cert_creation_outcome() ) -> oneway_return().
onCertificateRequestOutcome( State,
			_CertCreationOutcome={ certificate_ready, BinCertFilePath } ) ->

	FQDN = ?getAttr(fqdn),

	?info_fmt( "Certificate generation success for '~s', "
			   "certificate stored in '~s'.", [ FQDN, BinCertFilePath ] ),

	SetState = case ?getAttr(renew_listener) of

		undefined ->
			State;

		ListenerPid ->
			% Most probably the US-Web server waiting in renewCertificates/1:
			ListenerPid ! { _AckAtom=certificate_renewal_over, self() },
			setAttribute( State, renew_listener, undefined )

	end,

	NewState = manage_renewal( _RenewDelay=?getAttr(cert_renewal_period),
							   BinCertFilePath, SetState ),

	wooper:return_state( NewState );


onCertificateRequestOutcome( State,
							 _CertCreationOutcome={ error, ErrorTerm } ) ->

	FQDN = ?getAttr(fqdn),

	?error_fmt( "Certificate generation failed for '~s': ~p.",
				[ FQDN, ErrorTerm ] ),

	% Reasonable offset for next attempt:
	RenewDelay = case ?getAttr(cert_mode) of

		development ->
			time_utils:dhms_to_seconds(
				?dhms_cert_renewal_delay_after_failure_development );

		production ->
			time_utils:dhms_to_seconds(
				?dhms_cert_renewal_delay_after_failure_production )

	end,

	MaybeBinCertFilePath = undefined,

	NewState = manage_renewal( RenewDelay, MaybeBinCertFilePath, State ),

	wooper:return_state( NewState );


onCertificateRequestOutcome( State, _CertCreationOutcome=UnexpectedError ) ->

	?error_fmt( "Unexpected certificate creation error: ~p.",
				[ UnexpectedError ] ),

	throw( { unexpected_cert_creation_error, UnexpectedError } ).



% (helper)
-spec manage_renewal( maybe( seconds() ), maybe( bin_file_path() ),
					  wooper:state() ) -> wooper:state().
manage_renewal( MaybeRenewDelay, MaybeBinCertFilePath, State ) ->

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
					SchedPid ! { registerOneshotTask, [ _Cmd=renewCertificate,
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

	setAttribute( NewState, cert_path, MaybeBinCertFilePath ).




% Requests this manager to return (indirectly, through the current LEEC FSM) the
% current thumbprint challenges to specified target process.
%
% Typically called from a web handler (see us_web_letsencrypt_handler,
% specifying its PID as target one) whenever the ACME well-known URL is read by
% an ACME server, to check that the returned challenges match the expected ones.
%
-spec getChallenge( wooper:state(), pid() ) -> const_oneway_return().
getChallenge( State, TargetPid ) ->

	FSMPid = ?getAttr(leec_pid),

	?debug_fmt( "Requested to return the current thumbprint challenges "
		"from LEEC FSM ~w, on behalf of (and to) ~w.", [ FSMPid, TargetPid ] ),

	letsencrypt:send_ongoing_challenges( FSMPid, TargetPid ),

	wooper:const_return().




% Callback triggered, as we trap exits, whenever a linked process stops
% (typically should the LEEC FSM crash).
%
-spec onWOOPERExitReceived( wooper:state(), pid(),
							basic_utils:exit_reason() ) -> oneway_return().
onWOOPERExitReceived( State, _StopPid, _ExitType=normal ) ->

	% Executables triggering useless messages:
	%?notice_fmt( "Ignoring normal exit from process ~w.", [ StopPid ] ),

	wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%						{wooper_oneway_failed,<0.44.0>,class_XXX,
	%							FunName,Arity,Args,AtomCause}}, [...]}"

	FQDN = ?getAttr(fqdn),

	% No need to overwhelm the ACME server in case of permacrash:
	%WaitDurationMs = 15000,
	WaitDurationMs = 4*3600*1000,

	?error_fmt( "Received an exit message '~p' from ~w (for FQDN '~s'), "
		"starting a new LEEC instance after a delay of ~s.",
		[ ExitType, CrashPid, FQDN,
		  time_utils:duration_to_string( WaitDurationMs ) ] ),

	timer:sleep( WaitDurationMs ),

	{ LEECPid, _RenewPeriodSecs } =
		init_leec( FQDN, ?getAttr(cert_mode), ?getAttr(cert_dir),
				   ?getAttr(private_key_path), State ),

	?info_fmt( "New LEEC instance started for '~s': ~w; requesting new "
			   "certificate.", [ FQDN, LEECPid ] ),

	% Immediately retries:
	self() ! renewCertificate,

	RestartState = setAttribute( State, leec_pid, LEECPid ),

	wooper:return_state( RestartState ).



% Static section.


% Returns SNI information suitable for https-enabled virtual hosts, i.e. the
% transport options for the domain of interest - i.e. the path to the PEM
% certificate for the main, default host (ex: foobar.org) and to its private
% key, together with SNI (Server Name Indication, see
% https://erlang.org/doc/man/ssl.html#type-sni_hosts) host information for all
% virtual host of all hosts (ex: baz.foobar.org, aa.buz.net, etc.).
%
% See also https://ninenines.eu/docs/en/ranch/2.0/manual/ranch_ssl/.
%
% Note: we rely on the user routes rather than on the domain table as we need to
% determine the first hots listed as the main one.
%
-spec get_sni_info( dispatch_routes(), bin_directory_path() ) ->
						  static_return( sni_info() ).
get_sni_info( _, _BinCertDir=undefined ) ->
	throw( no_certificate_directory_for_sni );

get_sni_info( _UserRoutes=[], _BinCertDir ) ->
	throw( no_hostname_for_sni );

get_sni_info( UserRoutes=[ { FirstHostname, _VirtualHosts } | _T ],
			  BinCertDir ) ->

	% For https with SNI, a default host must be defined, distinct from the SNI
	% ones; by convention it is the first one found in the user-defined dispatch
	% routes (with no sub-domain/virtual host considered here, i.e. foobar.org,
	% not something.foobar.org); a plain string is required, apparently.
	%
	MainTranspOpts = get_transport_opts_for( FirstHostname, BinCertDir ),

	% Options to apply for the host that matches what the client requested with
	% Server Name Indication (going through *all* the hosts listed by the user):
	%
	% Now that virtual hosts are listed as SANs in a single host-level
	% certificate:
	%
	SNIHostInfos = list_utils:flatten_once(
					[ get_virtual_host_sni_infos( H, VH, BinCertDir )
						|| { H, VH } <- UserRoutes ] ),


	% Redundant:
	%cond_utils:if_defined( us_web_debug_sni, trace_utils:debug_fmt(
	%	"SNI information: transport options for the default "
	%	"hostname are: ~p.~nVirtual host options are:~n~p",
	%	[ MainTranspOpts, SNIHostInfos ] ) ),

	wooper:return_static( { MainTranspOpts, SNIHostInfos } ).



% Returns an (ordered) list of the recommended ciphers for a webserver.
%
% An important security setting is to force the cipher to be set based on the
% server-specified order instead of the client-specified oner, hence enforcing
% the (usually more properly configured) security ordering of the server
% administrator.
%
-spec get_recommended_ciphers() -> static_return( [ cipher_name() ] ).
get_recommended_ciphers() ->
	wooper:return_static( [ "ECDHE-ECDSA-AES256-GCM-SHA384",
	  "ECDHE-RSA-AES256-GCM-SHA384",
	  "ECDHE-ECDSA-AES256-SHA384", "ECDHE-RSA-AES256-SHA384",
	  "ECDHE-ECDSA-DES-CBC3-SHA", "ECDH-ECDSA-AES256-GCM-SHA384",
	  "ECDH-RSA-AES256-GCM-SHA384", "ECDH-ECDSA-AES256-SHA384",
	  "ECDH-RSA-AES256-SHA384", "DHE-DSS-AES256-GCM-SHA384",
	  "DHE-DSS-AES256-SHA256", "AES256-GCM-SHA384", "AES256-SHA256",
	  "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256",
	  "ECDHE-ECDSA-AES128-SHA256", "ECDHE-RSA-AES128-SHA256",
	  "ECDH-ECDSA-AES128-GCM-SHA256", "ECDH-RSA-AES128-GCM-SHA256",
	  "ECDH-ECDSA-AES128-SHA256", "ECDH-RSA-AES128-SHA256",
	  "DHE-DSS-AES128-GCM-SHA256", "DHE-DSS-AES128-SHA256",
	  "AES128-GCM-SHA256", "AES128-SHA256", "ECDHE-ECDSA-AES256-SHA",
	  "ECDHE-RSA-AES256-SHA", "DHE-DSS-AES256-SHA", "ECDH-ECDSA-AES256-SHA",
	  "ECDH-RSA-AES256-SHA", "AES256-SHA", "ECDHE-ECDSA-AES128-SHA",
	  "ECDHE-RSA-AES128-SHA", "DHE-DSS-AES128-SHA", "ECDH-ECDSA-AES128-SHA",
	  "ECDH-RSA-AES128-SHA", "AES128-SHA" ] ).



% Helper section.


% No domain-level wildcard certificate:
get_virtual_host_sni_infos( _Hostname=default_domain_catch_all,
							_VHostInfos, _BinCertDir ) ->
	[];

% Not visible outside of the LAN:
get_virtual_host_sni_infos( _Hostname="localhost", _VHostInfos, _BinCertDir ) ->
	[];

get_virtual_host_sni_infos( Hostname, VHostInfos, BinCertDir ) ->
	get_vh_sni_infos_for( Hostname, VHostInfos, BinCertDir, _Acc=[] ).



% (helper)
get_vh_sni_infos_for( _Hostname, _VHostInfos=[], _BinCertDir, Acc ) ->
	% Preferring to respect the order from the user rules:
	lists:reverse( Acc );

% No host-level wildcard certificate, yet we list it as a SNI option (multiple
% hostnames can be managed by a single server):
%
get_vh_sni_infos_for( Hostname,
		_VHostInfos=[ { _VH=default_vhost_catch_all, _ContentRoot } | T ],
		BinCertDir, Acc ) ->
	HPair = { Hostname, get_transport_opts_for( Hostname, BinCertDir ) },
	get_vh_sni_infos_for( Hostname, T, BinCertDir, [ HPair | Acc ] );

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

	% Now that SANs are used, we point to the corresponding global, host-level
	% certificate information:
	%
	%TranspOpts = get_transport_opts_for( FQDN, BinCertDir ),
	TranspOpts = get_transport_opts_for( Hostname, BinCertDir ),

	{ FQDN, TranspOpts }.



% Returns transport options suitable for specified FQDN.
-spec get_transport_opts_for( net_utils:string_fqdn(), bin_directory_path() ) ->
								static_return( [ ssl_option() ] ).
get_transport_opts_for( FQDN, BinCertDir ) ->

	CertFilename = FQDN ++ ?cert_extension,
	CertFilePath = file_utils:join( BinCertDir, CertFilename ),

	PrivKeyFilename = FQDN ++ ?priv_key_extension,
	PrivKeyFilePath = file_utils:join( BinCertDir, PrivKeyFilename ),

	wooper:return_static(
	  [ { certfile, CertFilePath }, { keyfile, PrivKeyFilePath } ] ).



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
			text_utils:format( "LEEC FSM of PID ~w", [ LeecPid ] )

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
