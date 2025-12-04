% Copyright (C) 2020-2025 Olivier Boudeville
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

-moduledoc """
Class in charge of managing the **generation and renewal of X.509 certificates**
for a given domain.
""".


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
% namely https://github.com/Olivier-Boudeville/Ceylan-LEEC.
%
% Each instance of this class is dedicated to the certificates for a given FQDN.
% This allows multiplexing requests possibly for several FQDNs in parallel more
% easily.
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
% certificate. SAN information (and, possibly in the future with other challenge
% types, wildcard certificates) are also useful for that.
%
% Initially, the call to leec:obtain_certificate_for/3 below was synchronous
% (blocking), however then the corresponding LEEC web handler process that would
% be spawned whenever the ACME server requested the token would lead to this
% LEEC handler process requesting the challenge from the current certificate
% manager - whereas this one was blocked by design. So now certificate managers
% use non-blocking calls to obtain their certificate.

% Note that just *creating* (even before requesting any certificate) multiple
% certificate managers may already lead to hitting the rate limits of the ACME
% server (because of the LEEC initialisation, resulting in its FSM getting
% directory map, nonces, etc.). So the creation of certificate managers shall
% preferably done with a synchronous new operator.

% Initially certificate managers did not trap exit messages, so that any crash
% was to propagate to the US-Web config server; now managers trap them to detect
% for example the crash of the associated LEEC FSM, and manage such a crash
% (resisting and relaunching it).

% Finally, because a long time is to elapse between two certificate renewals for
% a given domain, no LEEC instance must be re-used (the lastly used nonce must
% have expired a long time ago), and thus after a renewal LEEC must be stopped
% (a new nonce will be obtained as soon as the next instance starts).


-doc "The PID of a certificate manager.".
-type cert_manager_pid() :: class_USServer:server_pid().


% One day ssl will export these types:


-doc "Per-virtual-host SNI (SSL-related) option (ranch_ssl:opts()).".
-type ssl_option() :: ssl:server_option() | ssl:common_option().



-doc "Virtual-host pair storing SNI-related certificate information.".
-type sni_host_info() :: { net_utils:string_host_name(), [ ssl_option() ] }.



-doc """
Information regarding Server Name Indication: transport information for the
main, default hostname, and per-virtual host information.
""".
-type sni_info() :: { [ ssl_option() ], [ sni_host_info() ] }.



-doc "Identifier of a cipher (e.g. 'AES128-SHA').".
-type cipher_name() :: atom().


-export_type([ cert_manager_pid/0, ssl_option/0, sni_host_info/0, sni_info/0,
               cipher_name/0 ]).



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type bin_fqdn() :: net_utils:bin_fqdn().

-type seconds() :: unit_utils:seconds().

-type bin_directory_path() :: file_utils:bin_directory_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type bridge_spec() :: trace_bridge:bridge_spec().

%-type task_id() :: class_USScheduler:task_id().

-type dispatch_routes() :: class_USWebCentralServer:dispatch_routes().
-type cert_mode() :: class_USWebCentralServer:cert_mode().
-type https_transport_info() :: class_USWebCentralServer:https_transport_info().

-type leec_caller_state() :: leec:leec_caller_state().
-type challenge_type() :: leec:challenge_type().
-type dns_provider() :: leec:dns_provider().
-type bin_san() :: leec:bin_san().
-type leec_start_options() :: [ leec:start_option() ].




% The class-specific attributes:
-define( class_attributes, [

    { fqdn, bin_fqdn(), "the FQDN for which a certificate is to be managed" },

    { cert_mode, cert_mode(), "tells whether certificate generation is in "
      "staging or production mode" },

    { challenge_type, challenge_type(),
      "the type of ACME challenge to be used in order to generate "
      "these certificates" },

    { dns_provider, option( dns_provider() ),
      "the target DNS provider, to succeed any dns-01 challenge" },

    { credentials_dir, option( bin_directory_path() ),
      "the directory where the credentials information will be looked-up, "
      "typically to authenticate to one's DNS provider for dns-01 challenges" },

    { cert_dir, bin_directory_path(),
      "the directory where certificates shall be written" },

    { cert_path, option( bin_file_path() ),
      "the full path where the final certificate file (if any) is located" },

    { sans, [ bin_san() ], "the Subject Alternative Names to be included "
      "in the generated certificates" },

    { private_key_path, option( bin_file_path() ),
      "the (absolute) path to the TLS private key to be used by the LEEC "
      "agent driven by this certificate manager (if not using certbot)" },

    { cert_renewal_period, option( seconds() ),
      "the base delay between two successful certificate renewals" },

    { renew_listener, option( pid() ),
      "the PID of the process (if any) to notify once the next certificate is "
      "obtained" },

    { leec_caller_state, option( leec_caller_state() ),
      "the (opaque) caller state for our private LEEC FSM "
      "(if any currently exists)" },

    { leec_start_opts, leec_start_options(),
      "the start options to be re-used for the next created LEEC instance" },

    { bridge_spec, bridge_spec(),
      "the specification of a corresponding trace bridge, typically for "
      "the next created LEEC instance" },

    { task_id, option( task_id() ), "the identifier of the scheduler task "
      "(if any) in charge of requesting the certificate renewals" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Certificate Management" ).


-define( cert_manager_registration_name, us_web_certificate_manager ).

-define( cert_manager_registration_scope, global_only ).



% The default DHMS periodicity at which the X.509 certificate renewal will be
% requested by a given manager instance for the FQDN it takes care of.
%
% Let’s Encrypt certificate lifetime is 90 days (cf. duration in
% [https://letsencrypt.org/docs/faq/]); we trigger a renewal with some margin:

% For development, each hour:
-define( dhms_cert_renewal_period_development, { 0, 1, 0, 0 } ).

% Every 70 days (minimum being 60 days, as lasting for up to 90 days and only
% renewable in the last 30 days), for production:
%
-define( dhms_cert_renewal_period_production, { 70, 0, 0, 0 } ).


% The maximum additional (positive) jitter introduced to smooth the load when
% having multiple domains.
%
% A sufficiently large jitter is all the more welcome than for (post-creation)
% renewals, the LEEC agents for the various domains of interest are not
% specifically synchronised (e.g. no task ring for them), and we do not want too
% many of these renewals to possibly happen at the same time, lest we hit the
% numerous rate limits enforced by the ACME letsencrypt servers.

% 30-sec max jitter, for testing (warning: concurrent requests may thus
% happen and hit rate limits):
%
-define( max_dhms_cert_renewal_jitter_development, { 0, 0, 0, 30 } ).

% 4-day max jitter, for production:
-define( max_dhms_cert_renewal_jitter_production, { 4, 0, 0, 0 } ).


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



-doc """
Constructs a US certificate manager for the specified FQDN (host in specified
domain), relying on the specified challenge type and possibly DNS provider, in
production mode, using the specified directory to write certificate information.

No directory for credentials specified.
""".
-spec construct( wooper:state(), bin_fqdn(), [ bin_san() ], challenge_type(),
        option( dns_provider() ), bin_directory_path(),
        option( bin_file_path() ) ) -> wooper:state().
construct( State, BinFQDN, BinSans, ChalType, MaybeDNSProvider, BinCertDir,
           MaybeBinAgentKeyPath ) ->
    construct( State, BinFQDN, BinSans, ChalType, MaybeDNSProvider,
        _MaybeBinCredentialsDir=undefined, _CertMode=production, BinCertDir,
        MaybeBinAgentKeyPath, _IsSingleton=false ).



-doc """
Constructs a US certificate manager for the specified FQDN (host in specified
domain), in the specified certificate management mode, relying on the specified
challenge type and possibly DNS provider, using specified directory to write
certificate information.

(most complete constructor)
""".
-spec construct( wooper:state(), bin_fqdn(), [ bin_san() ], cert_mode(),
    challenge_type(), option( dns_provider() ), option( bin_directory_path() ),
    bin_directory_path(), option( bin_file_path() ), boolean() ) ->
                                                            wooper:state().
construct( State, BinFQDN, BinSans, CertMode, ChalType, MaybeDNSProvider,
        MaybeBinCredentialsDir, BinCertDir, MaybeBinAgentKeyPath,
        _IsSingleton=true ) ->

    % Relies first on the next, main constructor clause:
    InitState = construct( State, BinFQDN, BinSans, CertMode, ChalType,
        MaybeDNSProvider, MaybeBinCredentialsDir, BinCertDir,
        MaybeBinAgentKeyPath, _Sing=false ),

    % Then self-registering:
    RegName = ?cert_manager_registration_name,
    RegScope = ?cert_manager_registration_scope,

    naming_utils:register_as( RegName, RegScope ),

    % Inherited attributes:
    setAttributes( InitState, [ { registration_name, RegName },
                                { registration_scope, RegScope } ] );


% Main constructor; no self-registering here:
construct( State, BinFQDN, BinSans, CertMode, ChalType, MaybeDNSProvider,
           MaybeBinCredentialsDir, BinCertDir, MaybeBinAgentKeyPath,
           _IsSingleton=false ) ->

    ServerName =
        text_utils:format( "Certificate manager for ~ts", [ BinFQDN ] ),

    % First the direct mother classes, then this class-specific actions:
    %
    % (trapping EXITs, as wanting to detect any crash of a LEEC FSM, calling
    % then onWOOPERExitReceived/3)
    %
    SrvState = class_USServer:construct( State,
        ?trace_categorize(ServerName), _TrapExits=true ),


    %?send_info( SrvState, "Construction started." ),

    % Just a start thereof (LEEC plus its associated, linked, FSM; no
    % certificate request issued yet):
    %
    { MaybeLEECCallerState, RenewPeriodSecs, LEECStartOpts, BridgeSpec } =
        init_leec( BinFQDN, CertMode, ChalType, MaybeDNSProvider,
            MaybeBinCredentialsDir, BinCertDir, MaybeBinAgentKeyPath,
            SrvState ),

    % Registration to scheduler to happen in next (first) renewCertificate/1
    % call.

    ReadyState = setAttributes( SrvState, [
        { fqdn, BinFQDN },
        { cert_mode, CertMode },

        % Will be better checked by LEEC:
        { challenge_type, type_utils:check_atom( ChalType ) },
        { dns_provider, type_utils:check_atom( MaybeDNSProvider ) },
        { credentials_dir, MaybeBinCredentialsDir },
        { cert_dir, BinCertDir },
        { cert_path, undefined },
        { sans, BinSans },
        { private_key_path, MaybeBinAgentKeyPath },
        { cert_renewal_period, RenewPeriodSecs },
        { renew_listener, undefined },
        { leec_caller_state, MaybeLEECCallerState },
        { leec_start_opts, LEECStartOpts },
        { bridge_spec, BridgeSpec },
        { task_id, undefined } ] ),

    ?send_info_fmt( ReadyState, "Constructed: ~ts.",
                    [ to_string( ReadyState ) ] ),

    % Would be too early, as the HTTP webserver, which is needed to validate the
    % ACME challenges, is not launched yet:
    %
    % self() ! renewCertificate,

    ReadyState.



-doc "Initialises our LEEC private instance.".
-spec init_leec( bin_fqdn(), cert_mode(), challenge_type(),
        option( dns_provider() ), option( bin_directory_path() ),
        bin_directory_path(), option( bin_file_path() ), wooper:state() ) ->
    { option( leec_caller_state() ), seconds(), leec_start_options(),
      bridge_spec() }.
init_leec( BinFQDN, CertMode, ChalType, MaybeDNSProvider,
           MaybeBinCredentialsDir, BinCertDir, MaybeBinAgentKeyPath, State ) ->

    file_utils:is_existing_directory( BinCertDir ) orelse
        throw( { non_existing_certificate_directory,
                 text_utils:binary_to_string( BinCertDir ) } ),

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
        + round( JitterMaxSecs * random_utils:get_uniform_value() ),

    % Let's start LEEC now:

    % Refer to [https://leec.esperide.org/#usage-example].

    % (BinCertDir must be writable by this process)

    % If no agent_key_file_path is specified, a suitable agent key will be
    % generated.
    %

    % 8 minutes:
    HttpQueryTimeoutMs = 8 * 60 * 1000,

    StartBaseOpts = [ { agent_key_file_path, MaybeBinAgentKeyPath },
                      { cert_dir_path, BinCertDir },
                      { http_timeout, HttpQueryTimeoutMs } ],

    ACMEEnv = case CertMode of

        development ->
            % For testing only, then based on fake certificates:
            staging;

        production ->
            production

    end,

    ChalOpts = case ChalType of

        'http-01' ->
            % Slave mode, as we control the webserver for the challenge:
            % (no TCP port to be specified on slave mode)
            %
            [ { interfacing_mode, slave } ];

        'dns-01' ->
            MaybeDNSProvider =/= undefined orelse
                throw( { dns_provider_not_set, ChalType } ),

            MaybeBinCredentialsDir =/= undefined orelse
                throw( { credentials_directory_not_set, ChalType } ),

            [ { dns_provider, MaybeDNSProvider },
              { cred_dir_path, MaybeBinCredentialsDir } ]

    end,

    StartOpts = [ { environment, ACMEEnv } | ChalOpts ] ++ StartBaseOpts,

    % Enabling the integration of its traces:

    % Otherwise opens subcategories in traces, as an emitter:
    DotlessFQDN = text_utils:substitute( $., $:, BinFQDN ),
    TraceEmitterName = text_utils:format( "LEEC for ~ts", [ DotlessFQDN ] ),

    BridgeSpec = trace_bridge:get_bridge_spec( TraceEmitterName,
        ?trace_emitter_categorization, ?getAttr(trace_aggregator_pid) ),

    % We used to create once for all a LEEC FSM, yet several months may elapse
    % between two renewals, and keeping around a LEEC instance and its
    % connection cache has no interest; so now we spawn such an instance for
    % each need (see request_certificate/1):

    % Would be a LCS now:
    %MaybeLEECFsmPid = try leec:start( StartOpts, BridgeSpec ) of
    %
    %   { ok, FsmPid } ->
    %       ?debug_fmt( "LEEC initialized, using FSM of PID ~w, based on "
    %           "following start options:~n  ~p", [ FsmPid, StartOpts ] ),
    %       FsmPid;
    %
    %   { error, Reason } ->
    %       ?error_fmt( "Initialization of LEEC failed: ~p.~n"
    %           "Start options were:~n  ~p", [ Reason, StartOpts ] ),
    %       throw( { leec_initialization_failed, Reason } )
    %
    %
    %       catch AnyClass:Exception ->
    %           ?error_fmt( "Starting failed, with a thrown exception ~p "
    %                       "(of class: ~p).", [ Exception, AnyClass ] ),
    %           throw( { leec_initialization_failed, Exception, AnyClass } )
    %
    %end,

    MaybeLEECCallerState = undefined,

    { MaybeLEECCallerState, RenewPeriodSecs, StartOpts, BridgeSpec }.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    % Unregistering only from any scheduler, not from any task ring, as we
    % registered to any former and have been registered to any latter.

    MaybeCertTaskId = case ?getAttr(task_id) of

        undefined ->
            undefined;

        CertTaskId ->
            ?debug( "Being destructed, unregistering from scheduler." ),

            % Any extra schedule trigger sent will be lost; not a problem, as it
            % is a oneway:
            %
            class_USScheduler:get_server_pid() !
                { unregisterTask, [ CertTaskId ], self() },

            CertTaskId

    end,

    case ?getAttr(leec_caller_state) of

        undefined ->
            ok;

        LEECCallerState ->
            LCSStr = leec:caller_state_to_string( LEECCallerState ),

            ?notice_fmt( "Shutting down LEEC (~ts).", [ LCSStr ] ),
            try

                leec:terminate( LEECCallerState )

            catch AnyClass:Exception ->
                ?error_fmt( "Final termination failed, with a thrown "
                    "exception ~p (of class: ~p); ~ts.",
                    [ Exception, AnyClass, LCSStr ] )

            end,

            ?notice_fmt( "~ts shut down.", [ LCSStr ] )

    end,

    % End of interleaving:
    case MaybeCertTaskId of

        undefined ->
            ok;

        ThisCertTaskId ->
            receive

                { wooper_result, { task_unregistered, ThisCertTaskId } } ->
                    ok;

                % Would be surprising:
                { wooper_result, { task_already_done, ThisCertTaskId } } ->
                    ok;

                { wooper_result,
                        { task_unregistration_failed, Reason,
                          ThisCertTaskId } } ->
                    ?error_fmt( "Unregistration of task #~B failed "
                            "at deletion: ~p.", [ ThisCertTaskId, Reason ] )

            end

    end,

    ?info( "Deleted." ),

    State.




% Method section.


-doc "Renews asynchronously the certificate for the managed hostname.".
-spec renewCertificate( wooper:state() ) -> oneway_return().
renewCertificate( State ) ->

    ?debug( "Requested to renew certificate asynchronously." ),

    ReqState = request_certificate( State ),

    % onCertificateRequestOutcome/2 callback to be triggered soon.

    wooper:return_state( ReqState ).



-doc """
Renews the certificate for the managed hostname on a synchronisable manner.

The specified listener is typically the US-Web configuration server, so that
HTTPS support can be triggered only when certificates are ready and/or to ensure
that no two certificate requests overlap (to avoid hitting a rate limit
regarding ACME servers or having concurrent certbot instances).
""".
-spec renewCertificateSynchronisable( wooper:state(), pid() ) ->
                                                oneway_return().
renewCertificateSynchronisable( State, ListenerPid ) ->

    % Note that these managers must not be frozen here in the waiting of a
    % onCertificateRequestOutcome message, as they have to remain responsive to
    % branch the US-Web LEEC handler to the corresponding LEEC FSM, so that the
    % challenges can be returned to the ACME server for this procedure to
    % complete.

    % So that it is visible even in production mode:
    ?notice( "Requested to renew certificate in a synchronisable manner." ),

    % Also a check:
    { ListenState, undefined } =
        swapInAttribute( State, renew_listener, ListenerPid ),

    ReqState = request_certificate( ListenState ),

    % onCertificateRequestOutcome/2 callback to be triggered soon.

    wooper:return_state( ReqState ).



% (helper)
-spec request_certificate( wooper:state() ) -> wooper:state().
request_certificate( State ) ->

    % The task_id attribute may or may not be defined.

    FQDN = ?getAttr(fqdn),

    % We used to rely on a synchronous (blocking) call (no callback was used),
    % which was a mistake (this certificate manager must remain responsive here;
    % see implementation notes); now using an asynchronous call instead.
    %
    % Another problem occurred then: certbot checks that only a single instance
    % thereof exists, resulting in all of the mostly parallel certificate
    % requests to fail except the first. Now the (US-Web) configuration server
    % triggers the certificate renewals only in turn.

    % Closure:
    Self = self(),

    Callback = fun( CertObtainedOutcome ) ->
                Self ! { onCertificateRequestOutcome, [ CertObtainedOutcome ] }
               end,

    ChallengeType = ?getAttr(challenge_type),

    MaybeDNSProvider = ?getAttr(dns_provider),

    ActualSans = case ?getAttr(sans) of

        undefined ->
            [];

        S ->
            % Only wanting to disclose SANs when necessary:
            case ChallengeType of

                'http-01' ->
                    S;

                _ ->
                    []

            end

    end,

    % So that it is visible even in production mode:
    ?notice_fmt( "Requesting a ~ts certificate for '~ts', with following SAN "
                 "information:~n  ~p.", [ ChallengeType, FQDN, ActualSans ] ),

    % We used to request a certificate directly from the initial LEEC instance,
    % yet this is not a proper solution, as months are likely elapse between
    % renewals, and at least the nonce is bound to have expired in the meantime.
    %
    % So now we shut down LEEC instances once over, and recreate them at each
    % renewal.

    % Check:
    case ?getAttr(leec_caller_state) of

        undefined ->
            ok;

        CallerState ->
            ?error_fmt( "Ignoring unexpected former ~ts.",
                        [ leec:caller_state_to_string( CallerState ) ] )

    end,

    % Creates a linked, bridged instance of the LEEC service FSM:
    case leec:start( ChallengeType, ?getAttr(leec_start_opts),
                     ?getAttr(bridge_spec) ) of

        { ok, LEECCallerState } ->

            LCSStr = leec:caller_state_to_string( LEECCallerState ),
            ?notice_fmt( "New ~ts created for '~ts'.", [ LCSStr, FQDN ] ),

            try

                % Should result in onCertificateRequestOutcome/2 being called:
                async = leec:obtain_certificate_for( FQDN, LEECCallerState,
                    % An 'email' entry could be added:
                    _CertReqOptionMap=#{ async => true,
                                         callback => Callback,
                                         sans => ActualSans,
                                         dns_provider => MaybeDNSProvider } ),
                ?notice( "Certificate creation request initiated." ),
                setAttribute( State, leec_caller_state, LEECCallerState )

            catch AnyClass:Exception ->
                ?error_fmt( "Certificate request failed, with a thrown "
                    "exception ~p (of class: ~p); ~ts.",
                    [ Exception, AnyClass, LCSStr ] ),
                State

            end;

        Error ->
            ?critical_fmt( "Error when (re)starting LEEC: ~p, "
                           "no certificate recreation triggered.", [ Error ] ),
            State

    end.



-doc """
Oneway called back whenever the outcome of a certificate request is known, in
response to `request_certificate/1`.
""".
-spec onCertificateRequestOutcome( wooper:state(),
            leec:obtained_outcome() ) -> oneway_return().
% Success case:
onCertificateRequestOutcome( State,
        _CertObtainedOutcome={ certificate_generation_success, BinCertFilePath,
                               BinPrivKeyFilePath } ) ->

    FQDN = ?getAttr(fqdn),

    ?notice_fmt( "Certificate generation success for '~ts', "
        "certificate stored in '~ts' (private key in '~ts').",
        [ FQDN, BinCertFilePath, BinPrivKeyFilePath ] ),

    % Useful, as otherwise startup is so long that the user may believe it is
    % failing:
    %
    trace_utils:notice_fmt( "Certificate generation success for domain '~ts'.",
                            [ FQDN ] ),

    % The US-Web server expects the certificate and its private key directly in
    % cert_dir and as foobar.org.{crt,key}, whereas certbot put them in
    % leec-certbot-internal-state/live/foobar.org/{fullchain,privkey}.pem, so we
    % symlink them:

    BinCertDir = ?getAttr(cert_dir),
    FQDN = ?getAttr(fqdn),

    % First the certificate:
    ExpectedCertLinkName = text_utils:format( "~ts.crt", [ FQDN ] ),
    NewCertLinkPath = file_utils:bin_join( BinCertDir, ExpectedCertLinkName ),

    file_utils:remove_file_or_link_if_existing( NewCertLinkPath ),
    file_utils:create_link( _TargetPath=BinCertFilePath, NewCertLinkPath ),


    % Then its associated private key:
    ExpectedPrivKeyLinkName = text_utils:format( "~ts.key", [ FQDN ] ),
    NewPrivKeyLinkPath =
        file_utils:bin_join( BinCertDir, ExpectedPrivKeyLinkName ),

    file_utils:remove_file_or_link_if_existing( NewPrivKeyLinkPath ),
    file_utils:create_link( BinPrivKeyFilePath, NewPrivKeyLinkPath ),

    SetState = case ?getAttr(renew_listener) of

        undefined ->
            State;

        ListenerPid ->
            % Most probably the US-Web server, either waiting for a
            % acknowledgement atom in renewCertificates/1 in the case of the
            % initial certificate creations, or to be interpreted as an actual
            % oneway call for the later, next renewals:
            %
            ListenerPid ! { onCertificateRenewalOver, self() },
            setAttribute( State, renew_listener, undefined )

    end,

    NewState = manage_renewal( _RenewDelay=?getAttr(cert_renewal_period),
                               BinCertFilePath, SetState ),

    wooper:return_state( NewState );


% Failure case:
onCertificateRequestOutcome( State,
        _CertObtainedOutcome={ certificate_generation_failure, ErrorTerm } ) ->

    FQDN = ?getAttr(fqdn),

    ?error_fmt( "Certificate generation failed for '~ts': ~p.",
                [ FQDN, ErrorTerm ] ),

    % Reasonable offset for next attempt:
    FailureRenewDelay = case ?getAttr(cert_mode) of

        development ->
            time_utils:dhms_to_seconds(
                ?dhms_cert_renewal_delay_after_failure_development );

        production ->
            time_utils:dhms_to_seconds(
                ?dhms_cert_renewal_delay_after_failure_production )

    end,

    MaybeBinCertFilePath = undefined,

    NewState = manage_renewal( FailureRenewDelay, MaybeBinCertFilePath, State ),

    wooper:return_state( NewState );

onCertificateRequestOutcome( State, _CertCreationOutcome=UnexpectedError ) ->

    ?error_fmt( "Unexpected certificate creation error: ~p.",
                [ UnexpectedError ] ),

    throw( { unexpected_cert_creation_error, UnexpectedError } ).



% Manages the next renewal, either after a success (hence quite later, when the
% obtained certificates will be not far from expiring) or after a failure (hence
% very soon, but not too much to let any cause of problem vanish by itself while
% reducing the risk of hitting rate limits).
%
% (helper)
-spec manage_renewal( option( seconds() ), option( bin_file_path() ),
                      wooper:state() ) -> wooper:state().
manage_renewal( MaybeRenewDelay, MaybeBinCertFilePath, State ) ->

    %?notice_fmt( "Entering manage_renewal/1 (MaybeRenewDelay=~p)",
    %             [ MaybeRenewDelay ] ),

    % In all cases we shut down our LEEC instance, as it cannot linger between
    % longer renewals:
    %
    ShutState = case ?getAttr(leec_caller_state) of

        undefined ->
            % Surprising:
            ?error( "No LEEC PID was available when managing a possible "
                    "renewal." ),
            State;

        LEECCallerState ->
            % Not just stopping it:
            try

                leec:terminate( LEECCallerState )

            catch AnyClass:Exception ->
                ?error_fmt( "Termination after renewal failed, "
                    "with a thrown exception ~p (of class: ~p); ~ts",
                    [ Exception, AnyClass,
                      leec:caller_state_to_string( LEECCallerState ) ] )

            end,

            % No more reuse between renewals:
            setAttribute( State, leec_caller_state, undefined )

    end,

    % Switching to notice to remain available in production mode:
    %?notice( "Continuing in manage_renewal/1" ),

    RenewState = case MaybeRenewDelay of

        undefined ->
            ?notice( "No certificate renewal will be attempted "
                     "(no periodicity defined)." ),
            ShutState;

        RenewDelay ->
            %?notice( "Preparing task"),

            % A bit of interleaving:

            RenewCmd = renewCertificate,

            class_USScheduler:get_server_pid() ! { registerOneshotTask,
                [ RenewCmd, _Delay=RenewDelay, _ActPid=self() ], self() },

            NextTimestamp = time_utils:offset_timestamp(
                time_utils:get_timestamp(), RenewDelay ),

            ?notice_fmt( "Next attempt of certificate renewal to "
                "take place in ~ts, i.e. at ~ts.",
                [ time_utils:duration_to_string( 1000 * RenewDelay ),
                  time_utils:timestamp_to_string( NextTimestamp ) ] ),

            receive

                { wooper_result, { task_registered, TaskId } } ->
                    setAttribute( ShutState, task_id, TaskId );

                % Quite unlikely, yet possible:
                { wooper_result, task_done } ->
                    ShutState

            end

    end,

    setAttribute( RenewState, cert_path, MaybeBinCertFilePath ).



-doc """
Requests this manager to return (indirectly, through the current LEEC FSM) the
current thumbprint challenges to the specified target process.

Typically called from a web handler (see `us_web_leec_handler`, specifying its
PID as target one) whenever the ACME well-known URL is read by an ACME server,
to check that the returned challenges match the expected ones.
""".
-spec getChallenge( wooper:state(), pid() ) -> const_oneway_return().
getChallenge( State, TargetPid ) ->

    % Opaque:
    LCS = ?getAttr(leec_caller_state),

    LCSStr = leec:maybe_caller_state_to_string( LCS ),

    ?notice_fmt( "Requested to return the current thumbprint challenges "
        "with ~ts, on behalf of (and to) ~w.", [ LCSStr, TargetPid ] ),

    case LCS of

        undefined ->
            ?warning_fmt( "Thumbprint challenges requested to be sent "
                "(to process ~w), yet LEEC FSM seems to exist currently "
                "(no LEEC caller state). This should be investigated.",
                [ TargetPid ] );

        _ ->
            try

                leec:send_ongoing_challenges( LCS, TargetPid )

            catch AnyClass:Exception:StackTrace ->

                % Then probably that the target process will never receive these
                % challenges, so it should time-out:

                ?error_fmt( "Sending of challenges failed, with a thrown "
                    "exception ~p (of class: ~p; stacktrace: ~ts; ~ts).",
                    [ Exception, AnyClass,
                      code_utils:interpret_stacktrace( StackTrace ), LCSStr ] )

            end

    end,

    wooper:const_return().



-doc """
Callback triggered, as we trap exits, whenever a linked process stops (typically
should the LEEC FSM crash).
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
                            basic_utils:exit_reason() ) -> oneway_return().
onWOOPERExitReceived( State, _StopPid, _ExitType=normal ) ->

    % Executables triggering useless messages:
    %?notice_fmt( "Ignoring normal exit from process ~w.", [ StopPid ] ),

    wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

    % Typically: "Received exit message '{{nocatch,
    %   {wooper_oneway_failed,<0.44.0>,class_XXX,
    %       FunName,Arity,Args,AtomCause}}, [...]}"

    % We would like to restart LEEC iff the crashed process is its FSM, yet we
    % do not know its PID, so:
    %
    % (potentially linked also to the scheduler)
    NonFSMMaybePids = [ ?getAttr(renew_listener) ],

    case lists:member( CrashPid, NonFSMMaybePids ) of

        true ->
            ?warning_fmt( "Received an exit message of type '~p' from "
                "non-FSM process ~w; ignoring it.", [ ExitType, CrashPid ] ),

            wooper:const_return();

        false ->

            FQDN = ?getAttr(fqdn),

            % No need to overwhelm the ACME server in case of permacrash:
            %WaitDurationMs = 15000,
            % 1 hour:
            WaitDurationMs = 1*3600*1000,

            ?error_fmt( "Received an exit message '~p' from ~w, "
                "presumably from the LEEC FSM for FQDN '~ts'; "
                "starting a new LEEC instance after a delay of ~ts.",
                [ ExitType, CrashPid, FQDN,
                  time_utils:duration_to_string( WaitDurationMs ) ] ),

            timer:sleep( WaitDurationMs ),

            { MaybeLEECCallerState, _RenewPeriodSecs, StartOpts, _BridgeSpec } =
                init_leec( FQDN, ?getAttr(cert_mode), ?getAttr(challenge_type),
                    ?getAttr(dns_provider), ?getAttr(credentials_dir),
                    ?getAttr(cert_dir), ?getAttr(private_key_path), State ),

            ?notice_fmt( "New LEEC instance started for '~ts', "
                "with ~ts (start options: ~p); requesting a new certificate.",
                [ FQDN,
                  leec:maybe_caller_state_to_string( MaybeLEECCallerState ),
                  StartOpts ] ),

            % Then immediately retries:
            self() ! renewCertificate,

            RestartState =
                setAttribute( State, leec_caller_state, MaybeLEECCallerState ),

            wooper:return_state( RestartState )

    end.




% Static section.


-doc """
Returns the PID of the certificate manager, if it was created as a singleton,
waiting (up to a few seconds, as all US-Web server processes are bound to be
launched mostly simultaneously) if needed.

Typically useful for the various US-Main auxiliary, thematical servers, so that
they can easily access to their configuration information.

It is better to obtain the PID of a server each time from the naming service
rather than to resolve and store its PID once for all, as, for an increased
robustness, servers may be restarted (hence any stored PID may not reference a
live process anymore).
""".
-spec get_server_pid () -> static_return( cert_manager_pid() ).
get_server_pid() ->

    CertManPid = class_USServer:resolve_server_pid(
        _RegName=?cert_manager_registration_name,
        _RegScope=?cert_manager_registration_scope ),

    wooper:return_static( CertManPid ).


-doc """
Returns the https transport options and the SNI information suitable for
https-enabled virtual hosts, that is the transport options for the domain of
interest - meaning the path to the PEM certificate for the main, default host
(e.g. foobar.org) and to its private key, together with SNI (Server Name
Indication, see <https://erlang.org/doc/man/ssl.html#type-sni_hosts>) host
information for all virtual host of all hosts (e.g. baz.foobar.org, aa.buz.net,
etc.).

See also <https://ninenines.eu/docs/en/ranch/2.0/manual/ranch_ssl/>.

Note: we rely on the user routes rather than on the domain table as we need to
determine the first hots listed as the main one.
""".
-spec get_https_transport_info( dispatch_routes(), bin_directory_path() ) ->
        static_return( https_transport_info() ).
get_https_transport_info( _, _BinCertDir=undefined ) ->
    throw( no_certificate_directory_for_sni );

get_https_transport_info( _UserRoutes=[], _BinCertDir ) ->
    throw( no_hostname_for_sni );

get_https_transport_info( UserRoutes=[ { FirstHostname, _VirtualHosts } | _T ],
                          BinCertDir ) ->

    % For https with SNI, a default host must be defined, distinct from the SNI
    % ones; by convention it is the first one found in the user-defined dispatch
    % routes (with no sub-domain/virtual host considered here, i.e. foobar.org,
    % not something.foobar.org); a plain string is required, apparently.
    %
    MainTranspOpts = get_transport_opts_for( FirstHostname, BinCertDir ),

    % Options to apply for the host that matches what the client requested with
    % Server Name Indication (going through *all* the hosts listed by the user),
    % now that virtual hosts are listed as SANs in a single host-level
    % certificate:
    %
    SNIHostInfos = list_utils:flatten_once(
        [ get_virtual_host_sni_infos( H, VH, BinCertDir )
            || { H, VH } <- UserRoutes ] ),

    % Redundant:
    %cond_utils:if_defined( us_web_debug_sni, trace_utils:debug_fmt(
    %   "SNI information: transport options for the default "
    %   "hostname are: ~p.~nVirtual host options are:~n~p",
    %   [ MainTranspOpts, SNIHostInfos ] ) ),

    wooper:return_static( { MainTranspOpts, SNIHostInfos } ).



-doc """
Returns an (ordered) list of the recommended ciphers for a webserver.

An important security setting is to force the cipher to be set based on the
server-specified order instead of the client-specified oner, hence enforcing the
(usually more properly configured) security ordering of the server
administrator.

Apparently Erlang (i.e. `cowboy:start_tls/3`, relying on `ranch_ssl:opts()`, which
corresponds roughly to `ssl:erl_cipher_suite()`) relies on cipher suites expressed
with IANA conventions, whereas sites such as SSL Labs uses OpenSSL
conventions.

For conversions, see reference table in
[https://github.com/erlang/otp/wiki/Cipher-suite-correspondence-table].
""".
-spec get_recommended_ciphers() -> static_return( [ cipher_name() ] ).
get_recommended_ciphers() ->

    % Recommended ciphers for TLS v.3 (source:
    % [https://wiki.mozilla.org/Security/Server_Side_TLS]), only modern settings
    % apply, namely, with OpenSSL conventions:
    %
    % - TLS_AES_128_GCM_SHA256
    % - TLS_AES_256_GCM_SHA384
    % - TLS_CHACHA20_POLY1305_SHA256
    %
    % We now use only ciphers designated with the OpenSSL conventions, that can
    % be translated in their Erlang counterpart with the (unofficial)
    % ssl:str_to_suite/1 function, like in:
    %
    % ssl:str_to_suite("TLS_AES_128_GCM_SHA256") =
    %  #{cipher => aes_128_gcm,key_exchange => any,mac => aead,
    %    prf => sha256}

    BaseV1dot3Ciphers = [ "TLS_AES_128_GCM_SHA256", "TLS_AES_256_GCM_SHA384",
                          "TLS_CHACHA20_POLY1305_SHA256" ],

    % A priori non-TLV 1.3 recommended ciphers (source:
    % [https://github.com/ssllabs/research/wiki/SSL-and-TLS-Deployment-Best-Practices]):
    %
    BaseOtherCiphers = [ "TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256",
                         "TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384",
                         "TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA",
                         "TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA",
                         "TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256",
                         "TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384",
                         "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256",
                         "TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384",
                         "TLS_DHE_RSA_WITH_AES_128_GCM_SHA256",
                         "TLS_DHE_RSA_WITH_AES_256_GCM_SHA384" ],

    % To increase accessibility/client support:
    WeakerCiphers = [ "TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA",
                      "TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA",
                      "TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256",
                      "TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384",
                      "TLS_DHE_RSA_WITH_AES_128_CBC_SHA",
                      "TLS_DHE_RSA_WITH_AES_256_CBC_SHA",
                      "TLS_DHE_RSA_WITH_AES_128_CBC_SHA256",
                      "TLS_DHE_RSA_WITH_AES_256_CBC_SHA256" ],


    % No default token value to force a choice:
    AllCiphers = cond_utils:switch_set_to( us_web_security,
        [ { relaxed, BaseV1dot3Ciphers ++ BaseOtherCiphers ++ WeakerCiphers },
          { strict, begin
                        basic_utils:ignore_unused( WeakerCiphers ),
                        BaseV1dot3Ciphers ++ BaseOtherCiphers
                    end } ] ),

    ErlCiphers = lists:foldl(
        fun( C, Acc ) ->
            case ssl:str_to_suite( C ) of

                { error, { not_recognized, _C } } ->
                    Acc;

                CSuite ->
                    [ CSuite | Acc ]

            end

        end,
        _Acc0=[],
        AllCiphers ),

    wooper:return_static( ErlCiphers ).



% Helper section.


-doc "Returns SNI information regarding specified virtual host.".
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

% See next clause (may result in having that hostname referenced twice):
get_vh_sni_infos_for( Hostname,
        _VHostInfos=[ { _VH=without_vhost, _ContentRoot } | T ],
        BinCertDir, Acc ) ->
    HPair = { Hostname, get_transport_opts_for( Hostname, BinCertDir ) },
    get_vh_sni_infos_for( Hostname, T, BinCertDir, [ HPair | Acc ] );

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



-doc """
Returns transport options suitable for specified FQDN.

Their existing is not tested, as they may be expected to be generated later.
""".
-spec get_transport_opts_for( net_utils:string_fqdn(), bin_directory_path() ) ->
                                static_return( [ ssl_option() ] ).
get_transport_opts_for( FQDN, BinCertDir ) ->

    CertFilename = FQDN ++ ?cert_extension,
    CertFilePath = file_utils:join( BinCertDir, CertFilename ),

    PrivKeyFilename = FQDN ++ ?priv_key_extension,
    PrivKeyFilePath = file_utils:join( BinCertDir, PrivKeyFilename ),

    wooper:return_static(
        [ { certfile, CertFilePath }, { keyfile, PrivKeyFilePath } ] ).



-doc """
Checks the specified HTTPS transport options regarding the `certfile` and
`keyfile` entries; throws an exception if they are not found or not currently
applicable (no associated file found).

SNI information not checked, as usually deriving from HTTPS transport options.
""".
-spec check_https_transport_options( https_transport_info() ) ->
                                                static_void_return().
check_https_transport_options( _MaybeHttpsTranspInfo=undefined ) ->
    wooper:return_static_void();

check_https_transport_options(
        _HttpsTranspInfo={ HttpTranspOpts, _SniInfo } ) ->

    ReqKeysWithDefs = [ { certfile, undefined }, { keyfile, undefined } ],

    { [ MaybeCertFile, MaybeKeyFile ], OtherOpts } =
        list_table:extract_entries_with_defaults( ReqKeysWithDefs,
                                                  HttpTranspOpts ),

    case MaybeCertFile of

        undefined ->
            throw( { no_certfile, HttpTranspOpts } );

        CertFile ->
            file_utils:is_existing_file_or_link( CertFile ) orelse
                throw( { non_existing_certfile, CertFile } )

    end,

    case MaybeKeyFile of

        undefined ->
            throw( { no_keyfile, HttpTranspOpts } );

        KeyFile ->
            file_utils:is_existing_file_or_link( KeyFile ) orelse
                throw( { non_existing_keyfile, KeyFile } )

    end,

    OtherOpts =:= [] orelse
        throw( { unexpected_extra_https_transport_options, OtherOpts } ).



-doc "Returns a textual description of this manager.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    PeriodStr = case ?getAttr(cert_renewal_period) of

        undefined ->
            "no periodic renewal";

        PeriodSecs ->
            text_utils:format( "a renewal every ~ts",
                [ time_utils:duration_to_string( 1000 * PeriodSecs ) ] )

    end,

    FSMStr = leec:maybe_caller_state_to_string( ?getAttr(leec_caller_state) ),

    CertStr = case ?getAttr(cert_path) of

        undefined ->
            "not having generated a certificate yet";

        CertPath ->
            text_utils:format( "having generated a certificate in '~ts'",
                               [ CertPath ] )

    end,

    SansStr = case ?getAttr(sans) of

        [] ->
            "no Subject Alternative Names";

        Sans ->
            text_utils:format( "~B Subject Alternative Names: ~ts",
                [ length( Sans ),
                  text_utils:binaries_to_listed_string( Sans ) ] )

    end,

    text_utils:format( "US certificate manager for '~ts' in ~ts mode, "
        "using certificate directory '~ts', with ~ts, using ~ts, ~ts; "
        "it is to specify ~ts",
        [ ?getAttr(fqdn), ?getAttr(cert_mode), ?getAttr(cert_dir), PeriodStr,
          FSMStr, CertStr, SansStr ] ).
