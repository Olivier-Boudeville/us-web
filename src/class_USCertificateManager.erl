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
		 "certificates." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).



% Managing X.509 certificates on behalf of the US framework, for a given need
% (typically a specific FQDN).
%
% This includes:
% - requesting and obtaining such certificates, typically for such a FQDN
% - renewing them on time
%
% Certificates are obtained thanks to Let's Encrypt.
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
% of course shall be writable by this Erlang VM). A 'mydomain.tld.csr' is also
% created.
%
% In addition to TCP port 80 for http, the TCP port 443 must be opened for
% https.


-type manager_pid() :: class_UniversalServer:server_pid().

-export_type([ manager_pid/0 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type bin_fqdn() :: net_utils:bin_fqdn().

-type bin_directory_path() :: file_utils:bin_directory_path().
%-type bin_file_name() :: file_utils:bin_file_name().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().


-type cert_mode() :: class_USWebConfigServer:cert_mode().


% The class-specific attributes:
-define( class_attributes, [

	{ fqdn, bin_fqdn(), "the FQDN for which a certificate is to be managed" },

	{ cert_mode, cert_mode(), "tells whether certificate generation is in "
	  "testing/staging mode or not" },

	{ cert_dir, bin_directory_path(),
	  "the directory where certificates shall be written" },

	{ cert_renewal_period, maybe( unit_utils:seconds() ),
	  "the base delay between two successful certificate renewals (if any)" },

	{ scheduler_pid, maybe( scheduler_pid() ),
	  "the PID of any scheduler used by this manager" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization,
		 "US.Configuration.USCertificateManager" ).


-define( registration_name, us_web_certificate_manager ).

-define( registration_scope, global_only ).



% The default DHMS periodicity at which the X.509 certificate renewal will be
% requested by a given manager instance for the FQDN it takes care of.
%
% Let’s Encrypt certificate lifetime is 90 days (cf. duration in
% https://letsencrypt.org/docs/faq/), we trigger a renewal with some margin:

% Every 4 minutes (for development):
-define( dhms_cert_renewal_period_development, { 0, 0, 4, 0 } ).

% Every 75 days (minimum being 60 days, as lasting for 90 days and only renewed
% the last 30 days), for production:
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
% certificate information, and any specified scheduler for automatic certificate
% renewal.
%
-spec construct( wooper:state(), bin_fqdn(), bin_directory_path(),
				 maybe( scheduler_pid() ) ) -> wooper:state().
construct( State, BinFQDN, BinCertDir, MaybeSchedulerPid ) ->
	construct( State, BinFQDN, _CertMode=production, BinCertDir,
			   MaybeSchedulerPid, _IsSingleton=false ).



% Constructs a US certificate manager for the specified FQDN (host in specified
% domain), in specified mode, using specified directory to write certificate
% information, and any specified scheduler.
%
-spec construct( wooper:state(), bin_fqdn(), cert_mode(), bin_directory_path(),
				 maybe( scheduler_pid() ), boolean() ) -> wooper:state().
construct( State, BinFQDN, CertMode, BinCertDir, MaybeSchedulerPid,
		   _IsSingleton=true ) ->

	% Relies first on the next, main constructor:
	InitState = construct( State, BinFQDN, CertMode, BinCertDir,
						   MaybeSchedulerPid, _Sing=false ),

	% Then self-registering:
	RegName = ?registration_name,
	RegScope = ?registration_scope,

	naming_utils:register_as( RegName, RegScope ),

	setAttributes( InitState, [ { registration_name, RegName },
								{ registration_scope, RegScope } ] );


construct( State, BinFQDN, CertMode, BinCertDir, MaybeSchedulerPid,
		   _IsSingleton=false ) ->

	% Rather than relying on a periodic scheduling, each renewal will just plan
	% the next one, to better account for any failed attemps / delay introduced:
	% (and certificates shall be created directly when starting up:
	%
	self() ! requestCertificate,

	% As periodic renewal may be disabled:
	MaybeRenewPeriodSecs = case MaybeSchedulerPid of

		undefined ->
			undefined;

		_ ->
			{ BaseRenewPeriodDHMS, JitterMaxDHMS } = case CertMode of

				% Fake certificates in staging:
				development ->
					{ ?dhms_cert_renewal_period_development,
					  ?max_dhms_cert_renewal_jitter_development };

				% Renewal must be within ]60,90[ days:
				production ->
					{ ?dhms_cert_renewal_period_production,
					  ?max_dhms_cert_renewal_jitter_production }

			end,

			BaseRenewPeriodSecs =
				time_utils:dhms_to_seconds( BaseRenewPeriodDHMS ),

			JitterMaxSecs = time_utils:dhms_to_seconds( JitterMaxDHMS ),

			% Random value in [0.0, 1.0[:
			BaseRenewPeriodSecs
				+ round( JitterMaxSecs * random_utils:get_random_value() )

	end,

	ServerName =
		text_utils:format( "Certificate manager for ~s", [ BinFQDN ] ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State,
										   ?trace_categorize(ServerName) ),

	case file_utils:is_existing_directory( BinCertDir ) of

		true ->
			ok;

		false ->
			throw( { non_existing_certificate_directory, BinCertDir } )

	end,

	ReadyState = setAttributes( TraceState, [
		{ fqdn, BinFQDN },
		{ cert_dir, BinCertDir },
		{ cert_renewal_period, MaybeRenewPeriodSecs },
		{ scheduler_pid, MaybeSchedulerPid } ] ),

	?send_info( ReadyState, "Just created: " ++ to_string( ReadyState ) ),

	ReadyState.



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

	?trace( "Shutting down Let's Encrypt." ),
	letsencrypt:stop(),

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
-spec requestCertificate( wooper:state() ) -> const_oneway_return().
requestCertificate( State ) ->

	FQDN = ?getAttr(fqdn),

	?trace_fmt( "Requesting certificate for '~s'.", [ FQDN ] ),

	% Synchronous (blocking) call (no callback used); this clause also returns
	% (after some delay, like 30 seconds) the delay (with some jitter) until the
	% next certificate renewal (quickly if having just failed, normally if
	% having succeeded):
	%
	MaybeDHMSRenewDelay =
			case letsencrypt:make_cert( FQDN, #{ async => false } ) of

		{ ok, #{ cert := BinCertPath, key := BinCertKey } } ->

			?trace_fmt( "Certificate generation success for '~s': "
				"certificate file is '~s' and certificate key is '~s'.",
				[ FQDN, BinCertPath, BinCertKey ] ),

			?getAttr(cert_renewal_period);


		{ error, Reason } ->

			?error_fmt( "Certificate generation failed for '~s': ~p.",
						[ Reason ] ),

			% Reasonable offset for next attempt:
			case ?getAttr(cert_mode) of

				development ->
					?dhms_cert_renewal_delay_after_failure_development;

				production ->
					?dhms_cert_renewal_delay_after_failure_production

			end

	end,

	case ?getAttr(scheduler_pid) of

		undefined ->
			?info( "No certificate renewal will be attempted "
				   "(no scheduler registered)." );

		SchedPid ->
			case MaybeDHMSRenewDelay of

				undefined ->
					?info( "No certificate renewal will be attempted "
						   "(no periodicity defined)." );

				DHMSRenewDelay ->

					NextTimestamp = time_utils:offset_timestamp(
						time_utils:get_timestamp(), DHMSRenewDelay ),

					?debug_fmt( "Next attempt of certificate renewal to "
					  "take place in ~s, i.e. at ~s.",
					  [ time_utils:dhms_to_string( DHMSRenewDelay ),
						time_utils:timestamp_to_string( NextTimestamp ) ] ),

					SchedPid ! { registerOneshotTask, [ _Cmd=requestCertificate,
							_Delay=DHMSRenewDelay, _ActPid=self() ] }

			end

	end,

	wooper:const_return().


% Static section.



% Helper section.



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

	text_utils:format( "US certificate manager for '~s', using "
		"certificate directory '~s', with ~s",
		[ ?getAttr(fqdn), ?getAttr(cert_dir), PeriodStr ] ).
