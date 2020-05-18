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


% Cowboy-compliant handler for us_web:
%
% - provides static dispatches so that the websites corresponding to the virtual
% hosts can be appropriately served; see conf/us_web_vhosts.config, which holds
% the data relative to virtual hosts
%
% - handles HTTP errors (notably 404 ones, thanks to a specific web page)
%
-module(us_web_handler).


-export([ init/2, return_404/3,
		  get_header/3, get_base_footer/0,
		  manage_access_log/3, manage_error_log/4 ]).



% Module to handle web content through dispatch rules (ex: 'cowboy_rest'):
-type handler_module() :: basic_utils:module_name().


% State carried by the process in charge of a request, for most handlers:
-type handler_state() :: maps:map().


% A US-specialised web handler, a map which may notably contain entries with
% following keys:
%
% - css_path: file_utils:bin_file_path(), a path relative to the content root of
% the corresponding virtual host pointing to the default CSS file to be used
% (ex: for the 404 page)
%
% - image_404 :: file_utils:bin_file_path(), a path relative to the content root
% of the corresponding virtual host pointing to an image (ex: PNG) to be used
% when generating a 404 error page
%
-type us_handler_state() :: handler_state().



% The (binary) path for a web content:
-type bin_content_path() :: file_utils:bin_file_path().

% See https://en.wikipedia.org/wiki/List_of_HTTP_status_codes:
-type http_status_code() :: non_neg_integer().


% See https://ninenines.eu/docs/en/cowboy/2.7/manual/cowboy_handler/:
-type handler_return() :: { 'ok' | handler_module(), cowboy_req:req(),
							handler_state() | 'error' }.


-export_type([ handler_module/0, handler_state/0, us_handler_state/0,
			   bin_content_path/0, http_status_code/0, handler_return/0 ]).


-type log_line() :: class_USWebLogger:log_line().



% Returns a suitable term to notify the request initiator that a 404 error was
% triggered (meaning that the requested content could not be found).
%
% Maybe this dynamic implementation is too resource-demanding (prone to denial
% of service attack) and may be replaced with a static binary string (yet then
% copied to each per-request process), determined once for all.
%
-spec return_404( cowboy_req:req(), bin_content_path(), handler_state() ) ->
						handler_return().
% maps not fully supported in R22:
%return_404(
%  { host := VHost, port := TCPPort, path_info := NonExistingPath }=Req,
%  BinFullFilepath, #{ image_404 := MaybeImage404 }=HState ) ->
return_404( Req, BinFullFilepath, HState ) ->

	%trace_utils:debug_fmt( "Returning 404 for full path '~s' and request:~n~p",
	%					   [ BinFullFilepath, Req ] ),

	[ VHost, TCPPort, PathInfo ] =
		[ maps:get( K, Req ) || K <- [ host, port, path_info ] ],

	MaybeBinCssFile = maps:get( css_path, HState ),

	MaybeBinIconFile = maps:get( icon_path, HState ),

	MaybeImage404 = maps:get( image_404, HState ),

	PathForBrowser = case PathInfo of

		% path_info may not be set, if for example just an hostname was
		% specified with no path, "index.html" being automatically appended
		% afterwards (whereas the full file path will be set):
		%
		undefined ->
			RootPath =
				text_utils:binary_to_string( maps:get( content_root, HState ) ),

			% Not wanting to expose absolute, internal paths:
			file_utils:make_relative( BinFullFilepath, RootPath );

		_ ->
			file_utils:join( PathInfo )

	end,

	PageTitle = text_utils:format( "Page '~s' not found on ~s",
								   [ PathForBrowser, VHost ] ),

	MiddleString = case MaybeImage404 of

		undefined ->
			"<td style=\"font-size:1000%\">0</td>";

		% Typically "images/404.png":
		ImagePath ->
			% In the general case, we are currently serving content stored in a
			% subdirectory, whereas the shared content (here, to handle 404
			% errors) is defined relatively to the content root (and of course
			% we do not want to expose to the client absolute paths on the
			% server); so:
			%
			RootRelImgPath = file_utils:join(
				   [ ".." || _PathElem <- tl( PathInfo ) ], ImagePath ),

			text_utils:format( "<td><center><img src=\"~s\" border=\"0\" "
							   "width=\"50%\"></center></td>",
							   [ RootRelImgPath ] )

	end,


	PageBody = text_utils:format(
	  <<"<h1>Requested page '~s' not found</h1>
  <p>I could not find the document you wanted, sorry.</p>
  <p style=\"padding-left:10%\">       --- The ~s server.</p>
  <p>
	<center>
		<table style=\"border:none\" width=\"30%\">
		  <tr>
			<td style=\"font-size:1000%\">4</td>
			~s
			<td style=\"font-size:1000%\">4</td>
		  </tr>
		</table>
	</center>
  </p>">>,
  % Reporting currently disabled, not wanting to leak an e-mail address:
  %
  %<p>You can report this issue: <a href=\"mailto:error-404@~s?subject=Error%20404\">click here</a>. Thanks!</p>">>,
	%[ PathForBrowser, VHost, MiddleString, VHost ] ),
	[ PathForBrowser, VHost, MiddleString ] ),

	ReplyBody = [ get_header( PageTitle, MaybeBinCssFile, MaybeBinIconFile ),
				  PageBody, get_footer( VHost, TCPPort ) ],

	ReplyHeaders = get_http_headers( ReplyBody ),

	HttpStatusCode = 404,

	ReplyReq = cowboy_req:reply( HttpStatusCode, ReplyHeaders, ReplyBody, Req ),

	HandlerReturn = { ok, ReplyReq, error },

	manage_access_log( HandlerReturn, HttpStatusCode, HState ),

	HandlerReturn.



% Returns a suitable document header.
get_header( Title, MaybeBinCssFile, MaybeBinIconFile ) ->

	CSSString = case MaybeBinCssFile of

		undefined ->
			"";

		BinCssFile ->
			text_utils:format(
			  "<link rel=\"stylesheet\" type=\"text/css\" href=\"~s\">",
			  [ BinCssFile ] )

	end,

	FavIconString = case MaybeBinIconFile of

		undefined ->
			"";

		BinIconFile ->
			text_utils:format( "<link rel=\"icon\" href=\"~s\">",
							   [ BinIconFile ] )

	end,

	[ <<"<!DOCTYPE html>
<html lang=\"EN\">
 <head>
  <title>">>, Title, <<"</title>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
  <meta http-equiv=\"Content-Language\" content=\"en\">">>,
	  CSSString,
	%<meta name=\"Description\" content=\"Foobar\">
	%<meta name=\"Identifier-URL\" content=\"http://foobar.com\">"
	  FavIconString,
<<"
 </head>
 <body>
">> ].



% Returns a base HTML footer.
get_base_footer() ->
	"
 </body>
</html>
".


% Returns a suitable document footer.
get_footer( VHost, Port ) ->
	text_utils:format( <<"
	<p>
	  <center>From there you can go back to the <a href=\"javascript:history.back();\">previous page</a>, or to the <a href=\"http://~s:~B\">website root</a>.
	  </center>
	</p>
~s">>, [ VHost, Port, get_base_footer() ]).


% Returns a suitable document header.
get_http_headers( Body ) ->
	#{ <<"content-type">> => <<"text/html">>,
	   <<"content-length">> => integer_to_list( iolist_size( Body ) ),
	   % A bit of obfuscation:
	   <<"server">> => <<"Apache/2.4.1 (Unix)">> }.



% Test handler possibly called through routing.

% Typically called because a dispath rules mentioned that module as a (debug)
% handler:
%
-spec init( cowboy_req:req(), handler_state() ) -> handler_return().
init( Req, HState ) ->

	us_web_utils:debug_fmt( "Request ~p to be handled, while handler state "
							"is ~p...", [ Req, HState ] ),

	Reply = cowboy_req:reply( 200, #{ <<"content-type">> => <<"text/plain">> },
							  <<"Hello from Universal web server!">>, Req ),

	{ ok, Reply, HState }.



% Facilities.


% Access log facility offered to web handlers, taking advantage of the
% request-specific process in order to further parallelize their processing.
%
-spec manage_access_log( handler_return(), http_status_code(),
						 handler_state() ) -> void().
manage_access_log( HandlerReturn, HttpStatusCode, HState ) ->

	case maps:get( logger_pid, HState ) of

		undefined ->
			%trace_utils:debug_fmt( "[~w] Request served (code: ~B), "
			%   "no logger.", [ self(), HttpStatusCode ] );
			ok;

		LoggerPid ->

			BinLog = generate_access_log( HandlerReturn, HttpStatusCode ),

			% Oneway call:
			LoggerPid ! { reportAccess, [ BinLog ] }

			%trace_utils:debug_fmt(
			%  "[~w] Request served (code: ~B), logger ~w notified.",
			%  [ self(), HttpStatusCode, LoggerPid ] )

	end.



% Returns a binary access log line from specified request information.
-spec generate_access_log( handler_return(), http_status_code() ) -> log_line().
generate_access_log( _HandlerReturn={ _Atom, Req, _HState }, HttpStatusCode ) ->

	%trace_utils:debug_fmt( "Generating access log for following handler "
	%                       "return:~n~p", [ HandlerReturn ] ),

	% We target here the "NCSA combined with several virtualhostname sharing
	% same log file" log format, except that we replaced the clumsy '%time1'
	% (ex: including the 3 first letters of the month, possibly with a capital,
	% and a UTC offset difficult to sort out) with more straightforward, less
	% demanding '%time2': "[dd/mmm/yyyy:hh:mm:ss +0x00]" becomes for the better
	% "yyyy-mm-dd hh:mm:ss".

	% More precisely, as discussed in
	% https://awstats.sourceforge.io/docs/awstats_config.html#LogFormat:
	%
	% - %time1 Date and time with format: [dd/mon/yyyy:hh:mm:ss +0000] or
	% [dd/mon/yyyy:hh:mm:ss]
	%
	% - %time2 Date and time with format: yyyy-mm-dd hh-mm-ss

	% Log format is thus defined as: "%virtualname %host %other %logname %time2
	% %methodurl %code %bytesd %refererquot %uaquot" (this should be reflected
	% in the configuration of any web log analyzer).

	% For example: virtualserver1 62.161.78.73 - - yyyy-mm-dd hh:mm:ss
	% "GET /page.html HTTP/1.1" 200 1234 "http://www.from.com/from.htm"
	% "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)"

	% See also:
	%  - https://ninenines.eu/docs/en/cowboy/2.7/manual/cowboy_req/
	%  - https://httpd.apache.org/docs/1.3/logs.html
	%  - https://awstats.sourceforge.io/docs/awstats_faq.html#PERSONALIZEDLOG

	VirtualName = maps:get( host, Req ),

	% To be resolved later as an hostname (reverse look-up):
	{ IP, _Port } = maps:get( peer, Req ),

	IPString = io_lib:format( "~B.~B.~B.~B", tuple_to_list( IP ) ),

	% RFC 1413 identity of the client (unreliable):
	Other = "-",

	% Identifier of the user requesting the document, as determined by HTTP
	% authentication:
	%
	Login = "-",

	% A cheap time is more  than sufficient (%time2 here):
	Time = time_utils:get_time2_textual_timestamp(),

	Method = maps:get( method, Req ),
	Path = maps:get( path, Req ),
	Version = maps:get( version, Req ),

	MethodURL = text_utils:format( "~s ~s ~s", [ Method, Path, Version ] ),

	% Size of the body object returned to the client, not including the response
	% headers:
	%
	% (often zero; with some log formats - others than %B, "0" shall be
	% translated to "-"; true for %b for example)
	%
	SentSize = maps:get( body_length, Req ),

	Headers = maps:get( headers, Req ),

	% May not be defined:
	Referer = maps:get( <<"referer">>, Headers, _DefaultReferer="" ),

	% May not be defined also:
	UserAgent = maps:get( <<"user-agent">>, Headers,
						  _DefaultUserAgent= <<"(unknown user agent)">> ),

	text_utils:bin_format( "~s ~s ~s ~s ~s \"~s\" ~B ~B \"~s\" \"~s\"~n",
		[ VirtualName, IPString, Other, Login, Time, MethodURL, HttpStatusCode,
		  SentSize, Referer, UserAgent ] ).



% Error log facility offered to web handlers, taking advantage of the
% request-specific process in order to further parallelize their processing.
%
-spec manage_error_log( basic_utils:error_reason(), cowboy_req:req(),
				bin_content_path(), handler_state() ) -> void().
manage_error_log( Error, Req, BinFullFilePath, HState ) ->

	%trace_utils:debug_fmt( "Logging error for full path '~s' and "
	%					   "request:~n~p", [ BinFullFilePath, Req ] ),

	% Unlike accesses, no specific log format seems to apply here:

	Host = maps:get( host, Req, no_host ),

	BinErrorMsg = text_utils:bin_format(
		"[~s][~s] Error '~p' while requested to serve '~s'; full ~s~n",
		[ time_utils:get_textual_timestamp(), Host, Error, BinFullFilePath,
		  request_to_string( Req ) ] ),

	case maps:get( logger_pid, HState ) of

		undefined ->
			trace_utils:error_fmt( "[~w] No logger to record ~s",
								   [ self(), BinErrorMsg ] );

		LoggerPid ->

			% Oneway call:
			LoggerPid ! { reportError, [ BinErrorMsg ] }

			%trace_utils:debug_fmt( "[~w] reported ~s.",
			%					   [ self(), BinErrorMsg ] )

	end.



% Returns a textual description of specified request.
-spec request_to_string( cowboy_req:req() ) -> text_utils:string().
request_to_string( Req ) ->

	% Map in map better special cases:
	{ Headers, ShrunkReq } = table:extract_entry( headers, Req ),

	text_utils:format( "request corresponding without the headers to a ~s~n"
					   "Request headers corresponding to a ~s",
		  [ table:to_string( ShrunkReq ), table:to_string( Headers ) ] ).