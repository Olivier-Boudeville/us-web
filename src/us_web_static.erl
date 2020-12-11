%% Copyright (c) 2019-2020, Olivier Boudeville <olivier.boudeville@esperide.com>
%% Copyright (c) 2013-2017, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2011, Magnus Klaar <magnus.klaar@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.



% This is a US-Web custom, derived version of cowboy_static.erl, modified
% (mostly stripped-down) in order to comply with the conventions of Universal
% Server and to support 404 errors and logging.
%
% Table-based states could have been used, this could have even been a WOOPER
% instance per request, however we went for the least overhead and for direct
% map management.
%
% As done by Cowboy, exactly one process is spawned per request.
%
-module(us_web_static).

-export([ init/2, malformed_request/2, forbidden/2, content_types_provided/2,
		  charsets_provided/2, ranges_provided/2, resource_exists/2,
		  last_modified/2, generate_etag/2, get_file/2 ]).


-type extra_charset() :: { charset, module(), function() }
					   | { charset, binary() }.

-type extra_etag() :: { etag, module(), function() } | { etag, false }.

-type extra_mimetypes() :: { mimetypes, module(), function() }
 | { mimetypes, binary() | { binary(), binary(), [ { binary(), binary() } ] } }.

-type extra() :: [ extra_charset() | extra_etag() | extra_mimetypes() ].

-type opts() :: { file | dir, string() | binary() }
	| { file | dir, string() | binary(), extra() }
	| { priv_file | priv_dir, atom(), string() | binary() }
	| { priv_file | priv_dir, atom(), string() | binary(), extra() }.

-export_type([ opts/0 ]).

-include_lib("kernel/include/file.hrl").


-type path_info()  :: [ text_utils:bin_string() ].


% Shorthands:

-type file_info() :: file_utils:file_info().
-type bin_content_path() :: us_web_handler:bin_content_path().

-type handler_state() :: us_web_handler:handler_state().
-type handler_return() :: us_web_handler:handler_return().


-type access_type() :: 'direct' | 'archive'.


% State for the cowboy_rest handler:
-type rest_handler_state() :: { bin_content_path(),
		{ access_type(), file_info() } | basic_utils:error_term(), extra() }.


% See our web_utils module:
-define( internal_server_error, 500 ).



% Resolves the file that will be sent, and gets its file information.
%
% If the handler is configured to manage a directory, checks that the requested
% file is inside the configured directory.
%
-spec init( cowboy_req:req(), handler_state() ) -> handler_return().
% Apparently pattern-matching maps directly in heads is not supported (R22:
% "illegal map key in pattern"), so:
%
%init( Req, HState=#{ type := Type } ) ->
init( Req, HState ) ->

	BinPath = maps:get( path, HState ),

	cond_utils:if_defined( us_web_debug_handlers,
		class_TraceEmitter:register_as_bridge(
		  _Name=text_utils:format( "Static handler for path ~s",
								   [ BinPath ] ),
		  _Categ="US.Web.StaticHandler" ) ),

	%trace_utils:debug_fmt(
	%  "[~s:~w] Serving request as a ~s~nInitial handler state being ~s",
	%  [ ?MODULE, self(), table:to_string( Req ), table:to_string( HState ) ] ),

	CowboyOpts = maps:get( cowboy_opts, HState ),

	HReturn = case maps:get( _Key=type, HState ) of

		file ->
			init_info( Req, _BinIndex=BinPath, CowboyOpts, HState );

		directory ->
			init_dir( Req, _BinContentRoot=BinPath, CowboyOpts, HState )

	end,

	% Logging already done in the init_* functions above.

	%trace_utils:debug_fmt( "[~s:~w] Returning:~n~p",
	%                       [ ?MODULE, self(), HReturn ] ),

	HReturn.



% Handles the request for a file.
init_info( Req, BinFullFilePath, CowboyOpts, HState ) ->

	%trace_utils:debug_fmt( "init_info for file '~s' (opts: ~p)",
	%                     [ BinFullFilePath, CowboyOpts ] ),

	case file:read_file_info( BinFullFilePath, [ { time, universal } ] ) of

		{ ok, FileInfo } ->

			RestHandlerState = { BinFullFilePath, { direct, FileInfo },
								  CowboyOpts },

			HReturn = { cowboy_rest, Req, RestHandlerState },

			% We now take advantage of this process to perform logging:
			us_web_handler:manage_access_log( HReturn,
				_HttpSuccessStatusCode=200, HState ),

			HReturn;

		{ error, enoent } ->
			% Performs logging as well:
			us_web_handler:return_404( Req, BinFullFilePath, HState );

		{ error, Error } ->
			us_web_handler:manage_error_log( Error, Req, BinFullFilePath,
											 HState ),
			% error

			% Supposedly better (no ranch crash report); wget reports "ERROR
			% 500: Internal Server Error." as intented, yet a browser just
			% displays a blank page; a nice "Internal server error" page could
			% be generated and returned instead?
			%
			{ ok, cowboy_req:reply( ?internal_server_error, Req ), error }

	end.



% Handles the request for a directory.
init_dir( Req, BinContentRoot, CowboyOpts, HState ) ->

	%trace_utils:debug_fmt( "init_dir for directory '~s'.",
	%                      [ BinContentRoot ] ),

	case cowboy_req:path_info( Req ) of

		% When dir/priv_dir are used, and there is no path_info, this is a
		% configuration error and we abort immediately:
		%
		undefined ->
			us_web_handler:manage_error_log( _Error=no_path_info_for_dir, Req,
											 BinContentRoot, HState ),
			{ ok, cowboy_req:reply( ?internal_server_error, Req ), error };

		PathInfo ->
			case validate_reserved( PathInfo ) of

				ok ->
					BinFullFilepath =
						filename:join( [ BinContentRoot | PathInfo ] ),

					Len = byte_size( BinContentRoot ),

					case fullpath( BinFullFilepath ) of

						<< BinContentRoot:Len/binary, $/, _/binary >> ->
							%trace_utils:debug( "init_dir: case A." ),
							init_info( Req, BinFullFilepath, CowboyOpts,
									   HState );

						<< BinContentRoot:Len/binary >> ->
							%trace_utils:debug( "init_dir: case B." ),
							init_info( Req, BinFullFilepath, CowboyOpts,
									   HState );

						_ ->
							%trace_utils:debug( "init_dir: case C." ),
							us_web_handler:manage_error_log(
							  _Error={ no_full_path_for, PathInfo }, Req,
							  BinContentRoot, HState ),
							{ cowboy_rest, Req, error }

					end;


				error ->
					%trace_utils:debug( "init_dir: case D." ),
					us_web_handler:manage_error_log(
					  _Error={ invalid_path_info, PathInfo }, Req,
					  BinContentRoot, HState ),
					{ cowboy_rest, Req, error }


			end


	end.



% Validates specified path information.
-spec validate_reserved( path_info() ) -> 'ok' | 'error'.
validate_reserved( _PathInfo=[] ) ->
	ok;

validate_reserved( _PathInfo=[ Path | T ] ) ->

	case validate_reserved_helper( Path ) of

		ok ->
			validate_reserved( T );

		error ->
			error

	end.


% We always reject forward slash, backward slash and NUL as those have special
% meanings across the supported platforms.
%
% We could support the backward slash on some platforms but for the sake of
% consistency and simplicity we do not.
%
validate_reserved_helper( <<>> ) ->
	ok;

validate_reserved_helper( <<$/, _/bits>> ) ->
	error;

validate_reserved_helper( <<$\\, _/bits>> ) ->
	error;

validate_reserved_helper( <<0, _/bits>> ) ->
	error;

validate_reserved_helper( <<_, Rest/bits>> ) ->
	validate_reserved_helper( Rest ).


% Normalises specified path.
fullpath( Path ) ->
	fullpath( filename:split( Path ), _Acc=[] ).

fullpath( [], Acc ) ->
	filename:join( lists:reverse( Acc ) );

fullpath( [ <<".">> | T ], Acc ) ->
	fullpath( T, Acc );

fullpath( [ <<"..">> | T ], Acc=[ _ ] ) ->
	fullpath( T, Acc );

fullpath( [ <<"..">> | T ], [ _ | Acc ] ) ->
	fullpath( T, Acc );

fullpath( [ Segment | T ], Acc ) ->
	fullpath( T, [ Segment | Acc ] ).





% Reject requests that tried to access a file outside the target directory, or
% used reserved characters.
%
-spec malformed_request( Req, State ) -> { boolean(), Req, State }.
malformed_request( Req, State ) ->
	{ State =:= error, Req, State }.



% Directories, files that cannot be accessed at all, and files with no read flag
% are forbidden.
%
-spec forbidden( Req, State  ) -> { boolean( ), Req, State  }
	when State::rest_handler_state(  ).
forbidden( Req, State={ _, { _, #file_info{ type=directory } }, _ } ) ->
	{ true, Req, State };

forbidden( Req, State={ _, { error, eacces }, _ } ) ->
	{ true, Req, State };

forbidden( Req, State={ _, { _, #file_info{ access=Access } }, _ } )
		when Access =:= write orelse Access =:= none ->
	{ true, Req, State };

forbidden( Req, State ) ->
	{ false, Req, State }.




% Detects the mimetype of the file.
-spec content_types_provided( Req, State ) ->
							{ [ { binary(), get_file } ], Req, State }
	when State::rest_handler_state().
content_types_provided( Req, State={ Path, _, Extra } ) when is_list( Extra ) ->

	case lists:keyfind( mimetypes, 1, Extra ) of

		false ->
			{ [ { cow_mimetypes:web( Path ), get_file } ], Req, State };

		{ mimetypes, Module, Function } ->
			{ [ { Module:Function( Path ), get_file } ], Req, State };

		{ mimetypes, Type } ->
			{ [ { Type, get_file } ], Req, State }

	end.


% Detects the charset of the file.
-spec charsets_provided( Req, State ) -> { [ binary() ], Req, State }
	when State::rest_handler_state().
charsets_provided( Req, State={ Path, _, Extra } ) ->

	case lists:keyfind( charset, 1, Extra ) of

		% We simulate the callback not being exported:
		false ->
			no_call;

		{ charset, Module, Function } ->
			{ [ Module:Function( Path ) ], Req, State };

		{ charset, Charset } when is_binary( Charset ) ->
			{ [ Charset ], Req, State }

	end.


% Enables support for range requests.
-spec ranges_provided( Req, State ) -> { [ { binary(), auto } ], Req, State }
	when State::rest_handler_state().
ranges_provided( Req, State ) ->
	{ [ { <<"bytes">>, auto } ], Req, State }.


% Assumes the resource doesn't exist if it's not a regular file.
-spec resource_exists( Req, State ) -> { boolean(), Req, State }
										 when State::rest_handler_state().
resource_exists( Req, State={ _, { _, #file_info{ type=regular } }, _ } ) ->
	{ true, Req, State };

resource_exists( Req, State ) ->
	{ false, Req, State }.



% Generates an etag for the handler-referenced file.
-spec generate_etag( Req, State ) -> { { strong | weak, binary() }, Req, State }
									   when State::rest_handler_state().
generate_etag( Req, State={ Path, { _, #file_info{ size=Size, mtime=Mtime } },
		Extra } ) ->

	case lists:keyfind( etag, 1, Extra ) of

		false ->
			{ generate_default_etag( Size, Mtime ), Req, State };

		{ etag, Module, Function } ->
			{ Module:Function( Path, Size, Mtime ), Req, State };

		{ etag, false } ->
			{ undefined, Req, State }

	end.



% Generates a default etag.
generate_default_etag( Size, Mtime ) ->
	{ strong, integer_to_binary( erlang:phash2( { Size, Mtime },
												16#ffffffff ) ) }.



% Returns the time of last modification of the handler-referenced file.
-spec last_modified( cowboy_req:req(), rest_handler_state() ) ->
	   { calendar:datetime(), cowboy_req:req(), rest_handler_state() }.
last_modified( Req, HState={ _BinContenPath,
		 { _AccessType, #file_info{ mtime=ModifiedTime } }, _Extra } ) ->
	{ ModifiedTime, Req, HState }.



% Streams the handler-referenced file.
-spec get_file( cowboy_req:req(), rest_handler_state() ) ->
		  { { 'sendfile', 0, non_neg_integer(), binary() }, cowboy_req:req(),
			rest_handler_state() }.
get_file( Req, HState={ BinPath, { direct, #file_info{ size=Size } },
						_Extra } ) ->
	{ { sendfile, 0, Size, BinPath }, Req, HState };

get_file( Req, HState={ BinPath, { archive, _FileInfo}, _Extra } ) ->
	FilePath = binary_to_list( BinPath ),
	{ ok, FileBin, _ } = erl_prim_loader:get_file( FilePath ),
	{ FileBin, Req, HState }.
