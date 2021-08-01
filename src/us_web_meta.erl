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


% @doc Module to handle <b>'meta' websites</b>, that is (generated) websites
% about websites, to help their quick monitoring.
%
-module(us_web_meta).


-export([ get_page_header/0, get_page_body/6, get_page_footer/0 ]).

% To silence functions not used anymore:
-export([ get_vhost_description/3 ]).


% For the vhost_config_entry record:
-include("class_USWebConfigServer.hrl").


% Shorthands:

-type tcp_port() :: net_utils:tcp_port().

-type maybe_url() :: maybe( web_utils:url() ).

-type protocol_type() :: web_utils:protocol_type().
-type html_element() :: web_utils:html_element().

-type domain_id() :: class_USWebConfigServer:domain_id().
-type vhost_config_table() :: class_USWebConfigServer:vhost_config_table().
-type vhost_config_entry() :: class_USWebConfigServer:vhost_config_entry().
-type domain_config_table() :: class_USWebConfigServer:domain_config_table().
-type meta_web_settings() :: class_USWebConfigServer:meta_web_settings().


% Anchors:

-define( domain_catch_all_target, "domain-catch-all" ).
-define( vhost_catch_all_target, "vhost-catch-all" ).


% Silencing:
-export([ get_vhost_display_name/2 ]).


% @doc Returns the HTML header for a meta page.
-spec get_page_header() -> html_element().
get_page_header() ->
	"<!DOCTYPE html>
<html lang=\"EN\">
  <head>
	<title>Meta website</title>
	<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
	<meta http-equiv=\"Content-Language\" content=\"en\">
	<style>
	  body { background-color: #ecce77 ;}

	  table, th, td {
		  border: 1px solid black;
		  border-collapse: collapse;
		  padding: 15px;
	  }

	  /* Taken from https://stackoverflow.com/questions/166160/how-can-i-scale-the-content-of-an-iframe */

	  .wrap
	  {

		  width: 640px;
		  height: 384px;
		  padding: 0;
	  }

	  .frame
	  {

		  width: 1280px;
		  height: 768px;

		  -ms-transform: scale(0.5);
		  -moz-transform: scale(0.5);
		  -o-transform: scale(0.5);
		  -webkit-transform: scale(0.5);
		  transform: scale(0.5);

		  -ms-transform-origin: 0 0;
		  -moz-transform-origin: 0 0;
		  -o-transform-origin: 0 0;
		  -webkit-transform-origin: 0 0;
		  transform-origin: 0 0;
	  }

/* Taken from http://www.w3.org/Style/Examples/007/menus.html */

div.banner {
  z-index: 1000;
  position: absolute;
  background: #ecad77;
  margin: 0;
  font-size: 65%;
  font-weight: bold;
  line-height: 1.0;
  text-align: left;
  position: fixed;
  top: 1em;
  left: auto;
  width: 12em;
  right: 1em;
}


div.banner p {
  color: #555555;
  background: #de8941;
  margin-left: 0em;
  padding:  0em;
  font-family: Arial, sans-serif;
  border: thin outset #ecad77;
  margin-top: 0%;
  margin-bottom: 0%;
  margin-left: 0%;
  margin-right: 0%;
}


div.banner a, div.banner em { display: block; margin: 0em; }
div.banner a, div.banner em { border-top: 1px groove #EEEEEE; } /* separator */
div.banner a:first-child { border-top: none; }
div.banner em { color: #FFFFFF; font-weight: bold; } /* head of column */

div.banner a:link { color: #EEEEEE; text-decoration: none;  }
div.banner a:visited { color: #CCCCCC; text-decoration: none;  }
div.banner a:hover { color: #000000; background: #FFFFFF;  }
	</style>
  </head>
".



% @doc Returns the HTML body for a meta page.
-spec get_page_body( protocol_type(), tcp_port(), domain_config_table(),
   time_utils:timestamp(), meta_web_settings(), boolean() ) -> html_element().
get_page_body( Scheme, Port, DomainCfgTable, StartTimestamp, MetaWebSettings,
			   LogAnalysisEnabled ) ->

	text_utils:format(
		"<body>"
		"<div class=\"banner\"><p><em>Domain selection</em>"
		"<a href=\"#meta_top\">[go to top]</a> "
		++ [ text_utils:format( "- ~ts~n", [ get_domain_link( D ) ] )
			 || { D, _MaybeCertManagerPid, _VHostCfgTable }
					<- table:values( DomainCfgTable ) ] ++
		"<a href=\"#meta_bottom\">[go to bottom]</a>"
		"</p></div>~n~n"
		"<center><h1>Welcome to the US-Web <b>meta</b> website</h1>"
		"</center>~n~n"
		"<h2>General Information</h2>~n"
		"<p>Server started on ~ts, running on ~ts (TCP port #~B).</p>~n"
		"<p>Note that this meta website does not self-reference.</p>~n"
		"<a name=\"meta_top\"></a>~n"
		"<p><center><a href=\"#meta_bottom\">[Go to bottom]</a></center></p>~n"
		"<p>~ts</p>~n~n~ts~n",
		[ time_utils:get_textual_timestamp( StartTimestamp ), Scheme, Port,
		  create_toc( DomainCfgTable ),
		  describe_hosting( Scheme, Port, DomainCfgTable, MetaWebSettings,
							LogAnalysisEnabled ) ] ).


% @doc Creates a table of contents for domains.
create_toc( DomainCfgTable ) ->

	"This US-Web server manages " ++ case table:values( DomainCfgTable ) of

		% Would be quite surprising:
		[] ->
			"no domain.";

		_SingleDomInfo=[ { DomainId, _MaybeCertManagerPid, _VHostCfgTable } ] ->
			"a single domain: " ++ get_domain_ref( DomainId );

		DomInfos ->
			text_utils:format( "~B domains:~n~ts", [ length( DomInfos ),
				web_utils:get_unordered_list( [ get_domain_link( DomId )
		|| { DomId, _MaybeCertManagerPid, _VHostCfgTable } <- DomInfos ] ) ] )

	end.


% @doc Describes (in HTML) the current hosting, that is all the virtual hosts
% (except the meta website itself) of all known domains.
%
describe_hosting( Scheme, Port, DomainCfgTable,
	  _MetaWebSettings={ MetaDomainId, MetaVhostId, _BinMetaContentRoot },
	  LogAnalysisEnabled ) ->

	% Ex: MaybeBinMetaURL="http://meta.foobar.org:8080".
	MaybeMetaBaseURL = case LogAnalysisEnabled of

		true ->
			text_utils:format( "~ts://~ts.~ts:~B",
							   [ Scheme, MetaVhostId, MetaDomainId, Port ] );

		false ->
			undefined

	end,

	% Even if this would work (tested as such), we prefer not including (and
	% displaying recursively) the 'meta' website within itself, so we remove it
	% first:

	{ _MetaDomainId, MaybeCertManagerPid, VHostsIncludingMeta } =
		table:get_value( MetaDomainId, DomainCfgTable ),

	VHostsWithoutMeta = table:remove_existing_entry( MetaVhostId,
													 VHostsIncludingMeta ),

	NewMetaInfo = { MetaDomainId, MaybeCertManagerPid, VHostsWithoutMeta },

	MetaLessCfgTable = table:add_entry( MetaDomainId, NewMetaInfo,
										DomainCfgTable ),

	get_domain_descriptions( Scheme, Port, table:values( MetaLessCfgTable ),
							 MaybeMetaBaseURL ).




% @doc Returns the HTML description for the specified domains.
-spec get_domain_descriptions( protocol_type(), tcp_port(),
	[ class_USWebConfigServer:domain_info() ], maybe_url() ) -> html_element().
get_domain_descriptions( Scheme, Port, DomainInfos, MaybeMetaBaseURL ) ->

	DomStrings = lists:sort( [
	 get_domain_description( Scheme, Port, DomId,  VCfgTable, MaybeMetaBaseURL )
		  || { DomId, _MaybeCertManagerPid, VCfgTable } <- DomainInfos ] ),

	% Just for the newline:
	[ text_utils:format( "<p>~ts</p>~n", [ DomStr ] ) || DomStr <- DomStrings ].



% @doc Returns the HTML description for the specified domain.
-spec get_domain_description( protocol_type(), tcp_port(), domain_id(),
				vhost_config_table(), maybe_url() ) -> html_element().
get_domain_description( Scheme, Port, _DomainId=default_domain_catch_all,
						VCfgTable, MaybeMetaBaseURL ) ->

	text_utils:format(
	  "<a name=\"" ++ ?domain_catch_all_target ++ "\"></a>~n"
	  "<h2>For the domain catch-all</h2>~n"
	  "<p>Note that the corresponding URLs are likely not resolvable</p>~ts"
	  ++ go_to_top(),
	  [ get_vhost_descriptions( Scheme, Port, VCfgTable, MaybeMetaBaseURL ) ] );

get_domain_description( Scheme, Port, DomainId, VCfgTable, MaybeMetaBaseURL ) ->
	 text_utils:format( "<a name=\"~ts\"></a>~n"
			"<h2>Hosting for domain <code>~ts</code></h2>~n<p>~ts</p>~n",
		[ DomainId, DomainId, get_vhost_descriptions( Scheme, Port, VCfgTable,
													  MaybeMetaBaseURL ) ] )
		++ go_to_top().



% @doc Returns the HTML description for the specified virtual hosts.
-spec get_vhost_descriptions( protocol_type(), tcp_port(), vhost_config_table(),
							  maybe_url() ) -> html_element().
get_vhost_descriptions( Scheme, Port, VHostCfgTable, MaybeMetaBaseURL ) ->

	% (domain better fetched from the vhost entries)

	%trace_utils:debug_fmt( "Vhost configuration table: ~ts",
	%					   [ table:to_string( VHostCfgTable ) ] ),

	"This domain comprises " ++ case table:values( VHostCfgTable ) of

		[] ->
			"no virtual host.";

		V=[ _VHostCfgEntry ] ->
			text_utils:format( "a single virtual host: ~ts.",
			%	[ get_vhost_description( Port, VHostCfgEntry ) ] );
				[ get_table_for( V, Scheme, Port, MaybeMetaBaseURL ) ] );

		VHostCfgEntries ->
			%VStrings = lists:sort( [ get_vhost_description( Port, VE )
			%			 || VE <- VHostCfgEntries ] ),
			text_utils:format( "~B virtuals hosts:~n~ts~n~ts",
				[ length( VHostCfgEntries ), web_utils:get_unordered_list(
					[ get_vhost_link( VHostId, ParentHost )
					  || #vhost_config_entry{ virtual_host=VHostId,
								parent_host=ParentHost } <- VHostCfgEntries ] ),
				  get_table_for( VHostCfgEntries, Scheme, Port,
								 MaybeMetaBaseURL ) ] )

	end.



% @doc Returns the HTML description for the specified virtual host.
-spec get_vhost_description( protocol_type(), tcp_port(),
							 vhost_config_entry() ) -> html_element().
get_vhost_description( Scheme, Port,
					   #vhost_config_entry{ virtual_host=VHost,
											parent_host=ParentHost } ) ->

	{ URLVHost, NameVHost } = case VHost of

		default_vhost_catch_all ->
			{ "NON_EXISTING_VHOST", "*" };

		_ ->
			{ VHost, VHost }

	end,

	{ URLDomain, NameDomain } = case ParentHost of

		default_domain_catch_all ->
			{ "NON_EXISTING_DOMAIN", "*" };

		_ ->
			{ ParentHost, ParentHost }

	end,

	TargetURL = text_utils:format( "~ts://~ts.~ts:~B/",
							[ Scheme, URLVHost, URLDomain, Port ] ),

	TargetName = text_utils:format( "~ts://~ts.~ts:~B/",
							[ Scheme, NameVHost, NameDomain, Port ] ),

	ToBegin = case VHost of

		default_vhost_catch_all ->
			"virtual host catch-all is ";

		_ ->
			text_utils:format( "<b>~ts</b> is ", [ VHost ] )

	end,

	ToAdd = case ParentHost of

		default_domain_catch_all ->
			"(not expected to be routable)";

		_ ->
			""

	end,

	text_utils:format( "~ts<a href=\"~ts\" target=\"_blank\">[~ts]</a> ~ts",
					   [ ToBegin, TargetURL, TargetName, ToAdd ] ).



% @doc Returns an HTML table suitable for specified vhost entries.
-spec get_table_for( [ vhost_config_entry() ], protocol_type(), tcp_port(),
					 maybe_url() ) -> html_element().
get_table_for( VHostCfgEntries, Scheme, Port, MaybeMetaBaseURL ) ->

	Begin = "
<p>
  <center>
	<table style=\"width:90%\">

	  <tr>
		<th>Virtual Host</th>
		<th>Index Page</th>
		<th>404 Page</th>
	  </tr>
",

	Cells = [ get_cell_for( VHEnt, Scheme, Port, MaybeMetaBaseURL )
			  || VHEnt <- VHostCfgEntries ],

	End = "
	</table>
  </center>
</p>
",
	[ Begin, Cells, End ].



% @doc Returns the HTML code for the specified table cell.
get_cell_for( #vhost_config_entry{ virtual_host=VHost,
								   parent_host=ParentHost },
			  Scheme, Port, MaybeMetaBaseURL ) ->

	{ DisplayVHost, URLVHost } = case VHost of

		without_vhost ->
			{ "", "" };

		default_vhost_catch_all ->
			{ "*.", "ANY_VIRTUAL_HOST." };

		_ ->
		   VHostDot = text_utils:binary_to_string( VHost ) ++ ".",
			{ VHostDot, VHostDot }

	end,

	{ DisplayParentHost, URLParentHost } = case ParentHost of

		default_domain_catch_all ->
			{ "*", "ANY_DOMAIN" };

		_ ->
			{ ParentHost, ParentHost }

	end,

	URL = text_utils:format( "~ts://~ts~ts:~B",
							 [ Scheme, URLVHost, URLParentHost, Port ] ),

	DisplayFullHost = text_utils:format( "~ts~ts",
										 [ DisplayVHost, DisplayParentHost ] ),

	LogAnalysisLink = case MaybeMetaBaseURL of

		undefined ->
			"";

		_ ->
			% Opening a new (blank) page is preferred, to avoid reloading more
			% frequently than needed the whole Meta page, which may be very
			% heavy (as including many full websites):
			%
			PageFilename = class_USWebLogger:get_file_prefix_for( ParentHost,
									VHost, _Tool=awstats ) ++ ".html",
			text_utils:format(
			  "<a href=\"~ts/~ts\" target=\"_blank\">[log analysis]</a>",
			  [ MaybeMetaBaseURL, PageFilename ] )

	end,

	text_utils:format( "
 <tr>
	<td><center><a href=\"~ts\">~ts</a> <br>~ts</center></td>

	<td>~ts
	  <div class=\"wrap\">
		<iframe src=\"~ts\" class=\"frame\"></iframe>
	  </div>
	</td>

	<td>
	  <div class=\"wrap\">
		<iframe src=\"~ts/NON_EXISTING_PAGE.html\" class=\"frame\"></iframe>
	  </div>
	</td>

  </tr>
	", [ URL, DisplayFullHost, LogAnalysisLink,
		 get_vhost_ref( VHost, ParentHost ), URL, URL ] ).


% @doc Returns the anchor corresponding to the specified .domain
get_domain_anchor( _DomainId=default_domain_catch_all ) ->
	"domain-catch-all";

get_domain_anchor( DomainId ) ->
	DomainId.


% @doc Returns the display name for the specified domain.
get_domain_display_name( _DomainId=default_domain_catch_all ) ->
	"domain catch-all";

get_domain_display_name( DomainId ) ->
	text_utils:format( "domain ~ts", [ DomainId ] ).


% @doc Returns the short display name for the specified domain.
get_domain_short_display_name( _DomainId=default_domain_catch_all ) ->
	"*";

get_domain_short_display_name( DomainId ) ->
	text_utils:format( "~ts", [ DomainId ] ).


% @doc Returns a reference to the specified domain anchor.
get_domain_ref( DomainId ) ->
	text_utils:format( "<a name=\"~ts\"></a>",
					   [ get_domain_anchor( DomainId ) ] ).


% @doc Returns a link to the specified domain anchor.
get_domain_link( DomainId ) ->
	text_utils:format( "<a href=\"#~ts\">~ts</a>",
					   [ get_domain_anchor( DomainId ),
						 get_domain_short_display_name( DomainId ) ] ).



% @doc Returns the anchor corresponding to the specified vhost.
get_vhost_anchor( VHostId, ParentHost ) ->

	DomainAnchor = get_domain_anchor( ParentHost ),

	text_utils:format( case VHostId of

		 without_vhost ->
			"no-vhost";

		default_vhost_catch_all ->
			"vhost-catch-all";

		_ ->
			text_utils:format( "vhost-~ts", [ VHostId ] )

					   end ++ "-~ts", [ DomainAnchor ] ).



% @doc Returns the display name for the specified vhost.
get_vhost_display_name( _VHostId=without_vhost, ParentHost ) ->
	text_utils:format( "host for the ~ts itself",
					   [ get_domain_display_name( ParentHost ) ] );

get_vhost_display_name( _VHostId=default_vhost_catch_all, ParentHost ) ->
	text_utils:format( "virtual host catch-all for ~ts",
					   [ get_domain_display_name( ParentHost ) ] );

get_vhost_display_name( VHostId, ParentHost ) ->
	text_utils:format( "virtual host ~ts for ~ts",
					   [ VHostId, get_domain_display_name( ParentHost ) ] ).



% @doc Returns the short display name for the specified vhost.
get_vhost_short_display_name( _VHostId=without_vhost, ParentHost ) ->
	 get_domain_short_display_name( ParentHost );

get_vhost_short_display_name( _VHostId=default_vhost_catch_all, ParentHost ) ->
	text_utils:format( "*.~ts",
		[ get_domain_short_display_name( ParentHost ) ] );

get_vhost_short_display_name( VHostId, ParentHost ) ->
	text_utils:format( "~ts.~ts",
		[ VHostId, get_domain_short_display_name( ParentHost ) ] ).



% @doc Returns a reference to the specified vhost anchor.
get_vhost_ref( VHostId, ParentHost ) ->
	text_utils:format( "<a name=\"~ts\"></a>",
					   [ get_vhost_anchor( VHostId, ParentHost ) ] ).



% @doc Returns a link to the specified vhost anchor.
get_vhost_link( VHostId, ParentHost ) ->
	text_utils:format( "<a href=\"#~ts\">~ts</a>",
		[ get_vhost_anchor( VHostId, ParentHost ),
		  get_vhost_short_display_name( VHostId, ParentHost ) ] ).



% @doc Returns the HTML footer for a meta page.
-spec get_page_footer() -> html_element().
get_page_footer() ->
	"<a name=\"meta_bottom\"></a>"
		"</body>"
		"</html>".



% @doc Returns a "go to top" link.
-spec go_to_top() -> html_element().
go_to_top() ->
	% Just for the newline:
	text_utils:format(
	  "<p><center><a href=\"#meta_top\">[Go to top]</a></center></p>~n~n", [] ).
