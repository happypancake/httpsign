% @doc Apply the Http signature protocol to web requests.
%
% Written by <a href="http://www.happypancake.com">Happy Pancake</a>.
%
% This is a way to add origin authentication, message
% integrity, and replay resistance to HTTP requests.  It is intended to
%  be used over the HTTPS protocol.
%
% See <a href="http://tools.ietf.org/html/draft-cavage-http-signatures-00">the http signature draft</a> for more information.
% @reference License: <a href="https://raw.github.com/HappyPancake/http_signatures/master/LICENSE">Apache 2.0</a>.
% @reference Source: <a href="https://github.com/HappyPancake/http_signatures">GitHub</a>.
% @end

-module(http_signatures).
-export([get_signed_response/2,get_signed_response/3]).
-export([register_key/3,is_signed_response/1]).

-define(SERVER,http_signatures_server).

-type key_id_type() :: binary().
-type filename_type() :: list().
-type algo_type() :: 'sha256'.
-type response_type() :: binary().
-type header_name() :: list(). % Lowercase

%% @doc Register a key by defining it's PEM file and the algorithm used.
%% If the last parameter is a binary, this will be considered as the 
%% content of the PEM-file. If it is a list, this will be assumed to be 
%% a filename, and the content will be read from a file with this filename.
-spec register_key(key_id_type(), algo_type(), filename_type()|binary()) -> 'ok'.
%% @end
register_key(Name, Algorithm, KeyFileName) when is_list(KeyFileName) ->
  {ok, KeyData} = file:read_file(KeyFileName),
  register_key(Name, Algorithm, KeyData);
register_key(Name, Algorithm, KeyData) ->
  gen_server:call(?SERVER, {set_key, {Name, Algorithm, KeyData}}).

%% @doc Sign a response using the KeyId and the "date" header from the response.
%% This returns a signed response.
-spec get_signed_response(response_type(), key_id_type()) -> response_type().
%% @end
get_signed_response(Response, KeyId) ->
  get_signed_response(Response, KeyId, ["date"]).

%% @doc Sign a response using the KeyId and the defined headers from the response.
%% The headers must be in lowercase list format, for example:
%%    
%%   <code>get_signed_response(AResponse, AKey, ["date","host","content-type"])</code>
%%
%% The headers must exist in the response!
%% This returns a signed response.
-spec get_signed_response(response_type(), key_id_type(),[header_name()]) -> response_type().
%% @end
get_signed_response(Response, KeyId, Fields) ->
  gen_server:call(?SERVER, {sign_response, {Response, KeyId, Fields }}).

%% @doc Returns true when the response is signed and valid, false otherwise.
%% It extracts the key from the authentication header.
-spec is_signed_response(response_type()) -> boolean().
%% @end
is_signed_response(Response) ->
  gen_server:call(?SERVER, {validate_response, Response}).
