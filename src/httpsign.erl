% @doc Apply the Http signature protocol to web requests.
%
%
% This is a way to add origin authentication, message
% integrity, and replay resistance to HTTP requests.  It is intended to
%  be used over the HTTPS protocol.
%
% It does this by adding an `Authorization' header to the response. If it exists,
% it gets replaced.
%
% This header contains a `KeyId', the algorithm used to calculate a signature,
% a list of the headers used to calculate the signature, and the signature in
% itself.
%
% Check <a href="http://tools.ietf.org/html/draft-cavage-http-signatures-00">the http signature draft</a> for more information.
%
% == An example using the CLI ==
%
% ```
%   tojans@ubuntu:/mnt/hgfs/develop/erlang/httpsign$ erl -pa ebin
%   Erlang R16B02 (erts-5.10.3) [source-5d89ddd] [64-bit] [async-threads:10] [kernel-poll:false]
%
%   Eshell V5.10.3  (abort with ^G)
%   1> application:start(httpsign).
%   ok
%   2> {ok, PemFile} = file:read_file("priv/fixtures/test.pem").
%   {ok,<<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDCFENGw33yGihy92pDjZQhl0C3\n6rPJj+CvfSC8+q28"...>>}
%   3> KeyId = <<"Test">>.
%   <<"Test">>
%   4> httpsign:register_key(KeyId, sha256, PemFile).
%   ok
%   5> {ok, Resp} = file:read_file("priv/fixtures/example_response.txt").
%   {ok,<<"POST /foo?param=value&pet=dog HTTP/1.1\nHost: example.com\nDate: Thu, 05 Jan 2012 21:31:40 GMT\nContent-Type: a"...>>}
%   6> httpsign:is_signed_response(Resp).
%   false
%   7> SignedResp = httpsign:get_signed_response(Resp,KeyId).
%   <<"POST /foo?param=value&pet=dog HTTP/1.1\nHost: example.com\nDate: Thu, 05 Jan 2012 21:31:40 GMT\nContent-Type: applicati"...>>
%   8> httpsign:is_signed_response(SignedResp).              
%   true
%   9> AlteredResp = binary:replace(SignedResp, <<"Thu, 05 Jan">>,<<"Thu, 06 Jan">>).
%   <<"POST /foo?param=value&pet=dog HTTP/1.1\nHost: example.com\nDate: Thu, 06 Jan 2012 21:31:40 GMT\nContent-Type: applicati"...>>
%   10> httpsign:is_signed_response(AlteredResp).                              
%   false
%   11> 
% '''
%
% @reference Written by: <a href="http://www.happypancake.com">Happy Pancake</a>.
% @reference License: <a href="https://raw.github.com/HappyPancake/httpsign/master/LICENSE">Apache 2.0</a>.
% @reference Source: <a href="https://github.com/HappyPancake/httpsign">GitHub</a>.
% @end

-module(httpsign).
-export([get_signed_response/2,get_signed_response/3]).
-export([register_key/3,is_signed_response/1]).

-define(SERVER,httpsign_server).

-type key_id_type() :: binary().
-type filename_type() :: list().
-type algo_type() :: 'sha256'.
-type response_type() :: binary().
-type header_name() :: list(). % Lowercase

%% @doc Register a `KeyId' by defining it's `PEM file' and the `algorithm' used.
%% If the last parameter is a `binary', this will be considered as the 
%% content of the PEM-file. If it is a `list', this will be assumed to be 
%% a filename, and the content will be read from a file with this filename.
-spec register_key(key_id_type(), algo_type(), filename_type()|binary()) -> 'ok'.
%% @end
register_key(Name, Algorithm, KeyFileName) when is_list(KeyFileName) ->
  {ok, KeyData} = file:read_file(KeyFileName),
  register_key(Name, Algorithm, KeyData);
register_key(Name, Algorithm, KeyData) ->
  gen_server:call(?SERVER, {set_key, {Name, Algorithm, KeyData}}).

%% @doc Add or replace an `Authorization' header in a `HTTP response' using
%% only the `date' header to sign the response.
%%
%% Use the `"date"' header from the `response' as input, and sign it
%% using the `KeyId'. 
-spec get_signed_response(response_type(), key_id_type()) -> response_type().
%% @end
get_signed_response(Response, KeyId) ->
  get_signed_response(Response, KeyId, ["date"]).

%% @doc Add or replace an `Authorization' header in a `HTTP response' using
%% user-defined headers to sign the response.
%%
%% Use the `defined headers' from the `response' to select the headers that
%% will be used to build the input for the signature, and sign it using the 
%% `KeyId'. 
%%
%% The `defined headers' are a list of lowercase strings containing header names.
%%    
%%  `get_signed_response(AResponse, AKey, ["date","host","content-type"])'
%%
%% The `headers' must exist in the `response'!
-spec get_signed_response(response_type(), key_id_type(),[header_name()]) -> response_type().
%% @end
get_signed_response(Response, KeyId, Fields) ->
  gen_server:call(?SERVER, {sign_response, {Response, KeyId, Fields }}).

%% @doc Returns `true' when the `HTTP response' is contains an `authorization'
%% header  with a known `KeyId' and a valid signature, `false' otherwise.
-spec is_signed_response(response_type()) -> boolean().
%% @end
is_signed_response(Response) ->
  gen_server:call(?SERVER, {validate_response, Response}).
