-module(http_signatures).
-export([get_authorization_header/2,get_authorization_header/3]).
-export([get_signed_response/2,get_signed_response/3]).
-export([register_key/3,is_signed_response/1]).

-define(SERVER,http_signatures_server).

register_key(Name, Algorithm, KeyData) ->
  gen_server:call(?SERVER, {set_key, {Name, Algorithm, KeyData}}).

get_authorization_header(Response, KeyId) ->
  get_authorization_header(Response, KeyId, ["date"]).

get_authorization_header(Response, KeyId, Fields) ->
  gen_server:call(?SERVER, {get_authorization, {Response, KeyId, Fields }}).

get_signed_response(Response, KeyId) ->
  get_signed_response(Response, KeyId, ["date"]).

get_signed_response(Response, KeyId, Fields) ->
  gen_server:call(?SERVER, {sign_response, {Response, KeyId, Fields }}).

is_signed_response(Response) ->
  gen_server:call(?SERVER, {validate_response, Response}).
