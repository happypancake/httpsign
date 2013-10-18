-module(http_signatures).
-export([get_authorization/2,register_key/3]).

-define(SERVER,http_signatures_server).

register_key(Name, Algorithm, KeyData) ->
  gen_server:call(?SERVER, {set_key, {Name, Algorithm, KeyData}}).

get_authorization(ARequest, KeyId) ->
  gen_server:call(?SERVER, {get_authorization, {ARequest, KeyId, ["date"]}}).
