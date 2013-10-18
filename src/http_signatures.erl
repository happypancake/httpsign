-module(http_signatures).

-define(SERVER,http_signatures_server).

-export([get_authentication_header/1]).

get_authentication_header(AHeader) ->
  gen_server:call({get_header, AHeader}).
