-module(http_signatures_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, KeyGetter) ->
  http_signatures_sup:start_link([KeyGetter]).

stop(_State) ->
  ok.
