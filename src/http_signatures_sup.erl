-module(http_signatures_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Pars), {I, {I, start_link, Pars}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link([KeyGetter]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [KeyGetter]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([KeyGetter]) ->

  {ok, { {one_for_one, 5, 10}, [
        ?CHILD(http_signatures_server, worker, [KeyGetter])
        ]} }.

