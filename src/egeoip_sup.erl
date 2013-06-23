%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

-module(egeoip_sup).
-author('bob@redivi.com').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SharedState = {shared_state,
      {egeoip_shared_state, start_link, []},
      permanent, 5000, worker, [egeoip_shared_state]},
    Pool = {egeoip_pool,
      {egeoip_pool, start_link, []},
      permanent, 5000, supervisor, [egeoip_pool, egeoip_shared_state]},
    {ok, {{one_for_all, 5, 300}, [SharedState, Pool]}}.
