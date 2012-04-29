-module(chatter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mnesia:start(),
    {ok, Pid_of_supervisor} = chatter_sup:start_link(),
    chatter_db:create_tables(),
    {ok, Pid_of_supervisor}.

stop(_State) ->
    ok.
