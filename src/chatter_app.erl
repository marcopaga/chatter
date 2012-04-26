-module(chatter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mnesia:start(),
    chatter_db:create_tables(),
    chatter_sup:start_link().

stop(_State) ->
    ok.
