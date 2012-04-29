-module(chatter_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(LISTEN_PORT, 9090).

-record(state, {listening_Socket, userEmail}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, Socket} = gen_tcp:listen(?LISTEN_PORT, [{active, true}]),
	TimeoutToContinueInHandleInfo = 0, 
    {ok, #state{listening_Socket = Socket}, TimeoutToContinueInHandleInfo}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,_Socket,RawData}, _State) ->
	error_logger:info_msg("Data received: ~s \n", [RawData]),
	{noreply, _State};
handle_info(timeout, #state{listening_Socket = Listening_Socket} = State) ->
	error_logger:info_msg("Accepting socket connection. \n"),
	{ok, _Sock} = gen_tcp:accept(Listening_Socket),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listening_Socket = Listening_Socket} = _State) ->
	gen_tcp:close(Listening_Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

