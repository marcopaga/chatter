-module(chatter_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(LISTEN_PORT, 9090).

-record(state, {listening_socket, userEmail}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, parseCredentials/1]).

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
	{ok, ListeningSocket} = gen_tcp:listen(?LISTEN_PORT, [{active, true}]),
	TimeoutToContinueInitializationInHandleInfo = 0, 
    {ok, #state{listening_socket = ListeningSocket, userEmail = ""}, TimeoutToContinueInitializationInHandleInfo}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,Socket,RawData}, State = #state{} ) when State#state.userEmail == "" ->
	{success, UserEmail} = try_authentication(RawData),
	{noreply, State#state{userEmail = UserEmail}};

handle_info({tcp,Socket,RawData}, State = #state{} ) ->
	error_logger:info_msg("~s : ~s",[State#state.userEmail, RawData]),
	gen_tcp:send(Socket,RawData),
	{noreply, State};

handle_info(timeout, #state{listening_socket = ListeningSocket} = State) ->
	error_logger:info_msg("Accepting socket connections. \n"),
	{ok, Socket} = gen_tcp:accept(ListeningSocket),
	gen_tcp:send(Socket, "Please authenticate. Type your username/password\n"),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listening_socket = ListeningSocket} = _State) ->
	gen_tcp:close(ListeningSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
try_authentication(RawData) ->
    {credentials, User, Password} = parseCredentials(RawData),
	error_logger:info_msg("~s tries to login with ~s",[User, Password]),
	{success, User}.

parseCredentials(RawData) ->
	WithoutLineEndings = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [User, Password]} = re:run(WithoutLineEndings, "(.*)/(.*)",[anchored, {capture, [1,2], list}]),
    {credentials, User, Password}.