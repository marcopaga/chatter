-module(chatter_db).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% Mnesia Tables

-record(user, {email, password}).
-record(message, {from_user_email, to_user_email, text, timestamp}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, create_tables/0, create_user/2, create_message/3]).

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

create_tables() ->
	gen_server:call(?SERVER, create_tables).

create_user(Email, Password) ->
	gen_server:call(?SERVER, {create_user, [{email, Email}, {password, Password}] }).

create_message(From, To, Text) ->
	gen_server:cast(?SERVER, {create_message, [{from, From}, {to, To}, {text, Text}]}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(create_tables, _From, State) ->
	mnesia:create_table(user, [
									{attributes, record_info(fields, user)},
									{type, bag}
								]),
    mnesia:create_table(message, [
    								{attributes, record_info(fields, message)},
    								{type, bag}
								]),
    error_logger:info_msg("The tables are created.\n"),
    {reply, ok, State};
handle_call({create_user, [ {email, Email}, {password, Password}] }, _From, State) ->
	mnesia:dirty_write(#user{email = Email, password = Password}),
	{reply, ok, State}.

handle_cast({create_message,[{from, From}, {to,To}, {text, Text}]}, State) ->
    Now = erlang:localtime(),
	NewMessage = #message{from_user_email = From, to_user_email = To, text = Text, timestamp = Now},
	Transaction = fun() ->
		mnesia:write(NewMessage)
	end,
	mnesia:transaction(Transaction),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------