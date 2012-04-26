-module(chatter_db).

-record(user, {id, email}).
-record(message, {from_user_id, to_user_id, text, timestamp}).

-export([create_tables/0]).

create_tables() ->
    mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
    mnesia:create_table(message, [{attributes, record_info(fields, message)}]).
