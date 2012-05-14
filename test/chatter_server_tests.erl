-module(chatter_server_tests).
-include_lib("eunit/include/eunit.hrl").

parseCredentials_test() ->
	{credentials, "marco", "password"} = chatter_server:parseCredentials("marco/password"),
	ok.
