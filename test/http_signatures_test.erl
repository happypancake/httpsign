-module(http_signatures_test).

-include_lib("eunit/include/eunit.hrl").

rsa_sha_test() ->
  ExampleRequest = get_fixture("example_request.txt"),
  ExpectedAuthenticationHeader = get_fixture("expected_authentication_header.txt"),
  PemBin = get_fixture("test.pem"),

  application:start(crypto),
  application:start(http_signatures),
  
  ok = http_signatures:register_key(<<"Test">>,<<"rsa-sha256">>,PemBin),
  Result = http_signatures:get_authorization(ExampleRequest,<<"Test">>),

  ?_assertEqual(ExpectedAuthenticationHeader, Result).

get_fixture(FileName) ->
  {ok, Content} = file:read_file("../priv/fixtures/"++FileName),
  Content.
