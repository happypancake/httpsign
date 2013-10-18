-module(http_signatures_test).

-include_lib("eunit/include/eunit.hrl").

rsa_sha_test() ->
  http_signatures_app:start(normal, fun get_key_by_id/1),
  ExampleHeader = get_fixture("example_header.txt"),
  ExpectedAuthenticationHeader = get_fixture("expected_authentication_header.txt"),

  Result = http_signatures:get_authentication_header(ExampleHeader),

  ?_assertEqual(ExpectedAuthenticationHeader, Result).

get_key_by_id(<<"Test">>) ->
  PemBin = get_fixture("test.pem"),
  [RSAEntry] = public_key:decode(PemBin),
  RSAEntry.

get_fixture(FileName) ->
  {ok, Content} = file:read_file("../priv/fixtures/"++FileName),
  Content.
