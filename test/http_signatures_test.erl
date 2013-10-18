-module(http_signatures_test).

-include_lib("eunit/include/eunit.hrl").

-define(RSA_KEY,<<"test">>).

rsa_sha_test() ->
  ExampleRequest = get_fixture("example_request.txt"),
  ExampleResponse = get_fixture("example_response.txt"),
  ExpectedSignature = get_fixture("expected_signature.txt"),
  PemBin = get_fixture("test.pem"),

  application:start(crypto),
  application:start(http_signatures),
  
  ok = http_signatures:register_key(?RSA_KEY,<<"rsa-sha256">>,PemBin),

  ExpectedSignature =
                http_signatures:get_authorization(ExampleRequest,?RSA_KEY),

  ?assertEqual(no_authentication, 
                http_signatures:is_a_valid_response(ExampleRequest)),
  
  ?assertEqual(ok, 
                http_signatures:is_a_valid_response(ExampleResponse)).
  

get_fixture(FileName) ->
  {ok, Content} = file:read_file("../priv/fixtures/"++FileName),
  Content.
