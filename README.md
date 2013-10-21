# http_signatures

[Http signature draft](http://tools.ietf.org/html/draft-cavage-http-signatures-00#section-4.1) 
implementation for Erlang.

Currently only implements `sha256`.

## Documentation

[Online here](http://happypancake.github.io/http_signatures/http_signatures.html)

## Usage
```erlang
tojans@ubuntu:/mnt/hgfs/develop/erlang/http_signatures$ erl -pa ebin
Erlang R16B02 (erts-5.10.3) [source-5d89ddd] [64-bit] [async-threads:10] [kernel-poll:false]

Eshell V5.10.3  (abort with ^G)
1> application:start(http_signatures).
ok
2> {ok, PemFile} = file:read_file("priv/fixtures/test.pem").
{ok,<<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDCFENGw33yGihy92pDjZQhl0C3\n6rPJj+CvfSC8+q28"...>>}
3> KeyId = <<"Test">>.
<<"Test">>
4> http_signatures:register_key(KeyId, sha256, PemFile).
ok
5> {ok, Resp} = file:read_file("priv/fixtures/example_response.txt").
{ok,<<"POST /foo?param=value&pet=dog HTTP/1.1\nHost: example.com\nDate: Thu, 05 Jan 2012 21:31:40 GMT\nContent-Type: a"...>>}
6> http_signatures:is_signed_response(Resp).
false
7> SignedResp = http_signatures:get_signed_response(Resp,KeyId).
<<"POST /foo?param=value&pet=dog HTTP/1.1\nHost: example.com\nDate: Thu, 05 Jan 2012 21:31:40 GMT\nContent-Type: applicati"...>>
8> http_signatures:is_signed_response(SignedResp).              
true
9> AlteredResp = binary:replace(SignedResp, <<"Thu, 05 Jan">>,<<"Thu, 06 Jan">>).
<<"POST /foo?param=value&pet=dog HTTP/1.1\nHost: example.com\nDate: Thu, 06 Jan 2012 21:31:40 GMT\nContent-Type: applicati"...>>
10> http_signatures:is_signed_response(AlteredResp).                              
false
11> 
```
