-module(http_signatures_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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

init([]) ->
  {ok, []}.

handle_call({set_key,{Id, Algo, Data}}, _From, Keys) ->
  NewKeys = lists:keystore(Id, 1, Keys, {Id, Algo, Data}),
  {reply, ok, NewKeys};
handle_call({get_authorization, {ARequest, KeyId, Fields}}, _From, Keys) ->
  Result = get_signature(ARequest, KeyId, Fields, Keys),
  {reply, Result, Keys};
handle_call({validate_response,_}, _From, Keys) ->
  {reply, not_implemented, Keys}.

handle_cast(_Msg, State) ->
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

get_signature(ARequest, KeyId, Fields, Keys) ->
  {Headers, _} = parse_request(ARequest),
  Key = lists:keyfind(KeyId, 1, Keys),
  {Algo, Signature} = generate_signature(Key, Headers, Fields),
  Hash = base64:encode(Signature),
  build_authorization_string(KeyId, Algo, Hash).

parse_request(ARequest) ->
  [RequestLine,From2] = binary:split(ARequest,<<"\n">>),
  [HeaderText,Body] = binary:split(From2,<<"\n\n">>),
  HeaderLines = binary:split(HeaderText,<<"\n">>,[global]),
  Headers = lists:map(fun line_to_header_tuple/1,HeaderLines),
  {[{"request-line",RequestLine}|Headers], Body}.

line_to_header_tuple(Line) ->
  [UnicodeKey,Value] = binary:split(Line, <<": ">>),
  Key = string:to_lower(binary:bin_to_list(UnicodeKey)),
  io:format("~p ~p ~n", [Key, Value]),
  {Key,Value}.

generate_signature(false, _Headers, _Fields) ->
  {<<"unknown">>,<<"Unknown keyId">>};
generate_signature({_KeyId, Algo = <<"rsa-sha256">>,KeyData}, Headers, Fields) ->
  Input = get_fields(Headers, Fields),
  PemEntries = public_key:pem_decode(KeyData),
  [PublicKey, PrivateKey] = lists:map(fun public_key:pem_entry_decode/1, PemEntries),
  Signature = public_key:sign(Input, sha, PrivateKey),
  true = public_key:verify(Input, sha, Signature, PublicKey),
  {Algo, Signature}.

build_authorization_string(KeyId, Algo, Hash) ->  
  io:format("~p ~p ~p ~n",[KeyId, Algo, Hash]),
  bjoin([<<"Signature keyId=\"">>, KeyId, <<"\",algorithm=\"">>, Algo, 
         <<"\",signature=\"">>, Hash,  <<"\"">>]).

get_fields(Headers,Fields) ->
  get_fields(Headers, Fields, []).

get_fields(_Headers, [], Acc) ->
  Bin = bjoin(Acc),
  binary:part(Bin,0, byte_size(Bin)-1);
get_fields(Headers, [H|T], Acc) ->
  {H, Value} = lists:keyfind(H,1,Headers),
  ToAdd = [list_to_binary(H),<<": ">>,Value,<<"\n">>],
  get_fields(Headers, T, [Acc| ToAdd]).

bjoin(List) ->
  F = fun(A, B) -> <<A/binary, B/binary>> end,
  lists:foldr(F, <<>>, lists:flatten(List)).
