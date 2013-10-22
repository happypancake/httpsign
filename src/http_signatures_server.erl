% @private
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

handle_call({set_key,{Id, Algo=sha256, Data}}, _From, Keys) ->
  PemEntries = public_key:pem_decode(Data),
  [_PublicKey, PrivateKey] = 
      lists:map(fun public_key:pem_entry_decode/1, PemEntries),
  NewKeys = lists:keystore(Id, 1, Keys, {Id, Algo, PrivateKey}),
  {reply, ok, NewKeys};
handle_call({sign_response,{Response, KeyId, Fields}}, _from, Keys) ->
  Auth = get_signature(Response,KeyId,Fields,Keys),
  Result = insert_authorization_in_response(Response,Auth),
  { reply, Result, Keys };
handle_call({validate_response,ARequest}, _From, Keys) ->
  {Headers, _Body} = parse_request(ARequest),
  Auth = lists:keyfind("authorization",1,Headers),
  SignatureInfo = parse_signature_info(Auth),
  {<<"keyId">>,KeyId} = lists:keyfind(<<"keyId">>,1,SignatureInfo),
  FieldInfo = lists:keyfind(<<"headers">>,1,SignatureInfo),
  Fields = parse_field_info(FieldInfo),
  Expected = get_signature(ARequest,KeyId,Fields,Keys),
  {reply, {"authorization",Expected} == Auth, Keys}.

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

external_algo_name(sha256) ->
  <<"rsa-sha256">>;
external_algo_name(_) ->
  <<"unknown">>.

get_signature(ARequest, KeyId, Fields, Keys) ->
  {Headers, _} = parse_request(ARequest),
  Key = lists:keyfind(KeyId, 1, Keys),
  {Algo, Signature} = generate_signature(Key, Headers, Fields),
  Hash = base64:encode(Signature),
  build_authorization_string(KeyId, Algo, Hash, Fields).

parse_request(ARequest) ->
  [RequestLine,From2] = binary:split(ARequest,<<"\n">>),
  [HeaderText,Body] = binary:split(From2,<<"\n\n">>),
  HeaderLines = binary:split(HeaderText,<<"\n">>,[global]),
  Headers = lists:map(fun line_to_header_tuple/1,HeaderLines),
  {[{"request-line",RequestLine}|Headers], Body}.

line_to_header_tuple(Line) ->
  [UnicodeKey,Value] = binary:split(Line, <<": ">>),
  Key = string:to_lower(binary:bin_to_list(UnicodeKey)),
  {Key,Value}.

parse_signature_info({"authorization",Signature}) ->
  [<<>>,Remainder] = binary:split(Signature,<<"Signature ">>),
  Items = binary:split(Remainder,<<"\",">>,[global]),
  lists:foldl(fun parse_signature_items/2, [], Items);
parse_signature_info(_) ->
  [{<<"keyId">>,<<"unknown">>},{<<"headers">>,<<"">>}].

parse_signature_items(KVs, Result) ->
  KeyValue = binary:split(KVs,<<"=\"">>),
  parse_signature_value(KeyValue, Result).

parse_signature_value([Name, Value], Result) ->
  lists:keystore(Name, 1, Result, {Name,Value});
parse_signature_value(_DontCare, Result) ->
  Result.

parse_field_info({<<"headers">>,FieldInfo}) ->
  Splitted = binary:split(FieldInfo,<<" ">>,[global]),
  Fields = lists:map(fun binary_to_list/1, Splitted),
  lists:map(fun string:to_lower/1, Fields).

generate_signature(false, _Headers, _Fields) ->
  {<<"unknown">>,<<"Unknown keyId">>};
generate_signature({_KeyId, Algo, PrivateKey}, Headers, Fields) ->
  Input = get_fields(Headers, Fields),
  Signature = public_key:sign(Input, Algo, PrivateKey),
  {Algo, Signature}.

build_authorization_string(KeyId, Algo, Hash, Fields) ->  
  iolist_to_binary([<<"Signature keyId=\"">>, KeyId, 
         <<"\",algorithm=\"">>, external_algo_name(Algo), 
         <<"\",headers=\"">>, build_fields(Fields),
         <<"\",signature=\"">>, Hash,  
         <<"\"">>]).

build_fields(Fields) ->
  list_to_binary(string:join(Fields," ")).

get_fields(Headers,Fields) ->
  get_fields(Headers, Fields, []).

get_fields(_Headers, [], Acc) ->
  Bin = iolist_to_binary(Acc),
  binary:part(Bin,0, byte_size(Bin)-1);
get_fields(Headers, [H|T], Acc) ->
  {H, Value} = lists:keyfind(H,1,Headers),
  ToAdd = [list_to_binary(H),<<": ">>,Value,<<"\n">>],
  get_fields(Headers, T, [Acc| ToAdd]).

insert_authorization_in_response(Response, Authorization) ->
  [Pre,Post] = binary:split(remove_authorization(Response), <<"\n\n">>),
  iolist_to_binary([Pre, <<"\nAuthorization: ">>, Authorization, <<"\n\n\n">>, Post]).

remove_authorization(Response) ->
  try
    [Pre, Post] = binary:split(Response,"\nAuthorization: "),
    [_,SecondPart] = binary:split(Post,"\n"),
    iolist_to_binary([Pre, <<"\n">>, SecondPart])
  catch
    error:badarg -> Response
  end.

