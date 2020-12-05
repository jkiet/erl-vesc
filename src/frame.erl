-module(frame).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([scan/1, form/1]).


scan(<<2, N:1/binary, T/binary>>) ->
  [L] = binary_to_list(N),
  case T of
    <<P:L/binary, C:2/binary, S/binary>> ->
      case {<<(crc16:crc(xmodem, P)):16>>, S} of
        {C, <<3>>} ->
          {ok, P};
        {_, <<>>} ->
          {continue, <<2, N/binary, T/binary>>};
        _ ->
          {corrupted, <<2, N/binary, T/binary>>}
      end;
    _ ->
      {continue, <<2, N/binary, T/binary>>}
  end;
scan(<<2, B/binary>>) ->
  {continue, <<2, B/binary>>};
scan(X) ->
  {unknown, X}.

form(Payload) ->
  Len = length(binary_to_list(Payload)),
  Crc = crc16:crc(xmodem, Payload),
  <<2, Len:8, Payload/binary, Crc:16, 3>>.

-ifdef(TEST).
scan_test() ->
  ?assertEqual({continue, <<2>>}, scan(<<2>>)),
  ?assertEqual({unknown, <<3>>}, scan(<<3>>)),
  ?assertEqual({continue, <<2, 1>>}, scan(<<2, 1>>)),
  ?assertEqual({unknown, <<3, 1>>}, scan(<<3, 1>>)),
  ?assertEqual({continue, <<2, 1, 0>>}, scan(<<2, 1, 0>>)),
  ?assertEqual({continue, <<2, 1, 0, 0, 0>>}, scan(<<2, 1, 0, 0, 0>>)),
  ?assertEqual({corrupted, <<2, 1, 0, 0, 0, 0>>}, scan(<<2, 1, 0, 0, 0, 0>>)),
  ?assertEqual({ok, <<0>>}, scan(<<2, 1, 0, 0, 0, 3>>)),
  ?assertEqual({corrupted, <<2, 1, 0, 0, 0, 0, 0>>}, scan(<<2, 1, 0, 0, 0, 0, 0>>)),
  ?assertEqual(
    {ok, <<$1, $2, $3, $4, $5, $6, $7, $8, $9>>},
    scan(<<2, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9, 16#31, 16#c3, 3>>)),
  ?assertEqual({ok, <<"foo">>}, scan(form(<<"foo">>))).
-endif.



