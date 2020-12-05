-module(crc16).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CRC_VARIANT,
  #{xmodem => {16#1021, 0, 0, false},
    kermit => {16#8408, 0, 0, true},
    modbus => {16#a001, 16#ffff, 0, true},
    maxim => {16#a001, 0, 16#ffff, true}}).

-export([crc/2]).

crc(Variant, Bytes) ->
  {Poly, Init, FinX, Reflect} = maps:get(Variant, ?CRC_VARIANT),
  F = fun
        F(V, 0) ->
          (V band 16#ffff);
        F(V, N) ->
          F(case V band 16#8000 of
              0 ->
                V bsl 1;
              _ ->
                (V bsl 1) bxor Poly
            end,
            N - 1)
      end,
  FR = fun
         FR(V, 0) ->
           (V band 16#ffff);
         FR(V, N) ->
           FR(case (V band 1) of
                1 ->
                  (V bsr 1) bxor Poly;
                _ ->
                  V bsr 1
              end,
             N - 1)
       end,
  lists:foldl(
    fun(V, A) ->
      if
        Reflect ->
          (A bsr 8) bxor FR((A band 16#ff) bxor V, 8);
        true ->
          (A bsl 8) bxor F(((((A bsr 8) band 16#ff) bxor V) bsl 8), 8)
      end band 16#ffff
    end,
    Init,
    binary:bin_to_list(Bytes)) bxor FinX.

-ifdef(TEST).
crc_test() ->
  ?assertEqual(16#31c3, crc(xmodem, <<$1, $2, $3, $4, $5, $6, $7, $8, $9>>)),
  ?assertEqual(16#2189, crc(kermit, <<$1, $2, $3, $4, $5, $6, $7, $8, $9>>)),
  ?assertEqual(16#4b37, crc(modbus, <<$1, $2, $3, $4, $5, $6, $7, $8, $9>>)),
  ?assertEqual(16#44c2, crc(maxim, <<$1, $2, $3, $4, $5, $6, $7, $8, $9>>)).
-endif.










