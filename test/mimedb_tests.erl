%% Copyright (c) 2021 Bryan Frimin <bryan@frimin.fr>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mimedb_tests).

-include_lib("eunit/include/eunit.hrl").

with_db(Tests) ->
  {setup,
   fun () ->
       {ok, _} = application:ensure_all_started(mimedb),
       ok
   end,
   fun (_) ->
       error_logger:tty(false),
       ok = application:stop(mimedb),
       error_logger:tty(true)
   end,
   Tests}.

find_by_name_test_() ->
  with_db(
    [?_assertMatch(
        {ok, _},
        mimedb:find_by_name(<<"JSON document">>, mimedb_default)),
     ?_assertMatch(
        error,
        mimedb:find_by_name(<<"kjaslkjasdjlkj">>, mimedb_default))]).

get_by_name_test_() ->
  with_db(
    [?_assertMatch(
        #{comment := <<"JSON document">>},
        mimedb:get_by_name(<<"JSON document">>, mimedb_default)),
     ?_assertException(
        error,
        {unknown_mimetype, <<"foo">>},
        mimedb:get_by_name(<<"foo">>, mimedb_default)),

     ?_assertMatch(
        #{comment := <<"JSON document">>},
        mimedb:get_by_name(<<"JSON document">>, mimedb_default, #{})),
     ?_assertMatch(
        #{},
        mimedb:get_by_name(<<"foo">>, mimedb_default, #{}))]).

find_by_type_test_() ->
  with_db(
    [?_assertMatch(
        {ok, _},
        mimedb:find_by_type(<<"application/json">>, mimedb_default)),
     ?_assertMatch(
        error,
        mimedb:find_by_type(<<"kjaslkjasdjlkj">>, mimedb_default))]).

get_by_type_test_() ->
  with_db(
    [?_assertMatch(
        #{comment := <<"JSON document">>},
        mimedb:get_by_type(<<"application/json">>, mimedb_default)),
     ?_assertException(
        error,
        {unknown_mimetype, <<"foo">>},
        mimedb:get_by_type(<<"foo">>, mimedb_default)),

     ?_assertMatch(
        #{comment := <<"JSON document">>},
        mimedb:get_by_type(<<"application/json">>, mimedb_default, #{})),
     ?_assertMatch(
        #{},
        mimedb:get_by_type(<<"foo">>, mimedb_default, #{}))]).

find_by_extension_test_() ->
  with_db(
    [?_assertMatch(
        {ok, _},
        mimedb:find_by_extension(<<"json">>, mimedb_default)),
     ?_assertMatch(
        error,
        mimedb:find_by_extension(<<"kjaslkjasdjlkj">>, mimedb_default))]).

get_by_extension_test_() ->
  with_db(
    [?_assertMatch(
        #{comment := <<"JSON document">>},
        mimedb:get_by_extension(<<"json">>, mimedb_default)),
     ?_assertException(
        error,
        {unknown_mimeextension, <<"foo">>},
        mimedb:get_by_extension(<<"foo">>, mimedb_default)),

     ?_assertMatch(
        #{comment := <<"JSON document">>},
        mimedb:get_by_extension(<<"json">>, mimedb_default, #{})),
     ?_assertMatch(
        #{},
        mimedb:get_by_extension(<<"foo">>, mimedb_default, #{}))]).

is_text_test_() ->
  with_db(
    [?_assert(mimedb:is_text(#{type => <<"text/plain">>})),
     ?_assert(mimedb:is_text(#{type => <<"text/html">>})),
     ?_assert(mimedb:is_text(#{type => <<"application/json">>})),
     ?_assertNot(mimedb:is_text(#{type => <<"video/3gpp">>})),
     ?_assertNot(mimedb:is_text(#{type => <<"image/png">>}))]).

is_image_test_() ->
  [?_assert(mimedb:is_image(#{type => <<"image/png">>})),
   ?_assert(mimedb:is_image(#{type => <<"image/gif">>})),
   ?_assertNot(mimedb:is_image(#{type => <<"video/3gpp">>})),
   ?_assertNot(mimedb:is_image(#{type => <<"text/plain">>})),
   ?_assertNot(mimedb:is_image(#{type => <<"application/json">>}))].

is_audio_test_() ->
  [?_assert(mimedb:is_audio(#{type => <<"audio/mp3">>})),
   ?_assert(mimedb:is_audio(#{type => <<"audio/wave">>})),
   ?_assertNot(mimedb:is_audio(#{type => <<"image/png">>})),
   ?_assertNot(mimedb:is_audio(#{type => <<"video/3gpp">>})),
   ?_assertNot(mimedb:is_audio(#{type => <<"text/html">>}))].

is_video_test_() ->
  [?_assert(mimedb:is_video(#{type => <<"video/3gpp">>})),
   ?_assert(mimedb:is_video(#{type => <<"video/mp4">>})),
   ?_assertNot(mimedb:is_video(#{type => <<"image/png">>})),
   ?_assertNot(mimedb:is_video(#{type => <<"text/plain">>}))].
