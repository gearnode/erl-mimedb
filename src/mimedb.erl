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

-module(mimedb).

-export([is_text/1, is_image/1, is_audio/1, is_video/1,
         is_child/2, equal/2]).

-export_type([mimetype/0,
              extension/0, type/0, comment/0]).

-type mimetype() :: #{extensions := [extension()],
                      type := type(),
                      parents => [type()],
                      comment := comment()}.

-type extension() :: binary().

-type type() :: binary().

-type comment() :: binary().

-spec find_by_name(comment(), et_gen_server:ref()) -> {ok, mimetype()} | error.
find_by_name(Name, Database) ->
  mimedb_storage:search_by_name(Database, Name).

-spec get_by_name(comment(), et_gen_server:ref()) -> mimetype().
get_by_name(Name, Database) ->
  case find_by_name(Name, Database) of
    {ok, Value} ->
      Value;
    error ->
      error({unknown_mimetype, Name})
  end.

-spec get_by_name(comment(), et_gen_server:ref(), mimetype()) -> mimetype().
get_by_name(Name, Database, Default) ->
  case find_by_name(Name, Database) of
    {ok, Value} ->
      Value;
    error ->
      Default
  end.

-spec find_by_type(type(), et_gen_server:ref()) -> {ok, mimetype()} | error.
find_by_type(Name, Database) ->
  mimedb_storage:search_by_type(Database, Name).

-spec get_by_type(type(), et_gen_server:ref()) -> mimetype().
get_by_type(Type, Database) ->
  case find_by_type(Type, Database) of
    {ok, Value} ->
      Value;
    error ->
      error({unknown_mimetype, Type})
  end.

-spec get_by_type(type(), et_gen_server:ref(), mimetype()) -> mimetype().
get_by_type(Type, Database, Default) ->
  case find_by_type(Type, Database) of
    {ok, Value} ->
      Value;
    error ->
      Default
  end.

-spec is_text(mimetype() | type()) -> boolean().
is_text(#{type := Type}) ->
  is_text(Type);
is_text(Type) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"text">> orelse is_child(Type, <<"text/plain">>).

-spec is_image(mimetype() | type()) -> boolean().
is_image(#{type := Type}) ->
  is_image(Type);
is_image(Type) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"image">>.

-spec is_audio(mimetype() | type()) -> boolean().
is_audio(#{type := Type}) ->
  is_audio(Type);
is_audio(Type) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"audio">>.

-spec is_video(mimetype() | type()) -> boolean().
is_video(#{type := Type}) ->
  is_video(Type);
is_video(Type) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"video">>.

-spec is_child(mimetype() | type(), mimetype() | type()) -> boolean().
is_child(#{type := Child}, #{type := Parent}) ->
  is_child(Child, Parent);
is_child(Child, Parent) when Child =:= Parent ->
  true;
is_child(Child, Parent) ->
  case mimedb_storage:search_by_type(Child) of
    {ok, #{parents := Parents}} ->
      lists:any(fun (P) -> is_child(P, Parent) end, Parents);
    {ok, _} ->
      false;
    error ->
      false
  end.

-spec equal(mimetype() | type(), mimetype() | type()) -> boolean().
equal(#{type := Type1}, #{type := Type2}) ->
  equal(Type1, Type2);
equal(Type1, Type2) ->
  Type1 =:= Type2.
