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

-export([is_text/1, is_image/1, is_audio/1, is_video/1, has_same_family/2]).

-export_type([mimetype/0,
              extension/0, type/0, comment/0]).

-type mimetype() :: #{extensions => [extension()],
                      type => type(),
                      comment => comment()}.

-type extension() :: binary().
-type type() :: binary().
-type comment() :: binary().

-spec is_text(mimetype()) -> boolean().
is_text(#{type := Type}) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"text">>.

-spec is_image(mimetype()) -> boolean().
is_image(#{type := Type}) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"image">>.

-spec is_audio(mimetype()) -> boolean().
is_audio(#{type := Type}) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"audio">>.

-spec is_video(mimetype()) -> boolean().
is_video(#{type := Type}) ->
  [MediaType, _] = binary:split(Type, [<<$/>>]),
  MediaType =:= <<"video">>.

-spec has_same_family(Parrent :: mimetype(), mimetype()) -> boolean().
has_same_family(#{type := Type1}, #{type := Type2}) ->
  [MediaType1, _] = binary:split(Type1, [<<$/>>]),
  [MediaType2, _] = binary:split(Type2, [<<$/>>]),
  MediaType1 =:= MediaType2.
