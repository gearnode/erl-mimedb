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

-export([locate_db/0]).

-export_type([mimetype/0,
              extension/0, type/0, media_type/0, subtype/0, comment/0]).

-type mimetype() :: #{extensions => [extension()],
                      type => type(),
                      media_type => media_type(),
                      subtype => subtype(),
                      comment => comment()}.

-type extension() :: binary().
-type type() :: binary().
-type media_type() :: binary().
-type subtype() :: binary().
-type comment() :: #{binary() => binary()}.

-spec locate_db() -> {ok, file:filename()} | error.
locate_db() ->
  F = fun (Filename) -> filelib:is_regular(Filename) end,
  case lists:search(F, possible_db_paths()) of
    {value, Filename} ->
      {ok, Filename};
    false ->
      error
  end.

-spec possible_db_paths() -> [file:filename()].
possible_db_paths() ->
  ["/usr/share/mime/packages/freedesktop.org.xml", %% Linux
   "/opt/homebrew/share/mime/packages/freedesktop.org.xml"]. %% Homebrew MacOS
