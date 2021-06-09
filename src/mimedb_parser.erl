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

-module(mimedb_parser).

-include_lib("xmerl/include/xmerl.hrl").

-export([locate_db/0, open/0, open/1]).

-type error_reason() ::
        mimedb_not_found
      | {io, file:posix() | badarg | terminated | system_limit}
      | {decode, term()}.

-spec locate_db() -> {ok, file:filename()} | error.
locate_db() ->
  F = fun (Filename) -> filelib:is_regular(Filename) end,
  case lists:search(F, possible_db_paths()) of
    {value, Filename} ->
      {ok, Filename};
    false ->
      error
  end.

-spec open() -> {ok, [mimedb:mimetype()]} | {error, error_reason()}.
open() ->
  case locate_db() of
    {ok, Filename} ->
      open(Filename);
    error ->
      {error, mimedb_not_found}
  end.

-spec open(file:filename()) ->
        {ok, [mimedb:mimetype()]} | {error, error_reason()}.
open(Filename) ->
  case file:read_file(Filename) of
    {ok, File} ->
      decode_file(binary_to_list(File));
    {error, Reason} ->
      {error, {io, Reason}}
  end.

-spec decode_file(string()) ->
        {ok, [mimedb:mimetype()]} | {error, error_reason()}.
decode_file(File) ->
  try 
    {Document, _} = xmerl_scan:string(File),
    {ok, parse_mime_info(Document)}
  catch
    exit:Reason ->
      {error, {decode, Reason}}
  end.

-spec parse_mime_info(xmerl_scan:xmlElement()) -> [mimedb:mimetype()].
parse_mime_info(#xmlElement{name = 'mime-info', content = Elements}) ->
  parse_mime_types(Elements, []).

-spec parse_mime_types([xmerl_scan:xmlElement()], [map()]) ->
        [mimedb:mimetype()].
parse_mime_types([], Acc) ->
  lists:reverse(Acc);
parse_mime_types([#xmlElement{name = 'mime-type'} = Element | T], Acc) ->
  MimeType = parse_mime_type(Element),
  parse_mime_types(T, [MimeType | Acc]);
parse_mime_types([_ | T], Acc) ->
  parse_mime_types(T, Acc).

-spec parse_mime_type(xmerl_scan:xmlElement()) -> mimedb:mimetype().
parse_mime_type(Element) ->
  Type = find_type(Element),
  construct_mime_type(Element#xmlElement.content, #{type => Type}).

-spec construct_mime_type([xmerl_scan:xmlElement()], map()) ->
        mimedb:mimetype().
construct_mime_type([], Acc) ->
  Acc;
construct_mime_type([#xmlElement{name = glob,
                                 attributes = Attributes} | T], Acc) ->
  Extensions = maps:get(extensions, Acc, []),
  F = fun (#xmlAttribute{name = pattern}) -> true;
          (_) -> false
      end,
  case lists:search(F, Attributes) of
    {value, Attribute} ->
      Extension = iolist_to_binary(Attribute#xmlAttribute.value),
      construct_mime_type(T, Acc#{extensions => [Extension | Extensions]});
    false ->
      construct_mime_type(T, Acc)
  end;
construct_mime_type([#xmlElement{name = comment,
                                 attributes = [],
                                 content = [Content]} | T], Acc) ->
  Name = iolist_to_binary(Content#xmlText.value),
  construct_mime_type(T, Acc#{comment => Name});
construct_mime_type([_ | T], Acc) ->
  construct_mime_type(T, Acc).

find_type(#xmlElement{attributes = Attributes}) ->
  F = fun (#xmlAttribute{name = type}) -> true;
          (_) -> false
      end,
  case lists:search(F, Attributes) of
    {value, Attribute} ->
      iolist_to_binary(Attribute#xmlAttribute.value);
    false ->
      <<>>
  end.

-spec possible_db_paths() -> [file:filename()].
possible_db_paths() ->
  ["/usr/share/mime/packages/freedesktop.org.xml", %% Linux
   "/opt/homebrew/share/mime/packages/freedesktop.org.xml"]. %% Homebrew MacOS
