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

-module(mimedb_storage).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-define(MIMEDB, mimedb_data).
-define(MIMEDB_EXTENSION_INDEX, mimedb_extension_index).
-define(MIMEDB_COMMENT_INDEX, mimedb_comment_index).

-export([search_by_type/1, search_by_name/1]).

-export([reload/1]).

-export([start_link/1,
         init/1, terminate/2,
         handle_continue/2, handle_call/3, handle_cast/2]).

reload(Ref) ->
  gen_server:cast(Ref, reload).

search_by_type(Type) ->
  case ets:lookup(?MIMEDB, Type) of
    [{_, Value}] ->
      {ok, Value};
    [] ->
      error
  end.

search_by_name(Name) ->
  case ets:lookup(?MIMEDB_COMMENT_INDEX, Name) of
    [{_, Key}] ->
      [{_, Value}] = ets:lookup(?MIMEDB, Key),
      Value;
    [] ->
      error
  end.

start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

init([Options]) ->
  Flags = [protected, ordered_set, named_table, {read_concurrency, true}],
  ets:new(?MIMEDB, Flags),
  ets:new(?MIMEDB_COMMENT_INDEX, Flags),
  ets:new(?MIMEDB_EXTENSION_INDEX, Flags),
  {ok, #{options => Options}, {continue, init}}.

terminate(_, _) ->
  ets:delete(?MIMEDB),
  ets:delete(?MIMEDB_COMMENT_INDEX),
  ets:delete(?MIMEDB_EXTENSION_INDEX),
  ok.

handle_continue(init, #{options := Options} = State) ->
  Filename = maps:get(filename, Options, <<>>),
  load_file(Filename, State);

handle_continue(Msg, State) ->
  ?LOG_WARNING("unhandled call ~p", [Msg]),
  {noreply, State}.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

handle_cast(reload, #{options := Options} = State) ->
  Filename = maps:get(filename, Options, <<>>),
  load_file(Filename, State);

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

load_file(<<>>, State) ->
  case mimedb_parser:open() of
    {ok, Data} ->
      populate_db(Data, State);
    {error, Reason} ->
      {stop, Reason, State}
  end;
load_file(Filename, State) ->
  case mimedb_parser:open(Filename) of
    {ok, Data} ->
      populate_db(Data, State);
    {error, Reason} ->
      {stop, Reason, State}
  end.

populate_db([], State) ->
  {noreply, State};
populate_db([H | T], State) ->
  Key = maps:get(type, H),
  ets:insert(?MIMEDB, {Key, H}),
  ets:insert(?MIMEDB_COMMENT_INDEX, {maps:get(comment, H), Key}),
  lists:foreach(
    fun (Ext) -> ets:insert(?MIMEDB_EXTENSION_INDEX, {Ext, Key}) end,
    maps:get(extensions, H, [])),
  populate_db(T, State).
