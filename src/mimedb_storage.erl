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

-export([start_link/1,
         init/1, terminate/2,
         handle_continue/2, handle_call/3, handle_cast/2]).

start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

init([Options]) ->
  Filename = maps:get(filename, Options, <<>>),
  EtsFlags = [ordered_set, named_table, {read_concurrency, true}],
  Tab1 = ets:new(mimedb_data, EtsFlags),
  Tab2 = ets:new(mimedb_extension_index, EtsFlags),
  Tab3 = ets:new(mimedb_name_index, EtsFlags),
  State = {Tab1, Tab2, Tab3},
  {ok, State, {continue, {init, Filename}}}.

terminate(_Reason, {Tab1, Tab2, Tab3}) ->
  ets:delete(Tab1),
  ets:delete(Tab2),
  ets:delete(Tab3),
  ok.

handle_continue({init, Filename}, State) ->
  load_file(Filename, State);
handle_continue(Msg, State) ->
  ?LOG_WARNING("unhandled call ~p", [Msg]),
  {noreply, State}.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

load_file(<<>>, State) ->
  case mimedb:open() of
    {ok, Data} ->
      populate_db(Data, State);
    {error, Reason} ->
      {stop, Reason, State}
  end;
load_file(Filename, State) ->
  case mimedb:open(Filename) of
    {ok, Data} ->
      populate_db(Data, State);
    {error, Reason} ->
      {stop, Reason, State}
  end.

populate_db([], State) ->
  {noreply, State};
populate_db([H | T], {Tab1, Tab2, Tab3} = State) ->
  Key = maps:get(type, H),
  ets:insert(Tab1, {Key, H}),
  ets:insert(Tab2, {maps:get(comment, H), Key}),
  lists:foreach(fun (Ext) ->
                    ets:insert(Tab3, {Ext, Key})
                end, maps:get(extensions, H, [])),
  populate_db(T, State).
