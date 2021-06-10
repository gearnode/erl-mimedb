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

-export([search_by_type/2, search_by_name/2]).

-export([start_link/2,
         init/1, terminate/2,
         handle_continue/2, handle_call/3, handle_cast/2]).

-type state() :: #{options := term(),
                   store := {ets:tab(), ets:tab(), ets:tab()}}.

-spec search_by_type(et_gen_server:ref(), mimedb:type()) ->
        {ok, mimedb:mimetype()} | error.
search_by_type(Ref, Type) ->
  gen_server:call(Ref, {by_type, Type}, infinity).

-spec search_by_name(et_gen_server:ref(), mimedb:comment()) ->
        {ok, mimedb:mimetype()} | error.
search_by_name(Ref, Name) ->
  gen_server:call(Ref, {by_name, Name}, infinity).

start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  Flags = [private, ordered_set, {read_concurrency, true}],
  T1 = ets:new(mimedb, Flags),
  T2 = ets:new(mimedb_comment_index, Flags),
  T3 = ets:new(mimedb_extension_index, Flags),
  {ok, #{store => {T1, T2, T3}, options => Options}, {continue, init}}.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_, #{store := {T1, T2, T3}}) ->
  ets:delete(T1),
  ets:delete(T2),
  ets:delete(T3),
  ok.

-spec handle_continue(term(), state()) ->
        et_gen_server:handle_continue_ret(state()).
handle_continue(init, #{options := Options} = State) ->
  Filename = maps:get(filename, Options, <<>>),
  load_file(Filename, State);

handle_continue(Msg, State) ->
  ?LOG_WARNING("unhandled call ~p", [Msg]),
  {noreply, State}.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call({by_name, Name}, _, #{store := {T1, T2, _}} = State) ->
  case ets:lookup(T1, Name) of
    [{_, Key}] ->
      [{_, Value}] = ets:lookup(T2, Key),
      {reply, Value, State};
    [] ->
      {reply, error, State}
  end;

handle_call({by_type, Type}, _, #{store := {T1, _, _}} = State) ->
  case ets:lookup(T1, Type) of
    [{_, Value}] ->
      {reply, {ok, Value}, State};
    [] ->
      {reply, error, State}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec load_file(binary(), state()) ->
        et_gen_server:handle_continue_ret(state()).
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

-spec populate_db([mimedb:mimetype()], state()) ->
        et_gen_server:handle_continue_ret(state()).
populate_db([], State) ->
  {noreply, State};
populate_db([H | T], #{store := {T1, T2, T3}} = State) ->
  Key = maps:get(type, H),
  ets:insert(T1, {Key, H}),
  ets:insert(T2, {maps:get(comment, H), Key}),
  lists:foreach(
    fun (Ext) -> ets:insert(T3, {Ext, Key}) end,
    maps:get(extensions, H, [])),
  populate_db(T, State).
