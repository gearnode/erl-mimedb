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

-export([search_by_type/2, search_by_name/2, search_by_extension/2]).

-export([start_link/2,
         init/1, terminate/2,
         handle_continue/2, handle_call/3, handle_cast/2]).

-type state() :: #{options := term(),
                   db := sqlite_database:ref()}.

-spec search_by_type(et_gen_server:ref(), mimedb:type()) ->
        {ok, mimedb:mimetype()} | error.
search_by_type(Ref, Type) ->
  gen_server:call(Ref, {by_type, Type}, infinity).

-spec search_by_name(et_gen_server:ref(), mimedb:comment()) ->
        {ok, mimedb:mimetype()} | error.
search_by_name(Ref, Name) ->
  gen_server:call(Ref, {by_name, Name}, infinity).

-spec search_by_extension(et_gen_server:ref(), mimedb:extension()) ->
        {ok, mimedb:mimetype()} | error.
search_by_extension(Ref, Extension) ->
  gen_server:call(Ref, {by_extension, Extension}, infinity).

start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  try
    Ref = open_database(),
    State = #{db => Ref, options => Options},
    update_schema(State),
    {ok, State, {continue, import}}
  catch
    throw:{error, Reason} ->
      {stop, Reason}
  end.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_, #{db := Ref}) ->
  sqlite:close(Ref),
  ok.

-spec handle_continue(term(), state()) ->
        et_gen_server:handle_continue_ret(state()).
handle_continue(import, #{options := Options} = State) ->
  Filename = maps:get(filename, Options, <<>>),
  load_file(Filename, State);

handle_continue(Msg, State) ->
  ?LOG_WARNING("unhandled call ~p", [Msg]),
  {noreply, State}.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call({by_name, Name}, _, State) ->
  try
    Query = ["SELECT mt.type, parents, comment, GROUP_CONCAT(extension,';')"
             " FROM mime_types mt"
             " LEFT JOIN extensions e ON mt.type = e.type"
             " WHERE comment = ?1",
             " GROUP BY mt.type",
             " LIMIT 1"],
    case query(Query, [{text, Name}], State) of
      [] ->
        {reply, error, State};
      [Row] ->
        {reply, {ok, decode_row(Row)}, State}
    end
  catch
    throw:{error, Reason} ->
      error(Reason)
  end;

handle_call({by_type, Type}, _, State) ->
  try
    Query = ["SELECT mt.type, parents, comment, GROUP_CONCAT(extension,';')"
             " FROM mime_types mt"
             " LEFT JOIN extensions e ON mt.type = e.type"
             " WHERE mt.type = ?1",
             " GROUP BY mt.type",
             " LIMIT 1"],
    case query(Query, [{text, Type}], State) of
      [] ->
        {reply, error, State};
      [Row] ->
        {reply, {ok, decode_row(Row)}, State}
    end
  catch
    throw:{error, Reason} ->
      error(Reason)
  end;

handle_call({by_extension, Extension}, _, State) ->
  try
    Query = ["SELECT mt.type, parents, comment, GROUP_CONCAT(extension,';')"
             " FROM mime_types mt"
             " LEFT JOIN extensions e ON mt.type = e.type"
             " WHERE e.extension = ?1",
             " GROUP BY mt.type",
             " LIMIT 1"],
    case query(Query, [{text, Extension}], State) of
      [] ->
        {reply, error, State};
      [Row] ->
        {reply, {ok, decode_row(Row)}, State}
    end
  catch
    throw:{error, Reason} ->
      error(Reason)
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec open_database() -> sqlite_database:ref().
open_database() ->
  case sqlite:open(<<":memory:">>, #{}) of
    {ok, Ref} ->
      Ref;
    {error, Reason} ->
      throw({error, {open, Reason}})
  end.

-spec update_schema(state()) -> ok.
update_schema(State) ->
  Queries =
    [["PRAGMA foreign_keys = ON"],
     ["CREATE TABLE mime_types",
      "  (type TEXT PRIMARY KEY,",
      "   parents TEXT,",
      "   comment TEXT NOT NULL)"],
     ["CREATE TABLE extensions",
      "  (extension TEXT NOT NULL,",
      "   type TEXT,",
      "   FOREIGN KEY(type) REFERENCES mime_types(type))"],
     ["CREATE INDEX extensions_extension_idx",
      "  ON extensions(extension)"]],
  lists:foreach(fun (Query) -> query(Query, [], State) end, Queries).

-spec query(sqlite:query(), [sqlite:parameter()], state()) -> [sqlite:row()].
query(Query, Parameters, #{db := Ref}) ->
  case sqlite:query(Ref, Query, Parameters) of
    {ok, Rows} ->
      Rows;
    {error, Reason} ->
      throw({error, {query, Reason, Query}})
  end.

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
populate_db([H | T], State) ->
  try
    insert(H, State),
    populate_db(T, State)
  catch
    throw:{error, Reason} ->
      error(Reason)
  end.

insert(MimeType, State) ->
  Query1 = ["INSERT INTO mime_types (type, parents, comment)",
           " VALUES (?1, ?2, ?3)"],
  Parameters1 =
    [{text, maps:get(type, MimeType)},
     {text, iolist_to_binary(lists:join($;, maps:get(parents, MimeType, [])))},
     {text, maps:get(comment, MimeType)}],
  query(Query1, Parameters1, State),

  Query2 = ["INSERT INTO extensions (extension, type)",
            " VALUES (?1, ?2)"],

  lists:foreach(fun (Extension) ->
                    Parameters2 =
                      [{text, Extension},
                       {text, maps:get(type, MimeType)}],
                    query(Query2, Parameters2, State)
                end, maps:get(extensions, MimeType, [])).

decode_row([Type, Parents, Comment, Extensions]) ->
  #{type => Type,
    parents => binary:split(Parents, <<$;>>, [global]),
    comment => Comment,
    extensions => binary:split(Extensions, <<$;>>, [global])}.
