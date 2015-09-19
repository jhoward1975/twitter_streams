%%%----------------------------------------------------------------------------
%%% @author Jeremy Howard <fishoutawata@gmail.com>
%%% @copyright 2015
%%% @doc Twitter Public API Streaming Client.  This module connects to
%%%      Twitter's public streaming api at
%%%      https://stream.twitter.com/1.1/statuses/filter.json
%%% @end
%%%----------------------------------------------------------------------------
-module(twitter_streams_client).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(CONSUMER_KEY, "XXXXXXXXXX").
-define(CONSUMER_SECRET, "XXXXXXXXX").
-define(ACCESS_TOKEN, "XXXXXXXXX").
-define(ACCESS_TOKEN_SECRET, "XXXXXXXXX").

-record(state, {tweet}).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Starts the client
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @doc Stops the client
%%
%% @spec stop() -> ok
%% @end
%%-----------------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) ->
    Consumer = {
        ?CONSUMER_KEY,
        ?CONSUMER_SECRET,
        hmac_sha1
    },
    oauth:post("https://stream.twitter.com/1.1/statuses/filter.json",
        [{"track","cats,cheese"}], Consumer, ?ACCESS_TOKEN,
        ?ACCESS_TOKEN_SECRET, [{sync, false},{stream, self}]
    ),
    {ok, #state{tweet = []}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({http, {_RequestId, stream_start, _Headers}}, State) ->
    {noreply, State};
handle_info({http, {_RequestId, stream, BinBodyPart}}, State) ->
    case re:run(BinBodyPart, <<"\\r\\n$">>, [dotall]) of
      nomatch ->
        TweetPart = lists:append(State#state.tweet, [BinBodyPart]),
        {noreply, State#state{tweet = TweetPart}};
      {match, _} ->
        TweetPart = lists:append(State#state.tweet, [BinBodyPart]),
        Tweet = << <<B/bits>> || B <- TweetPart >>,
        io:format("~p~n", [Tweet]),
        {noreply, State#state{tweet = []}}
    end;
handle_info({http, {_RequestId, stream_end, _Headers}}, State) ->
    io:format("stream_end~n"),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
