-module(egeoip_shared_state).

-behaviour(gen_server).

%% gen_server based API
-export([start_link/0,
         get_file/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-record(data_file, {
        changed,
        data :: binary()
}).
-record(state, {files :: dict()}).

srv_name() ->
    egeoip_shared_state.

get_file(Path) ->
    gen_server:call(srv_name(), {get_file, Path}).

start_link() ->
    gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

%% gen_server callbacks

init([]) ->
    State = #state{
        files=dict:new()
    },
    {ok, State}.

handle_call({get_file, Path}, _From, State=#state{files=Files}) ->
    Changed = filelib:last_modified(Path),
    case Changed of
        0 ->
            {reply, {error, not_exist}, State};
        _ ->
            case dict:find(Path, Files) of
                {ok, #data_file{changed=Changed, data=Data}} ->
                    {reply, {ok, Data}, State};
                %% File was changed or not yet loaded.
                _ ->
                    case read_file(Path) of
                        {error, Reason} ->
                            {reply, {error, Reason}, State};
                        {ok, Data} ->
                            Rec = #data_file{changed=Changed,
                                             data=Data},
                            State1 = State#state{files=dict:store(Path, Rec, Files)},
                            {reply, {ok, Data}, State1}
                    end
            end
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.


read_file(Path) ->
    case file:read_file(Path) of
        {ok, Raw} ->
            case filename:extension(Path) of
                ".gz" ->
                    {ok, zlib:gunzip(Raw)};
                _ ->
                    {ok, Raw}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

