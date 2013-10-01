%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Loader of nifs and loadble drivers
%%% @end
%%% Created : 22 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(dloader).


-export([cache_driver/2, load_driver/2]).
-export([cache_nif/2]).

-include_lib("kernel/include/file.hrl").

-compile(export_all).

%% @doc 
%%   Cache and load a driver
%% @end
load_driver(Path, Driver) ->
    case cache_driver(Path, Driver) of
	{ok, Path1} ->
	    erl_ddll:load_driver(Path1, Driver);
	Error ->
	    Error
    end.

%% @doc 
%%   Cache the the driver locally if needed
%% @end

cache_driver(Path, Driver) ->
    case load_locally() of
	true ->
	    {ok,Path};
	false ->
	    load_path(Path, name_ext(Driver))
    end.

%% @doc
%%   Cache the nif file locally
%% @end

cache_nif(Path, Nif) ->
    case load_locally() of
	true ->
	    {ok, filename:join(Path, Nif)};
	false ->
	    NifExt = name_ext(Nif),
	    case load_path(Path, NifExt) of
		{ok,Path1} ->
		    {ok, filename:join(Path1, Nif)};
		Error ->
		    Error
	    end
    end.

%% fixme: check that cache is a directory
%% must probably be located under /tmp/<id>/..
%%
cache_dir() ->
    case os:getenv("ERL_DRIVER_CACHE") of
	false -> "/tmp";
	"" -> "/tmp";
	Path -> Path
    end.


%% return name with the correct driver/nif extension
name_ext(Name) ->
    Ext = case os:type() of
	      {unix,_} -> ".so";
	      {win32,_} -> ".dll"
	  end,
    case filename:extension(Name) of
	"" -> Name++Ext;
	Ext -> Name
    end.

%%
%% Check if loading of nif/driver can be done locally
%% 
load_locally() ->
    case lists:keyfind(loader, 1, init:get_arguments()) of
	{loader,["inet"]} -> false;
	{loader,["efile"]} -> true;
	_ -> true
    end.

%%
%% Load file and cache it if needed, otherwise
%% return the cached copy
%%

load_path(Path, File) ->
    case find_file([make_system_path(Path), Path], File) of
	{ok,{Path1,FI}} ->
	    CacheDir = cache_dir(),
	    case is_modified(File, FI, CacheDir) of
		false ->
		    %% io:format("dloader: using cached copy of ~s\n", 
		    %% [filename:join(Path1,File)]),
		    {ok, CacheDir};
		true ->
		    RemoteFile = filename:join(Path1,File),
		    case erl_prim_loader:get_file(RemoteFile) of
			{ok,Data,_RPath} ->
			    %% io:format("dloader: load & cache ~s\n",[_RPath]),
			    ok = write_cache_entry(File, FI, CacheDir, Data),
			    {ok, CacheDir};
			Error ->
			    Error
		    end
	    end;
	Error ->
	    Error
    end.

%% write file entry in cache
-ifdef(_not_used__).
write_cache_entry(File, Data) ->
    write_cache_entry(File, cache_dir(), Data).

write_cache_entry(File, CacheDir, Data) ->
    {ok,FI} = erl_prim_loader:read_file_info(File),
    write_cache_entry(File, FI, CacheDir, Data).
-endif.

write_cache_entry(File, FI, CacheDir, Data) ->
    Base = filename:basename(File),
    CacheFile = filename:join(CacheDir,Base),
    ok = prim_file:write_file(CacheFile,Data),
    {ok,CI} = prim_file:read_file_info(CacheFile),
    %% set mtime to match the remote file
    CI1 = CI#file_info { mtime = FI#file_info.mtime },
    prim_file:write_file_info(CacheFile, CI1).



%% read the remote file modification
-ifdef(_not_used__).
is_modified(File) ->
    is_modified(File, cache_dir()).

is_modified(File, CacheDir) ->
    {ok,FI} = erl_prim_loader:read_file_info(File),
    is_modified(File, FI, CacheDir).
-endif.

is_modified(File, FI, CacheDir) ->
    Base = filename:basename(File),
    case file:read_file_info(filename:join(CacheDir,Base)) of
	{ok,CI} ->
	    FI#file_info.mtime > CI#file_info.mtime;
	_ ->
	    true
    end.


find_file([Path|Ps], Name) ->
    File = filename:join(Path,Name),
    case erl_prim_loader:read_file_info(File) of
	{ok,FI} ->
	    {ok,{Path,FI}};
	error ->
	    find_file(Ps, Name)
    end;
find_file([], _NameExt) ->
    {error,enoent}.


make_system_path(Path) ->
    filename:join(Path,erlang:system_info(system_architecture)).
