Dynamic loader
==============

dloader is used to load drivers and nifs using the inet loader.
This can be used when developing embedded code or when runnig
slave nodes that are using drivers or nifs.

# Loading drivers

Code that are loading driver explicitly should be changed from

    erl_ddll:load(code:priv_dir(my_app), "foo_drv")

to 

    dloader:load_driver(code:priv_dir(my_app), "foo_drv")

dloader will load driver as normal if erl\_prim\_loader is loading from
files (default), but will cache a local copy to load from otherwise.

# Loading nifs

Since the call that load nifs must be called from the module
that "imports" them, the call sequence is slightly different
when loading nifs.

Existing code looks like

    Nif = filename:join(code:priv_dir(my_app), "bar_nif"),
    erlang:load_nif(Nif, 0).

is changed to

    {ok,Nif} = dloader:cache_nif(code:priv_dir(my_app), "bar_nif"),
    erlang:load_nif(Nif, 0).

# Cross compiled drivers and nifs

Cross compiled drivers and nifs may be stored in sub directories
under the path used to locate the driver or nif. dloader will
check for the driver or nif firstly in the path with the system_architecuture
name appended to name that is filename:join(Path, erlang:system\_info(system\_architecture)
before checking the path.

## Starting the boot server

    erl -kernel start_boot_server true boot_server_slaves '[{10,0,0,1}]'
        optional [ -sname master -setcookie my_cookie ]
        
## Loading code from boot server

    erl -loader inet -id slave -hosts 10.0.0.2 
        optional [ -sname slave -setcookie my_cookie ]

slave node in the above example is located at ip 10.0.0.1 and the server is located at ip 10.0.0.2

