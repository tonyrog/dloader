Dynamic loader
==============

dloader is used to load drivers and nif using the inet loader.
This can be used when developing embedded code or is runnig
slave nodes that are using driver or nifs.

# Loading drivers

code that are loading driver expliclty should be changed 

    erl_ddll:load(code:priv_dir(gpio), "gpio_drv")

to 

    dloader:load_driver(code:priv_dir(gpio), "gpio_drv")

dloader will load as normal if erl_prim_loader is loading from
files. But will use a cached copy if possible or read and cache
a copy otherwise.

# Loading nifs

Since the call that load a nif must be called from the module
that "imports" the functions the call sequence is slightly different
for nifs that for dirvers.

Existing code

    Nif = filename:join(code:priv_dir(cl), "cl_nif"),
    erlang:load_nif(Nif, 0).

Must be changed to

   {ok,Nif} = dloader:cache_nif(code:priv_dir(cl), "cl_nif"),
   erlang:load_nif(Nif, 0).

# Cross compiled drivers and nifs

Cross compiled nifs and drivers should be stored in sub directories
under the path used to locate the driver or nif. This means that
if for example code:priv_dir(cl) is passed to dloader:cache_nif then 
the directory code:priv_dir(cl) / erlang:system_info(system_architecture) 
is searched before code:priv_dir(cl) in the boot server.

## Loading code from boot server

    erl -loader inet -id slave -hosts 10.0.0.2 
        optional [ -sname slave -setcookie abcd ]

## Starting the boot server

    erl -kernel start_boot_server true boot_server_slaves '[{10,0,0,1}]'
        optional -sname master -setcookie abcd
