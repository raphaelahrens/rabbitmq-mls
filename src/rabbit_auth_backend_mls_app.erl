%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_auth_backend_mls_app).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

start(_Type, _StartArgs) ->
    logger:info("MLS START AUTHN PLUGIN"),
    supervisor:start_link({local,?MODULE},?MODULE,[]).

stop(_State) ->
    ok.

%%----------------------------------------------------------------------------

init([]) ->
    logger:info("MLS INIT AUTHN PLUGIN"),
    {ok, {{one_for_one,3,10},[]}}.
