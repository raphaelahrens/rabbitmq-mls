%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_auth_backend_mls).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("rabbit_auth_backend_mls.hrl").

-import(rabbit_basic, [header/2]).
-import(erl_cbor, [decode/1]).

-behaviour(rabbit_channel_interceptor).
-export([description/0, intercept/3, applies_to/0, init/1]).
%-behaviour(rabbit_authn_backend).
-behaviour(rabbit_authz_backend).
-export([user_login_authorization/2,
         check_vhost_access/3, check_resource_access/4, check_topic_access/4,
         state_can_expire/0]).

-rabbit_boot_step({?MODULE,
                   [{description, "mls interceptor"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"mls interceptor">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"mls interceptor">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

init(_Ch) ->
    logger:info("MLS intercepter init"),
    undefined.

description() ->
    [{description,
      <<"Checks mls of a messages as they enter RabbitMQ">>}].

intercept(#'basic.publish'{} = Method, Content, IState) ->
    logger:info("MLS intercept basic publish"),
    logger:info("MLS              Method: ~p", [Method]),
    logger:info("MLS              IState: ~p", [IState]),
    {content, A, B, C, D, [Payload]} = Content,
    logger:info("MLS              A: ~p", [A]),
    logger:info("MLS              B: ~p", [B]),
    logger:info("MLS              C: ~p", [C]),
    logger:info("MLS              D: ~p", [D]),
    {
        ok,
        #{
            <<"data">>:= _Payload2,
            <<"ad">>:= AdditionalData,
            <<"sign">>:=_Signature
        },
        <<>>
      } = erl_cbor:decode(Payload),
    {
        ok,
        #{<<"label">>:=Label},
        <<>>
    }= erl_cbor:decode(AdditionalData),
    logger:info("MLS            Label: ~p",[Label]),
    {Method, Content};

intercept(Method, Content, VHost) ->
    logger:info("MLS intercept"),
    logger:info("MLS              Method: ~p", [Method]),
    logger:info("MLS              Content: ~p", [Content]),
    logger:info("MLS              VHost: ~p", [VHost]),
    {Method, Content}.

applies_to() -> 
    ['basic.publish', 'queue.bind', 'queue.unbind', 'exchange.bind', 'exchange.unbind'].

user_login_authorization(Username,AuthProps) ->
    logger:info("MLS User Login AuthZ Username: ~p", [Username]),
    logger:info("MLS      AuthProps: ~p", [AuthProps]),
    {ok, {}, [test]}.

check_vhost_access(#auth_user{username = Username, tags = Tags}, VHost, AuthzData) ->
    logger:info("MLS Vhost Access Username: ~p", [Username]),
    logger:info("MLS              Tags: ~p", [Tags]),
    logger:info("MLS              Vhost: ~p", [VHost]),
    logger:info("MLS              AuthzData: ~p", [AuthzData]),
    true.

check_resource_access(#auth_user{
                         username = Username,
                         impl = Impl,
                         tags = Tags
                      },
                      #resource{virtual_host = VHost, kind = Type, name = Name},
                      Permission,
                      AuthzContext) ->
    logger:info("MLS resource access"),
    logger:info("MLS              Username: ~p", [Username]),
    logger:info("MLS              Impl: ~p", [Impl]),
    logger:info("MLS              Tags: ~p", [Tags]),
    logger:info("MLS              VHost: ~p", [VHost]),
    logger:info("MLS              Type: ~p", [Type]),
    logger:info("MLS              Name: ~p", [Name]),
    logger:info("MLS              Permission: ~p", [Permission]),
    logger:info("MLS              AuthzContext: ~p", [AuthzContext]),
    true.

check_topic_access(#auth_user{username = Username, tags = Tags},
                   #resource{virtual_host = VHost, kind = topic = Type, name = Name},
                   Permission,
                   AuthzContext) ->
    logger:info("MLS topic access"),
    logger:info("MLS              Username: ~p", [Username]),
    logger:info("MLS              Tags: ~p", [Tags]),
    logger:info("MLS              VHost: ~p", [VHost]),
    logger:info("MLS              Type: ~p", [Type]),
    logger:info("MLS              Name: ~p", [Name]),
    logger:info("MLS              Permission: ~p", [Permission]),
    logger:info("MLS              AuthzContext: ~p", [AuthzContext]),
    true.

state_can_expire() -> false.
