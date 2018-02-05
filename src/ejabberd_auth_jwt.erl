%%%-------------------------------------------------------------------
%%% @author Steven Livingstone-Perez
%%% @copyright (C) 2015, Paramount Ventures
%%% @doc
%%% This module will verify an incoming JWT token and confirm the username claim is present in the request.
%%% - Ensure you store your symmetric key in the "secret" key/value pair ejabberd_auth_jwt.app setting.
%%% - HS256 support only at the moment.
%%% - The user_id is the only required claim and must match the username passed into eJabberd.
%%% - Optionally, pass a "Host" claim and it will check the eJabberd server name for equality.
%%% @end
%%% Created : 16. Dec 2015 17:39
%%%-------------------------------------------------------------------
-module(ejabberd_auth_jwt).
-author('Steven Livingstone-Perez').

-behaviour(ejabberd_auth).

-behaviour(ejabberd_config).

%% API
-export([start/1,
  store_type/1,
  plain_password_required/1,
  check_password/4,
  check_password/6,
  opt_type/1,
  get_password/2,
  get_password_s/2,
  remove_user/2,
  remove_user/3,
  set_password/3,
  try_register/3,
  user_exist/2,
  stop/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%% Implementation
-spec start(binary()) -> ok.
start(_Host) ->
  error_logger:info_msg("Started ejabberd_auth_jwt!"),
  ok.

% Needed so that the check_password/3 is called.
-spec plain_password_required(binary()) -> true.
plain_password_required(Server) ->
  store_type(Server) == scram.

% Needed so that the check_password/3 is called.
-spec store_type(binary()) -> external.
store_type(_) ->
  external.

-spec check_password(ejabberd:luser(), binary(), ejabberd:lserver(), binary()) -> boolean().
check_password(LUser,  _AuthzId, LServer, Token) ->
  error_logger:info_msg(io_lib:format("Unwrapping token ~s for User ~s at ~s", [Token, LUser, LServer])),

  % shared key data
  application:load(ejabberd_auth_jwt),
  {_, Key} = application:get_env(ejabberd_auth_jwt, secret),

  % Get the asserted user id
  ParsedClaims = ejwt:parse_jwt(Token, Key),

  case ParsedClaims of
    invalid ->
      error_logger:info_msg("Invalid Token for ~s", [LUser]),
      false;
    expired ->
      error_logger:info_msg("Token has expired for ~s", [LUser]),
      false;
    {AssertedClaims} ->
      Uid = proplists:get_value(<<"user_id">>, AssertedClaims),
      Host = proplists:get_value(<<"host">>, AssertedClaims),

      % if the claim passes a host then also check this
      case Host of
        undefined ->
          % Check that the authenticated
          if LUser == Uid ->
            true;
            true ->
              error_logger:info_msg("Invalid Claim for ~s for as ~s", [LUser, Uid]),
              false
          end;
        _ ->
          % Check that the authenticated
          if LUser == Uid andalso LServer == Host ->
            true;
            true ->
              error_logger:info_msg("Invalid Claim for ~s as ~s on ~s", [LUser, Uid, LServer]),
              false
          end
      end
  end.

stop(_Host) ->
  ok.

%opt_type() -> fun (V) -> V end;
opt_type(_) -> [].

%not used
-spec check_password(ejabberd:luser(), binary(), ejabberd:lserver(), binary(), binary(), fun()) -> boolean().
check_password(_LUser, _AuthzId, _LServer, _Password, _Digest, _DigestGen) ->
  erlang:error(not_implemented).

-spec remove_user_req(binary(), binary(), binary(), binary()) ->
  ok | not_exists | not_allowed | bad_request.
remove_user_req(_LUser, _LServer, _Password, _Method) ->
  erlang:error(not_implemented).

-spec get_password(ejabberd:luser(), ejabberd:lserver()) -> false | binary() |
{binary(), binary(), binary(), integer()}.
get_password(_LUser, _LServer) ->
  error.
  %erlang:error(not_implemented).

% in using jwt we will assume a valid user exists - could extend this by checking the backing database
-spec user_exists(binary(), binary()) -> boolean().
user_exists(_User, <<"">>) ->
  false;
user_exists(User, Server) ->
  true

-spec get_password_s(ejabberd:luser(), ejabberd:lserver()) -> binary().
get_password_s(_User, _Server) ->
  erlang:error(not_implemented).


-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok | not_exists | not_allowed | bad_request.
remove_user(LUser, LServer) ->
  remove_user_req(LUser, LServer, <<"">>, <<"remove_user">>).

-spec remove_user(ejabberd:luser(), ejabberd:lserver(), binary()) -> ok | not_exists | not_allowed | bad_request.
remove_user(_LUser, _LServer, _Password) ->
  erlang:error(not_implemented).

-spec set_password(ejabberd:luser(), ejabberd:lserver(), binary()) -> ok | {error, term()}.
set_password(_LUser, _LServer, _Password) ->
  erlang:error(not_implemented).

-spec try_register(ejabberd:luser(), ejabberd:lserver(), binary()) -> {atomic, ok | exists} | {error, term()}.
try_register(_LUser, _LServer, _Password) ->
  erlang:error(not_implemented).