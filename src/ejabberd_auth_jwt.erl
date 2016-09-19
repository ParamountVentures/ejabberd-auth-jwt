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
  store_type/0,
  plain_password_required/0,
  check_password/4,
  check_password/6,
  is_user_exists/2,
  opt_type/1,
  dirty_get_registered_users/0,
  get_password/2,
  get_password_s/2,
  get_vh_registered_users/1,
  get_vh_registered_users/2,
  get_vh_registered_users_number/1,
  get_vh_registered_users_number/2,
  remove_user/2,
  remove_user/3,
  set_password/3,
  try_register/3,
  stop/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%% Implementation
-spec start(binary()) -> ok.
start(_Host) ->
  error_logger:info_msg("ejabberd_auth_jwt"),
  ok.

% Needed so that the check_password/3 is called.
plain_password_required() -> true.

% Needed so that the check_password/3 is called.
store_type() -> external.

is_user_exists(User, _Server) ->
  error_logger:info_msg(User),
  true.

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

-spec dirty_get_registered_users() -> [].
dirty_get_registered_users() ->
  [].

-spec get_password(ejabberd:luser(), ejabberd:lserver()) -> false | binary() |
{binary(), binary(), binary(), integer()}.
get_password(_LUser, _LServer) ->
  erlang:error(not_implemented).


-spec get_password_s(ejabberd:luser(), ejabberd:lserver()) -> binary().
get_password_s(_User, _Server) ->
  erlang:error(not_implemented).

-spec get_vh_registered_users(ejabberd:lserver()) -> [].
get_vh_registered_users(_Server) ->
  [].

-spec get_vh_registered_users(ejabberd:lserver(), list()) -> [].
get_vh_registered_users(_Server, _Opts) ->
  [].

-spec get_vh_registered_users_number(binary()) -> 0.
get_vh_registered_users_number(_Server) ->
  0.

-spec get_vh_registered_users_number(ejabberd:lserver(), list()) -> 0.
get_vh_registered_users_number(_Server, _Opts) ->
  0.


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