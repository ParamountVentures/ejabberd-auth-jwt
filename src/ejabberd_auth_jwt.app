{application, ejabberd_auth_jwt,
  [
    {id, "My Id"},
    {description, ""},
    {vsn, "0.0.1"},
  {registered, []},
  {applications, [
    kernel,
    stdlib,
    ejwt
    ]},
  {mod, { ejabberd_auth_jwt_app, []}},
  {env, [
    {secret, "53F61451CAD6231FDCF6859C6D5B88C1EBD5DC38B9F7EBD990FADD4EB8EB9063"}
  ]}
]}.