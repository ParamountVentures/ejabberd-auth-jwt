# JWT authentication module
An eJabberd Authentication plugin for JWT verification written in Erlang.

## Overview

This module will verify an incoming JWT token and confirm the username claim is present in the request.

## Setup
- Install [eJabberd](https://github.com/processone/ejabberd)
- git clone [this repo](https://github.com/ParamountVentures/ejabberd-auth-jwt) 
- Add a symmetric key (any random string) to src/ejabberd_auth_jwt.app
- Compile the main Erlang file (include the path to the eJabberd includes) **erlc -I /some/path/ejabberd/include ejabberd_auth_jwt.erl**
- Copy the output .beam file to the /some/path/ejabberd/ebin folder
- Copy the output .app file to the /some/path/ejabberd/ebin folder
- Copy all of the .beam files from the deps subfolders to the /some/path/ejabberd/ebin folder

### How to enable

Replace default `auth_method` option in
`ejabberd.yml` with `auth_method: jwt`.


Example configuration:
```
auth_method: jwt

```

- Restart eJabberd **ejabberdctl restart**
- Check for errors in the log file **tail -f /var/log/ejabberd/ejabberd.log**
 
## Demo
- Run eJabberd
- Download Adium or some other XMPP client
- Go to http://jwtbuilder.jamiekurtz.com/ and use your secret key to generate an HS256 claim, ensuring you set a "user_id" claim type.
- Copy the signed JWT on that web page.
- Open Adium and create an account on your XMPP server, with the username equal to the "user_id" claim above and the password the JWT you generated in the previous step. 
- Log in successfully :-)


Please let me know of any issues - this is my first Erlang project so be gentle :-)