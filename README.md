# JWT authentication module
An eJabberd Authentication plugin for JWT verification written in Erlang.

# Note
As of ejabberd 16.03 authentication modules must be compiled with the ejabberd source rather than included as beam files.

## Overview

This module will verify an incoming JWT token and confirm the username claim is present in the request.

## Setup
- Add the ejabberd_auth_jwt.erl file to the src folder of the ejabberd repository
- Compile eJabberd
- cp ejabberd_auth_jwt.app /lib/ejabberd-16.09/ebin/
- Compile each of the deps libraries.
- cp package/base64url.beam /lib/ejabberd-16.09/ebin/
- cp package/ej.beam /lib/ejabberd-16.09/ebin/
- cp package/ejabberd_auth_jwt.app /lib/ejabberd-16.09/ebin/
- cp package/ejabberd_auth_jwt.beam /lib/ejabberd-16.09/ebin/
- cp package/ejwt.beam /lib/ejabberd-16.09/ebin/
- cp package/jiffy.beam /lib/ejabberd-16.09/ebin/
- cp package/jiffy_utf8.beam /lib/ejabberd-16.09/ebin/


The dependencies can be found at :

- https://github.com/dvv/base64url
- https://github.com/seth/ej
- https://github.com/kato-im/ejwt
- https://github.com/davisp/jiffy

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