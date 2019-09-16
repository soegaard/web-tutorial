# How to write web applications with Racket

The folder `listit` contains a basic MVC-based implementation
of a "submit a link and vote" site.

The folder `listit2` expands on the initial example and add users.
Adding users is conceptually simple, but in practise there are
many details to consider. 

The folder `listit3` has the following changes:
 - validation of form data on the "submit new entry" page
 - introduces url dispatch in the control (instead of dispatching on action)
 - front page renamed to home


## Changes
```
  model.rkt     - schema user added to database
                - new operations: create-user authenticate-user
  view.rkt      - new page: login page (slash create new account)
                - front page: only show voting arrows for logged in users
  control.rkt   - add dispatch rules to new page and login/logout operations
                - stores login-status in id-cookies on the client
                - use current-user and current-login-status to communicate with view.rkt
New:

  usernames.rkt       - validation and normalization of usernames
  authentication.rkt  - key derivation from password and salt
```

## Usernames

Validating usernames is not as simple as one might think.
We want usernames to be unique, displayable (they are going to be shown
on the html pages). Furthermore we don't want users to pick 
usernames like "admin", "root" etc. See "usernames.rkt".

## Passwords
Principles:
```
   1. Don't store passwords in plain text
      (It's a problem if an attacker gets access to the database)
      Store a key derived from the password.

   2. Don't store a simple hash of the password.
      (An attacker might have rainbow tables)
      Conclusion: Use a salt (random string, non-secret)
      string together with the passwords to derive a key.

   3. Use a standard KDF (Key Derivation function)
      Conclusion: Use the `crypto` package.
```	  
The file "authentication.rkt" provides `derive-key` and `random-salt`
for the model. The funtion `derive-key` computes a key based
on a password and salt.


## Storing login-status

There is a choice to be made: should the login-status be stored
on the client, the web-server(s) or the database server?

See [Stateful and stateless authentication](https://medium.com/@kennch/stateful-and-stateless-authentication-10aa3e3d4986)
for a full discussion.

In the name of scalability we will store the login-status on the client
in the form of cookies. Since cookies can be altered, we can't use plain
cookies, but need to embed a digest hash of both a salt and the stored 
information in the cookie. The web-server library provides an 
abstraction `id-cookie` which works great for this purpose.
See more in `control.rkt`


![Front Page](https://i.imgur.com/C05Mli7.png)
![Login Page](https://i.imgur.com/Yx5R3KS.png)
![About](https://i.imgur.com/ituNrxn.png)
