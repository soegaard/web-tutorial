# TODO

## Done

+ input validation for submit form
+ use urls for navigation
+ only logged-in users can vote
+ submit  : not logged in -> login page
+ only logged-in users can submit new entries
+ home    : navigation between pages of entries
+ show current page in nav bar, even if it isn't one of the always featured
+ user    : show information on user
+ prevent voting multiple times on same entry
+ home    : show site after title
+ from    : page showing all submissions from a certain site
+ vote    : stay on same page after voting
+ submit  : only allow resubmission after a month has passed
+ new     : show recently submitted entries
+ vote    : register ip of voter
+ home    : show newest
+ other   : favicon
+ other   : images   in files-root/static/     (stopped using imgur)
+ other   : favicons in files-root/favicons/ 
+ home    : remove down votes / change icon to thumbs-up
+ popular : sorted after votes, last day, week, month, year
+ general : show thumbs-up only if the user hasn't voted previously
+ profile : user can change "about" text
+ buy domain : racket-stories.com
+ cookie  : same site cookie (needed domain)
+ ssl     : certificate using certbot (Let's encrypt)
+ session : add sessions to model, use them to determine if user is logged in
+ cookie  : use secure cookie (needs ssl) on server
+ cookie  : use samesite cookies 

## Must Do

- ssl     : setup ssl at racket level (is this needed?)
- daemon  : start racket server as daemon
- log     : 
- mail    : email confirmation
- signup  : feedback from input validation

# Like To Do
- home    : the sort order should consider both votes and age 
            (need more entries to make sense)
- profile : order digest of new submissions pr mail
- tag     : show only entries with given tag
- admin   : manually add tags

- login   : google login
- search  : users / sites / tags
- user    : show more information
- admin   : statistics
- user    : show about text
- feed    : rss or atom 
- other   : karma

## Other

- populate database with lots of entries
- find a suitable name
- does ~> memoize?, if not perhaps memoize query:popular and query:newest
- setup server
- launch



