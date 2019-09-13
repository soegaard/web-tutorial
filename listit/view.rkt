#lang at-exp racket/base
;;;
;;; VIEW
;;;

; The view presents data from the model to the user.
; In the case of an web-app the view generates html which
; is sent to the user's browser.

; There is a choice to be made on how to represent html in the program.

; A simple approach is to use plain strings, but manipulating html
; in string form has the potential of being very ineffecient (reparseing,
; repeated creation of new strings vhen strings are concatenated etc.)

; A popular choice of representation in the Racket/Scheme world is S-expressions.
; Here another choice has been made: html will be represented using the
; (html) element struct from `scribble/html`.  

; The choice has a few advantages:
;   - Scribble's at-expressions can be used to construct html.
;   - calling functions that constructs pieces of html
;     looks the same as a call that constructs actual html elements
;   - typos in tag names will be caught on compile time
;   - it is easy to mix html tags and text

; For each html tag  p, h1, a, img, div, span, etc. there is a function
; of the same name that constructs an element representing a piece of html with that tag.

; The general syntax is:
;     @name[attribute1: "value1" attribute2: "value2"]{Some text}
; When no attributes are needed:
;     @name{Some text}
; When no text is needed:
;     @name[attribute1: "value1" attribute2: "value2"]

; Some examples:
;   @p{This is some text}            (p "This is some text")            <p>This is some text</p>
;   @div[class: "score"]{42 points}  (div 'class: "score" "42 points")  <div class="score">42 points</div>

; Nesting is easy:

;   @p{This @it{word} is in italics.}

;   @div[class: "centered"]{
;     @div[class: "title"]{Racket News}
;     @div[class: "score"]{42 points}}

; Note that one can abbreviate the above as:

;   @div[class: "centered"
;     @div[class: "title"]{Racket News}
;     @div[class: "score"]{42 points}]

; Use ~x to turn an html element into a string.


;;;
;;; Exports
;;;

;; The control needs the following functions:

(provide html-about-page
         html-front-page
         html-submit-new-page)


;; Dependencies

(require (for-syntax racket/base)
         racket/format racket/file racket/match net/sendurl
         urlang/html (only-in scribble/html label)
         (prefix-in html: urlang/html)
         "def.rkt" "model.rkt")

;;;
;;; Parameters
;;;

(define current-page (make-parameter #f)) ; used by navigation-bar

;;;
;;; External Resources
;;;

(define html5shiv-js   "https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js")
(define respond-js     "https://oss.maxcdn.com/respond/1.4.2/respond.min.js")
(define fontawesome-js "https://kit.fontawesome.com/d1076de1c9.js")

(define racket-logo       "https://i.imgur.com/WI0rFIg.png")
(define white-racket-logo "https://i.imgur.com/HnF66fo.png")

;;;
;;; STYLING
;;;

;; This web-app uses Bootstrap to style the html.

;; Bootstrap is a so-called CSS framework.
;; By giving an html element a class, the browser will style
;; the element according to a style sheet. The Bootstrap
;; documentation outlines how to use the class names.
;; One advantage of using a CSS framework is that there
;; are many themes to choose from - without needing any
;; changes in the code.

; main-column
;   The main column has a navigation bar at top and a colored middle section below.
;   The styles "container" and "container-fluid" are from Bootstrap.
;   The style main_colum is defined in the section below styling.
(define (main-column . xs)
  @div[class: "container"
    @navigation-bar{}                       
    @div[class: "container-fluid main_column" style]{@xs}])

; navigation-bar
;   The navigation bar appears on top of all pages and shows the user
;   the current (active) page.
(define (navigation-bar)
  ; The nav-item corresponding to the current page
  ; needs the class "active" (makes item stand out)
  (define (active item)
    (cond [(equal? (~a item) (~a (current-page))) " active"]
          [else                                   ""]))  
  @navbar{
      @img[class: "racket-logo" src: white-racket-logo
           alt: "racket logo" width: "40px" height: "40px"]
      @nbsp @nbsp
      @div[class: "navbar-nav-scroll"]{
        @ul[class: "navbar-nav bd-navbar-nav flex-row"]{
          @li[class: (~a "nav-item" (active 'home))]{
            @a[class: "nav-link" href: "?"]{Home}}
          @span[class: "navbar-text"]{ | }
          @li[class: (~a "nav-item" (active 'submit))]{
            @a[class: "nav-link" href: "?action=submitnew"]{Submit}}
          @span[class: "navbar-text"]{ | }
          @li[class: (~a "nav-item" (active 'about))]{
            @a[class: "nav-link" href: "?action=about"]{About}}}}})


(define (submit-button . xs)
  @button[type: "submit" class: "btn btn-primary"]{@xs})

(define (form-group . xs)
  @div[class: "form-group"]{@xs})
    
(define (form-input . xs)
  ; Bootstrap uses the class "form-control" for inputs.
  (apply input (list* class: "form-control" xs)))

(define (navbar . xs)
  ; Bootstrap uses the class "navbar" for the navigation bar.
  ; A dark navigation bar will get white text.
  @html:nav[class: "navbar navbar-expand-lg navbar-dark"]{@xs})

(define (navbar-brand-a . xs)
  ; Bootstrap uses the class "navbar-brand" for the brand (logo).
  (apply a (list* class: "navbar-brand" xs)))

(define (list-group . xs)
  (apply div (list* class: "list-group" @xs)))

(define (list-item . xs)
  @div[class: "list-group-item"]{@xs})

(define (list-item-action . xs)
  (apply a (list* class: "list-group-item" xs)))


;;;
;;; Stylesheet
;;;

; border: 1px solid red

(define stylesheet "
    .mw600px        { max-width: 600px; }
    body            { font-size: 1rem; margin: 2rem; }
    a               { display: inline; }
    nav             { background-color: var(--purple);}
    .main_column    { background-color: #f6f6ef; }

    .entry-row          { vertical-align: middle; }
      .rank-col         { vertical-align: middle; text-align: right;   font-size: 300%; }
      .arrow-col        { vertical-align: middle; text-align: center; }
        form.arrows     { vertical-align: middle; margin: auto;       }
          .updowngrid   { vertical-align: middle; margin: auto; padding: 0px;  }
          .updowngrid a { display: grid; }
      .titlescore-col   { vertical-align: middle; text-align: left; display: inline-grid;}
        .titlescore     {                         text-align: left; display: inline-grid; margin: auto auto auto 0px;}

")


;;;
;;; HTML Page 
;;;


;; Functions to embed scripts and css into a page
(define (script   url) @~a{<script src=@url ></script>})    
(define (link-css url) @~a{<link href=@url rel="stylesheet">})

(define (html-page #:title title #:body body)
  ;; Given a body (a string) wrap it in a basic html template  from 
  ;; the Bootstrap project:
  ;;     http://getbootstrap.com/getting-started/#template
  ;; Finally load icons from FontAwesome.
  (def the-body (match body
                  [(? string? body) body]
                  [(? list? body)   (string-append* (map ~x body))]
                  [else             (~x body)]))
  @~a{
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <!-- The above 3 meta tags *must* come first in the head;
         any other head content must come *after* these tags -->
    <title>@title </title>

    <!-- Bootstrap -->
    <link rel="stylesheet" 
          href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" 
          integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" 
          crossorigin="anonymous">

    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
    <!--[if lt IE 9]>
    @script[html5shiv-js]
    @script[respond-js]
    <![endif]-->
    @script[fontawesome-js]
    <style> @|stylesheet| </style>
  </head>
  <body>
      @the-body
      <!-- Include all compiled plugins (below), or include individual files as needed -->
      <!-- jQuery first, then Popper.js, then Bootstrap JS -->
      <!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
    <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"
            integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" 
            crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" 
            integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1"
            crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
            integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
            crossorigin="anonymous"></script> 
  </body>
  </html>})

;;;
;;; The About Page
;;;

(define (html-about-page)
  (define (github-a name)
    (def url (~a "https://github.com/soegaard/web-tutorial/tree/master/listit/" name))
    (@a[href: url]{@tt{@name}}))
  ; links to the source files at Github
  (def model.rkt   (github-a "model.rkt"))
  (def view.rkt    (github-a "view.rkt"))
  (def control.rkt (github-a "control.rkt"))
  (def server.rkt  (github-a "server.rkt"))
  
  (current-page "about") ; used by navigation-bar to hightlight the current page
  (html-page
   #:title "List it! - About"
   #:body
   @main-column{
     @h1{About}

     @p{This little web-app demonstrates one way to write small web applications 
        with Racket. The intention is provide a starting point. Something you can tinker 
        with @mdash without inventing everything from scratch.}

     @p{Have fun with it.
             @br @em{Jens Axel Søgaard}
             @br @tt{jensaxel@"@"soegaard.net}}

     @h2{Installation}

     @p{The source code for this application is available at Github:
            
              @div[class: "mw600px mx-auto text-left"
                    @a[href: "https://github.com/soegaard/web-tutorial"]{https://github.com/soegaard/web-tutorial}]
              @br
        Clone the repository. Install a few packages (see next paragraph). 
        Open "server.rkt" in your editor and run it.
        This will start a web-server and instruct your web-browser to show you
        the front page.}

     @p{Here are the packages you need besides the packages that are included in
        the standard Racket distribution.
              @div[class: "mw600px mx-auto text-left"
                @code{raco pkg install deta}      @br
                @code{raco pkg install urlang}    @br
                    @code{raco pkg install threading} @br]}

     @h2{Overview}

     @p{The app uses the Model-View-Controller architecture (MVC).}
            
     @p{To make this super clear the app consists of the files @model.rkt, @view.rkt and @|control.rkt|. 
        In a larger application it would make sense to make separate folders for the model,
        view and control @mdash and split the files into smaller pieces.
        For example each html page in @view.rkt could get its own file.}

     @p{The @em{model} (see @model.rkt) consists of a database in which the
        the urls and scores are stored. You can open @model.rkt in your editor,
        run it and then experiment in the repl with @tt{(top 3)}, @tt{(page 1)} etc.}

     @p{The @em{view} (see @view.rkt) presents data from the model to the user.
        In the case of an web-app the view generates html which
        is sent to the user's browser.}

     @p{The @em{control} (see @control.rkt) accepts input from the user,
        reads or writes to the model, and then sends output back using
        the view to present the output}

     @p{The server (see @server.rkt) takes care of receiving requests
        from the users and sending back responses. For this application
        the server will send all requests to the control to be handled.}
     
     @h2{Packages}
     
     @p{The app relies on the following packages:
            @div[class: "mw600px mx-auto"
              @list-group[
                @list-item-action[href: "https://docs.racket-lang.org/deta/index.html"]{
                  @h5{deta}
                  @p{The Deta library is used to map database tables to Racket structs.
                     After updating the values of a struct, Deta can send the relevant changes to the database.
                     In fact Deta can be used to perform Create-Read-Update-Delete (CRUD) operations on
                     your model @mdash as well as making arbitrary queries.}
                  @p{You can think of Deta as an Object-Relational Mapper using structs instead of objects.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/db/index.html"]{
                  @h5{db}
                  @p{The @tt{db} library is the foundation for Deta.
                     We are also using it to create the initial database connection.
                     Using @tt{db} has the effect that we can easily switch between
                     PostgreSQL, SQLite and MySQL backends.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/scribble-pp/html.html"]{
                  @h5{scribble/html}
                  @p{A popular approach of representing html is S-expression.
                     In this app we have however chosen to represent html as structures.
                     This choice has a few advantages: First of all we can use Scribble's
                     at-expressions to construct html. Second, calling functions that
                     constructs pieces of html looks the same as a call that constructs
                     an actual html construct.}}
                @p{}
                @list-item{
                  @h5{urlang/html}
                  @p{A wrapper of @tt{scribble/html} that provides a few utilities that
                       makes it easier to work with the element structure of @tt{scribble/html}.
                       One of the provided functions is @tt{~x} which converts an element
                       into string.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/threading/index.html"]{
                  @h5{threading}
                  @p{The @tt{threading} library provides a few macros that flatten nested function calls.
                         Here we/Deta use @tt{~>} to construct database queries with a nice syntax.}}
                @p{}
                @list-item-action[href: "https://docs.racket-lang.org/web-server/index.html"]{
                  @h5{web-server}
                  @p{The @tt{web-server} library provides tools for working with requests
                         and responses. It also makes it easy to get a server up and running
                         without a lengthy installation and configuration process.}}
                @p{ }
                ]]}

     }))


;;;
;;; The Submit-new-entry page
;;;

;; The function html-submit-new-page builds an html-expression representing the submission page.
;; It takes no arguments, since it doesn't rely on data from the model:

; Note: input has no body only attributes

(define (html-submit-new-page)
  (current-page "submit")
  (html-page
   #:title  "List it! - submit"
   #:body
   @main-column{
     @h2{Submit a new entry}
     @form[name: "submitnewform" action: "control.rkt"]{
       @input[name: "action" value: "submit" type: "hidden"]
       @form-group{
         @label[for: "url"]{URL}
         @form-input[name: "url"   type: "text" value: "http://"]}
       @form-group{
         @label[for: "title"]{Title}
         @form-input[name: "title" type: "text" value: "Enter a title"]}
       @submit-button{Submit}}
     @p{}
     @p{Everything Racket related has interest.}}))

;;;
;;; The Front Page
;;;

(define (html-front-page page-number rank-of-first-entry entries)
  (current-page "home")
  (html-page
   #:title "List it! - Front Page"
   #:body
   @main-column{
     @(html-list-of-entries page-number rank-of-first-entry entries)}))


;;; List of entries

(define (html-list-of-entries page-number rank-of-first-entry entries)
  (define (entries->rows entries)
    (for/list ([e entries] [rank (in-naturals rank-of-first-entry)])
      (entry->table-row e rank)))
  (define (entry->table-row e rank)
    (defm (struct* entry ([title the-title] [url the-url] [score the-score] [id id])) e)
    (def  form-name (~a "arrowform" id))
    @div[class: "entry-row row"]{  
      @span[class: "rank-col  col-auto"]{ @rank }
      @span[class: "arrow-col col-auto row" 
        @form[class: "arrows" name: form-name action: "control.rkt"]{
          @input[name: "arrow" type: "hidden"] 
          @input[name: "entry_id"  type: "hidden" value: id]
          @input[name: "action"    type: "hidden" value: "updown"]
          @span[class: "updowngrid"
            @(html-a-submit form-name "arrow" "up"   (html-icon 'chevron-up))
            @(html-a-submit form-name "arrow" "down" (html-icon 'chevron-down))]}]
      @span[class: "titlescore-col col"
         @span[class: "titlescore"
           @a[href: the-url]{ @the-title } 
           @span[class: "score"]{@the-score points}]]})
                                                       
  @span[class: "entries container-fluid"]{
    @(entries->rows entries)})


(define (html-a-submit form-name form-item id text)
  @a[href: @~a{javascript:
               document.@|form-name|.@|form-item|.value='@|id|';
               document.@|form-name|.submit(); 
               }]{@text})

;;;
;;; Icons
;;;

; Icons are from FontAwesome. Used for the up/down arrows.

(define (html-icon name)
  @i[class: (~a "fas fa-" name)])
