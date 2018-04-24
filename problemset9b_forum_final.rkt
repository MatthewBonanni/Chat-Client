;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname problemset9b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require racket/string)

;;;;;;;;;;;;;;;;;;;;
; DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;

; A World is one of
; - Viewall
; - Threadview
; - Newitem
; - Search
; INTERPRETATION: Represents four different "views" in your program.

; TEMPLATE
(define (world-temp world)
  (cond
    [(viewall? world) (viewall-temp world)]
    [(threadview? world) (threadview-temp world)]
    [(newitem? world) (newitem-temp world)]
    [(search? world) (search-temp world)]))

; A Viewall is a (make-viewall String History)
(define-struct viewall [edit history])
;INTERPRETATION: The user is viewing all posts (but not replies), 
; and possibly typing a Command.

; TEMPLATE
(define (viewall-temp va)
  (... (viewall-edit va) ...
       (history-temp (viewall-history va)) ...))

; A Command is a string of the form
; – "catchup"                      <<=== send the "CATCHUP" message
; – "new"                          <<=== start creating a new post
; – "reply ", followed by a number <<=== start replying to the post with that ID#
; – "view ", followed by a number  <<=== view the full thread of post of that ID#

; A Threadview is a (make-threadview Post History)
(define-struct threadview [post history])
; INTERPRETATION: The user is viewing a specific Post and its replies.

; TEMPLATE
(define (threadview-temp tv)
  (... (post-temp (threadview-post tv)) ...
       (history-temp (threadview-history tv)) ...))

; A Newitem is a (make-newitem [Maybe Natural] String History)
(define-struct newitem [id edit history])
;INTERPRETATION: The user is entering a new post (if the maybe is #false),
;   or a new reply (if the maybe is a number).

; TEMPLATE
(define (newitem-temp ni)
  (... (newitem-id ni) ...
       (newitem-edit ni) ...
       (history-temp (newitem-history ni)) ...))

; A Search is a (make-search History String)
(define-struct search [history search])
; INTERPRETATION: The user is trying to view only a subset
;   of the existing messages.

; TEMPLATE
(define (search-temp s)
  (... (history-temp (search-history s))...
       (search-search s)))
                 
; An Edit is a String
; INTERPRETATION: the contents of the post the user is currently
; editing.

; A History is a [List-of Post]
; INTERPRETATION: the prior posts received from the server.

; TEMPLATE
(define (history-temp hist)
  (cond
    [(empty? hist) ...]
    [(cons? hist) (... (first hist)
                       (history-temp (rest hist)) ...)]))

; A Post is a (list Number (make-post String String [List-of Reply]))
(define-struct post [author content replies])
; INTERPRETATION:
; - the Number is the ID# of a post
; - author is the author of the post
; - content is the content of the post
; - replies is the list of replies that have been seen so far

; TEMPLATE
(define (post-temp p)
  (... (first p) ...
       (post-author (second p)) ...
       (post-content (second p)) ...
       (reply-temp (post-replies (second p)))...))
 
; A Reply is a (make-reply String String)
(define-struct reply [author content])
; INTERPRETATION:
; - author is the author of the reply
; - content is the content of the reply

; TEMPLATE
(define (reply-temp r)
  (... (reply-author r) ...
       (reply-content r) ...))

; A ClientMsg is one of
; - "CATCHUP"
; - (list "POST" String)
; - (list "REPLY" Natural String)

; INTERPRETATION:
; – Sending the message "CATCHUP" tells the server you would like it
;    to send you all the prior posts the server has received.  You are only
;    allowed to ask the server for catch-up messages once; a second request
;    will result in an error.
; – Sending the message (list "POST" String) – i.e., a two-item
;    list whose first item is the string "POST", and whose second item is a
;    String – indicates you are writing a new post, where the string provides
;    the text of the post
; – Sending the message (list "REPLY" Natural String) indicates that
;    you are replying to an existing post (Note: you cannot reply to a reply).
;    The number indicates the post's id, and the string is the text of the post.

; TEMPLATE

(define (clientmsg-temp cm)
  (cond
    [(string? cm) ...]
    [(string=? "POST" (first cm)) (... (second cm) ...)]
    [(string=? "REPLY" (first cm)) (... (second cm) ...
                                        (third cm) ...)]))

; A ServerMsg is one of:
; - (list "POST" Natural String String)
; - (list "REPLY" Natural String String)
; - (list "ERROR" String)

; INTERPRETATION:
; – Receiving a "POST" message means there is a new post with the given ID#,
;    author, and contents.  This is the same
;    information as you've been receiving via "id:author:contents", except
;    that the data is properly broken apart for you, instead of mashed into
;    one string.
; – Receiving a "REPLY" message there is a new reply containing the ID# of
;    the parent post (that this is a reply to), the author of the reply as 
;    the next string, and whose content is the final string.
; – Receiving an "ERROR" message means the client made a mistake, with the
;    error message given as the string.

; TEMPLATE

(define (servermsg-temp sm)
  (cond
    [(string=? "POST" (first sm)) (... (second sm) ...
                                       (third sm) ...
                                       (fourth sm) ...)]
    [(string=? "REPLY" (first sm)) (... (second sm) ...
                                        (third sm) ...
                                        (fourth sm) ...)]
    [(string=? "ERROR" (first sm)) (... (second sm) ...)]))

;;;;;;;;;;;;;;;;;;;;
; DATA EXAMPLES
;;;;;;;;;;;;;;;;;;;;

(define CM-1 "CATCHUP")
(define CM-2 (list "POST" "Hello"))
(define CM-3 (list "REPLY" 0 "Goodbye"))

(define SM-1 (list "POST" 0 "John" "hello world"))
(define SM-2 (list "REPLY" 0 "Jane" "Goodbye!"))
(define SM-3 (list "ERROR" "You done goofed!"))

(define REPLY-1 (make-reply "Jack" "Cool"))
(define REPLY-2 (make-reply "Joe" "No"))

(define POST-1 (list 0 (make-post "John Doe" "hello world" '())))
(define POST-2 (list 1 (make-post "Jane Doe" "Goodbye!" '())))
(define POST-3 (list 2 (make-post "Mary Smith" "qwerty" '())))
(define POST-4 (list 3 (make-post "First Last" "Yes" (list REPLY-1))))

(define LOP-1 '())
(define LOP-2 (list POST-1 POST-2 POST-3))
(define LOP-3 (list POST-1 POST-2 POST-3 POST-4))

(define EDIT-1 "")
(define EDIT-2 "Hello World!")

(define HISTORY-1 LOP-1)
(define HISTORY-2 LOP-2)
(define HISTORY-3 LOP-3)

(define SEARCH-1 "")
(define SEARCH-2 "hello")

(define VA-1 (make-viewall "Hello" HISTORY-1))
(define VA-2 (make-viewall "Hello" HISTORY-2))
(define VA-3 (make-viewall "" HISTORY-1))
(define VA-4 (make-viewall "catchup" HISTORY-1))
(define VA-5 (make-viewall "catchup" HISTORY-3))
(define VA-6 (make-viewall "new" HISTORY-3))
(define VA-7 (make-viewall "reply 1" HISTORY-3))
(define VA-8 (make-viewall "view 2" HISTORY-3))

(define TV-1 (make-threadview POST-1 HISTORY-1))
(define TV-2 (make-threadview POST-4 HISTORY-3))

(define NI-1 (make-newitem #false "Hello" HISTORY-1))
(define NI-2 (make-newitem 0 "Goodbye!" HISTORY-2))
(define NI-3 (make-newitem 1 "Done!" HISTORY-3))

(define S-1 (make-search HISTORY-1 ""))
(define S-2 (make-search HISTORY-2 "hello"))
(define S-3 (make-search HISTORY-2 "no"))
(define S-4 (make-search HISTORY-3 "yes"))
  
(define BACKGROUND (rectangle 350 500 "solid" "white"))

(define WORLD-INIT (make-viewall "" '()))

;;;;;;;;;;;;;;;;;;;;
; PROGRAM
;;;;;;;;;;;;;;;;;;;;

; Main function - generate forum
; Sends, recieves, and displays given posts made by users on a server
; simple-net-forum : World -> World

(define (simple-net-forum ws)
  (big-bang ws
    [on-tick sort-history 1]
    [to-draw draw-forum]
    [on-key handle-key]
    [on-receive receive-message]
    [register "dictionary.ccs.neu.edu"]
    [port 10006]
    [name "bonanni.m:7245"]))

; Sort history of given world according to post id (useful for CATCHUP calls)
; sort-history : World -> World

(check-expect (sort-history (make-viewall ""
                                          (list (list 1 (make-post "first" "first" '()))
                                                (list 3 (make-post "third" "third" '()))
                                                (list 2 (make-post "second" "second" '())))))
              (make-viewall ""
                            (list (list 3 (make-post "third" "third" '()))
                                  (list 2 (make-post "second" "second" '()))
                                  (list 1 (make-post "first" "first" '())))))

(define (sort-history ws)
  (cond
    [(viewall? ws) (make-viewall (viewall-edit ws)
                                 (sort (viewall-history ws) id-compare))]
    [(threadview? ws) (make-threadview (threadview-post ws)
                                       (sort (threadview-history ws) id-compare))]
    [(newitem? ws) (make-newitem (newitem-id ws)
                                 (newitem-edit ws)
                                 (sort (newitem-history ws) id-compare))]
    [(search? ws) (make-search (sort (search-history ws) id-compare)
                               (search-search ws))]))

; Compare two posts to determine whether the first comes before the second
; id-compare : Post Post -> Boolean

(check-expect (id-compare POST-1 POST-2) #false)
(check-expect (id-compare POST-3 POST-2) #true)

(define (id-compare p1 p2)
  (> (first p1) (first p2)))

; Draw current image of forum state onto an empty scene
; draw-forum : World -> Image

(check-expect (draw-forum VA-3)
              (place-image/align (above/align
                                  "left"
                                  empty-image
                                  (overlay/align
                                   "left"
                                   "middle"
                                   (beside (text "Command: " 12 "blue")
                                           (text "" 12 "black"))
                                   (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                                 0
                                 (image-height BACKGROUND)
                                 "left"
                                 "bottom"
                                 BACKGROUND))

(check-expect (draw-forum VA-2)
              (place-image/align (above/align
                                  "left"
                                  (draw-history HISTORY-2)
                                  (overlay/align
                                   "left"
                                   "middle"
                                   (beside (text "Command: " 12 "blue")
                                           (text "Hello" 12 "black"))
                                   (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                                 0
                                 (image-height BACKGROUND)
                                 "left"
                                 "bottom"
                                 BACKGROUND))

(check-expect (draw-forum TV-2)
              (place-image/align (above/align
                                  "left"
                                  (draw-post-and-replies POST-4)
                                  (overlay/align
                                   "left"
                                   "middle"
                                   (text "Press F1 or F2 to switch modes" 12 "blue")
                                   (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                                 0
                                 (image-height BACKGROUND)
                                 "left"
                                 "bottom"
                                 BACKGROUND))

(check-expect (draw-forum NI-3)
              (place-image/align (above/align
                                  "left"
                                  (draw-history HISTORY-3)
                                  (overlay/align
                                   "left"
                                   "middle"
                                   (beside (text "Compose: " 12 "blue")
                                           (text "Done!" 12 "black"))
                                   (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                                 0
                                 (image-height BACKGROUND)
                                 "left"
                                 "bottom"
                                 BACKGROUND))

(check-expect (draw-forum S-4)
              (place-image/align (above/align
                                  "left"
                                  (draw-history (search-posts HISTORY-3 "yes"))
                                  (overlay/align
                                   "left"
                                   "middle"
                                   (beside (text "Search: " 12 "blue")
                                           (text "yes" 12 "black"))
                                   (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                                 0
                                 (image-height BACKGROUND)
                                 "left"
                                 "bottom"
                                 BACKGROUND))

(define (draw-forum ws)
  (cond
    [(viewall? ws)
     (place-image/align (above/align
                         "left"
                         (draw-history (viewall-history ws))
                         (overlay/align
                          "left"
                          "middle"
                          (beside (text "Command: " 12 "blue")
                                  (text (viewall-edit ws) 12 "black"))
                          (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                        0
                        (image-height BACKGROUND)
                        "left"
                        "bottom"
                        BACKGROUND)]
    [(threadview? ws)
     (place-image/align (above/align
                         "left"
                         (draw-post-and-replies (threadview-post ws))
                         (overlay/align
                          "left"
                          "middle"
                          (text "Press F1 or F2 to switch modes" 12 "blue")
                          (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                        0
                        (image-height BACKGROUND)
                        "left"
                        "bottom"
                        BACKGROUND)]
    [(newitem? ws)
     (place-image/align (above/align
                         "left"
                         (draw-history (newitem-history ws))
                         (overlay/align
                          "left"
                          "middle"
                          (beside (text "Compose: " 12 "blue")
                                  (text (newitem-edit ws) 12 "black"))
                          (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                        0
                        (image-height BACKGROUND)
                        "left"
                        "bottom"
                        BACKGROUND)]
    [(search? ws)
     (place-image/align (above/align
                         "left"
                         (draw-history (search-posts (search-history ws) (search-search ws)))
                         (overlay/align
                          "left"
                          "middle"
                          (beside (text "Search: " 12 "blue")
                                  (text (search-search ws) 12 "black"))
                          (rectangle (image-width BACKGROUND) 20 "solid" "WhiteSmoke")))
                        0
                        (image-height BACKGROUND)
                        "left"
                        "bottom"
                        BACKGROUND)]))

; Generate post history image from given LoP
; draw-history : History -> Image

(check-expect (draw-history HISTORY-1)
              empty-image)
(check-expect (draw-history HISTORY-2)
              (above/align "left"
                           (above/align "left"
                                        (above/align "left"
                                                     empty-image
                                                     (draw-post POST-3))
                                        (draw-post POST-2))
                           (draw-post POST-1)))

(define (draw-history lop)
  (local (; Renders and stacks two posts on top of each other
          (define (combine-history post1 post2)
            (above/align "left"
                         post1
                         post2)))
    (foldl combine-history empty-image (map draw-post-and-replies lop))))

; Renders an image of the given post and its replies
; draw-post-and-replies : Post -> Image

; The original post will have the author rendered on the first line in red, followed by ":"
; The content of the original post will be rendered on the second line in black.
; All replies to this post will be rendered in the same way, and placed below the original post.
; Replies will be indented for clarity

(check-expect (draw-post-and-replies POST-1)
              (draw-post POST-1))
(check-expect (draw-post-and-replies POST-4)
              (above/align
               "left"
               (draw-post POST-4)
               (draw-reply REPLY-1)))

(define (draw-post-and-replies post)
  (local (; combine-reply : Image Image -> Image
          ; Combine two rendered replies (images stacked vertically)
          (define (combine-reply reply1 reply2)
            (above/align
             "left"
             reply2
             reply1)))
    (foldl combine-reply
           (draw-post post)
           (map draw-reply (post-replies (second post))))))

; Draw an individual reply
; draw-reply : Reply -> Image

; The reply will be rendered as described above

(check-expect (draw-reply REPLY-1)
              (beside
               (square 20 "solid" "white")
               (above/align
                "left"
                (text "Jack:" 12 "red")
                (text "Cool" 12 "black"))))

(define (draw-reply reply)
  (beside
   (square 20 "solid" "white")
   (above/align
    "left"
    (text (string-append (reply-author reply) ":") 12 "red")
    (text (reply-content reply) 12 "black"))))

; Generate rendered image of a given post
; draw-post : Post -> Image

(check-expect (draw-post POST-1)
              (above/align
               "left"
               (text (string-append "("
                                    "0"
                                    ") "
                                    "John Doe"
                                    ":") 12 "red")
               (text "hello world" 12 "black")))

(define (draw-post post)
  (above/align
   "left"
   (text (string-append "("
                        (number->string (first post))
                        ") "
                        (post-author (second post))
                        ":") 12 "red")
   (text (post-content (second post)) 12 "black")))

; Return a List of Posts from the given History that contain the given String
; search-posts : History String -> LoP

; NOTE: search-posts will search across all fields of each post

(check-expect (search-posts HISTORY-1 SEARCH-1) '())
(check-expect (search-posts HISTORY-1 SEARCH-2) '())
(check-expect (search-posts HISTORY-2 SEARCH-1) HISTORY-2)
(check-expect (search-posts HISTORY-2 SEARCH-2) (list POST-1))
(check-expect (search-posts HISTORY-2 "e") HISTORY-2)
(check-expect (search-posts HISTORY-2 "z") '())

(define (search-posts history str)
  (local (; search-helper : String -> Boolean
          ; Checks if given string contains search str
          ; Examples - for str == "hello"
          ; (search-helper "hello 1") -> #true
          ; (search-helper "hi") -> #false
          (define (search-helper s)
            (string-contains? s str))

          ; reply-contains? : Reply -> Boolean
          ; Checks whether any part of reply contains search str
          ; Examples - for str == "Jack"
          ; (reply-contains? REPLY-1) -> #true
          ; (reply-contains? REPLY-2) -> #false
          (define (reply-contains? r)
            (or (search-helper (reply-author r))
                (search-helper (reply-content r))))

          ; post-contains? : Post -> Boolean
          ; Checks whether any part of given post contains search str
          ; POST-1 "" -> #true
          ; POST-1 "John" -> #true
          ; POST-1 "hello" -> #true
          ; POST-1 "goodbye" -> #false
          (define (post-contains? post)
            (or (search-helper (number->string (first post)))
                (search-helper (post-author (second post)))
                (search-helper (post-content (second post)))
                (ormap reply-contains? (post-replies (second post))))))
    (filter post-contains? history)))

; Handle keyboard input depending on the given world state
; handle-key : World KeyEvent -> World

(check-expect (handle-key VA-3 "h") (make-viewall "h" HISTORY-1))
(check-expect (handle-key VA-3 "\r") VA-3)
(check-expect (handle-key VA-3 "\b") VA-3)
(check-expect (handle-key VA-1 "\b") (make-viewall "Hell" HISTORY-1))
(check-expect (handle-key VA-2 "x") (make-viewall "Hellox" HISTORY-2))
(check-expect (handle-key VA-2 "\t") VA-2)
(check-expect (handle-key VA-1 "f1") VA-1)
(check-expect (handle-key VA-1 "f2") (make-search HISTORY-1 ""))

(check-expect (handle-key TV-1 "\b") TV-1)
(check-expect (handle-key TV-2 "\b") TV-2)
(check-expect (handle-key TV-1 "\r") TV-1)
(check-expect (handle-key TV-2 "\r") TV-2)
(check-expect (handle-key TV-1 "a") TV-1)
(check-expect (handle-key TV-2 "a") TV-2)
(check-expect (handle-key TV-2 "f1") (make-viewall "" HISTORY-3))
(check-expect (handle-key TV-2 "f2") (make-search HISTORY-3 ""))

(check-expect (handle-key S-1 "h") (make-search HISTORY-1 "h"))
(check-expect (handle-key S-2 "x") (make-search HISTORY-2 "hellox"))
(check-expect (handle-key S-3 "\r") S-3)
(check-expect (handle-key S-3 "\b") (make-search HISTORY-2 "n"))
(check-expect (handle-key S-3 "\t") S-3)
(check-expect (handle-key S-4 "!") (make-search HISTORY-3 "yes!"))
(check-expect (handle-key S-4 "f1") (make-viewall "" HISTORY-3))
(check-expect (handle-key S-4 "f2") S-4)

(check-expect (handle-key NI-1 "!") (make-newitem #false "Hello!" HISTORY-1))
(check-expect (handle-key NI-3 "x") (make-newitem 1 "Done!x" HISTORY-3))
(check-expect (handle-key NI-2 "\b") (make-newitem 0 "Goodbye" HISTORY-2))
(check-expect (handle-key NI-1 "\b") (make-newitem #false "Hell" HISTORY-1))
(check-expect (handle-key NI-1 "\r") (send-message NI-1))
(check-expect (handle-key NI-3 "\r") (send-message NI-3))
(check-expect (handle-key NI-1 "\t") NI-1)
(check-expect (handle-key NI-3 "\t") NI-3)
(check-expect (handle-key NI-1 "f1") (make-viewall "" HISTORY-1))
(check-expect (handle-key NI-2 "f1") (make-viewall "" HISTORY-2))
(check-expect (handle-key NI-1 "f2") (make-search HISTORY-1 ""))
(check-expect (handle-key NI-3 "f2") (make-search HISTORY-3 ""))
       

(define (handle-key ws key)
  (cond
    [(viewall? ws) (write-va ws key)]
    [(threadview? ws) (write-tv ws key)]
    [(newitem? ws) (write-ni ws key)]
    [(search? ws) (write-sp ws key)]))

; Handle keyboard input and manipulates message accordingly 
; write-va : Viewall KeyEvent -> World

(check-expect (write-va VA-3 "h") (make-viewall "h" HISTORY-1))
(check-expect (write-va VA-3 "\r") VA-3)
(check-expect (write-va VA-3 "\b") VA-3)
(check-expect (write-va VA-1 "\b") (make-viewall "Hell" HISTORY-1))
(check-expect (write-va VA-2 "x") (make-viewall "Hellox" HISTORY-2))
(check-expect (write-va VA-2 "\t") VA-2)
(check-expect (write-va VA-1 "f1") VA-1)
(check-expect (write-va VA-1 "f2") (make-search HISTORY-1 ""))

(define (write-va ws key)
  (if (string=? key "f2")
      (make-search (viewall-history ws) "")
      (if (= (string-length key) 1)
          (cond
            [(string=? key "\b") (make-viewall (backspace (viewall-edit ws))
                                               (viewall-history ws))]
            [(string=? key "\r") (handle-command ws)]
            [(string=? key "\t") ws]
            [else (make-viewall (string-append (viewall-edit ws) key)
                                (viewall-history ws))])
          ws)))

; Handle command sent when the world is a Viewall
; handle-command : Viewall -> World

(check-expect (handle-command VA-1) (make-viewall "ERROR: Invalid command" HISTORY-1))
(check-expect (handle-command VA-4) (make-package (make-viewall "" HISTORY-1) "CATCHUP"))
(check-expect (handle-command VA-5) (make-package (make-viewall "" HISTORY-3) "CATCHUP"))
(check-expect (handle-command VA-6) (make-newitem #false "" HISTORY-3))
(check-expect (handle-command VA-7) (make-newitem 1 "" HISTORY-3))
(check-expect (handle-command VA-8) (view-helper 2 (make-viewall "view" HISTORY-3)))

(define (handle-command ws)
  (local (; Split the command into two separate strings: command name and id#
          ; String -> String String
          ; (string-split "reply 1") -> "reply" "1'"
          ; (string-split "view 2") -> "view" "two"
          (define COMMAND (string-split (viewall-edit ws))))
    (cond
      [(empty? COMMAND) ws]
      [(string=? (first COMMAND) "catchup")
       (make-package (make-viewall "" (viewall-history ws))
                     "CATCHUP")]
      [(string=? (first COMMAND) "new")
       (make-newitem #false
                     ""
                     (viewall-history ws))]
      [(and (string=? (first COMMAND) "reply")
            (number? (string->number (second COMMAND))))
       (make-newitem (string->number (second COMMAND))
                     ""
                     (viewall-history ws))]
      [(and (string=? (first COMMAND) "view")
            (number? (string->number (second COMMAND))))
       (view-helper (string->number (second COMMAND)) ws)]
      [else (make-viewall "ERROR: Invalid command" (viewall-history ws))])))

; Handle creation of threadview
; view-helper : Number Viewall -> Post

(check-expect (view-helper 1 VA-1) VA-1)
(check-expect (view-helper 2 VA-6)
              (make-threadview (list 2 (make-post "Mary Smith" "qwerty" '()))
                               HISTORY-3))
(check-expect (view-helper 3 VA-8)
              (make-threadview (list 3 (make-post "First Last" "Yes" (list REPLY-1)))
                               HISTORY-3))

(define (view-helper id va)
  (local (; Find posts in history that are of the given id
          ; Number History -> Post
          ; (POST  3 HISTORY-3) -> (list 3 (make-post "First Last" "Yes" REPLY-1))
          ; (POST 2 HISTORY-3) -> (list 2 (make-post "Mary Smith" "qwerty" '()))
          (define POST (assoc id (viewall-history va))))
    (if (boolean? POST)
        va
        (make-threadview POST
                         (viewall-history va)))))

; Handle keyboard input when the world is a Threadview
; write-tv : Threadview KeyEvent -> World

(check-expect (write-tv TV-1 "\b") TV-1)
(check-expect (write-tv TV-2 "\b") TV-2)
(check-expect (write-tv TV-1 "\r") TV-1)
(check-expect (write-tv TV-2 "\r") TV-2)
(check-expect (write-tv TV-1 "a") TV-1)
(check-expect (write-tv TV-2 "a") TV-2)
(check-expect (write-tv TV-2 "f1") (make-viewall "" HISTORY-3))
(check-expect (write-tv TV-2 "f2") (make-search HISTORY-3 ""))

(define (write-tv ws key) 
  (cond
    [(string=? key "f1") (make-viewall "" (threadview-history ws))]
    [(string=? key "f2") (make-search (threadview-history ws) "")]
    [else ws]))

; Handle keyboard input when typing a new message
; write-ni : Newitem KeyEvent -> World

(check-expect (write-ni NI-1 "!") (make-newitem #false "Hello!" HISTORY-1))
(check-expect (write-ni NI-3 "x") (make-newitem 1 "Done!x" HISTORY-3))
(check-expect (write-ni NI-2 "\b") (make-newitem 0 "Goodbye" HISTORY-2))
(check-expect (write-ni NI-1 "\b") (make-newitem #false "Hell" HISTORY-1))
(check-expect (write-ni NI-1 "\r") (send-message NI-1))
(check-expect (write-ni NI-3 "\r") (send-message NI-3))
(check-expect (write-ni NI-1 "\t") NI-1)
(check-expect (write-ni NI-3 "\t") NI-3)
(check-expect (write-ni NI-1 "f1") (make-viewall "" HISTORY-1))
(check-expect (write-ni NI-2 "f1") (make-viewall "" HISTORY-2))
(check-expect (write-ni NI-1 "f2") (make-search HISTORY-1 ""))
(check-expect (write-ni NI-3 "f2") (make-search HISTORY-3 ""))

(define (write-ni ws key)
  (cond
    [(string=? key "f1") (make-viewall "" (newitem-history ws))]
    [(string=? key "f2") (make-search (newitem-history ws) "")]
    [else (if (= (string-length key) 1)
              (cond
                [(string=? key "\b") (make-newitem (newitem-id ws)
                                                   (backspace (newitem-edit ws))
                                                   (newitem-history ws))]
                [(string=? key "\r") (send-message ws)]
                [(string=? key "\t") ws]
                [else (make-newitem (newitem-id ws)
                                    (string-append (newitem-edit ws) key)
                                    (newitem-history ws))])
              ws)]))

; Modify the search term of the given Search based on the given String
; write-sp : Search String -> Search

(check-expect (write-sp S-1 "h") (make-search HISTORY-1 "h"))
(check-expect (write-sp S-2 "x") (make-search HISTORY-2 "hellox"))
(check-expect (write-sp S-3 "\r") S-3)
(check-expect (write-sp S-3 "\b") (make-search HISTORY-2 "n"))
(check-expect (write-sp S-3 "\t") S-3)
(check-expect (write-sp S-4 "!") (make-search HISTORY-3 "yes!"))
(check-expect (write-sp S-4 "f1") (make-viewall "" HISTORY-3))
(check-expect (write-sp S-4 "f2") S-4)


(define (write-sp ws key)
  (if (string=? key "f1")
      (make-viewall "" (search-history ws))
      (if (= (string-length key) 1)
          (cond
            [(string=? key "\b") (make-search (search-history ws)
                                              (backspace (search-search ws)))]
            [(string=? key "\r") ws]
            [(string=? key "\t") ws]
            [else (make-search (search-history ws)
                               (string-append (search-search ws) key))])
          ws)))

; Define backspace behavior
; backspace : String -> String

(check-expect (backspace "hi") "h")
(check-expect (backspace "") "")
(check-expect (backspace "h") "")

(check-expect (backspace "") "")
(check-expect (backspace "qwert") "qwer")

(define (backspace str)
  (cond
    [(string=? str "") str]
    [else (substring str
                     0
                     (- (string-length str) 1))]))

; Send message being typed to server, and remove it from the world state
; send-message : EditviewPosts -> Package (of Forum and ClientMsg)

#;(check-expect (send-message WORLD-1) WORLD-1)
#;(check-expect (send-message WORLD-2)
                (make-package (make-editview ""
                                             HISTORY-2
                                             "hello")
                              (list "POST" "Hello World!")))
#;(check-expect (send-message (make-editview "CATCHUP" '() ""))
                (make-package (make-editview "" '() "")
                              "CATCHUP"))

(define (send-message ws)
  (if (number? (newitem-id ws))
      (make-package (make-viewall "" (newitem-history ws))
                    (list "REPLY" (newitem-id ws) (newitem-edit ws)))
      (make-package (make-viewall "" (newitem-history ws))
                    (list "POST" (newitem-edit ws)))))

; Handle received message from server
; receive-message : World ServerMsg -> World

(check-expect (receive-message VA-1 SM-3)
              (make-viewall "ERROR: You done goofed!"
                            '()))
(check-expect (receive-message VA-2 SM-1)
              (make-viewall "Hello"
                            (cons (list 0 (make-post "John" "hello world" '()))
                                  (viewall-history VA-2))))
(check-expect (receive-message VA-2 SM-2)
              (make-viewall "Hello"
                            (list (list 0 (make-post "John Doe"
                                                     "hello world"
                                                     (list (make-reply "Jane" "Goodbye!"))))
                                  POST-2 POST-3)))

(define (receive-message ws sm)
  (cond
    [(viewall? ws) (va-receive ws sm)]
    [(threadview? ws) (tv-receive ws sm)]
    [(newitem? ws) (ni-receive ws sm)]
    [(search? ws) (sp-receive ws sm)]))

; Handle received message when world is a Viewall
; va-receive : Viewall ServerMsg -> Viewall

(check-expect (va-receive VA-1 SM-3)
              (make-viewall "ERROR: You done goofed!"
                            '()))
(check-expect (va-receive VA-2 SM-1)
              (make-viewall "Hello"
                            (cons (list 0 (make-post "John" "hello world" '()))
                                  (viewall-history VA-2))))
(check-expect (va-receive VA-2 SM-2)
              (make-viewall "Hello"
                            (list (list 0 (make-post "John Doe"
                                                     "hello world"
                                                     (list (make-reply "Jane" "Goodbye!"))))
                                  POST-2 POST-3)))

(define (va-receive ws sm)
  (cond
    [(string=? "ERROR" (first sm))
     (make-viewall (string-append (first sm) ": " (second sm))
                   (viewall-history ws))]
    [(string=? "REPLY" (first sm))
     (make-viewall (viewall-edit ws)   
                   (add-reply sm (viewall-history ws)))]
    [else
     (make-viewall (viewall-edit ws)
                   (add-post sm (viewall-history ws)))]))

; Handle received message when world is a Threadview
; tv-receive : Threadview ServerMsg -> Threadview

(check-expect (tv-receive TV-2 SM-1)
              (make-threadview POST-4
                               (cons (list 0 (make-post "John" "hello world" '()))
                                     (threadview-history TV-2))))
(check-expect (tv-receive TV-2 SM-2)
              (make-threadview POST-4
                               (list (list 0 (make-post "John Doe"
                                                        "hello world"
                                                        (list (make-reply "Jane" "Goodbye!"))))
                                     POST-2 POST-3 POST-4)))

(define (tv-receive ws sm)
  (cond
    [(string=? "ERROR" (first sm))
     (make-threadview (string-append (first sm) ": " (second sm))
                      (threadview-history ws))]
    [(string=? "REPLY" (first sm))
     (make-threadview (threadview-post ws)
                      (add-reply sm (threadview-history ws)))]
    [else
     (make-threadview (threadview-post ws)
                      (add-post sm (threadview-history ws)))]))

; Handle received message when world is a Newitem
; ni-receive : Newitem ServerMsg -> Newitem

(check-expect (ni-receive NI-1 SM-3)
              (make-newitem #false
                            "ERROR: You done goofed!"
                            '()))
(check-expect (ni-receive NI-2 SM-1)
              (make-newitem 0
                            "Goodbye!"
                            (cons (list 0 (make-post "John" "hello world" '()))
                                  (newitem-history NI-2))))
(check-expect (ni-receive NI-2 SM-2)
              (make-newitem 0
                            "Goodbye!"
                            (list (list 0 (make-post "John Doe"
                                                     "hello world"
                                                     (list (make-reply "Jane" "Goodbye!"))))
                                  POST-2 POST-3)))

(define (ni-receive ws sm)
  (cond
    [(string=? "ERROR" (first sm))
     (make-newitem (newitem-id ws)
                   (string-append (first sm) ": " (second sm))
                   (newitem-history ws))]
    [(string=? "REPLY" (first sm))
     (make-newitem (newitem-id ws)
                   (newitem-edit ws)
                   (add-reply sm (newitem-history ws)))]
    [else
     (make-newitem (newitem-id ws)
                   (newitem-edit ws)
                   (add-post sm (newitem-history ws)))]))

; Handle received message when world is a SearchPosts
; sp-receive : SearchPosts ServerMsg -> World

(check-expect (sp-receive S-1 SM-3)
              (make-search '()
                           "ERROR: You done goofed!"))
(check-expect (sp-receive S-2 SM-1)
              (make-search (cons (list 0 (make-post "John" "hello world" '()))
                                 (viewall-history VA-2))
                           "hello"))
(check-expect (sp-receive S-2 SM-2)
              (make-search (list (list 0 (make-post "John Doe"
                                                    "hello world"
                                                    (list (make-reply "Jane" "Goodbye!"))))
                                 POST-2 POST-3)
                           "hello"))

(define (sp-receive ws sm)
  (cond
    [(string=? "ERROR" (first sm))
     (make-search (search-history ws)
                  (string-append (first sm) ": " (second sm)))]
    [(string=? "REPLY" (first sm))
     (make-search (add-reply sm (search-history ws))
                  (search-search ws))]
    [else
     (make-search (add-post sm (search-history ws))
                  (search-search ws))]))

; Parses the new message and adds it to the given history
; add-post : ServerMsg History -> History

(check-expect (add-post SM-1 HISTORY-2)
              (cons (list 0 (make-post "John" "hello world" '())) HISTORY-2))

(define (add-post sm history)
  (cond
    [(string=? "POST" (first sm)) (cons (list (second sm)
                                              (make-post (third sm) (fourth sm) '()))
                                        history)]
    [(string=? "REPLY" (first sm)) history]
    [(string=? "ERROR" (first sm)) history]))

; Adds reply to relevant post in history
; add-reply : ServerMsg History -> History

(check-expect (add-reply (list "REPLY" 1 "Author1" "Hello") HISTORY-2)
              (list POST-1
                    (list 1 (make-post "Jane Doe"
                                       "Goodbye!"
                                       (list (make-reply "Author1" "Hello"))))
                    POST-3))
(check-expect (add-reply (list "REPLY" 4 "Author2" "Goodbye") HISTORY-2)
              HISTORY-2)

(define (add-reply sm hist)
  (local ((define old-post (assoc (second sm) hist)))
    (if (boolean? old-post)
        hist
        (replace-post hist (list (first old-post)
                                 (make-post (post-author (second old-post))
                                            (post-content (second old-post))
                                            (cons (make-reply (third sm) (fourth sm))
                                                  (post-replies (second old-post)))))))))

; Given a history, use the given post to replace the post whose ID# matches the given post
; replace-post : History Post -> History

(check-expect (replace-post HISTORY-2 (list 1 (make-post "New Author" "New Content" '())))
              (list POST-1
                    (list 1 (make-post "New Author" "New Content" '()))
                    POST-3))
(check-expect (replace-post HISTORY-2 (list 3 (make-post "New Author" "New Content" '())))
              HISTORY-2)
(check-expect (replace-post HISTORY-1 POST-1) HISTORY-1)

(define (replace-post hist new-post)
  (local (; Check if given post ID# matches ID# of new-post. If yes, replace with new-post
          ; swap-post-if-match : Post -> Post
          ; (swap-post-if-match (list 1 (make-post "Old" "Old" '()))
          ;                     (list 1 (make-post "New" "New" '())))
          ; -> (list 1 (make-post "New" "New" '()))
          ; (swap-post-if-match (list 1 (make-post "Old" "Old" '()))
          ;                     (list 0 (make-post "New" "New" '())))
          ; -> (list 0 (make-post "Old" "Old" '()))
          (define (swap-post-if-match post)
            (if (= (first post) (first new-post))
                new-post
                post)))
    (map swap-post-if-match hist)))