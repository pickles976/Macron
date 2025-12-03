#!/usr/bin/env -S csi -s

(import (chicken io))
(import (chicken string))


(define squiggle "-∿")
(define newline "  \n")

(define (italics . inners) 
  (conc "`*" (apply conc inners) "`*"))

(define (bold . inners) 
  (conc "`!" (apply conc inners) "`!"))

(define align-center "`c")
(define align-left "`a")

(define (set-bg-color color)
  (conc "`B" color))

(define (set-fg-color color)
  (conc "`F" color))

(define (link src label)
  (conc "`[" label "`" src "]"))

(define (h . inners) (conc ">" (apply conc inners)))

(define (input-field fieldname) 
  (conc "`<" fieldname "`>"))

(define (input-field-fixed fieldname size) 
  (conc "`<" size "|" fieldname "`>"))


;; TODO: make this simpler
(define (submit-field label dest page . fields)
  (let ((field-str (string-intersperse fields "|")))
    (conc "`[" label "`:" dest "`" field-str "|post_id=" page "]")))

(define (my-comment-field label fieldname size)
  (conc (set-bg-color "333") (set-bg-color "444") (input-field-fixed fieldname size) (set-bg-color "333") label newline))

(print
    squiggle newline
    align-center (bold "Hello! ") "This is output from " (italics "micron") newline
    squiggle newline
    ">Main" newline
    align-left
    (link "lxmf@exampleaddress" "Link Example") newline
    align-center newline
    ">>Comments" newline
    ">>Leave a Comment" newline

    (set-fg-color "aaa") align-left
    (my-comment-field " Name " "user_name" 16)
    (my-comment-field " LXMF Address (optional)" "user_name" 32)
    (my-comment-field " Comment " "user_name" 64)
    newline
    (set-bg-color "373") (submit-field "Submit" "/app/handle_comment.mu" "index" "user_name" ) (set-bg-color "333"))