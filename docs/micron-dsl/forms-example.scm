#!/usr/bin/env -S csi -s

;;; forms-example.scm - Interactive form building with Micron DSL

(import micron)

;; Helper to create styled input fields
(define (input-row label fieldname size)
  (conc
    (style '(fg "aaa"))
    label ": "
    (reset-style)
    (style '(bg "333" fg "fff"))
    (input-field-fixed fieldname size)
    (reset-style)
    nl))

(print
  (section "Contact Form Example")
  nl nl

  ;; Form introduction
  (style '(fg "ddd"))
  "Fill out the form below to get in touch."
  (reset-style)
  nl nl

  ;; Form fields
  (input-row "Name" "user_name" 24)
  (input-row "Email" "user_email" 32)
  (input-row "LXMF Address (optional)" "user_lxmf" 32)
  nl

  ;; Multi-line concept (Nomadnet limitation: no textarea, use regular field)
  (style '(fg "aaa"))
  "Message:"
  (reset-style)
  nl
  (style '(bg "333" fg "fff"))
  (input-field-fixed "message" 64)
  (reset-style)
  nl nl

  ;; Submit button
  (style '(bg "373" fg "fff"))
  (submit-field "Send Message" "/app/actions/contact.scm" "contact"
                "user_name" "user_email" "user_lxmf" "message")
  (reset-style)
  nl nl

  ;; Form notes
  (style '(fg "888"))
  "Note: All fields use " (code "input-field-fixed") " with specified width."
  nl
  "The submit button passes all field names to the handler script."
  (reset-style))

;; === FORM PROCESSING EXAMPLE ===
;;
;; In your handler script (/app/actions/contact.scm):
;;
;; (import (chicken process-context))
;;
;; (define name (get-environment-variable "field_user_name"))
;; (define email (get-environment-variable "field_user_email"))
;; (define lxmf (get-environment-variable "field_user_lxmf"))
;; (define message (get-environment-variable "field_message"))
;;
;; ;; Process the form data
;; ;; (save to database, send message, etc.)
;;
;; ;; Display confirmation
;; (print "Thank you, " name "!")
;; (print "Your message has been received.")
