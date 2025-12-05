#!/usr/bin/env -S csi -s

;;; 01-basic-usage.scm - Basic ORM CRUD operations
;;;
;;; This example shows the fundamental ORM operations:
;;; - Initialize the ORM
;;; - Create instances (INSERT)
;;; - Read instances (SELECT)

(import orm)

;; Step 1: Load settings with absolute paths
(load "pages/app/settings.scm")

;; Step 2: Initialize ORM - this loads models and generates constructors
(orm-init (app-models-path))

;; Step 3: Open database connection
(db-open (app-db-path))

;; Step 4: CREATE - Insert new records
(print "Creating comments...")

(define comment1
  (make-comment '((name . "Alice")
                  (text . "First comment!")
                  (page-name . "index")
                  (timestamp . "2025-12-04 10:00")
                  (address . ""))))

(define comment2
  (make-comment '((name . "Bob")
                  (text . "Great framework!")
                  (page-name . "index")
                  (timestamp . "2025-12-04 11:00")
                  (address . "abc123"))))

(db-save comment1)
(db-save comment2)
(print "✓ Saved 2 comments")
(newline)

;; Step 5: READ - Retrieve records
(print "Reading all comments...")
(define all-comments (db-list 'comment))
(print "Found " (length all-comments) " total comments")
(newline)

;; Access fields using alist-ref
(print "Comment details:")
(for-each
  (lambda (comment)
    (print "  ID: " (alist-ref 'id comment))
    (print "  Name: " (alist-ref 'name comment))
    (print "  Text: " (alist-ref 'text comment))
    (newline))
  all-comments)

;; Step 6: Close database connection
(db-close)

(print "Done! Check your database with:")
(print "  sqlite3 " (app-db-path))
(print "  sqlite> SELECT * FROM comment;")
