#!/usr/bin/env -S csi -s

;;; 02-filtering.scm - Filtering database queries
;;;
;;; Shows how to filter results using the ORM's simple filter syntax

(import orm)
(import srfi-1)  ; For list operations

(load "pages/app/settings.scm")
(orm-init (app-models-path))
(db-open (app-db-path))

;; === FILTERING BASICS ===

;; Get all comments (no filter)
(print "=== All Comments ===")
(define all-comments (db-list 'comment))
(print "Total: " (length all-comments))
(newline)

;; Filter by single field (equality only)
(print "=== Comments on 'index' page ===")
(define index-comments
  (db-list 'comment '((page-name . "index"))))
(print "Found: " (length index-comments))
(for-each
  (lambda (c)
    (print "  " (alist-ref 'name c) ": " (alist-ref 'text c)))
  index-comments)
(newline)

;; Filter by multiple fields (AND only)
(print "=== Comments by Alice on 'index' ===")
(define alice-index
  (db-list 'comment '((page-name . "index")
                      (name . "Alice"))))
(print "Found: " (length alice-index))
(for-each
  (lambda (c)
    (print "  " (alist-ref 'text c)))
  alice-index)
(newline)

;; === WORKAROUNDS FOR LIMITATIONS ===

;; The ORM doesn't support OR, <, >, LIKE, etc.
;; Solution: Fetch all and filter in Scheme

(print "=== Workaround: Filter in Scheme ===")

;; Example: Find comments with "great" in text (case-insensitive)
(define (text-contains? comment substring)
  (let ((text (alist-ref 'text comment)))
    (and text
         (string-contains-ci text substring))))

(define great-comments
  (filter (lambda (c) (text-contains? c "great"))
          all-comments))

(print "Comments containing 'great': " (length great-comments))
(for-each
  (lambda (c)
    (print "  " (alist-ref 'name c) ": " (alist-ref 'text c)))
  great-comments)
(newline)

;; Example: Find comments by multiple authors (OR logic)
(define (by-authors? comment authors)
  (member (alist-ref 'name comment) authors))

(define multi-author-comments
  (filter (lambda (c) (by-authors? c '("Alice" "Bob")))
          all-comments))

(print "Comments by Alice OR Bob: " (length multi-author-comments))
(newline)

;; === BEST PRACTICES ===
(print "=== Tips ===")
(print "1. Use ORM filters for simple equality checks")
(print "2. Use Scheme's filter/map for complex logic")
(print "3. Use raw SQL for advanced queries (see 03-raw-sql.scm)")

(db-close)
