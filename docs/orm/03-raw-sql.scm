#!/usr/bin/env -S csi -s

;;; 03-raw-sql.scm - Using raw SQL with sql-de-lite
;;;
;;; When the ORM isn't enough, use sql-de-lite directly for full SQL power

(import sql-de-lite)
(import srfi-13)  ; For string operations

(load "pages/app/settings.scm")

;; === WHY RAW SQL? ===

(print "Use raw SQL when you need:")
(print "  - ORDER BY, LIMIT, OFFSET")
(print "  - Aggregations (COUNT, SUM, AVG)")
(print "  - JOINs across tables")
(print "  - Complex WHERE clauses (<, >, LIKE, OR)")
(newline)

;; === BASIC RAW SQL ===

(print "=== Example 1: ORDER BY and LIMIT ===")

(let* ((db (open-database (app-db-path)))
       ;; Write your SQL query with ? for parameters
       (query "SELECT id, name, text FROM comment ORDER BY id DESC LIMIT ?")
       ;; Execute and fetch all rows
       (rows (query fetch-all (sql db query) 5)))

  (print "Last 5 comments:")
  (for-each
    (lambda (row)
      (let ((id (list-ref row 0))
            (name (list-ref row 1))
            (text (list-ref row 2)))
        (print "  [" id "] " name ": " text)))
    rows)

  (close-database db))
(newline)

;; === AGGREGATIONS ===

(print "=== Example 2: COUNT by page ===")

(let* ((db (open-database (app-db-path)))
       (query "SELECT page_name, COUNT(*) as count
               FROM comment
               GROUP BY page_name
               ORDER BY count DESC")
       (rows (query fetch-all (sql db query))))

  (print "Comments per page:")
  (for-each
    (lambda (row)
      (let ((page (list-ref row 0))
            (count (list-ref row 1)))
        (print "  " page ": " count " comments")))
    rows)

  (close-database db))
(newline)

;; === COMPLEX WHERE CLAUSES ===

(print "=== Example 3: LIKE and wildcards ===")

(let* ((db (open-database (app-db-path)))
       (query "SELECT name, text FROM comment WHERE text LIKE ? LIMIT 10")
       (rows (query fetch-all (sql db query) "%framework%")))

  (print "Comments mentioning 'framework':")
  (for-each
    (lambda (row)
      (let ((name (list-ref row 0))
            (text (list-ref row 1)))
        (print "  " name ": " text)))
    rows)

  (close-database db))
(newline)

;; === COMPARISON: ORM vs RAW SQL ===

(print "=== Comparison ===")
(print "")
(print "ORM returns alists:")
(print "  '((__model__ . comment) (id . 1) (name . \"Alice\") ...)")
(print "  Access: (alist-ref 'name instance)")
(print "")
(print "Raw SQL returns lists:")
(print "  '((1 \"Alice\" \"Hello\") (2 \"Bob\" \"Hi\"))")
(print "  Access: (list-ref row 0)  ; column 0")
(print "")
(print "Choose based on your needs:")
(print "  - Simple CRUD → Use ORM")
(print "  - Complex queries → Use raw SQL")

;; For a complete example, see:
;; pages/app/templates/recent-comments.scm
