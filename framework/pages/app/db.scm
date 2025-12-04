#!/usr/bin/env -S csi -s

(import sql-de-lite)
(import (chicken string))

(define db-name "comments.sqlite3")

;; 1. Create database with table
(define (create-db)
  (let ((db (open-database db-name)))
    (exec (sql db "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT(16), text TEXT)"))
    (close-database db)))

;; 2. Insert recipe
(define (insert-comment comment)
  (let ((db (open-database db-name)))
    (apply exec (cons (sql db "INSERT INTO comments (name, text) VALUES (?,?)") comment))
    (let ((id (last-insert-rowid db)))
      (close-database db)
      id)))

;; 3. Get recipe by ID
(define (get-comment id)
  (let ((db (open-database db-name)))
    (let ((result (query fetch-value (sql db "SELECT text FROM comments WHERE id = ?;") id)))
      (close-database db)
      (if (null? result) #f result))))


;; Use the three functions
(create-db)                               ; Creates table
(print (conc "Inserted ID: " (insert-comment '("Bob" "suck me"))))
(print (conc "Retrieved comment: " (get-comment 1)))