# ORM Documentation

A simple, learning-focused ORM for Nomadnet apps in Chicken Scheme.

## Quick Start

### 1. Define Your Models

Edit `pages/app/models.scm`:

```scheme
(define comment-model
  '((name . comment)
    (fields . (
      ((name . id) (type . integer) (primary-key . #t) (autoincrement . #t))
      ((name . text) (type . text))
      ((name . author) (type . text) (size . 32))))))

(define all-models (list comment-model))
```

### 2. Configure Database Path

Edit `pages/app/settings.scm` with absolute paths:

```scheme
(define db-path "/absolute/path/to/pages/app/app.db")
(define models-path "/absolute/path/to/pages/app/models.scm")
```

### 3. Generate Tables

```bash
csi -s framework/manage.scm --generate \
  --db-path /absolute/path/to/app.db \
  --models-path /absolute/path/to/models.scm
```

### 4. Use in Your Pages

```scheme
(import orm)
(load "app/settings.scm")

;; Initialize
(orm-init (app-models-path))

;; Open database
(db-open (app-db-path))

;; Create
(define comment (make-comment '((text . "Hello") (author . "Alice"))))
(db-save comment)

;; Read
(db-list 'comment)                          ; All comments
(db-list 'comment '((author . "Alice")))    ; Filtered

;; Close
(db-close)
```

## Core Concepts

### Models as Alists
Everything is an alist (association list):
```scheme
'((name . comment)
  (fields . (...)))
```

### Auto-Generated Constructors
The ORM generates `make-<model>` functions automatically:
```scheme
(make-comment '((text . "Hello")))  ; Created from comment-model
```

### Instances
Returned instances are alists with a special `__model__` key:
```scheme
'((__model__ . comment)
  (id . 1)
  (text . "Hello")
  (author . "Alice"))
```

Access fields with `alist-ref`:
```scheme
(alist-ref 'text instance)  ; Returns "Hello"
```

### Parameters
Thread-safe configuration using parameters:
```scheme
(app-db-path)      ; Get configured database path
(app-models-path)  ; Get configured models path
```

## Limitations (By Design)

- **No UPDATE/DELETE** - Keeps it simple for learning
- **No JOINs** - Single table queries only
- **Equality filters only** - No `<`, `>`, `LIKE`
- **AND filters only** - No OR (filter in Scheme instead)

**For complex queries, use raw SQL** - See `pages/app/templates/recent-comments.scm`

## Files in This Folder

- `01-basic-usage.scm` - Simple CRUD example
- `02-filtering.scm` - How to filter results
- `03-raw-sql.scm` - Bypass ORM with sql-de-lite
- `parameters.md` - Understanding Scheme parameters
- `constructors.md` - How auto-constructors work
