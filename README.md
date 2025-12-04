# Nomadnet ORM Framework

A simple, learnable ORM (Object-Relational Mapper) for building Nomadnet applications with SQLite in Chicken Scheme.

## Project Structure

```
workspace/
├── src/                    # Source code
│   ├── orm.scm            # CLI tool for table generation
│   ├── orm-lib.scm        # Runtime ORM library
│   ├── models.scm         # Model definitions
│   └── micron-dsl.scm     # Micron markup DSL
│
├── pages/                  # Example Nomadnet page
│   ├── index.mu           # Main page (ORM-powered comments)
│   └── app/
│       ├── actions/       # Form handlers
│       │   └── handle_comment.scm
│       └── templates/     # Page templates
│           └── comments.scm
│
├── docs/                   # Documentation and examples
│   ├── README.md          # Complete learning guide
│   ├── QUICK_START.md     # Quick reference
│   ├── AUTO_CONSTRUCTORS.md
│   ├── DB_CONNECTION_EXPLAINED.md
│   ├── DB_LIST_EXPLAINED.md
│   ├── PARAMETER_SUMMARY.md
│   └── *.scm             # Runnable examples
│
├── app.db                 # SQLite database
└── README.md             # This file
```

## Quick Start

### 1. Generate Database Tables

```bash
cd workspace
csi -s src/orm.scm --generate
```

### 2. Run Example Page

```bash
csi -s pages/index.mu
```

### 3. Try Examples

```bash
# Run any example from docs/
csi -s docs/full-crud-demo.scm
csi -s docs/test-db-list.scm
```

## Core API

### Models (src/models.scm)

Define your data structures as alists:

```scheme
(define comment-model
  '((name . comment)
    (fields . (
      ((name . id)
       (type . integer)
       (primary-key . #t)
       (autoincrement . #t))
      ((name . text)
       (type . text))))))

(define all-models (list comment-model ...))
```

### Create (INSERT)

```scheme
(load "src/orm-lib.scm")

(db-open "app.db")

;; Constructors are auto-generated from models!
(define my-comment
  (make-comment
    '((name . "Alice")
      (page-name . "index")
      (text . "Hello!"))))

(db-save my-comment)

(db-close)
```

### Read (SELECT)

```scheme
;; Get all
(db-list 'comment)

;; Filter by field
(db-list 'comment '((page-name . "index")))

;; Multiple filters (AND)
(db-list 'comment '((name . "Alice") (page-name . "index")))
```

### Working with Results

Results are alists - easy to process:

```scheme
(define comments (db-list 'comment '((page-name . "index"))))

;; Access fields
(alist-ref 'name (car comments))
(alist-ref 'text (car comments))

;; Map/filter
(map (lambda (c) (alist-ref 'text c)) comments)
(filter (lambda (c) (> (alist-ref 'id c) 5)) comments)
```

## Features

- ✅ **Automatic table generation** from model definitions
- ✅ **Auto-generated constructors** - add a model, get `make-<model>` for free
- ✅ **Simple INSERT** with `db-save`
- ✅ **Flexible SELECT** with `db-list` and filtering
- ✅ **SQL injection protection** via parameterized queries
- ✅ **Thread-safe** database connection management
- ✅ **Micron DSL** for generating terminal-friendly markup

## Example: Nomadnet Page with Comments

The `pages/` folder contains a fully functional Nomadnet page:

- **index.mu** - Main page that displays comments from database
- **comments.scm** - Template that queries and renders comments
- **handle_comment.scm** - Form handler that saves new comments

All powered by the ORM!

## Documentation

See `docs/` for comprehensive guides:

- **README.md** - Complete learning guide with all concepts explained
- **QUICK_START.md** - API reference and common patterns
- **AUTO_CONSTRUCTORS.md** - How automatic code generation works
- **DB_CONNECTION_EXPLAINED.md** - Understanding parameters
- **DB_LIST_EXPLAINED.md** - SELECT queries in depth

## Learning Examples

The `docs/` folder includes runnable examples:

- `full-crud-demo.scm` - Complete Create + Read workflow
- `test-db-list.scm` - Comprehensive query tests
- `nomadnet-page-example.scm` - Page rendering with comments
- `parameters-explained.scm` - Understanding Scheme parameters
- `constructor-gen.scm` - Dynamic function generation
- And more!

## Key Concepts Covered

This project teaches essential Scheme concepts through practical use:

- **Association lists** (alists) - Simple key-value data structures
- **Parameters** - Thread-safe global state management
- **Quasiquote & eval** - Metaprogramming and code generation
- **Variadic functions** - Functions with optional arguments
- **Higher-order functions** - map, filter, for-each
- **SQL integration** - Safe database operations

## Design Philosophy

This ORM is intentionally **simple and learnable**:

- No magic - understand what every line does
- Clear error messages
- Extensive inline documentation
- Learning-focused examples
- Minimal abstractions

Perfect for learning Scheme while building real applications!

## Requirements

- Chicken Scheme 5.x
- sql-de-lite egg
- srfi-1, srfi-13, srfi-19

## License

Use freely for your Nomadnet applications!

## Next Steps

1. Read `docs/README.md` for the complete learning guide
2. Try the examples in `docs/`
3. Modify `src/models.scm` to add your own models
4. Build your Nomadnet application!
