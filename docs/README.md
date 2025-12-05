# Macron Documentation

Complete documentation for building Nomadnet applications with Macron.

## What is Macron?

Macron is a learning-focused framework for building interactive Nomadnet pages using Chicken Scheme. It provides:

- **Micron DSL** - Generate micron markup with Scheme functions
- **Markdown Converter** - Write content in Markdown, render as Micron
- **Simple ORM** - SQLite database integration with minimal boilerplate
- **Learning-First** - Designed to teach Scheme concepts through practical use

## Documentation Structure

### [ORM](./orm/)
Database integration and CRUD operations.

**Key Files:**
- `README.md` - ORM overview and quick start
- `01-basic-usage.scm` - Creating and reading data
- `02-filtering.scm` - Querying with filters
- `03-raw-sql.scm` - Advanced queries with sql-de-lite
- `parameters.md` - Understanding Scheme parameters
- `constructors.md` - How auto-constructors work

**Topics Covered:**
- Model definition as alists
- Auto-generated constructors
- Database operations (CREATE, READ)
- Filtering and workarounds
- Raw SQL for complex queries

### [Micron DSL](./micron-dsl/)
Generate Micron markup programmatically.

**Key Files:**
- `README.md` - DSL overview and API reference
- `demo.scm` - Complete feature demonstration
- `forms-example.scm` - Interactive form building
- `styling-guide.md` - Advanced styling patterns

**Topics Covered:**
- Text styling (bold, italic, colors)
- Links and navigation
- Forms and user input
- Layout control
- Color schemes and patterns

### [Markdown](./markdown/)
Convert Markdown to Micron on-the-fly.

**Key Files:**
- `README.md` - Converter overview
- `example-simple.md` - Basic markdown example
- `example-blog.md` - Blog post example
- `converter-demo.scm` - Conversion examples

**Topics Covered:**
- Supported Markdown syntax
- String and file conversion
- Mixing Markdown with Micron DSL
- Best practices for content management

### [Examples](./examples/)
Complete working applications.

**Key Files:**
- `README.md` - Examples overview
- `complete-page.scm` - All features together
- `guestbook.scm` - Simple interactive app

**Topics Covered:**
- Full page structure
- Database integration
- Form handling
- Real-world patterns

## Learning Path

### Beginner

1. **Start with Micron DSL** - Learn basic page generation
   - Read: `micron-dsl/README.md`
   - Run: `micron-dsl/demo.scm`
   - Practice: Create a simple static page

2. **Add Markdown** - Make content easier to write
   - Read: `markdown/README.md`
   - Run: `markdown/converter-demo.scm`
   - Practice: Convert a markdown file to a page

### Intermediate

3. **Understand the ORM** - Add data persistence
   - Read: `orm/README.md` and `orm/parameters.md`
   - Run: `orm/01-basic-usage.scm`
   - Practice: Create a model and save data

4. **Work with Forms** - Add interactivity
   - Read: `micron-dsl/forms-example.scm`
   - Study: `pages/app/actions/handle_comment.scm`
   - Practice: Build a contact form

### Advanced

5. **Build Complete Apps** - Put it all together
   - Study: `examples/complete-page.scm`
   - Build: `examples/guestbook.scm`
   - Create: Your own application

6. **Use Raw SQL** - Handle complex queries
   - Read: `orm/03-raw-sql.scm`
   - Study: `pages/app/templates/recent-comments.scm`
   - Practice: Write advanced queries

## Quick Reference

### File Structure

```
pages/
  index.mu                  # Main page
  subpages/                 # Additional pages
  app/
    settings.scm            # Configuration (absolute paths)
    models.scm              # Database models
    actions/                # Form handlers
    templates/              # Reusable components
    markdown/               # Markdown content files

framework/
  manage.scm                # Database table generation
  orm-lib.scm               # ORM runtime
  micron.scm                # Micron DSL module
  markdown.scm              # Markdown converter
```

### Common Commands

```bash
# Generate database tables
csi -s framework/manage.scm --generate \
  --db-path /absolute/path/to/app.db \
  --models-path /absolute/path/to/models.scm

# Run a page
cd pages
csi -s index.mu

# Test an example
csi -s docs/examples/complete-page.scm

# Check database
sqlite3 /path/to/app.db
```

### Essential Imports

```scheme
(import micron)          ; Micron DSL
(import markdown)        ; Markdown conversion
(import orm)             ; Database ORM

;; Chicken Scheme standard library
(import (chicken file))
(import (chicken process-context))
(import (chicken string))

;; SRFI libraries
(import srfi-1)          ; List operations
(import srfi-13)         ; String operations
(import srfi-19)         ; Date/time

;; Direct SQL access
(import sql-de-lite)
```

## Configuration

### Settings (pages/app/settings.scm)

Set absolute paths for database and models:

```scheme
(define db-path "/home/user/project/pages/app/app.db")
(define models-path "/home/user/project/pages/app/models.scm")

(define app-db-path (make-parameter db-path))
(define app-models-path (make-parameter models-path))
```

### Models (pages/app/models.scm)

Define database schemas as alists:

```scheme
(define comment-model
  '((name . comment)
    (fields . (
      ((name . id) (type . integer) (primary-key . #t) (autoincrement . #t))
      ((name . text) (type . text))
      ((name . author) (type . text) (size . 32))))))

(define all-models (list comment-model))
```

## Getting Help

- **Read the code** - Framework files are heavily commented
- **Run examples** - All examples are runnable and documented
- **Check subpages** - `pages/subpages/` has interactive guides
- **Experiment** - The framework is designed for learning by doing

## Philosophy

Macron prioritizes:

1. **Simplicity** - Easy to understand, minimal magic
2. **Learning** - Every feature teaches a Scheme concept
3. **Practicality** - Real applications, not toy examples
4. **Iteration** - Start simple, add complexity as needed

## What's Next?

- Study the topic that interests you most
- Run the examples to see them in action
- Build something small to practice
- Gradually combine features as you learn

Happy hacking on the mesh! 🚀
