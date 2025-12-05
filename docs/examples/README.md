# Complete Examples

Full working examples combining ORM, Micron DSL, and Markdown.

## Files

- `complete-page.scm` - Full page showing all features
- `blog-system.scm` - Simple blog with database
- `guestbook.scm` - Guestbook with comments

## Running Examples

All examples expect to be run from the project root with proper paths configured.

### Setup

1. Configure absolute paths in `pages/app/settings.scm`
2. Generate database tables:
   ```bash
   csi -s framework/manage.scm --generate \
     --db-path /absolute/path/to/app.db \
     --models-path /absolute/path/to/models.scm
   ```

### Run

```bash
cd pages
csi -s ../docs/examples/complete-page.scm
```

## Learning Path

1. Start with `complete-page.scm` - See all features together
2. Study `blog-system.scm` - Understand data persistence
3. Build your own based on `guestbook.scm`
