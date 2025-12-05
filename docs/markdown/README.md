# Markdown Converter Documentation

Convert Markdown to Micron markup on-the-fly in your Nomadnet pages.

## Why Use Markdown?

- **Faster to write** than raw micron markup
- **Familiar syntax** - standard Markdown
- **Focus on content** not formatting
- **Convert at load time** - no build step needed

## Quick Start

```scheme
(import markdown)

;; From string
(print
  (markdown->micron "# Hello\n\nThis is **bold** text."))

;; From file
(print
  (markdown-file->micron "content/post.md"))
```

## Supported Markdown

### Headers

```markdown
# H1 Header        → > H1 Header
## H2 Header       → >> H2 Header
### H3 Header      → >>> H3 Header
```

### Emphasis

```markdown
**bold text**      → `!bold text`!
*italic text*      → `*italic text`*
__also bold__      → `!also bold`!
_also italic_      → `*also italic`*
```

### Lists

```markdown
- Item one         → • Item one
- Item two         → • Item two
  - Nested         →   • Nested

1. First           → 1. First
2. Second          → 2. Second
```

### Links

```markdown
[Link text](url)   → `[Link text`url]
```

### Code

```markdown
`inline code`      → `=inline code`=

```
code block
```                → `=code block`=
```

### Horizontal Rules

```markdown
---
***
___                → (divider)
```

## API Functions

### markdown->micron

Convert a markdown string to micron markup.

```scheme
(markdown->micron markdown-string)
```

**Alias**: `md->micron`

### markdown-file->micron

Load markdown from a file and convert it.

```scheme
(markdown-file->micron "path/to/file.md")
```

**Alias**: `md-file->micron`

## Complete Example

```scheme
#!/usr/bin/env -S csi -s

(import micron)
(import markdown)

(print
  ;; Static header using micron
  (style '(align center))
  (section "My Blog")
  (style '(align left))
  nl nl

  ;; Dynamic content from markdown file
  (markdown-file->micron "posts/2025-12-04.md")

  nl nl

  ;; Static footer
  (style '(fg "5af"))
  (link "/page/index.mu" "Back to Home")
  (reset-style))
```

## Markdown File Example

```markdown
## New Feature Release

We're excited to announce **version 2.0** of our framework!

### What's New

- Improved performance
- Better documentation
- New examples

Check out the [GitHub repo](https://github.com/example/repo) for details.

### Installation

Run the following command:

```
npm install framework@2.0
```

---

*Posted on December 4, 2025*
```

## Limitations

The converter handles common Markdown but not:
- Tables
- Footnotes
- Task lists `- [ ]`
- Complex nested structures
- HTML passthrough

For these, use the Micron DSL directly.

## Best Practices

### Separate Content from Code

```
pages/
  blog.mu           ← Scheme code with micron
  content/
    post1.md        ← Pure markdown content
    post2.md
```

```scheme
;; In blog.mu
(print
  (header)
  (markdown-file->micron "content/post1.md")
  (footer))
```

### Mix and Match

```scheme
(print
  ;; Micron for structure
  (section "Article")
  nl

  ;; Markdown for content
  (markdown-file->micron "article.md")

  nl nl

  ;; Micron for interactive elements
  (submit-field "Comment" "/submit.scm" "page" "comment"))
```

## Files in This Folder

- `example-simple.md` - Basic markdown example
- `example-blog.md` - Blog post example
- `converter-demo.scm` - Usage demonstration
