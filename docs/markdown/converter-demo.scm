#!/usr/bin/env -S csi -s

;;; converter-demo.scm - Markdown to Micron conversion examples

(import micron)
(import markdown)

(print
  (style '(align center))
  (section "Markdown Converter Demo")
  (style '(align left))
  nl nl

  ;; === EXAMPLE 1: Inline Markdown ===
  (subsection "Example 1: Inline Markdown")
  nl
  (style '(fg "888"))
  "Converting markdown from a string..."
  (reset-style)
  nl nl

  (markdown->micron "## This is a heading

This paragraph has **bold** and *italic* text.

- List item one
- List item two

Check out [this link](https://example.com)!")

  nl nl

  ;; === EXAMPLE 2: From File ===
  (subsection "Example 2: Loading from File")
  nl
  (style '(fg "888"))
  "Loading and converting example-simple.md..."
  (reset-style)
  nl nl

  (markdown-file->micron "docs/markdown/example-simple.md")

  nl nl

  ;; === EXAMPLE 3: Mixed Content ===
  (subsection "Example 3: Mixing Micron and Markdown")
  nl
  (style '(fg "888"))
  "You can mix micron DSL and markdown:"
  (reset-style)
  nl nl

  ;; Micron DSL for structure
  (style '(fg "5af"))
  "This is styled with Micron DSL"
  (reset-style)
  nl nl

  ;; Markdown for content
  (markdown->micron "This is **markdown content** with a [link](/page/index.mu).")

  nl nl

  ;; === TIPS ===
  (subsection "Tips")
  nl
  (style '(fg "888"))
  "• Use markdown for content-heavy pages"
  nl
  "• Use micron DSL for interactive elements"
  nl
  "• Mix both for best results"
  nl
  "• Markdown files are easier to edit"
  (reset-style)
  nl nl

  (style '(fg "5af"))
  (link "/page/index.mu" "Back to Home")
  (reset-style))
