# Advanced Styling Guide

Tips and patterns for effective Micron DSL usage.

## Color Schemes

### Recommended Palettes

**Light on Dark** (terminal-friendly):
```scheme
(style '(fg "ddd" bg "222"))  ; Light gray on dark gray
(style '(fg "5af" bg "000"))  ; Blue on black
(style '(fg "fff" bg "333"))  ; White on charcoal
```

**Accent Colors**:
```scheme
(style '(fg "5af"))  ; Light blue - links
(style '(fg "fa5"))  ; Orange - warnings
(style '(fg "5f5"))  ; Green - success
(style '(fg "f55"))  ; Red - errors
(style '(fg "888"))  ; Gray - secondary text
```

## Style Management

### Always Reset Styles

```scheme
;; ✅ Good
(style '(fg "f00"))
"Error message"
(reset-style)

;; ❌ Bad (affects everything after)
(style '(fg "f00"))
"Error message"
```

### Helper Functions

Create reusable styling functions:

```scheme
(define (error-text text)
  (conc (style '(fg "f00")) text (reset-style)))

(define (link-styled url label)
  (conc (style '(fg "5af")) (link url label) (reset-style)))

;; Usage
(print (error-text "Invalid input!"))
(print (link-styled "/page/home.mu" "Home"))
```

### Nested Styles

Styles don't nest - last style wins:

```scheme
(style '(fg "f00"))
(style '(bg "000"))  ; This includes fg too!
"Text"
(reset-style)
```

Always specify all attributes together:

```scheme
(style '(fg "f00" bg "000"))
"Red text on black background"
(reset-style)
```

## Layout Patterns

### Centered Header

```scheme
(style '(align center))
(section "Page Title")
(style '(align left))
```

### Two-Column Effect

Micron doesn't support true columns, but you can fake it:

```scheme
"Left column text       " "Right column text"
nl
"More left text         " "More right text"
```

### Boxed Content

```scheme
(divider)
(style '(bg "222" fg "fff"))
"  Important message here  "
(reset-style)
nl
(divider)
```

## Form Styling

### Consistent Input Fields

```scheme
(define (styled-input label name width)
  (conc
    label nl
    (style '(bg "333" fg "aaa"))
    (input-field-fixed name width)
    (reset-style)
    nl))

(styled-input "Username" "user" 16)
(styled-input "Password" "pass" 16)
```

### Button Styling

```scheme
;; Primary button
(style '(bg "373" fg "fff"))
(submit-field "Submit" "/action.scm" "page" "field1")
(reset-style)

;; Secondary button (lighter background)
(style '(bg "444" fg "ddd"))
(submit-field "Cancel" "/back.mu" "page")
(reset-style)
```

## Performance Tips

### Use `conc` for Building Strings

```scheme
;; ✅ Efficient
(define content
  (conc
    (style '(fg "5af"))
    (link "/page.mu" "Click here")
    (reset-style)))

;; ❌ Less efficient (multiple string operations)
(string-append
  (style '(fg "5af"))
  (link "/page.mu" "Click here")
  (reset-style))
```

### Pre-compute Static Content

```scheme
;; Compute once
(define header
  (conc (style '(align center))
        (section "My Site")
        (style '(align left))))

;; Reuse
(print header "Page content...")
(print header "More content...")
```

## Common Patterns

### Navigation Menu

```scheme
(define (nav-link url label)
  (conc (style '(fg "5af"))
        (link url label)
        (reset-style)
        " | "))

(conc
  (nav-link "/page/index.mu" "Home")
  (nav-link "/page/about.mu" "About")
  (nav-link "/page/contact.mu" "Contact"))
```

### Info Box

```scheme
(define (info-box title content)
  (conc
    (divider) nl
    (style '(fg "5af")) (bold title) (reset-style) nl
    (style '(fg "ddd")) content (reset-style) nl
    (divider)))
```

### Status Messages

```scheme
(define (success msg)
  (conc (style '(fg "5f5")) "✓ " msg (reset-style)))

(define (warning msg)
  (conc (style '(fg "fa5")) "⚠ " msg (reset-style)))

(define (error msg)
  (conc (style '(fg "f55")) "✗ " msg (reset-style)))
```
