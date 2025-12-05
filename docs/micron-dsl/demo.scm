#!/usr/bin/env -S csi -s

;;; demo.scm - Complete Micron DSL demonstration

(import micron)

(print
  ;; === HEADERS ===
  (style '(align center))
  (section "Micron DSL Demo")
  (style '(align left))
  nl nl

  (subsection "Text Styling")
  nl
  (bold "Bold text") nl
  (italics "Italic text") nl
  (underline "Underlined text") nl
  (code "Code/monospace") nl
  nl

  (subsection "Colors")
  nl
  (style '(fg "f00")) "Red text" (reset-style) nl
  (style '(fg "0f0")) "Green text" (reset-style) nl
  (style '(fg "00f")) "Blue text" (reset-style) nl
  (style '(fg "fff" bg "000")) "White on black" (reset-style) nl
  nl

  (subsection "Combined Styling")
  nl
  (style '(fg "5af"))
  (bold "Bold blue text")
  (reset-style)
  nl
  (style '(fg "fa5" bg "333"))
  (italics "Italic orange on gray")
  (reset-style)
  nl nl

  (subsection "Links")
  nl
  (style '(fg "5af"))
  (link "https://github.com" "GitHub")
  (reset-style)
  " - External link"
  nl
  (style '(fg "5af"))
  (link "/page/index.mu" "Home")
  (reset-style)
  " - Internal page"
  nl nl

  (subsection "Dividers")
  nl
  "Above divider"
  nl
  (divider)
  nl
  "Below divider"
  nl nl

  (subsection "Alignment")
  nl
  (style '(align left))
  "Left aligned"
  nl
  (style '(align center))
  "Center aligned"
  nl
  (style '(align right))
  "Right aligned"
  nl
  (style '(align left))
  nl

  (subsection "Forms")
  nl
  "Name: "
  (style '(bg "333" fg "aaa"))
  (input-field-fixed "username" 16)
  (reset-style)
  nl
  "Email: "
  (style '(bg "333" fg "aaa"))
  (input-field-fixed "email" 32)
  (reset-style)
  nl nl
  (style '(bg "373"))
  (submit-field "Submit" "/handler.scm" "demo" "username" "email")
  (reset-style)
  nl nl

  (subsection "Tips")
  nl
  (style '(fg "888"))
  "- Always use " (code "reset-style") " after styling"
  nl
  "- Colors are 3-digit hex RGB"
  nl
  "- Combine functions with " (code "conc") " for complex layouts"
  (reset-style))
