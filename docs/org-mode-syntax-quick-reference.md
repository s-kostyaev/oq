# Org Mode Syntax: Short Reference

This is a concise overview of common syntax constructs supported by Org Mode.

## 1. Document structure (headlines)

Use leading stars to define outline levels:

```org
* Level 1
** Level 2
*** Level 3
```

Headlines can include TODO keywords, priorities, and tags:

```org
* TODO [#A] Write summary :work:docs:
```

## 2. Paragraphs and sections

A blank line separates paragraphs. Content under a headline belongs to that section.

## 3. Lists

Unordered, ordered, and description lists are supported:

```org
- item
- another item

1. first
2. second

- term :: definition
```

Checkboxes are supported inside list items:

```org
- [ ] pending
- [X] done
```

## 4. Emphasis and inline markup

```org
*bold* /italic/ _underline_ +strike-through+ =verbatim= ~code~
```

## 5. Links

```org
[[https://orgmode.org][Org website]]
[[file:notes.org][Local file]]
[[*Level 1][Headline link]]
<<anchor>>
[[#anchor][Target link]]
```

## 6. Blocks

Delimited blocks are written with `#+BEGIN_...` and `#+END_...`:

```org
#+BEGIN_SRC ocaml
let x = 42
#+END_SRC

#+BEGIN_QUOTE
Quoted text.
#+END_QUOTE
```

Common block types: `SRC`, `EXAMPLE`, `QUOTE`, `VERSE`, `CENTER`, `COMMENT`.

## 7. Drawers and properties

Use drawers to keep structured metadata:

```org
* Task
:PROPERTIES:
:OWNER: Alice
:EFFORT: 2h
:END:
```

## 8. Tables

Org has plain-text tables with `|` separators:

```org
| Name  | Value |
|-------+-------|
| alpha | 10    |
| beta  | 20    |
```

## 9. Planning and timestamps

Use timestamps and planning keywords in headlines:

```org
* TODO Submit report
SCHEDULED: <2026-02-18 Wed>
DEADLINE: <2026-02-20 Fri>
```

You can also use active/inactive timestamps:

```org
<2026-02-18 Wed>   [2026-02-18 Wed]
```

## 10. Footnotes

```org
Text with note.[fn:1]

[fn:1] Footnote text.
```

## 11. Keywords and comments

Document-level keywords:

```org
#+TITLE: Notes
#+AUTHOR: Jane Doe
#+OPTIONS: toc:nil
```

Comment line and comment block:

```org
# this line is a comment

#+BEGIN_COMMENT
Ignored by export
#+END_COMMENT
```

## Sources

- GNU Org Manual (stable): https://orgmode.org/org.html
- GNU Org Manual, single-page view (Org 9.7): https://www.gnu.org/software/emacs/manual/html_mono/org.html
- Org syntax references (Worg): https://orgmode.org/worg/org-syntax.html
