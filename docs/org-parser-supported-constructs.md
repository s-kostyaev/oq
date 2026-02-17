# Supported Org-Mode Constructs in the oq Parser

This document records the actual parser support at the `Oq.Org.parse_string` level.
Source of truth: implementation in `lib/oq.ml` and coverage tests in `test/org_parser_indexing.ml`.

## Supported

1. Headings `*`, `**`, `***`, and deeper
- Heading level recognition.
- Parent/child hierarchy construction.
- Section range calculation (`section_source`).
- Space or tab is accepted after the star prefix.
- References: `lib/oq.ml:305`, `lib/oq.ml:661`, `lib/oq.ml:842`.

2. Heading components
- TODO keyword (based on active TODO config).
- Priority cookie in the form `[#A]`.
- Trailing tags in the form `:tag1:tag2:`.
- Trailing tags are recognized when separated by spaces or tabs.
- Reference: `lib/oq.ml:349`.

3. Keyword lines `#+KEY: VALUE`
- Generic file-level keyword parsing.
- `#+TODO: ... | ...` parsing for open/done state groups.
- TODO keywords with fast-key/logging suffixes are normalized
  (for example `TODO(t)`, `WAIT(w@/!)`, `DONE(d!)`).
- Multiple `#+TODO:` lines are merged into one effective workflow config.
- References: `lib/oq.ml:369`, `lib/oq.ml:120`, `lib/oq.ml:795`.

4. Planning metadata
- `SCHEDULED: ...`
- `DEADLINE: ...`
- `CLOSED: ...`
- Stored as `planning_entry` values with `raw_value`.
- Multiple planning keywords on one line are split correctly into separate values.
- References: `lib/oq.ml:528`, `lib/oq.ml:705`.

5. Drawers and properties
- Drawer open/close parsing using `:NAME:` and `:END:`.
- Property extraction (`:KEY: VALUE`) from `:PROPERTIES:`.
- References: `lib/oq.ml:382`, `lib/oq.ml:396`, `lib/oq.ml:763`.

6. Blocks `#+BEGIN_...` / `#+END_...`
- Indexed block types:
  - `SRC` (optional language),
  - `EXAMPLE`,
  - `QUOTE`,
  - `EXPORT` (optional backend).
- Block headers accept space or tab as separator before optional parameters
  (for example `#+BEGIN_SRC\tocaml`).
- `SRC`/`EXPORT` header arguments after language/backend are tolerated
  (for example `#+BEGIN_SRC emacs-lisp :results output`).
- `SRC` blocks with header arguments and no language are handled as language-less
  (for example `#+BEGIN_SRC :results output`).
- Other valid block types (for example `CENTER`, `VERSE`, `COMMENT`) are parsed as opaque regions:
  - they do not fail parsing,
  - they are not added to `index.blocks`.
- References: `lib/oq.ml:419`, `lib/oq.ml:431`, `lib/oq.ml:463`.

7. Links in text
- Bracket links: `[[target][description]]` and `[[target]]`.
- Plain links: tokens starting with `http://` or `https://`.
- References: `lib/oq.ml:493`, `lib/oq.ml:502`, `lib/oq.ml:629`.

8. Tables
- Table regions start with lines whose trimmed form begins with `|`.
- Each table row is split into cells by `|`.
- References: `lib/oq.ml:469`, `lib/oq.ml:473`, `lib/oq.ml:777`.

## Errors and Limitations

1. Structural syntax errors
- Unterminated block -> `syntax_error`.
- Unterminated drawer -> `syntax_error`.
- References: `lib/oq.ml`, `test/org_parser_indexing.ml`.

2. Indexing scope limitation for blocks
- Only `{SRC, EXAMPLE, QUOTE, EXPORT}` appear in `index.blocks`.
- Opaque block kinds are skipped from block index, but the document still parses.
- References: `lib/oq.ml`, `test/org_parser_indexing.ml`.

3. Encoding
- Invalid UTF-8 input returns `invalid_utf8`.
- References: `lib/oq.ml:579`, `test/org_parser_indexing.ml:110`.

## Test Validation

Coverage is validated by `test/org_parser_indexing.ml`:
- headings/sections: `test/org_parser_indexing.ml:29`,
- TODO/planning: `test/org_parser_indexing.ml:50`,
- properties/drawers: `test/org_parser_indexing.ml:57`,
- blocks/links/tables: `test/org_parser_indexing.ml:66`,
- opaque block tolerance: `test/org_parser_indexing.ml`,
- error mapping: `test/org_parser_indexing.ml:78`.
