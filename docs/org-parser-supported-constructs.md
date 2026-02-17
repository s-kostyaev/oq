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
- Tags-only headings are supported (for example `* :work:docs:`).
- Reference: `lib/oq.ml:349`.

3. Keyword lines `#+KEY: VALUE`
- Generic file-level keyword parsing.
- Keyword markers must start at column 1; indented `#+...` lines are treated
  as regular text.
- `#+TODO: ... | ...` parsing for open/done state groups.
- `#+SEQ_TODO:` and `#+TYP_TODO:` are treated as TODO workflow aliases.
- TODO keywords with fast-key/logging suffixes are normalized
  (for example `TODO(t)`, `WAIT(w@/!)`, `DONE(d!)`).
- Multiple `#+TODO:` lines are merged into one effective workflow config.
- References: `lib/oq.ml:369`, `lib/oq.ml:120`, `lib/oq.ml:795`.

4. Comment lines
- Lines starting with `#` (but not `#+...`) are treated as comments and ignored by content indexing.
- References: `lib/oq.ml`, `test/org_parser_indexing.ml`.

5. Planning metadata
- `SCHEDULED: ...`
- `DEADLINE: ...`
- `CLOSED: ...`
- Planning keywords are matched case-insensitively (`scheduled:`/`deadline:`/`closed:` also work).
- Stored as `planning_entry` values with `raw_value`.
- Multiple planning keywords on one line are split correctly into separate values.
- Planning keywords are recognized only on planning lines
  (plain text mentioning `SCHEDULED:` does not create planning entries).
- `.scheduled/.deadline/.closed` without a range include entries even when timestamp
  format is unsupported for date-range filtering.
- References: `lib/oq.ml:528`, `lib/oq.ml:705`.

6. Drawers and properties
- Drawer open/close parsing using `:NAME:` and `:END:`.
- Property extraction (`:KEY: VALUE`) from `:PROPERTIES:`.
- Drawer names must be bare (no surrounding/interior whitespace), so fixed-width
  text lines like `: code:` are not treated as drawers.
- Drawer names cannot contain `:`; lines like `:a:b:` are treated as text,
  avoiding false unterminated-drawer errors on plain content.
- Drawer names are limited to `[A-Za-z0-9_-]+`; tokens like `:+1:` are treated
  as text instead of drawer markers.
- Known drawers (`PROPERTIES`/`LOGBOOK`, case-insensitive) are recognized
  conservatively when a matching `:END:` appears before the next heading.
- Custom `:NAME:` drawers are also recognized conservatively with the same
  before-next-heading `:END:` rule; this prevents false opens on isolated
  plain tokens
  while keeping drawer internals opaque to block/link parsing.
- Drawer markers may be indented with leading whitespace (for example inside
  list context), matching Org behavior.
- Drawer closing marker `:END:` is matched case-insensitively and may be
  indented with leading whitespace once a drawer is open.
- `.property("KEY")` matching is case-insensitive for property names.
- References: `lib/oq.ml:382`, `lib/oq.ml:396`, `lib/oq.ml:763`.

7. Blocks `#+BEGIN_...` / `#+END_...`
- Indexed block types:
  - `SRC` (optional language),
  - `EXAMPLE`,
  - `QUOTE`,
  - `EXPORT` (optional backend).
- Block markers must start at column 1; indented `#+BEGIN_...`/`#+END_...`
  lines are treated as plain content.
- Block headers accept space or tab as separator before optional parameters
  (for example `#+BEGIN_SRC\tocaml`).
- `SRC`/`EXPORT` header arguments after language/backend are tolerated
  (for example `#+BEGIN_SRC emacs-lisp :results output`).
- `SRC` blocks with header arguments and no language are handled as language-less
  (for example `#+BEGIN_SRC :results output`).
- `SRC` blocks with switches and no language are handled as language-less
  (for example `#+BEGIN_SRC -n :results output`, `#+BEGIN_SRC +n :results output`).
- `.code("lang")` matching is case-insensitive (`OCAML` matches `ocaml`).
- `#+END_...` block closing lines tolerate trailing text after block kind.
- Dynamic blocks (`#+BEGIN: ...` / `#+END:`) are treated as opaque regions,
  so unsupported internals do not break parsing.
- Dynamic block closing marker is matched strictly as `#+END:` (case-insensitive),
  so malformed endings like `#+END:foo` do not hide surrounding content.
- Block openings are recognized conservatively only when a matching closing
  marker appears before the next heading; otherwise `#+BEGIN_...` text is
  treated as plain content to avoid false parse failures and heading loss on
  valid Org files.
- Other valid block types (for example `CENTER`, `VERSE`, `COMMENT`) are parsed as opaque regions:
  - they do not fail parsing,
  - they are not added to `index.blocks`.
- References: `lib/oq.ml:419`, `lib/oq.ml:431`, `lib/oq.ml:463`.

8. Links in text
- Bracket links: `[[target][description]]` and `[[target]]`.
- Plain links: tokens with URI schemes in the form `scheme://...`
  (for example `https://...`, `HTTPS://...`, `ftp://...`).
- Plain `file:...` and `mailto:...` links are also recognized.
- Plain file links without `file:` prefix are recognized when they look like
  complete paths (for example `./notes.org`, `../docs/spec.org`, `~/notes.org`,
  `~alice/docs/spec.org`, `C:/work/notes.org`, `D:\docs\spec.org`,
  `\\server\share\notes.org`, `/ssh:me@host:/tmp/notes.org`).
- Plain `id:...` and `custom-id:...` links are also recognized.
- Plain `news:...`, `shell:...`, `elisp:...`, `help:...`, `info:...`, `man:...`,
  `woman:...`, `calc:...`, `tel:...`, `sms:...`, and `geo:...` links are also recognized.
- Plain `doi:...` links are also recognized.
- Plain `attachment:...` links are also recognized.
- Plain `coderef:...` links are also recognized.
- Plain `file+sys:...` and `file+emacs:...` links are also recognized.
- Plain `irc:...`, `gnus:...`, `rmail:...`, `docview:...`, and `bbdb:...` links are also recognized.
- Plain `mhe:...`, `wl:...`, `vm:...`, and `vm-imap:...` links are also recognized.
- `#+LINK: abbrev ...` keyword definitions are honored for plain links
  (for example `#+LINK: gh https://github.com/%s` enables `gh:owner/repo`).
- `#+LINK` abbreviations are collected file-wide, so usage order is flexible
  (`gh:owner/repo` can appear before its `#+LINK: gh ...` declaration).
- `#+LINK` inside opaque regions (block bodies and drawer bodies) is ignored
  (only real keyword lines define abbreviations).
- Plain link tokenization is whitespace-aware (spaces and tabs).
- Angle links `<...>` are parsed before tokenization, so targets with spaces are preserved
  (for example `<https://example.com/path with spaces>` and `<bbdb:R.* Stallman>`).
- Angle-link scanning is resilient to unrelated `<` text earlier on the same line
  (for example `if x < y then <https://example.com>` still indexes the URL).
- Repeated link occurrences are preserved (same target repeated on a line is indexed multiple times).
- Bracket links are not duplicated as plain links.
- Bracket-link scanning skips malformed opening fragments, so a stray `[[`
  before a valid link on the same line does not produce a bogus target.
- Plain links preserve balanced trailing parentheses in URLs.
- Leading punctuation before plain links is trimmed (including `.`), while
  relative file links starting with `./` or `../` are preserved.
- Trailing sentence punctuation like `!` and `?` is trimmed from plain-link targets.
- References: `lib/oq.ml:493`, `lib/oq.ml:502`, `lib/oq.ml:629`.

9. Tables
- Table regions start with lines whose trimmed form begins with `|`.
- Each table row is split into cells by `|`.
- References: `lib/oq.ml:469`, `lib/oq.ml:473`, `lib/oq.ml:777`.

## Errors and Limitations

1. Structural syntax handling
- Unterminated drawer/block-like openings are handled conservatively as plain
  content when no matching close marker exists later, instead of failing parse.
- References: `lib/oq.ml`, `test/org_parser_indexing.ml`.

2. Indexing scope limitation for blocks
- Only `{SRC, EXAMPLE, QUOTE, EXPORT}` appear in `index.blocks`.
- Opaque block kinds are skipped from block index, but the document still parses.
- References: `lib/oq.ml`, `test/org_parser_indexing.ml`.

3. Encoding
- Invalid UTF-8 input returns `invalid_utf8`.
- UTF-8 BOM at file start is tolerated and ignored.
- References: `lib/oq.ml:579`, `test/org_parser_indexing.ml:110`.

## Test Validation

Coverage is validated by `test/org_parser_indexing.ml`:
- headings/sections: `test/org_parser_indexing.ml:29`,
- TODO/planning: `test/org_parser_indexing.ml:50`,
- properties/drawers: `test/org_parser_indexing.ml:57`,
- blocks/links/tables: `test/org_parser_indexing.ml:66`,
- opaque block tolerance: `test/org_parser_indexing.ml`,
- error mapping: `test/org_parser_indexing.ml:78`.
