# Package Architecture and Helper Families

## Overview

`addin.tools` is a helper layer for RStudio addin packages. It focuses
on editor context, selection ranges, text extraction, and text mutation
helpers.

## Helper families

- Context and state helpers:
  [`rs_get_context()`](https://gegznav.github.io/addin.tools/reference/rs_get_context.md),
  [`is_visual_editor()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md)
- Selection helpers:
  [`rs_get_selection_text()`](https://gegznav.github.io/addin.tools/reference/rs_get_selection_text.md),
  [`rs_get_selection_range()`](https://gegznav.github.io/addin.tools/reference/rs_get_selection_range.md),
  [`rs_get_n_selections()`](https://gegznav.github.io/addin.tools/reference/rs_get_n_selections.md)
- Row and position helpers:
  [`rs_get_row_range()`](https://gegznav.github.io/addin.tools/reference/rs_get_row_range.md),
  `rs_get_index_*()`, `rs_get_position_*()`
- Text modification helpers:
  [`rs_insert_text()`](https://gegznav.github.io/addin.tools/reference/rs_insert_text.md),
  [`rs_replace_in_selection()`](https://gegznav.github.io/addin.tools/reference/rs_replace_in_selection.md),
  [`rs_select_rows()`](https://gegznav.github.io/addin.tools/reference/rs_select_rows.md)
- Formatting helpers:
  [`ensure_blank_line()`](https://gegznav.github.io/addin.tools/reference/ensure_blank_line.md),
  [`make_spaces()`](https://gegznav.github.io/addin.tools/reference/make_spaces.md),
  [`repeat_symbol()`](https://gegznav.github.io/addin.tools/reference/repeat_symbol.md)

## Common task patterns

### Insert a wrapper around the current selection

``` r

context <- rs_get_context()
selected_text <- rs_get_selection_text(context = context)

rs_replace_in_selection(
  replacement = paste0("<kbd>", selected_text, "</kbd>"),
  keep_selected = TRUE,
  selection = "first",
  context = context
)
```

### Insert a small helper at the cursor

``` r

context <- rs_get_context()

rs_insert_text(
  text = "message(\"Hello\")",
  context = context,
  spaces = FALSE
)
```

### Select a block of rows before formatting

``` r

context <- rs_get_context()

rs_select_rows(
  first = 10,
  last = 14,
  context = context
)
```

## Migration notes

- Use
  [`is_visual_editor()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md)
  in new code.
- [`is_rmd_visual_mode()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md)
  is retained as a deprecated wrapper for downstream compatibility.
- When adding new helpers, prefer a small focused API and add regression
  tests around selection and mutation behavior.

## Release notes for maintainers

- Use the release prep script under `data-raw/release-prep.R` for the
  standard release sequence.
- Run `data-raw/revdep-smoke-check.R` before tagging when downstream
  packages need a compatibility smoke test.
- Use the CRAN checklist under `.github/cran-submission-checklist.md`
  before tagging a release.
