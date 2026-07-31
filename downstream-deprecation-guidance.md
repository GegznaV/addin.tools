# Downstream Migration Guidance for addin.tools Deprecations

This package keeps compatibility aliases for renamed helpers, but
downstream packages should migrate proactively.

## Current rename

- [`is_rmd_visual_mode()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md)
  has been renamed to
  [`is_visual_editor()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md).
- The old name remains as a deprecated wrapper for now.

## Recommended downstream changes

1.  Replace imports/usages:
    - [`is_rmd_visual_mode()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md)
      -\>
      [`is_visual_editor()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md)
2.  Update namespace imports:
    - `importFrom(addin.tools, is_rmd_visual_mode)` -\>
      `importFrom(addin.tools, is_visual_editor)`
3.  Update tests:
    - Change expectations and fixtures to call
      [`is_visual_editor()`](https://gegznav.github.io/addin.tools/reference/is_visual_editor.md)
      directly.
4.  Update user-facing docs:
    - Change README text, examples, and help topics to the new name.
5.  Verify behavior:
    - Run package-local tests and `R CMD check` after the rename.

## Compatibility policy

- The alias will continue to work temporarily but emits a deprecation
  warning.
- Downstream packages should treat the alias as transitional and remove
  it from new code.

## Deprecation timetable

- The compatibility wrapper remains available for at least one minor
  release after the rename.
- Removal should not be considered before version `0.1.0`.
- Any future compatibility wrapper should document the target
  replacement and a removal milestone in this file and in `NEWS.md`.
