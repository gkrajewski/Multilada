# Multilada 0.7.0

* `fm_read()`: optional support for semicolon-delimited files (`csv2`)
and other improvements.
* `fm_variables()`: various fixes.

# Multilada 0.6.0

* `fm_read()`: translations in `translations_file` may be scoped to a single column
(rather than be applied across the board).
* `fm_variables()`: revamped `fm_fields()` --- can create (and save) an empty
(template) `variables_file`,
* `fm_grading()`: some fixes.

# Multilada 0.5.0

* `fm_matrix()` and `fm_grading()` don't crash on tibbles.
* `cdi_read()` and `fm_read()` always import to a tibble.
* `variables_file` used by `fm_read()` uses form field names (questions)
as they look in a `csv` file downloaded from *Form Maker*
(as long as they are unique), which makes it easier to work with the file.
* `fm_fields()` added: creates a column of field names (questions)
for `variables_file` from a `csv` file downloaded from *Form Maker*.
* `fm_read()` imports everything as `character`.
* `fm_read()` parsing messages (but not warnings) suppressed.

# Multilada 0.4.0

* Functions dealing with *Form Maker* data added.

# Multilada 0.3.1

* [Multiple submissions differentiated by `end_date`,
rather than `start_date`](https://github.com/gkrajewski/Multilada/issues/7)
* [`start` and `end` not renamed to `start_date` and `end_date`](https://github.com/gkrajewski/Multilada/issues/7)

# Multilada 0.3.0

* `cdi_adaptive()` to summarise adaptive CDIs submissions.

# Multilada 0.2.0

* Demographic data included in `cdi_submissions()` (when available).
* Package file structure improved.
* Multiple submissions with same ID handled.
* Proper age in months calculation with `age_months()`.

# Multilada 0.1.0

* Updated to stricter use of `summarise()` in new version of dplyr.
* Added a `README.md` and a `NEWS.md` files.
