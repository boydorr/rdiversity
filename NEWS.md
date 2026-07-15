# rdiversity 2.3.0

* Fixed `repartition()` for non-phylogenetic metacommunities: it previously passed a
  bare similarity matrix to `metacommunity()`, which has not accepted matrices for a
  while
* Added tests for `gen2dist()`, `inddiv()`, `repartition()` and the high-level
  diversity-measure wrappers
* Corrected the `gen2dist()` documentation
* Removed the vendored `binary.R` library; the taxonomic and genetic bit handling
  is now implemented directly in base R
* **Breaking:** `as.binary()` (and its associated S3 methods) is no longer
  exported, as it was only ever an internal implementation detail
* Removed unused `stats`/`utils` imports
* Dropped the `reshape2` dependency (superseded upstream); the `melt()` calls in
  `inddiv()`/`subdiv()`/`metadiv()` are replaced by a small internal base-R helper
* Migrated continuous integration from Travis CI and AppVeyor to GitHub Actions
* Added `lint`, `pr-commands` and R-hub v2 (`rhub`) workflows
* Formatted the codebase with `styler` and added a `.lintr` configuration so the
  package passes `lintr::lint_package()` cleanly
* Added a `cran-comments.md` template and tidied the repository (ignore rules,
  build artefacts).

# rdiversity 2.2.0

* Added internal support for binary (base-2) number objects, used when encoding
  taxonomic and genetic data as similarities (`taxid()`, `taxvec()`,
  `taxmask()`).
* Fixed CRAN check notes and corrected the package URL and maintainer field.

# rdiversity 2.1.3

* Add dependency on markdown to fix win-builder warning

# rdiversity 2.1.2

* Add vignette
* Remove deprecated functionality - `metacommunity()` no longer allows matrices as a similarity argument
* Increase unit tests coverage

# rdiversity 2.0

* Standardised metacommunity input
* New functions added to calculate genetic similarity matrices

# rdiversity 1.2.1

* Fix problems that occur when user doesn't have row names in the partition object
* Update citation

# rdiversity 1.2

* Allow partition types to be input in any order, or with missing or additional types
* Tests added for new functionality

# rdiversity 1.1

* Removed redundant argument `chainsaw(depth)`
* Renamed `chainsaw(interval)` to `chainsaw(depth)`
* Minor edits to documentation

# rdiversity 1.0

* Initial submission to CRAN
