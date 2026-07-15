## Submission

This is an update of the CRAN package `rdiversity` from 2.2.0 to 2.3.0.

This release removes `as.binary()` (and its associated S3 methods) from the
package's exports; it was only ever an internal implementation detail. There are
no reverse dependencies on CRAN, so no other packages are affected.

## Test environments

* local macOS install (R 4.6.1)
* GitHub Actions:
  * ubuntu-latest (R devel, release, oldrel-1)
  * windows-latest (R release)
  * macos-latest (R release)
* win-builder (devel and release)
* R-hub v2 (linux, windows, macos)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.
