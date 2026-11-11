## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* The words flagged as possibly misspelled in DESCRIPTION are SDMX terms:
  "Dataflows" and "codelists" are SDMX artefact names, and "eXchange" is
  the official spelling of the standard's name (Statistical Data and
  Metadata eXchange).

## Test environments

* local: Linux, R 4.6.1
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1), macos-latest
  (R release), windows-latest (R release)

## Notes for the reviewers

* The package is a client for SDMX (Statistical Data and Metadata eXchange)
  REST web services of central banks, statistical offices and international
  organizations.
* The tests do not use the network: HTTP answers are mocked with
  `httr2::with_mocked_responses()` and SDMX-ML fixtures in `tests/testthat`.
* The examples that contact a provider are wrapped in `\donttest{}` and in
  `tryCatch()` on the package error class `sdmx_error`: if the provider cannot
  be reached, answers with an HTTP error, or returns something that is not
  SDMX-ML, they print a message instead of failing. The European Central Bank
  occasionally answers HTTP 504 to the first request of a session; this is
  handled the same way.
* `\dontrun{}` is used only in `sdmx_proxy()`, whose example would ask for a
  proxy password interactively.
* Passwords and tokens are asked only in interactive sessions; the package
  does not write files and does not change options.
* 'xts' and 'getPass' are in Suggests and used only after
  `requireNamespace()`.
