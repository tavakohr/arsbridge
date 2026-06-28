# Contributing to arsbridge

This outlines how to propose a change to arsbridge.

## Fixing typos

Small typos or grammatical errors in documentation may be edited
directly using the GitHub web interface, so long as the changes are made
in the *source* file. This generally means you’ll need to edit [roxygen2
comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`,
not a `.Rd` file. You can find the `.R` file that generates the `.Rd` by
reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it’s a good idea to first file an
issue and make sure someone from the team agrees that it’s needed. If
you’ve found a bug, please file an issue that illustrates the bug with a
minimal [reprex](https://www.tidyverse.org/help/#reprex) (this will also
help you write a unit test, if needed).

### Pull request process

- Fork the package and clone onto your computer. If you haven’t done
  this before, we recommend using
  `usethis::create_from_github("tavakohr/arsbridge", fork = TRUE)`.
- Install all development dependencies with
  `devtools::install_dev_deps()`, and then make sure the package passes
  R CMD check by running `devtools::check()`. If R CMD check doesn’t
  pass cleanly, it’s a good idea to ask for help before continuing.
- Create a Git branch for your pull request (PR). We recommend using
  `usethis::pr_init("brief-description-of-change")`.
- Make your changes, commit to git, and then create a PR by running
  `usethis::pr_push()`, and following the prompts in your browser. The
  title of your PR should briefly describe the change. The body of your
  PR should contain `Fixes #issue-number`.
- For user-facing changes, add a bullet to the top of `NEWS.md`
  (i.e. just below the first header). Follow the style described in
  <https://style.tidyverse.org/news.html>.

### Code style

- New code should follow the tidyverse [style
  guide](https://style.tidyverse.org). You can use the
  [styler](https://CRAN.R-project.org/package=styler) package to apply
  these styles, but please don’t restyle code that has nothing to do
  with your PR.
- We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
  [Markdown
  syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html),
  for documentation.
- We use [testthat](https://cran.r-project.org/package=testthat)
  (edition 3) for unit tests. Contributions with test cases included are
  easier to accept.

## Architecture decisions

Design-level decisions live as numbered Architecture Decision Records in
[`adr/`](https://tavakohr.github.io/arsbridge/adr/). Read them before
proposing a change to the engine’s scope or the ARD contract — they
explain *why* the current boundaries exist:

- `0001-statistical-method-extensibility.md` — arsbridge bounds the
  *boundary*, not the *contents*, of the statistics space. New
  statistics are added as descriptors on the shared ARD shape, not as
  new `switch` branches. Inferential and model-based methods are tiered
  (descriptive → standard test → model-based scaffold → placeholder),
  and code emission stays deterministic while the LLM only classifies
  the shell.
- `0002-partial-results-traceability.md` — how a partially-computable
  table stays traceable: arsbridge fills the cells it can and reserves a
  keyed `manual_pending` stub ARD row, with provenance columns, for the
  rest. All values — computed or manual — enter at the ARD layer;
  nothing is typed straight into the rendered output. Status: proposed
  (phased plan, not yet implemented).

When you make a design-level change, add or update an ADR in the same
PR. Keep the standard `ARS → ARD → tfrmt` pipeline — the shell is never
the source of truth for layout.

## Code of Conduct

Please note that the arsbridge project is released with a [Contributor
Code of
Conduct](https://tavakohr.github.io/arsbridge/CODE_OF_CONDUCT.md). By
contributing to this project you agree to abide by its terms.
