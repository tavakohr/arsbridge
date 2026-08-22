## Structural reasoning over RESOLVED restrictions.
##
## The defect these exist for: structure was read from the annotation string,
## while the restriction a row actually computes under may have been supplied
## as a typed supplement clause instead. The two are the same restriction. A
## reader that only sees one of them decides layout on whichever channel the
## author happened to use, which is not a property of the table at all.
##
## So the contract under test is CHANNEL INDEPENDENCE: the same restriction,
## stated either way, must produce the same structural answer -- including when
## a parent states it one way and its children the other.
##
## Every identifier is invented. ADQX.RESCAT, ADZZ.TRTEMFL and their kin exist
## in no study in this repo, so a rule that keys on a familiar name fails here.

## --- restrictions in each of the two channels ------------------------------

## A typed clause, as a reviewed supplement supplies it.
.rs_eq <- function(dataset, variable, value, comparator = "EQ") {
  list(condition = list(dataset = dataset, variable = variable,
                        comparator = comparator, value = as.list(value)))
}
.rs_and <- function(...) {
  list(compoundExpression = list(logicalOperator = "AND",
                                 whereClauses = list(...)))
}
.rs_or <- function(...) {
  list(compoundExpression = list(logicalOperator = "OR",
                                 whereClauses = list(...)))
}

## A row whose restriction is AUTHORED, in the annotation.
.rs_authored <- function(label, annotation) {
  list(label = label, annotation = annotation, has_annot = nzchar(annotation))
}

## A row whose restriction is TYPED, supplied by a supplement. The annotation
## names the variable only -- which is what the supplement path leaves behind.
.rs_typed <- function(label, reference, where) {
  list(label = label, annotation = reference, has_annot = TRUE,
       detection_method = "supplement", supplement_where = where)
}

.rs_view <- function(rows) .row_restriction_view(rows)
.rs_one <- function(row) .rs_view(list(row))[[1]]

.rs_bind <- function(dataset = "ADQX", variable = "RESCAT") {
  list(dataset = dataset, variable = variable, discrete = TRUE)
}


## ---------------------------------------------------------------------------
## The view: one reading, whichever channel supplied the restriction
## ---------------------------------------------------------------------------

test_that("the restriction view reports the same clause from either channel", {
  authored <- .rs_one(.rs_authored("Improved", "ADQX.RESCAT='IMP'"))
  typed    <- .rs_one(.rs_typed("Improved", "ADQX.RESCAT",
                                .rs_eq("ADQX", "RESCAT", "IMP")))

  expect_equal(authored$status, "clause")
  expect_equal(typed$status, "clause")
  expect_equal(.restriction_domain_on(authored, "ADQX", "RESCAT"),
               .restriction_domain_on(typed, "ADQX", "RESCAT"))
  expect_equal(.restriction_domain_on(typed, "ADQX", "RESCAT")$values, "IMP")
})

test_that("the restriction view separates 'no restriction' from 'unreadable'", {
  ## The distinction the whole reservation contract rests on. A row that states
  ## nothing computes over every record legitimately; a row whose restriction
  ## could not be read must never be given the same answer.
  none <- .rs_one(.rs_authored("Measured value", "ADQX.MEASUR"))
  expect_equal(none$status, "none")
  expect_equal(.restriction_domain_on(none, "ADQX", "RESCAT")$status,
               "unrestricted")

  bad <- .rs_one(.rs_authored("Improved", "ADQX.RESCAT where <<unreadable"))
  expect_equal(bad$status, "unresolved")
  expect_equal(.restriction_domain_on(bad, "ADQX", "RESCAT")$status, "unknown")
})

test_that("a supplement clause is read even without the annotation flag", {
  ## The typed clause is the restriction. A view that could only reach it
  ## through a second field would report "no restriction" if the two ever
  ## parted -- the wrong direction to fail in.
  row <- list(label = "Improved", annotation = "", has_annot = FALSE,
              supplement_where = .rs_eq("ADQX", "RESCAT", "IMP"))
  expect_equal(.rs_one(row)$status, "clause")
  expect_equal(.restriction_domain_on(.rs_one(row), "ADQX", "RESCAT")$values,
               "IMP")
})

test_that("reading the view records no diagnostics of its own", {
  ## It is a reading, not the moment a row is built. The row loop reports an
  ## unreadable filter once, against the row it belongs to; reporting here as
  ## well would double every such message.
  diag_reset()
  invisible(.rs_view(list(
    .rs_authored("Improved", "ADQX.RESCAT where <<unreadable"),
    .rs_authored("Unchanged", "ADQX.RESCAT where <<also unreadable"))))
  expect_equal(nrow(diag_records()), 0L)
})


## ---------------------------------------------------------------------------
## What a restriction says about one variable
## ---------------------------------------------------------------------------

test_that("the domain reader answers four distinct ways", {
  free <- .rs_one(.rs_authored("Any", "ADQX.RESCAT"))
  expect_equal(.restriction_domain_on(free, "ADQX", "RESCAT")$status,
               "unrestricted")

  ## A restriction on a DIFFERENT variable leaves this one free.
  other <- .rs_one(.rs_authored("Any", "ADQX.RESCAT where ADZZ.TRTEMFL='Y'"))
  expect_equal(.restriction_domain_on(other, "ADQX", "RESCAT")$status,
               "unrestricted")

  set <- .rs_one(.rs_authored("Listed", "ADQX.RESCAT in ('IMP','UNC')"))
  got <- .restriction_domain_on(set, "ADQX", "RESCAT")
  expect_equal(got$status, "enumerated")
  expect_equal(got$values, c("IMP", "UNC"))

  ## A threshold speaks about the variable and names no finite set. This is
  ## what keeps a run of cumulative thresholds from reading as a partition.
  thr <- .rs_one(.rs_authored("At least 4", "ADQX.DURWK GE 4"))
  expect_equal(.restriction_domain_on(thr, "ADQX", "DURWK")$status, "narrowed")

  ## An OR gives no term that restricts the result on its own.
  disj <- .rs_one(.rs_typed("Either", "ADQX.RESCAT",
                            .rs_or(.rs_eq("ADQX", "RESCAT", "IMP"),
                                   .rs_eq("ADQX", "RESCAT", "UNC"))))
  expect_equal(.restriction_domain_on(disj, "ADQX", "RESCAT")$status, "unknown")
})

test_that("a variable of the same name in another dataset is not this one", {
  ## Two ADaM datasets may carry a variable spelled the same way. Matching on
  ## the name alone would attribute one domain's restriction to another's.
  elsewhere <- .rs_one(.rs_typed("Improved", "ADQX.RESCAT",
                                 .rs_eq("ADZZ", "RESCAT", "IMP")))
  expect_equal(.restriction_domain_on(elsewhere, "ADQX", "RESCAT")$status,
               "unrestricted")
  expect_equal(.restriction_domain_on(elsewhere, "ADZZ", "RESCAT")$values,
               "IMP")
})


## ---------------------------------------------------------------------------
## The residue: what else the restriction says
## ---------------------------------------------------------------------------

test_that("residue equality ignores the order terms were written in", {
  ## `A AND B` and `B AND A` are the same restriction. Comparing serialized
  ## text would call them different and refuse a valid collapse.
  ab <- .rs_one(.rs_typed("Mild", "ADQX.RESCAT",
                          .rs_and(.rs_eq("ADZZ", "TRTEMFL", "Y"),
                                  .rs_eq("ADZZ", "SAFEFL", "Y"),
                                  .rs_eq("ADQX", "RESCAT", "IMP"))))
  ba <- .rs_one(.rs_typed("Mild", "ADQX.RESCAT",
                          .rs_and(.rs_eq("ADQX", "RESCAT", "IMP"),
                                  .rs_eq("ADZZ", "SAFEFL", "Y"),
                                  .rs_eq("ADZZ", "TRTEMFL", "Y"))))
  expect_equal(.restriction_residue(ab, "ADQX", "RESCAT")$signature,
               .restriction_residue(ba, "ADQX", "RESCAT")$signature)
  expect_length(.restriction_residue(ab, "ADQX", "RESCAT")$signature, 2L)
})

test_that("residue is not known when the restriction cannot be split", {
  disj <- .rs_one(.rs_typed("Either", "ADQX.RESCAT",
                            .rs_or(.rs_eq("ADZZ", "TRTEMFL", "Y"),
                                   .rs_eq("ADQX", "RESCAT", "IMP"))))
  expect_false(.restriction_residue(disj, "ADQX", "RESCAT")$known)

  bad <- .rs_one(.rs_authored("Improved", "ADQX.RESCAT where <<unreadable"))
  expect_false(.restriction_residue(bad, "ADQX", "RESCAT")$known)
})


## ---------------------------------------------------------------------------
## Refinement: is the child the parent, plus a level?
## ---------------------------------------------------------------------------

test_that("a child refines its parent only when nothing else differs", {
  parent <- .rs_one(.rs_typed("Findings", "ADQX.RESCAT",
                              .rs_eq("ADZZ", "TRTEMFL", "Y")))
  same <- .rs_one(.rs_typed("Improved", "ADQX.RESCAT",
                            .rs_and(.rs_eq("ADZZ", "TRTEMFL", "Y"),
                                    .rs_eq("ADQX", "RESCAT", "IMP"))))
  expect_true(.restriction_refines(parent, same, "ADQX", "RESCAT"))

  ## The child states a restriction the parent does not. Folding it into the
  ## parent's single computation would DISCARD that restriction silently.
  extra <- .rs_one(.rs_typed("Improved", "ADQX.RESCAT",
                             .rs_and(.rs_eq("ADZZ", "TRTEMFL", "Y"),
                                     .rs_eq("ADZZ", "SAFEFL", "Y"),
                                     .rs_eq("ADQX", "RESCAT", "IMP"))))
  expect_false(.restriction_refines(parent, extra, "ADQX", "RESCAT"))

  ## The child omits one the parent states. Folding would IMPOSE it.
  fewer <- .rs_one(.rs_typed("Improved", "ADQX.RESCAT",
                             .rs_eq("ADQX", "RESCAT", "IMP")))
  expect_false(.restriction_refines(parent, fewer, "ADQX", "RESCAT"))
})

test_that("refinement is unproved, not false, when nothing can be read", {
  ## The three-valued contract. Only TRUE may be acted on; NA must never be
  ## read as either answer.
  parent <- .rs_one(.rs_authored("Findings", "ADQX.RESCAT"))
  bad <- .rs_one(.rs_authored("Improved", "ADQX.RESCAT where <<unreadable"))
  expect_true(is.na(.restriction_refines(parent, bad, "ADQX", "RESCAT")))
  expect_true(is.na(.restriction_refines(bad, parent, "ADQX", "RESCAT")))
})

test_that("a parent already narrowed on its own variable is not an axis", {
  parent <- .rs_one(.rs_authored("At least 4 weeks", "ADQX.DURWK GE 4"))
  child  <- .rs_one(.rs_authored("Exactly 4", "ADQX.DURWK=4"))
  expect_false(.restriction_refines(parent, child, "ADQX", "DURWK"))
})


## ---------------------------------------------------------------------------
## The partition relation, and its negative controls
## ---------------------------------------------------------------------------

## A parent row and its candidate children, in whichever channel each uses.
.rs_partition <- function(parent, children, dataset = "ADQX",
                          variable = "RESCAT") {
  view <- .rs_view(c(list(parent), children))
  .restriction_partition_relation(view[[1]], view[-1], dataset, variable)
}

test_that("POSITIVE: a free parent with one-value children proves a partition", {
  got <- .rs_partition(
    .rs_authored("Response category", "ADQX.RESCAT"),
    list(.rs_authored("Improved",  "ADQX.RESCAT='IMP'"),
         .rs_authored("Unchanged", "ADQX.RESCAT='UNC'"),
         .rs_authored("Worsened",  "ADQX.RESCAT='WOR'")))
  expect_equal(got$status, "proved")
  expect_equal(got$levels, c("IMP", "UNC", "WOR"))
  expect_equal(got$n_children, 3L)
})

test_that("POSITIVE: an enumerated parent subdivided exactly proves a partition", {
  ## The set-partition shape: the parent declares the domain and the rows
  ## beneath carve it up. One child names two of the values, the other names
  ## the third. Together they cover the parent's set exactly.
  got <- .rs_partition(
    .rs_authored("Listed categories", "ADQX.RESCAT in ('IMP','UNC','WOR')"),
    list(.rs_authored("Improved or unchanged", "ADQX.RESCAT in ('IMP','UNC')"),
         .rs_authored("Worsened", "ADQX.RESCAT='WOR'")))
  expect_equal(got$status, "proved")
  expect_equal(sort(got$levels), c("IMP", "UNC", "WOR"))
})

test_that("NEGATIVE: a child restricting another variable is not a level", {
  got <- .rs_partition(
    .rs_authored("Response category", "ADQX.RESCAT"),
    list(.rs_authored("Treatment emergent", "ADZZ.TRTEMFL='Y'")))
  expect_equal(got$status, "unproved")
  expect_equal(got$levels, character())
})

test_that("NEGATIVE: a child outside the parent's declared domain is not a level", {
  got <- .rs_partition(
    .rs_authored("Listed categories", "ADQX.RESCAT in ('IMP','UNC')"),
    list(.rs_authored("Improved", "ADQX.RESCAT='IMP'"),
         .rs_authored("Worsened", "ADQX.RESCAT='WOR'")))
  expect_equal(got$status, "unproved")
})

test_that("NEGATIVE: partial cover of a declared domain is a selection, not a subdivision", {
  ## The author declared exactly which values are in play. Rows covering only
  ## some of them do not subdivide it, and collapsing them would report the
  ## uncovered values under a level nobody wrote.
  got <- .rs_partition(
    .rs_authored("Listed categories", "ADQX.RESCAT in ('IMP','UNC')"),
    list(.rs_authored("Improved", "ADQX.RESCAT='IMP'")))
  expect_equal(got$status, "unproved")
})

test_that("NEGATIVE: overlapping children do not partition anything", {
  got <- .rs_partition(
    .rs_authored("Listed categories", "ADQX.RESCAT in ('IMP','UNC','WOR')"),
    list(.rs_authored("Improved or unchanged", "ADQX.RESCAT in ('IMP','UNC')"),
         .rs_authored("Unchanged or worsened", "ADQX.RESCAT in ('UNC','WOR')")))
  expect_equal(got$status, "unproved")
})

test_that("NEGATIVE: an unresolved child ends the run rather than joining it", {
  got <- .rs_partition(
    .rs_authored("Response category", "ADQX.RESCAT"),
    list(.rs_authored("Improved", "ADQX.RESCAT where <<unreadable"),
         .rs_authored("Unchanged", "ADQX.RESCAT='UNC'")))
  expect_equal(got$status, "unproved")
})

test_that("NEGATIVE: a relation the typed reader cannot establish stays unproved", {
  ## An OR child. Nothing here says the child selects one value of the parent's
  ## variable, and "cannot prove" must not become "data levels".
  got <- .rs_partition(
    .rs_authored("Response category", "ADQX.RESCAT"),
    list(.rs_typed("Improved or emergent", "ADQX.RESCAT",
                   .rs_or(.rs_eq("ADQX", "RESCAT", "IMP"),
                          .rs_eq("ADZZ", "TRTEMFL", "Y")))))
  expect_equal(got$status, "unproved")
})

test_that("NEGATIVE: a multi-value child under a FREE parent proves nothing", {
  ## With no declared domain there is nothing to check containment against, so
  ## a child naming several values could overlap its siblings undetectably.
  got <- .rs_partition(
    .rs_authored("Response category", "ADQX.RESCAT"),
    list(.rs_authored("Improved or unchanged", "ADQX.RESCAT in ('IMP','UNC')"),
         .rs_authored("Worsened", "ADQX.RESCAT='WOR'")))
  expect_equal(got$status, "unproved")
})


## ---------------------------------------------------------------------------
## CONTRACT: the channel that supplied a restriction cannot change the layout
## ---------------------------------------------------------------------------

## The same block, written four ways: both restrictions authored, both typed,
## and each of the two mixed directions. All four must give the same layout.
.rs_block_channels <- function() {
  common <- .rs_eq("ADZZ", "TRTEMFL", "Y")
  levels <- c(IMP = "Improved", UNC = "Unchanged", WOR = "Worsened")

  authored_parent <- .rs_authored("Findings by category",
                                  "ADQX.RESCAT where ADZZ.TRTEMFL='Y'")
  typed_parent    <- .rs_typed("Findings by category", "ADQX.RESCAT", common)

  authored_kids <- lapply(names(levels), function(code) {
    .rs_authored(levels[[code]],
                 sprintf("ADQX.RESCAT where ADZZ.TRTEMFL='Y' and ADQX.RESCAT='%s'",
                         code))
  })
  typed_kids <- lapply(names(levels), function(code) {
    .rs_typed(levels[[code]], "ADQX.RESCAT",
              .rs_and(common, .rs_eq("ADQX", "RESCAT", code)))
  })

  list(
    authored     = c(list(authored_parent), authored_kids),
    typed        = c(list(typed_parent),    typed_kids),
    parent_typed = c(list(typed_parent),    authored_kids),
    child_typed  = c(list(authored_parent), typed_kids))
}

test_that("CONTRACT: typed and authored restrictions are equivalent evidence", {
  variants <- .rs_block_channels()
  bind <- .rs_bind()

  answers <- lapply(variants, function(rows) {
    ctx <- .row_layout_context(rows, 1L, binding = bind)
    list(levels = ctx$level_children,
         shape  = .block_shape(rows[[1]], bind, ctx))
  })

  ## Non-vacuous: the block really is recognised, in every channel.
  for (name in names(answers)) {
    expect_equal(answers[[name]]$levels, c("IMP", "UNC", "WOR"),
                 info = name)
    expect_equal(answers[[name]]$shape$shape, "categorical_block", info = name)
    expect_equal(answers[[name]]$shape$expansion_source, "data_levels",
                 info = name)
  }

  ## And identical across all four, including the mixed directions.
  expect_equal(answers$typed$levels, answers$authored$levels)
  expect_equal(answers$parent_typed$levels, answers$authored$levels)
  expect_equal(answers$child_typed$levels, answers$authored$levels)
})

test_that("CONTRACT: the channel cannot rescue a block the structure disproves", {
  ## The mirror of the test above, and the one that keeps it honest. Stating
  ## the same non-partition in the typed channel must not make it a partition.
  common <- .rs_eq("ADZZ", "TRTEMFL", "Y")
  typed <- list(
    .rs_typed("Findings by category", "ADQX.RESCAT", common),
    .rs_typed("At least four weeks", "ADQX.RESCAT",
              .rs_and(common, .rs_eq("ADQX", "DURWK", 4, comparator = "GE"))))
  authored <- list(
    .rs_authored("Findings by category", "ADQX.RESCAT where ADZZ.TRTEMFL='Y'"),
    .rs_authored("At least four weeks",
                 "ADQX.RESCAT where ADZZ.TRTEMFL='Y' and ADQX.DURWK GE 4"))

  for (rows in list(typed, authored)) {
    ctx <- .row_layout_context(rows, 1L, binding = .rs_bind())
    expect_equal(ctx$level_children, character())
  }
})


## ---------------------------------------------------------------------------
## Metamorphic: the same shell in a second invented vocabulary
## ---------------------------------------------------------------------------

test_that("renaming every dataset and variable changes nothing", {
  ## If any of this keys on a name rather than a relationship, the two answers
  ## come apart here.
  rename <- function(text) {
    text <- gsub("ADQX", "ADYY", text, fixed = TRUE)
    text <- gsub("ADZZ", "ADWW", text, fixed = TRUE)
    text <- gsub("RESCAT", "OUTGRP", text, fixed = TRUE)
    gsub("TRTEMFL", "EMERGFL", text, fixed = TRUE)
  }
  original <- list(
    .rs_authored("Listed categories", "ADQX.RESCAT in ('IMP','UNC','WOR')"),
    .rs_authored("Improved or unchanged",
                 "ADQX.RESCAT in ('IMP','UNC')"),
    .rs_authored("Worsened", "ADQX.RESCAT='WOR'"))
  renamed <- lapply(original, function(r) {
    .rs_authored(r$label, rename(r$annotation))
  })

  a <- .row_layout_context(original, 1L, binding = .rs_bind())
  b <- .row_layout_context(renamed, 1L,
                           binding = .rs_bind("ADYY", "OUTGRP"))
  expect_equal(sort(a$level_children), c("IMP", "UNC", "WOR"))
  expect_equal(a$level_children, b$level_children)
})
