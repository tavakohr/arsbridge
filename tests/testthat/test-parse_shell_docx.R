test_that("parse_shell_docx finds the 2 TLF sections in the synthetic fixture", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_length(secs, 2)
  expect_equal(vapply(secs, `[[`, character(1), "tlf_number"),
               c("T-14-1-1", "T-14-3-1"))
  expect_equal(vapply(secs, `[[`, character(1), "tlf_type"),
               c("TABLE", "TABLE"))
})

test_that("title and population text captured", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_match(secs[[1]]$title, "Demographic")
  expect_match(secs[[1]]$population_text, "Safety Population")
})

test_that("population annotation extracted with full equality suffix", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_equal(secs[[1]]$population_annot, "ADSL.SAFFL='Y'")
  expect_equal(secs[[2]]$population_annot, "ADSL.SAFFL='Y'")
})

test_that("annotated rows are flagged has_annot = TRUE", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  annotated <- Filter(function(r) isTRUE(r$has_annot), secs[[1]]$stub_rows)
  expect_gte(length(annotated), 3)
  expect_true(any(vapply(annotated, function(r) r$annotation == "ADSL.AGE", logical(1))))
})

test_that("child sub-rows (n, Mean (SD), Median, Male, Female) have has_annot = FALSE", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  child_labels <- c("n", "Mean (SD)", "Median", "Male", "Female",
                    "White", "Black or African American")
  for (sec in secs) {
    children <- Filter(function(r) r$label %in% child_labels, sec$stub_rows)
    expect_true(all(vapply(children, function(r) !isTRUE(r$has_annot), logical(1))),
                info = sec$tlf_number)
  }
})

test_that("source datasets extracted from the Source: line", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_equal(secs[[1]]$source_datasets, "ADSL")
  expect_equal(secs[[2]]$source_datasets, c("ADSL", "ADAE"))
})

test_that("Layer 3 pattern detection sets confidence=high for full DATASET.VAR", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  annotated <- Filter(function(r) isTRUE(r$has_annot), secs[[1]]$stub_rows)
  expect_true(all(vapply(annotated, function(r) r$detection_method == "pattern", logical(1))))
  expect_true(all(vapply(annotated, function(r) r$detection_confidence == "high", logical(1))))
})

test_that("parse_shell_docx aborts on missing file", {
  expect_error(parse_shell_docx("nonexistent.docx"), "not found")
})
