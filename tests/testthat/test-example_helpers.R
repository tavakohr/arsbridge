test_that("arsbridge_example() with no args lists the bundle", {
  files <- arsbridge_example()
  expect_true(is.character(files))
  expect_true("annotated_shell.docx" %in% files)
  expect_true("adam_spec.xlsx"       %in% files)
  expect_true("ADaM.zip"             %in% files)
})

test_that("arsbridge_example(name) returns an absolute existing path", {
  p <- arsbridge_example("annotated_shell.docx")
  expect_true(file.exists(p))
  expect_match(p, "annotated_shell\\.docx$")

  p2 <- arsbridge_example("adam_spec.xlsx")
  expect_true(file.exists(p2))

  p3 <- arsbridge_example("ADaM.zip")
  expect_true(file.exists(p3))
  expect_gt(file.info(p3)$size, 100000)   # ~680 KB
})

test_that("arsbridge_example(unknown_file) errors with the available list", {
  expect_error(arsbridge_example("nope.docx"), "not in the bundle")
})

test_that("spec_to_ars_example errors clearly when API key missing", {
  expect_error(spec_to_ars_example(api_key = ""), "ANTHROPIC_API_KEY")
})
