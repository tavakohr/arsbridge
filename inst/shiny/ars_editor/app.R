## arsbridge -- inst/shiny/ars_editor/app.R
## ---------------------------------------------------------------------------
## Convenience launcher for anyone who prefers to open the reviewer the way
## Shiny apps are usually opened:
##
##   shiny::runApp(system.file("shiny/ars_editor", package = "arsbridge"))
##
## Point it at a reporting event with ARSBRIDGE_ARS; with nothing set it
## generates the bundled example first (deterministic, no API key needed).
##
## The supported entry points are arsbridge::view_ars() and
## arsbridge::edit_ars() -- this file is a thin wrapper around the same app.

ars_path <- Sys.getenv("ARSBRIDGE_ARS", "")
spec_path <- Sys.getenv("ARSBRIDGE_ADAM_SPEC", "")

if (!nzchar(ars_path)) {
  message("ARSBRIDGE_ARS is not set -- generating the bundled example.")
  result <- arsbridge::spec_to_ars_example(
    output_path = file.path(tempdir(), "reporting_event.json"),
    verbose     = FALSE
  )
  ars_path <- result$ars_path
}

if (!nzchar(spec_path)) {
  spec_path <- arsbridge::arsbridge_example("adam_spec.xlsx")
}

arsbridge::view_ars(ars_path, adam_spec_path = spec_path)
