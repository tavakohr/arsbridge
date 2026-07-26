## Thin launcher for the arsbridge guided workflow.
##
## Set ARSBRIDGE_PROJECT to resume an existing project folder; leave it unset
## to start on the project-setup step.
##
##   ARSBRIDGE_PROJECT=~/my_study Rscript -e \
##     'shiny::runApp(system.file("shiny/ars_workflow", package = "arsbridge"))'

project <- Sys.getenv("ARSBRIDGE_PROJECT", unset = "")
arsbridge::ars_workflow(project_dir = if (nzchar(project)) project else NULL)
