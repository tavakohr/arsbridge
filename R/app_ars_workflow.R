## arsbridge -- app_ars_workflow.R
## ---------------------------------------------------------------------------
## The guided workflow app: one place to run the whole journey from an
## annotated shell to a reviewed reporting event.
##
##   1. Project setup        folders + shell/spec paths + study id
##   2. Instruction files    the two-phase Copilot documents + JSON schema
##   3. Phase 1 (manual)     assistant returns tlf_extraction_blueprints.json
##   4. Phase 2 (manual)     assistant returns supplement.json (validated here)
##   5. Build                spec_to_ars() -> ars/reporting_event.json
##   6. Review & edit        hand off to the existing editor
##
## The editor is a blocking runApp whose save flow completes after it exits,
## so this app CHAINS to it: "Open in the editor" stops the workflow with a
## payload, ars_workflow() launches edit_ars(), and when the editor closes
## the workflow relaunches with every status re-derived from disk. Closing
## the app never loses anything -- the project folder is the state.

#' Run the guided arsbridge workflow
#'
#' Opens a step-by-step app that walks one study project from an annotated
#' TLF shell to a reviewed CDISC ARS reporting event:
#'
#' 1. **Project setup** -- pick a project folder and name the annotated shell
#'    (`.docx` or `.xlsx`), the ADaM spec (`.xlsx`/`.xml`), and a study id. The
#'    folder gets a fixed layout (`copilot/`, `ars/`) and a
#'    small `arsbridge_project.json` state file.
#' 2. **Instruction files** -- writes the two-phase Copilot instructions and
#'    the supplement JSON schema into `copilot/` (see
#'    [ars_copilot_instructions()]).
#' 3. **Phase 1 (manual)** -- upload the Phase 1 instructions, the shell, and
#'    the spec to your chat assistant; paste or upload its
#'    `tlf_extraction_blueprints.json` reply back here. A pre-flight check
#'    catches truncated or wrong-version files before they cost a Phase 2
#'    session.
#' 4. **Phase 2 (manual)** -- upload the Phase 2 instructions, the schema,
#'    the shell, and the blueprint; paste or upload `supplement.json` back.
#'    [ars_validate_supplement()] runs immediately, and any FAILs come with
#'    a paste-ready repair prompt for the assistant.
#' 5. **Build** -- runs [spec_to_ars()] with the supplement (or
#'    deterministically when you skip the Copilot phases) and reports the
#'    result: outputs, analyses, warnings, blockers, and the written files.
#' 6. **Review & edit** -- hands the built reporting event to [edit_ars()].
#'    When the editor closes, the workflow reopens with fresh statuses.
#'
#' Every step's status is derived from the files in the project folder, so
#' closing the app loses nothing: reopening the same `project_dir` resumes
#' exactly where the files stand. Steps 3 and 4 are skippable -- without a
#' supplement the build runs in deterministic mode.
#'
#' @param project_dir Path to the project folder. `NULL` (default) starts on
#'   the project-setup step; an existing project resumes.
#'
#' @return Invisibly, the project directory.
#'
#' @seealso [spec_to_ars()], [ars_copilot_instructions()],
#'   [ars_validate_supplement()], [edit_ars()].
#'
#' @examplesIf interactive()
#' ars_workflow()                 # start fresh
#' ars_workflow("~/my_study")     # resume an existing project
#' @export
ars_workflow <- function(project_dir = NULL) {
  rlang::check_installed(c("shiny", "bslib", "DT"),
                         reason = "to open the arsbridge workflow")
  if (!is.null(project_dir) && nzchar(project_dir) && dir.exists(project_dir)) {
    project_dir <- normalizePath(project_dir)
  }

  repeat {
    result <- shiny::runApp(.ars_workflow_app(project_dir))
    if (is.null(result) || !identical(result$action, "edit")) {
      if (!is.null(result$project_dir)) project_dir <- result$project_dir
      break
    }
    project_dir <- result$project_dir
    edit_ars(
      result$ars_path,
      adam_spec_path = result$adam_spec_path,
      report_path    = result$report_path,
      output_path    = result$ars_path
    )
    ## The editor has closed (saved or not); relaunch the workflow so the
    ## user lands back on the journey with statuses re-read from disk.
  }
  invisible(project_dir)
}

#' @noRd
.workflow_app_state <- function(project_dir) {
  list(
    project_dir         = shiny::reactiveVal(project_dir),
    nonce               = shiny::reactiveVal(0L),
    last_result         = shiny::reactiveVal(NULL),
    blueprint_findings  = shiny::reactiveVal(NULL),
    supplement_findings = shiny::reactiveVal(NULL)
  )
}

#' One status badge.
#' @noRd
.workflow_badge <- function(done, available) {
  if (isTRUE(done)) {
    shiny::span(class = "badge text-bg-success", "Done")
  } else if (isTRUE(available)) {
    shiny::span(class = "badge text-bg-primary", "Ready")
  } else {
    shiny::span(class = "badge text-bg-secondary", "Waiting")
  }
}

#' A copyable checklist of files to upload to the chat assistant.
#' @noRd
.workflow_upload_list <- function(title, paths) {
  paths <- Filter(function(p) !is.null(p) && nzchar(p), paths)
  shiny::div(
    class = "border rounded p-2 mb-3 small",
    shiny::div(class = "fw-bold mb-1", title),
    shiny::tags$ol(
      class = "mb-0",
      lapply(paths, function(p) shiny::tags$li(shiny::tags$code(p)))
    )
  )
}

#' @noRd
.ars_workflow_app <- function(project_dir = NULL) {
  initial_state <- if (!is.null(project_dir)) {
    .workflow_read_state(project_dir)
  }
  initial_status <- if (!is.null(initial_state)) {
    .workflow_status(project_dir)
  }
  first_open <- if (is.null(initial_status)) {
    "project"
  } else {
    pending <- initial_status$step[initial_status$available &
                                     !initial_status$done]
    if (length(pending) > 0) {
      pending[1]
    } else if (isTRUE(initial_status$done[initial_status$step == "build"])) {
      "review"
    } else {
      "project"
    }
  }

  ui <- bslib::page_fillable(
    title = "arsbridge workflow",
    theme = bslib::bs_theme(version = 5),

    shiny::div(
      class = "d-flex justify-content-between align-items-center my-2",
      shiny::h4(class = "mb-0", "arsbridge workflow"),
      shiny::div(
        class = "d-flex gap-2",
        shiny::uiOutput("project_label", inline = TRUE),
        shiny::actionButton("quit_workflow", "Close", class = "btn-sm")
      )
    ),

    bslib::accordion(
      id = "wizard",
      open = paste0("panel_", first_open),
      multiple = FALSE,

      bslib::accordion_panel(
        title = "1. Project setup", value = "panel_project",
        shiny::uiOutput("badge_project"),
        shiny::textInput("project_dir", "Project folder",
                         value = project_dir %||% "", width = "100%"),
        shiny::textInput("shell_path", "Annotated TLF shell (.docx or .xlsx)",
                         value = initial_state$shell_path %||% "",
                         width = "100%"),
        shiny::textInput("spec_path", "ADaM specification (.xlsx / .xml)",
                         value = initial_state$adam_spec_path %||% "",
                         width = "100%"),
        shiny::textInput("study_id", "Study id",
                         value = initial_state$study_id %||% "STUDY-001"),
        shiny::actionButton("init_project", "Create / update project",
                            class = "btn-primary btn-sm"),
        shiny::div(class = "text-muted small mt-2",
                   "The folder gets copilot/ and ars/ ",
                   "subfolders plus arsbridge_project.json. Reopening the ",
                   "same folder resumes the journey.")
      ),

      bslib::accordion_panel(
        title = "2. Instruction files", value = "panel_instructions",
        shiny::uiOutput("badge_instructions"),
        shiny::p(class = "small",
                 "Writes the Phase 1 and Phase 2 Copilot instructions and ",
                 "the supplement JSON schema into the project's copilot/ ",
                 "folder. Re-running refreshes them after a package upgrade."),
        shiny::uiOutput("instructions_actions"),
        shiny::uiOutput("instructions_paths")
      ),

      bslib::accordion_panel(
        title = "3. Phase 1 -- blueprint (manual Copilot step)",
        value = "panel_phase1",
        shiny::uiOutput("badge_phase1"),
        shiny::uiOutput("phase1_checklist"),
        shiny::p(class = "small",
                 "When the assistant replies, bring ",
                 shiny::tags$code("tlf_extraction_blueprints.json"),
                 " back here -- upload the file or paste the JSON."),
        shiny::fileInput("blueprint_upload", NULL, accept = ".json",
                         buttonLabel = "Upload blueprint...",
                         placeholder = "tlf_extraction_blueprints.json"),
        shiny::textAreaInput("blueprint_paste", "...or paste the JSON reply",
                             rows = 4, width = "100%"),
        shiny::actionButton("blueprint_save_paste", "Save pasted blueprint",
                            class = "btn-sm"),
        DT::DTOutput("blueprint_findings")
      ),

      bslib::accordion_panel(
        title = "4. Phase 2 -- supplement (manual Copilot step)",
        value = "panel_phase2",
        shiny::uiOutput("badge_phase2"),
        shiny::uiOutput("phase2_checklist"),
        shiny::p(class = "small",
                 "Bring back ", shiny::tags$code("supplement.json"),
                 " (and optionally the extraction validation report). It is ",
                 "validated immediately; FAILs come with a paste-ready ",
                 "repair prompt."),
        shiny::fileInput("supplement_upload", NULL, accept = ".json",
                         buttonLabel = "Upload supplement...",
                         placeholder = "supplement.json"),
        shiny::textAreaInput("supplement_paste", "...or paste the JSON reply",
                             rows = 4, width = "100%"),
        shiny::actionButton("supplement_save_paste", "Save pasted supplement",
                            class = "btn-sm"),
        shiny::fileInput("extraction_report_upload",
                         "Extraction validation report (optional)",
                         accept = ".json",
                         buttonLabel = "Upload report...",
                         placeholder = "extraction_validation_report.json"),
        DT::DTOutput("supplement_findings"),
        shiny::uiOutput("supplement_repair")
      ),

      bslib::accordion_panel(
        title = "5. Build the reporting event", value = "panel_build",
        shiny::uiOutput("badge_build"),
        shiny::uiOutput("build_mode"),
        shiny::uiOutput("build_actions"),
        shiny::uiOutput("build_summary")
      ),

      bslib::accordion_panel(
        title = "6. Review & edit", value = "panel_review",
        shiny::uiOutput("badge_review"),
        shiny::uiOutput("review_paths"),
        shiny::uiOutput("review_actions"),
        shiny::div(class = "text-muted small mt-2",
                   "The editor opens in this window's place; when you close ",
                   "it, the workflow returns with fresh statuses.")
      )
    )
  )

  server <- function(input, output, session) {
    state <- .workflow_app_state(project_dir)
    bump <- function() state$nonce(state$nonce() + 1L)

    current_dir <- shiny::reactive({
      dir <- state$project_dir()
      if (is.null(dir) || !nzchar(dir)) NULL else dir
    })
    meta <- shiny::reactive({
      state$nonce()
      dir <- current_dir()
      if (is.null(dir)) NULL else .workflow_read_state(dir)
    })
    paths_r <- shiny::reactive({
      dir <- current_dir()
      if (is.null(dir)) NULL else .workflow_paths(dir)
    })
    status <- shiny::reactive({
      state$nonce()
      dir <- current_dir()
      if (is.null(dir)) NULL else .workflow_status(dir)
    })
    step_row <- function(step) {
      st <- status()
      if (is.null(st)) return(list(done = FALSE, available = step == "project"))
      as.list(st[st$step == step, , drop = FALSE])
    }

    output$project_label <- shiny::renderUI({
      dir <- current_dir()
      if (is.null(dir)) return(NULL)
      shiny::span(class = "text-muted small align-self-center",
                  shiny::tags$code(dir))
    })

    ## --- badges -------------------------------------------------------
    for (step in .WORKFLOW_STEPS) {
      local({
        this_step <- step
        output[[paste0("badge_", this_step)]] <- shiny::renderUI({
          row <- step_row(this_step)
          done <- isTRUE(row$done)
          ## Phase 2 counts as done only when the received supplement has no
          ## FAILs -- the file existing is not enough to move on.
          if (identical(this_step, "phase2") && done) {
            findings <- state$supplement_findings()
            if (!is.null(findings) && any(findings$severity == "FAIL")) {
              done <- FALSE
            }
          }
          shiny::div(class = "mb-2",
                     .workflow_badge(done, isTRUE(row$available)),
                     shiny::span(class = "text-muted small ms-2",
                                 row$detail %||% ""))
        })
      })
    }

    ## --- step 1: project ------------------------------------------------
    shiny::observeEvent(input$init_project, {
      dir <- trimws(input$project_dir %||% "")
      if (!nzchar(dir)) {
        shiny::showNotification("Name a project folder.", type = "warning")
        return()
      }
      init <- tryCatch(
        .workflow_init(dir, input$shell_path, input$spec_path,
                       input$study_id),
        error = function(e) e
      )
      if (inherits(init, "error")) {
        shiny::showNotification(conditionMessage(init), type = "error",
                                duration = 8)
        return()
      }
      state$project_dir(normalizePath(dir))
      bump()
      shiny::showNotification("Project ready.", type = "message", duration = 4)
      bslib::accordion_panel_open("wizard", "panel_instructions")
    })

    ## --- step 2: instruction files ---------------------------------------
    output$instructions_actions <- shiny::renderUI({
      if (!isTRUE(step_row("instructions")$available)) {
        return(shiny::div(class = "text-muted small",
                          "Finish the project setup first."))
      }
      shiny::actionButton("write_instructions", "Write instruction files",
                          class = "btn-primary btn-sm")
    })

    shiny::observeEvent(input$write_instructions, {
      dir <- current_dir()
      if (is.null(dir)) return()
      written <- tryCatch(
        suppressMessages(ars_copilot_instructions(
          dir = .workflow_dirs(dir)$copilot, workflow = "two_phase",
          open = FALSE, overwrite = TRUE
        )),
        error = function(e) e
      )
      if (inherits(written, "error")) {
        shiny::showNotification(conditionMessage(written), type = "error",
                                duration = 8)
        return()
      }
      bump()
      shiny::showNotification("Instruction files written.", type = "message",
                              duration = 4)
      bslib::accordion_panel_open("wizard", "panel_phase1")
    })

    output$instructions_paths <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      if (is.null(paths) || !file.exists(paths$phase1_md)) return(NULL)
      .workflow_upload_list("Written into copilot/:", list(
        paths$phase1_md, paths$phase2_md, paths$schema
      ))
    })

    ## --- step 3: phase 1 blueprint ---------------------------------------
    output$phase1_checklist <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      m <- meta()
      if (is.null(paths) || is.null(m)) return(NULL)
      .workflow_upload_list(
        "Upload these to your chat assistant for Phase 1:",
        list(paths$phase1_md, m$shell_path, m$adam_spec_path)
      )
    })

    save_blueprint <- function(text) {
      paths <- paths_r()
      if (is.null(paths)) return()
      received <- .workflow_receive_json(text, paths$blueprint,
                                         "the Phase 1 blueprint")
      if (!received$ok) {
        shiny::showNotification(received$message, type = "error", duration = 8)
        return()
      }
      state$blueprint_findings(.validate_blueprint(paths$blueprint))
      bump()
      shiny::showNotification(received$message, type = "message", duration = 4)
      bslib::accordion_panel_open("wizard", "panel_phase2")
    }

    shiny::observeEvent(input$blueprint_upload, {
      ## Read the uploaded temp file immediately -- its datapath is not
      ## guaranteed to outlive this flush.
      text <- tryCatch(readLines(input$blueprint_upload$datapath, warn = FALSE),
                       error = function(e) "")
      save_blueprint(text)
    })
    shiny::observeEvent(input$blueprint_save_paste, {
      save_blueprint(input$blueprint_paste)
    })

    output$blueprint_findings <- DT::renderDT({
      state$nonce()
      findings <- state$blueprint_findings()
      paths <- paths_r()
      if (is.null(findings) && !is.null(paths) &&
          file.exists(paths$blueprint)) {
        findings <- .validate_blueprint(paths$blueprint)
        state$blueprint_findings(findings)
      }
      if (is.null(findings) || nrow(findings) == 0) return(NULL)
      DT::datatable(findings, rownames = FALSE, selection = "none",
                    options = list(pageLength = 8, dom = "tp"))
    }, server = TRUE)

    ## --- step 4: phase 2 supplement --------------------------------------
    output$phase2_checklist <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      m <- meta()
      if (is.null(paths) || is.null(m)) return(NULL)
      .workflow_upload_list(
        "Upload these to your chat assistant for Phase 2:",
        list(paths$phase2_md, paths$schema, m$shell_path,
             if (file.exists(paths$blueprint)) paths$blueprint)
      )
    })

    save_supplement <- function(text) {
      paths <- paths_r()
      m <- meta()
      if (is.null(paths)) return()
      received <- .workflow_receive_json(text, paths$supplement,
                                         "the Phase 2 supplement")
      if (!received$ok) {
        shiny::showNotification(received$message, type = "error", duration = 8)
        return()
      }
      findings <- tryCatch(
        suppressMessages(ars_validate_supplement(
          paths$supplement, adam_spec_path = m$adam_spec_path
        )),
        error = function(e) {
          data.frame(severity = "FAIL", tlf = NA_character_, where = "file",
                     problem = conditionMessage(e), stringsAsFactors = FALSE)
        }
      )
      state$supplement_findings(findings)
      bump()
      n_fail <- sum(findings$severity == "FAIL")
      if (n_fail > 0) {
        shiny::showNotification(
          paste(n_fail, "FAIL finding(s) -- paste the repair prompt back to the assistant."),
          type = "error", duration = 8
        )
      } else {
        shiny::showNotification("Supplement received and validated.",
                                type = "message", duration = 4)
        bslib::accordion_panel_open("wizard", "panel_build")
      }
    }

    shiny::observeEvent(input$supplement_upload, {
      text <- tryCatch(readLines(input$supplement_upload$datapath, warn = FALSE),
                       error = function(e) "")
      save_supplement(text)
    })
    shiny::observeEvent(input$supplement_save_paste, {
      save_supplement(input$supplement_paste)
    })

    shiny::observeEvent(input$extraction_report_upload, {
      paths <- paths_r()
      if (is.null(paths)) return()
      text <- tryCatch(
        readLines(input$extraction_report_upload$datapath, warn = FALSE),
        error = function(e) ""
      )
      received <- .workflow_receive_json(text, paths$extraction_report,
                                         "the extraction validation report")
      shiny::showNotification(received$message,
                              type = if (received$ok) "message" else "error",
                              duration = 5)
    })

    output$supplement_findings <- DT::renderDT({
      state$nonce()
      findings <- state$supplement_findings()
      paths <- paths_r()
      m <- meta()
      if (is.null(findings) && !is.null(paths) &&
          file.exists(paths$supplement)) {
        findings <- tryCatch(
          suppressMessages(ars_validate_supplement(
            paths$supplement, adam_spec_path = m$adam_spec_path
          )),
          error = function(e) NULL
        )
        state$supplement_findings(findings)
      }
      if (is.null(findings) || nrow(findings) == 0) return(NULL)
      DT::datatable(findings, rownames = FALSE, selection = "none",
                    options = list(pageLength = 8, dom = "tp"))
    }, server = TRUE)

    output$supplement_repair <- shiny::renderUI({
      findings <- state$supplement_findings()
      if (is.null(findings)) return(NULL)
      prompt <- attr(findings, "repair_prompt")
      if (is.null(prompt)) return(NULL)
      shiny::div(
        class = "mt-2",
        shiny::div(class = "fw-bold small",
                   "Repair prompt -- paste this back to the assistant:"),
        shiny::tags$pre(class = "small bg-body-tertiary p-2 rounded", prompt)
      )
    })

    ## --- step 5: build ----------------------------------------------------
    output$build_mode <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      if (is.null(paths)) return(NULL)
      if (file.exists(paths$supplement)) {
        shiny::p(class = "small",
                 "Mode: ", shiny::strong("supplement"),
                 " -- copilot/supplement.json will drive the extraction.")
      } else {
        shiny::p(class = "small",
                 "Mode: ", shiny::strong("deterministic"),
                 " -- no supplement present; the regex/annotation pass runs ",
                 "alone (you can add the supplement later and rebuild).")
      }
    })

    output$build_actions <- shiny::renderUI({
      if (!isTRUE(step_row("build")$available)) {
        return(shiny::div(class = "text-muted small",
                          "Finish the project setup and instruction files first."))
      }
      label <- if (isTRUE(step_row("build")$done)) "Rebuild reporting event"
               else "Build reporting event"
      shiny::actionButton("run_build", label, class = "btn-primary btn-sm")
    })

    shiny::observeEvent(input$run_build, {
      paths <- paths_r()
      m <- meta()
      if (is.null(paths) || is.null(m)) return()
      res <- tryCatch(
        shiny::withProgress(
          message = "Building the reporting event...",
          value = NULL,
          suppressMessages(suppressWarnings(.workflow_run_build(m, paths)))
        ),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        shiny::showNotification(
          paste("Build failed:", conditionMessage(res)),
          type = "error", duration = 10
        )
        return()
      }
      state$last_result(res)
      bump()
      shiny::showNotification("Reporting event built.", type = "message",
                              duration = 4)
      bslib::accordion_panel_open("wizard", "panel_review")
    })

    output$build_summary <- shiny::renderUI({
      state$nonce()
      res <- state$last_result()
      paths <- paths_r()
      if (is.null(res)) {
        if (!is.null(paths) && file.exists(paths$ars)) {
          return(shiny::div(
            class = "small text-muted",
            "Built earlier this project: ", shiny::tags$code(paths$ars),
            " (", format(file.mtime(paths$ars), "%Y-%m-%d %H:%M"), ")."
          ))
        }
        return(NULL)
      }
      blockers <- res$blockers
      shiny::div(
        class = "border rounded p-2 small",
        shiny::p(class = "mb-1",
                 shiny::strong("Extraction mode: "), res$extraction_mode, " | ",
                 shiny::strong(res$n_tlfs), " output(s), ",
                 shiny::strong(res$n_analyses), " analyses, ",
                 shiny::strong(res$n_warnings), " warning(s)."),
        if (!is.null(blockers) && nrow(blockers) > 0) {
          shiny::div(class = "alert alert-danger py-1 px-2 mb-1",
                     paste(nrow(blockers),
                           "blocker(s) -- see the validation report."))
        },
        shiny::div("ARS JSON: ", shiny::tags$code(res$ars_path)),
        if (!is.null(res$report_path)) {
          shiny::div("Validation report: ", shiny::tags$code(res$report_path))
        },
        if (!is.null(res$code_dir)) {
          shiny::div("cards scripts: ", shiny::tags$code(res$code_dir))
        }
      )
    })

    ## --- step 6: review hand-off -------------------------------------------
    output$review_paths <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      if (is.null(paths) || !file.exists(paths$ars)) return(NULL)
      .workflow_upload_list("Built artifacts:", list(
        paths$ars,
        if (file.exists(paths$report)) paths$report,
        if (dir.exists(paths$code_dir)) paths$code_dir
      ))
    })

    output$review_actions <- shiny::renderUI({
      if (!isTRUE(step_row("review")$available)) {
        return(shiny::div(class = "text-muted small",
                          "Build the reporting event first."))
      }
      shiny::actionButton("open_editor", "Open in the review editor",
                          class = "btn-primary btn-sm")
    })

    shiny::observeEvent(input$open_editor, {
      dir <- current_dir()
      if (is.null(dir)) return()
      shiny::stopApp(.workflow_handoff_payload(dir))
    })

    shiny::observeEvent(input$quit_workflow, {
      shiny::stopApp(list(action = "quit", project_dir = current_dir()))
    })
  }

  shiny::shinyApp(ui, server)
}
