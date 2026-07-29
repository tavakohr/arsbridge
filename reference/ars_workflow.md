# Run the guided arsbridge workflow

Opens a step-by-step app that walks one study project from an annotated
TLF shell to a reviewed CDISC ARS reporting event:

## Usage

``` r
ars_workflow(project_dir = NULL)
```

## Arguments

- project_dir:

  Path to the project folder. `NULL` (default) starts on the
  project-setup step; an existing project resumes.

## Value

Invisibly, the project directory.

## Details

1.  **Project setup** – pick a project folder and name the annotated
    shell (`.docx`), the ADaM spec (`.xlsx`/`.xml`), and a study id. The
    folder gets a fixed layout (`copilot/`, `ars/`) and a small
    `arsbridge_project.json` state file.

2.  **Instruction files** – writes the two-phase Copilot instructions
    and the supplement JSON schema into `copilot/` (see
    [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)).

3.  **Phase 1 (manual)** – upload the Phase 1 instructions, the shell,
    and the spec to your chat assistant; paste or upload its
    `tlf_extraction_blueprints.json` reply back here. A pre-flight check
    catches truncated or wrong-version files before they cost a Phase 2
    session.

4.  **Phase 2 (manual)** – upload the Phase 2 instructions, the schema,
    the shell, and the blueprint; paste or upload `supplement.json`
    back.
    [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
    runs immediately, and any FAILs come with a paste-ready repair
    prompt for the assistant.

5.  **Build** – runs
    [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
    with the supplement (or deterministically when you skip the Copilot
    phases) and reports the result: outputs, analyses, warnings,
    blockers, and the written files.

6.  **Review & edit** – hands the built reporting event to
    [`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md).
    When the editor closes, the workflow reopens with fresh statuses.

Every step's status is derived from the files in the project folder, so
closing the app loses nothing: reopening the same `project_dir` resumes
exactly where the files stand. Steps 3 and 4 are skippable – without a
supplement the build runs in deterministic mode.

## See also

[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md),
[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md),
[`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md),
[`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md).

## Examples

``` r
if (FALSE) { # interactive()
ars_workflow()                 # start fresh
ars_workflow("~/my_study")     # resume an existing project
}
```
