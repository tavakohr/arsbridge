# inputs/

Your working files. This folder is **default-deny** in `.gitignore`: only the
shared APX-DRM-301 practice files are allowlisted, and anything else you drop
here is ignored by default. A sponsor document copied in under any name cannot
be swept into a commit by `git add -A`.

To add a genuinely shareable file, allowlist it in `.gitignore`. To commit one
deliberately, `git add -f` it.

## What arsbridge needs

```
inputs/
├── <study>_TLF_Shells_annotated.docx   <- annotated TLF shell (.docx OR .xlsx)
├── adam_spec_<study>.xlsx              <- ADaM specification
└── ADaM/                               <- ADaM datasets (.xpt, .csv, .sas7bdat)
```

The shell may be a Word document or an Excel workbook. In Word the annotations
are coloured runs inside the table cells; in Excel each TLF is normally its own
worksheet and the annotation is coloured text in the stub cell. Both go through
the same `parse_shell()` front door.

The ADaM spec is what every proposed variable is gated against — an out-of-spec
reference is rejected and reported rather than shipped.

## The practice study

`APX-DRM-301` is a synthetic mock study, tracked in this folder so the pipeline
can be exercised without any real data:

| File | What it is |
|---|---|
| `APX-DRM-301_TLF_Shells_v1.0_sample_annotated.docx` | annotated shell (Word) |
| `APX-DRM-301_TLF_Shells_Clean_v1.0.docx` | the same shells, unannotated |
| `adam_spec_APX-DRM-301.xlsx` | ADaM specification |
| `ADaM.zip` | ADaM datasets — unzip to `inputs/ADaM/` |
| `APX-DRM-301_SAP_v1.0.docx` | statistical analysis plan |

There is no Excel shell for APX-DRM-301 yet, so a docx↔xlsx parity check needs
a different pair.

## Run it

From the package root. No API key is required — deterministic parsing is a
first-class mode, not a degraded one.

```r
devtools::load_all(".")

spec_to_ars(
  shell_path     = "inputs/APX-DRM-301_TLF_Shells_v1.0_sample_annotated.docx",
  adam_spec_path = "inputs/adam_spec_APX-DRM-301.xlsx",
  output_path    = "outputs/reporting_event.json"
)

ard <- ars_to_ard("outputs/reporting_event.json", adam_dir = "inputs/ADaM")
```

For an Excel shell, write the shell back with its own numbers in it:

```r
ars_fill_shell(
  shell_path  = "inputs/<study>_shells.xlsx",
  ars         = "outputs/reporting_event.json",
  ard         = ard,
  output_path = "outputs/filled_shell.xlsx",
  adam_dir    = "inputs/ADaM"
)
```

Or drive the whole thing from the app, which keeps the build off the UI process
and leaves a payload on disk you can come back to after a restart:

```r
ars_workflow("outputs/my_study")
```

## Without an API key, but with a chat assistant

`ars_copilot_instructions()` writes an instruction file and the supplement JSON
Schema. Upload those two plus your shell and your ADaM spec to
Copilot/ChatGPT, save the reply as `supplement.json`, and feed it back:

```r
spec_to_ars(..., supplement = "supplement.json",
            supplement_trust = "prefer_supplement")
```

For an Excel shell, run `write_supplement_draft()` first and upload the draft
too — the parser can already state the column axis and the row bindings, so the
assistant corrects a structured draft rather than authoring one from scratch.
Note the trust argument: the default `"fill_gaps"` lets the shell win a
conflict, which is not what you want for a file you have just reviewed.
