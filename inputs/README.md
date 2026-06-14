# inputs/

Drop your three working files here:

```
inputs/
├── shells.docx      <- blank TLF shells (combined Word document)
├── sap.docx         <- Statistical Analysis Plan
└── define.xml       <- ADaM define.xml
```

These files are excluded from the package build (.Rbuildignore) and from
git (.gitignore) — they will never end up in a public artifact. Use this
folder freely for real client data.

## Run

From the package root, with `ANTHROPIC_API_KEY` set:

```r
devtools::load_all(".")

# Agent 1: generate the CDISC ARS JSON
shell_to_ars(
  shell_path  = "inputs/shells.docx",
  sap_path    = "inputs/sap.docx",
  define_path = "inputs/define.xml",
  output_path = "outputs/reporting_event.json"
)

# Agent 2: produce the annotated Word shell, aligned to the ARS
shell_annotate(
  ars_path    = "outputs/reporting_event.json",
  shell_path  = "inputs/shells.docx",
  define_path = "inputs/define.xml",
  output_path = "outputs/annotated_shell.docx"
)
```
