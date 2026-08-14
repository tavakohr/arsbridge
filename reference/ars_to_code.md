# Generate the R analysis program for one or more outputs

Writes one self-contained `{cards}` `.R` program per output (TLF) from
an **already-saved** ARS. This is the explicit generation step:
authoring a reporting event and generating programs from it are two
deliberate actions, so
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
does not do this for you unless you ask it to with `emit_code = TRUE`.

## Usage

``` r
ars_to_code(ars_path, output_ids = NULL, code_dir, overwrite = FALSE)
```

## Arguments

- ars_path:

  Path to a saved ARS JSON file.

- output_ids:

  Character vector of output ids (or names) to generate. `NULL`, the
  default, generates every eligible output. An id matching no output is
  an error, not an empty result.

- code_dir:

  Directory to write the `.R` programs into, created if needed.

- overwrite:

  Replace an existing program whose content differs. Default `FALSE`.

## Value

Invisibly, a named character vector of the paths written – names are
output ids, in the order the reporting event lists them.

## Details

The reporting event is the semantic source of truth. Generation reads
the saved ARS and nothing else – never the original shell or ADaM
specification – so a program always reflects the reviewed, approved
document rather than a re-reading of the inputs it came from.

## What this does not do

It generates; it never executes. Nothing here reads data, so no
`adam_dir` is needed: the emitted program takes the ADaM location as its
own input, which you set before running it.

## The subject key comes from the event, not the call

There is no `subject_key` argument. The subject identifier is not
plumbing – it is the deduplication key, the cross-dataset join key and
the grouped-denominator merge key – so a caller-supplied value decides
what gets counted. The reporting event already settles it: every
method's `codeTemplate` names `USUBJID`, as does the generated
analysis-set condition. An override here could emit a program that
contradicts the method it implements. Should arsbridge support another
key, the event will have to declare it.

## An existing program is never silently replaced

If the target file exists and is byte-identical to what would be
written, nothing happens – regenerating is safe to repeat. If it exists
and *differs*, generation stops rather than discarding it.

arsbridge deliberately does not claim to know *why* it differs. Emitted
programs carry no provenance, so a file edited by hand and a file
generated from an earlier version of the ARS look exactly alike. Pass
`overwrite = TRUE` when you have decided the file on disk is expendable.

## See also

[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
to author the reporting event,
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
to compute results from it, and
[`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
to review it first.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- spec_to_ars_example()          # emit_code = FALSE by default
ars_to_code(res$ars_path, code_dir = "code")            # every output
ars_to_code(res$ars_path, "T_14_1_2", code_dir = "code") # just one
} # }
```
