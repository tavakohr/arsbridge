# arsbridge: convert annotated TLF shells to CDISC ARS v1.0 JSON

arsbridge reads a lead programmer's already-annotated TLF shell Word
document and the study's ADaM specification Excel, and produces a valid
CDISC Analysis Results Standard (ARS) v1.0 ARM-TS JSON file consumable
by
[`siera::readARS()`](https://clymbclinical.github.io/siera/reference/readARS.html).

## Details

Annotation reading is style-agnostic and uses two passes together to
extract as many annotation variants as possible: a deterministic
four-layer regex detector (font colour, character formatting, brackets,
plain text) and an LLM primary reader that separates display label from
variable reference in layouts no regex was written for. A row is read if
either pass finds it; on conflict the LLM wins and a warning flags it.

Core principle: the package extracts and converts – it does not invent.
Every LLM-proposed variable passes a hard gate against the ADaM
specification, so a variable absent from the spec is rejected and
logged, never shipped. Every variable in the ARS output traces back to a
real annotation grounded in the study's ADaM spec. See the "How
arsbridge reads an annotated shell" vignette.

## See also

Useful links:

- <https://github.com/tavakohr/arsbridge>

- Report bugs at <https://github.com/tavakohr/arsbridge/issues>

## Author

**Maintainer**: Hamid Tavakoli <htavakoli@gmail.com>
([ORCID](https://orcid.org/0000-0003-0574-7739))

Authors:

- Hamid Tavakoli <htavakoli@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-0574-7739))
