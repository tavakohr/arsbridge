# Package index

## Pipeline

Convert an annotated shell + ADaM spec to ARS, ARD, and formatted TLFs.

- [`spec_to_ars()`](spec_to_ars.md) : Convert annotated TLF shell and
  ADaM spec to CDISC ARS JSON
- [`spec_to_ars_example()`](spec_to_ars_example.md) : Run spec_to_ars()
  against the bundled example inputs
- [`ars_to_ard()`](ars_to_ard.md) : Execute ARS JSON and return an ARD
  object using 'cards'
- [`ars_to_tfrmt()`](ars_to_tfrmt.md) : Build a tfrmt specification for
  one ARS output
- [`ars_to_tfrmt_list()`](ars_to_tfrmt_list.md) : Build tfrmt specs for
  every renderable ARS output
- [`ars_render_tlf()`](ars_render_tlf.md) : Render an ARS output to a
  formatted clinical table
- [`ars_render_all()`](ars_render_all.md) : Render every output of a
  reporting event into one Word document
- [`ars_render_listing()`](ars_render_listing.md) : Render an ARS
  listing output to a GT table
- [`ars_render_figure()`](ars_render_figure.md) : Render an ARS figure
  output to a ggplot

## Inferential statistics & partial results

Executable descriptors for inferential statistics, and the manual-fill
round-trip for cells arsbridge cannot yet compute.

- [`ard_cmh_test()`](ard_cmh_test.md) : Cochran-Mantel-Haenszel test as
  an ARD row
- [`ard_proportion_ci_exact()`](ard_proportion_ci_exact.md) : Exact
  (Clopper-Pearson) binomial confidence interval as ARD rows
- [`ars_manual_worklist()`](ars_manual_worklist.md) : Manual-derivation
  worklist from an ARD
- [`ars_validate_manual_fills()`](ars_validate_manual_fills.md) :
  Validate manually-filled ARD cells

## Diagnostics

Plain-English findings that point at the input document to fix.

- [`ars_diagnostics()`](ars_diagnostics.md) : Retrieve pipeline
  diagnostics from the most recent run
- [`ars_blockers()`](ars_blockers.md) : Blocking problems from the most
  recent run, in plain English

## LLM providers

Configure and select the LLM that reads variant shells.

- [`set_anthropic_key()`](set_anthropic_key.md) : Set your Anthropic API
  key for arsbridge
- [`set_openai_key()`](set_openai_key.md) : Set your OpenAI API key for
  arsbridge
- [`set_gemini_key()`](set_gemini_key.md) : Set your Gemini API key for
  arsbridge
- [`set_llm_key()`](set_llm_key.md) : Set the API key for any supported
  LLM provider
- [`check_anthropic_key()`](check_anthropic_key.md) : Check whether the
  Anthropic API key is set
- [`get_active_llm()`](get_active_llm.md) : Get the active LLM provider
  and configurations
- [`show_active_llm()`](show_active_llm.md) : Show the active LLM
  provider and API key status

## Examples

- [`arsbridge_example()`](arsbridge_example.md) : Bundled training files
  shipped with arsbridge
