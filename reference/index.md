# Package index

## Pipeline

The end-to-end pipeline: annotated shell and ADaM spec in, formatted
clinical table out.

- [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  : Convert annotated TLF shell and ADaM spec to CDISC ARS JSON
- [`spec_to_ars_example()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars_example.md)
  : Run spec_to_ars() against the bundled example inputs
- [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  : Execute ARS JSON and return an ARD object using 'cards'
- [`ars_to_tfrmt()`](https://tavakohr.github.io/arsbridge/reference/ars_to_tfrmt.md)
  : Build a tfrmt specification for one ARS output
- [`ars_to_tfrmt_list()`](https://tavakohr.github.io/arsbridge/reference/ars_to_tfrmt_list.md)
  : Build tfrmt specs for every renderable ARS output
- [`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md)
  : Render an ARS output to a formatted clinical table
- [`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md)
  : Render every output of a reporting event into one Word document
- [`ars_render_listing()`](https://tavakohr.github.io/arsbridge/reference/ars_render_listing.md)
  : Render an ARS listing output to a GT table
- [`ars_render_figure()`](https://tavakohr.github.io/arsbridge/reference/ars_render_figure.md)
  : Render an ARS figure output to a ggplot

## Inferential statistics and partial results

Executable descriptors for inferential statistics, and the manual-fill
round-trip for cells arsbridge cannot yet compute automatically.

- [`ard_cmh_test()`](https://tavakohr.github.io/arsbridge/reference/ard_cmh_test.md)
  : Cochran-Mantel-Haenszel test as an ARD row
- [`ard_proportion_ci_exact()`](https://tavakohr.github.io/arsbridge/reference/ard_proportion_ci_exact.md)
  : Exact (Clopper-Pearson) binomial confidence interval as ARD rows
- [`ars_manual_worklist()`](https://tavakohr.github.io/arsbridge/reference/ars_manual_worklist.md)
  : Manual-derivation worklist from an ARD
- [`ars_validate_manual_fills()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_manual_fills.md)
  : Validate manually-filled ARD cells

## Diagnostics

Plain-English findings that point at the exact location in the input
document to fix.

- [`ars_diagnostics()`](https://tavakohr.github.io/arsbridge/reference/ars_diagnostics.md)
  : Retrieve pipeline diagnostics from the most recent run
- [`ars_blockers()`](https://tavakohr.github.io/arsbridge/reference/ars_blockers.md)
  : Blocking problems from the most recent run, in plain English

## LLM providers

Configure and select the LLM that reads variant shell layouts and
enriches ARS metadata.

- [`set_anthropic_key()`](https://tavakohr.github.io/arsbridge/reference/set_anthropic_key.md)
  : Set your Anthropic API key for arsbridge
- [`set_openai_key()`](https://tavakohr.github.io/arsbridge/reference/set_openai_key.md)
  : Set your OpenAI API key for arsbridge
- [`set_gemini_key()`](https://tavakohr.github.io/arsbridge/reference/set_gemini_key.md)
  : Set your Gemini API key for arsbridge
- [`set_llm_key()`](https://tavakohr.github.io/arsbridge/reference/set_llm_key.md)
  : Set the API key for any supported LLM provider
- [`check_anthropic_key()`](https://tavakohr.github.io/arsbridge/reference/check_anthropic_key.md)
  : Check whether the Anthropic API key is set
- [`get_active_llm()`](https://tavakohr.github.io/arsbridge/reference/get_active_llm.md)
  : Get the active LLM provider and configurations
- [`show_active_llm()`](https://tavakohr.github.io/arsbridge/reference/show_active_llm.md)
  : Show the active LLM provider and API key status

## Working without an API key

Boost accuracy with no LLM API by ferrying the work through a chat
assistant (Copilot/ChatGPT). See the “Using arsbridge without API
access” article.

- [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
  : Write the Copilot instruction file for the supplement workflow
- [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  : Validate a Copilot supplement file before running spec_to_ars()

## Examples

Access the bundled APX-DRM-301 study files for offline testing.

- [`arsbridge_example()`](https://tavakohr.github.io/arsbridge/reference/arsbridge_example.md)
  : Bundled training files shipped with arsbridge
