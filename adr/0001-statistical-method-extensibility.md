# ADR 0001 — Statistical-method extensibility: bound the boundary, not the contents

* Status: Accepted
* Date: 2026-06-19
* Context owner: arsbridge maintainers

## Context

arsbridge maps each TLF analysis onto a small set of descriptive `{cards}`
methods (summary statistics, count/percentage, AE frequency, subject counts,
listings, basic figures). The engine that produces the deliverable is the
deterministic emitter in `R/ars_to_code.R`: `.emit_block()` is a `switch` over
`method_id` that writes a pure-`{cards}` R script, and the default execution
path *sources that emitted script* (`.run_emitted_block()`). So the emitter is
the engine — emitted code and computed ARD are the same code.

A real shell exposed the limit. Output **T-14.2.1** ("Proportion of Subjects
Achieving EASI 75 at Week 16", responder analysis, ITT) needs:

* response rate `n / N' (%)` — descriptive, already supported;
* Clopper-Pearson exact CI — not in `{cards}` core;
* Cochran-Mantel-Haenszel p-value — not in `{cards}` core;
* Newcombe difference CI — no direct wrapper anywhere in the stack;
* non-responder imputation (NRI) — not a statistic at all, an ADaM derivation.

`{cards}` is a *descriptive* ARD engine by design. Inferential/model-based
statistics live in `{cardx}` (an open collection of `ard_*` test wrappers).
NRI belongs upstream in ADEFF, not in the TLF layer.

The naive response is to keep adding `switch` branches — one per statistic.
That does not scale: the statistics space is **open-world** while a `switch` is
**closed-world**. We cannot predict or pre-build a branch for every shell, and
chasing completeness is a losing game.

## Decision

We do **not** aim to compute every statistic. We architect the *boundary*, not
the *contents*, on three principles.

### 1. Wire the finite head; refuse the infinite tail

Clinical TLFs cluster: ~15–20 statistical patterns cover most of any
submission (descriptive, count/%, CMH, chi-square, t-test, Clopper-Pearson /
Wilson CIs, Kaplan-Meier, basic ANCOVA/logistic). `{cardx}` already enumerates
~20 `ard_*` functions over this head. That head is finite and worth wiring. The
sophisticated tail (MMRM, multiple imputation, Cox, Bayesian, adaptive designs)
is unbounded **and** demands analyst judgement (model specification, covariate
choice, convergence). arsbridge will not auto-generate the tail — fabricating a
wrong number is worse than declining.

### 2. Methods are data, not code — a descriptor contract on the ARD shape

The extensibility boundary already exists: every `cards`/`cardx` function
returns the same tidy **ARD shape**. `{cardx}` scales precisely because it is an
open bag of ARD-returning functions, not a `switch`. arsbridge should adopt the
same contract instead of growing `.emit_block`.

A method becomes a **descriptor** (data), not a `switch` branch (code):

```
method = {
  id,                 # e.g. MTH_CMH_TEST
  operands_schema,    # what the spec must supply (strata var, ref group, CI method, ...)
  emit_template,      # the cards/cardx call to write into the deliverable
  tfrmt_mapping       # which returned stat names map to which shell cells
}
```

The registry becomes a table of descriptors. A new statistic is **one
descriptor + one test**, contributed without core surgery. The dispatcher in
`.emit_block()` looks the descriptor up rather than branching on a literal.

### 3. Tiered, honest degradation — the placeholder is a feature

Route each analysis by capability tier:

| Tier | Examples | arsbridge action |
| --- | --- | --- |
| Descriptive | n, %, mean, SD | auto-generate (`cards`) |
| Standard test | CMH, chi-square, Clopper-Pearson CI, t-test | auto-generate (`cardx` descriptor) |
| Model-based | MMRM, Cox, ANCOVA, multiple imputation | **scaffold** — emit correct ADaM filter, population, and data prep with a stubbed model call marked `# TODO analyst`, then route to a human |
| Novel / unknown | bespoke | numbered placeholder + capability report (current behaviour) |

The Tier-3 *scaffold* is the key move: arsbridge does the ~80% deterministic
plumbing (right data, right population, right output shape) and hands the
analyst a runnable stub, without pretending to auto-solve the model. The Tier-4
**placeholder is intentional**, not a failure — it preserves shell numbering and
states plainly that the table needs a validated analysis script. A render
*error* is a separate, clearly-labelled case (see `gate` in
`R/ars_render_docx.R::.add_placeholder`).

### 4. Generation stays deterministic — the LLM classifies, never invents code

The LLM's job is **open-world recognition**: read the shell + annotation and
classify it to a known `method_id` with filled operands ("CMH stratified by
region", "Newcombe vs placebo", "exact CI"). Code emission stays **closed-world
and deterministic**, driven only by descriptors. Letting the LLM emit free-form
statistical R would break reproducibility and validation — unvalidatable code
has no place in a regulatory submission.

## Consequences

**Positive**

* arsbridge's value proposition is sharpened: deterministic, validated
  spec → data → shell plumbing. Statistics are pluggable cargo.
* New standard tests extend the tool without touching the core `switch`.
* Coverage is honest and auditable; nothing is silently coerced into a wrong
  number.

**Negative / costs**

* Coverage is deliberately finite. Some shells will always land on a
  placeholder or a scaffold. This is accepted, not a defect.
* Refactoring `.emit_block()` from a `switch` to a descriptor dispatcher is real
  work, plus matching `tfrmt` mapping for between-group statistics (a
  difference column + p-value footer is a different cell shape from per-arm
  n/%).
* `{cardx}` becomes a dependency once the first standard-test descriptor lands.

## Worked example — T-14.2.1, after this ADR

* `n / N' (%)` — Tier 1, already emitted.
* Clopper-Pearson CI — Tier 2 descriptor → `cardx::ard_categorical_ci(method = "clopper.pearson")`.
* CMH p-value — Tier 2 descriptor → `cardx::ard_stats_mantelhaen_test()`.
* Newcombe difference CI — Tier 2 descriptor with a custom wrapper
  (`DescTools::BinomDiffCI(method = "scorecc")`); no direct `cardx` function.
* NRI — **out of scope for the TLF layer**; pre-derive imputed `AVAL` in ADEFF
  upstream. arsbridge tabulates the supplied cut.

So even a perfect ADEFF does not make CMH / CP / Newcombe appear — the engine
still needs the descriptors. Both the upstream data fix and the engine
descriptors are required.

## Implementation order (non-binding)

1. Narrow the capability gate (`R/capability.R`) so Tier-2 stats are no longer
   auto-rejected before the engine can try them.
2. Add Tier-2 descriptors (CMH, Clopper-Pearson first — direct `cardx`).
3. Refactor `.emit_block()` to dispatch on descriptors.
4. Extend `tfrmt` mapping for between-group statistics.
5. Add the Newcombe wrapper.
6. Define the Tier-3 scaffold emitter (model-based stubs).
