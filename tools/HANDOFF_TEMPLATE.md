# Handoff template

Copy this file to `notes/handoffs/HANDOFF_<area>_<yyyy-mm-dd>.md` and fill it
in. `notes/` is gitignored, so a handoff lives on the machine that wrote it and
travels by hand — which is exactly why it has to be self-contained.

**The two blocks marked MANDATORY exist because their absence has cost real
time.** Half the findings in the 2026-08-10 locked-machine RCA described defects
that had been fixed days earlier, and nobody could tell until the installed
version was compared against `HEAD`. A handoff without a version stamp is a
description of an unknown build.

Delete the guidance in brackets as you go. Sections 3-7 are the shape the
existing handoffs already share; keep their order so a reader who has read one
can navigate the next.

---

# HANDOFF: [area, e.g. "Label fill and Shiny editor defect stack"]

Updated: [yyyy-mm-dd]

**Purpose of this file:** [one sentence — what the next person is meant to do
with it.]

**Status:** [where the work stands. Name merged PRs and what is next.]

**Evidence:** [what backs the findings below — a repro script, a workflow run
id, a diagnostics report, screenshots. Say where each artifact lives.]

**Constraint:** [anything that limits the reader — a locked machine, data that
cannot leave a site, a client shell that must not be committed.]

**Companions:** [other files needed to act on this one, e.g. a plan under
`notes/plans/`.]

No prior chat context should be required to act on this file.

---

## 1. Repository state — MANDATORY

Run this and paste the output verbatim. Do not summarise it.

```r
packageVersion("arsbridge")
```

```sh
git rev-parse --short HEAD
git log -1 --pretty='%h %s'
git status --short --branch | head -1
```

- Branch: `[...]`
- Working tree: `[clean | dirty — list what is uncommitted]`
- HEAD: `[sha] [subject]`
- Package version, **as loaded** (`packageVersion("arsbridge")`): `[0.1.0.90xx]`
- Package version, **as sourced** (`DESCRIPTION`): `[0.1.0.90xx]`
- Merged PRs relevant to this work: `[#n — merged as <sha> — <url>]`

> Those two version lines differ whenever the session is running
> `devtools::load_all()` over an older install, or the machine's install is
> stale. **When they differ, say so here in words** — it is the single most
> common reason a handoff's findings do not reproduce.

If the work happened on a machine whose install you did not build, state how
the install got there and when.

## 2. Fill debrief — MANDATORY when anything failed to fill

A census without its rollup is not a debrief. Run all three frames and paste
them; they are what tells the next reader *which stage* each pending cell died
at, instead of leaving them to reconstruct it by hand.

```r
res     <- ars_fill_shell(shell_path, ard, output_path = tempfile(fileext = ".xlsx"))
summary <- ars_fill_summary(res$census)

summary$sheets    # cells by status, per sheet
summary$columns   # per display column: how many filled, and the modal reason
summary$reasons   # each distinct reason, with its author-facing hint
```

Or write the workbook and attach it: `write_fill_debrief(res$census, path)`.

- `sheets`: [paste]
- `columns`: [paste]
- `reasons`: [paste]

For a whole run, `ars_workflow_run()` returns the same census on its payload,
alongside `payload$validation_gate` — paste the gate's `status` and any
`blocking_refs` too, since a blocked gate means no ARD or fill was attempted at
all and an empty census means nothing.

---

## 3. The problem

[What was observed, in the words of whoever observed it. The literal symptom —
"Low/Medium/High columns stayed `xx (xx.x)` while Total filled" — not the
diagnosis. Include the shell and study, and what the reader should expect to
see instead.]

## 4. Verified findings

[Only what was checked. Mark anything unverified as a hypothesis and say what
would settle it — RCA guesses stated as fact have been wrong here before.]

| Finding | Status | Evidence |
|---|---|---|
| [claim] | CONFIRMED / DISPROVED / HYPOTHESIS | [`R/file.R:123-130`, repro script, test name] |

[Then, per confirmed finding: the mechanism, with `file:line` anchors. Say
which layer it lives in — parse, build, validate, execute, fill, render.]

## 5. Target design

[What the fixed behaviour is, and why that shape rather than the alternatives
considered. Name the design rules it has to respect.]

## 6. Implementation plan

[One PR per phase, in dependency order. For each: title in this repo's commit
style, files touched, the tests to write first, and the blast radius. Say
explicitly what is deliberately deferred and why.]

## 7. Risks, and what is out of scope

[What could break, what is knowingly not addressed, and what would have to be
true for this plan to be wrong.]

## 8. How to see the problem, and how to verify the fix

[A block someone can paste. Reproduce first, then fix, then re-run.]

```r
devtools::load_all(".")
# reproduce:
# ...

# verify:
testthat::test_dir("tests/testthat")
```

[If the work is on a locked machine, say which artifacts may leave it and which
may not. See `tools/README_privacy_transfer.md`.]
