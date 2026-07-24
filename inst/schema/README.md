# Bundled schemas

## `arsbridge_supplement_v3.schema.json`

arsbridge's own schema for the no-API supplement workflow. See
`ars_copilot_instructions()`.

## `cdisc_ars_v1.0.0.schema.json` and `cdisc_ars_v1.0.0_ldm.yaml`

The official CDISC Analysis Results Standard v1.0 model, vendored so that
`ars_conformance()` validates against a pinned release rather than a moving
branch.

- Source repository: <https://github.com/cdisc-org/analysis-results-standard>
- Release tag: `v1.0.0` (April 19, 2024)
- Files:
  - `cdisc_ars_v1.0.0_ldm.yaml` -- the LinkML source of truth
    (`model/ars_ldm.yaml` at the tag)
  - `cdisc_ars_v1.0.0.schema.json` -- the JSON Schema generated from it
    (`project/jsonschema/ars_ldm.schema.json` at the tag)
- Retrieved: 2026-07-23
- Upstream license: MIT (see the LICENSE file in the source repository)

To regenerate the JSON Schema from the LinkML source instead of using the
repository's pre-generated export:

```bash
git clone --branch v1.0.0 https://github.com/cdisc-org/analysis-results-standard.git
python -m pip install --upgrade linkml
gen-json-schema analysis-results-standard/model/ars_ldm.yaml > cdisc_ars_v1.0.0.schema.json
```

To move to a newer release of the standard, replace both files with the same
paths at the new tag, rename them to carry the new version, and re-run the
`ars_conformance()` tests -- they pin what the generator is expected to
diverge on, so a standard upgrade shows up as test failures to review rather
than as silent drift.
