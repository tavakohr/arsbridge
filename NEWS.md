# arsbridge (development version)

* **The shell layout's `kind` field now answers one question instead of
  four.** It held four vocabularies at once: how a row expands
  (`categorical`, `continuous`, `row`, `level`, `label`, `nested_*`), which
  statistic a one-line row's cell shows (`subject_count`,
  `subject_count_pct`, `filtered_count`, `filtered_count_pct`), where the row
  came from (`supplement_added`), and why it carried no analysis (`manual`).
  Because they shared a field, code asking any one of them had to know the
  whole set -- and since the field was derived from the row's METHOD, a
  filter that changed the method could change what the renderer believed the
  row's STRUCTURE was.

  `kind` is now structural only, from a closed set: `categorical_block`,
  `stat_block`, `scalar_row`, `nested_parent`, `nested_child`, `level_row`,
  `label_row`, or `NA` where the structure was not proved -- which is a real
  answer and never a synonym for "one row". The other three questions moved
  to `stat_form`, `provenance` and `status`, each with its own closed
  vocabulary, and a field with nothing to say is omitted rather than written
  as null.

  Nothing about how a study renders changed: the same rows, in the same
  order, with the same labels, placeholders and markers. Two renderer
  behaviours that used to be read off the shape are now explicit, LOCAL
  rendering rules that never write back into the canonical metadata -- a
  reserved row still owns its block, and an orphaned nested child still
  expands under its authored label while remaining a `nested_child`.

  This is preparation: block shape is still derived from the method, exactly
  as before. Deciding it from structural evidence instead is the next change,
  and the values downstream code reads are already the structural ones.

* **A restriction the grammar can READ, the row can now CARRY.** A row's
  annotation could state a filter that parsed perfectly -- two conditions
  joined, a range, an envelope naming three -- and the builder still had one
  place to put it: a FLAT subset, one dataset, one variable, one comparator,
  one value. Anything larger did not fit, so the row reserved. The reservation
  was honest, but it withheld results nobody needed withheld: the clause was
  understood, and the carrier for it already existed -- the supplement path had
  been riding on `data_subset_compound` all along.

  There is now one reading of the annotation, shared by both row builders,
  with three outcomes: reserve, carry as a compound, or the flat shape the
  builder has always consumed. Reading is unchanged; this is about where the
  answer goes.

  Two limits stay exactly where they were, because accepting a tree shape is
  not the same as being able to execute it. A mixed-dataset compound is
  carried, and the restriction PLAN decides whether it runs -- a shape whose
  row coherence cannot be recovered still blocks, with its reason recorded,
  rather than being filtered by a guess. And `NOT`, or anything only partly
  readable, still reserves.

  A restriction that arrives by annotation and by supplement now converges on
  one DataSubset rather than two: subsets de-duplicate on what they filter, so
  provenance no longer mints a second definition of the same population.

  The reservation for an authored rule this version cannot carry out is now
  asked once, of the annotation alone, before the filter's source is decided.
  It therefore holds whether the filter is flat, compound, supplied by a
  supplement, or absent altogether -- a supplement is authoritative about
  which records survive, not evidence that a rule about the absent ones was
  implemented.
* **An authored instruction no restriction can carry out now reserves the
  row.** An annotation can say two things at once -- a filter, and a rule for
  records the filter does not select:

      ADEFF.ANL01FL='Y' and ADEFF.AVAL=1
        (a subject with no Week 16 record is a non-responder)

  The bracket is masked as an aside, which is right as far as the FILTER goes:
  it states no condition. The filter is then read, correct about which records
  qualify and silent about the rule. Computing it alone reports a responder
  rate over a smaller denominator than the one asked for, and nothing on the
  analysis says so.

  The reason is structural rather than a list of instruction words: a filter
  says which records SURVIVE, so an aside that speaks about the records it
  EXCLUDES is stating something no `WHERE` clause can implement -- those
  records are not there to be acted on.

  **Three** things must hold, and none is enough alone -- each exists because
  dropping it reserves a row that computes perfectly well:

  * **absent data**, as a construction: `with no`, `without`, `has no`,
    `missing`, `absent`. Not merely a negating word somewhere -- a negative
    attaches to whatever follows it, so `(records are not shown separately)`
    negates the *showing* and `(no record-level adjustment is applied)` negates
    the *adjustment*. Neither says a record is absent;
  * the **unit of observation** -- a subject, a record, a visit -- which is
    what the absence must be an absence *of*;
  * an **assignment** of a computed state, saying what becomes of those
    records. A bare copula is not enough: `(missing visits are displayed
    separately)` states an absence, of an observation, in a sentence containing
    `are`, and instructs no computation — it says where the rows appear, not
    what they count as. A treatment verb qualifies outright; `as <state>`
    qualifies whatever verb carries it, so `(… are reported as non-responders)`
    reserves while `(… are displayed as separate rows)` does not.

  Together they read as one sentence: *a unit whose data is NOT THERE IS
  treated as something*. So `(no units)`, `(except per protocol)`, `(except
  visit 1)`, `(records are not shown separately)`, `(without adjustment the
  totals are unchanged)`, `(missing visits are displayed separately)` and
  `(missing records are listed separately)` all go on computing, while `(a
  subject with no visit record is a non-responder)`, `(subjects with missing
  data are non-responders)` and `(… are reported as non-responders)` reserve.

  A unit, a planned N, a protocol reference, a variable that supplies a label
  all still compute: they describe the row rather than instructing a
  computation. Literals are masked first, so a category called `'Not
  Reported'`, or one written `'Cohort (no baseline)'`, is a value.

  Known limit, recorded rather than papered over: an unrepresented instruction
  phrased without a negation ("values are imputed from baseline") is not yet
  detected. This closes the case where the instruction concerns records the
  filter cannot reach -- the one that turns a filter into a wrong number rather
  than an incomplete one.

* **Leftover text that changes what a restriction MEANS no longer disappears.**
  The where-clause grammar finds a condition inside an operand and then asks
  one question about whatever text is left over: does it state a condition of
  its own? If not, the leftover was prose and was discarded. But text can
  decide what a condition means without stating one, and two kinds do.

  A **negating word** inverts it. `ADSL.USUBJID with no ADCM record where
  ADCM.CMATC3CD='D11AH'` was read as `CMATC3CD='D11AH'` -- so a row labelled
  "did not receive X" selected exactly the subjects who did. That is worse
  than computing nothing: it computes something, and it looks right.

  A **qualified reference in front of the condition** scopes it. `ADSL.USUBJID
  with an ADCM record where A, and with an ADCM record where B` asks a
  per-subject question about two records; read as a row-wise `AND` it asks for
  one record satisfying both, which is a different set of subjects -- the
  counterexample the restriction planner is built around.

  Both now mean the operand was not fully read, and an operand that was not
  fully read reserves. **Position decides**, so the forms this grammar has
  always read go on reading: a semicolon ends the head, leaving `HEAD;
  <filter>` intact; a note *after* a complete condition (`; label ADQX.QXCAT`)
  is still an aside; so is a descriptive suffix (`(per protocol)`); and
  literals are masked first, so a value legitimately called `'NOT REPORTED'`
  is a value.

  One conservative false positive is accepted deliberately rather than
  narrowed away: `count of ADEFF.USUBJID with ADEFF.ANL01FL='Y'` states a
  *statistic* in front of a genuine filter, and now reserves. Telling it apart
  from the existential form above needs a positive grammar for head directives
  rather than an exception carved out of the guard -- and preventing two
  demonstrably wrong filters is worth one row waiting for that grammar.

* **A committed mutation harness** (`tools/mutation_harness.R`) that refuses
  verdicts it cannot stand behind: a mutation that does not actually change
  the file is reported as unapplied rather than undetected, only assertion
  failures count as detection, and every restore is proven byte-for-byte.

* **An annotation's envelope is split from its head, and a bare variable
  inside it inherits only what the spec confirms.** A shell names what a row
  reports and then says which records it reports on --
  `ADPR.PRTRT (when PRCAT='DIAGNOSTIC TESTING' AND PRPRESP='Y')`. Those are two
  languages in one string: only `WHERE` was recognised, so `(when ...)` and
  `[where ...]` left the head as an unplaceable token and reserved a row that
  was written perfectly clearly.

  Where the form *was* recognised, the context it supplies was applied as a
  text prefix -- the head dataset pasted onto the front of the tail. That
  qualified only the FIRST clause, because a prefix lands once on a string, so
  `WHERE A='1' AND B='2' AND C='3'` filtered on one third of what the author
  wrote; and it ASSUMED the variable lives on that dataset rather than
  confirming it.

  The envelope is now parsed, and every bare name in it is qualified against
  the head reference's dataset **only where the ADaM spec confirms that exact
  `DATASET.VARIABLE`**. A name both the head and another dataset carry
  resolves to the head; a name only another dataset carries is unresolved,
  never borrowed; an explicitly qualified foreign clause does not move the
  context for the clauses after it; and quoted values and comparator syntax
  are never rewritten. Inheritance is **all-or-none** -- one name that cannot
  be confirmed reserves the whole filter, because a filter honoured in part
  restricts by less than was written and says nothing about it. Without a spec
  in reach nothing is confirmable, which is the same answer as "not there";
  an envelope whose clauses all name their own dataset asks nothing of the
  spec and reads without one.

  Where the envelope CLOSES is structure too, so the boundary is found on
  masked text: a bracket inside a quoted value -- `(when CAT='A)B' AND
  FLAG='Y')` -- is part of the value, and counting it as the closer would end
  the envelope mid-literal and keep whichever fragment came first. Only
  literals are hidden; a comparator's own `IN (...)` and a nested group are
  still counted.

  A single-clause envelope now computes. A compound one produces the correctly
  typed condition and still reserves, because the row DataSubset cannot carry
  a compound yet -- reserved for the right reason, and nothing about the
  annotation has to change when the carrier lands.

* **Boolean structure is parsed as a tree, with precedence.** Brackets bind
  tightest, then `NOT`, then `AND`, then `OR` -- the precedence every SQL and
  SAS author already writes to. `A AND (B OR C)` produces `AND(A, OR(B, C))`,
  `(A OR B) AND C` produces `AND(OR(A, B), C)`, and `A OR B AND C` produces
  `OR(A, AND(B, C))`. The population that motivated the previous release's
  refusal -- `ADSL.SAFFL='Y' AND (ADSL.COHORT='A' OR ADSL.COHORT='B')` -- is
  now carried as the nested condition it states, instead of being reserved for
  its structure. The nested tree reaches the analysis set, the column grouping,
  the row subset and the restriction plan, so what is executed and what is
  emitted are the same restriction.

  An accepted expression must consume the WHOLE token stream. Trailing text, a
  dangling operator, an unmatched bracket or an operator with nothing to bind
  refuses and reserves -- a tree built from part of the input is the same class
  of wrong answer as a flat split, because it looks finished. Where the parser
  cannot prove a complete parse, the reservation behaviour introduced in the
  previous release is unchanged.

  Three separations keep the structural reading off the atoms: quoted literals
  are masked before any structure is read, so `RACE='BLACK OR AFRICAN AMERICAN'`
  carries no joiner; atomic forms that own a bracket or the word `NOT` --
  `IN (...)`, `NOT IN (...)`, `is.na(...)`, `not missing`, a `BETWEEN`'s inner
  "and" -- are masked too; and a bracketed note that encloses no operand
  (`(mg)`, `(N=XX)`, `(per protocol)`) groups nothing and is left alone. The
  leaf grammar is unchanged.

* **An operand must be fully read, not merely started.** Consuming every
  structural token is not the same as consuming every condition: the leaf
  battery is unanchored and stops at the first form it recognises, so
  `A='Y' B='N'` was read as `A='Y'` and the second condition simply
  disappeared. Each operand is now checked for condition-bearing residue --
  the text the recognised leaf did not consume -- and an operand that leaves
  any reserves. Descriptive suffixes are unaffected: the leftover is put
  through the same note masking the expression itself gets before it is asked
  whether it carries an operator, so `AVAL GT 0 (per protocol)` and
  `SAFFL='Y' (N=XX)` still compute.

  This found a live defect in a real shell, fixed by the comma rule below: a
  population written `(ADSL.SAFFL='Y', ADVS.ANL01FL='Y')` was emitted as the
  safety flag alone. The second condition appeared nowhere in the reporting
  event -- the string "ANL01FL" occurred zero times in it -- and the figure's
  population was merged with the plain safety population it was written to
  differ from.

* **A comma between complete conditions is read as a conjunction.** Shells
  write a population as a list and mean every item of it, so
  `(ADSL.SAFFL='Y', ADVS.ANL01FL='Y')` now produces `AND(SAFFL, ANL01FL)`.

  This is annotation shorthand, not a claim that a comma is a Boolean
  operator, and the rule is narrow on purpose: it applies only when the WHOLE
  operand is a list of two or more independently complete atomic conditions,
  each read to its end with no residue, and only when the expression states no
  Boolean operator of its own. `A='Y', B='N' OR C='Y'` reserves rather than
  deciding how a comma binds against `OR`. So does a list with an item that is
  not a condition -- prose, a bare reference, or nothing at all
  (`A='Y',`). Commas that belong to something else are untouched:
  `IN ('A','B')` keeps its value list, `VAR='A,B'` keeps its value, and
  `A='Y' (N=10, planned)` keeps its aside. Prose containing a comma states no
  filter and still reserves nothing -- the rule may only ever rescue text that
  would otherwise be refused.

* **A bracket in operand position is an operand.** `A='Y' (N=XX)` puts the
  bracket after a condition, where an aside is exactly what it is. `A='Y' AND
  (N=XX)` puts it after a joiner, where the author has said the next thing is
  a term of the restriction -- and a term that quietly evaluates to nothing
  removes itself from the filter. `A AND ()` is the same failure with the span
  empty, and is now diagnosed as an empty operand rather than reduced to `A`.
  More generally, once any operand of an expression has parsed into a
  condition, an operand that states none reserves: the joiner is the author
  saying these are terms. Where nothing parsed, the words are a sentence --
  "safety population or better" states no filter and still reserves nothing.

* **Negation is recognised at its own precedence and reserves.** ARS has a
  `NOT`, but nothing downstream executes one: the evaluator and the predicate
  emitter both answer an unrecognised operator with "keep every row", so an
  emitted `NOT` would be a filter that silently does nothing. For a condition
  on another dataset it is worse -- negating row-wise and then projecting to
  subjects ("has a record that is not X") is a different set from negating the
  projection ("has no record that is X"), and the expression does not say which
  was meant. A `NOT` that governs a real condition therefore reserves, with a
  message naming the construct. A `NOT` that governs no condition -- "not
  applicable" -- negates nothing and states no filter, as before.

* **A displayed condition brackets an operand that binds differently.**
  `.where_to_annotation()` rendered a nested compound flat, so a typed
  `AND(A, OR(B, C))` was shown as `A and B or C` -- which states a different
  restriction, because `AND` binds tighter. The display string now brackets a
  nested operand whose operator differs from the one outside it. Same-operator
  nesting is associative and is left unbracketed, so a range (stored as an
  `AND` of `GE` and `LE`) picks up no punctuation it does not need. The
  round-trip property test is a serialization consistency check between this
  renderer and the parser; it makes no claim about the author's intent.

* **A restriction this grammar cannot represent is refused, not approximated.**
  The flat clause splitter answered an expression it could not hold with a
  DIFFERENT expression that is valid, executable and restricts other records:
  `A AND (B OR C)` and `(A OR B) AND C` both became `AND(A, B, C)`, which asks
  for something the author did not, and which is satisfied by no record at all
  when the two disjuncts exclude each other. This reached the emitted event
  through every path that parses a condition from text, and the most costly was
  the population -- an analysis set no subject belongs to, with every
  percentage under it divided by the wrong N and nothing on the page to say so.

  The refusal that replaced it is what the tree parser above was then built on,
  and those two expressions now *parse*; what remains refused is what still has
  no representation -- a negation, and any expression this grammar cannot read
  to the end. The line it draws is narrow in both directions: an unreadable
  restriction reserves rather than computing, because to the reader of the
  number an unrepresentable filter and an unreadable one are the same failure;
  and parentheses that wrap a whole expression or hold a single condition,
  `IN (...)`, `is.na(...)`, `not missing`, a `BETWEEN`'s inner "and", a
  comparator spelled with `!`, and prose carrying an English "not" are all left
  alone, because withholding a correct result is its own wrong answer.

* **A clause of a joined restriction is no longer dropped in silence.** Inside
  `A and B` the author has already said both are clauses, so a clause carrying
  an operator this grammar cannot read is a dropped condition even when it names
  its variable without a dataset -- which is how shells routinely write a filter
  after the head reference (`ADQX.QXTRT WHERE QXCAT='X' AND QXPRESP='Y'`). Only
  the first clause was qualified; the others matched no pattern, carried no
  `DATASET.VARIABLE`, and so were never even counted as dropped. The row
  filtered on one clause of three and reported nothing.

* **A filter that was read but cannot be carried reserves instead of vanishing.**
  A row's subset holds a single condition, so a compound one flattened to
  `NULL` -- and `NULL` already meant "this annotation states no filter". A
  perfectly readable `A AND B` therefore produced an analysis with no
  `DataSubset`: it computed over every record in the dataset and looked
  finished. It now carries the author's text and withholds the result. This is
  a reservation, not a refusal; when the subset builder can carry a compound
  expression these rows compute, and no annotation has to change.

* **A filter says which records survive, not which statistic is reported.**
  "The filter is on the row's own variable" and "I could not read the filter"
  both presented as an empty filter variable, and both selected the
  subject-count family -- so an unreadable annotation was typed as a count of
  something nobody could evaluate, under a line displaying two statistics, and
  `ADQX.QXVAL WHERE ADQX.QXVAL GT 0` became a subject count under a block asking
  for Mean/SD. A filter is now classified as `none`, `on_primary`,
  `scoping_other`, `mixed_conjunctive` or `unknown` -- read from the restriction
  the row will actually compute under rather than from whether one function
  managed to flatten it -- and **the role selects no method**. An unreadable
  restriction contributes nothing at all: the row is reserved on the marker it
  carries, and whatever method it shows for layout comes from evidence that is
  independently known.

  One case still lets a restriction select the method, and it is a **temporary
  structural dependency rather than a semantic rule**: a restriction pinning
  the row's variable to a single value keeps the subject-count family. Not
  because equality requests a count -- a shell may filter to one value and then
  ask for Mean/SD, and the filter would still only be saying which observations
  survive -- but because block construction downstream reads what a block IS
  from the method its first row was given, so classifying pinned sibling rows
  as distributions collapses them into the first. It requires an equality the
  grammar actually READ; an unresolved clause is evidence about nothing,
  however it is written. This is to be removed once block shape is determined
  independently, before method selection.

  Consequently three expectations moved, in `test-method_from_intent.R` and
  `test-derivation_note_annotation.R`: a threshold on a numeric variable now
  reports that variable's own summary rather than counting subjects. Both files
  carry the reasoning. What those tests protect is unchanged -- five spellings
  of one restriction still agree, and a row's label still cannot move the
  verdict.

* **A variable's type is answered for the dataset that was asked about.** The
  spec lookup fell back to scanning every dataset for a variable of the same
  name and taking the first hit. In a table whose rows come from different ADaM
  domains that answers a question about one dataset's variable with another
  dataset's metadata -- and the answer decides the row's method. Dataset
  identity is part of the evidence; an undescribed variable now yields no
  verdict rather than a borrowed one.

* **A row keeps its own enrichment: a display label may assist a match, never
  outrank identity.** Two rows of one table may legitimately carry the same
  visible text while describing different data -- a level label belongs to more
  than one variable. Both places that paired a shell row with an enrichment
  looked the pairing up by label alone, so whichever candidate came first won
  and the second row silently inherited the first's variable and filter. The
  result was well formed and wrong, and where the two variables happened to
  agree in a data cut it did not even look wrong.

  Pairing is now decided by qualified `DATASET.VARIABLE` identity: a candidate
  compatible with the row's own declared source wins; a candidate that
  contradicts it is refused *even when its label is unique*; several compatible
  candidates, or several carrying no evidence at all, resolve to nothing rather
  than to the first. An unresolved pairing is not a downgrade -- dataset,
  variable and subset are rebuilt from the row's own annotation, which is
  authoritative for them.

  The nested-block classifier read its rows the same way and is now on the same
  resolver. It decides parent/child by comparing the variables of consecutive
  token rows, so a label-keyed lookup gave two same-labelled rows one shared
  variable, collapsed the run to a single distinct variable, and could take a
  genuine parent/child hierarchy for a flat one-variable block.

* **An unresolved non-default `variableRole` now blocks the event instead of
  becoming `ANALYSIS`.** Of everything an enrichment carries, `variableRole` is
  the one field no annotation can restate, so discarding an unattributable
  non-default role did not leave a gap -- it asserted the ARS default, a
  semantics the row never claimed, and the event then ran and produced numbers
  under it. `validate_ars_model()` gains `UNRESOLVED_VARIABLE_ROLE`, a `FAIL`
  that closes the validation gate. This is deliberately not `manual_pending`,
  which means "the semantics are known and a human must supply the value";
  here the semantics are precisely what could not be determined.

* **An analysis may only compute from data its own annotation named.**
  `validate_ars_model()` gains a structural check: every qualified
  `DATASET.VARIABLE` the built analysis resolves -- its analysis variable, and
  every variable of its data subset, recursively through compound expressions
  -- must appear among the references that analysis's **own annotation**
  declares. A source that appears in the analysis and nowhere in its annotation
  came from somewhere else, and that is a `FAIL`.

  This detects a failure with no visible symptom. A display label is not an
  identity: the same stub text is often a legitimate level of more than one
  variable, and when two rows share a label, the second has been observed to
  inherit the first's variable and filter. The number that comes out is
  well-formed, plausible, and computed from the wrong column. Where the two
  columns happen to agree in a data cut, **the output is numerically correct
  and the analysis is still wrong**, so no amount of checking the numbers
  would find it.

  The rule is **containment, not equality**, and one-way. An annotation of the
  form `<dataset>.<summarised> WHERE <dataset>.<restricted>='...'` analyses one
  variable while restricting on another, which is ordinary and correct; an
  annotation may also name more than the analysis uses. Only the reverse --
  using what was never named -- is a finding. Grouping variables are excluded:
  they come from the column header, not the row.

  It is a **one-way provenance invariant, not a proof of semantic equivalence**
  between an annotation and the ARS built from it. It detects a source that
  construction introduced and the row never declared; it does not by itself
  detect every role swap among several references an annotation legitimately
  names. If a row declares two variables and a later defect exchanges which one
  is summarised and which one restricts, containment still holds, because both
  were declared. That case needs a check of its own.

  Reference extraction is deliberately **independent of the where-clause
  parser**. It asks only which identifiers appear in the text, never what the
  condition means, so an annotation the condition grammar cannot yet read still
  declares its sources correctly. Both known parser weaknesses come from
  operator-like text inside a quoted literal -- a literal containing the clause
  joiner, or one containing a comparison character -- and neither can affect an
  extractor that never looks for operators. Reusing the parser would make the
  check blind in exactly the cases it exists to police.

  The check is defined on the *relationship* between two sets of qualified
  references, not on any study's vocabulary: the package contains no dataset or
  variable name from any particular study. The test suite proves this with
  cases built from invented identifiers, including a renaming that must leave
  every verdict unchanged.

  Coverage is measured in three states that are deliberately kept apart, since
  merging any two would let an unexamined event read as a clean one:
  **checkable** (the annotation declares qualified references, so containment
  is evaluated), **not checkable** (an annotation is present but yields no
  qualified reference -- the row made a claim this check could not settle), and
  **no source claim** (no annotation at all). These counts are asserted by the
  test suite, so a run can be shown to have been non-vacuous; they are not yet
  part of `validate_ars_model()`'s return value, which would change its result
  shape.

  The guarantee is therefore conditional, and is worth stating as such: *where
  an analysis carries source annotation from which qualified references can be
  established, arsbridge verifies that the resolved analysis introduces no
  undeclared semantic source.* Because `annotation` is an arsbridge extension,
  an ARS produced by another tool legitimately carries none; every other ARS
  check still runs against it, but this particular provenance invariant is
  unavailable. Such a file has **not passed** the semantic-source check -- it
  has not been assessed by it.

* **The generated program's calculations are now pinned against the reference
  engine.** arsbridge computes an output's ARD by emitting `{cards}` blocks and
  evaluating them in memory; it never reads back the `.R` file `ars_to_code()`
  writes. Those two assemblies of one generator had nothing holding them
  together. A regression test now executes the written file and compares it
  against `ars_to_ard()` on the complete row identity, so a change to either
  half that moves a number fails the suite.

  What is locked is a **subset relation, not equality** -- measuring it is what
  showed why. On the bundled CDSC-ALZ-201 event, across all five table outputs:
  every row the program produces matches a reference row, every shared row is
  identical on `stat`, `stat_label`, `fmt_fun`, `warning` and `error`, and the
  program produces **zero** rows the reference does not have. But the reference
  produces 18 rows the program does not, on T_14_1_1: when a subset filter
  empties an arm, `.complete_zero_groups()` states that arm as an explicit
  zero. Those are real computed results, not metadata. They are characterised
  exactly -- 18 rows, all `computed`, only `n`/`N`/`p`, with `n = 0`, `p = 0`
  and `N > 0` -- and pinned by count as well as by rule, so a change to the
  completion layer has to be understood rather than absorbed.

  The invariant holds **under the canonical subject key**.
  `ars_to_ard(subject_key = )` remains an independently configurable
  reference-engine option and is outside it: given a different key it neither
  errors nor no-ops, it computes different things and still labels most of the
  result `computed`.

* **Generating analysis programs is now its own step: `ars_to_code()`.**
  Authoring a reporting event and generating R programs from it are two
  deliberate actions, so `spec_to_ars()` no longer writes programs by default
  (`emit_code = FALSE`). `emit_code = TRUE` keeps the older one-call
  behaviour.

  ```r
  res <- spec_to_ars(...)                                  # the ARS, nothing else
  ars_to_code(res$ars_path, code_dir = "code")             # every output
  ars_to_code(res$ars_path, "T_14_1_2", code_dir = "code") # one of them
  ```

  This matters because generation used to happen *before* review: the programs
  on disk were always emitted from the reporting event as it stood before
  anyone opened the editor, and nothing regenerated them afterwards.

  `ars_to_code()` reads the saved ARS and nothing else -- never the original
  shell or ADaM specification -- so a program reflects the reviewed document
  rather than a re-reading of the inputs. It needs no `adam_dir`: generation
  defines the analysis, and the emitted program takes the data location as its
  own input. It generates, and never executes.

  **An existing program is not silently replaced.** Regenerating identical
  content is a no-op, so repeating the call is safe; a file that *differs*
  stops generation, and every other output is left alone (all scripts are
  built before any is written). arsbridge deliberately does not claim to know
  *why* it differs: emitted programs carry no provenance, so a file edited by
  hand and one generated from an earlier ARS are indistinguishable, and the
  message says exactly that. Pass `overwrite = TRUE` when the file on disk is
  expendable. Real customization detection needs a stored baseline, which does
  not exist yet.

  An unknown `output_ids` value is an error naming the available outputs, not
  a silent empty result.

  **There is no `subject_key` argument.** The subject identifier is not
  plumbing -- it is the deduplication key, the cross-dataset join key and the
  grouped-denominator merge key, so a caller-supplied value decides what gets
  counted. The reporting event already settles it: every method's
  `codeTemplate` names `USUBJID`, as does the generated analysis-set
  condition. Exposing an override would let a program contradict the method it
  implements. Should arsbridge support another key, the event will have to
  declare it.

* **The generated analysis scripts no longer invite an edit that has no
  effect.** Each emitted `.R` opened with `## <id> -- generated {cards} analysis
  script (edit freely).` That was not true: arsbridge computes an output's ARD
  from the reporting event, by emitting and evaluating its own per-analysis
  blocks, and never reads these files back. Editing one changed nothing, and
  nothing said so.

  The header now states what the file is — generated from the ARS, not the
  artefact that computes the ARD, and self-contained so it can be run to
  reproduce the numbers.

  An output whose script is a stub says so in its own terms, because the two
  cases are not the same: a listing is genuinely produced directly from the
  ADaM data, whereas a figure's content could be expressed as a structured
  summary and simply has no emitter yet. One shared line would have quietly
  asserted the second question was settled.

  This is documentation of current behaviour, not a change to it. Whether a
  hand-edited program should drive execution is a separate architectural
  question, and this change deliberately stops the file from answering it
  wrongly in the meantime.

* **A reviewed supplement ships with the training bundle.**
  `arsbridge_example("supplement.json")` is what a chat assistant could return
  for the bundled Word shell inside a closed environment, corrected by hand.
  It makes the offline path runnable from the bundle alone -- deterministic
  parse, reviewed supplement, enriched ARS, with no key, no network and no
  `ellmer`:

  ```r
  spec_to_ars_example(supplement = arsbridge_example("supplement.json"))
  ```

  Shipping it changes nothing by default. It is an example artifact:
  `spec_to_ars()` does not discover or apply a supplement sitting next to the
  shell, and the bundled study converts exactly as before unless the file is
  named. Both halves are asserted -- the deterministic run is checked for the
  absence of each enrichment before the supplemented run is checked for its
  presence, so neither assertion can pass vacuously.

  What it repairs on that shell, all of it real rather than illustrative: the
  disposition table's population, which the shell states only as prose, becomes
  a typed condition instead of an analysis set with nothing to filter on; the
  treatment axis becomes the three columns the shell actually prints, each with
  its own condition, rather than a data-driven axis that would take its levels
  from the data (including a screen-failure arm the tables do not show); and
  the row heading Table 14.3.2, which the shell leaves unannotated, becomes a
  subject-count analysis with its own data subset. Judgements a reviewer made
  but arsbridge does not compute -- record filters, sort keys, and the reasons
  for leaving certain rows alone -- travel in `provenance$reviewItems` and on
  the output's `_meta`, so nothing the reviewer decided is silently lost.

  The core-minimal CI job now asserts those three enrichments against the
  bundled file rather than a marker set by hand in the job itself.

* **A supplement run no longer reports itself as a degraded LLM response.**
  Supplement answers reach the enricher through the same path a live model
  answer does, but the v4 format has no `row_enrichments` channel -- per-row
  detail arrives as bindings instead. Judging it against the LLM response
  schema therefore emitted one "LLM response missing 'row_enrichments'"
  warning per TLF on every offline run, which was neither true nor actionable.
  Those two checks now apply to live answers only; what a supplement did
  supply is reported by the supplement stage, and `ars_validate_supplement()`
  checks its required fields before the run starts.

* **`ellmer` is optional, and the offline supplement is the documented
  completion path.** ellmer moves from Imports to Suggests, taking `httr2`,
  `curl`, `coro`, `later`, `promises`, `S7`, `otel` and `fastmap` with it: 12
  hard dependencies and a 50-package closure, down from 18 and 106 when this
  dependency work began.

  The reading hierarchy is unchanged in behaviour and now stated plainly in the
  documentation. Deterministic parsing always runs and is a supported mode on
  its own. On top of it at most one gap-filler applies: an offline
  **supplement** -- a reviewed JSON file, typically produced by a chat
  assistant inside a closed environment, applied with no key and no network --
  or, only when no supplement is supplied, an optional **live LLM** pass. A
  supplement wins outright: supplying one makes no live call whatever `use_llm`
  says, so it can never require ellmer. The DESCRIPTION, the package
  documentation and the reading-engine vignette no longer describe the live LLM
  as the primary reader; it is a convenience for sites that permit it.

  The guard sits on the single line where `spec_to_ars()` resolves the
  extraction mode to `"llm"` -- opted in, with a provider and a usable key --
  because that is the only mode that reaches ellmer at all. `deterministic`
  enriches with `offline = TRUE` and `supplement` enriches from courier
  answers, and both return before a chat object is built. Placing it there
  rather than at the ellmer call sites keeps every supported path reachable
  without the package: `use_llm = FALSE` never asks for it even with a key
  configured; `use_llm = TRUE` with no usable key falls back deterministically
  and silently, exactly as before; and a preferred provider whose key is
  missing keeps its existing warning and its fallback, because that provider is
  unusable whether or not ellmer is installed. Only the last case -- opted in,
  usable configuration, ellmer absent -- fails, and it fails naming ellmer.
  That one has to fail: silently handing back regex-extracted output to
  somebody who configured a key and asked for the LLM would be a quieter
  answer than they asked for, not a safer one.

  `use_llm` semantics are unchanged. This release does not redefine what
  opting in means; it only decides when the package is required.


* **`ars_to_tfrmt_list()` reports a missing tfrmt once, instead of skipping
  every output.** It wrapped each output in the per-output handler that is
  right for a table which could not be built and wrong for an absent package:
  that is the same answer for every output, so a core install got one warning
  per output and a list of `NULL`s -- "install tfrmt" arriving as a warning
  storm over an empty result. It now raises the same condition
  `ars_to_tfrmt()` does, once, through the handler added with the rendering
  tier. Genuine per-output failures are untouched: one bad output still skips
  with its reason and leaves a `NULL` in its place while the others render.
  This closes the rendering tier -- every affected export is now covered by an
  exact requirement assertion in the core-minimal job.

* **Rendering is an optional capability, and each capability asks only for what
  it needs.** `tfrmt`, `gt`, `flextable` and `ggplot2` move from Imports to
  Suggests. A core install -- read a shell and an ADaM spec, produce and
  validate ARS, execute it to an ARD, write the filled workbook back -- now
  carries 13 hard dependencies instead of 17, and 59 packages instead of 103.
  The 44 that leave include the whole knitr/rmarkdown/sass/htmltools chain, and
  with them go two external system libraries: `V8` (libv8, via gt) and
  `gdtools` (cairo, freetype2, fontconfig, via flextable).

  The guards are deliberately NOT one "rendering requires four packages" gate,
  because that would make someone who wants a figure install a Word table
  writer. Each sits at the boundary where its capability is actually requested:
  `ars_to_tfrmt()` asks for tfrmt alone, since it builds a specification and
  renders nothing; `ars_render_listing()` asks for gt alone, since a listing is
  read straight from the data; `ars_render_figure()` asks for ggplot2 alone;
  `ars_render_tlf()` asks for tfrmt and gt together, named in one message so
  nobody is sent back twice; and flextable is asked for by the two functions
  that convert and write a Word/RTF table, so a reporting event of figures only
  is never told to install it. The composite renderers inherit their
  requirement from the outputs they actually meet.

  One related fix: the composite renderers record a failed output and carry on,
  which is right for a table that could not be built and wrong for an absent
  package -- that is the same answer for every output, so it landed in the
  manifest once per output and the run then aborted with "no output could be
  rendered", naming nothing. A missing-package condition is now re-raised
  instead of recorded, so "install gt" cannot turn into "inspect the
  diagnostics".

  The `core-minimal` CI job proves all of this where the four packages are
  genuinely absent, which is the only place it can be proven rather than
  simulated: it asserts that each single-capability export names its own
  package and none of the other three, and that no composite demands flextable
  before it has a table to convert.

* **`callr` is optional, and there is now a CI job that proves what "optional"
  means.** The background build the workflow app starts is a responsiveness
  optimisation, never a requirement -- everything the worker does,
  `ars_workflow_run()` also does in this process -- and the fallback for a
  missing callr has been written and guarded since `0.1.0.9061`. It was
  unreachable only because callr was an Import. It is now a Suggests, so a core
  install has no callr, no `processx` and no `ps`, and the app says why the
  build will pause rather than failing or falling silent. No exported function
  changed, and no public API depends on it.

  The larger half of this change is the `core-minimal` CI job, which installs
  arsbridge's HARD dependencies and nothing else, then proves the core contract
  in that library: convert an annotated shell and an ADaM spec to ARS, validate
  it, execute it to an ARD with nothing blocked, and write the filled workbook
  back. It asserts that nothing is installed which is not a hard dependency --
  stated that way rather than as "no Suggests installed", because several
  Suggests (`withr`, `knitr`, `rmarkdown`, `bslib`, `askpass`) are also
  legitimate hard dependencies of Imports and would otherwise false-alarm --
  and, by name, that every optional package is absent. Two details are what
  make the job mean anything: dependency resolution is `"hard"` rather than the
  usual `TRUE`, which would install Suggests and pass while testing nothing;
  and the smoke script uses base assertions rather than testthat, because
  testthat hard-depends on callr and installing it would reinstate the very
  package whose absence is being proven. The job is also what makes the
  remaining dependency work verifiable: as further tiers move to Suggests they
  leave the hard closure and the script begins requiring their absence with no
  edit to it.

* **A grouping's dataset is read from one place, and the denominator is the
  population.** A grouping can name its dataset twice -- the flat
  `groupingDataset` that arsbridge itself writes, and the nested
  `groupingVariable$dataset` a spec-correct ARS from elsewhere carries. The
  execution side read only the nested form, so everything the converter
  produced resolved to "no dataset" and the denominator repair it drives never
  fired. An AE table whose columns were annotated `ADAE.TRTA` therefore
  reported every percentage out of the whole study on the emitted path while
  the executor got it right -- proven from an annotated shell through
  `spec_to_ars()` to the ARD, N = 6 where it should have been 3. Both forms are
  now read by one resolver, and a grouping that names two DIFFERENT datasets is
  refused rather than resolved by precedence, because choosing either could
  move a denominator: `validate_ars_model()` reports it as a blocking FAIL
  naming both datasets, and execution refuses the event through the structural
  gate rather than picking one. The rule the denominator follows is population-first: if
  the population frame already carries the grouping variable it is
  authoritative, whatever the metadata names, so a treatment variable copied
  onto an event domain can no longer decide the denominator and subjects with
  no event stay in it. Emitted code makes the same decision at runtime on the
  same frame, so the two engines cannot part company. Three situations now fail
  closed instead of reporting a whole-study N with a warning attached: a
  subject carrying two different grouping values in the foreign dataset, a
  population subject the foreign dataset does not know, and a same-named
  variable whose value disagrees between the population frame and the analysis
  domain -- the last of which could previously report n = 3 against N = 2. Once
  the population cannot supply the variable, the foreign dataset is the only
  source of group membership, so an unresolvable subject makes every per-group
  N partly invented. A subject simply missing from the domain is NOT an error
  when the population has the variable: the population answers for them.

* **New ARD status `blocked`: an analysis whose filter could not run reports
  nothing rather than a plausible wrong number.** This extends the ARD
  contract. `result_status` now takes four values, and they are not
  interchangeable: `computed` is a trustworthy engine result, `manual_pending`
  is valid work a programmer must derive by hand, `manual_filled` is a
  validated manual result, and `blocked` means computation could not safely
  proceed because required data or filter semantics could not be satisfied.
  Three things block: a referenced dataset that is not in the ADaM directory, a
  referenced dataset with no subject key to carry its filter back on, and a
  where-clause whose row semantics are not determined. Each previously carried
  on against a population nobody had asked for -- a filter that did not run is
  a wrong denominator, and a wrong denominator is invisible in a rendered
  table. The middle case was worse than silent: it warned that the filter had
  not been applied while the emitted code then failed on the missing column and
  dropped the analysis, so the diagnostic promised a run that continued and it
  did not. A blocked analysis now emits no computed rows on either execution
  path, the emitter writes no code for it, and one FAIL names the analysis and
  the cause. The reason is recoverable without a new identifier -- the blocked
  row carries `analysis_id` and the FAIL carries the same id in `location`, so
  `ars_blockers()` maps a reserved cell to why it is reserved -- with a
  `block_reason` column alongside for direct inspection. Every consumer is
  explicit rather than inheriting an `else`: `ars_manual_worklist()` excludes
  blocked rows, because sending a programmer to derive a number that cannot be
  derived until the spec is repaired would be worse than saying nothing;
  `ars_validate_manual_fills()` ignores them; `derived_dt` stays NA; the shell
  fill reports the cell as blocked rather than as an ordinary absent value; the
  Word placeholder names the blocked cells and their reason instead of
  rendering empty columns; and a rendered table carries a note pointing at
  `ars_blockers()`. That note is table-level rather than a per-cell marker, and
  deliberately so: a blocked analysis reserves one row with no statistic name,
  because the filter never ran and nothing decided which statistics it would
  have produced. A missing dataset is also now remembered as missing, so it is
  reported once per run instead of once per lookup.

* **A condition spanning several datasets is filtered correctly, and
  same-record semantics are preserved.** A where-clause is now turned into a
  restriction PLAN that both halves consume -- the executor interprets it, the
  emitter renders it as dplyr -- so executed filtering and emitted filtering
  cannot drift apart. The rule the plan encodes: a maximal subexpression
  naming ONE dataset is evaluated ROW-WISE on that dataset, and only then, if
  that dataset is foreign, are qualifying rows projected to subject ids and
  combined with the rest. That is what keeps `ADCM.CMDECOD='ASPIRIN' AND
  ADCM.CONTRTFL='Y'` from admitting a subject whose two predicates are
  satisfied by two DIFFERENT conmed records. Three things were wrong before
  and are now right: a valid compound clause naming two foreign datasets
  emptied the population silently in the executor and emitted code referencing
  a column its frame did not have; `OR` across datasets behaved as `AND`,
  because the old path intersected subject sets whatever the operator said;
  and same-dataset predicates separated by an intervening foreign sibling --
  `(ADCM.A AND ADSL.S) AND ADCM.B` -- are now regrouped, by associativity and
  commutativity, so they are judged against one record again. Regrouping never
  crosses an `OR`: that would require distribution, which changes the answer.
  An expression whose row coherence cannot be recovered that way, such as
  `(ADCM.A OR ADSL.S) AND ADCM.B`, is reported as unsupported and refused by
  BOTH halves rather than guessed at -- the executor computes nothing and the
  emitter writes no script. Every valid single-dataset spec is numerically
  unchanged, percentages included, and the three ARS goldens are unchanged.

* **A where-clause names its datasets in one place, and says so when a spec
  names two.** Reading ADaM by name, and applying a where-clause across
  datasets, lived inside `ars_to_ard()` as four closures -- out of reach of
  every other function and of any test. The executor carried its own copy of
  the dataset-reference rule, and the two copies differed: `.where_datasets()`
  coerces each `dataset` through `.as_scalar_char()`, while the copy returned
  the raw field. A `dataset` written as more than one value therefore made the
  emitter filter on the first and the executor intersect them all -- one spec,
  two filters, and nothing to say which was running. The copy is gone;
  `.where_datasets()` is the single source of truth. Because the ARS schema
  has `dataset` as a scalar, the scalar reading wins, and the one deliberate
  behaviour change is that narrowing is no longer silent: a `dataset` field
  naming more than one dataset now raises a WARN saying which datasets were
  named and which one is used. Every valid spec produces numerically identical
  results -- percentages included -- and the three ARS goldens are unchanged.
  The promoted helpers can now be called directly, which is the practical
  payoff: the cross-dataset rule that decides every denominator has unit tests
  for the first time, rather than only being reachable by running a whole
  conversion.

* **The converted reporting event is pinned by golden files, so silent drift
  fails the build.** Focused tests pin behaviours -- a grouping resolves, a
  denominator counts the right subjects -- but nothing pinned the DOCUMENT, so
  a change in groupings, conditions, analysis sets, output references, methods,
  columns or result metadata that no single test happened to assert passed the
  suite and shipped. Measured, not assumed: changing the deliverable filename
  every output declares, from `T-14-1-1.rtf` to `T_14_1_1.rtf`, left all 5843
  tests green, because only `fileType` was ever asserted and never the name.
  Three goldens now cover the CDSC-ALZ-201 study through both readers and the
  APX acceptance shell, all on the deterministic tier -- an LLM is a moving
  oracle and cannot be pinned this way. Comparison is structural on a canonical
  form rather than byte-for-byte, because a change in how JSON is indented is
  not a converter change and a gate that fails on one gets muted. What
  canonicalisation removes is ordering noise in id-keyed collections and two
  volatile fields, and nothing else: arrays whose position carries meaning --
  `orderedGroupings`, whose first element IS the column axis,
  `referencedAnalysisIds`, a grouping's `groups`, the table of contents --
  stay ordered and are compared ordered. Before either volatile field is
  replaced with a sentinel its form is asserted, so a generator that stops
  carrying a version or a timestamp that arrives in local time fails rather
  than being normalised away; ids are asserted present and unique before any
  collection is sorted, because sorting assumes the id identifies the element
  and a duplicate id is a defect to report rather than an input to tidy; and a
  separate assertion keeps absolute and temporary paths out of the output,
  which is what makes a committed golden portable in the first place.
  Regeneration is a deliberate `data-raw/regenerate_goldens.R`, never an
  environment variable the test run honours.

* **Crash-recovery files no longer accumulate forever.** An autosave is
  cleared when the file is saved and when the reviewer chooses to start
  fresh, so what survived was what nobody came back for -- a session that
  died on a study never reopened -- and nothing ever pruned it. The cache
  grew by one file per study for the life of the installation. The editor now
  sweeps autosaves older than 30 days when it opens, configurable through
  `options(arsbridge.autosave_max_age_days = )`. Thirty days because a
  recovery file's worth decays fast: work resumed at all is resumed within
  days, and an offer to restore changes from months ago is one a reviewer
  cannot judge, because they no longer remember what those changes were. It
  also matters that this is bounded at all -- unlike the recent-projects
  list, which holds paths and nothing else, an autosave holds the whole model
  and edit log, so a stale one is study content sitting in a cache directory
  indefinitely. Only the editor's own `editor-*.rds` files are considered;
  the cache is shared, and a sweep by age alone would eventually take
  something it never wrote.

* **The where-clause evaluator is a package function, not a closure.**
  `.eval_where_clause()`, `.eval_condition()` and `.clean_var_name()` were
  defined inside `ars_to_ard()`, which put them out of reach of everything
  else in the package. `.complete_zero_groups()` is defined at namespace
  level and called from inside that closure, so its call to the evaluator
  resolved lexically to nothing: the branch that completes a zero-count
  column the shell declared by CONDITION -- rather than by the variable's own
  values -- would have died with `could not find function`. Nothing reaches
  that branch today, because both the emitted code and the legacy path derive
  the grouping as `factor(levels = every declared label)` and so no declared
  column is ever missing from the result; the defect was latent, and R CMD
  check had been reporting it as an undefined global. The three functions now
  live in `utils_where_clause.R` beside the predicate-string emitter they are
  the executable half of, unchanged in behaviour. Having them reachable also
  makes the deterministic-equivalence guarantee testable directly: emitter
  and executor are now asserted to agree on every annotation in
  `test-where_to_filter_expr`, where before one half was pinned to masks
  typed out by hand and the other trusted to match.

* **An entity's findings say which child they are about, and stop burying the
  editor.** The spec check reports once per clause, which is right -- each is
  a separate place to fix -- but the panel rendered only the sentence, never
  the field. So a grouping with three compound children showed
  `Variable ADSL.CGHGR1N is not in the ADaM spec.` four times and
  `ADSL.COHORTN` three times, seven identical-looking rows with nothing
  saying which child each belonged to, stacked above the editor and pushing
  the child-group cards off the bottom of the panel. Findings that share a
  message now collapse into one row that lists the parts it affects --
  `Affects: Variable, Low, Medium, High` -- naming children by the label
  their cards carry rather than by group id. Seven rows become two. When an
  entity genuinely has more than three distinct problems the block folds
  behind a one-line summary, so the fields the reviewer came for stay on
  screen; nothing is dropped, it is one click away.

* **A column axis is read from what varies, not from what is named first.**
  When every group column ANDs the same restriction onto its own value --
  `ADSL.COHORTN=1 AND ADSL.CGHGR1N=1`, `... AND ADSL.CGHGR1N=2`, and so on --
  the converter took the first variable each header named. That is `COHORTN`
  every time, so the grouping came out called "Grouping by COHORTN" with
  every one of its levels holding `COHORTN=1`, and the variable that actually
  distinguishes the columns never appeared. The columns still selected the
  right subjects, because each level kept its whole condition; it was the axis
  IDENTITY that was wrong, and nothing said so. The shared predicate is now
  recognised and the varying variable names the axis, reported as an INFO.
  Each level keeps its full condition exactly as before -- nothing is
  factored out of the groups, only the axis is named differently.

  Recognition is deliberately narrow: every level header must be an AND of
  exactly two plain conditions, one of which is identical across all of them,
  leaving exactly one variable that varies. OR-joined headers, deeper nesting
  and three-way splits are left to the previous behaviour untouched and
  silently, since they are not this pattern. When the pattern IS present but
  the axis is not decidable -- no shared predicate, or two variables varying
  at once -- it is a WARN that names the reason and changes nothing, because
  a guessed axis would look deliberate and a wrong one is invisible in the
  output.

* **The test suite no longer writes into the user's cache directory.** The
  editor autosaves to `tools::R_user_dir("arsbridge", "cache")` on purpose --
  the crash it protects against takes `tempdir()` with it -- but under test
  that is exactly wrong. Every `testServer()` run that reached
  `.write_autosave()` left an `.rds` behind under a fresh path key, and
  nothing ever pruned them; 255 had accumulated on one developer machine.
  `.autosave_dir()` now reads `getOption("arsbridge.autosave_dir")` before
  falling back to the real cache, exactly as `.workflow_recent_path()` already
  did for the recent-projects list, and `tests/testthat/setup.R` points it at
  a temporary directory for the whole run. Nothing changes for an interactive
  session: with the option unset, autosaves live where they always have.

* **Bulk grouping assignment shows what each line would become, not just how
  many.** The confirmation named the new groupings and counted the lines, but
  it never said which output the action was scoped to, and it never showed
  what those lines were grouped by already -- so the reviewer confirmed a
  count and discovered the diff afterwards, in the edit log. The dialog now
  names the target output and lists every affected line beside its current
  groupings and the ones it would get, both spelled as labels rather than
  bare ids. The preview and the edit that follows are computed by one
  function, so they cannot describe different things. If the reporting event
  moves while the dialog is open -- an undo, an autosave restore, a second
  window -- confirming re-previews the changed plan instead of applying the
  one that was displayed.

* **A column condition that names a variable the ADaM spec does not have is
  reported.** The spec overlay read only the flat condition columns, so it
  never looked inside a grouping's child groups, and it skipped a compound
  analysis set or data subset outright. A child group's condition is exactly
  where a wrong variable is easiest to write and hardest to notice: the column
  still renders, it is simply empty, and nothing upstream says why. Every
  where clause is now walked to any depth, so `ADSL.TRT01AN` in a column that
  should have said `ADSL.TRT01A` is a WARN naming the child it came from
  rather than a silent empty column. A variable repeated across clauses is one
  finding, not one per clause. This checks that the VARIABLE exists; it does
  not check the values compared against it, so a real variable with an
  impossible value still passes here.

* **A validation finding says which output it is about, and opens it.** The
  table identified a finding only by id, while the sidebar and the detail
  header call the same thing by its label -- so `F_14_3_1` and
  `Mean (+/- SE) Pulse Rate Over Time by Treatment` read as two unrelated
  problems, and the count in the header looked unconnected to the badge beside
  the output. The finding now carries the name the rest of the app uses, next
  to the id. Selecting a row also lands somewhere: it already set the
  selection, but the panel that shows it is on another tab, so the click
  appeared to do nothing. A finding about a shared entity now opens the
  Entities tab with its row selected, and one about an output or an analysis
  opens Details.

* **A compound column condition can be edited in the editor.** A group whose
  columns are defined by an `AND`/`OR` expression used to be shown read-only,
  with the raw-JSON hatch as the only way to change it -- the one shape a
  reviewer is least able to repair by hand. Its clauses are now editable one at
  a time, with the operator beside them and the whole expression previewed as a
  sentence (`ADSL.COHORTN is missing OR ADSL.COHORTN EQ 99`) rather than as the
  R predicate that will run, so what the shell meant and what the column does
  can be compared in the same words. Clauses can be added, cloned, reordered
  and deleted, and a simple condition grows into a compound by adding a clause
  to it -- keeping the condition it already had as the first one, which is the
  only route into a compound expression that does not go through raw JSON.
  A clause that is itself nested stays read-only and says so, but can still be
  reordered or removed whole, and saving the group walks past it untouched.

* **A group's compound condition can be edited from R.** `model_add_clause()`,
  `model_remove_clause()`, `model_move_clause()`, `model_set_clause_condition()`
  and `model_set_group_operator()` write the clauses of an `AND`/`OR`
  expression, which until now could only be changed through the raw-JSON hatch.
  A group holds either a simple condition or a compound expression and never
  both, so the accessors move it between the two shapes as clauses come and go:
  adding a clause to a group that had a plain condition keeps that condition as
  the first clause, and removing back down to one unwraps the compound rather
  than leaving a one-clause expression, which the ARS does not want persisted.
  A clause that is itself compound is preserved but refuses a flat edit --
  replacing it would drop its own clauses -- and when such a clause is the last
  one left its expression is hoisted rather than wrapped again.

* **A subject count over an occurrence frame no longer undercounts terms on the
  legacy path.** `ars_to_ard(legacy = TRUE)` deduplicated a Subject Count and a
  Subject Count (%) on the subject alone, which on a frame that carries several
  rows per subject -- an AE dataset, one term per row -- kept whichever row came
  first and reported that subject's every other term as zero. A subject with a
  headache and a nausea counted only towards the headache. The term now joins
  the deduplication, as it always has in the emitted `{cards}` block and in the
  neighbouring AE Frequency method, so the two engines agree on an occurrence
  frame and the count is the number of subjects reporting each term. The default
  path was never affected; the equivalence suite exercised the method only on
  ADSL, so nothing caught it.

* **Child groups can be added, cloned, reordered and deleted in the editor.**
  With the previous entry, a grouping's column levels can now be authored
  without the raw-JSON hatch. Deleting a group that a declared result path
  names is refused, and says which output holds the path, because removing it
  would leave that path pointing at a group that no longer exists. Every
  action is one undo step and one line in the save summary.

* **The editor can change a grouping's child groups.** Each child now carries
  its label and its condition as fields, saved one child at a time, so the
  four condition fields commit together rather than a keystroke at a time. A
  compound child stays read-only and says which hatch to use, since replacing
  it with a simple condition would drop its other clauses. A grouping's
  data-driven mode is a checkbox, and if turning it off leaves a grouping with
  no groups, the panel says so where the reviewer is standing and offers to
  mark it data-driven again -- an explicit, logged action, never an automatic
  one, because the two modes mean different things about what the columns are.

* **A grouping's child groups can be edited from R.** `model_add_group()`,
  `model_remove_group()`, `model_move_group()`, `model_set_group_field()` and
  `model_set_group_condition()` write the grouping node directly, the way a
  method's operations already worked, so the columns a reader sees
  (`n_groups`, `group_labels`) are re-derived rather than left stale. Two of
  them do what the flat column path cannot: a condition can be *created* on a
  child that had none, and an empty value list is kept as one, which is how a
  where-clause says "is missing". A child named by a declared result path
  refuses to be removed, and a relabelled child keeps its id, because result
  paths reference those ids.

* **A large codelist still decodes.** Above the 15-term expansion cap the
  decode was dropped entirely, not just the expansion: the ARD then carried
  raw codes, no shell row labelled with a decoded value matched one, and the
  whole block filled as placeholders behind a single WARN. The cap now governs
  only what it was written for -- a 195-term codelist must not become 195
  rows -- while the decode still applies and the engine drops the levels the
  data never took. So a large codelist shows the terms actually observed,
  under their proper labels. Column groups are unaffected: they were always
  capped separately, and still are. The diagnostic is now INFO and says which
  behaviour was chosen.

* **A Total column on an ordinary treatment axis is no longer invisible.**
  When a table declares its column axis on the stub header
  (`[columns -> ADSL.TRT01A]`) and leaves the column headers as plain text,
  no column header carries an annotation -- and the resolver returned before
  it ever looked for an overall column. A displayed `Total` was therefore
  never recorded, the reporting event carried no Total, every Total cell kept
  its placeholder, and nothing reported that a displayed column had produced
  nothing. The bare label is now read on this path too, scoping the pass by
  the analysis set, which is all an unannotated Total can mean. The bundled
  example gains a Total column on its nested adverse-event table, where the
  parent and child rows both fill.

* **Two populations that share a name no longer share a definition.** An
  `AnalysisSet` id is minted from the population text while its filter comes
  from the annotation, so two tables both headed "Safety Population" with
  different annotations collided: the first definition won and every later
  analysis silently ran the wrong population, which a filled table cannot show
  you. Analysis sets and data subsets are now deduplicated by what they filter,
  the way groupings already were -- one definition written twice is one entity,
  and one name over two definitions keeps both, the second under a variant id
  and a diagnostic. A population named but left unannotated still joins its
  annotated namesake rather than splitting from it, and if the unannotated one
  came first, the annotated definition replaces it.

* **A Total column scoped on a subject-level variable now computes on the
  legacy engine.** A shell whose Total header is annotated with an ADSL
  variable (`[ADSL.COHORTN IN (1,2)]`) names a column an occurrence dataset
  cannot answer from its own rows, and `legacy = TRUE` was evaluating the
  clause row by row against the AE frame -- where a variable the frame lacks
  reads as FALSE for every row. The Total pass therefore selected nothing, and
  because an empty frame cannot be tabulated, the whole analysis failed rather
  than just its Total column. Both frames are now masked against their own
  dataset, the AEs on the analysis dataset and the population on ADSL, which is
  what the emitted code always did. Cross-dataset dataset names are also
  compared case-insensitively, as the emitter compares them.

* **The editor shows a grouping's child groups.** The entity library listed
  how many groups a grouping had and what they were called, but never the
  groups themselves. Selecting a grouping now lists each child with its order,
  level, id, label, and the filter its condition stands for -- compound
  expressions included, read as the whole expression. A grouping with no
  children says which kind of empty it is: levels discovered from the data, or
  a fixed grouping that defines no result columns. Editing children is still
  the raw-JSON hatch.

* **Blocking ARS findings now stop runnable deliverables.** The ARS JSON and
  validation report are retained, while code, ARD, shell fill, and fill debrief
  are skipped until the model is repaired; editor saves use the same gate. Three
  structural checks now block empty fixed groupings, flat-axis display/grouping
  mismatches, and method/placeholder slot mismatches. The fill census and
  workflow per-cell view include row, method, placeholder, grouping, variable,
  parent, and ARD lookup provenance, so a child Total miss exposes the retained
  parent key directly.

* **Entities created during an editor session remain editable.** The entity
  library now registers field observers as methods, analysis sets, data subsets,
  and groupings enter the model, rather than taking a one-time snapshot at app
  startup. Registrations are retired when entities disappear and recreated when
  an id returns, so added, cloned, detached, catalogue-inserted, and restored
  entities keep their edits. Blank labels on new groupings now receive the
  generated `Grouping by <VARIABLE>` label.

* **Subject-count rows now honor two-statistic placeholders.** An annotation
  such as `Count of unique USUBJID where SAFFL='Y'` previously selected the
  count-only method before consulting the shell's placeholder shape, leaving
  the percentage token in `xx (xx.x)` unfilled. Every subject-count annotation
  form now selects Count and Percentage when the row declares two slots, while
  one-slot rows remain count-only. Grouped subject-key rows also carry real
  treatment keys and use each treatment arm as their denominator, so unequal
  arms fill completely with correct percentages instead of whole-study shares.
  Total passes now emit the same count, denominator, and percentage rows, and
  the exported siera template performs the same scalar distinct-subject count.

* **Nested child rows now fill in the Total column.** The scoped Total pass
  removed every grouping before calculation, then restored only the Total
  column label. A child result therefore lost its parent key, and identical
  terms under different parents were also counted together. The Total pass now
  removes only the column-axis grouping, retains every row grouping during the
  calculation, and shifts those row keys behind the stamped Total identity.
  The retained keys do not partition the Total denominator. Nested SOC/PT Total
  cells fill completely, and same-named terms and subject counts remain
  separately calculated within each parent.

* **A second table that groups the same variable differently no longer
  inherits the first table's columns.** A grouping's id was minted from its
  variable name alone, and groupings were deduplicated on that id without ever
  comparing what they contained. So a workbook whose demographics table split
  `COHORTN` into cohorts, and whose subgroup table split the *same first-named*
  variable into Low/Medium/High (each column a compound condition,
  `COHORTN=1 AND CGHGR1N=k`), emitted one grouping carrying the cohort columns
  -- and the subgroup table's analyses were quietly pointed at it. The ARD then
  came back keyed by cohort, no shell column matched, and every subgroup cell
  kept its `xx (xx.x)` placeholder while Total filled from its own scoped pass.
  Groupings are now deduplicated by DEFINITION: same definition, one shared
  grouping; same variable defined differently, both survive and the later one
  takes a variant id, with an INFO diagnostic naming it. The per-level group
  ids move with it, because a group id is only variable + label and one
  output's result path would otherwise resolve another output's condition. A
  study that defines each variable once is unaffected, byte for byte.

* **A listing's dates are now covered by a test that could fail.** Dates only
  reach the fill writer AS dates when the data came from `.xpt` or
  `.sas7bdat`; read a `.csv` and every column is character. Every committed
  ADaM fixture was `.csv`, so the writer's date formatting ran on every real
  study and never once in CI. There is now a small typed fixture
  (`adam_apx_drm_301_xpt/ADSL.xpt`, four subjects) behind it, pinning both the
  ISO spelling and the rule that a missing date is a blank cell rather than
  the text "NA". No behaviour changed. (#2)

* **Filling a real-sized listing is no longer measured in minutes.**
  `ars_fill_shell()` expanded a listing's template row by appending one cell
  at a time to the worksheet's cell table and reparsing that column's
  template XML for each of them, so the cost grew with the *square* of the
  row count -- a 7,500-row listing took around ten minutes, and it was the
  bulk of a fifteen-minute workbook. The block is now built a column at a
  time: each column's XML is parsed once and the cells are put into the
  table in two vector operations. Same workbook, byte for byte; a 7,500-row
  listing fills in about a second and a half, and the cost is now linear in
  the number of rows. (#1)

* **Step 5 of the workflow app answers "why is this cell empty" on its
  own.** Above the diagnostics: one line per sheet saying how much of it
  filled, and a callout naming every display column that filled *nothing*
  while its siblings filled -- with the reason all its cells share (the
  lost-column shape that previously had to be reconstructed cell by cell).
  Below the per-cell table: the reasons grouped, each with how many cells
  it explains and the author-facing hint; the per-cell table itself gains
  the hint column. A payload archived by an older version renders
  "available after the next build" instead of erroring.

* **Every build writes a fill debrief workbook.** `outputs/fill_debrief.xlsx`
  -- the durable, human-readable record of what the fill did, for the
  machine nothing may leave. Sheets: the full cell census (rows tinted
  filled/pending/skipped), the per-column rollup (a column that lost every
  cell is tinted FAIL with its modal reason beside it), the reason
  histogram with the author-facing hint for each, the run's complete
  diagnostics, and the shared legend. Written by the new exported
  `write_fill_debrief()` after the fill stage; a debrief failure can never
  take a finished build down -- it degrades to a WARN in the payload. The
  workflow payload gains `fill_census` and the `fill_debrief` artifact
  path, and the app's artifact list links it; the run-log link now says it
  may contain console output and should be reviewed before sharing.

* **The fill census keeps every cell, and every reason carries a hint.**
  Diagnosing a half-empty workbook on a machine nothing may leave needs the
  whole answer on-screen, and the old census could not give it: filled
  cells were dropped (so no per-column fill rate was computable), the cell
  position and owning analysis were dropped with them, and the fill stage's
  own WARNs lived only in the session collector a CLI caller loses on exit.

  `ars_fill_shell()` now returns `census` -- one row per cell record,
  filled cells included, with row/column, the display column's label, the
  owning analysis, status and reason -- and `findings`, the run's own
  diagnostics. The new `ars_fill_summary()` rolls a census up into the
  three tables a reader actually asks for: per-sheet counts, per-column
  fill rates with the lost column's modal reason, and a reason histogram.
  Every reason the fill can write now carries an author-facing hint
  (`.fill_reason_hint()`), and a coverage test holds the contract: a new
  reason string cannot ship without one. The old unfilled-only
  `diagnostics` field is gone; `ars_workflow_run()`'s `unfilled_cells`
  payload keeps its shape.

* **Categorical template blocks expand in the filled workbook: one row per
  level.** The other half of the template work -- the visible one. A
  recorded block's mock rows are replaced at fill time: each level of the
  variable takes a row (decoded labels, codelist order, zero-count levels
  included), values land formatted to the template's own placeholder
  decimals, and rows below move down when the levels outgrow the authored
  rows -- the same `.shift_rows_down()` machinery the nested SOC/PT and
  listing expanders already trust. Authored rows the levels do not reach
  are cleared, and reported, rather than left showing "<Reason #n>".

  Several blocks per sheet compose (nested pair included): expansions run
  bottom-up, so a shift never moves a block that is still waiting. A sheet
  carrying formulas or other unshiftable features declines the expansion
  with a FAIL naming the block, and fills everything else. The default
  level order is the spec/codelist order the Word renderer already prints
  -- the workbook and the document cannot disagree -- with the authored
  "sort:" overrides available as on nested blocks.

* **A mock block with no annotated header above it is a self-template, not
  debris.** Shells in the field author the same "levels unknown until the
  data arrives" intent the other way round: a plain-text header over a
  single `<Reason #1>` mock carrying the bare variable, or numbered
  subcategory mocks under a row annotated on a different variable. Those
  runs used to fall apart -- one analysis per mock, deduped by signature,
  nothing marked for expansion, and the mock text rendered as if it were a
  real row.

  Now such a run is a *self-template*: its first annotated row carries the
  block's single count-and-percentage analysis, the repeats collapse into
  it, the layout entry is flagged `self_template` with the run's sheet rows
  as `template_rows`, and the Word renderer stops printing the mock label
  as a header line -- the authored plain header above remains the block's
  visible title. Template-row cells in mapped columns are bound to the
  owning analysis (slots and all), so the fill plan (`fill$categorical`,
  one block per template entry, several per sheet allowed) knows exactly
  which rows to expand and from what. The expansion itself lands next; a
  block whose recorded rows are not contiguous is reported and skipped
  rather than expanded over unrelated rows.

* **A categorical mock block is kept as an expansion template, not dropped.**
  Shells author "levels unknown until the data arrives" as a header annotated
  with the bare variable ("Primary reason for discontinuation, n (%)
  [ADSL.DCSREASN]") over mock rows ("<Reason #1>", "<Reason #2>",
  "<Reason #n>"). The mocks still collapse into the header's single
  count-and-percentage analysis, but their sheet rows now ride on the
  parent's layout entry as `template_rows` -- the recorded shape a later
  fill step needs to expand the block into one row per level. Until that
  expansion exists, the block's placeholder cells say so: the cell map
  reports them as *awaiting row expansion* instead of the misleading "no
  analysis covers this row".

  Two behaviours around the mocks are corrected with it:

  * A mock that restates the variable with an illustrative level code
    ("[ADSL.DCSREASN=1]", or the generic "[ADSL.DCSREASN=n]") is recognised
    as an illustration. The value is no longer backfilled as a subset
    filter, and the unparseable "=n" no longer surfaces as a
    dropped-condition WARN -- the collapse is reported as INFO, as it
    always was for the bare dialect.
  * A Word shell, which has no cell addresses, records no template rows --
    the entry gains nothing that merely looks like one.

* **A promised Total must arrive, and a displayed column must receive
  something.** The remaining two checks from the guidance document, both aimed
  at the same field failure from the other end: three cohort columns full of
  numbers, a Total column of placeholders, and a build that reported success.

  * **Rule 2, at execution.** `includeTotal` says an overall column *will* be
    computed. `ars_to_ard()` now checks it was: an analysis that declares one
    and produces no result for it is a WARN naming the analysis and the
    column. Nothing else compared the promise with the delivery, so an overall
    pass that silently produced nothing reached the workbook unremarked.
  * **Rule 6, at fill.** A display column whose every cell kept its
    placeholder *while other columns filled* is a WARN naming the column and
    how many did fill. The per-cell census already recorded each unfilled
    cell, but a reader had to notice that all of one column's cells happened
    to share a reason -- and a whole lost column is a different finding from a
    scattering of pending cells.

  Both stay quiet where they should. A partly filled column ("58.0 (xx.xx)")
  is a column that received results, not a lost one. A table where *nothing*
  filled has a different problem, already reported, and repeating it once per
  column would bury it. A column the cell map never reached is not a fill
  failure. The bundled example still runs with zero WARN and zero FAIL.

* **A Total column the shell displays must be a Total column the metadata can
  produce.** The delivered workbook that prompted the previous entry had
  numbers in every cohort column, placeholders in every Total cell, and *not
  one diagnostic* saying a displayed column had been dropped. The build
  reported success.

  `spec_to_ars()` now gates it: a table whose headers show a Total or Overall
  column that nothing declares is a **FAIL**, naming the table and the header,
  and saying both ways out -- annotate the header with the subjects it covers,
  or leave it unannotated to take the union of the group columns. It is a FAIL
  rather than a warning because the deliverable is wrong, not merely
  incomplete, and nothing downstream can tell.

  Alongside it, guidance rule 7: a producible overall column records what its
  scope *means* in the output metadata -- the label it fills, whether the
  scope was authored or derived, and the shell's own annotation text kept
  verbatim beside the parsed clause. A reviewer can check what Total covers
  against the shell without reading a WhereClause.

* **A shell annotated with double quotes had every condition thrown away, and
  its Total column never computed.** A field study delivered a workbook whose
  cohort columns held real numbers -- 482, 441, 27 -- and whose Total column
  was still `xx` in every row. Three defects, stacked.

  * **The where-clause grammar accepted only `'single'` quotes.** Every string
    pattern hardcoded `'…'`; not one accepted `"…"`. That study annotates the
    way its programmers write SAS -- `[ADSL.COMPLFL="Y"]` on the title,
    `ADMH.MHCAT="GENERAL MEDICAL"` on a row -- so every one of those
    conditions was dropped, silently, including the population filters. This
    was never only a Total-column problem: it was a wrong-population problem
    on every table. Both spellings are now accepted and a test asserts they
    produce the *identical* WhereClause, not merely that both parse.
  * **A value list had to be quoted.** `ADSL.RACE IN ('WHITE','ASIAN')`
    parsed; `ADSL.COHORTN IN (1,2)` -- how a coded column axis is actually
    written -- did not. Bare numbers are now accepted beside quoted values.
  * **A Total column had nowhere to go.** `include_total_hint` was only set
    for a Total header conditioning on a *different* variable than the column
    axis, so a `Total [ADSL.COHORTN IN (1,2)]` was neither a group nor a
    total: no metadata, no ARD rows, placeholder in the workbook.

  The rule now, per the shell: **the annotation on the column wins.** A Total
  column is computed from exactly the condition it carries -- including when,
  as here, that deliberately excludes a displayed column and so does *not*
  equal the sum of the columns beside it. Only when the column carries no
  usable annotation is its scope derived, as the union of the group columns.

  A Total column is never a level of the grouping factor. The emitted grouping
  is a first-match-wins `case_when`, so a Total level would be shadowed by the
  very columns it totals and report zero -- a wrong number where there had at
  least been an honest placeholder. It becomes a scoped overall pass instead,
  filtered on both the numerator and the denominator so the percentage comes
  out of the Total column's own N, and stamped with the shell's own header
  text so the fill can find the column it belongs to.

  Pinned end to end: a shell with cohorts of 40 and 30 beside an unknown
  cohort of 7 reports Total = 70, not 77 and not 0.

* **The fill was quadratic, and that is what the stuck progress bar was
  showing.** Tracing a full run of the bundled example with a timestamp on
  every progress event put 138 of its 165 seconds inside a single output --
  the nested SOC/PT adverse-event table. Profiling that output put 94% of the
  time in `gsub`.

  `.ard_value()` matches a cell's column heading against the ARD's by folding
  both to a common form -- the shell stacks two header rows, the ARD joins the
  grouping path with its own separator, and only the punctuation between
  identical components differs. But it folded the ARD side *inside the
  per-cell lookup*: two regex passes over every row of the ARD, for every cell
  of every output. Cells times rows, in `gsub`.

  The fold is now computed once, when the index is built, and each lookup
  folds only its own one-element needle. On the bundled example the fill goes
  from **153s to 13s** and the whole run from **165s to 25s**, writing a
  byte-identical workbook -- every one of the 15 entries in the archive
  unchanged, the same 820 cells filled.

  Two tests pin it: that the index carries the folded column, and that a
  differently punctuated heading still finds its value. `.ard_value()` stays
  total over its input -- an index built by hand, as a fixture does, folds on
  the spot rather than failing.

* **Writing the ARD reports itself.** The same trace found a second silence:
  `saveRDS()` of the results ran *between* two stages, where neither one's
  progress hook was installed, and took 6.6 seconds saying nothing. It now
  happens inside the stage that produced the results, so the hook is live, the
  stage's recorded time includes the write it is responsible for, and a write
  that fails is recorded as a failure of that stage rather than thrown.

* **The workflow app knows which project you meant.** `ars_workflow()` with no
  argument opened on five empty boxes every time, so a study set up the day
  before had to be typed out again from memory -- four paths, by hand, with no
  browser anywhere in the package.

  * `ars_workflow()` now resolves the project it was given none: the working
    directory if you are standing in one, otherwise the last project opened.
    Only a genuine first run gets the empty form, and when a project is
    resumed the panel says so and where the values came from.
  * Projects opened before are offered in a dropdown; choosing one repopulates
    every field and switches the app to it. The list lives in
    `tools::R_user_dir("arsbridge", "config")` and holds folder paths and
    nothing else -- no study data leaves the project folder. A folder that is
    no longer readable is hidden from the list rather than dropped from it, so
    a project on an unplugged drive comes back when the drive does.
  * With `shinyFiles` installed (a new `Suggests`), each of the four path
    fields comes with a **Browse** button -- folder pickers for the project
    and ADaM directories, file pickers for the shell and the spec, rooted on
    the home directory and the platform's volumes. The chosen path is written
    back into the text field, which stays the one place a path lives. Without
    the package the panel is exactly what it was, plus a line saying what
    would make it clickable.

* **A build in the workflow app can always be left.** The same field run
  reported the second half of the problem: a `supplement.json` was dropped
  into the project, the Build panel correctly flipped its mode line to
  *supplement* -- and there was no way to build with it, because the button
  was a disabled "Building..." that never came back.

  `running` had exactly one exit: the poller watching the worker process die.
  A build that outlived the user's patience had no way out, and a poll that
  threw was worse -- a Shiny observer that throws is destroyed, taking the
  only thing that would ever clear the flag with it, for the rest of the
  session.

  * A **Cancel** button sits beside "Building..." for the whole of a
    background build. It kills the worker and its children and hands the
    panel straight back, so the next build -- with the supplement, if there
    is one -- is available immediately.
  * The poller no longer dies on a failed poll. `readLines()` on a file the
    worker is writing can fail; that now costs one skipped tick instead of
    the build panel.
  * `running` with no worker to wait for self-heals rather than sitting
    there. It should be unreachable; it was also unrecoverable.
  * A worker that dies without returning a result now names its run log, so
    "callr subprocess failed" comes with somewhere to look.

* **The workflow app's progress bar stops going quiet before the run ends.**
  A field run reported it stuck on `Filling the workbook -- T_14_1_5 (4/4)`,
  full bar, nothing moving. Nothing was wrong with the build: the ticks only
  ever covered the three per-item LOOPS, so everything on either side of them
  ran with nothing to say. The longest of those silences is
  `openxlsx2::wb_save()`, which is the slowest single step of a real fill and
  runs *after* the last output has been walked.

  * Reading the inputs, writing the reporting event, binding the ARD and
    saving the workbook now each tick as a named step, so no stage runs out
    silent. A test asserts every stage's final event is one of them.
  * The per-item counter counts what is FINISHED rather than what is
    starting, and the panel says so -- `T_14_1_5 (3 of 4 done)`. The bar
    reached 100% as the last item *began*, which is most of why the end of a
    stage looked like a hang.
  * The panel now shows how long the build has been running and how long
    since it last reported anything, and says plainly when a build has gone
    quiet for more than three minutes. Elapsed time keeps moving however long
    a stage stays silent, which is what separates a slow run from a dead one.
  * The three log lines under the bar are gone. They were the last three
    *non*-progress lines, and with `verbose = FALSE` the worker writes nothing
    after start-up -- so they were pinned forever on the package-attach
    message from its first second and read as evidence of the hang. The raw
    log moved behind a "Run log" expander.

* **The reference bundle runs without a single warning, and a test keeps it
  that way.** Three findings, all of them the package complaining about
  something it had already understood:

  * **The enricher read the shell's column axis after deciding it had none.**
    A section whose shell states `[columns -> ADSL.TRT01A]` was run through the
    "no grouping variable identified" fallback first, warned about, and *then*
    given the axis the shell had stated all along -- six WARNs on the bundled
    example, one per table. The authored axis is now resolved first, which also
    closes a latent defect: when the spec's treatment variable differed from
    the authored one, the fallback added it *beside* the axis and turned a
    one-level column into a two-level cross.
  * **A listing was classified by its title.** "Listing of Adverse Events"
    matched the AE keyword and came back `AE_FREQUENCY`, so an output with no
    columns went through the grouping fallback and was warned about for having
    none. The shell's own output number decides whether something is a listing
    or a figure; a classifier -- heuristic or LLM -- does not overrule it.
  * **A directive clause was reported as a failed filter.**
    `once/subject ADAE.AOCCIFL` names a variable and carries text around it, so
    it looked like an attempted condition to the where-clause parser -- but it
    has its own consumer, and nothing was dropped. The warning told authors to
    fix an annotation the package reads correctly.

  The example test now asserts zero WARN and zero FAIL across every stage of
  the payload. `ars_diagnostics()` would not have caught any of this: it holds
  only the last stage's records, which is why a run that reported "no
  diagnostics" was carrying eight warnings from earlier stages.

* **A shell whose column axis names a domain variable is told so while it is
  still a shell.** Percentages divide by ADSL, and {cards} splits that frame by
  the analysis' own column variable -- so `[columns -> ADCM.TRTA]` against an
  ADSL that carries `TRT01A` cannot be split at all, and every percentage in
  the table comes out of the whole study. The parser now checks the resolved
  column axis against the ADaM spec and reports it:

  * **WARN** when the spec has no ADSL variable of that name -- the numbers
    will be wrong, and the message says what they would be out of and to point
    the axis at `ADSL.TRT01A`.
  * **INFO** when ADSL carries the same name (`ADAE.TRT01A`): the numbers are
    right, but the columns and their denominators then come from two datasets,
    which is worth saying and not worth warning about.

  Reported at parse time because it is a property of the shell and the author
  is who can fix it. `ars_to_ard()` still refuses to repair it by inference:
  `TRTA` and `TRT01A` are not interchangeable in a crossover or multi-period
  design, and a treatment mapping the engine guessed is not one anybody can
  review.

  The bundled example follows its own advice: every table's column axis now
  names `ADSL.TRT01A`, whatever domain the table's data comes from, so the
  reference shell shows the recommended form and parses without a finding. The
  numbers are unchanged -- ADAE and ADEX carry `TRT01A` with ADSL's values,
  which is exactly why that case was an INFO and not a WARN.

* **A column axis on a domain variable no longer computes every percentage out
  of the whole study.** Percentages divide by a population frame -- ADSL under
  the analysis set's filter -- and {cards} splits that frame by the analysis'
  own `by` variable. When the frame does not carry that variable, cards says
  nothing and uses the whole frame for every column:

  ``` r
  ard_categorical(cm, variables = "CMCLAS", by = "TRTA", denominator = adsl)
  #  A: n=2 N=10 p=0.2    <- N is the whole study, in both columns
  #  B: n=1 N=10 p=0.1
  ```

  That is the ordinary shape of a conmeds or medical-history table: those
  domains carry `TRTA`, ADSL carries `TRT01A`. The variable is subject-level,
  so it is now carried onto the denominator by subject -- from the domain under
  the **population** filter only, since a data subset selects events, not
  subjects, and must never shrink a denominator. Only the column axis is
  touched: a nested block's row grouping is appended after it and must not key
  the denominator, or a preferred term's percentage would come out of its
  system organ class instead of its arm.

  Two cases are refused rather than guessed, both with a diagnostic that says
  what the percentages are out of and how to fix the shell: a subject carrying
  two different values (a data problem, not something to resolve by picking
  one), and a population subject with no record in the domain -- joining anyway
  would make each column's N mean "subjects with a record in this domain",
  which is neither the population nor the study. arsbridge never infers that
  `TRTA` must be `TRT01A`.

  The emitted script decides it the same way, from the ARS alone (is the column
  axis on a non-ADSL dataset?), and writes the refusal out as a runtime guard --
  so a script taken away and run by hand reaches the same denominator as the
  engine did.

* **The bundled example's AE table counts treatment-emergent events, as its
  title says.** Its `<System Organ Class>` / `<Preferred Term>` rows carried no
  filter, so the block counted every adverse event under a table headed
  "Treatment-Emergent Adverse Events" -- 12 Placebo subjects with a nervous
  system event where 8 had a treatment-emergent one. Nothing propagates the
  "Subjects with any TEAE" row's filter down a block, so each token row now
  carries `WHERE ADAE.TRTEMFL='Y'` itself, and the bundle test checks the
  numbers against the raw dataset rather than against the pipeline that
  produced them. An authoring fix, not an engine one: the shell said something
  it did not mean.

* **A filled Excel shell now expands a nested block into the hierarchy it
  stands for.** `<System Organ Class>` over `<Preferred Term>` is a pattern,
  not two rows: it means "repeat this per system organ class". The fill writer
  had no way to say that -- only a listing could change a sheet's shape -- so
  the two token rows came back declined, and the bundled example's AE table
  shipped with placeholders where the study's classes and terms belong. The
  pair now expands: each class, then its own terms underneath, each line
  written from its own authored row so fonts, indents and the decimals the
  placeholder states all carry down the block. Rows below the block, footnotes
  and their merges included, move down with it.

  The order is the one thing that could quietly go wrong: the Word renderer
  and the Excel filler now present the same block, and two orderings would
  mean one study with two answers to "which class comes first". So the
  ordering is one function both call (`.nested_level_order()`), and the
  bundled example asserts the filled sheet and the rendered table agree row
  for row -- 265 lines, in the same order.

  The example study now fills completely: 856 cells, nothing pending.

* **A treatment arm with no qualifying subject now reads `0 (0.0)`, not a
  placeholder.** An analysis's data subset is applied before `{cards}` runs, so
  an arm the filter empties is not in the frame the executor sees --
  `ard_categorical()` can only report the `by` levels it is given, and the arm
  ended up with no row in the ARD at all. Everything downstream trusts the ARD,
  so the filled shell left the cell showing `xx (xx.x)`: a blank where the
  answer is zero, and a blank in a clinical table reads as missing. "No Placebo
  subject had a serious adverse event" is a finding; "we did not look" is not.

  For counting methods the ARD is now completed after execution -- one row per
  statistic the other arms report, with `n = 0`, `p = 0` and that arm's own
  denominator, counted from the same population the percentages use. It happens
  in `ars_to_ard()`, so the fill writer, the renderer and an exported ARD all
  get the same complete answer, and a completed row is stamped `computed` like
  any other because it is. A continuous summary is never completed this way:
  the mean of nothing is nothing, and a column axis declared by annotation
  completes from its declared labels rather than the raw codes underneath them.

  In the bundled example this closes the last fillable gap: Table 14.3.1's
  Serious TEAE row reads `0 (0.0)` for Placebo, and the run now fills 61 cells
  with 6 pending -- all six being the `<System Organ Class>` /
  `<Preferred Term>` block, which needs row expansion.

* **A `(N=XX)` column header is now filled, not only stripped.** The
  decoration was removed before matching a column to the ARD and then
  discarded, so every filled workbook shipped with `Placebo (N=XX)` above real
  numbers. Each result column's header now gets the denominator its own
  percentages are computed against -- `Placebo (N=86)` in the bundled example
  -- written as an ordinary cell edit, so the arm's name, its font and the
  author's spacing inside the brackets are untouched, and the write is
  counted and reported like any other.

  Where the number comes from is deliberate and narrow: the `N` of the
  analyses **this column shows a percentage for**, so a reader can check the
  header against the cells beneath it. A column whose analyses disagree, and a
  column showing no percentage at all, keep their placeholder -- `N` on a
  continuous analysis is {cards}' count of non-missing values, a different
  quantity wearing the same name, and a header is not the place to guess.

* **The bundled Excel shell is now the whole study, not half of it.** It
  carried four of CDSC-ALZ-201's eight outputs while the Word shell carried
  all eight -- and the bundle's own README advertised "one worksheet per
  output". The generator now authors the missing four: the TEAE overview
  (14.3.1), adverse events by system organ class and preferred term (14.3.2),
  study drug exposure (14.2.1), and the concomitant-medications listing
  (16.2.4.1), in the Word shell's order. `spec_to_ars_example(shell_format =
  "xlsx")` reports 8 outputs, and a new test compares the two shells output
  for output, so the drift cannot come back unnoticed.

  The example run fills 59 cells and leaves 7: the six cells of 14.3.2's
  `<System Organ Class>` / `<Preferred Term>` template rows, which the Excel
  writer does not expand, and one Serious-TEAE cell for an arm that had no
  serious event -- a cell with no result keeps its placeholder rather than
  being written as a zero. It reports no diagnostics, and takes about 30
  seconds end to end.

  `ADaM.zip` shrinks to 217 KB: ADCM keeps each subject's first occurrence of
  a medication, which is what its listing shows -- the full 7,510 records
  would have made the filled workbook a listing of repeat administrations.

* **A count row that shows a percentage now declares one.** A row annotated as
  a filter on its own variable -- "Completed study `[ADSL.EOSSTT =
  'COMPLETED']`" -- was always typed `MTH_SUBJECT_COUNT`, which declares the
  count and nothing else. Shells write those rows as `xx (xx.x)`, so the second
  number had no statistic to fetch and the cell shipped as `58 (xx.x)` with a
  warning, even though the engine had computed the percentage all along. The
  method inference now reads what the row actually displays -- from the cell
  grid in an Excel shell, from the row's own data cells in a Word one, so both
  formats still build the same ARS -- and types a two-number row as the new
  `MTH_SUBJECT_COUNT_PCT`.

  That method is Subject Count's arithmetic (one row per subject, then counted)
  with the percentage and denominator declared alongside, rather than Count and
  Percentage, which counts RECORDS: on a record-level dataset
  (`[ADAE.TRTEMFL = 'Y']`) borrowing that method would have turned a subject
  count into an event count without saying so. The executor is shared with
  `MTH_SUBJECT_COUNT`, so no number changes -- only which of them the ARS says
  the output shows. The example study's disposition rows now fill as
  `58 (67.4)` / `28 (32.6)`, and its run reports no diagnostics at all.

* **A filled listing no longer opens in Excel with its columns empty.** Every
  cell arsbridge added to a sheet -- the rows a listing's template expands
  into, the block a figure's series is written to -- was filed against
  openxlsx2's internal cell key of the row it was CLONED from, not the row it
  was written to. openxlsx2 serializes cells into `<row>` elements by that
  key, so the cells landed inside the wrong row element; Excel treats such a
  cell as invalid and discards it, while openxlsx2's own reader goes by the
  cell reference and shows the sheet as complete. The bundled example's
  1,191-row listing shipped with 4,760 of its 5,963 cells unreadable in Excel
  and every R-side check green. Rows written past the end of the sheet were
  dropped outright for a second reason -- openxlsx2 writes rows from its
  row records, and those rows had none -- which is what left figure series
  truncated (the example figure now writes all 36 series rows, not 7).

  Both invariants are now maintained wherever cells move or are added, stated
  in one place (`.cc_problems()`), checked on every sheet before the workbook
  is saved, and asserted against the raw XML by the fill tests -- the check
  that would have caught this the day it appeared.

* **The bundled example study is now CDSC-ALZ-201 -- an Alzheimer's study
  built entirely from public pharmaverse data.** The invented APX-DRM-301
  dermatology bundle retires; its files live on locally under `inputs/DRM/`
  (never tracked). The replacement's datasets are the CDISC pilot Xanomeline
  data straight from `{pharmaverseadam}` (Apache-2.0, no real patient data),
  and -- for the first time -- the whole bundle is reproducible:
  `data-raw/build_example_bundle.R` generates every file from one curated
  variable list. The old bundle had no generator at all; it was committed
  as opaque binaries nobody could rebuild.

  The bundle also gains what the old one never had: an annotated **Excel**
  shell (`annotated_shell.xlsx`), so the example now demos the package's
  headline end to end, offline -- build the ARS, execute it, and write the
  shell back filled with the study's real numbers. A permanent test pins
  that chain with pharmaverseadam itself as the oracle (86 / 96 / 72
  safety subjects landing under their own arm headers).
  `spec_to_ars_example()` gains `shell_format = c("docx", "xlsx")`, runs
  deterministically in seconds (8 TLFs), and stamps the new study identity.
  The ADaM spec -- pharmaverseadam labels and types, plus codelists for the
  decoded categories -- is also written to
  `inputs/adam_spec_CDSC-ALZ-201.xlsx` for hands-on use.

* **`inputs/` is now purely your workspace.** No study files are tracked
  there any more except the public example spec above and the README:
  everything else is ignored by default, so `git add -A` can never sweep in
  study material. The old per-file allowlist for the retired example is
  gone with it.

* **callr is now an Imports**, so it installs with the package -- as a
  Suggests it never arrived automatically, and every workflow-app build on
  a fresh machine silently fell back to the frozen in-process mode. The
  app's fallback gate stays, as a defensive backstop.

* **The build shows its progress -- in both of the app's modes.** A real
  study takes minutes, and the person watching had no way to tell a slow
  run from a hung one: the in-process fallback showed a bare
  "Building (in this process)..." with no bar and no updates, and even a
  background build's log tail was near-empty. `ars_workflow_run()` gains
  `on_progress`, a callback receiving one event per stage entry and per
  TLF / analysis / sheet within it. In-process the app forwards events to
  a live Shiny progress bar ("Building the reporting event -- T-14-1-1
  (3/12)"); in background mode the worker writes one `[progress]` line
  per event into `run.log` and the build panel renders a real bar from
  the last one, with the log's human lines beneath. Opt-in: a run with
  nobody listening behaves exactly as before.

* **Every road to the in-process fallback now says so.** Three of the four
  were silent -- a user staring at a frozen UI could not learn why, or
  that `install.packages("callr")` would fix it. The callr-missing gate
  says exactly that; the option gate and a failed background launch
  notify too; and the version-skew message no longer prints "the
  installed arsbridge is NA" when the package is absent.

* **Fixed: an `(N=XX)` decoration on a column header made every cell in
  that column unfillable.** The ARS side strips the decoration when it
  derives grouping levels, but the fill map stored the header label
  verbatim -- so "Placebo (N=XX)" could never match the ARD's "Placebo",
  and every result cell came back pending with no visible cause. The fill
  map now strips the same way, with the same helper.

* **Fixed: a header cell holding only an annotation shifted every fill
  column one to the left.** An annotation-only stub header leaves a blank
  label once the directive is lifted; the compacted header vector dropped
  the blank and renumbered, so the fill map's columns pointed one cell
  left of their headers -- the wrong-number-under-the-right-header
  failure, which nothing downstream can catch. The parser now keeps the
  uncompacted per-physical-column labels and the fill map is built from
  those; a blank column stays unmapped rather than joining wrongly. Both
  fixes are pinned by a new fixture workbook recreating the dialect
  (uppercase `XX` placeholders and `[a]` footnote markers included).

* **An unfilled workbook now says so, everywhere it can.** The field
  failure: a build completes, every artifact exists, and the "filled"
  workbook shows every placeholder intact -- with the explanation buried.
  Now `ars_fill_shell()` warns with the dominant reason when it filled
  nothing (naming the clean-shell case outright: no annotations detected
  -- annotate the shell, or declare the bindings in a reviewed
  supplement, which can bind a clean shell's rows); the two silent exits
  (an output with no cell map, a sheet missing from the workbook) leave
  skipped records instead of vanishing; the payload carries the headline
  counts as `payload$fill`; the app's completion notification
  distinguishes "complete, but the workbook was left unfilled"; the
  Results step renders the per-cell census as a filterable table and
  shows a clean-shell callout; and the supplement step's copy no longer
  claims it is "only if the build got something wrong".
  `tools/LOCKED_MACHINE_DEBUGGING.md` gains a reason-by-reason triage
  table, sharable as-is because the reason strings carry no study text.

  And the fully clean shell -- the variant the live acceptance run caught:
  zero analyses execute, `ars_to_ard()` returns `NULL`, and the fill stage
  is *skipped*, so there is no census to warn from and the run announced
  plain success. The payload now carries two WARN diagnostics ("no
  executable analyses" / "the filled workbook was not produced"), and the
  app's notification and clean-shell callout recognise the fill-never-ran
  shape as well as the filled-nothing one.

* **Fixed: a reviewed `methodId` was silently dropped whenever the supplement
  agreed with the shell about the variable.** Found by the end-to-end
  acceptance run, and it broke the exact loop the draft workflow is for.
  `.apply_supplement_bindings()` treated an already-annotated row as settled
  the moment the supplement's variable matched the shell's, and returned
  before it ever read the row's `methodId`. But a draft from
  `write_supplement_draft()` is generated *from* the shell, so its variable
  always matches — meaning the one correction a reviewer most often makes,
  the statistical method, was the one correction that could never land. It
  only worked if you also changed the variable to something else, which is
  not a thing anyone would do on purpose.

  The method is now recorded on an agreeing row too. Nothing else about
  agreement changes: the shell's annotation still stands, the row is not
  flagged as a conflict, and the decision to honour the value still belongs
  to `build_ars_json()`, which applies it for a supplement-bound row or under
  `supplement_trust = "prefer_supplement"`. An off-catalogue id is still
  ignored.

* **Fixed: the supplement instructions still told you the shell had to be a
  `.docx`.** Excel shells have been readable since the parser landed, but the
  three Copilot instruction files, the console hint from
  `ars_copilot_instructions()`, and its help page all named `.docx` as the
  input — so anyone with an Excel shell was being told, by the package
  itself, that their shell was the wrong kind of file. All of them now say
  `.docx` or `.xlsx`, and the single-file instructions describe what an
  Excel shell looks like to the assistant: one worksheet per output, the
  annotation coloured in the stub cell, the header band merged across the
  top.

* **Fixed: the README said supplement format v3.** Same fault as the vignette
  fixed above, in the "three ways to read the shell" section. The shipped
  schema is v4 and a v3 file is rejected.

* Both the README and `ars_copilot_instructions()` now point Excel users at
  `write_supplement_draft()` first. It is a materially better loop: the Excel
  parser already settles the column axis and the row bindings on its own, so
  the assistant corrects a structured draft instead of authoring one from
  nothing — and what is left for it to decide is the statistical judgement
  (which `methodId`, which denominator), which is where an assistant is
  actually worth consulting.

* Fixed: `inputs/README.md` documented `shell_to_ars()` and `shell_annotate()`,
  neither of which exists — the file predated `spec_to_ars()` and had never
  been updated. It also promised the folder was excluded from git, which
  stopped being true when the APX-DRM-301 practice files were allowlisted.
  Rewritten against the actual API, the actual default-deny policy, and the
  files that are really in there.

* `DESCRIPTION` and the pkgdown home page described a Word-only reader.
  `DESCRIPTION` also now carries the pkgdown site URL alongside the GitHub
  one.

* **A draft supplement now states the column axis for an Excel shell.**
  `write_supplement_draft()` fills `groupings`, `columnHierarchy`,
  `includeTotal` and `listingColumns` from the parse — a nested header becomes
  a column hierarchy, a flat one a grouping, and a listing gets its columns.
  These stay blank for a Word shell, deliberately: a Word header grid has to
  be inferred, and a draft that asserts an axis and gets it wrong is worse
  than one that stays quiet, because a reviewer checks what is flagged and
  trusts what is stated. An Excel sheet has no such doubt.

* Fixed: `vignette("no-api-access")` told you to have the assistant reply in
  supplement format **v3**. The shipped schema is v4, and v3 is rejected — so
  anyone following the vignette produced a supplement the validator refused.

* **`ars_workflow()` is one phase, not two.** The app used to make you carry a
  blueprint out to a chat assistant and a supplement back before anything could
  be built — two manual round-trips in front of the first result. A
  deterministic build needs neither, so the build now comes **second**, right
  after recording the inputs, and the supplement became an optional loop after
  it: generate a draft from what the parser already found, correct the handful
  of judgements that are wrong, rebuild. Correcting specific decisions beats
  authoring a document from scratch.

  Five panels replace six: project setup, build, supplement (optional), review
  & edit, and a new **Results** step showing every artifact, every diagnostic
  with its severity and its fix, and the cells the run declined to fill. The
  results are read from the payload on disk, so they survive closing and
  reopening the app.

  **The build runs off the UI's process.** A real study takes minutes, and an
  app that runs that on its own process is frozen for the duration — you cannot
  tell a slow build from a hung one. It goes to a background R process
  (`callr`, a new Suggests) and the panel tails the run log while it runs. A
  fresh process each time means no state carries over between runs. Without
  `callr`, or with `options(arsbridge.workflow_background = FALSE)`, the build
  runs in-process instead: a frozen UI is worse than a responsive one, and much
  better than not being able to build.

  The project layout follows: `copilot/` is now `supplement/`, and `ars/` also
  holds `ard.rds`, `filled_shells.xlsx`, `run.log`, and the payload of the last
  run. A project can now record an ADaM folder, so one build produces the
  reporting event, the results and the filled workbook together; without one
  the reporting event is still built and the rest is reported as not produced
  rather than treated as a failure.

* **New `ars_workflow_run()`: the whole pipeline in one call, as a value.**
  Takes paths, runs `spec_to_ars()` → `ars_to_ard()` → `ars_fill_shell()`, and
  returns one structured list — status, per-stage timings, every artifact's
  path, the metadata the run was built with, the diagnostics, the cells
  reserved for manual derivation, and the workbook cells left unfilled with
  their reasons. It holds no state and **never throws**: a run that dies still
  returns a payload saying which stage failed and where its log is, which is
  when a log is worth having.

  It exists because `ars_workflow()` is a Shiny app and a six-minute build
  must not run on the UI's process. The build is now a plain function that
  knows nothing about Shiny, so it can be sent to a background process. It is
  also useful on its own — in a script, a scheduled job, or a validation run.

  **Diagnostics now survive a process boundary.** They live in a package-level
  environment, so in a background worker they accumulate there and
  `ars_diagnostics()` in the calling session returns nothing; every FAIL and
  WARN would vanish silently. They are returned in the payload instead, as one
  table with a `severity` column rather than split by severity — so `INFO`
  findings survive too, including the shell-label-to-data-code pairings the
  fill writer reports for review.

  Diagnostics are harvested **after each stage**, not once at the end, because
  `spec_to_ars()` and `ars_to_ard()` each call `diag_reset()` on entry. On the
  nine-sheet fixture, reading once at the end returns 1 finding; harvesting
  per stage returns 41.

  A background process also inherits no options and no environment, so
  `derived_dt` (which pins ARD timestamps, and therefore reproducibility), the
  LLM provider and the API key are explicit parameters, echoed back in
  `metadata` so an archived payload says what produced it.

* **`ars_fill_shell()` now fills listings and figures too, not just tables.**

  A listing's shell states ONE template row standing for however many rows the
  data has, so filling it changes the shape of the sheet: the row expands into
  a row per record, and everything below — a footnote and its merge — moves
  down to make room. The written rows are built from the template row's own
  cells, so the author's fonts and alignment carry down the block. Its rows
  come from `.listing_data()`, factored out of `ars_render_listing()` and now
  shared, so a filled listing and a rendered one cannot contain different
  subjects.

  A figure sheet has no analyses at all — the shell states the plot as prose
  (`Y axis -> mean of ADVS.AVAL`, `Series (colour) -> ADVS.TRTA`,
  `Filter -> ADVS.PARAMCD='PULSE'`) — so its series is computed here, from the
  datasets, and written as a data block where the annotation block was: one
  row per series and x-value with `n`, the mean and its standard error. Those
  go in as **numbers**, not rounded text, because a programmer picks them up
  to draw the chart and a rounded series would no longer equal what they
  compute from the same data. The chart object itself is left as the author
  made it.

  Both need the new `adam_dir` argument: a listing's rows *are* the
  subject-level data, and a figure's series is computed from it. Tables still
  need only the ARD. (`adam_dir` replaces the unused `datasets` placeholder.)

  Empty and unreadable cases are reported rather than guessed at, and told
  apart: a listing that selected no rows is a real answer, one whose data
  could not be read is not, and neither is filled silently.

  Implementation note for anyone extending this: openxlsx2 has no
  row-insertion API, so the expansion goes through a new internal
  `.shift_rows_down()`, which moves cell references, per-row records, merged
  ranges and the sheet's declared extent together. Missing any one of those
  still produces a file that opens, which is what makes it dangerous — a
  stale merge silently swallows a data row. A sheet carrying anything else
  row-bounded that arsbridge cannot move — conditional formatting, data
  validation, hyperlinks, an autofilter, a worksheet table, or any formula —
  is **refused rather than half-moved**: it is left exactly as authored and
  reported as a FAIL, and every other sheet is still filled. A listing that
  is visibly unfilled is a better outcome than one whose conditional format
  now highlights the wrong row.

* Emptying a cell that was entirely an annotation now clears its formatting
  as well as its text. Previously the cell kept its red font, which showed up
  the moment anything was written there — as a figure's series is, into the
  block the annotation occupied.

* **Excel shells, end to end: `spec_to_ars()` reads them, and
  `ars_fill_shell()` writes the results back into them.** A study can now be
  authored as a `.xlsx` with one worksheet per output, annotated in-cell in
  red, and the rest of the pipeline behaves exactly as it does for the `.docx`
  you have always passed — `spec_to_ars()`, `parse_decision_digest()`,
  `write_supplement_draft()` and `ars_workflow()` all dispatch on the file
  extension. Word support is unchanged and permanent: every Word fixture was
  checked byte-for-byte at each step of this work.

  The reason to author in Excel is the new deliverable. `ars_fill_shell()`
  takes the shell, the ARS built from it and the ARD, and returns the author's
  own workbook with the numbers in their placeholders and the annotations
  gone. Nothing is laid out or re-created: the layout, row labels, column
  headers, merges, fonts, column widths and footnotes come through untouched
  because none of them were ever lost, and each placeholder's own
  `xx.x (xx.xx)` decides how its number is formatted, punctuation and all.

  Which result belongs in which cell is decided when the ARS is built, not
  when the workbook is written — each output carries a `_meta$shell_fill` cell
  map, recorded at the one moment the shell's geometry and the analyses are
  both in hand. `_meta` is arsbridge's own namespace, so conformance is
  unaffected and a consumer that ignores it sees what it always saw.

  Nothing uncertain is written. A cell keeps its placeholder and is reported
  when the ARD has no result for it, when the result is reserved for a manual
  derivation (ADR 0002), or when the row is a template standing for a repeated
  block such as `<System Organ Class>` — writing one system organ class's
  count there would hide every other one behind a real-looking number. An
  empty cell in a clinical table reads as a zero, which is why the placeholder
  stays; `keep_pending_placeholders = FALSE` blanks them instead, and
  `strip_annotations = FALSE` keeps the annotations beside the numbers while
  reviewing. Listings and figures are not filled yet.

  Two disagreements this surfaced that nothing compared before. A placeholder
  asking for more statistics than its analysis produces — a row showing
  `xx (xx.x)` typed as a plain subject count — is now a WARN naming the row.
  And a shell's decoded row labels are matched to the data's codes, so rows
  reading "Female" and "Male" fill from an `ADSL.SEX` holding `F` and `M`;
  that pairing is refused rather than guessed unless each label names exactly
  one value and the whole block is one-to-one, and every pairing used is
  reported as an INFO diagnostic.

  The two readers are held together by a test rather than by intention:
  `test-parity_docx_xlsx.R` parses the same study authored both ways and
  requires the identical-semantics fields to match and the same ARS to come
  out. Design records: `adr/0004-xlsx-shell-input.md` (why both formats are
  permanent, and the three classes every section field falls in) and
  `adr/0005-filled-shell-output.md` (the cell map, and why the writer edits
  run XML instead of going through openxlsx2's data model — a run in a real
  shell carries `rFont` and `sz`, which rebuilding would silently reset).
  New tooling: `tools/parity_check_shell.R` compares your own Word/Excel pair
  outside CI, `tools/shell_structure_digest_xlsx.R` is the privacy-safe
  geometry digest for a locked machine, and `tools/xlsx_roundtrip_check.R`
  re-verifies the writer's assumptions after an openxlsx2 upgrade.

* **Fixed: `ars_to_ard()` failed outright on a study mixing a declared
  multi-grouping column axis with ordinary by-treatment analyses.** The two
  executor paths disagreed on whether the `*_level` columns are list columns,
  and binding them aborted with "Can't combine `<list>` and `<character>`",
  losing the entire ARD for a perfectly well-formed study. Columns are now
  aligned to the `{cards}` convention before binding.

* **New `parse_decision_digest()`: privacy-safe record of what the parser
  decided.** The counterpart of `tools/shell_structure_digest.R`: the same
  A/a/9 silhouette rules applied to the parser's conclusions -- header rows
  flagged vs inferred, physical grid width vs flattened column-label count,
  column labels and stub-row labels as silhouettes, column-tree shape, and
  per-severity diagnostic counts. Diffing the two JSONs on a locked machine
  localizes where a parse diverged from the document (e.g. column headers
  that came out placeholder-shaped) without any study text leaving it.
  `.populate_table()` now records its header decision
  (`header_rows_flagged` / `header_rows_inferred` / `n_header_rows` /
  `n_physical_cols`) on the section to support this.

* **A cell's paragraph list is now the lossless source both consumers build
  from.** New `.cell_paragraphs()` keeps a multi-paragraph cell's paragraphs
  intact (normalized, trimmed, order preserved), and annotation detection
  gains a second view: when the joined cell text yields nothing, the
  plain-text layer retries on the paragraphs joined BARE
  (`.detect_annotation_wrapped()`), because a wrapped annotation belongs
  joined with nothing while a wrapped label belongs joined with a space --
  one string cannot serve both. The recovered row carries
  `detection_method = "pattern_wrapped"` (medium confidence); the label is
  rebuilt from the paragraph list with spaces, so words never fuse. This
  removes the failure mode behind the `45e0481`/`1985e15`
  regression-of-a-regression: a wrap the join heuristic cannot classify no
  longer silently drops the annotation. Applied to stub cells, header
  cells, and the column-tree header grid.

* **Grid-first table model.** New internal `.table_grid()` expands a Word
  table into its physical R x C occupancy grid -- gridSpan cells repeated
  across the columns they cover, vMerge continuations resolving to their
  anchor, ragged rows padded -- so geometry questions are answered from the
  grid instead of raw cell indices. First consumers: (1) a continuation
  table under the same heading is appended only when its physical column
  count matches the open display; a mismatched table is refused with a FAIL
  diagnostic instead of silently welding misaligned rows on (two genuinely
  distinct tables under one heading no longer merge); (2) the
  "annotation found in data column N" diagnostic reports the physical grid
  column, which a gridSpan cell earlier in the row previously shifted.

* **New reviewed-manifest workflow: `write_supplement_draft()`.** The parse
  can now be exported as a v4 supplement JSON -- one entry per TLF with the
  title, typed analysis-set condition, and one analysis per annotated stub
  row; everything the parser was unsure about (unannotated rows, out-of-spec
  references, annotated statistic sub-rows) is listed in that entry's
  `provenance$reviewItems`. Review the draft once, correct it, keep it under
  version control, and feed it back with
  `spec_to_ars(supplement = ..., supplement_trust = "prefer_supplement")`:
  the reviewed file then overrides a wrong parse instead of only filling
  gaps, turning a parser miss into a file edit instead of a code change.

* **A supplement can now REMOVE a wrongly-parsed row.** A per-analysis
  `"suppress": true` entry (rowLabel only, no variable) drops the matching
  stub row -- for captions swept into the stub column or wrongly-merged
  continuation rows. Honoured only under
  `supplement_trust = "prefer_supplement"`; under `fill_gaps` the request is
  surfaced as a WARN instead of applied, and either way nothing disappears
  silently.

* **Table/figure number collisions no longer cross-bind.** A study can have
  both Table 14.3.1 and Figure 14.3.1; supplement matching previously
  reduced both to "14.3.1" and bound whichever entry came first.
  `.match_supplement_tlf()` now breaks number ties with the key's designator
  prefix ("T-14-3-1", "Figure 14.3.1") or the entry's `outputType`, and
  drafts are keyed by the full designator-bearing number.

* **Row and table accounting is now an enforced invariant.** Every `<w:tr>`
  in a parsed table must end up classified -- header row, data row, or
  skipped with a known reason (no cells, vMerge ghost, struck through) --
  and every top-level `<w:tbl>` in the body must be handled: attached to a
  section, recognised as the TOC, or flagged as unparsed. Any miss raises a
  FAIL diagnostic naming the TLF, so the silently-dropped-rows bug class
  (the one that once cost 23 tables without a message) now fails loudly at
  parse time. A table appearing before any recognised TLF heading is
  reported with a WARN instead of vanishing. A new combinatorial fixture
  generator sweeps all 64 combinations of continuation table, unflagged
  multi-row header, vMerge stub, multi-paragraph cell, strikethrough, and
  gridSpan data row, asserting the invariant on each.

* **Continuation tables are no longer dropped.** A shell that splits one
  display across several Word tables (a page break mid-table) lost every row
  after the first table: arsbridge modelled one table per TLF heading and
  discarded the rest with a warning. On the study that surfaced this, 23
  tables were being dropped, which is why several outputs came out with no
  rows at all. Extra tables under the same heading now append their rows to
  the open display, in document order, with an INFO diagnostic. A repeated
  header row at the top of a continuation is recognised and skipped, and
  never overwrites the captured column geometry.

* **An annotation that wraps mid-token is read whole again.** Word breaks a
  narrow header cell in the middle of a reference (`[ADSL.C` / `OHORTN=1]`).
  Joining a cell's paragraphs with a space -- introduced to stop wrapped
  LABELS fusing ("dataextraction") -- truncated such an annotation to
  `ADSL.C` and dropped its condition silently, which left conditioned
  sub-columns unreadable and their column hierarchy flat. Paragraphs now
  join bare when the break falls inside an unclosed bracket or mid-reference,
  and with a space otherwise, so both cases are correct.

* **Client-content guards hardened.** `inputs/` is now default-deny in
  `.gitignore` with the synthetic APX-DRM-301 practice files allowlisted, so
  a sponsor document copied in under any name cannot be swept into a commit
  by `git add -A` (it previously could -- only `inputs/ADaM/` and
  `inputs/.shell_backup/` were ignored). Local `HANDOFF_*.md` planning notes
  are ignored by the same principle. Both are reversible with `git add -f`.

* **New: privacy-safe shell structure extractor** (`tools/`). For shells that
  may not leave the machine they live on, `digest_shell()` writes a JSON
  digest of a `.docx` describing only its SHAPE -- table geometry, merged
  cells, paragraph and run counts, strikethrough and colour flags -- with all
  text reduced to character-class silhouettes and tokenised annotation
  skeletons. Needs only xml2 and jsonlite, so it runs where arsbridge is not
  installed. `redact_diagnostics()` does the same for a diagnostics frame.

* **The `(Q1, Q3)` row is no longer empty.** A continuous summary maps
  `p25`/`p75` (and `q1`/`q3`) onto a "(Q1, Q3)" stat line, but no formatter
  emitted that structure -- the row rendered with its label and no numbers,
  silently dropping the quartiles. Both stat-name spellings now format as
  `(xx.x, xx.x)`, matching the `(Min, Max)` line beside it.

* **Figures carry their number and title.** A figure was written into the
  combined deliverable as a bare image: no heading, no title, no
  footnotes -- nothing tying it to its shell. Figures now get the same
  heading / title / footnote band that tables and listings get.

* **A spanning header that names its grouping VARIABLE now builds a column
  hierarchy.** A two-level header whose parent carries the variable
  (`Alpha Cohort [ADSL.COHORTN]`) and whose sub-columns carry the values
  (`Low [ADSL.COHGR1N=1]`, ...) was classified FLAT, collapsing an
  unambiguous hierarchy into a row of undifferentiated columns. A top-level
  node now declares the column axis either by carrying its own condition or
  by being a group over conditioned children. Genuinely flat headers are
  unaffected.

* **A header that cannot be resolved into a hierarchy says so.** When a
  spanning header exists but no column tree could be built, a WARN
  diagnostic names the spanning columns and the precondition that failed
  (no readable sub-column conditions, or a parent declaring neither a
  condition nor a variable) and states what to annotate. Previously the
  fallback to flat columns was silent. A spanning header over STATISTIC
  sub-columns ("Treatment A" over "n" / "(%)") is a display split, not a
  hierarchy, and is never warned about.

* **Numbered mock rows (`SOC#1` / `PT#n`) are a second token dialect.**
  Shells author a nested block either with angle tokens
  (`<System Organ Class>` / `<Preferred Term>`) or with numbered ones
  (`SOC#1`, `PT#1`, `PT#2`, `PT#n`, then `SOC#2` ...), annotating only the
  first of each and leaving the repeats bare. Only the angle dialect was
  recognised, so a numbered block produced no hierarchy and its bare
  repeats were persisted as authored label rows -- the placeholder text
  rendered as if it were real data. Token rows now inherit their variable
  from the first annotated row sharing their stem, and mock rows are
  resolved before the label-only branch.

* **Single-level mock rows collapse into the row they illustrate.** A run
  of token rows on ONE variable directly under a categorical row of that
  same variable (`Primary reason for discontinuation [ADSL.DCSREASN]`
  followed by `<Reason #1>`, `<Reason #2>`, `...`) illustrates that row's
  levels rather than naming new analyses. Those rows -- and a trailing
  bare `...` continuation -- are now emitted nowhere, with an INFO
  diagnostic each; the categorical analysis above already expands every
  observed level. A mock run on a DIFFERENT variable than the row above it
  is left alone.

* **Struck-through shell rows are no longer programmed.** A stub row whose
  every run carries `<w:strike/>` is a row the shell author removed from
  scope; arsbridge parsed it as live and re-added the dropped analysis to
  the deliverable. Such rows are now skipped with an INFO diagnostic
  naming the row, so the reviewer can see what was dropped and why. A
  PARTIALLY struck cell (a value crossed out and retyped beside it) stays
  live.

* **Stub labels that wrap in Word keep their spaces.** A cell holding
  several paragraphs was flattened by concatenating every text run with
  nothing between them, so a label wrapped after "...the data" came back
  as "dataextraction", and two categorical levels authored as separate
  paragraphs fused into one line. Paragraphs inside a cell now join with a
  space; runs within a paragraph still join bare.

* **Real-world "standardized bracket" shell conventions are understood.**
  Shells that nest a `[PROGRAMMING DATASETS USED: ...]` directive inside a
  filter annotation used to corrupt the population string (the directive
  text leaked into the filter VALUE, poisoning every downstream subset).
  A new bracket normalizer scans top-level `[...]` spans with a nesting
  counter and rewrites each one: nested directives are lifted out;
  instruction wrappers are UNWRAPPED to the condition they carry
  (`[Use the stated source variable ... apply the stated condition:
  USUBJID WHERE ADSL.COMPLFL="Y". Keep the display label separate ...]`
  becomes `ADSL.COMPLFL='Y'; count of unique USUBJID`, and the
  `[Use DS.VAR for this displayed row; apply FILTER ...]` variant becomes
  `DS.VAR WHERE FILTER`); prose count instructions (`UNIQUE SUBJECTS
  WITH ...`) gain the distinct-subject marker; footnote markers (`[a]`,
  `Total[c]`) are stripped from labels; `Repeat ...` template directives
  and pure guidance prose are dropped with a record of what was removed.
  A space after the dataset dot (`ADSL. COMPLFL`, a Word line break) is
  closed up. Numeric `IN` lists (`ADSL.COHORTN IN (1,2)`) are now a
  recognised annotation form. Applied at stub cells, column headers,
  population paragraphs, and heading tails.

* **Workflow projects use a single `copilot/` exchange folder.** The
  `phase1/` and `phase2/` subfolders are gone; the blueprint,
  `supplement.json`, and the extraction validation report now land in
  `copilot/` beside the instruction files, so a project folder is just
  `copilot/` + `ars/` + the state file.

* **Nested AE/MH/ConMed blocks sort by descending frequency by default.**
  A nested SOC/PT (or body-system/term, ATC/medication) block now renders
  the most frequent parent level first, with the child terms in descending
  frequency under each parent (ties alphabetical) -- the standard safety
  presentation -- instead of A-Z at both levels. An authored `sort:`
  clause on the parent token row overrides the default:
  `sort: alphabetical` keeps A-Z, `sort: desc-freq('<column>')` counts a
  single treatment column as the sort basis (falling back to all columns
  where that column saw no events). The clause travels on the
  `_meta.shell_layout` entry; an unreadable clause raises a WARN
  diagnostic and keeps the default.

* **Conflict secondaries inherit the winning row's counting discipline.**
  A supplement proposal that conflicts with the shell's own annotation is
  built as its own analysis beside the winner -- but its method was
  re-inferred from the annotation alone, so the variant of a
  distinct-subject AE row degraded to record-counting
  count-and-percentage. Records over a subject denominator rendered
  impossible percentages (p > 1) in the variant block; the per-analysis
  rescale guard contained the display, but the numbers were wrong. The
  secondary now inherits `MTH_AE_FREQUENCY_COUNT` from the winning row
  (and an explicit `once/subject` clause achieves the same without one).
  On the incident study every computed proportion is now within [0, 1].

* **Listing headers no longer leak annotations into production output.**
  Rendering the CDSC-ALZ-201 listings surfaced three related parser
  defects. (1) A listing's DISPLAY column labels were captured raw at
  parse time, so the in-cell programmer annotation travelled into the ARS
  display columns and the rendered header ("Subject[ADAE.USUBJID]"
  instead of "Subject"); the annotation-stripped labels the header
  detection produces are now the display text. (2) When the colour layer
  found an annotation inline in a single-paragraph header cell, the label
  kept the annotation text; the detected text is now cut out of the
  label. (3) A fully qualified DATASET.VAR reference in a header
  annotation was tokenised -- the dataset name read as a variable,
  fabricating "ADAE.ADAE (ADSL.TRT01A)" from "[ADAE.TRT01A]"; dotted
  references are now taken whole. Net effect on the incident study: clean
  listing headers end to end and eight fewer pipeline warnings.

* **Layout renderer: three containment fixes for corrupt cells** (found
  rendering the regenerated CDSC-ALZ-201 study end to end). (1) A
  `supplement_added` row whose analysis computed a multi-level
  distribution (the conflict-variant of a categorical or nested block)
  now expands like a categorical block instead of piling every level onto
  one authored line and corrupting the pivot into list-cells. (2) An
  authored EMPTY label (a spacer) is excluded from the tolerant
  sub-row matching -- `startsWith(x, "")` matched every computed level, so
  a spacer could swallow a level and render it blank at the spacer's
  position. Supplement rows also get their own (noprint) group identity,
  so a conflict-variant repeating its shell twin's exact label can never
  collide with it. (3) The proportion-to-percent rescale is now gated per
  analysis rather than per output: one analysis whose `p` escaped [0, 1]
  (a denominator defect in a supplement variant, tracked separately) no
  longer silently blocks the rescale for every healthy analysis in the
  table, which had been printing `0.2%` where `20.0%` was meant.

* **Nested two-level row hierarchies, validation and polish** (nested
  SOC/PT handoff, Phase N4 -- completes the handoff). Two
  `validate_ars_model()` checks guard the new structure:
  `NESTED_CHILD_UNLINKED` (WARN) when a nested_child layout row no longer
  resolves to its parent row, and `NESTED_GROUPING_MISSING` (WARN) when
  the child analysis lost the parent's variable from its groupings --
  each naming the repair. The editor's shell view badges nested rows. An
  integration test now proves the whole chain from the real annotated
  shell document: parsing the authored `<System Organ Class>` /
  `<Preferred Term>` token blocks yields the linked analyses, the
  data-driven SOC grouping, the distinct-subject method, and a clean
  validation. The deterministic fixture was regenerated through the new
  generator -- a near-no-op (version stamp plus one validation row from a
  newer codelist check), confirming non-nested studies are untouched.

* **Nested two-level row hierarchies, renderer interleave** (nested SOC/PT
  handoff, Phase N3). The layout renderer now expands a
  `nested_parent`/`nested_child` pair per parent level: the parent's own
  line (SOC counts), then every child term observed under it, repeating
  per level -- the standard interleaved AE presentation the shell
  authored, replacing the two stacked flat blocks. The authored token
  labels never print; the data levels take their place. {cards}' full
  parent-by-child level cartesian is filtered to observed cells, so a term
  never lists under a parent it did not occur in, while a term genuinely
  coded under two parents renders in both blocks with per-cell counts.
  Levels sort alphabetically (the frequency-sort annotation is a later
  phase). `.shell_layout()` carries the new `parent_order` linkage; an
  orphaned child row (parent removed) degrades to the flat categorical
  block instead of erroring.

* **Nested two-level row hierarchies, engine verification** (nested SOC/PT
  handoff, Phase N2). The distinct-subject counting behind
  `MTH_AE_FREQUENCY_COUNT` now includes the analysis's grouping variables:
  a subject is counted once per (arm, parent level, term) cell, so dirty
  data that repeats one term string under two parent levels counts its
  subjects in each cell instead of silently dropping them from all but the
  first (previously the distinct ran on subject + term only). Applied
  identically to the emitted {cards} block and the legacy executor, with an
  engine-equivalence test. End-to-end tests on a generated nested event pin
  the rest of the Phase N2 checklist against hand computations: the child
  ARD is keyed by (arm, SOC, PT) through the standard {cards} `by` path,
  duplicate records collapse to one subject, and percentage denominators
  are subjects per arm from the ADSL population (a non-population subject
  is excluded from N).

* **Nested two-level row hierarchies, generation side** (nested SOC/PT
  handoff, Phase N1). The shell generator now recognises the nested
  token-block pattern AE/MH/ConMed shells author -- a data-driven parent
  token row ("<System Organ Class>", `ADAE.AESOC`) followed by child token
  rows on a second variable of the same dataset ("<Preferred Term>",
  `ADAE.AEDECOD`), repeating as further mock examples -- and stops
  flattening it into unrelated analyses. The first parent/child pair now
  becomes two linked analyses: both count DISTINCT subjects
  (`MTH_AE_FREQUENCY_COUNT`, previously record-counting
  count-and-percentage), and the child carries the parent's variable as a
  new data-driven row grouping in `orderedGroupings` -- standard ARS
  vocabulary, so {cards}, siera and the ARD engine all see the hierarchy.
  The authored skeleton records the pair as `nested_parent` /
  `nested_child` layout rows linked by `parent_order`; template repeats
  collapse with an INFO diagnostic. Separately, the previously ignored
  `once/subject VAR` annotation clause now routes a count row to the
  distinct-subject method. Rendering still shows the two blocks unmerged --
  the interleaved SOC-then-its-PTs presentation is Phase N3 (renderer).

* **Shell-faithful review view** (shell-faithful editor handoff, Phases 1-2).
  The Details tab of `view_ars()` / `edit_ars()` now opens an output as the
  table the reviewer actually thinks in: a centered title band, the
  population line, the shell's column header, and every authored row from
  `_meta.shell_layout` -- including the section headers, stat lines and
  category rows that own no analysis and were previously invisible -- in
  shell order and indentation, with `xx.x (x.xx)`-style placeholders in the
  body cells. Clicking a row highlights it and opens its owning analysis in
  a panel directly beneath the full-width table (the full edit form in edit
  mode, the read-only detail in view mode), so a correction never replaces
  the table being reviewed; rows without an analysis explain themselves
  instead. Every analysis panel now carries a "Back to the output table"
  link, so selecting a line from the tree or the bottom list no longer
  strands the reviewer without a way back. Clicking the column header opens
  a read-only panel naming the grouping entities that produce the columns
  (variable, groups, shared-by count) and where each piece is edited --
  deliberately not an edit surface, since the column axis is shared state
  with no single safe write target from one table's header. Both that panel
  and the analysis form's shared-entity notice now carry an "Edit in
  Entities" jump that switches to the Entities tab and selects the entity's
  row, so shared definitions (groupings, populations, subsets, methods) are
  one click from wherever they are encountered.
  Built on a new pure builder (`.shell_table_data()` in `R/shell_table.R`)
  that reuses the renderer's `.shell_layout()` parser and mirrors its
  block-consumption rule for row ownership. Outputs without a layout
  (listings, figures, older events) fall back to one row per referenced
  analysis. The old metadata block moved into a collapsed "Output
  properties" section; the analysis reorder list is unchanged. No new
  dependencies, no renderer changes.

* **Compound-clause awareness in the supplement merge** (render-fidelity
  handoff, Phase 1: RC-1 + RC-2). Two build paths inspected only simple
  (single-condition) where-clauses while the compound machinery already
  existed and worked; a supplement that declared leaf rows as
  `AND(TRTEMFL='Y', ASEV='MILD')` therefore rendered every categorical
  level twice, and 28 of 43 analyses on the incident study ran with no row
  filter at all (an unfiltered `ard_categorical()` whose full value
  distribution then deparsed into single cells as `c(" 69", " 0")`).
  Level-row detection now sees through a compound AND via the new
  `.where_leaf_on()` helper -- the single EQ term on the parent block's own
  variable is extracted, so clause *shape* no longer decides whether an
  authored row is a level. And `emit_extra_analysis()` (conflict
  secondaries, free-standing supplement rows) now receives the typed
  whereClause instead of re-parsing the annotation string, so a compound
  clause builds the same `compoundExpression` DataSubset the primary row
  path already produced. A new WARN diagnostic surfaces any extra analysis
  whose declared filter could not be parsed into a DataSubset, so the next
  occurrence lands in the validation report instead of a rendered cell.

* **Annotation corruption guardrails.** Four independent fixes closing gaps
  surfaced by a real hand-edited annotation that arsbridge's own validation
  passed as clean. `validate_annotations_spec()` now cross-checks the
  *value* bound to a DATASET.VARIABLE reference against the spec's declared
  length and codelist, not just that the variable exists -- so a
  seriousness flag bound to a multi-character severity term (right variable,
  wrong literal) is now a FAIL instead of a silent PASS. `parse_shell_docx()`
  lints each programmer annotation for authoring corruption -- typographic
  quotes and mid-annotation colour drift, both visual signs a line was
  hand-edited/pasted over rather than typed fresh -- and WARNs through the
  existing diagnostics stream. `ars_validate_supplement()` strips ANSI/OSC-8
  escape codes from caught condition messages before they reach a FAIL/WARN
  finding, so a parse error never renders as literal escape-code soup in the
  Shiny repair-prompt panel. The two-phase Copilot instruction files now warn
  against pasting quoted annotation text into a JSON string unescaped, the
  gap that let a chat assistant's own repair-prompt explanation break its
  own JSON in the incident that surfaced all of this.

* **`ars_workflow()`: a guided app for the whole journey.** One stepper
  walks a study project from an annotated shell to a reviewed reporting
  event: project setup (fixed folder layout -- `copilot/`, `phase1/`,
  `phase2/`, `ars/` -- plus a small `arsbridge_project.json`), writing the
  two-phase Copilot instruction files, the two manual assistant round-trips
  (paste or upload the replies straight from the chat; a new blueprint
  pre-flight catches truncated or wrong-version files before they cost a
  Phase 2 session, and `ars_validate_supplement()` runs the moment
  `supplement.json` lands, with the repair prompt shown on FAILs), the
  `spec_to_ars()` build (supplement-driven, or deterministic when the
  Copilot phases are skipped), and a hand-off into `edit_ars()` -- when the
  editor closes, the workflow returns with every status re-derived from the
  files on disk, so closing the app never loses progress. Launchable
  standalone via `inst/shiny/ars_workflow/`. In passing, the shipped
  Copilot instruction files' stale "format version 3" doc-control strings
  were corrected to version 4.

* **Declared result-group paths in the ARS JSON (`resultGroupPaths`).** An
  output built from a hierarchical column tree now declares its result
  columns explicitly: one path per display column, in shell order, each
  referencing the standard group levels whose conditions compose it. The
  grouping factors themselves stay fully standard (one flat factor per
  header level, all linked from every analysis through `orderedGroupings`,
  outermost first) -- the extension only records WHICH combinations are
  valid, because ARS v1.0 has no valid-path construct and an ordered
  grouping list alone would invite Cartesian products. A SUBTOTAL path
  references only its parent's group (`totalStrategy: condition_based`); a
  GRAND_TOTAL path references no group (`totalStrategy: analysis_set`) and
  replaces the `includeTotal` boolean for these outputs. `ars_conformance()`
  strips the documented extension; the parsed column tree travels in the
  output's `_meta` as provenance. `validate_ars_model()` gains structural
  checks keyed by machine-readable codes (`DISPLAY_COLUMN_COUNT_MISMATCH`,
  `UNMAPPED_LEAF_COLUMN`, `INVALID_CARTESIAN_PRODUCT`,
  `GROUPING_VARIABLE_NOT_LINKED`, `SUBTOTAL_SCOPE_UNDEFINED`,
  `DUPLICATE_RESULT_PATH`, `GROUPING_ORDER_AMBIGUOUS`,
  `HEADER_TREE_MISSING`, `SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES`), so a
  structurally damaged event fails review before any ARD is computed.

* **Path-aware rendering.** `ars_to_tfrmt()` recognizes a declared-path ARD:
  the column axis is the stable `result_group_path` ("Cohort A > Mild", ...,
  "Total"), one display column per declared path, locked to shell order via
  `result_group_order`; the per-level `group*_level` columns are treated as
  the same column identity, never as row groups. Spanning parent headers
  (a two-level `tfrmt::span_structure`) are deliberately deferred to a
  visual-QA cycle -- the flat "parent > child" labels carry the full
  identity meanwhile.

* **LLM enrichment understands the header tree.** When the parser found a
  hierarchical header, the enrichment payload includes the parsed tree and
  the response schema gains an optional `column_hierarchy` answer. Hard
  grounding: the model may only reclassify a column's role
  (DETAIL/SUBTOTAL/GRAND_TOTAL) or name the grouping variable of an
  unannotated level -- an answer describing different columns is discarded
  whole, and a variable outside the ADaM spec is ignored, both with WARNs.
  Geometry is parser truth; the LLM never redraws it.

* **Saving and leaving the editor are now separate decisions.** The review
  stage gains **Save** (writes to the opened file and stays in the editor,
  after a confirmation showing the destination path, the change summary, and
  the validation status; the previous file is backed up first and the dirty
  state resets) and **Save As** (writes a timestamped copy elsewhere; the
  original file and the editing session are untouched). *Save and close* and
  *Discard* keep their existing behavior, and every save modal now names the
  exact file it will touch.

* **Bulk grouping assignment.** One table usually shares one column layout,
  so the analysis panel gains "Apply this line's groupings to every line in
  this output": a preview modal lists the affected lines, the ordered
  `Grouped by` set is copied across in one action, one undo reverses all of
  it, and each changed line is recorded in the edit log.

* **Grouping lifecycle in the entity library.** The Groupings pool gains
  Add (spec-gated dataset/variable choices, starts data-driven), Clone, and
  Delete. Delete is refused while any analysis still references the grouping
  -- the dependent analysis lines are named, so unassign-then-delete is
  explicit -- and a grouping's detail view now lists the analyses that use
  it, not just the count.

* **A "Columns" panel in the review stage.** `view_ars()` / `edit_ars()` gain
  a tab that shows the parsed header tree (node type badges, N hints, raw
  header annotations), the declared result paths in shell order, and the
  structural warnings for the selected output. In edit mode a path's `role`
  and `totalStrategy` are editable in place -- the one judgment a reviewer
  must make deliberately is a subtotal's scope (parent condition, which
  includes unknown-category subjects, vs the sum of the displayed children)
  -- with full history/undo, edit-log, and re-validation like every other
  edit. Deeper surgery (adding/removing paths, changing conditions) stays in
  the raw-JSON escape hatch by design.

* **Declared-path execution.** For an output carrying `resultGroupPaths`,
  `ars_to_ard()` and the emitted `{cards}` deliverable scripts compute one
  pass per declared result column instead of crossing the grouping
  variables: each pass filters both the analysis frame and the denominator
  frame by the path's composed condition and runs the method idiom
  ungrouped, so a subtotal's N is the parent-condition count (unknown child
  categories included), the grand total's N the analysis-set count, and an
  undeclared combination (a sibling cohort crossed with another parent's
  child levels) can never appear in the ARD. Each row carries its display
  identity -- the per-level `group*`/`group*_level` columns plus stable
  `result_group_id` / `result_group_path` / `result_group_order` /
  `result_group_level` fields. The emitted script IS the executor (the
  emitted == executed invariant is unchanged), and a declaration that does
  not resolve blocks the whole output with a FAIL instead of silently
  degrading to flat groupings. Flat and crossed tables execute exactly as
  before.

* **Condition-defined `groups[]` now ship in the official ARS v1.0 Group
  shape.** Per the standard, a Group IS-A WhereClause: it carries `level`
  and `order`, with its `condition` (or `compoundExpression`) directly on
  the node. arsbridge previously emitted its internal wrapped WhereClause
  under `condition`, which the official schema rejects -- a gap the
  conformance suite never saw because no conformance fixture used
  condition-defined groups. Emission is now official at the boundary
  (`.official_group()`), and internal consumers read both shapes through
  one accessor (`.group_where()`), so existing fixtures and supplements
  keep working.

* **Supplement format v4: `columnHierarchy`.** A supplement TLF entry can now
  declare the hierarchical column tree explicitly -- one typed node per
  display column or spanning parent (`GROUP`/`LEAF`/`SUBTOTAL`/`GRAND_TOTAL`,
  each with its own condition), mirroring what the parser infers from header
  geometry. The declared tree wins over the parsed one, passes the same hard
  ADaM-spec gate (one bad node rejects the whole hierarchy -- a partial tree
  would silently drop display columns), and `ars_validate_supplement()`
  pre-flights the structure: the `includeTotal`/`columnHierarchy` conflict,
  node completeness, parent references, and every node condition. v4 is the
  single supported version (early-phase policy: no readers for older
  arsbridge formats); the shipped schema and both Copilot instruction files
  are updated, and a v2/v3 file fails loudly with a regenerate message.

* **Hierarchical and asymmetric column headers parse into an explicit column
  tree.** The shell parser now retains the raw header-cell geometry (grid
  spans, vertical merges) instead of only the flattened per-column labels,
  and builds `sec$column_tree`: every visible result column becomes one
  declared path with a composed condition. A parent column spanning
  conditioned child columns (with an optional per-parent subtotal), a
  sibling column with no children, and an overall Total therefore all
  survive parsing as first-class structure -- the child grouping variable is
  no longer dropped with a "several variables" warning. A subtotal column's
  condition is the parent's condition by construction (not the union of the
  displayed children), so a subtotal that includes unknown-category subjects
  computes correctly. Detection is condition-driven: spanned headers over
  statistic sub-columns ("n" / "(%)") and classic one-row conditioned
  headers keep the existing flat single-axis behavior exactly.

* **WhereClause algebra helpers** (internal groundwork for hierarchical
  column groupings): `combine_conditions()` composes clauses under AND/OR
  with NULL-tolerant, same-operator flattening; `canonicalize_condition()`
  produces an order-insensitive, case-normalized canonical form;
  `conditions_equal()` compares two clauses structurally; and
  `condition_implies(child, parent)` answers whether a child column's
  condition sits inside its parent's scope (conservative FALSE when a
  clause contains OR). These let a hierarchical column tree build a leaf
  condition as AND(ancestor conditions, own condition) and assert that a
  parent subtotal's condition is exactly the parent condition.

* **A freshly generated reporting event now validates clean against the
  official CDISC ARS v1.0 schema** (beyond the documented extensions, which
  `ars_conformance()` strips and reports). All six known divergences are
  closed:
  - Every analysis carries the required `reason` and `purpose` controlled
    terminology. The run-level defaults -- `"SPECIFIED IN SAP"` and
    `"EXPLORATORY OUTCOME MEASURE"` -- are new `spec_to_ars()` arguments
    (`analysis_reason` / `analysis_purpose`, validated against the closed
    CDISC vocabularies), and both fields are editable per line in
    `edit_ars()`, which is where the handful of endpoint tables get their
    `PRIMARY`/`SECONDARY` purpose corrected.
  - `version` fields are integers, as required.
  - Displays are written in the official `OrderedDisplay{order, display}`
    wrapper with their own `id`/`name`, and footnotes as
    `orderedSubSections[]` wrapping `subSection{id, text}`.
  - `fileSpecifications[].fileType` is a terminology object.
  - The self-referential operation-role placeholders carry a valid term
    (`NUMERATOR`) instead of `""`.
  - Contents-list analysis entries are named.

  siera compatibility was verified by running `siera::readARS()` on the same
  event in both shapes: identical output. (siera reads none of the changed
  fields by position, and the new keys are additive.)

* **No compatibility readers for arsbridge's own earlier output shapes.**
  Early-phase policy: when a correction changes the emitted JSON, the old
  shape is not carried. Readers target the official shape only -- an
  unrecognized shape reads as unset rather than crashing or being misread,
  and the round trip preserves what it does not understand rather than
  guessing at it. The remedy for an outdated file is regenerating it with
  `spec_to_ars()`, and `ars_conformance()` names exactly what is wrong with
  it. The bundled minimal fixture is now official-shaped too.

* **`ars_conformance()` validates a reporting event against the official
  CDISC ARS v1.0 JSON Schema.** The schema ships with the package, pinned to
  the `v1.0.0` release of `cdisc-org/analysis-results-standard` alongside its
  LinkML source (`inst/schema/`, provenance in the README there), so the
  answer never changes because the standard's development branch did.
  arsbridge's documented extensions -- `_meta`, `referencedAnalysisIds`, the
  nested `analysisVariable` duplicate and friends -- are stripped before
  validating and reported in an attribute, so the findings show genuine
  divergences instead of burying them under sanctioned fields. The
  generator's known gaps (missing `reason`/`purpose` terminology on analyses,
  string `version`s, flat display objects, string `fileType`s, placeholder
  operation roles) are reported honestly and pinned by tests, and every save
  from `edit_ars()` prints a one-line conformance count. This resolves the
  open question of *which* schema to validate against: the LinkML model is
  the source of truth, vendored at `inst/schema/cdisc_ars_v1.0.0_ldm.yaml`.

* **The editor no longer goes quietly stale around structural changes.**
  Moving a line now visibly reorders the panel, applying a raw-JSON
  replacement refreshes the fields it changed, the outputs tree keeps its
  open panels across edits instead of collapsing, and the entity-library
  tables update their data in place so row selection survives the very edit
  it enabled. Same family as the undo-display fix: the model changed and the
  screen did not.

* **Writing to a derived model column is now refused instead of silently
  reverted.** Columns computed from the node (`output_id`, `n_analyses`,
  `condition_summary`, ...) used to accept a write that the next refresh
  undid; `model_set_field()` now says so instead.

* **A review session can no longer be lost.** Every change is undoable
  (and redoable) with the arrows in the header -- field edits, added and
  removed lines, reordering, detaching, library edits, all of it. And every
  change is written to a crash-recovery copy in the user's cache directory, so
  a browser or an R session that dies mid-review offers the work back when the
  editor is next opened on the same file. The file being edited is never
  touched until an explicit save, and the recovery copy is cleared once the
  work is safely on disk.

* **Shared entities are editable from the library.** Methods, analysis sets,
  data subsets and groupings can now be corrected in the Entities tab, which
  is the right place to fix a population that is wrong everywhere rather than
  opening thirty analyses. The panel says how many analyses a change will
  affect, and points at detaching when the intent is to change one line only.
  Nested shapes the flat fields cannot express -- compound conditions,
  grouping levels -- have a raw-JSON escape hatch that refuses anything
  invalid instead of applying it.

* **`export_edit_log()` turns a review session into a QC record.** The sidecar
  `<name>.edits.json` becomes a styled workbook: what changed, from what to
  what, who saved it and when. Repeated edits to one field collapse to a
  single before/after row, and a field edited back to where it started does
  not appear at all. The ARS JSON still carries no provenance fields, so the
  deliverable stays CDISC-conformant.

* **`review_ars()`** is an alias for `edit_ars()`, for whichever framing fits.

* **Fixed: the raw-JSON escape hatch silently discarded the edit it reported
  applying.** Replacing an entity's node re-derived its row by patching the
  new node from the row's *old* column values, which undid the replacement.
  Node-replacing edits now treat the node as authoritative.

* **The lines the generator missed can now be added, and a gap tells you
  exactly which one.** Selecting a coverage finding -- "the shell annotates
  this but no analysis uses that variable" -- opens the add-analysis wizard
  pre-filled with the dataset and variable the shell named, at the output it
  belongs to. The wizard is reuse-first by construction: method, population,
  data subset and groupings all default to what the output's other lines
  already use, so the normal outcome of adding a line is that no new shared
  entity appears and the event stays readable. Display order is meaningful, so
  the line can be inserted at a chosen position, and an output's lines can be
  reordered or removed.

  An added line is indistinguishable from a generated one: the same node
  shape, the same `AN_<TLF>_<nnn>` id convention (collision-checked), the same
  self-referential operation placeholders siera needs. The tables of contents
  rebuild themselves, because they were already derived from the outputs.
  Adding a line and then removing it restores the event byte for byte.

* **A shared population, data subset or grouping can be detached for one
  analysis.** Editing a population used by thirty analyses changes all thirty;
  detaching copies it under a new id and repoints only this line, so it can
  then be changed on its own. Methods deliberately cannot be detached: the
  engine dispatches on the method id, so a per-analysis copy would have no
  executor and would quietly degrade a computed line into a generic summary.
  Changing which method one line uses is what the method dropdown is for.

* **`validate_ars_model()` findings gained a `ref` column** carrying what the
  finding is about in machine-readable form, which is what lets a coverage gap
  turn into a pre-filled wizard rather than a re-typing exercise.

* **`edit_ars()` closes the loop: generate, review and correct, then execute.**
  The same structured viewer, with the detail panels editable. Methods,
  populations, data subsets and groupings are chosen from what actually
  exists -- the entities in the file, the methods the engine can execute
  (labelled with what it will do with each: computed, needs a prerequisite,
  reserved for manual computation), and, with the ADaM spec supplied, the
  variables the study really has. Choosing a standard method the file does not
  carry adds it first, so an analysis can never point at a method that is not
  there. Every dropdown says how many analyses share the entity, because
  editing a shared method edits all of them.

  Nothing is written until you save, and saving shows a from/to table of what
  changed first. The previous file is backed up to `<name>.json.bak-<time>`,
  the new content is written to a temporary file in the same directory and
  renamed into place so an interrupted save cannot destroy the file it was
  replacing, and the edit log is written to `<name>.edits.json` beside it --
  keeping provenance out of the ARS JSON so the deliverable stays CDISC-clean.
  `spec_to_ars()` now also returns `adam_spec_path`, so `edit_ars(result)`
  wires up spec-driven dropdowns and gap detection on its own.

* **`view_ars()` opens a reporting event as the structure a programmer already
  recognises.** Each output is a collapsible panel with its analysis lines
  beneath it -- the shell's skeleton, read straight from the standard's
  `mainListOfContents` -- and validation findings are overlaid as badges, so
  the displays needing attention are visible before anything is opened.
  Selecting a line resolves its ids into what they mean: the method's name
  plus whether the engine can actually execute it, the population's condition,
  the variables results are grouped by. A shared-entity library shows how many
  analyses each method, analysis set, data subset and grouping is used by,
  which is the thing a flat JSON view hides. The viewer never writes anything.
  `shiny`, `bslib` and `DT` are Suggests, so the package is unaffected when
  they are not installed.

* **An ARS reporting event can now be read as editable tables and written
  back losslessly.** `ars_to_model()` turns the nested JSON into one data
  frame per entity pool (analyses, methods, analysis sets, data subsets,
  groupings, outputs), each row carrying the flat fields a reviewer edits
  plus the original untouched node; `model_to_ars()` is its exact inverse.
  Fields the model does not surface -- including `_meta` and any future ARS
  key -- ride along untouched, so an unedited model round-trips to a
  structurally identical event and an edited one differs only where it was
  edited. The two tables of contents are copied verbatim unless a structural
  change means they have to be rebuilt from the outputs. This is the
  foundation of the human review stage between `spec_to_ars()` and
  `ars_to_ard()`.

* **`validate_ars_model()` checks a reporting event for the problems that
  matter before execution.** Every reference resolves (an empty
  `dataSubsetId` correctly means "no subset"), no id is duplicated, and each
  analysis is classified by how `ars_to_ard()` will actually treat its method
  -- computed natively, dependent on a prerequisite, silently falling back to
  the generic summarizer, or reserved for manual computation. Given the ADaM
  spec it also checks datasets and variables exist, and given the annotation
  validation report it reports annotated shell lines that no analysis
  covers -- the lines the generator missed.

* **Spec codelists decode coded categorical variables end to end.**
  `parse_adam_spec()` now reads the spec workbook's Codelists sheet (both the
  `"Codelist Name" / "Term (Code)" / "Decoded Value"` and the
  `"ID" / "Term" / "Order" / "Decoded Value"` header conventions, with
  merged-cell fill-down and a `Used By Variables` fallback link) and, for
  define.xml input, the `CodeList` / `CodeListRef` nodes. The parsed
  codelists ship in the ARS JSON as `_meta$value_decodes`, and both the
  execution engine and the emitted {cards} deliverable derive the coded
  analysis variable as a factor (`factor(as.character(VAR), levels = codes,
  labels = decodes)`) before `cards::ard_categorical()`. The ARD therefore
  shows decoded labels ("DEATH") instead of raw codes ("1"), keeps codelist
  order, and reports EVERY codelist term -- unobserved categories appear
  with n = 0, matching disposition shells that list all reasons. Authored
  level rows (`ADSL.DCSREASN=1` under the parent) are stamped with the
  decoded label (raw code kept as `level_code`) so renderer level matching
  is unaffected. Codelists larger than 15 terms (e.g. COUNTRY) are skipped
  with a WARN so tables never explode into hundreds of zero rows.

* **Column groups fall back to the spec codelist.** When a grouping
  variable's column headers carry no condition annotations (the
  `GF_<VAR>` `groups: []` case that rendered coded column labels), the
  grouping factor's per-level groups are now derived from the variable's
  codelist -- decoded label, `EQ` term condition, codelist order -- with a
  WARN asking for review. Header-annotated groups always win.

* **BREAKING: supplement format version 3 -- typed CDISC ARS conditions.**
  The no-API supplement now carries every filter, population, and column
  condition as a typed ARS `WhereClause` object
  (`{condition: {dataset, variable, comparator, value}}` /
  `{compoundExpression: {logicalOperator, whereClauses}}`) instead of a string.
  This ends the string-parsing fragility (double `=`, smart quotes, `OR`,
  `="..."` value repair) that caused real-world extraction failures. Per-TLF
  entries gain typed `analysisSet`, `groupings` (with typed group conditions),
  `analyses` (was `bindings`; `variable` is now a `{dataset, variable}` object
  and the filter a typed `whereClause`), plus `listingColumns`, `recordFilter`,
  `sorting`, per-row `methodId`/`parentRowLabel`/`denominator`, `anchors`, and
  `provenance`. `read_supplement()` accepts only `supplement_version` 3 and
  aborts loudly on a v2 file -- regenerate with `ars_copilot_instructions()`.
  A JSON Schema (`inst/schema/arsbridge_supplement_v3.schema.json`) ships with
  the package, is uploaded to the assistant for self-checking, and is used by
  `ars_validate_supplement()` when `jsonvalidate` (new Suggests) is installed.

* **`spec_to_ars(supplement_trust=)` -- configurable conflict resolution.**
  `"fill_gaps"` (default, unchanged) lands a supplement value only where the
  regex left a gap; `"prefer_supplement"` lets a validated, spec-gated
  supplement value override the shell on a conflict, with a WARN recording both
  and the shell original kept as a secondary analysis. The hard ADaM-spec gate
  is never bypassed in either mode. The mode is recorded at
  `_meta.supplement_trust`.

* **Packaged two-phase Copilot workflow.**
  `ars_copilot_instructions(workflow = "two_phase")` writes a Phase-1
  (evidence blueprint) and Phase-2 (semantic construction + repair) instruction
  set for large or complex shells, alongside the single-file workflow. Both
  emit supplement version 3 and are shipped in step with the reader, so the
  instructions and the accepted format can no longer diverge. Each instruction
  file opens with a **"How to run this"** block -- the operator steps (which
  files to attach, what to save) plus a paste-ready prompt for the chat.
  `ars_copilot_instructions()` now returns the vector of paths it wrote.

* **`ars_validate_supplement()` rewritten for v3** with typed-condition checks,
  comparator/enum/arity validation, parent-row resolution, and a paste-ready
  `repair_prompt` attribute that bundles every FAIL for the assistant.

* **Native SAS `.sas7bdat` ADaM cuts are now read everywhere.** The per-TLF
  standalone `{cards}` scripts emitted by `write_tlf_code()`, the execution
  engine (`ars_to_ard()`), and the listing/figure renderers previously loaded
  only `.xpt` and `.csv`; they now also match and read `.sas7bdat` via
  `haven::read_sas()`. Loaders remain case-insensitive on the dataset name, and
  when several formats of the same dataset are present the native SAS formats
  (`.xpt`, then `.sas7bdat`) are preferred over `.csv`.

* **Rewritten Copilot instruction file (extraction guidance version 3).** The
  file `ars_copilot_instructions()` writes is substantially expanded to make a
  chat assistant classify each annotation before emitting JSON: a mandatory
  role taxonomy (population / result-column / displayed-row / listing-column /
  supporting-filter / programming-note), Word label reconstruction rules,
  footnote-mapping resolution, data-type-aware literal quoting, TLF inventory
  reconciliation against the Table of Contents, and per-TLF and final
  validation checklists. The `column_groups` fallback (format version 2) is
  folded into the new column-structure guidance: the assistant supplies
  per-column conditions there only when the shell headers carry no
  machine-readable `DATASET.VARIABLE=value` filter, each `where` written with
  the full dataset prefix. All examples use generic ADaM datasets/variables and
  generic TLF labels.

* **Supplement format version 2: a `column_groups` field.** In the no-API
  supplement workflow, a table whose result columns are values of one variable
  (cohort columns keyed on `ADSL.COHORTN`) could not be expressed when the shell
  header cells carried no machine-readable `DATASET.VAR=value` filter -- the
  format had nowhere to hold the per-column conditions, so the grouping shipped
  with an empty `groups[]`. Each TLF entry may now carry an ordered
  `column_groups` array of `{label, where}` objects; `where` is a full
  condition (`ADSL.COHORTN=1`, `is.na(ADSL.COHORTN)`) using the same grammar as
  an annotated header. `spec_to_ars(supplement = ...)` feeds these into the
  existing group builder, so each becomes one display column (including a
  missing/Unknown bucket) with a WhereClause in the ARS JSON. The shell's own
  header-annotation path still wins when it captured the columns itself; every
  `where` passes the ADaM-spec gate; `ars_validate_supplement()` checks the new
  field. The instruction file `ars_copilot_instructions()` writes is updated to
  version 2 and now tells the assistant to put column conditions here, never to
  fold them into `bindings`. Existing version-1 supplements must be regenerated.

* **Column-header annotations now parse the `is.na()` / `missing()` call
  forms** that annotated shells actually use for a missing/Unknown group --
  R's `is.na(ADSL.COHORTN)` and SAS's `missing(COHORTN)`, plus the negations
  `!is.na(...)` / `not missing(...)`. Previously only the prose
  `DATASET.VAR is missing` form was recognized, so a call-form Unknown-cohort
  header silently failed to parse and its column vanished from the axis. A
  companion coverage check now WARNs when a header names the column-axis
  variable but its annotation does not parse into a condition, reporting how
  many columns were captured versus expected -- so a narrowed axis is
  surfaced rather than shipped quietly.

* **Annotation-defined column axis: per-column filters in table header
  cells.** When two or more column headers carry a filter on the same
  variable -- `Cohort A (N=XX) ADSL.COHORTN=1`, `Cohort B (N=XX)
  ADSL.COHORTN=2`, `Unknown Cohort (N=XX) ADSL.COHORTN is missing` -- each
  condition now becomes one display column, in shell order. This makes a
  merged/derived column (an "Unknown" bucket collecting missing values)
  expressible purely by annotation, with no ADaM change: the engine derives
  the grouping in memory from the conditions, identically in the executed
  ARD and the emitted `{cards}` scripts (a `case_when` factor built from the
  same where-clause predicates). The conditions are carried in the ARS JSON
  as per-level `groups[]` entries with WhereClauses. Rows matching no column
  are excluded from the group columns and counted in a WARN; a
  `Total (N=XX) ...` header is recognized as the overall column and switches
  `includeTotal` on. The annotation grammar also gains the positive
  `DATASET.VAR is missing` / `is null` form and parenthesized
  `IN ('a','b')` value lists.

* **The Copilot instruction file now asks for a downloadable `supplement.json`
  file** (written programmatically with a real JSON serializer) rather than an
  on-screen block, with the fenced block kept as a fallback. Delivering a
  serialized file avoids the copy-paste smart-quote / stray-double-quote /
  truncation errors that a pasted chat block introduces.

* **A supplement with double-quoted where-clause values now loads instead of
  aborting.** The most common Copilot mistake -- a comparison value quoted with
  double quotes (`MHSCAT="UNDERLYING CONDITIONS"`), which breaks the JSON --
  is now auto-repaired to single quotes before parsing. In valid JSON a `"` is
  never preceded by `=`, so `="..."` can only be a value comparison, making the
  rewrite safe; escaped, already-valid quotes are left untouched. The
  instruction file still asks for single quotes, and `read_supplement()`'s error
  still names the fix for any malformation the repair cannot cover.

* **The validation report now carries a `Legend` sheet.** `spec_validation_report.xlsx`
  gains a final worksheet that names each status/severity, its meaning, and the
  exact fill hex it is tinted with (PASS `E2EFDA`, WARN `FFF2CC`, FAIL `FCE4D6`,
  INFO `DDEBF7`). The same legend is documented in the README. The tint palette
  is now a single constant so the key can never drift from the report.

* **The Copilot supplement workflow is hardened against invalid JSON.** The most
  common failure -- a value quoted with double quotes (e.g.
  `MHSCAT="UNDERLYING CONDITIONS"`), which breaks the JSON -- is now called out
  explicitly in the instruction file (single quotes inside every value, plus a
  "before you send" self-check), and `read_supplement()`'s error now names that
  cause and the single-quote fix.

* **The supplement now confirms the correct set of tables.** A Copilot
  supplement may carry a `title` per TLF (the instruction file now asks the
  assistant to enumerate every output with its exact title). `spec_to_ars()`
  cross-checks that inventory against what it parsed and records non-blocking
  WARNs for a supplement entry that matches no parsed table, a parsed table the
  supplement never mentions, and a title that disagrees between the two -- so a
  wrong or incomplete table set surfaces for review. When the shell heading
  gave no title but the supplement has one, the parsed section adopts it
  (INFO). `title` is optional and backward compatible (no version bump); a
  supplement without one still runs, and `ars_validate_supplement()` suggests
  adding it.

* **One-line TLF headings are now read deterministically.** The shell parser
  previously recognised an inline title only after a literal colon
  (`Table 14.1.1: Title`). It now also reads a colon-less one-line heading
  that packs the number, title, a dash-separated population, an inline
  annotation, and a `[PROGRAMMING DATASETS USED: ...]` suffix into a single
  paragraph -- e.g.
  `Table 14.1.1 Summary of Disposition - Screened Subjects ADSL.SCRNFL='Y' [PROGRAMMING DATASETS USED: ADSL]`.
  The title, population, population annotation, and source datasets are split
  out of that line. Recognition stays conservative: ordinary prose that
  mentions a table number (`Table 14.1.1 shows the summary`), cross-references
  (`See Table 14.1.1 ...`), table-of-contents entries, and bare section
  numbers (`14.1 Demographic and Baseline Tables`) are still not headings.

* Annotation values written with straight or smart **double quotes**
  (`ADSL.SCRNFL="Y"`) and **unquoted numeric equality** (`ADSL.COHORTN=1`,
  common in column headers) are now detected. Captured values are
  canonicalized to single quotes so the emitted ARS JSON stays uniform
  regardless of the shell's quote style. Text is Unicode-normalized before
  matching (non-breaking spaces, zero-width characters, and smart quotes),
  while en/em dashes are preserved as meaningful title separators.

* New `spec_to_ars(heading_patterns = ...)` escape hatch: a character vector
  of PCRE patterns (with named `number`/`type`/`title` groups) tried before
  the built-in grammars, for sponsor shells whose headings the built-ins do
  not recognise -- no package edit required.

* When no TLF sections are found, the warning and the `spec_to_ars()` abort
  now list the heading-shaped lines that were seen and rejected, with the
  reason for each, and repeat a one-line recommendation for how to write an
  identifiable heading before pointing at `heading_patterns`.

* New WARN when a heading's number is found but **no title text** is
  identified (e.g. a bare `Table 14.1.1` with the title stranded in a text
  box): the section is kept but flagged with the same how-to-write-an-
  identifiable-heading guidance, so a missing title is surfaced rather than
  shipped silently.

* Documented the recommended heading convention in one place --
  `?spec_to_ars` gains a "Writing identifiable TLF headings" section, and the
  README gains a "TLF heading format" section -- so the guidance the error
  and warning messages give matches the docs.

* The cosmetic "Undefined namespace prefix" warning that `officer` emits while
  reading `docProps/core.xml` in some e-signed (DocuSign) shells is now
  muffled; every other warning still surfaces.

# arsbridge 0.1.0

* **The LLM tier is now opt-in.** `spec_to_ars()` gains `use_llm` (default
  `FALSE`): by default the pipeline runs regex-only (deterministic) and makes
  NO live LLM call, *even when an API key is configured*. Pass `use_llm = TRUE`
  to use the LLM for extraction and enrichment when a key is available. This
  makes regex the first-class default and the LLM an explicit choice -- ideal
  for CI, automation, and regex baselines. (A `supplement` still takes
  precedence; it also makes no live LLM calls.) **Breaking:** callers that
  relied on a configured key auto-selecting the LLM must now pass
  `use_llm = TRUE`.

* Deterministic (regex) and supplement (Copilot) runs are fully silent about
  API keys: `spec_to_ars()` never asks for a key nor raises a key-related
  error or warning in those modes. The old "running in
  deterministic mode" WARN is now a neutral INFO provenance note, and the
  "no API key?" console nudge is gone. Genuine, table-specific findings
  (e.g. a capability blocker for an inferential table) are unaffected and
  still surface in every mode.

* Three-tier reading engine; the LLM API key is now optional
  (`R/spec_to_ars.R`, `R/supplement.R`). `spec_to_ars()` no longer aborts
  without a key: it resolves a mode from what you have —
  * **deterministic** (shell + spec only): regex + keyword heuristics, one
    `WARN` recording the reduced accuracy;
  * **supplement** (`spec_to_ars(supplement = "supplement.json")`): a JSON
    file produced by a chat assistant from the uploaded shell + spec fills
    the annotations the regex could not find and supplies per-TLF
    enrichment, with **no API call**;
  * **llm** (API key set): unchanged live behaviour.

  New exports: `ars_copilot_instructions()` writes the static, versioned
  instruction file to upload to Copilot/ChatGPT alongside the shell and
  spec; `ars_validate_supplement()` pre-flights the reply. Supplement
  bindings fill gaps only — authored shell annotations win any disagreement
  (`WARN`) — and every proposed variable passes the same hard ADaM-spec gate
  as a live LLM proposal. The tier is recorded in
  `_meta.extraction_mode` of the ARS JSON and in the `spec_to_ars()` result.
  See `vignette("no-api-access")`.

  `ars_copilot_instructions()` copies the instruction file shipped inside the
  installed package (`inst/copilot/`) into the working directory (creating the
  target folder if needed), so users never touch the internal package path.
  The no-API path is now cross-referenced from `?arsbridge`, `?spec_to_ars`,
  every `?set_*_key` / `?get_active_llm` help page, the README (including an
  install-time pointer), and the `getting-started` vignette.

* Shell-parsing robustness for cross-sponsor variation
  (`R/parse_shell_docx.R`, robustness findings F1-F4). The shell reader now
  tolerates inline headings (`Table 14.1.1: Title`), two-line titles, listings
  with no population line, page-header-stored titles/populations,
  `gridSpan`/`vMerge` merged cells and multi-row headers, and Word comments,
  highlights, tracked changes, and text boxes as annotation channels.
  Pre-merge hardening:
  - A page-header title/population is adopted only when the header's TLF
    number matches the body section's; a mismatch (stale template header,
    or a header belonging to another TLF) is refused with a WARN instead of
    silently mislabelling the section.
  - A multi-row nested header with no `<w:tblHeader/>` flag is inferred from
    the spanned first row (so the subcolumn labels survive and no ghost stub
    row is produced), with a WARN that the header was a heuristic guess.
  - A treatment-column mapping line (`Treatment columns -> ADSL.TRT01A`)
    placed right after the title is no longer misread as the population; it
    now reaches `bind_annotations()` as the column-axis grouping. A paragraph
    with no population wording counts as the population only when its
    annotation is a population-flag reference (`...FL='Y'`).
  - A pre-table footnote (`Note: ...`) between title and table is kept as a
    footnote instead of being glued onto the title.
  - A Word comment carrying an annotation is bound even when it is anchored
    to a data cell rather than the stub cell.
  - Fuzzy stub-label matching no longer lets a one/two-character label (`n`,
    `%`) substring-match an unrelated longer phrase.
  - Known limitations, consciously deferred: page headers are read only for
    single-section documents (a multi-section docx with per-section headers
    is not attempted); the annotation highlight-exclusion list is
    `none`/`black` only; the text-box fixture uses the direct
    `w:txbxContent` shape rather than Word's `mc:AlternateContent` wrapper.

* Shell layout fidelity (ADR 0003, phases 1-5). arsbridge now carries a
  first-class model of the authored table layout from the annotated shell all
  the way to the rendered output:
  - *Footnote/annotation split.* Programmer annotation lines outside the stub
    cells (coloured runs, ADaM-pattern text, or `Label -> DATASET.VAR` arrow
    paragraphs below a table) are routed to `programmer_annotations` and never
    shipped as footnotes. `spec_to_ars(ship_annotations = FALSE)` is the
    default; `TRUE` re-attaches them for debugging.
  - *Convention-agnostic binding.* New `bind_annotations()` fuzzy-matches each
    below-table `Label -> annotation` line back to its stub row (in-cell
    detections still win), splits multi-label lines
    (`Completed / Discontinued -> ADSL.EOSSTT (COMPLETED / DISCONTINUED)`)
    into per-row value filters, and captures a
    `Treatment columns -> ADSL.TRT01A` line as the authoritative column-axis
    grouping.
  - *Layout persistence + no-drop.* `build_ars_json()` walks every authored
    stub row in order: annotated rows become analyses whose method is inferred
    deterministically from the annotation form (count expression -> subject
    count; `VAR='val'` -> filtered subject count; bare variable -> categorical
    or continuous per the ADaM spec), label-only rows are kept as layout
    entries, and an annotated row whose variable cannot resolve is reserved as
    a traceable `manual_pending` analysis instead of being dropped. The
    ordered layout is persisted per output as arsbridge-private
    `_meta.shell_layout`, alongside `_meta.source_datasets`.
  - *Layout-driven rendering + column restriction.* When `_meta.shell_layout`
    is present, `ars_render_tlf()` builds the stub from the authored labels
    (joined to the ARD by `analysis_id`), pins the authored row order, expands
    categorical/continuous analyses beneath their authored label, renders
    missing rows blank (never dropped), and restricts the treatment columns to
    the arm levels named in the shell headers -- a population level like
    "Screen Failure" in `TRT01A` no longer leaks in as a treatment column.
    Outputs without the layout metadata render exactly as before.
  - *Listings/figures.* A LISTING section always emits `MTH_LISTING` analyses
    regardless of the LLM's analysis-type guess, and `ars_render_figure()` now
    resolves its default dataset from the shell's `Source:` line
    (`_meta.source_datasets`) instead of assuming `ADEFF`.
  - Fixed a tfrmt warning ("Unable to apply `frmt_combine` due to uniqueness
    of column/row identifiers") by using named single-parameter formats
    instead of one-parameter `frmt_combine()` in generated body plans.
* Initial release.
* Classification wiring (ADR 0001): a capability-gated table is no longer
  reserved wholesale. `build_ars_json()` now classifies which of its statistics
  arsbridge can compute (deterministic keyword scan of the section's title,
  footnotes, and labels) and builds a *partial* section -- the descriptive rows
  compute, and each detected executable method (a Clopper-Pearson CI; a CMH
  p-value when "stratified by <VAR>" names a strata variable) is appended as its
  own analysis with operands. Only the residual indicators it still cannot
  compute (e.g. a Newcombe difference) are reserved as `manual_pending` and
  named on the placeholder. With no residual, the table is no longer flagged
  unsupported at all -- it renders with the computed CI / CMH cells. An LLM
  enrichment can supersede the keyword layer later.
* Second executable descriptor: Cochran-Mantel-Haenszel p-value (ADR 0001). New
  exported `ard_cmh_test()` wraps base R's `stats::mantelhaen.test()` (the cardx
  wrapper is not used) and returns the CMH p-value as a one-row ARD. When a
  `MTH_CMH_TEST` analysis carries a stratification operand (`strata` on the
  analysis, resolved against the data), arsbridge emits an
  `arsbridge::ard_cmh_test()` call and computes the p-value (`value_source =
  "stats"`); with no resolvable strata it degrades to a `manual_pending` stub.
  The executable-method registry is now general (`.EXEC_DESCRIPTORS`: a
  `value_source` plus an `available(res)` predicate per method), replacing the
  cardx-only flag. `resolve_analysis()` carries the new `strata` operand.
* First executable descriptor: exact (Clopper-Pearson) binomial CI (ADR 0001).
  When `{cardx}` is installed, the `MTH_PROPORTION_CI_EXACT` method is no longer
  reserved as a manual cell -- arsbridge emits a `cardx::ard_categorical_ci()`
  call and computes the per-arm CIs like any other result (`value_source =
  "cardx"`). It needs no operand beyond the response variable and the treatment
  grouping. Without `{cardx}` the same cell degrades gracefully to a
  `manual_pending` stub. Cochran-Mantel-Haenszel and the Newcombe difference
  stay reserve-only until their stratification / reference-group operands are
  carried through the spec. `{cardx}` is a soft dependency (Suggests).
* Manual-fill round-trip + guard (ADR 0002, phase 5). After computing a reserved
  `manual_pending` cell with a validated script, the analyst writes the value
  back into the ARD row (`stat`, `result_status = "manual_filled"`,
  `value_source`, `derivation_ref`) -- the ARD is a diffable, auditable data
  frame. New `ars_validate_manual_fills()` flags any `manual_filled` cell that
  has no `derivation_ref` or no value; `ars_render_all()` raises each as a
  blocker before rendering, so an untraceable manual number can never ship. A
  filled cell then renders its value like any other. See
  `vignette("getting-started")` for the round-trip. ADR 0002 is now fully
  implemented (phases 1-5).
* Partial table rendering (ADR 0002, phase 4). An output that arsbridge can
  compute only in part now renders: the computable cells are filled and each
  reserved `manual_pending` cell renders as a loud `[‡ manual]` marker (never
  blank, never `NA`, never a number), keyed to a table footnote. An output with
  no computable cell at all stays a whole-table numbered placeholder, which now
  also names the reserved cells. The render manifest flags a partial table as
  `partial -- manual cells reserved`.
* Partial-results traceability (ADR 0002, phases 1-3). `ars_to_ard()` now stamps
  every row with provenance columns (`result_status`, `value_source`,
  `derivation_ref`, `derived_by`, `derived_dt`). A declared-but-unexecutable
  method (e.g. `MTH_CMH_TEST` -- a statistic describable in the ARS but with no
  `{cards}`/`{cardx}` executor) no longer skips or coerces the analysis: it
  reserves keyed `manual_pending` stub ARD rows (`stat = NA`) so the table cell
  keeps a slot tied to its analysis/method/output. A later validated manual
  computation fills that slot rather than typing an orphan value into the
  rendered output. New `ars_manual_worklist()` lists every pending cell as the
  analyst's checklist. The capability gate no longer strips analyses from a
  gated table: the ARS keeps the analysis and a declarative
  `MTH_UNSUPPORTED_ANALYSIS` method (flagged `supported = FALSE` with the
  capability reason), so the Output -> Analysis -> Method chain is intact and
  the engine reserves a stub cell for it. The renderer still emits a numbered
  placeholder until partial rendering (phase 4) lands. Additive only --
  computed results are unchanged.
* Architecture decision records under `adr/`: ADR 0001 sets the
  statistical-method extensibility boundary (bound the boundary, not the
  contents -- descriptor contract on the shared ARD shape, tiered honest
  degradation, deterministic emission with the LLM only classifying); ADR 0002
  proposes partial results with intact traceability (reserved stub ARD rows +
  provenance columns so a cell arsbridge cannot compute keeps a keyed slot for
  a validated manual fill, never an orphan value). ADR 0002 is a plan, not yet
  implemented.
* Capability gate: tables needing inferential or model-based methods
  (Cochran-Mantel-Haenszel, Clopper-Pearson / Newcombe intervals, p-values,
  odds/hazard ratios, regression, ANCOVA/MMRM, NRI imputation) are detected
  (LLM + keyword scan), raised as blockers, and NOT coerced into a
  meaningless count. They are carried to the final output as a numbered
  placeholder so the table numbering still matches the shell exactly. The
  placeholder now reads as an intentional capability gate (not a bug) and
  points to a separate validated analysis script; render *failures* emit a
  distinct placeholder clearly labelled as an error, not a gate. The
  rationale and the path to extending coverage are recorded in
  `adr/0001-statistical-method-extensibility.md`.
* Hybrid shell reading: a deterministic four-layer regex detector and an
  LLM primary reader (`extract_shell_llm()`) run together and take the
  union, to extract as many annotation variants as possible. Every
  LLM-proposed `DATASET.VARIABLE` passes a hard ADaM-spec gate -- out-of-spec
  proposals are rejected and logged as blockers, never shipped. With no API
  key the reader degrades to the deterministic pass. See
  `vignette("reading-engine")`.
* Provider registry (`R/llm_providers.R`): Anthropic, OpenAI, Gemini, and
  OpenAI-compatible providers such as GLM are defined in one place. Adding a
  provider is a single entry. New generic `set_llm_key()` setter; select the
  active provider with `ARS_LLM_PROVIDER`.
* `spec_to_ars()`: parse an annotated TLF shell `.docx` plus an ADaM
  specification (`define.xml` or Excel) into CDISC Analysis Results
  Standard (ARS) v1.0 JSON.
* `ars_to_ard()`: execute an ARS JSON natively into a tidy Analysis
  Results Data (ARD) object via `{cards}`, applying `analysisSets` and
  `dataSubsets` filters against `.xpt` / `.csv` datasets.
* `ars_render_tlf()`, `ars_render_all()`, `ars_to_tfrmt()`: render ARS
  outputs to publication-ready GT and Word tables via `{tfrmt}`.
* Multi-provider LLM enrichment (Anthropic, OpenAI, Gemini) with a
  keyword-heuristic fallback so the pipeline runs without an API key.
* `ars_diagnostics()` / `ars_blockers()`: plain-English diagnostics that
  point the user at the input document to fix.
