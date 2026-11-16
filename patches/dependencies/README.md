# Checked Babel/YASON derived sources

Candidate `autolith-babel-yason-derived-v2` repairs Babel's ISO-2022-JP positive
character-count limit, KSC-5601 encoding errors and YASON's declaration order.
The manifest authenticates the complete reference-matched diagnostic Babel/YASON
trees, their exact derived bytes, both owned patches, and the CFFI ASD/string
consumer protocol. It declares diagnostic authority, not production authority.

Use a cold SBCL with ASDF and the available Linux Nix build utilities
`/run/current-system/sw/bin/sha256sum` and `patch`. Do not load Babel, YASON or
CFFI first. Load `script/prepare-dependency-sources.lisp`, then call:

```lisp
(prepare-dependency-sources
 :manifest #P"/checkout/patches/dependencies/manifest.sexp"
 :source-root #P"/reference-matched/"
 :output-root #P"/private/fresh-sources/"
 :logs #P"/private/preparation-logs/"
 :authority :reference-matched-diagnostic)
```

Create the log directory first. Supply a nonexistent output directory outside
the input trees. The returned directory contains an identity-keyed source copy
and `repair.sexp`. Authenticate every input before creating the copy; authenticate
all output files after exact patch application. Preserve a failed preparation's
logs and partial output instead of reusing that directory.

After preparation exits, mount its complete output read-only for compilation.
Load the selected existing project setup, then `script/dependency-setup.lisp`
and call `select-derived-dependency-sources` with `:manifest`, `:source-root`,
`:derived-root` (the returned directory), `:logs`, `:cache-root` (nonexistent),
and the same `:authority`. The adapter checks the exact patches and source trees,
registers the derived Babel/YASON ASDs and authenticated CFFI ASD ahead of the
original source collection, verifies actual ASDF selection, and selects a fresh
manifest-identity-keyed FASL directory before any factory consumer loads.

Run the cold finite fixture in a separate SBCL process:

```text
sbcl --no-sysinit --no-userinit --script script/check-dependency-repairs.lisp \
  /checkout/patches/dependencies/manifest.sexp /reference-matched/ \
  /private/fresh-sources/IDENTITY/ /private/check-logs/ /private/fresh-fasls/
```

The fixture records all raw warning conditions through compilation-unit closure,
compiles ENCODE before PARSE, checks pre-PARSE unbound Boolean variables,
exercises seven ISO cases through vector and CFFI pointer counters, and checks
default/dynamically rebound Boolean encoding and parser symbol selection.
`ORIGINS.sexp` records the actual factory-consumer sources and newly built FASLs.
KSC controls exercise actual vector and CFFI pointer encoders/decoders, mapped
GETA MARK replacement and mixed-input round trips, slice/destination bounds,
returned values, and genuine strict encoding conditions with source context.

## Build and source entrypoints

The Nix package applies the checked patches inside its existing Lisp package
scope, with full Babel/YASON input/output hashes and CFFI protocol guards.
Upstream sources, patches, flags and hooks are preserved. Defingerprinter uses
that scope too. The generated project setup records the derived provider ASD
paths and manifest identity; the image identity includes all three derivations.
`build-manifest.json` is a projection of `manifest.sexp` for the Nix evaluator.
Change them together and verify every projected record against the Lisp data.
A successful local diagnostic fixture does not authenticate the pinned Nix
providers or prove the resulting closure was built.

For an explicitly prepared source candidate, set `AUTOLITH_DEPENDENCY_SELECTION`
to an absolute canonical file containing this one argument list, in this order:

```lisp
(:manifest #P"/checkout/patches/dependencies/manifest.sexp"
 :derived-root #P"/private/fresh-sources/IDENTITY/"
 :source-root #P"/reference-matched/"
 :cache-root #P"/private/fresh-fasls/"
 :logs #P"/private/check-logs/"
 :authority :reference-matched-diagnostic)
```

Create the log directory and use new cache and log roots for each process. Source
launch, bootstrap, checks, restart publication, server and static-release setup
route this selection before loading consumers. Direct ASD loading participates
too. Internal cold consumers can pass `:consumer "name"` to the setup helper to
select separate fresh cache/log subdirectories of the descriptor's roots.
Bootstrap uses `bootstrap` and does not build an active core for prepared sources.
Saving or installing a prepared-source active core is explicitly unsupported,
even after removing the descriptor environment variable. Ordinary source and
packaged active-image builds are unaffected. The shell launcher bypasses source-only
cores when a descriptor is present. Without a descriptor or generated Nix identity,
source setup makes no repaired-provider claim. Minimal recovery does not depend
on this preparation or selection.

Production acceptance requires the actual pinned providers and transitive
closure to pass these guards and the finite controls. Packaging changes do not
replace the installed closure used by `autolith` or `autolith-dev`.

For unrepresentable KSC-5601 input with `:errorp nil`, substitute U+3013 GETA MARK
through Babel's KSC table. The encoder resolves a nullable mapping before its
packed-unit type assertion and calls the existing `encoding-error` API; strict
calls signal `character-encoding-error` with the original code, encoding, buffer
and position. Neither raw U+FFFD nor the generic ASCII substitution byte is a
valid KSC replacement. The historical `babel-bounded-count.patch` filename now
contains both the unchanged bounded ISO hunk and this KSC encoder hunk, matching
the single-patch-per-provider projection. Decoder and valid-input bytes are
unchanged. This is a local/vendored repair; upstream contact requires separate
permission. Original failures are retained. Candidate checks do not constitute
production or evaluator acceptance.

Babel uses the MIT license (Luis Oliveira and contributors); YASON uses the BSD
license (Hans Huebner and contributors). Retain their original notices and
LICENSE files in every copy. Remove each owned patch when the selected upstream
includes its semantic repair and the finite controls pass on that unpatched
provider.
