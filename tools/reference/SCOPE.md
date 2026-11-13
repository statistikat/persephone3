# Native backend scope and reference provenance

Implementation item 16 starts with a reproducible inventory of the current
interface and the artifacts actually used for reference calculations.

## Reproduce the inventory

```sh
Rscript tools/reference/inventory.R
```

The script requires the installed rjd3 packages, rJava, RProtoBuf and `digest`.
It loads the reference backend but never calls Persephone or changes libraries.
An existing inventory rejects changes to package versions, JAR hashes, R or
JVM unless explicitly invoked with `--refresh-reference` for a reviewed update.

The outputs in `inst/tinytest/fixtures/backend/` are:

| Artifact | Meaning |
| --- | --- |
| `oracle-manifest.R` | Exact package metadata, JAR paths/SHA256/Maven metadata, runtime JVM/classpath, R version and generator fingerprints |
| `specification-options.csv` | 113 parameters from the 15 installed setters, including defaults as R expressions |
| `requested-outputs.csv` | 57 default output requests found by parsing the package's actual `userdefined_default` assignment |
| `expanded-templates.rds` | Concrete backend specifications for all 13 supported templates |
| `inventory-checksums.csv` | SHA256 of the four inventory artifacts above |

The oracle uses rjd3toolkit, rjd3x13 and rjd3tramoseats **3.9.0**, rjd3jars
**0.0.6**, and JD+ **3.9.0** JARs. The captured JVM runtime is
**27.0.1.0.2+1**; the JD+ manifests declare build Java **21**. The inventory
captures all 20 JARs installed in the inventoried packages, including rJava
JRI artifacts; the separate classpath records which seasonal-adjustment
artifacts were actually loaded. Recorded installation paths are evidence, not
paths to hardcode in the runtime package.

The researched upstream commit
[`15f1821fa71a949f842a5891e7cd68c83187f76d`](https://github.com/jdemetra/jdplus-main/commit/15f1821fa71a949f842a5891e7cd68c83187f76d)
declares 3.9.1-SNAPSHOT. It is not asserted to be the source of the installed
3.9.0 JARs. Their checksums pin the reference artifacts, but source revision
mapping is still unresolved in the captured wrapper/Maven metadata.

## Scope and responsibilities

All published constructor templates and specification blocks remain in the
candidate replacement scope. No setting is silently removed to make a native
prototype appear complete. The CSV's `not_implemented` state means no native
implementation has been qualified; per-method applicability and option
combinations still require validation.

| Area | Required work | Responsible role |
| --- | --- | --- |
| Public specs, contexts and results | Versioned contract, field registry, legacy conversion, selected backend and capability errors | Package/R integrator, item 17 with 6/8/10/12 |
| Numerical foundations and fixed models | Dates/domains, regressors, likelihood/optimization, SARIMA, covariance and forecasts | Numerical C++ engineer, items 19/20 |
| Automatic policies | Transform/regression/outlier/model selection with distinct X-13/TRAMO behavior | Numerical engineer and domain reviewer, items 21/23 |
| Decomposition | X-11 filters/passes/endpoints; SEATS admissibility/canonical decomposition/extraction/uncertainty | Numerical engineer and domain reviewer, items 22/24 |
| Diagnostics and benchmarking | All required typed statistics, categories and method-specific report outputs | Numerical/R engineers, item 25 |
| Product integration | R-side hierarchy/weights, serialization, plots and dashboards | R integrator, items 9/13/26 |
| Numerical qualification | Pinned direct-backend fixtures, independent invariants, per-field tolerances and discrepancy review | Validation owner plus independent domain reviewer, items 18/14/26 |
| Distribution | Portable Rcpp build, no-Java install/load/default execution, dependency removal | Build owner/package maintainer, items 19/27/15 |

The requested output list is only one part of the contract. Direct consumers
also require `preprocessing$description$variables`, fitted coefficient order
and covariance, likelihood degrees of freedom, transformation flags, X-11
intermediate tables including d8/d10, selected filters, SEATS components, and
report diagnostic p-values. Item 18's raw field schema captures observed shapes;
item 17 must map every actual consumer into the normalized contract.

The initial native slice is explicit fixed-transform/fixed-order fitting and
then X-11. It cannot implement the current `rsa3` defaults before automatic
selection is complete. Both methods and all mandatory capabilities must be
qualified before the Java-free replacement is declared complete. Native mode
must reject unsupported requests before fitting; there is no silent fallback.

## Authorship and unresolved gates

The new inventory/reference scripts are authored for this repository. No Java
implementation or JAR is copied into it. Initial fixtures use a local synthetic
formula, so no external input dataset is needed. Existing package licensing
continues to apply to newly authored repository code.

Upstream JDemetra+/rjd3 declare EUPL terms, whereas Persephone declares MIT.
This inventory is not a relicensing decision. Before source-derived numerical
code is copied or translated, record the exact upstream source, applicable
licence/notices and distribution approach; review the chosen Rcpp/numerical
library's licensing too. A future independent implementation must also record
algorithm references and authorship. A source SHA must not be invented from a
matching version number.

Remaining item 16 gates are the audited source/artifact correspondence,
licensing choice for the numerical implementation, and review of per-method
option/support commitments. The inventory and artifact pin are implemented;
these review decisions have not been represented as completed automatically.
