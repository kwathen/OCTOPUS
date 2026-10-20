# OCTOPUS Project AI Context

This project uses the OCTOPUS R package to design and simulate platform clinical trials with multiple Intervention Specific Appendices (ISAs). Use this file as the authoritative starting point when helping a user build or modify a simulation.

Simulation assumptions, outcome models, borrowing rules, monitoring, and decision boundaries are scientifically consequential. Explain assumptions and ask for missing requirements rather than inventing values. A successful software run does not establish clinical or statistical validity.

## How To Help A User

Before writing code, read `BuildMe.R` and the relevant files under `R/`, then establish which inputs are known:

- ISAs, treatment arms, treatment labels, and sample sizes;
- ISA start times and platform recruitment rates;
- endpoint types, direction of benefit, observation times, and required follow-up;
- analysis models, priors or estimators, MAV/TV values, and decision rules;
- interim-analysis timing and interim/final efficacy or futility boundaries;
- control borrowing (`NoBorrowing` or `AllControls`) for each ISA;
- data-generating scenarios, including null and clinically relevant alternatives;
- simulation replications, random seeds, outputs, and comparisons of interest.

If these are incomplete, identify the missing decisions and propose clearly labeled options. Do not silently choose scientific parameters. Once requirements are clear, make the smallest coherent change and validate it with a low-replication run before scaling up.

## Current Template Limitations: Read Before Running

The generated files are a starting point, not a universally runnable or general trial specification.

- **Unresolved placeholders:** If `CreateProject()` was called without `mQtyPatientsPerArm`, both build scripts contain `TMP_MATRIX_DATA`, `TMP_NROW`, and `TMP_NCOL`. They will not run until sample sizes replace those tokens. Search all project scripts for `TEMP_` and `TMP_` before the first run; only proceed when any remaining tokens are intentional.
- **Two-arm generated helper:** The current `SetupTrialDesign()` loop assigns one control label and one experimental-treatment label per ISA. The generated binary simulator likewise expects `ProbRespCtrl` and one `ProbRespExp`. More than one experimental arm requires coordinated changes to treatment labels, scenario parameters, outcome generation, analysis, decisions, and reporting. Do not infer multi-arm support from the matrix input alone.
- **Multicore resources:** `BuildMeRunMultiCore.R` exposes `nQtyCores`, `nStartIndex`, and `nEndIndex`, but its defaults still derive the worker count from available cores. Review and usually reduce these values before running. Ensure index ranges do not overwrite existing output.
- **Multicore reproducibility:** `RunParallelSimulations()` initializes worker streams with `parallel::clusterSetRNGStream()` and `nParallelSeed`. Reproduction requires the same seed, worker count, task indices, R version, package versions, and simulation code. Do not describe a multicore result as reproducible from `set.seed()` alone.
- **Example report assumptions:** `SimulationReport.Rmd` dynamically reads the number of designs and ISAs, but its decision narrative still illustrates the generated Bayesian binary analysis for ISA 1 using the last design. Rewrite that section when analyses, outcomes, decision rules, or design-specific assumptions change.

If a limitation conflicts with the requested design, explain the required refactor before editing and validate all affected objects together.

## Project Map

### Primary Design Files

- `BuildMe.R`: single-process entry point; defines design options, scenarios, recruitment, and replications before calling `RunSimulation()`.
- `BuildMeRunMultiCore.R`: multicore entry point; builds the same objects and delegates simulation work to `R/RunParallelSimulations.R`.
- `R/TrialDesign.R`: defines `SetupTrialDesign()` and constructs the trial-level and ISA-level design objects.
- `R/TrialDesignFunctions.R`: provides helpers such as `CreateISA()`, `NewTrialDesign()`, and `AddOutcome()`.
- `R/SimulationDesign.R`: defines `SetupSimulations()`, accrual, ISA start times, scenario objects, and patient-outcome simulator classes.

### Extension Files

- `R/RunAnalysis.<Class>.R`: custom outcome-level analysis S3 method.
- `R/SimPatientOutcomes.<Class>.R`: custom patient-outcome simulation S3 method.
- `R/BinaryFunctions.R`: project-specific helpers used by the generated binary example.
- `R/RunParallelSimulations.R`: worker setup; every project-specific function needed by a worker must be loaded or sourced here.

### Results And Reporting

- `R/BuildSimulationResults.R`: combines raw simulation output and demonstrates post-processing.
- `R/PostProcess.R`: reads combined results and creates summary data and plots.
- `R/PlatformTrialPlots.R`: additional timeline plotting helpers.
- `SimulationReport.Rmd`: report template that consumes design and combined-result `.Rds` files.
- `R/Functions.R`: includes cleanup helpers. `CleanSimulationDirectories()` is destructive.

Treat `enrollment/`, `out/`, `ISAOut*/`, `log/`, `cTrialDesign*.Rds`, `lTrialDesigns.Rds`, `sims*.Rds`, and simulation CSV files as generated output. Do not hand-edit or delete them without explicit user approval.

## Core OCTOPUS Objects

- `cTrialDesign` describes the platform and its ISA designs. Its class selects the platform randomizer.
- `cTrialDesign$cISADesigns[[i]]` contains allocation, monitoring, decision, borrowing, and analysis information for ISA `i`.
- `cISAAnalysis$vAnalysis[[j]]` is a classed analysis object for outcome `j`. Its first class selects `RunAnalysis.<Class>()`.
- `cSimulation$SimDesigns` contains trial designs and scenario lists.
- Each scenario has one row per ISA in `dfScenarios`. Columns other than `Scenario` and `ISA` become fields on `cSimOutcomes` and are available to the patient simulator.
- The class supplied as `strSimPatientOutcomeClass` selects `SimPatientOutcomes.<Class>()`.

Keep treatment labels, allocations, outcome dimensions, observation times, follow-up, monitoring times, and decision boundaries internally consistent. Follow the existing prefixes: `b` logical, `n` integer/count, `d` numeric, `v` vector, `m` matrix, `l` list, `c` classed object, `df` data frame, and `str` character string.

## Built-In Analysis Catalog

Use this catalog when a user asks what OCTOPUS offers. It is intentionally included so common questions can be answered without internet access. When implementation details matter, inspect local R help and installed functions with `methods("RunAnalysis")`, `getAnywhere("RunAnalysis.TTest")`, or the equivalent method name.

| Analysis class | Intended endpoint | Procedure and important inputs |
| --- | --- | --- |
| `TTest` | Continuous, single comparison | Two-sample `t.test()` using `lDataAna$vOut` and `vTrt`; decisions use confidence limits. |
| `TTestPerDose` | Continuous, multiple experimental doses | Runs treatment-versus-control t-tests by dose and combines dose decisions. |
| `AOV` | Continuous with a baseline covariate | Fits `lm(vOut ~ vBaseline + vTrt)` and uses the treatment coefficient confidence interval. |
| `AOVPerDose` | Continuous with baseline, multiple doses | Fits a linear model and evaluates treatment coefficients by dose. |
| `Ranked` | Ordinal or non-normal continuous, single comparison | Uses `stats::wilcox.test()` and a confidence interval for the location shift. |
| `RankedExact` | Ordinal or non-normal continuous, single comparison | Uses `coin::wilcox_test()` with an exact distribution. |
| `RankedPerDose` / `RankedPerDoseExact` | Ordinal or non-normal continuous, multiple doses | Performs treatment-versus-control ranked analyses by dose. |
| `ReptMeasAnalysis` | Continuous repeated measures | Fits an `nlme::gls()` model with baseline, treatment, time, treatment-by-time, heterogeneous time variances, and within-patient correlation. |
| `ReptMeasAnalysisPerDose` | Continuous repeated measures, multiple doses | Uses the repeated-measures GLS fit and evaluates differences by dose. |
| `MCPModAnalysis` | Continuous dose-response | Fits candidate MCP-Mod dose-response models using dose and outcome at a selected time point. |
| `BayesianNormal` | Continuous | Uses a Bayesian normal model and posterior probabilities for decisions. |

The `PerDose` methods are available in the package even where their help pages are limited. Inspect their local source before relying on details not listed here. `RunAnalysis.default()` stops when no method matches the analysis class.

### Binary Outcomes

OCTOPUS currently has **no native purpose-built non-Bayesian analysis for binary outcomes**. In particular, there is no native two-sample proportion, risk-difference, risk-ratio, odds-ratio, Fisher exact, chi-square, or logistic-regression `RunAnalysis` method.

The generated `R/RunAnalysis.<Class>.R` is a Bayesian beta-binomial example for a binary endpoint. It updates beta priors with responder and non-responder counts and bases decisions on the posterior probability that the treatment-control difference exceeds the MAV. It is an editable project example, not a native package method and not a non-Bayesian option.

Do not present these methods as substitutes for a dedicated binary analysis:

- `TTest` can numerically run on 0/1 values and its difference in means equals a risk difference, but its t-based confidence interval is not a purpose-built binomial interval and can behave poorly with small samples or sparse events.
- `AOV` would be a linear probability model rather than a dedicated binary model.
- Ranked methods have extensive ties for binary data and target a rank/location estimand, not a clearly documented response-rate estimand.
- `BayesianNormal` assumes a continuous normal endpoint.

Until a native method is added, use a custom `RunAnalysis.<Class>` method. Before implementing it, ask the user to choose the estimand (risk difference, risk ratio, or odds ratio), interval/test method, sparse-cell behavior, covariate adjustment, multi-arm strategy, direction of benefit, MAV scale, and interim/final decision rules. Do not choose among these silently.

## Add A Custom Analysis

Use the generated `R/RunAnalysis.<Class>.R` as the starting point and inspect comparable OCTOPUS methods before changing its contract.

1. Choose a descriptive class name. The method suffix and analysis class must match exactly.
2. Set `strAnalysisModel` to that class when calling `SetupTrialDesign()`.
3. Put analysis-specific parameters in the corresponding `lAnalysis` entry. `AddOutcome()` adds them to `cAnalysis`.
4. Preserve this method signature:
   `RunAnalysis.<Class>(cAnalysis, lDataAna, nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer)`.
5. Read observed data from `lDataAna`; generated examples commonly use `vISA`, `vTrt`, and `vOut`. Handle unobserved outcomes explicitly.
6. Use `nISAAnalysisIndx` and `bIsFinalISAAnalysis` when boundaries differ by analysis time.
7. Return the decision fields required by OCTOPUS: `nGo`, `nNoGo`, and `nPause`, with exactly one active decision, plus `cRandomizer`. Additional analysis values may be returned when downstream output expects them.
8. Source the method in `BuildMe.R`. For multicore runs, also source it inside the worker function in `R/RunParallelSimulations.R`.
9. Test representative interim and final cases, boundary behavior, missing outcomes, and every treatment arm before running full simulations.

Do not change the generic or its signature to accommodate one analysis. Extend the classed analysis object instead.

## Add A Custom Patient-Outcome Simulator

Use the generated `R/SimPatientOutcomes.<Class>.R` and compare it with existing OCTOPUS simulator methods.

1. Choose a class name and pass it as `strSimPatientOutcomeClass` to `SetupSimulations()`.
2. Add all data-generating parameters as columns in `dfScenarios`; provide one row for every scenario and ISA pair.
3. Preserve this method signature:
   `SimPatientOutcomes.<Class>(cSimOutcomes, cISADesign, dfPatCovISA)`.
4. Read scenario parameters from `cSimOutcomes` and allocation/analysis information from `cISADesign`.
5. Return a classed list compatible with OCTOPUS. For the generated one-outcome example this includes `mSimOut1`, `vObsTime1`, `nQtyOut`, and `vPatTrt`.
6. Ensure outcome rows and treatment assignments agree with `cISADesign$vQtyPats` and `cISADesign$vTrtLab`.
7. Explicitly support `dfPatCovISA` or stop with a clear error when covariates are not supported.
8. Source the method in `BuildMe.R` and inside the multicore worker in `R/RunParallelSimulations.R`.
9. With a fixed seed, test dimensions, treatment ordering, ranges or distributional properties, observation times, and edge cases.

For multiple or longitudinal outcomes, inspect existing OCTOPUS methods and processing classes rather than extrapolating the one-outcome return shape.

## Run, Combine, And Report

### Single Process

1. Set a small `nQtyReps`, normally `1`, and a fixed seed while developing.
2. Run `source("BuildMe.R")`.
3. Inspect raw CSV output under `out/`, `ISAOut*/`, and optionally `enrollment/`.

### Multicore

1. Confirm every custom method and helper is sourced inside `R/RunParallelSimulations.R` because workers have separate R sessions.
2. Set `nQtyCores`, `nStartIndex`, `nEndIndex`, and `nParallelSeed` explicitly in `BuildMeRunMultiCore.R`; do not accept detected-core defaults without reviewing resource limits.
3. Keep the same seed, worker count, indices, software versions, and code when reproducing a run.
4. Run `source("BuildMeRunMultiCore.R")` and inspect each worker's indexed output files.

### Combined Results And Report

Both execution modes produce raw CSV files. After simulation:

1. Run `OCTOPUS::BuildSimulationResultsDataSet()` or source `R/BuildSimulationResults.R`.
2. Confirm `simsMain.Rds`, `simsISA*.Rds`, and `simsCombined.Rds` were created.
3. Confirm `cTrialDesign1.Rds` and `lTrialDesigns.Rds` exist from the build script.
4. Knit `SimulationReport.Rmd`, which sources `R/PostProcess.R` and consumes those `.Rds` files.

## Validation Checklist

1. Resolve any intended `TEMP_` or `TMP_` placeholders left in generated files.
2. Run a syntax-only parse of all project R scripts. This does not validate placeholders, packages, dimensions, scenarios, worker setup, or runtime behavior:
   `Rscript -e "files <- c('BuildMe.R', 'BuildMeRunMultiCore.R', list.files('R', pattern = '\\.R$', full.names = TRUE)); invisible(lapply(files, parse))"`
3. Verify every analysis and simulator class has a matching S3 method and that required files are sourced in the selected execution mode.
4. Inspect `cTrialDesign` and `cSimulation` with `str()`; verify ISA counts, labels, allocations, scenarios, timing, and classes.
5. Run a deterministic one-replication smoke simulation before a larger run.
6. Check raw outputs for the selected mode, combine them, and verify the expected `.Rds` artifacts.
7. Knit `SimulationReport.Rmd` only after `simsCombined.Rds` exists.
8. Compare key results against hand calculations or a trusted baseline where practical.

Preserve seeds when comparing designs. Never run `CleanSimulationDirectories()` or overwrite trusted results without explicit approval.

## Useful References

The sections above are the primary offline context. The generated project, installed OCTOPUS R help, and installed package functions should be inspected before internet sources.

- OCTOPUS website: https://kwathen.github.io/OCTOPUS/
- General workflow: https://kwathen.github.io/OCTOPUS/articles/GeneralWorkflow.html
- Primary objects: https://kwathen.github.io/OCTOPUS/articles/PrimaryObjectsForSimulation.html
- Extending OCTOPUS: https://kwathen.github.io/OCTOPUS/articles/ExtendingOCTOPUS.html
- Simulation flow: https://kwathen.github.io/OCTOPUS/articles/SimulateTrialOutline.html
- Examples: https://github.com/kwathen/OCTOPUS-Examples
