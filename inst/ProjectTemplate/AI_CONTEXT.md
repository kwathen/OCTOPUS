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
2. Start with a small worker count and index range in `BuildMeRunMultiCore.R`.
3. Run `source("BuildMeRunMultiCore.R")` and inspect each worker's indexed output files.

### Combined Results And Report

Both execution modes produce raw CSV files. After simulation:

1. Run `OCTOPUS::BuildSimulationResultsDataSet()` or source `R/BuildSimulationResults.R`.
2. Confirm `simsMain.Rds`, `simsISA*.Rds`, and `simsCombined.Rds` were created.
3. Confirm `cTrialDesign1.Rds` and `lTrialDesigns.Rds` exist from the build script.
4. Knit `SimulationReport.Rmd`, which sources `R/PostProcess.R` and consumes those `.Rds` files.

## Validation Checklist

1. Resolve any intended `TEMP_` or `TMP_` placeholders left in generated files.
2. Parse all project R scripts:
   `Rscript -e "files <- c('BuildMe.R', 'BuildMeRunMultiCore.R', list.files('R', pattern = '\\.R$', full.names = TRUE)); invisible(lapply(files, parse))"`
3. Verify every analysis and simulator class has a matching S3 method and that required files are sourced in the selected execution mode.
4. Inspect `cTrialDesign` and `cSimulation` with `str()`; verify ISA counts, labels, allocations, scenarios, timing, and classes.
5. Run a deterministic one-replication smoke simulation before a larger run.
6. Check raw outputs for the selected mode, combine them, and verify the expected `.Rds` artifacts.
7. Knit `SimulationReport.Rmd` only after `simsCombined.Rds` exists.
8. Compare key results against hand calculations or a trusted baseline where practical.

Preserve seeds when comparing designs. Never run `CleanSimulationDirectories()` or overwrite trusted results without explicit approval.

## Useful References

- OCTOPUS website: https://kwathen.github.io/OCTOPUS/
- General workflow: https://kwathen.github.io/OCTOPUS/articles/GeneralWorkflow.html
- Primary objects: https://kwathen.github.io/OCTOPUS/articles/PrimaryObjectsForSimulation.html
- Extending OCTOPUS: https://kwathen.github.io/OCTOPUS/articles/ExtendingOCTOPUS.html
- Simulation flow: https://kwathen.github.io/OCTOPUS/articles/SimulateTrialOutline.html
- Examples: https://github.com/kwathen/OCTOPUS-Examples
