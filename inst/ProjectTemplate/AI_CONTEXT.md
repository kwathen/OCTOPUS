# OCTOPUS Project AI Context

This project simulates platform clinical trials with the OCTOPUS R package. Treat simulation assumptions, decision boundaries, borrowing rules, and outcome models as scientifically consequential. Do not invent or silently change them.

## Start Here

1. Read `BuildMe.R` to understand the configured designs, scenarios, and execution flow.
2. Read `R/TrialDesign.R`, `R/TrialDesignFunctions.R`, and `R/SimulationDesign.R` before changing a design.
3. Read custom `R/RunAnalysis.*.R` and `R/SimPatientOutcomes.*.R` files before changing analysis or outcome behavior.
4. Use the OCTOPUS documentation at https://kwathen.github.io/OCTOPUS/ when a package contract is unclear.

## Project Workflow

- `BuildMe.R` is the primary entry point for local simulations.
- `BuildMeRunMultiCore.R` is the parallel execution entry point.
- `SetupTrialDesign()` constructs the trial design and ISA analysis objects.
- `SetupSimulations()` constructs scenario-specific simulation objects.
- `RunSimulation()` runs the designs, scenarios, and replications.
- `SimulationReport.Rmd` summarizes and visualizes simulation results.

## Files To Edit

- `BuildMe.R`: design options, scenarios, replications, recruitment, and execution.
- `R/TrialDesign.R`: trial structure, sample sizes, analyses, monitoring, and decision boundaries.
- `R/TrialDesignFunctions.R`: helpers used to construct ISA and analysis objects.
- `R/SimulationDesign.R`: accrual, ISA start times, scenarios, and outcome simulator objects.
- `R/RunAnalysis.<Class>.R`: a custom analysis S3 method, when present.
- `R/SimPatientOutcomes.<Class>.R`: a custom patient-outcome S3 method, when present.
- `SimulationReport.Rmd`: result summaries and graphics.

Treat `enrollment/`, `out/`, `ISAOut*/`, `log/`, `*.Rds`, and combined simulation result files as generated output. Do not hand-edit them.

## OCTOPUS Conventions

- OCTOPUS extensibility is class-driven S3 dispatch. A class name must exactly match its method suffix, such as `RunAnalysis.MyAnalysis` or `SimPatientOutcomes.MyOutcome`.
- Preserve existing method signatures and return structures. Compare with the generated template and installed OCTOPUS methods before changing a contract.
- Scenario data must contain one row for every scenario and ISA combination. Additional columns become parameters on the ISA outcome simulator.
- Keep treatment labels, patient allocations, observation times, follow-up times, interim-analysis timing, and decision boundaries internally consistent.
- Borrowing is configured per ISA. Confirm whether `NoBorrowing` or `AllControls` is intended before changing it.
- Follow the existing Hungarian-style prefixes: `b` logical, `n` integer/count, `d` numeric, `v` vector, `m` matrix, `l` list, `c` classed object, `df` data frame, and `str` character string.
- Preserve random seeds when comparing designs or validating a behavior change.

## Guardrails For AI Changes

- State any statistical or clinical assumptions before implementing them.
- Ask for clarification when efficacy, futility, safety, prior, estimand, borrowing, or multiplicity requirements are ambiguous.
- Never describe a simulation change as clinically validated merely because code runs.
- Make the smallest change that satisfies the request and do not rewrite generated results.
- Do not add package dependencies without explaining why they are required.

## Validation

After changing project code:

1. Parse edited R files:
   `Rscript -e "files <- list.files('R', pattern = '\\.R$', full.names = TRUE); invisible(lapply(files, parse))"`
2. Source the design helpers and inspect the resulting objects with `str()`.
3. Check that each analysis and outcome class has a matching S3 method.
4. Run a small deterministic smoke simulation, normally with `nQtyReps <- 1`, before a full run.
5. Verify expected files appear under `enrollment/`, `out/`, and `ISAOut*/`.
6. Knit `SimulationReport.Rmd` after simulation results are available.

Do not delete existing simulation results unless the user explicitly requests cleanup. `CleanSimulationDirectories()` is destructive.

## Useful References

- OCTOPUS website: https://kwathen.github.io/OCTOPUS/
- General workflow: https://kwathen.github.io/OCTOPUS/articles/GeneralWorkflow.html
- Primary objects: https://kwathen.github.io/OCTOPUS/articles/PrimaryObjectsForSimulation.html
- Extending OCTOPUS: https://kwathen.github.io/OCTOPUS/articles/ExtendingOCTOPUS.html
- Simulation flow: https://kwathen.github.io/OCTOPUS/articles/SimulateTrialOutline.html
- Examples: https://github.com/kwathen/OCTOPUS-Examples