# GitHub Copilot Instructions

Read and follow `AI_CONTEXT.md` for every task in this OCTOPUS simulation project.

- Inspect `BuildMe.R` and relevant `R/` setup files before editing.
- Preserve class-driven S3 dispatch and existing function contracts.
- Treat clinical and statistical assumptions as user decisions; surface ambiguity instead of inventing values.
- Do not edit generated output under `enrollment/`, `out/`, `ISAOut*/`, or `log/`.
- Parse changed R files and recommend a one-replication deterministic smoke run before full simulations.
- Use the OCTOPUS Design Reviewer agent for a read-only consistency review of trial and simulation designs.