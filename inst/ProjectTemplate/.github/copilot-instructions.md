# GitHub Copilot Instructions

Read and follow `AI_CONTEXT.md` for every task in this OCTOPUS simulation project.

- Inspect `BuildMe.R` and relevant `R/` setup files before editing.
- Answer OCTOPUS questions from `AI_CONTEXT.md`, workspace files, installed R help, and installed package functions before relying on internet access.
- For a new simulation, inspect relevant examples from the installed package template and get approval of design options before using `OCTOPUS::CreateProject()` as the starting point. For an existing project, adapt its files instead of replacing them.
- Preserve class-driven S3 dispatch and existing function contracts.
- Treat clinical and statistical assumptions as user decisions; surface ambiguity instead of inventing values.
- Do not edit generated output under `enrollment/`, `out/`, `ISAOut*/`, or `log/`.
- Parse changed R files and recommend a one-replication deterministic smoke run before full simulations.
- Use the OCTOPUS Project Builder agent for implementation tasks and the OCTOPUS Design Reviewer agent for an independent read-only consistency review.