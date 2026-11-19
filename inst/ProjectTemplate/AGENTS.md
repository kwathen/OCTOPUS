# AI Instructions For This OCTOPUS Project

Read `AI_CONTEXT.md` before analyzing or modifying this project. It is the authoritative project context for architecture, editable files, generated outputs, S3 extension conventions, scientific guardrails, and validation.

For a new simulation, gather requirements and get the user's approval of design options before using `OCTOPUS::CreateProject()` as the starting point. Inspect relevant examples from the installed package's `system.file("ProjectTemplate", package = "OCTOPUS")` directory instead of recreating OCTOPUS structures from scratch. Treat template assumptions as examples, not user decisions. For an existing project, adapt its files and do not replace them with a new template.

When working in this repository:

- Inspect `BuildMe.R` and the relevant files under `R/` before proposing changes.
- Answer OCTOPUS questions from `AI_CONTEXT.md`, local project files, installed R help, and installed package functions before relying on internet access.
- Preserve OCTOPUS S3 method signatures, class names, and return structures.
- State assumptions that affect trial design, simulation behavior, or interpretation.
- Do not modify generated simulation outputs or run destructive cleanup without explicit approval.
- Validate with parsing and a minimal deterministic smoke run before recommending a full simulation.