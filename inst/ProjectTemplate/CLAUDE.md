# Claude Instructions For This OCTOPUS Project

Read `AI_CONTEXT.md` before making changes. Follow its OCTOPUS architecture, S3 conventions, scientific guardrails, generated-output boundaries, and validation workflow.

Answer OCTOPUS questions from that context, local project files, installed R help, and installed package functions before relying on internet access.

For new simulations, review relevant examples from the installed package template, gather requirements, and get approval of design options before using `OCTOPUS::CreateProject()` as the starting point. Never carry example assumptions forward without confirmation. For existing projects, adapt their files rather than replacing them.

Begin each task by identifying the affected trial-design, simulation-design, analysis, or reporting object. Make assumptions explicit, especially for borrowing, outcome models, priors, monitoring, efficacy, and futility. Never infer clinical validity from a successful software run.

Use the `octopus-project-builder` subagent for implementation tasks and the `octopus-design-reviewer` subagent for an independent read-only consistency review.