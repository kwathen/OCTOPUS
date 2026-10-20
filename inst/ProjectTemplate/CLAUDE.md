# Claude Instructions For This OCTOPUS Project

Read `AI_CONTEXT.md` before making changes. Follow its OCTOPUS architecture, S3 conventions, scientific guardrails, generated-output boundaries, and validation workflow.

Answer OCTOPUS questions from that context, local project files, installed R help, and installed package functions before relying on internet access.

Begin each task by identifying the affected trial-design, simulation-design, analysis, or reporting object. Make assumptions explicit, especially for borrowing, outcome models, priors, monitoring, efficacy, and futility. Never infer clinical validity from a successful software run.

Use the `octopus-project-builder` subagent for implementation tasks and the `octopus-design-reviewer` subagent for an independent read-only consistency review.