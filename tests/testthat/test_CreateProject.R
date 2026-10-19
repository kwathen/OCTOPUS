##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


context("CreateProject.R")


test_that("CreateProject - Test 1",
{

    gstrProjectDirectory <- paste( getwd(), "/Test1", sep="")
    gstrProjectName      <- "MyNewProject"
    strRes <- CreateProject( strProjectDirectory =  gstrProjectDirectory, strProjectName =  gstrProjectName,
                   strAnalysisName          = "MyBinaryAnalysis",
                   strSimPatientOutcomeName = "MyBinarySimulator",
                   strBorrowing             = "AllControls"  )
    bProjectCreated <- dir.exists( paste(gstrProjectDirectory, "/", gstrProjectName, sep=""))
    expect_true( bProjectCreated, label ="Directory created")

    strProjectPath <- file.path( gstrProjectDirectory, gstrProjectName )
    vAIContextFiles <- c(
        "AI_CONTEXT.md",
        "AGENTS.md",
        "CLAUDE.md",
        ".github/copilot-instructions.md",
        ".github/agents/octopus-project-builder.agent.md",
        ".github/agents/octopus-design-reviewer.agent.md",
        ".claude/agents/octopus-project-builder.md",
        ".claude/agents/octopus-design-reviewer.md",
        ".cursor/rules/octopus.mdc"
    )
    expect_true(
        all( file.exists( file.path( strProjectPath, vAIContextFiles ) ) ),
        label = "AI context and agent files created"
    )

    vSimulationReport <- readLines( file.path( strProjectPath, "SimulationReport.Rmd" ), warn = FALSE )
    expect_true( any( grepl( 'source\\("R/PostProcess.R"\\)', vSimulationReport ) ) )
    expect_true( any( grepl( 'readRDS\\( "cTrialDesign1.Rds"\\)', vSimulationReport ) ) )
    expect_true( any( grepl( 'readRDS\\( "lTrialDesigns.Rds"\\)', vSimulationReport ) ) )
    expect_false( any( grepl( "\\.RData", vSimulationReport ) ) )

    unlink( paste( getwd(),"/Test1", sep=""), recursive = TRUE)
})
