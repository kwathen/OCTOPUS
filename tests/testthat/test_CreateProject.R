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
    expect_match( strRes, "TMP_ sample-size placeholders" )
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

    vAIContext <- readLines( file.path( strProjectPath, "AI_CONTEXT.md" ), warn = FALSE )
    expect_true( any( grepl( "Built-In Analysis Catalog", vAIContext, fixed = TRUE ) ) )
    expect_true( any( grepl( "no native purpose-built non-Bayesian analysis for binary outcomes", vAIContext, fixed = TRUE ) ) )
    expect_true( any( grepl( "Current Template Limitations", vAIContext, fixed = TRUE ) ) )
    expect_true( any( grepl( "syntax-only parse", vAIContext, fixed = TRUE ) ) )
    expect_true( any( grepl( "Plan Design Options Before Editing", vAIContext, fixed = TRUE ) ) )
    expect_true( any( grepl( "explicit approval before increasing replications", vAIContext, fixed = TRUE ) ) )
    expect_true( any( grepl( "Name And Rename Custom Methods", vAIContext, fixed = TRUE ) ) )

    vParallelRunner <- readLines( file.path( strProjectPath, "R/RunParallelSimulations.R" ), warn = FALSE )
    expect_true( any( grepl( "clusterSetRNGStream", vParallelRunner, fixed = TRUE ) ) )
    expect_true( any( grepl( "nSeed = 12345", vParallelRunner, fixed = TRUE ) ) )
    expect_true( any( grepl( "on.exit", vParallelRunner, fixed = TRUE ) ) )
    expect_false( any( grepl( "error = function", vParallelRunner, fixed = TRUE ) ) )

    vSimulationReport <- readLines( file.path( strProjectPath, "SimulationReport.Rmd" ), warn = FALSE )
    expect_true( any( grepl( 'source\\("R/PostProcess.R"\\)', vSimulationReport ) ) )
    expect_true( any( grepl( 'readRDS\\( "cTrialDesign1.Rds"\\)', vSimulationReport ) ) )
    expect_true( any( grepl( 'readRDS\\( "lTrialDesigns.Rds"\\)', vSimulationReport ) ) )
    expect_false( any( grepl( "\\.RData", vSimulationReport ) ) )
    expect_false( any( grepl( "lTrialDesigns[[3]]", vSimulationReport, fixed = TRUE ) ) )

    unlink( paste( getwd(),"/Test1", sep=""), recursive = TRUE)
})

test_that("CreateProject uses descriptive names for default extension methods",
{
    strProjectDirectory <- file.path( getwd(), "DefaultNames" )
    strProjectName <- "DescriptiveDefaults"

    CreateProject(
        strProjectDirectory = strProjectDirectory,
        strProjectName = strProjectName,
        mQtyPatientsPerArm = matrix( c( 10, 10 ), nrow = 1 )
    )

    strProjectPath <- file.path( strProjectDirectory, strProjectName )
    strAnalysisFile <- file.path( strProjectPath, "R", "RunAnalysis.BayesianBetaBinomial.R" )
    strOutcomeFile <- file.path( strProjectPath, "R", "SimPatientOutcomes.BinaryOutcome.R" )

    expect_true( file.exists( strAnalysisFile ) )
    expect_true( file.exists( strOutcomeFile ) )
    expect_true( any( grepl( "RunAnalysis.BayesianBetaBinomial <-", readLines( strAnalysisFile ), fixed = TRUE ) ) )
    expect_true( any( grepl( "SimPatientOutcomes.BinaryOutcome <-", readLines( strOutcomeFile ), fixed = TRUE ) ) )

    vGeneratedRFiles <- list.files( file.path( strProjectPath, "R" ), full.names = TRUE )
    vGeneratedCode <- unlist( lapply( vGeneratedRFiles, readLines, warn = FALSE ) )
    expect_false( any( grepl( "NewAnalysisName|NewSimOutcomeName", vGeneratedCode ) ) )

    unlink( strProjectDirectory, recursive = TRUE )
})
