##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name CreateProject
#' @title  CreateProject
#' @description Created an R Studio project with example files to simulate a platform trial utilizing the OCTOPUS package.
#' @param strProjectDirectory The complete path to where the project is created.  If a complete path
#' is not provided then the directory is relative to the working directory.  If this is left blank, the current working directory is used.
#' @param strProjectName The name of the project.   A directory with the name strProjectName is created in strProjectDirectory if bCreateProjectSubdirectory = TRUE
#' @param strAnalysisName The name of the analysis that should be done. If the analysis is part of OCTOPUS, then no new
#' files are created.  If the analysis is new, a file named RunAnalysis.strAnalysisName.R is created.
#' @param strSimPatientOutcomeName The name of the patient simulator that should be utilized. If the patient simulator
#' is part of OCTOPUS, then no new
#' files are created.  If the patient simulator is new, a file named SimPatientOutocmes.strSimPatientOutcomeName.R is created.
#' @param strBorrowing Specify the option for sharing control patients across ISAs.  If strBorrowing = "NoBorrowing"
#' then patients are not shared between ISAs.  If strBorrowing = "AllControls" then all control patients are included in
#' the data set that is sent to the RunAnalysis funciton.
#' @param nQtyReps The number of replications to simulate for each scenario.
#' @param mQtyPatientsPerArm Matrix with 2 or more columns to define the number of patients per arm on each ISA.  Each row represents an ISA and
#' column 1 the number of patients on control/placebo and column 2,... the number of patients on the treatment arms in the ISA.  Each ISA must have
#' the same number of arms (a control or placbo and treatment arms(s) ).  This option can be chnaged once the project is created by modifing the
#' SetupTrialDesign file.  If this parameter is NULL you will need to change the options in the BuildMe.R file that is created as part of the project.
#' @param dQtyMonthsFU The number of months of follow-up the last patient before running the final analysis (FA). By default, the design created
#' only includes a final analysis with NO interim analysis.  To use another option please see the TrialDesign.R file in the projected that is created.
#' @param  bCreateProjectSubdirectory  If TRUE the a subdirectory with the project name will be created, if FALSE then the project is created in strProjectDirectory.
#' @export
CreateProject <- function( strProjectDirectory        = "",
                           strProjectName             = "NewProject",
                           strAnalysisName            = "NewAnalysisName",
                           strSimPatientOutcomeName   = "NewSimOutcomeName",
                           strBorrowing               = "NoBorrowing",
                           nQtyReps                   = 10,
                           mQtyPatientsPerArm         = NULL,
                           dQtyMonthsFU               = 1.0,
                           vISAStartTimes             = NULL,
                           bCreateProjectSubdirectory = TRUE)
{
    strRet <- ""

    if( strProjectDirectory == "" | is.na( strProjectDirectory )  | is.null( strProjectDirectory ))
    {
        # Use the current working directory and create a folder named strProjectName
        strProjectDirectory  <- getwd()
    }

    if( strProjectName == "" )
    {
        strProjectName <- "NewProject"
    }

    if( bCreateProjectSubdirectory )
        strNewProjectDirectory <- paste( strProjectDirectory, "/", strProjectName, sep="" )
    else
        strNewProjectDirectory <- strProjectDirectory

    dir.create( strNewProjectDirectory , recursive = TRUE )

    #Copy the contents of the template project located in inst/ProjectTemplate director
    strPackage  <- "OCTOPUS"
    #strTemplateDirectory <- paste( getwd(), "/inst/ProjectTemplate", sep="")

    #Get the directory where OCTOPUS is installed
    strTemplateDirectory <- system.file("ProjectTemplate", package = strPackage)

    vFilesToCopy         <- list.files( strTemplateDirectory)
    vResults             <- file.copy( file.path(strTemplateDirectory, vFilesToCopy ), strNewProjectDirectory )

    nCopySuccess         <- sum( vResults )
    nCopyFail            <- length( vResults ) - nCopySuccess

    strRProjName         <- paste( strNewProjectDirectory, "/", "ProjectTemplate.Rproj", sep = "" )
    if( file.exists( strRProjName ) )
    {
        strNewRProjName <- paste( strNewProjectDirectory, "/", strProjectName, ".Rproj", sep = "" )

        file.rename( strRProjName, strNewRProjName )
        strRProjName  <- strNewRProjName
    }

    #Replace Analysis name everyone it needs to be changed
    strAnalysisRet <- ReplaceAnalysisInfo( strNewProjectDirectory, strAnalysisName )
    strSimOutRet   <- ReplaceSimPatientOutcomeInfo( strNewProjectDirectory, strSimPatientOutcomeName )

    #Replace options in the BuildMe.R file ####
    strBuildMeFile <- paste( strNewProjectDirectory, "/BuildMe.R", sep="" )
    strTemplate    <- readLines( strBuildMeFile )
    strTemplate    <- gsub( "TEMP_BORROWING", strBorrowing, strTemplate)
    strTemplate    <- gsub( "TEMP_QTY_REPS", nQtyReps, strTemplate )
    strTemplate    <- gsub( "TMP_QTY_MONTHS_FU", dQtyMonthsFU, strTemplate )

    if( is.null( mQtyPatientsPerArm ) == FALSE )
    {

        strTemplate    <- gsub( "TMP_MATRIX_DATA", paste( mQtyPatientsPerArm, collapse =","), strTemplate )
        strTemplate    <- gsub( "TMP_NROW", nrow( mQtyPatientsPerArm ), strTemplate )
        strTemplate    <- gsub( "TMP_NCOL", ncol( mQtyPatientsPerArm ), strTemplate )
    }
    if( is.null( vISAStartTimes ) == FALSE )
    {
         strTemplate    <- gsub( "TEMP_ISA_START_TIME", paste( "c( ", paste( vISAStartTimes, collapse =","), ")" ), strTemplate )
    }
    else
    {
        strTemplate    <- gsub( "TEMP_ISA_START_TIME", paste( "NULL" ), strTemplate )
    }


    #Save the updates to the file
    writeLines( strTemplate, con = strBuildMeFile )

    strRet          <- paste( nCopySuccess, "file(s) were successfully coppied and ", nCopyFail, " file(s) were not coppied correctly.")
    strInstrucitons <- paste( "Use R Studio to open the", strRProjName, "project file and begin by reading the BuildMe.R file.")
    strRet          <- c( strRet, strAnalysisRet, strSimOutRet, strInstrucitons )
    strRet          <- paste( strRet, collapse = "\n")
    return( strRet )
}

ReplaceAnalysisInfo <- function( strProjectDirectory, strAnalysisName )
{
    strRet <- ""
    if( strAnalysisName != "" )
    {


        strBuildMeFile <- paste( strProjectDirectory, "/BuildMe.R", sep="" )
        strTemplate    <- readLines( strBuildMeFile )
        strTemplate    <- gsub( "TEMP_ANALYSIS_MODEL", paste( strAnalysisName,  sep=""), strTemplate)
        writeLines( strTemplate, con = strBuildMeFile )

        if( !MethodExists( "RunAnalysis", strAnalysisName ) )
        {
            #This analysis is not part of OCTOPUS, need to create the new file and change the template to match
            strRet <- paste( "The analysis you requested", strAnalysisName, "is not part of the OCTOPUS package.  A file named" )
            strRet <- paste( "RunAnalysis.", strAnalysisName, ".R was created and the BuildMe.R set to source the new file.", sep="" )

            strAnalysisFileName <- paste( strProjectDirectory, "/RunAnalysis.", strAnalysisName, ".R", sep="")
            file.rename( paste( strProjectDirectory, "/RunAnalysis.TEMP_ANALYSIS_MODEL.R", sep=""), strAnalysisFileName )

            strTemplate    <- readLines( strAnalysisFileName )
            strTemplate    <- gsub( "TEMP_ANALYSIS_MODEL", paste( strAnalysisName,  sep=""), strTemplate)
            writeLines( strTemplate, con = strAnalysisFileName )
        }
        else
        {
            #The analysis method exists, need to delete the file name and remove the source command for it from the BuildMe

             strRet <- paste( "The analysis you requested, RunAnalysis.", strAnalysisName, " is part of the OCTOPUS package OR", sep="" )
            strRet <- paste( strRet, " a function called RunAnalysis.", strAnalysisName, " exists in the current environement. ", sep="" )
            strRet <- paste( strRet, " A new RunAnalysis function was NOT created.")


            file.remove(  paste( strProjectDirectory, "/RunAnalysis.TEMP_ANALYSIS_MODEL.R", sep="") )
            strBuildMeFile <- paste( strProjectDirectory, "/BuildMe.R", sep="" )
            strTemplate    <- readLines( strBuildMeFile )
            strSouceCmd    <- paste( "source\\( 'RunAnalysis.", strAnalysisName, ".R' \\)", sep = "")
            strTemplate    <- gsub( strSouceCmd, "", strTemplate)
            writeLines( strTemplate, con = strBuildMeFile )
        }

    }
    return( strRet )

}


ReplaceSimPatientOutcomeInfo <- function( strProjectDirectory, strSimPatientOutcomeName )
{
    strRet <- ""
    if( strSimPatientOutcomeName != "" )
    {


        strBuildMeFile <- paste( strProjectDirectory, "/BuildMe.R", sep="" )
        strTemplate    <- readLines( strBuildMeFile )
        strTemplate    <- gsub( "TEMP_SIM_PATIENT_OUTCOME", paste( strSimPatientOutcomeName,  sep=""), strTemplate)
        writeLines( strTemplate, con = strBuildMeFile )

        if( !MethodExists( "SimPatientOutcomes", strSimPatientOutcomeName) )
        {
            #This SimPatient is not part of OCTOPUS, need to create the new file and change the template to match
            strRet <- paste( "The patient simualator, SimPatientOutcomes", strSimPatientOutcomeName, "is not part of the OCTOPUS package.  A file named" )
            strRet <- paste( "SimPatientOutcomes.", strSimPatientOutcomeName, ".R was created and the BuildMe.R set to source the new file.", sep="" )

            strFileName <- paste( strProjectDirectory, "/SimPatientOutcomes.", strSimPatientOutcomeName, ".R", sep="")
            file.rename( paste( strProjectDirectory, "/SimPatientOutcomes.TEMP_SIM_PATIENT_OUTCOME.R", sep=""), strFileName )

            strTemplate    <- readLines( strFileName )
            strTemplate    <- gsub( "TEMP_SIM_PATIENT_OUTCOME", paste( strSimPatientOutcomeName,  sep=""), strTemplate)
            writeLines( strTemplate, con = strFileName )
        }
        else
        {

            strRet <- paste( "The patient simulator, SimPatientOutcomes.", strSimPatientOutcomeName, "is part of the OCTOPUS package OR" )
            strRet <- paste( strRet, " a function called SimPatientOutcomes.", strSimPatientOutcomeName, " exists in the current environement. ", sep="" )
            strRet <- paste( strRet, " A new SimPatientOutcomes function was NOT created.")

            #The analysis method exists, need to delete the file name and remove the source command for it from the BuildMe
            file.remove(  paste( strProjectDirectory, "/SimPatientOutcomes.TEMP_SIM_PATIENT_OUTCOME.R", sep="") )
            strBuildMeFile <- paste( strProjectDirectory, "/BuildMe.R", sep="" )
            strTemplate    <- readLines( strBuildMeFile )
            strSouceCmd    <- paste( "source\\( 'SimPatientOutcomes.", strSimPatientOutcomeName, ".R' \\)", sep = "")
            strTemplate    <- gsub( strSouceCmd, "", strTemplate)
            writeLines( strTemplate, con = strBuildMeFile )
        }

    }
    return( strRet )

}

MethodExists <- function( strGenericFctnName, strClassName )
{
    bFound <- !is.null( getS3method(strGenericFctnName, strClassName, optional=TRUE) )
    return( bFound )
}

