#' @description Created an R Studio project with example files to simulate a platform trial utilizing the OCTOPUS package.
#' @param strProjectDirectory The complete path to where the project is create
#' @param strProjectName The name of the project.   A directory with the name strProjectName is created in strProjectDirectory
#' @example Example 1 - If strProjectDirectory = "" and strProjectName = "" then a folder named NewProject is created in
#' the working directory.

CreateProject <- function( strProjectDirectory = "", strProjectName = "NewProject",
                           strAnalysisName, strSimPatientOutcomeName  )
{
    if( strProjectDirectory == "" )  # Use the current working directory and create a folder named strProjectName
    {
        strProjectDirectory  <- getwd()
    }

    if( strProjectName == "" )
    {
        strProjectName <- "NewProject"
    }

    strNewProjectDirectory <- paste( strProjectDirectory, "/", strProjectName, sep="" )
    dir.create( strNewProjectDirectory , recursive = TRUE )

    #Copy the contents of the template project located in inst/ProjectTemplate director
    strPackage  <- "OCTOPUS"
    strTemplateDirectory <- paste( getwd(), "/inst/ProjectTemplate", sep="")
    #Once installed
    #strTemplateDirectory <- system.file("ProjectTemplate", package = strPackage)
    #vFileToCopy <- list.files()

    vFilesToCopy <- list.files( strTemplateDirectory)
    vResults <- file.copy( file.path(strTemplateDirectory, vFilesToCopy ), strNewProjectDirectory )

    nCopySuccess <- sum( vResults )
    nCopyFail    <- length( vResults ) - nCopySuccess

    strRProjName <- paste( strNewProjectDirectory, "/", "ProjectTemplate.Rproj", sep = "" )
    if( file.exists( strRProjName ) )
    {
        strNewRProjName <- paste( strNewProjectDirectory, "/", strProjectName, ".Rproj", sep = "" )
        file.rename( strRProjName, strNewRProjName )
    }

    #Replace Analysis name everyone it needs to be changed
    ReplaceAnalysisName( strNewProjectDirectory, strAnalysisName )
    #ReplaceSimPatientOutcomeName( strNewProjectDirectory, strSimPatientOutcomeName )

    print( paste( nCopySuccess, "file(s) were successfully coppied and ", nCopyFail, " file(s) were not coppied correctly.") )
}

ReplaceAnalysisName <- function( strProjectDirectory, strAnalysisName )
{
    if( strAnalysisName != "" )
    {


        strBuildMeFile <- paste( strProjectDirectory, "/BuildMe.R", sep="" )
        strTemplate    <- readLines( strBuildMeFile )
        strTemplate    <- gsub( "TEMP_ANALYSIS_MODEL", paste( strAnalysisName,  sep=""), strTemplate)
        writeLines( strTemplate, con = strBuildMeFile )

        if( !MethodExists( "RunAnalysis", strAnalysisName ) )
        {
            #This analysis is not part of OCTOPUS, need to create the new file and change the template to match

            strAnalysisFileName <- paste( strProjectDirectory, "/RunAnalysis.", strAnalysisName, ".R", sep="")
            file.rename( paste( strProjectDirectory, "/RunAnalysis.TEMP_ANALYSIS_MODEL.R", sep=""), strAnalysisFileName )

            strTemplate    <- readLines( strAnalysisFileName )
            strTemplate    <- gsub( "TEMP_ANALYSIS_MODEL", paste( strAnalysisName,  sep=""), strTemplate)
            writeLines( strTemplate, con = strAnalysisFileName )
        }
        else
        {
            browser()
            #The analysis method exists, need to delete the file name and remove the source command for it from the BuildMe
            file.remove(  paste( strProjectDirectory, "/RunAnalysis.TEMP_ANALYSIS_MODEL.R", sep="") )
            strBuildMeFile <- paste( strProjectDirectory, "/BuildMe.R", sep="" )
            strTemplate    <- readLines( strBuildMeFile )
            strSouceCmd    <- paste( "source\\( 'RunAnalysis.", strAnalysisName, ".R' \\)", sep = "")
            strTemplate    <- gsub( strSouceCmd, "", strTemplate)
            writeLines( strTemplate, con = strBuildMeFile )
        }

    }

}

MethodExists <- function( strGenericFctnName, strClassName )
{
    bFound <- !is.null( getS3method(strGenericFctnName, strClassName, optional=TRUE) )
    return( bFound )
}
# Test 1 - Should create a directory named NewProject in the current working directory
# CreateProject()
# dir.exists( paste( getwd(), "/NewProject", sep=""))
#
# #Test 2 - Should create a directory named Test2/NewProject in the current working directory
# gstrProjectDirectory <- paste( getwd(), "/Test2", sep="")
# CreateProject( strProjectDirectory =  gstrProjectDirectory )
# dir.exists( paste( getwd(), "/Test2/NewProject", sep=""))


#Test 3 - Should create a directory named Test3/MyNewProject in the current working directory
gstrProjectDirectory <- paste( getwd(), "/Test3", sep="")
gstrProjectName      <- "MyNewProject"
CreateProject( strProjectDirectory =  gstrProjectDirectory, strProjectName =  gstrProjectName,
               strAnalysisName = "Ranked")
#dir.exists( paste( getwd(), "/Test3/MyNewProject", sep=""))


