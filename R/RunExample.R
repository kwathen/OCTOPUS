##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name RunExample
#' @title RunExample
#' @description {This function is used to run the example in the package.  For a current list of example call
#'   OCTOPUS::RunExample() and you will get a list of the available examples.
#'
#'   There are example(s) that require shiny so you must install the shiny package to run some of the examples.
#'   }
#' @examples \dontrun{{OCTOPUS::RunExample( "CompareRecruitment" )
#' }}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RunExample.R}{View Code on GitHub} }
#' @export
RunExample <- function(example) {
    strPackage <- "OCTOPUS"
    # locate all the shiny app examples that exist  call using runExample("myapp")
    validExamples <- list.files(system.file("ShinyExamples", package = strPackage))

    validExamplesMsg <-
        paste0(
            "Valid examples are: '",
            paste(validExamples, collapse = "', '"),
            "'")

    # if an invalid example is given, throw an error
    if (missing(example) || !nzchar(example) ||
        !example %in% validExamples) {
        stop(
            'Please run `RunExample()` with a valid example app as an argument.n',
            validExamplesMsg,
            call. = FALSE)
    }

    # find and launch the app
    appDir <- system.file("ShinyExamples", example,"ShinyApp.R", package = strPackage)
    shiny::runApp(appDir, display.mode = "normal")
}
