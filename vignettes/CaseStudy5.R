## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----stup, echo=FALSE----------------------------------------------------

    nQtyCol   <- 7

    vMeanTrt  <- c( 3.5, 3.4, 3.0, 2.5,  2.9, 1.4, 0.9 )
    vMeanCtrl <- c( 3.5, 3.5, 3.5, 3.5,  3.5, 3.5, 3.5 )

    mVarCov   <- matrix( c( 0.5, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.5, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.5, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.5, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5), ncol=nQtyCol )
    dfMean <- data.frame(  rbind( Nonresponders =vMeanCtrl,Responder=vMeanTrt)  )
    row.names( dfMean) <- c("Non-responders", "Responders")
    colnames(dfMean) <- c( "Baseline", paste("Week",seq(4,24,4)))
    knitr::kable( dfMean, caption="True Means", label="TrueMeans" ) 
    
    
    mVarCov   <- matrix( c( 0.5, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.5, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.5, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.5, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5), ncol=nQtyCol )



## ----echo=FALSE----------------------------------------------------------

write_matrix <- function(strMatrixName, mMatrix) {
  begin <- paste( "$$", strMatrixName, " = \\begin{bmatrix}", sep="")
  end <- "\\end{bmatrix}$$"
  X <-
    apply(mMatrix, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  paste(c(begin, X, end), collapse = "")
}



