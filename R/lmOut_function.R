#' Regression summary output
#'
#' This function collects coefficients, t-stats, p-values, f-stats, etc from a regression summary and writes output to csv or returns dataframe.
#' @param res regression model.
#' @param ndigit number of rounding digits.
#' @param writecsv write to csv flag. Defaults to FALSE
#' @param file output CSV file name. Defaults to empty string
#' @keywords lmOut
#' @export
#' @examples
#' lmOut(myres)
#' lmOut(myres, writecsv=TRUE, file="my-results.csv")
#' write.xlsx(lmOut(myres), file=filepath, sheetName="Summary", row.names=FALSE)

lmOut <- function(res, ndigit=3, writecsv=FALSE, file="") {
     # If summary has not been run on the model then run summary
     if (length(grep("summary", class(res)))==0) res <- summary(res)

     co <- res$coefficients
     nvar <- nrow(co)
     ncol <- ncol(co)
     f <- res$fstatistic
     formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)

     # This sets the number of rows before we start recording the coefficients
     nstats <- 4

     # G matrix stores data for output
     G <- matrix("", nrow=nvar+nstats, ncol=ncol+1)

     G[1,1] <- toString(res$call)

     # Save rownames and colnames
     G[(nstats+1):(nvar+nstats),1] <- rownames(co)
     G[nstats, 2:(ncol+1)] <- colnames(co)

     # Save Coefficients
     G[(nstats+1):(nvar+nstats), 2:(ncol+1)] <- formatter(co)

     # Save F-stat
     G[1,2] <- paste0("F(",f[2],",",f[3],")")
     G[2,2] <- formatter(f[1])

     # Save F-p value
     G[1,3] <- "Prob > P"
     G[2,3] <- formatter(1-pf(f[1],f[2],f[3]))

     # Save R2
     G[1,4] <- "R-Squared"
     G[2,4] <- formatter(res$r.squared)

     # Save Adj-R2
     G[1,5] <- "Adj-R2"
     G[2,5] <- formatter(res$adj.r.squared)

     # Convert matrix to dataframe for printing consistency
     G <- as.data.frame(G)

     if (writecsv && file != '') {
          write.csv(G, file=file, row.names=F)
     } else {
          return(G)
     }
}
