

#' Converts a table to a data.frame
#'
#' @param xtable a table or an xtable object
#' @param dname2ID a boolean variable indicating if the row names should be used as the index for the new table.
#'
#' @return an R data frame with the data from the table plus the optional ID column made with the dimnames of the table
#' @export
#'
#' @details If dname2ID is true the dimnames will be converted into a column that can be considered as an index column.
#' If dname2ID is false then the dimnames of the tables remain as is. Once the dimnames have been converted into a column they are deleted.

xtable2df <- function(xtable, dname2ID=TRUE){
    df <- as.data.frame.matrix(xtable)
    idcol <- names(dimnames(xtable)[1])
    df$tmpID <- rownames(df)
    allcols <- colnames(df)
    allcols[length(allcols)] <- idcol
    colnames(df) <- allcols
    rownames(df) <- NULL
    rownames(df) <- row(df, as.factor = FALSE)[,1]

    return(df)

}



#' Moves a data.frame column to a new position
#'
#' @param df An R data.frame
#' @param from an integer indicating the current position of the column to move.
#' @param to an integer indicating the target position of the column to move.
#'
#' @return A data.frame identical to the input data.frame in terms of contents but with reordered columns.
#' @details This function does not work with data tables. The default position for 'from' is the last column of the data.frame.
#' @export
moveCol <- function(df, from="last", to=1){
#Works with a data frame but not with a data.table
    xnum <- vector(mode = "numeric")
    cnum <- ncol(df)
    if(from=="last") from <- cnum

    if (to>from){
        df <- df[, cnum:1]
        alpha <- cnum-from+1
        omega <- cnum-to+1
        xnum <- permIntVec.1by1(alpha, omega, cnum)
        df <- df[, xnum]
        df <- df[, cnum:1]


    }else {
        alpha <- from
        omega <- to
        xnum <- permIntVec.1by1(alpha, omega, cnum)
        df <- df[, xnum]
    }

    return(df)

}



#' Reorders a vector of ordered integers.
#'
#' @param alpha An integer indicating the index of the item to be moved.
#' @param omega An integer indicating the index of the destination target integer.
#' @param cnum An integer indicating the number of items included in the vector of integers.
#'
#' @return A vector of the indexes of the columns in the new order.
#' @export
permIntVec.1by1 <- function(alpha, omega, cnum){
    # Coerce input to integer if possible
    alpha <- as.integer(alpha); omega <- as.integer(omega); cnum <- as.integer(cnum)

    # Check if the parameters are integers
    if(is.integer(alpha) & is.integer(omega) & is.integer(cnum)){
        #browser()
        # if cnum less than both alpha or omega
        if((cnum >= alpha) & (cnum >= omega)){
            # If alpha and omega are equal, do nothing.
            if(alpha==omega) {
                break()
            } else {
                # alpha and omega are not equal then check for direction
                l2r <- logical()

                if(alpha < omega){
                    l2r <- TRUE

                } else l2r <- FALSE

                xnum <- vector(mode = "integer") # container

                # Move item from left to rigth.
                if(l2r==TRUE){
                    # Move item from left to right
                    part2 <- seq(alpha+1, omega)
                    if(omega<cnum) part3 <- seq(omega+1, cnum) else part3 <- NULL
                    if(alpha>1) part1 <- seq(1,alpha-1) else part1 <- NULL
                    xnum <- c(part1, part2, alpha,  part3)

                } else {

                    # Move item from right to left
                    part2 <- seq(omega, alpha-1)
                    if(alpha<cnum) part3 <- seq(alpha+1, cnum) else part3 <- NULL
                    if(omega>1) part1 <- seq(1,omega-1) else part1 <- NULL
                    xnum <- c(part1, alpha, part2, part3)
                }
            }
        } else stop("Cannot move item beyond the length of the input vector.")


    } else stop("Function's parameters must be all integers.")

        return(xnum)
}


#' Saves to disc a data.frame as csv file
#'
#' @param sfile The source file. An R data.frame, a matrix, or a vector
#' @param tfile The target csv file that will be saved to disc.
#' @param wdir A boolean variable indicating if the file will be saved in the working directory or in a user-selected directory will be used.
#' @param rnames A boolean variable indicating if row names will be saved in the csv file or not.
#'
#' @export
savefile <- function(sfile, tfile, wdir=TRUE, rnames=FALSE){
    if(wdir==TRUE){
        write.csv(sfile, tfile, row.names = rnames)
    }else {
        fp <- choose.dir()
        fp <- gsub("\\\\", "/", fp)
        fn <- file.path(fp, paste0(tfile,".csv"))
        write.csv(sfile, fn, row.names = rnames)
    }
}

