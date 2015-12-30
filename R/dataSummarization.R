

#' Creates age groups using a numeric age variable
#'
#' @param df a data.frame containing the age variable
#' @param age_var the index of the age variable within the data.frame. It is a
#'   numeric column. The unit does not matter.
#' @param ll an ordered vector containing the lower limit for each age group. It
#'   is a numeric vector.
#' @param ul an ordered vector containing the upper limit for each age group. It
#'   is a numeric vector.
#'
#' @return the same data.frame to which a new "Agecat" column has been added.
#'   The class of the column is character.
#' @export
#'
#' @details If ll and/or ul are not provided the function uses a default age
#'   grouping containg the following groups: 0-4, 5-17, 18-44, 45-64, 65+ (65
#'   and above).The internal upper limit for the default age grouping is 120
#'   years. Above that treshold the age will be classified as "U" for unknown.
agegroup.create <- function(df, age_var, ll = NULL, ul = NULL){
  if(is.null(ll) | is.null(ul)){
    ag <- data.frame("ll"= c(0, 5, 18, 45, 65),
                     "ul"= c(5, 18, 45, 65, 120))
  }else if(length(ll) == length(ul)){
    ag <- as.data.frame(cbind(ll, ul))
  }else stop()

  for(a in 1:nrow(ag)){
    if(a < nrow(ag)){
      df$Agecat[df[, age_var] >= ag[a, 1] &
                  df[, age_var] < ag[a,2] ] <- paste0(ag[a, 1],  "-", (ag[a,2]-1))
    }else {
      if(a==nrow(ag))
      { df$Agecat[df[, age_var] >= ag[a, 1] &
                    df[, age_var] < ag[a,2] ] <- paste0(
                      ag[a, 1],  "+"
                    )
      }
    }

  }
  df$Agecat[is.na(df[,age_var])] <- "U"
  return(df)

}
