#' Sample beta values
#'
#' Example dataset for testing analyses, and to see input structure
#' @name sub02
#' @docType data
#' @format Objects of class tbl_df (inherits from tbl, data.frame) a minimal tibble for the vtuner analysis (`r nrow(sub02)` x `r ncol(sub02)`)
#'
#' \describe{
#' \item{sub}{factor ID of subject}
#' \item{voxel}{factor ID of voxel}
#' \item{contrast}{Factor with different values of contrast. Note that the factor _must_ have the lower levels of contrast correspond
#' to a lower integer representation (e.g., low == 1 and high == 2)}
#' \item{orientation}{Orientation presented on trial (-pi,pi)}
#' \item{y}{beta values in given condition (corresponds to beta in betas2)}
#' }
#' @return a [tibble][tibble::tbl_df]
#' @examples
#' sub02
"sub02"