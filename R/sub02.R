#' Sample beta values
#'
#' Example dataset for testing analyses, and to see input structure
#'
#' @name sub02
#' @docType data
#' @format Objects of class tbl_df (inherits from tbl, data.frame) a minimal tibble for the `nmmr` analysis (`r nrow(sub02)` x `r ncol(sub02)`)
#'
#' \describe{
#' \item{sub}{factor ID of subject}
#' \item{run}{factor ID of session}
#' \item{voxel}{factor ID of voxel}
#' \item{contrast}{Factor with different values of contrast. Note that the factor _must_ have the lower levels of contrast correspond
#' to a lower integer representation (e.g., low == 1 and high == 2).}
#' \item{orientation}{Orientation presented on trial, in radians (between \eqn{\pm\pi}).
#' This is the _tuning variable_. Most functions require the numeric values, but some functions require only the factor levels (e.g., [Deming$new()][Deming] ).}
#' \item{y}{beta values in given condition (corresponds to beta in betas2)}
#' \item{ses}{factor ID of session}
#' }
#' @return a [tibble][tibble::tbl_df]
#'
#' @details
#'
#' Where possible, the columns of this dataframe follow the [BIDS](https://bids.neuroimaging.io/)
#' format. That is, `sub` specifies participant, and `run` is the run number
#' within session `ses`.
#'
#' The main functions of this package require data formatted analogously. At a
#' minimum, you will need a column indexing voxels (e.g., `voxel`, in `sub02`),
#' a column for the dependent variable (e.g., `y`), a column indexing the stimulus
#' values for which voxels might be tuned (e.g., `orientation`), and a column
#' indexing the experimental factor across which the tuning may be modulated
#' (e.g., `contrast`). A column indexing participant (e.g., `sub`) is required
#' for running the full hierarchical versions of the models.  The other columns
#' (e.g., `run` and `ses`) are not always necessary, but they help keep the data
#' tidy and may also help with troubleshooting (e.g., voxel activity often differs
#' across sessions).
#'
#' @examples
#' sub02
"sub02"
