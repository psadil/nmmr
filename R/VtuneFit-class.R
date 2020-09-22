
#' VtuneFit inherits from [rstan::stanfit] in the \pkg{rstan} package
#'
#' @aliases VtuneFit
#' @slot rawdata tbl_df. see example [betas]
#' @slot standata list.
#'
#' @details
#' checkout [rstan::stanfit]
#' and maybe [Rstudio](http://rstudio.com)
#' @importClassesFrom rstan stanfit
setClass("VtuneFit",
         slots = c(
           rawdata = "tbl_df",
           standata = "list"
         ),
         prototype = list(
           rawdata = tibble::tibble(),
           standata = list()
         ),
         contains = "stanfit"
)

#' extract raw data
#'
#' @param object object to grab rawdata from.
setGeneric("rawdata", function(object) standardGeneric("rawdata"))

#' @describeIn VtuneFit extract data used to construct [rawdata()]
#' @param object object to grab data from.
#' @export
setMethod("rawdata", "VtuneFit", function(object) object@rawdata)


#' extract data that was passed to model
#'
#' @param object object to grab rawdata from.
#'
#' @details
#' see output of [make_standata()]
setGeneric("standata", function(object) standardGeneric("standata"))

#' @describeIn VtuneFit extract data passed to stan. can be reused as input to [vtf()]
#' @export
setMethod("standata", "VtuneFit", function(object) object@standata)


bind_fits <- function(sfitlist){
  stanfit <- rstan::sflist2stanfit(sfitlist)
  fits_bound <- methods::new("VtuneFit",
                             stanfit,
                             rawdata=rawdata(sfitlist[[1]]),
                             standata=standata(sfitlist[[1]]))
  return(fits_bound)
}

#' Bind list of VtuneFit objects into single fit
#'
#' @param sfitlist list of VtuneFit objects
#'
#' @details
#' thin wrapper around [rstan::sflist2stanfit()]
setGeneric("bind_fits", function(sfitlist) standardGeneric("bind_fits"))

#' @describeIn VtuneFit Bind list of VtuneFit objects into single fit
#' @param sfitlist list of VtuneFit objects
#' @export
setMethod("bind_fits", "VtuneFit", bind_fits)


