#' Kobus and Milos (2012) ordinal inequality measure (EXPERIMENTAL)
#'
#' Estimate the Kobus and Milos (2012) inequality measure, a measure of inequality for ordinal variables.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' This measure only allows for strictly positive variables.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svygei}}
#'
#' @references Nicholas Rohde (2016). J-divergence measurements of economic inequality.
#' J. R. Statist. Soc. A, v. 179, Part 3 (2016), pp. 847-870.
#' URL \url{http://onlinelibrary.wiley.com/doi/10.1111/rssa.12153/abstract}.
#'
#' Martin Biewen and Stephen Jenkins (2002). Estimation of Generalized Entropy
#' and Atkinson Inequality Indices from Complex Survey Data. \emph{DIW Discussion Papers},
#' No.345,
#' URL \url{https://www.diw.de/documents/publikationen/73/diw_01.c.40394.de/dp345.pdf}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' svykmoi( ~eqincome , design = subset( des_eusilc , eqincome > 0 ) )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svykmoi( ~eqincome , design = subset( des_eusilc_rep , eqincome > 0 ) )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svykmoi( ~py010n , design = subset( des_eusilc , py010n > 0 | is.na( py010n ) ) )
#' svykmoi( ~py010n , design = subset( des_eusilc , py010n > 0 | is.na( py010n ) ), na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svykmoi( ~py010n , design = subset( des_eusilc_rep , py010n > 0 | is.na( py010n ) ) )
#' svykmoi( ~py010n , design = subset( des_eusilc_rep , py010n > 0 | is.na( py010n ) ) , na.rm = TRUE )
#'
#' # database-backed design
#' library(MonetDBLite)
#' library(DBI)
#' dbfolder <- tempdir()
#' conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfolder,
#' 		dbtype="MonetDBLite"
#' 	)
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' svykmoi( ~eqincome , design = subset( dbd_eusilc , eqincome > 0 ) )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svykmoi <- function(formula, design, ...) {

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  warning("The svykmoi function is experimental and is subject to changes in later versions.")

  UseMethod("svykmoi", design)

}

#' @rdname svykmoi
#' @export
svykmoi.survey.design <- function ( formula, design, na.rm = FALSE, ... ) {

  stop("this function hasn't been implemented for non-replicate designs")

}


#' @rdname svykmoi
#' @export
svykmoi.svyrep.design <- function ( formula, design, na.rm = FALSE, ... ) {

  ordvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(ordvar)
    design<-design[!nas,]
    df <- model.frame(design)
    ordvar <- ordvar[!nas]
  }

  ws <- weights(design, "sampling")

  cmprop <- calc.cmprop( x = ordvar, weights = ws )
  kmoi <- calc.kmoi( cmprop )

  ww <- weights(design, "analysis")
  qq.cmprop <- apply(ww, 2, function(wi) calc.cmprop( ordvar, wi ) )
  qq <- apply( qq.cmprop, 2, function(iter) { calc.kmoi( iter ) } )
  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence"

    return(rval)

  } else {
    variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }
  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "j-divergence"

  return( rval )

}

#' @rdname svykmoi
#' @export
svykmoi.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

    NextMethod("svykmoi", design)
  }

# cumulative proportion:
calc.cmprop <- function( x, weights ) {

  x <- x[ weights > 0]
  weights <- weights[ weights > 0]

  N <- sum( weights )

  result <- sapply( levels(x) , FUN = function( i ) {
    sum( ifelse( x <= i , weights , 0 ) ) / N
  } , USE.NAMES = TRUE )

  return( result )

}

# median category
calc.medcat <- function( proportions ) {

  proportions <- proportions[ proportions <= .5 ]
  proportions[ which.min( abs( proportions - .5 ) ):length(proportions) ]

}

# J-divergence measure:
calc.kmoi <- function( proportions , medcat = NULL , a = 1 , b = 1 ) {

  k <- length(proportions)
  if (is.null(medcat)) { medcat <- calc.medcat( proportions ) ; m <- match( names( medcat ) , names(proportions) ) }
  else { m <- medcat }
  gamma <- a * sum( proportions[ 1:(m - 1) ] ) - b * sum( proportions[ m:k ] )
  c1 <- b*( k + 1 - m )
  c2 <- ( m - 1 ) * a/2 - ( k + 2 - m )*b/2 + c1
  (gamma + c1)/c2

}
