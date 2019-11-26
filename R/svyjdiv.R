#' J-divergence measure
#'
#' Estimate the J-divergence measure, an entropy-based measure of inequality
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
#' @seealso \code{\link{svyjdivdec}} , \code{\link{svygei}}
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
#' svyjdiv( ~eqincome , design = subset( des_eusilc , eqincome > 0 ) )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svyjdiv( ~eqincome , design = subset( des_eusilc_rep , eqincome > 0 ) )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyjdiv( ~py010n , design = subset( des_eusilc , py010n > 0 | is.na( py010n ) ) )
#' svyjdiv( ~py010n , design = subset( des_eusilc , py010n > 0 | is.na( py010n ) ), na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyjdiv( ~py010n , design = subset( des_eusilc_rep , py010n > 0 | is.na( py010n ) ) )
#' svyjdiv( ~py010n , design = subset( des_eusilc_rep , py010n > 0 | is.na( py010n ) ) , na.rm = TRUE )
#'
#' # database-backed design
#' library(RSQLite)
#' library(DBI)
#' dbfile <- tempfile()
#' conn <- dbConnect( RSQLite::SQLite() , dbfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfile,
#' 		dbtype="SQLite"
#' 	)
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' svyjdiv( ~eqincome , design = subset( dbd_eusilc , eqincome > 0 ) )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyjdiv <- function(formula, design, ...) {

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  UseMethod("svyjdiv", design)

}

#' @rdname svyjdiv
#' @export
svyjdiv.survey.design <- function ( formula, design, na.rm = FALSE, ... ) {

  w <- 1/design$prob
  incvar <- model.frame( formula , design$variables, na.action = na.pass)[,]

  if (na.rm) {
    nas <- is.na( incvar )
    design <- design[nas == 0, ]
    w <- 1/design$prob
    incvar <- model.frame( formula , design$variables, na.action = na.pass)[,]
  }

  if ( any( is.na( incvar ) [ w > 0 ] ) ) {

    rval <- NA
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence decomposition"
    class(rval) <- c( "cvystat" , "svrepstat" )

    return( rval )

  }

  if ( any( incvar[ w > 0 ] <= 0 , na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes only." )

  # internal functions
  jdiv_efun <- function( y , w ) {
    N <- sum( w )
    mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
    sum( ifelse( w > 0 , w * ( y / mu - 1 ) * log( y / mu ) , 0 ) ) / N
  }
  jdiv_linfun <- function( y , w ) {
    N <- sum( w )
    mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
    gei1 <- sum( ifelse( w > 0 , w * ( y / mu ) * log( y / mu ) , 0 ) ) / N
    jdiv <- sum( ifelse( w > 0 , w * ( y / mu - 1 ) * log( y / mu ) , 0 ) ) / N
    ifelse( w > 0 , (1/N) * ( ( y / mu - 1 ) * log( y / mu ) - jdiv ) - ( gei1 / mu ) * ( y - mu ) / N , 0 )
  }

  # estimates
  rval <- jdiv_efun( incvar , w )

  # linearization
  lin <- jdiv_linfun( incvar , w )

  # variance
  variance <- survey::svyrecvar( lin/design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

  # output object
  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "statistic") <- "j-divergence"
  attr(rval, "var") <- variance
  attr(rval, "lin") <- lin

  rval

}


#' @rdname svyjdiv
#' @export
svyjdiv.svyrep.design <- function ( formula, design, na.rm = FALSE, ... ) {

  # aux funs
  jdiv_efun <- function( y , w ) {
    N <- sum( w )
    mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
    sum( ifelse( w > 0 , w * ( y / mu - 1 ) * log( y / mu ) , 0 ) ) / N
  }

  ws <- weights(design, "sampling")
  incvar <- model.frame( formula , design$variables, na.action = na.pass)[ , ]

  if (na.rm) {
    nas <- rowSums( is.na( incvar ) & ws > 0 ) > 0
    design <- design[nas == 0, ]
    incvar <- model.frame( formula , design$variables, na.action = na.pass)[ , ]
    ws <- weights(design, "sampling")
  }

  if ( any( incvar[ ws > 0 ] <= 0, na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes only." )

  if ( any( is.na( incvar ) [ ws > 0 ] ) ) {

    rval <- NA
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence decomposition"
    class(rval) <- c( "cvystat" , "svrepstat" )

    return(rval)

  }

  ww <- weights(design, "analysis")

  # estimate
  rval <- jdiv_efun( incvar , ws )

  # replcates
  qq <- apply(ww, 2, function(wi) jdiv_efun( incvar , wi ) )

  # variance
  variance <- survey::svrVar( qq , design$scale, design$rscales, mse = design$mse, coef = matrix( ttl.jdiv, within.jdiv, between.jdiv ) )

  # outrput object
  names( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "j-divergence"

  return( rval )

}

#' @rdname svyjdiv
#' @export
svyjdiv.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

    NextMethod("svyjdiv", design)
  }


# J-divergence measure:
calc.jdiv <-  function( x, weights ) {

  x <- x[ weights > 0]
  weights <- weights[ weights > 0]

  avg <- sum( x * weights ) / sum( weights )
  jdiv = ( ( x - avg ) / avg ) * log( x / avg )

  return( sum( jdiv * weights ) / sum( weights ) )

}
