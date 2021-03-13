#' Foster-Wolfson Polarization index
#'
#' Estimates the Foster and Wolfson (1992) income (bi-)polarization index.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped? Observations containing missing values in income or group variables will be dropped.
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvydstat}", which are vectors with a "\code{var}" attribute giving the variance-covariance matrix and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob
#'
#' @references J. E. Foster and M. C. Wolfson (2010). Polarization and the decline of the middle class:
#' Canada and the u.S. \emph{The Journal of Economic Inequality}, Vol. 8, No. 2, p. 247-273. Originally published in 1992.
#'
#' M. Kovacevic and D. A. Binder (1997). Variance Estimation for Measure of Income Inequality
#' and Polarization - the Estimating Equations Approach. \emph{Journal of Official Statistics}, Vol. 13, No. 1 , p. 41-58.
#'
#' M. C. Wolfson (1992). When Inequalities Diverge. \emph{The American Econommic Review},
#' Vol. 84, No. 2, p. 353-358.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' svyfwp( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svyfwp( ~eqincome , design = des_eusilc_rep )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyfwp( ~ py010n , design = des_eusilc )
#' svyfwp( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyfwp( ~ py010n , design = des_eusilc_rep )
#' svyfwp( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
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
#' svyfwp( ~ eqincome , design = dbd_eusilc )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyfwp <-
  function( formula, design,  ...) {

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svyfwp", design)

  }

#' @rdname svyfwp
#' @export
# linearization-based object
svyfwp.survey.design <- function ( formula , design , na.rm = FALSE , deff = FALSE , ... ) {

  # collect data
  incvar <- model.frame( formula, design$variables, na.action = na.pass )[,]

  # treat missing
  if (na.rm) {
    nas <- is.na( incvar )
    design <- design[ !nas , ]
    incvar <- model.frame( formula, design$variables, na.action = na.pass )[,]
  }

  # collect sampling weights
  w <- 1/design$prob

  # calculate estimate
  estimate <- CalcFosterWolfson( incvar , w )

  ### linearization

  # compute median
  Q50 <- computeQuantiles( incvar , w , .5 )

  # compute density at the median
  Fprime <- densfun( formula , design , Q50 , na.rm = na.rm )

  # compute linearized variable
  lin <- CalcFosterWolfson_LIN( incvar , w , Fprime )

  # treat out of sample
  if ( length( lin ) != length( design$prob ) ) {
    names( lin ) <- rownames( design$variables )[ w > 0 ]
    lin <- lin[ pmatch( rownames( design$variables ) , names(lin) ) ]
    lin[ w <= 0] <- 0
  }

  # compute variance
  variance <- survey::svyrecvar( lin/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata )
  variance[ which( is.nan( variance ) ) ] <- NA
  colnames( variance ) <- rownames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

  # compute deff
  if ( is.character(deff) || deff) {
    nobs <- sum( weights( design ) != 0 )
    npop <- sum( weights( design ) )
    if (deff == "replace") vsrs <- svyvar( lin , design, na.rm = na.rm) * npop^2/nobs
    else vsrs <- svyvar( lin , design , na.rm = na.rm ) * npop^2 * (npop - nobs)/(npop * nobs)
    deff.estimate <- variance/vsrs
    names(deff.estimate) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  }

  # keep necessary influence functions
  lin <- lin[ 1/design$prob > 0 ]

  # build result object
  rval <- estimate
  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "foster-wolfson"
  # attr(rval,"influence") <- lin
  if ( is.character(deff) || deff) attr( rval , "deff") <- deff.estimate
  rval

}

#' @rdname svyfwp
#' @export
svyfwp.svyrep.design <- function ( formula , design , na.rm = FALSE , deff = FALSE , ... ) {

  # collect income data
  incvar <- model.frame( formula, design$variables, na.action = na.pass )[,]

  # treat missing
  if (na.rm) {
    nas <- is.na( incvar )
    design <- design[ !nas , ]
    incvar <- model.frame( formula, design$variables, na.action = na.pass )[,]
  }

  # collect sampling weights
  ws <- weights( design , "sampling" )

  # calculate point estimate
  estimate <- CalcFosterWolfson( incvar , ws )

  ### variance calculation

  # collect replication weights
  wr <- weights( design , "analysis" )

  # calculate replicates
  qq <- apply( wr, 2, function(wi) CalcFosterWolfson( incvar , wi ) )


  # calculate variance
  if (anyNA(qq)) {
    rval <- estimate
    variance <- matrix(NA)
    colnames( variance ) <- rownames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "foster-wolfson"
    if ( is.character(deff) || deff) attr( rval , "deff" ) <- NA
    return( rval )
  }
  variance <- survey::svrVar( qq , design$scale , design$rscales , mse = design$mse, coef = estimate )
  names( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

  # compute deff
  if ( is.character(deff) || deff ) {

    # compute median
    Q50 <- computeQuantiles( incvar , ws , .5 )

    # compute density at the median
    Fprime <- densfun( formula , design , Q50 , na.rm = na.rm )

    # compute linearized variable
    lin <- CalcFosterWolfson_LIN( incvar , ws , Fprime )

    # compute deff
    nobs <- length( design$pweights )
    npop <- sum( design$pweights )
    vsrs <- unclass( svyvar( lin , design, na.rm = na.rm, return.replicates = FALSE, estimate.only = TRUE)) * npop^2/nobs
    if (deff != "replace") vsrs <- vsrs * (npop - nobs)/npop
    deff.estimate <- variance / vsrs

    # filter observation
    names( lin ) <- rownames( design$variables )

  }

  # build result object
  rval <- estimate
  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "foster-wolfson"
  if ( is.character(deff) || deff) attr( rval , "deff" ) <- deff.estimate
  # if ( is.character(deff) || deff) attr( rval , "influence" ) <- lin
  rval

}

#' @rdname svyfwp
#' @export
svyfwp.DBIsvydesign <-
  function (formula , design, ...) {

    design$variables <-
        getvars( formula , design$db$connection , design$db$tablename , updates = design$updates , subset = design$subset )

    NextMethod("svyfwp", design)

  }

# function for point estimate
CalcFosterWolfson <- function( y , w ) {

  # drop observations
  y <- y[ w > 0 ]
  w <- w[ w > 0 ]

  # calculate median
  median.estimate  <- computeQuantiles( y , w , .5 )

  # overall, upper, and lower mean
  middle.mark <- y >= median.estimate
  overall.mean <- sum( w  * y ) / sum( w )
  upper.mean <- sum( middle.mark * (w  * y) ) / sum( middle.mark * w )
  lower.mean <- sum( (!middle.mark) * (w  * y) ) / sum( (!middle.mark) * w )
  bigT <- ( upper.mean - lower.mean ) / overall.mean

  # calculate Gini
  Gini <- CalcGini( y , w )

  # compute polarization
  fwp <- ( bigT/2 - Gini ) * overall.mean/median.estimate

  # return value
  fwp

}

# function for lienarizzed varible (through estimating equations)
CalcFosterWolfson_LIN <- function( y , w , f_m ) {

  # drop observations
  y <- y[ w > 0 ]
  w <- w[ w > 0 ]

  # reorder
  ordinc <- order( y )
  y <- y[ ordinc ]
  w <- w[ ordinc ]

  # replace missing
  y <- ifelse( w > 0 , y , 0 )

  # population size
  N <- sum( w )

  # calculate median
  median.estimate  <- computeQuantiles( y , w , .5 )

  # overall, upper, and lower mean
  middle.mark <- y > median.estimate
  overall.mean <- sum( w  * y ) / N
  upper.mean <- sum( ifelse( middle.mark , w  * y , 0 ) ) / sum( ifelse( middle.mark , w , 0 ) )
  lower.mean <- sum( ifelse( !middle.mark , w  * y , 0 ) ) / sum( ifelse( !middle.mark , w , 0 ) )
  bigT <- ( upper.mean - lower.mean ) / overall.mean

  # calculate Gini
  Gini <- CalcGini( y , w )

  # compute polarization measure
  P <- ( bigT/2 - Gini ) * overall.mean/median.estimate

  # auxilliary variables
  F_y <- cumsum( w ) / N
  A_y <- F_y - ( Gini + 1 ) / 2
  B_y <- ( sum( w * y ) - cumsum( w * y ) ) / N

  # calculate linearization
  uvar <-
    ( 2/median.estimate ) *
    ( ( median.estimate - y ) * ( ( y <= median.estimate ) - .5 ) -
        ( A_y * y + B_y - overall.mean * ( Gini + 1 ) / 2 + y * Gini/2 ) ) +
    ( P / (  median.estimate * f_m ) ) * ( ( ( y <= median.estimate ) - .5 ) - P )
  uvar <- uvar / N

  # original order
  uvar <- uvar[ order( ordinc ) ]

  # return variable
  uvar

}
