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
svyfwp.survey.design <- function ( formula , design , na.rm = FALSE , deff = FALSE , influence = FALSE ) {

  # define formula
  # this.expression <- quote( ( 4*(.5 - L50) - G ) * ( MU / Q50 ) ) # Foster-Wolfson (1992)
  this.expression <- quote( ( 2*(.5 - L50) - G ) * ( MU / Q50 ) ) # Kovacevic & Binder (1997) and Hoffmann (2017)

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

  # compute Lorenz function at the median
  L50 <- list( value = convey:::CalcLorenz( incvar , w , .5 ) , lin = convey:::CalcLorenz_IF( incvar , w , .5 ) )

  # compute Gini index
  G <- list( value = convey:::CalcGini( incvar , w ) , lin = convey:::CalcGini_IF( incvar , w ) )

  # compute median
  Q50 <- list( value = convey:::computeQuantiles( incvar , w , .5 ) , lin = convey:::CalcQuantile_IF( incvar , w , .5 ) )

  # compute mean
  Y <- sum( ifelse( w >= 0 , incvar * w , 0 ) )
  N <- sum( w )
  MU <- list( value = Y / N , lin = ( ( incvar - Y/N ) / N )[ w > 0 ] )

  # compute foster-wolfson
  list_all <- list( L50 = L50 , G = G , MU = MU , Q50 = Q50 )
  FWP <-  convey::contrastinf( this.expression , list_all )
  lin <- FWP$lin[ , 1 ]

  # ensure length
  if ( length( lin ) != length( design$prob ) ) {
    tmplin <- rep( 0 , nrow( design$variables ) )
    tmplin[ w > 0 ] <- lin
    lin <- tmplin ; rm( tmplin )
  }
  names( lin ) <- rownames( design$variables )

  # compute variance
  variance <- survey::svyrecvar( lin/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata )
  variance[ which( is.nan( variance ) ) ] <- NA
  colnames( variance ) <- rownames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

  # treat missing
  if (anyNA( variance)) {
    rval <- NA
    variance <- matrix(NA)
    colnames( variance ) <- rownames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "foster-wolfson"
    if ( influence ) attr( rval , "influence" ) <- NA
    if ( is.character(deff) || deff) attr( rval , "deff" ) <- NA
    return( rval )
  }

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

  # coerce to matrix
  lin <- matrix( lin , nrow = length( lin ) , dimnames = list( names( lin ) , strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]] ) )

  # build result object
  rval <- FWP$value
  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "foster-wolfson"
  if ( influence ) attr(rval,"influence") <- lin
  if ( is.character( deff ) || deff ) attr( rval , "deff") <- deff.estimate
  rval

}

#' @rdname svyfwp
#' @export
svyfwp.svyrep.design <- function ( formula , design , na.rm = FALSE , deff = FALSE , influence = FALSE , return.replicates = FALSE ) {

  # point estimate
  CalcFosterWolfson <- function( y , w ) {

    # drop observations
    y <- y[ w > 0 ]
    w <- w[ w > 0 ]

    # intermediate estimates
    Q50  <- convey:::computeQuantiles( y , w , .5 )
    MU <- sum( w  * y ) / sum( w )
    L50 <- convey:::CalcLorenz( y , w , .5 )
    G <- convey:::CalcGini( y , w )

    # compute polarization
    ( 2*( .5 - L50 ) - G ) * ( MU / Q50 )

  }

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
  variance <- survey::svrVar( qq , design$scale , design$rscales , mse = design$mse, coef = estimate )
  names( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

  # compute deff
  if ( is.character(deff) || deff || influence ) {

    # define formula
    this.expression <- quote( ( 2*(.5 - L50) - G ) * ( MU / Q50 ) ) # Kovacevic & Binder (1997) and Hoffmann (2017)

    # compute Lorenz function at the median
    L50 <- list( value = convey:::CalcLorenz( incvar , ws , .5 ) , lin = convey:::CalcLorenz_IF( incvar , ws , .5 ) )

    # compute Gini index
    G <- list( value = convey:::CalcGini( incvar , ws ) , lin = convey:::CalcGini_IF( incvar , ws ) )

    # compute median
    Q50 <- list( value = convey:::computeQuantiles( incvar , ws , .5 ) , lin = convey:::CalcQuantile_IF( incvar , ws , .5 ) )

    # compute mean
    Y <- sum( ifelse( ws >= 0 , incvar * ws , 0 ) )
    N <- sum( ws )
    MU <- list( value = Y / N , lin = ( ( incvar - Y/N ) / N )[ ws > 0 ] )

    # compute foster-wolfson
    list_all <- list( L50 = L50 , G = G , MU = MU , Q50 = Q50 )
    FWP <-  convey::contrastinf( this.expression , list_all )
    lin <- FWP$lin[ , 1 ]

    # compute deff
    nobs <- length( design$pweights )
    npop <- sum( design$pweights )
    vsrs <- unclass( svyvar( lin , design, na.rm = na.rm, return.replicates = FALSE, estimate.only = TRUE)) * npop^2/nobs
    if (deff != "replace") vsrs <- vsrs * (npop - nobs)/npop
    deff.estimate <- variance / vsrs

    # filter observation
    names( lin ) <- rownames( design$variables )

    # coerce to matrix
    lin <- matrix( lin , nrow = length( lin ) , dimnames = list( names( lin ) , strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]] ) )

  }

  # build result object
  rval <- estimate
  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "foster-wolfson"
  if ( is.character(deff) || deff) attr( rval , "deff" ) <- deff.estimate
  if ( influence ) attr( rval , "influence" ) <- lin

  # keep replicates
  if (return.replicates) {
    attr( qq , "scale") <- design$scale
    attr( qq , "rscales") <- design$rscales
    attr( qq , "mse") <- design$mse
    rval <- list( mean = rval , replicates = qq )
    class( rval ) <- c( "cvystat" , "svrepstat" )
  }

  # return object
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
