#' Kobus and Milos (2012) ordinal inequality measure (EXPERIMENTAL)
#'
#' Estimate the Kobus and Milos (2012) inequality measure, a measure of inequality for ordinal variables.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param ref.point a string giving the labels of the "reference point". If not given, it defaults to the median category.
#' @param a a parameter giving the sensivity towards inequality below the reference point. Default to 1.
#' @param b a parameter giving the sensivity towards inequality over the reference point. Default to 1.
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
#' @references Martyna Kobus and Pyotr Milos. Inequality decomposition by population subgroups for ordinal data.
#' \emph{Journal of Health Economics}, Vol.31, No.1 (2016), pp. 15-21.
#' URL \url{http://www.sciencedirect.com/science/article/pii/S0167629611001664}.
#'
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(ses) ; names( ses ) <- tolower( names( ses ) )
#'
#' # linearized design
#' des_ses <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = ses )
#' des_ses <- convey_prep(des_ses)
#' des_ses <- update( des_ses, education = ordered( education ) )
#'
#' svykmoi( ~education , design = des_ses , ref.point = "ISCED 3 and 4" )
#'
#' # replicate-weighted design
#' des_ses_rep <- as.svrepdesign( des_ses , type = "bootstrap" )
#' des_ses_rep <- convey_prep(des_ses_rep)
#'
#' svykmoi( ~education , design = des_ses_rep , ref.point = "ISCED 3 and 4" )
#'
#' \dontrun{
#'
#' # database-backed design
#' library(MonetDBLite)
#' library(DBI)
#' dbfolder <- tempdir()
#' conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
#' dbWriteTable( conn , 'ses' , ses )
#'
#' dbd_ses <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="ses",
#' 		dbname=dbfolder,
#' 		dbtype="MonetDBLite"
#' 	)
#'
#' dbd_ses <- convey_prep( dbd_ses )
#' dbd_ses <- update( dbd_ses, education = ordered( education ) )
#'
#' svykmoi( ~education , design = dbd_ses , ref.point = "ISCED 3 and 4" )
#'
#' dbRemoveTable( conn , 'ses' )
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
svykmoi.survey.design <- function ( formula, design, ref.point = NULL, a = 1 , b = 1 , na.rm = FALSE , ... ) {

  if( a < 0 ) stop( "a= parameter has to be greater or equal to zero. " )
  if( b < 0 ) stop( "b= parameter has to be greater or equal to zero. " )

  ordvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if (!("ordered" %in% class(ordvar))) stop( "This function is defined for ordinal variables only. See ?svykmoi for examples." )

  if(na.rm){
    nas<-is.na(ordvar)
    design<-design[!nas,]
    df <- model.frame(design)
    ordvar <- ordvar[!nas]
  }

  ws <- weights(design, "sampling")

  cmprop <- calc.cmprop( x = ordvar, weights = ws )

  N <- sum( ws )
  lin.mat <- sapply( levels(ordvar) , FUN = function( i ) {
    ifelse( ordvar <= i , 1 , 0 ) / N
  } , USE.NAMES = TRUE )
  lin.mat <- as.matrix(lin.mat)

  # reference point
  if ( is.null(ref.point) ) {
    ref.point <- names( calc.medcat( cmprop ) )
    m <- match( ref.point , names( cmprop ) )
    if ( length(m) > 1 ) { stop("non-unique median category. Use refpoint= to set a reference point.") }
  } else {
    m <- match( ref.point , names( cmprop ) )
  }

  # number of categories
  k <- length(cmprop)

  # normalizing constants
  c1 <- b * ( k + 1 - m )
  c2 <- (m - 1)*a/2 - (k + 2 - m)*b/2 + c1

  # estimate Kobus & Milos (2012) ordinal inequality measure
  part1 <- a * ( if ( m > 2 ) rowSums( lin.mat [ , 1:( m - 1 ) ] )  else lin.mat [ , 1 ] )
  part2 <- b * ( if ( m < k ) rowSums( lin.mat [ , m:k ] )  else lin.mat [ , k ] )

  estimate <-
    contrastinf( quote( (A - B + c1)/c2 ),
                 list( A = list( value = a * sum( cmprop[ 1:(m-1) ] ) , lin = part1 ) ,
                       B = list( value = b * sum( cmprop[ m:k ] ), lin = part2 ) ,
                       c1 = list( value = c1 , lin = rep( 0 , length(part1) ) ) ,
                       c2 = list( value = c2 , lin = rep( 0 , length(part1) ) ) ) )

  variance <- survey::svyrecvar(estimate$lin/design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata)

  rval <- estimate$value
  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "kobus-milos measure"
  attr(rval, "reference") <- ref.point

  return( rval )

}


#' @rdname svykmoi
#' @export
svykmoi.svyrep.design <- function ( formula, design, ref.point = NULL , a = 1 , b = 1 , na.rm = FALSE , ... ) {

  if( a < 0 ) stop( "a= parameter has to be greater or equal to zero. " )
  if( b < 0 ) stop( "b= parameter has to be greater or equal to zero. " )

  ordvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if (!("ordered" %in% class(ordvar))) stop( "This function is defined for ordinal variables only. See ?svykmoi for examples." )

  if(na.rm){
    nas<-is.na(ordvar)
    design<-design[!nas,]
    df <- model.frame(design)
    ordvar <- ordvar[!nas]
  }

  ws <- weights(design, "sampling")

  cmprop <- calc.cmprop( x = ordvar, weights = ws )
  # reference point
  if ( is.null(ref.point) ) {
    ref.point <- names( calc.medcat( cmprop ) )
    m <- match( ref.point , names( cmprop ) )
    if ( length(m) > 1 ) { stop("non-unique median category. Use refpoint= to set a reference point.") }
  } else {
    m <- match( ref.point , names( cmprop ) )
  }
  kmoi <- calc.kmoi( cmprop , ref.point = ref.point , a = a , b = b )


  ww <- weights(design, "analysis")
  qq.cmprop <- apply(ww, 2, function(wi) calc.cmprop( ordvar, wi ) )
  qq <- apply( qq.cmprop, 2, function(iter) { calc.kmoi( iter , ref.point = ref.point , a = a , b = b ) } )
  if ( any(is.na(qq))) {

    rval <- NA
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- NA
    attr(rval, "statistic") <- "kobus-milos measure"
    attr(rval, "reference") <- NA

    return(rval)

  } else {
    variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }

  rval <- kmoi
  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "kobus-milos measure"
  attr(rval, "reference") <- if ( is.null(ref.point) ) { names( calc.medcat( cmprop ) ) } else { ref.point }

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

# Kobus-Milos ordinal inequality measure:
calc.kmoi <- function( proportions , ref.point , a = 1 , b = 1 ) {

  k <- length(proportions)
  m <- match( ref.point , names(proportions) )
  gamma <- a * sum( proportions[ 1:(m - 1) ] ) - b * sum( proportions[ m:k ] )
  c1 <- b*( k + 1 - m )
  c2 <- ( m - 1 ) * a/2 - ( k + 2 - m )*b/2 + c1
  (gamma + c1)/c2

}
