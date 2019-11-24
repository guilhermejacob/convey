#' J-divergence Decomposition
#'
#' Estimates the subgroup decomposition of the J-divergence
#'
#' @param formula a formula specifying the income variable
#' @param subgroup a formula specifying the group variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped? Observations containing missing values in income or subgroup variable will be dropped.
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' This measure only allows for strictly positive variables.
#'
#' @return Object of class "\code{cvydstat}", which are vectors with a "\code{var}" attribute giving the variance-covariance matrix and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob
#'
#' @seealso \code{\link{svyjdiv}}
#'
#' @references Anthony F. Shorrocks (1984). Inequality decomposition
#' by population subgroups. \emph{Econometrica}, v. 52, n. 6, 1984, pp. 1369-1385.
#' URL \url{http://www.jstor.org/stable/1913511}.
#'
#' Nicholas Rohde (2016). J-divergence measurements of economic inequality.
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
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' # linearized design
#' svyjdivdec( ~eqincome , ~rb090 , subset(des_eusilc, eqincome > 0) )
#'
#' # replicate-weighted design
#' svyjdivdec( ~eqincome , ~rb090 , subset(des_eusilc_rep, eqincome > 0) )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' sub_des_eusilc <- subset(des_eusilc, py010n > 0 | is.na(py010n) )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc , na.rm = TRUE )
#'
#' # replicate-weighted design using a variable with missings
#' sub_des_eusilc_rep <- subset(des_eusilc_rep, py010n > 0 | is.na(py010n) )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc_rep )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc_rep , na.rm = TRUE )
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
#' # database-backed linearized design
#' svyjdivdec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) )
#'
#' # database-backed linearized design using a variable with missings
#' sub_dbd_eusilc <- subset(dbd_eusilc, py010n > 0 | is.na(py010n) )
#' svyjdivdec( ~py010n , ~rb090 , sub_dbd_eusilc )
#' svyjdivdec( ~py010n , ~rb090 , sub_dbd_eusilc , na.rm = TRUE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyjdivdec <-
  function( formula, subgroup, design, ...) {

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    if( length( attr( terms.formula( subgroup ) , "term.labels" ) ) > 1 ) stop( "the `subgroup=` argument must have only one variable" )

    UseMethod("svyjdivdec", design)

  }

#' @rdname svyjdivdec
#' @export
svyjdivdec.survey.design <-
  function ( formula, subgroup, design, na.rm = FALSE, ... ) {

    w <- 1/design$prob
    ff <- sapply( list( formula , subgroup ) , function(z) attr( terms.formula( z ) , "term.labels" ) )
    ff <- as.formula( paste0( "~" , paste( ff , collapse = " + ") ) )
    mm <- model.frame( ff , design$variables, na.action = na.pass , drop.unused.levels = TRUE )

    if( class( mm[ , 2] ) != "factor" ) stop( "the `subgroup=` argument must specify a factor variable" )

    if (na.rm) {
      nas <- rowSums( is.na( mm ) ) > 0
      design <- design[nas == 0, ]
      w <- 1/design$prob
      mm <- model.frame( ff , design$variables, na.action = na.pass , drop.unused.levels = TRUE )
    }

    if ( any( is.na( mm ) [ w > 0 , ] ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( rep(NA,9), ncol = 3, dimnames = list( c( "total", "within", "between" ), c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "j-divergence decomposition"
      attr(rval,"group")<- as.character( subgroup )[[2]]
      class(rval) <- c( "cvydstat" , "cvystat" , "svystat" )

      return(rval)

    }

    incvar <- mm[ , 1 ]
    gmat <- model.matrix( update( subgroup , ~.+0 ) , mm )
    gmat[ is.na( gmat ) ] <- 0
    if ( !all( apply( gmat [  w > 0 , ] , 2 , sum ) > 0 ) ) gmat <- gmat[ , apply( gmat [  w > 0 , ] , 2 , sum ) > 0 ]
    rm( mm )
    if ( any( incvar[ w > 0 ] <= 0, na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes only." )

    # internal functions
    gei0_efun <- function( y , w ) {
      N <- sum( w )
      mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
      - sum( ifelse( w > 0 , w * log( y / mu ) , 0 ) ) / N
    }
    gei1_efun <- function( y , w ) {
      N <- sum( w )
      mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
      sum( ifelse( w > 0 , w * ( y / mu ) * log( y / mu ) , 0 ) ) / N
    }
    gei0_linfun <- function( y , w ) {
      N <- sum( w )
      mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
      gei0 <- - sum( ifelse( w > 0 , w * log( y / mu ) , 0 ) ) / N
      ifelse( w > 0 , -(1/N) * ( log( y / mu ) + gei0 ) + (1/mu) * ( y - mu ) / N , 0 )
    }
    gei1_linfun <- function( y , w ) {
      N <- sum( w )
      mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
      gei1 <- sum( ifelse( w > 0 , w * ( y / mu ) * log( y / mu ) , 0 ) ) / N
      ifelse( w > 0 , (1/N) * ( ( y / mu ) * log( y / mu ) - gei1 ) - (1/mu) *( gei1 + 1 ) * ( y - mu ) / N , 0 )
    }
    phi_efun <- function( y , w , alpha , ind ) {
      N_j <- sum( ifelse( ( ind * w ) > 0 , w , 0 ) )
      N <- sum( ifelse( w > 0 , w , 0 ) )
      Y_j <- sum( ifelse( ( ind * w ) > 0 , w * y , 0 ) )
      Y <- sum( ifelse( w > 0 , w * y , 0 ) )
      sest <- Y_j / Y ; pest <- N_j / N
      sest^alpha * pest^( 1 - alpha )
    }
    phi_linfun <- function( y , w , alpha , ind ) {
      N_j <- sum( ifelse( ( ind * w ) > 0 , w , 0 ) )
      N <- sum( ifelse( w > 0 , w , 0 ) )
      Y_j <- sum( ifelse( ( ind * w ) > 0 , w * y , 0 ) )
      Y <- sum( ifelse( w > 0 , w * y , 0 ) )
      sest <- Y_j / Y ; pest <- N_j / N
      phiest <- sest^alpha * pest^( 1 - alpha )
      slin <- ifelse( w > 0 , (1/Y) * ( ind * y - sest * y ) , 0 )
      plin <- ifelse( w > 0 , (1/N) * ( ind - pest ) , 0 )
      ifelse( w  > 0 , phiest * ( alpha * slin / sest + ( 1 - alpha ) * plin / pest ) , 0 )
    }

    # estimates
    ttl.jdiv <- gei1_efun( incvar , w ) + gei0_efun( incvar , w )
    within.jdiv  <- apply( gmat, 2 , function( ind ) {
      phi_efun( incvar , w , alpha = 0 , ind = ind ) * gei0_efun( incvar , w * ind ) +
        phi_efun( incvar , w , alpha = 1 , ind = ind ) * gei1_efun( incvar , w * ind )
    } )
    within.jdiv <- sum( within.jdiv )
    between.jdiv <- ttl.jdiv - within.jdiv

    # linearization
    ttl.jdiv.lin <- gei0_linfun( incvar , w ) + gei1_linfun( incvar , w )
    within.jdiv.lin  <- apply( gmat , 2 , function( ind ) {
      phi0 <- phi_efun( incvar , w , alpha = 0 , ind = ind )
      gei0 <- gei0_efun( incvar , w * ind )
      phi1 <- phi_efun( incvar , w , alpha = 1 , ind = ind )
      gei1 <- gei1_efun( incvar , w * ind )
      phi0_lin <- phi_linfun( incvar , w , alpha = 0 , ind = ind )
      gei0_lin <- gei0_linfun( incvar , w * ind )
      phi1_lin <- phi_linfun( incvar , w , alpha = 1 , ind = ind )
      gei1_lin <- gei1_linfun( incvar , w * ind )
      pt0 <- gei0 * phi0_lin + gei0_lin * phi0
      pt1 <- gei1 * phi1_lin + gei1_lin * phi1
      ifelse( w > 0 , pt0 + pt1 , 0 )
    } )
    within.jdiv.lin <- rowSums( within.jdiv.lin )
    between.jdiv.lin <- ttl.jdiv.lin - within.jdiv.lin

    lin.matrix <- matrix( data = c(ttl.jdiv.lin, within.jdiv.lin, between.jdiv.lin), ncol = 3, dimnames = list( NULL, c( "total", "within", "between" ) ) )

    estimates <- matrix( c( ttl.jdiv, within.jdiv, between.jdiv ), dimnames = list( c( "total", "within", "between" ) ) )[,]
    variance <- survey::svyrecvar( lin.matrix/design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence decomposition"
    attr(rval,"group")<- as.character( subgroup )[[2]]
    class(rval) <- c( "cvydstat" , "cvystat" , "svystat" )

    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.svyrep.design <-
  function( formula, subgroup, design, na.rm=FALSE, ...) {

    # aux funs
    gei0_efun <- function( y , w ) {
      N <- sum( w )
      mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
      - sum( ifelse( w > 0 , w * log( y / mu ) , 0 ) ) / N
    }
    gei1_efun <- function( y , w ) {
      N <- sum( w )
      mu <- sum( ifelse( w > 0 , w * y , 0 ) ) / N
      sum( ifelse( w > 0 , w * ( y / mu ) * log( y / mu ) , 0 ) ) / N
    }
    phi_efun <- function( y , w , alpha , ind ) {
      N_j <- sum( ifelse( ( ind * w ) > 0 , w , 0 ) )
      N <- sum( ifelse( w > 0 , w , 0 ) )
      Y_j <- sum( ifelse( ( ind * w ) > 0 , w * y , 0 ) )
      Y <- sum( ifelse( w > 0 , w * y , 0 ) )
      sest <- Y_j / Y ; pest <- N_j / N
      sest^alpha * pest^( 1 - alpha )
    }

    ws <- weights(design, "sampling")
    ff <- sapply( list( formula , subgroup ) , function(z) attr( terms.formula( z ) , "term.labels" ) )
    ff <- as.formula( paste0( "~" , paste( ff , collapse = " + ") ) )
    mm <- model.frame( ff , design$variables, na.action = na.pass , drop.unused.levels = TRUE )

    if( class( mm[ , 2] ) != "factor" ) stop( "the `subgroup=` argument must specify a factor variable" )

    if (na.rm) {
      nas <- rowSums( is.na( mm ) & ws > 0 ) > 0
      design <- design[nas == 0, ]
      mm <- model.frame( ff , design$variables, na.action = na.pass , drop.unused.levels = TRUE )
      ws <- weights(design, "sampling")
    }

    if ( any( is.na( mm ) [ ws > 0 , ] ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( rep(NA,9), ncol = 3, dimnames = list( c( "total", "within", "between" ), c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "j-divergence decomposition"
      attr(rval,"group")<- as.character( subgroup )[[2]]
      class(rval) <- c( "cvydstat" , "cvystat" , "svystat" )

      return(rval)

    }

    incvar <- mm[ , 1 ]
    gmat <- model.matrix( update( subgroup , ~.+0 ) , mm )
    gmat[ is.na( gmat ) ] <- 0
    if ( !all( apply( gmat [  ws > 0 , ] , 2 , sum ) > 0 ) ) gmat <- gmat[ , apply( gmat [  ws > 0 , ] , 2 , sum ) > 0 ]
    rm( mm )
    if ( any( incvar[ ws > 0 ] <= 0, na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes only." )

    ww <- weights(design, "analysis")

    # Total
    ttl.jdiv <- gei1_efun( incvar , ws ) + gei0_efun( incvar , ws )
    qq.ttl.jdiv <- apply(ww, 2, function(wi) gei1_efun( incvar , wi ) + gei0_efun( incvar , wi ) )

    # within
    within.jdiv <- sum( apply( gmat, 2 , function( ind ) {
      phi_efun( incvar , ws , alpha = 0 , ind = ind ) * gei0_efun( incvar , ws * ind ) +
        phi_efun( incvar , ws , alpha = 1 , ind = ind ) * gei1_efun( incvar , ws * ind )
    } ) )
    qq.within.jdiv <- apply( ww, 2, function(wi) {

      sum( apply( gmat, 2 , function( ind ) {
        phi_efun( incvar , wi , alpha = 0 , ind = ind ) * gei0_efun( incvar , wi * ind ) +
          phi_efun( incvar , wi , alpha = 1 , ind = ind ) * gei1_efun( incvar , wi * ind )
      } ) )

    } )

    # Between:
    between.jdiv <- ttl.jdiv - within.jdiv
    qq.between.jdiv <- qq.ttl.jdiv - qq.within.jdiv

    # variance
    qq.matrix <- matrix( c( qq.ttl.jdiv, qq.within.jdiv, qq.between.jdiv ), ncol = 3, dimnames = list( NULL, c( "total", "within", "between" ) ) )
    variance <- survey::svrVar( qq.matrix, design$scale, design$rscales, mse = design$mse, coef = matrix( ttl.jdiv, within.jdiv, between.jdiv ) )

    rval <- list( estimate = matrix( c( ttl.jdiv, within.jdiv, between.jdiv ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence decomposition"
    attr(rval,"group")<- as.character( subgroup )[[2]]
    class(rval) <- c( "cvydstat" , "cvystat" , "svrepstat" , "svystat" )
    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.DBIsvydesign <-
  function (formula, subgroup, design, ...) {

    design$variables <-
      cbind(
        getvars(formula, design$db$connection,design$db$tablename, updates = design$updates, subset = design$subset),
        getvars(subgroup, design$db$connection, design$db$tablename,updates = design$updates, subset = design$subset)
      )

    NextMethod("svyjdivdec", design)

  }

