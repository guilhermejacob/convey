#' Watts poverty index decomposition (EXPERIMENTAL)
#'
#' Estimate the Watts (1968) poverty measure and its components
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param abs_thresh poverty threshold value if type_thresh is "abs"
#' @param thresh return the poverty threshold value
#' @param na.rm Should cases with missing values be dropped?
#' @param ... passed to \code{svyarpr} and \code{svyarpt}
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvydstat}", with estimates for the Watts index, FGT(0), FGT(1), and Theil(poor incomes) with a "\code{var}" attribute giving the variance-covariance matrix.
#' A "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svywatts},\link{svyfgt},\link{svyfgt}}
#'
#' @references Satya Chakravarty, Joseph Deutsch and Jacques Silber (2008).
#' On the Watts multidimensional poverty index and its decomposition.
#' \emph{World Development}, Vol.36, No.6, pp.1067-1077.
#'
#' Harold W. Watts (1968). An economic definition of poverty.
#' \emph{Institute For Research on Poverty Discussion Papers}, n.5.
#' University of Wisconsin. URL \url{https://www.irp.wisc.edu/publications/dps/pdfs/dp568.pdf}.
#'
#' Guillaume Osier (2009). Variance estimation for complex indicators
#' of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.
#'
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#'
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' # absolute poverty threshold
#' svywattsdec(~eqincome, des_eusilc, abs_thresh=10000)
#'
#' #  using svrep.design:
#' # absolute poverty threshold
#' svywattsdec(~eqincome, des_eusilc_rep, abs_thresh=10000)
#'
#' \dontrun{
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
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' # absolute poverty threshold
#' svywattsdec(~eqincome, dbd_eusilc, abs_thresh=10000)
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svywattsdec <-
  function(formula, design, ...) {

    warning("The svywattsdec function is experimental and is subject to changes in later versions.")

    # if( 'type_thresh' %in% names( list( ... ) ) && !( list(...)[["type_thresh"]] %in% c( 'abs' ) ) ) stop( 'type_thresh= must be "abs". See ?svywattsdec for more detail.' )
    if( !( 'abs_thresh' %in% names( list(...) ) ) ) stop( "abs_thresh= parameter must be specified." )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svywattsdec", design)

  }

#' @rdname svywattsdec
#' @export
svywattsdec.survey.design <-
  function(formula, design, abs_thresh=NULL, na.rm = FALSE, thresh = FALSE, ...){

    # domain
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      if (length(nas) > length(design$prob))incvar <- incvar[!nas] else incvar[nas] <- 0
    } else {
      nas <- rep(0 , nrow( design ) )
    }

    w <- 1/design$prob

    if( any( incvar[w > 0] <= 0 , na.rm = TRUE ) ){
      warning("keeping strictly positive incomes only.")
      nps<-incvar <= 0
      design<-design[!nps,]
      if (length(nps) > length(design$prob)) incvar <- incvar[!nps] else incvar[nps] <- 0
      w <- 1/design$prob
    }

    N <- sum(w)

    # linearization
    th <- abs_thresh

    watts <- suppressWarnings( svywatts( formula , design , type_thresh = "abs" , abs_thresh = abs_thresh ) )
    fgt0 <- svyfgt( formula , design , g = 0 , type_thresh = "abs" , abs_thresh = abs_thresh )
    fgt1 <- svyfgt( formula , design , g = 1 , type_thresh = "abs" , abs_thresh = abs_thresh )

    if ( length( attr(watts , "lin" ) ) < length( attr(fgt0 , "lin" ) ) ) {
      lin <- rep( 0 , length( attr(fgt0 , "lin" ) ) )
      lin[ as.numeric( rownames(design) ) ] <- attr(watts , "lin" )[ attr(watts , "lin" ) != 0 ]
      attr(watts , "lin" ) <- lin ; rm( lin )
    }

    # mean income of the poor
    fgt0 <- list( value = fgt0[[1]], lin = attr( fgt0 , "lin" ) )
    fgt1 <- list( value = fgt1[[1]], lin = attr( fgt1 , "lin" ) )

    th <- list( value = th , lin = rep( 0 , length( fgt0$lin ) ) )

    list_all <- list( fgt0 = fgt0 , fgt1 = fgt1 , th = th )
    mip <- convey::contrastinf( quote( ( ( fgt0 - fgt1 ) * th ) / fgt0 ) , list_all )
    rm(list_all)

    # Watts Poverty Gap Ratio
    W_pgr <- convey::contrastinf( quote( log( th / mip ) ) , list( th = th , mip = mip ) )

    # Theil inequality index of incomes among the poor
    stopifnot( length( incvar ) == length( design$prob ) )
    design_poor <- design[ incvar <= th$value , ]
    ID <- as.numeric( rownames( design_poor ) )
    L_poor <- svygei( formula , design_poor , epsilon = 0 )
    if ( length(fgt0$lin) > length(  attr(L_poor , "lin" ) ) ) {
      lin <- rep( 0 , length(fgt0$lin) )
      lin[ ID ] <- attr(L_poor , "lin" )
      attr(L_poor , "lin" ) <- lin ; rm(lin , design_poor )
    }
    #L_poor <- list( value = L_poor[[1]] , lin = ifelse( incvec <= th$value , attr(L_poor , "lin" ) , 0 ) )
    L_poor <- list( value = L_poor[[1]] , lin = attr( L_poor , "lin" ) )


    test.estimate <- convey::contrastinf( quote( fgt0 * ( W_pgr + L_poor ) ) , list( fgt0 = fgt0 , W_pgr = W_pgr , L_poor = L_poor ) )
    #stopifnot( length(attr(watts, "lin")) == length(test.estimate$lin) )

    lin.matrix <- cbind(test.estimate$lin, fgt0$lin, fgt1$lin , L_poor$lin)
    lin.matrix <- as.matrix( lin.matrix )
    colnames(lin.matrix) <- c( "watts", "fgt0", "fgt1" , "theil(poor)" )

    if ( length(design$prob) > nrow( lin.matrix ) ) {
      lin.matrix <- apply( lin.matrix , 2 , function(x) { y = 1/design$prob ; y[ y > 0 ] <- x ; return( y )  } )
      lin.matrix <- as.matrix( lin.matrix )
    }
    if ( length(design$prob) < nrow( lin.matrix ) ) {
      lin.matrix <- lin.matrix [ as.numeric(rownames(design) ) , ]
    }

    # stopifnot( abs(sqrt(survey::svyrecvar( test.estimate$lin/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)) - sqrt(survey::svyrecvar( attr(watts , "lin") /design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata))) < 10^-10 )

    estimates <- matrix( c( test.estimate$value, fgt0$value, fgt1$value , L_poor$value ), dimnames = list( c( "watts", "fgt0", "fgt1" , "theil(poor)" ) ) )[,]
    variance <- survey::svyrecvar( lin.matrix/design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "SE") <- sqrt(diag(variance[1:4,1:4]))
    attr(rval, "var") <- variance[1:4,1:4]
    attr(rval, "statistic") <- "watts index decomposition"
    class(rval) <- c( "cvydstat" , "cvystat" , "svystat" , "svrepstat" )

    rval


  }


#' @rdname svywattsdec
#' @export
svywattsdec.svyrep.design <-
  function(formula, design, abs_thresh=NULL, na.rm = FALSE, thresh = FALSE, ...){

    # svyrep design ComputeIndex functions
    ComputeWatts <-
      function( y , w , thresh ) {
        y <- y[ w > 0 ]
        w <- w[ w > 0 ]
        N <- sum(w)
        h <- function( y , thresh ) ifelse( y != 0 , ifelse( y <= thresh , log( thresh / y ) , 0 ) , 0 )
        sum( w * h( y , thresh ) ) / N
      }
    ComputeFGT <-
      function( y , w , g , thresh ) {
        y <- y[ w > 0 ]
        w <- w[ w > 0 ]
        N <- sum(w)
        h <- function(y,thresh,g) ( ( ( thresh - y ) / thresh )^g ) * ( y <= thresh )
        sum( w * h( y , thresh , g ) ) / N
      }
    ComputeGEI <-
      function( y , w , epsilon ) {

        y <- y[ w > 0 ]
        w <- w[ w > 0 ]

        if ( epsilon == 0 ) {
          result.est <- -T_fn( y , w , 0 ) / U_fn( y , w , 0 ) + log( U_fn( y , w , 1 ) / U_fn( y , w , 0 ) )
        } else if ( epsilon == 1 ) {
          result.est <- ( T_fn( y , w , 1 ) / U_fn( y , w , 1 ) ) - log( U_fn( y , w , 1 ) / U_fn( y , w , 0 ) )
        } else {
          result.est <- ( epsilon * ( epsilon - 1 ) )^( -1 ) * ( U_fn( y , w , 0 )^( epsilon - 1 ) * U_fn( y , w , 1 )^( -epsilon ) * U_fn( y , w , epsilon ) - 1 )
        }

        result.est

      }

    # domain
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      if (length(nas) > length(design$prob))incvar <- incvar[!nas] else incvar[nas] <- 0
    }

    ws <- weights(design, "sampling")

    if( any( incvar[ws > 0] <= 0 , na.rm = TRUE ) ){
      warning("keeping strictly positive incomes only.")
      nps<-incvar <= 0
      design<-design[!nps,]
      if (length(nps) > length(design$prob)) incvar <- incvar[!nps] else incvar[nps] <- 0
      ws <- weights(design, "sampling")
    }

    # poverty threshold
    th <- abs_thresh

    # estimates
    watts <- ComputeWatts(incvar, ws, thresh = th )
    fgt0 <- ComputeFGT(incvar, ws, g = 0 , thresh = th )
    fgt1 <- ComputeFGT(incvar, ws, g = 1 , thresh = th )
    mip <- sum( incvar * ifelse( incvar <= th , ws , 0 ) ) / sum( ifelse( incvar <= th , ws , 0 ) )
    L_poor <- ComputeGEI( incvar, ifelse( incvar <= th , ws , 0 ) , epsilon = 0 )

    ww <- weights(design, "analysis" )

    # get replicates
    qq.watts <- apply( ww, 2, function(wi){ ComputeWatts( incvar, wi, thresh = th ) } )
    qq.fgt0 <- apply( ww, 2, function(wi){ ComputeFGT( incvar, wi, g = 0 , thresh = th ) } )
    qq.fgt1 <- apply( ww, 2, function(wi){ ComputeFGT( incvar, wi, g = 1 , thresh = th ) } )
    qq.mip <- apply( ww, 2, function(wi){ sum( incvar * ifelse( incvar <= th , wi , 0 ) ) / sum( ifelse( incvar <= th , wi , 0 ) ) } )
    qq.L_poor <- apply( ww, 2, function(wi){ ComputeGEI( incvar, ifelse( incvar <= th , wi , 0 ) , epsilon = 0 ) } )

    qq <- cbind( qq.watts , qq.fgt0 , qq.fgt1 , qq.L_poor )
    colnames(qq) <- c( "watts" , "fgt0" , "fgt1" , "theil(poor)" )

    # test.estimate <- fgt0 * ( log( th / mip ) + L_poor )
    # qq.test.estimate <- qq.fgt0 * ( log( th / qq.mip ) + qq.L_poor )

    if (anyNA(qq)) variance <- NA else variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    estimates <- matrix( c( watts, fgt0, fgt1 , L_poor ), dimnames = list( c( "watts", "fgt0", "fgt1" , "theil(poor)" ) ) )[,]

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "SE") <- sqrt(diag(variance[1:4,1:4]))
    attr(rval, "var") <- variance[1:4,1:4]
    attr(rval, "statistic") <- "watts index decomposition"
    if(thresh) attr(rval, "thresh") <- th
    class(rval) <- c( "cvydstat" , "cvystat" , "svrepstat" , "svystat" )

    rval

  }


#' @rdname svywattsdec
#' @export
svywattsdec.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars( formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset )

    NextMethod("svywattsdec", design)
  }
