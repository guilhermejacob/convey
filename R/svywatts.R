#' Watts measure of poverty
#'
#' Estimate the Watts measure for the cases: \code{alpha=0} headcount ratio and \code{alpha=1} poverty gap index.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param type_thresh type of poverty threshold. If "abs" the threshold is fixed and given the value
#' of abs_thresh; if "relq" it is given by percent times the quantile; if "relm" it is percent times the mean.
#' @param abs_thresh poverty threshold value if type_thresh is "abs"
#' @param percent the multiple of the the quantile or mean used in the poverty threshold definition
#' @param quantiles the quantile used used in the poverty threshold definition
#' @param thresh return the poverty threshold value
#' @param na.rm Should cases with missing values be dropped?
#' @param ... passed to \code{svyarpr} and \code{svyarpt}
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob
#'
#' @seealso \code{\link{svyarpt}}
#'
#' @references Harold W. Watts (1968). An economic definition of poverty.
#' \emph{Institute For Research on Poverty Discussion Papers}, n.5.
#' University of Wisconsin. URL \url{https://www.irp.wisc.edu/publications/dps/pdfs/dp568.pdf}.
#'
#' Buhong Zheng (2001). Statistical inference for poverty measures with relative poverty lines.
#' \emph{Journal of Econometrics}, Vol. 101, pp. 337-356.
#'
#' Vijay Verma and Gianni Betti (2011). Taylor linearization sampling errors and design effects for poverty measures
#' and other complex statistics. \emph{Journal Of Applied Statistics}, Vol.38, No.8, pp. 1549-1576,
#' URL \url{http://dx.doi.org/10.1080/02664763.2010.515674}.
#'
#' Anthony B. Atkinson (1987). On the measurement of poverty.
#' \emph{Econometrica}, Vol.55, No.4, (Jul., 1987), pp. 749-764,
#' URL \url{http://www.jstor.org/stable/1911028}.
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
#' library(laeken)
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
#' # filter positive incomes
#' des_eusilc <- subset( des_eusilc , eqincome > 0 )
#' des_eusilc_rep <- subset( des_eusilc_rep , eqincome > 0 )
#'
#' # poverty threshold fixed
#' svywatts(~eqincome, des_eusilc ,  abs_thresh=10000)
#' # poverty threshold equal to arpt
#' svywatts(~eqincome, des_eusilc , type_thresh= "relq", thresh = TRUE)
#' # poverty threshold equal to 0.6 times the mean
#' svywatts(~eqincome, des_eusilc , type_thresh= "relm" , thresh = TRUE)
#' # using svrep.design:
#' # poverty threshold fixed
#' svywatts(~eqincome, des_eusilc_rep  ,  abs_thresh=10000)
#' # poverty threshold equal to arpt
#' svywatts(~eqincome, des_eusilc_rep  , type_thresh= "relq", thresh = TRUE)
#' # poverty threshold equal to 0.6 times the mean
#' svywatts(~eqincome, des_eusilc_rep  , type_thresh= "relm" , thresh = TRUE)
#'
#' \dontrun{
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
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' # poverty threshold fixed
#' svywatts(~eqincome, dbd_eusilc ,  abs_thresh=10000)
#' # poverty threshold equal to arpt
#' svywatts(~eqincome, dbd_eusilc , type_thresh= "relq", thresh = TRUE)
#' # poverty threshold equal to 0.6 times the mean
#' svywatts(~eqincome, dbd_eusilc , type_thresh= "relm" , thresh = TRUE)
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svywatts <-
  function(formula, design,  ...) {

    if( 'type_thresh' %in% names( list( ... ) ) && !( list(...)[["type_thresh"]] %in% c( 'relq' , 'abs' , 'relm' ) ) ) stop( 'type_thresh= must be "relq" "relm" or "abs".  see ?svywatts for more detail.' )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svywatts", design)

  }

#' @rdname svywatts
#' @export
svywatts.survey.design <-
  function(formula, design, type_thresh="abs",  abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE, ...){

    # check for convey_prep
    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    # check for threshold type
    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    #  survey design h function
    h <- function( y , thresh ) ifelse( y <= thresh , log( thresh / y ) , 0 )

    # ht function
    ht <- function( y , thresh ) ifelse( y <= thresh , 1 / thresh , 0 )

    ### threshold calculation

    # collect income from full sample
    incvec <- model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    # treat missing values
    if(na.rm){
      nas<-is.na( incvec )
      full_design<-full_design[!nas,]
      if ( length(nas) > length( full_design$prob ) ) incvec <- incvec[!nas] else incvec[nas] <- 0
    }

    # collect full sample weights
    wf <- 1/full_design$prob

    # branch on threshold type and its influence function
    if( type_thresh == 'relq' ) {

      ARPT <- svyiqalpha(formula = formula, full_design, alpha=quantiles,  na.rm=na.rm, ...)
      th <- percent * coef(ARPT)[[1]]
      arptlin <- percent * attr( ARPT , "influence" )

    } else if( type_thresh == 'relm') {

      # ARPT <- svymean( formula, full_design , influence = TRUE ,  na.rm=na.rm )
      # th <- percent * coef( ARPT )[[1]]
      # arptlin <- (wf > 0) * ( percent * incvec - th ) / sum( wf )
      # names( arptlin ) <- rownames( full_design$variables )[ 1/full_design$prob > 0 ]

      Yf <- sum( wf * incvec )
      Nf <- sum( wf )
      muf <- Yf/Nf
      th <- percent * muf
      arptlin <- ( wf > 0 ) * percent * ( incvec - muf ) / Nf
      names( arptlin ) <- rownames( full_design$variables )[ wf > 0 ]

    } else if ( type_thresh == 'abs' ) {
      th <- abs_thresh
      arptlin <- rep( 0 , length( wf ) )
    }

    ### domain poverty measure estimate

    # collect income from domain sample
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    # treat missing values
    if ( na.rm ){
      nas <- is.na( incvar )
      design <- design[!nas,]
      if ( length(nas) > length( design$prob ) ) incvar <- incvar[!nas] else incvar[nas] <- 0
    }

    # collect full sample weights
    w <- 1/design$prob

    # check for strictly positive incomes
    if ( any( incvar[ w > 0 ] <= 0, na.rm = TRUE) ) stop( "The Watts measure is defined for strictly positive variables only.\nNegative and zero values not allowed." )

    # domain population size
    N <- sum( w )

    # compute value
    estimate <- sum( ifelse( w > 0 , w * h( incvar , th ) , 0 ) ) / N

    ### linearization

    # compute linearized variable under fixed threshold
    wattslin <- ifelse( w > 0 , w * h( incvar , th ) , 0 ) / N

    # ensure length
    if ( length( wattslin ) != length( full_design$prob ) ) {
      names( wattslin ) <- rownames( design$variables )[ w > 0 ]
      wattslin <- wattslin[ pmatch( rownames( full_design$variables ) , names( wattslin ) ) ]
      names( wattslin ) <- rownames( full_design$variables )
      wattslin[ is.na( wattslin ) ] <- 0
    }

    # add linearized threshold
    ID <- 1 *( rownames( full_design$variables ) %in% rownames( design$variables )[ 1/design$prob > 0 ] )
    wattslin1 <-  ( ifelse( wf > 0 , h( incvec , th ) , 0 ) - estimate ) / N
    Fprime <- densfun( formula , design = design , th , h = NULL , FUN = "F", na.rm = na.rm )
    ahat <- sum( w * ifelse( w > 0 , ht( incvar , th ) , 0 ) ) / N
    ahF <- ahat + h( th , th ) * Fprime
    if ( type_thresh %in% c( "relq" , "relm" ) ) wattslin2 <- ahF * arptlin else wattslin2 <- 0
    wattslin <- ID * wattslin1 + wattslin2

    # compute variance
    variance <- survey::svyrecvar( wattslin/full_design$prob, full_design$cluster, full_design$strata, full_design$fpc, postStrata = full_design$postStrata )
    variance[ which( is.nan( variance ) ) ] <- NA
    colnames( variance ) <- rownames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

    options( error =recover )

    # keep necessary influence functions
    wattslin <- wattslin[ 1/full_design$prob > 0 ]

    # setup result object
    rval <- estimate
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "watts"
    attr(rval, "influence") <- wattslin
    if(thresh) attr(rval, "thresh") <- th
    rval

  }



#' @rdname svywatts
#' @export
svywatts.svyrep.design <-
  function(formula, design, type_thresh="abs", abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE,...) {

    # checked for convey_prep
    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

    # check for threshold type
    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    # svyrep design h function
    h <- function( y , thresh ) ifelse( y <= thresh , log( thresh / y ) , 0 )

    # svyrep design ComputeWatts function
    ComputeWatts <-
      function(y, w, thresh){
        y <- y[w>0]
        w <- w[w>0]
        N <- sum(w)
        sum( w * h( y , thresh ) ) / N
      }

    # collect domain data
    df <- model.frame(design)
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    # treat missing values
    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
    }

    # collect domain sampling weights
    ws <- weights(design, "sampling")
    names(ws) <- rownames(design$variables)

    # collect data from full sample
    df_full<- model.frame(full_design)
    incvec <- model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    # treat missing values
    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      df_full <- model.frame(full_design)
      incvec <- incvec[!nas]
    }

    # collect weights from full sample
    wsf <- weights(full_design,"sampling")
    names(incvec) <- names(wsf) <- row.names(df_full)
    ind<- row.names(df)

    # poverty threshold
    if(type_thresh=='relq') th <- percent * computeQuantiles( incvec, wsf, p = quantiles)
    if(type_thresh=='relm') th <- percent*sum(incvec*wsf)/sum(wsf)
    if(type_thresh=='abs') th <- abs_thresh

    # check for strictly positive incomes
    if ( any( incvar[ ws > 0 ] <= 0, na.rm = TRUE) ) stop( "The Watts measure is defined for strictly positive variables only.\nNegative and zero values not allowed." )

    # compute point estimate
    rval <- ComputeWatts(incvar, ws, th)

    # collect full sample analysis weights
    wwf <- weights(full_design, "analysis")

    # calculate replicates
    qq <- apply( wwf, 2, function(wi) {

      names( wi ) <- row.names( df_full )

      if(type_thresh=='relq') thr <- percent * computeQuantiles( incvec , wi , p = quantiles )
      if(type_thresh=='relm') thr <- percent*sum( incvec*wi )/sum( wi )
      if(type_thresh=='abs')  thr <- abs_thresh
      wsi <- ifelse( names( wi ) %in% names( ws ) , wi , 0 )

      ComputeWatts( incvec , wsi , thr )

    } )

    # treat missing
    if ( anyNA( qq ) ) {
      variance <-  as.matrix( NA )
      names( rval ) <- names( variance ) <- rownames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      class(rval) <- c( "cvystat" , "svrepstat" )
      attr( rval , "var" ) <- variance
      attr(rval, "statistic") <- "watts"
      if(thresh) attr(rval, "thresh") <- th
      return( rval )
    }

    # calculate variance
    variance <- survey::svrVar( qq, full_design$scale , full_design$rscales, mse = full_design$mse, coef = rval )

    # setup result object
    names( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "watts"
    if(thresh) attr(rval, "thresh") <- th
    rval

  }

#' @rdname svywatts
#' @export
svywatts.DBIsvydesign <-
  function (formula, design, ...){

    if (!( "logical" %in% class(attr(design, "full_design"))) ){

      full_design <- attr( design , "full_design" )

      full_design$variables <-
        getvars(
          formula,
          attr( design , "full_design" )$db$connection,
          attr( design , "full_design" )$db$tablename,
          updates = attr( design , "full_design" )$updates,
          subset = attr( design , "full_design" )$subset
        )

      attr( design , "full_design" ) <- full_design

      rm( full_design )

    }

    design$variables <-
      getvars(
        formula,
        design$db$connection,
        design$db$tablename,
        updates = design$updates,
        subset = design$subset
      )

    NextMethod("svywatts", design)
  }

