#' Richness measures (EXPERIMENTAL)
#'
#' Estimate Peichl, Schaefer and Scheicher (2010) richness measures.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param type_measure A string "Cha", "FGTT1" or "FGTT2" defining the richness measure.
#' @param type_thresh type of richness threshold. If "abs" the threshold is fixed and given the value of abs_thresh; if "relq" it is given by \code{percent} times the quantile; if "relm" it is \code{percent} times the mean.
#' @param abs_thresh richness threshold value if type_thresh is "abs"
#' @param g Richness preference parameter.
#' @param percent the multiple of the quantile or mean used in the richness threshold definition. Defaults to \code{percent = 1.5}; i.e., 1.5 times the quantile or mean.
#' @param quantiles the quantile used used in the richness threshold definition. Defaults to \code{quantiles = .5}, the median.
#' @param thresh return the richness threshold value
#' @param na.rm Should cases with missing values be dropped?
#' @param ... passed to \code{svyarpt}
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svyfgt}}
#'
#' @references Michal Brzezinski (2014). Statistical Inference for Richness Measures. \emph{Applied Economics},
#' Vol. 46, No. 14, pp. 1599-1608, URL \url{http://dx.doi.org/10.1080/00036846.2014.880106}.
#'
#' Andreas Peichl, Thilo Schaefer, and Christoph Scheicher (2010). Measuring richness and poverty: A micro data
#' application to Europe and Germany. \emph{Review of Income and Wealth}, Vol. 56, No.3, pp. 597-619.
#'
#' Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality.
#' \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.
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
#' # concave FGT-like richness measure
#' # headcount ratio, richness threshold fixed
#' svyrich(~eqincome, des_eusilc, type_measure = "FGTT1" , g=0,  abs_thresh=30000)
#' # richness gap index, richness threshold fixed
#' svyrich(~eqincome, des_eusilc, type_measure = "FGTT1" , g=1,  abs_thresh=30000)
#' # headcount ratio, richness threshold equal to the median
#' svyrich(~eqincome, des_eusilc, type_measure = "FGTT1" , g=0, type_thresh= "relq" )
#' # richness gap index, richness threshold equal to the median
#' svyrich(~eqincome, des_eusilc, type_measure = "FGTT1" , g=1, type_thresh= "relq" )
#' # headcount ratio, richness threshold equal to the mean
#' svyrich(~eqincome, des_eusilc, type_measure = "FGTT1" , g=0, type_thresh= "relm" )
#' # richness gap index, richness threshold equal to the mean
#' svyrich(~eqincome, des_eusilc, type_measure = "FGTT1" , g=1, type_thresh= "relm" )
#'
#' #  using svrep.design:
#' # headcount ratio, richness threshold fixed
#' svyrich(~eqincome, des_eusilc_rep, type_measure = "FGTT1" , g=0, abs_thresh=30000 )
#' # richness gap index, richness threshold fixed
#' svyrich(~eqincome, des_eusilc_rep, type_measure = "FGTT1" , g=1, abs_thresh=30000 )
#' # headcount ratio, richness threshold equal to the median
#' svyrich(~eqincome, des_eusilc_rep, type_measure = "FGTT1" , g=0, type_thresh= "relq" )
#' # richness gap index, richness threshold equal to the median
#' svyrich(~eqincome, des_eusilc_rep, type_measure = "FGTT1" , g=1, type_thresh= "relq" )
#' # headcount ratio, richness threshold equal to the mean
#' svyrich(~eqincome, des_eusilc_rep, type_measure = "FGTT1" , g=0, type_thresh= "relm" )
#' # richness gap index, richness threshold equal to the mean
#' svyrich(~eqincome, des_eusilc_rep, type_measure = "FGTT1" , g=1, type_thresh= "relm" )
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
#' # headcount ratio, richness threshold fixed
#' svyrich(~eqincome, dbd_eusilc, type_measure = "FGTT1" , g=0, abs_thresh=30000 )
#' # richness gap index, richness threshold fixed
#' svyrich(~eqincome, dbd_eusilc, type_measure = "FGTT1" , g=1, abs_thresh=30000 )
#' # headcount ratio, richness threshold equal to the median
#' svyrich(~eqincome, dbd_eusilc, type_measure = "FGTT1" , g=0, type_thresh= "relq" )
#' # richness gap index, richness threshold equal to the median
#' svyrich(~eqincome, dbd_eusilc, type_measure = "FGTT1" , g=1, type_thresh= "relq" )
#' # headcount ratio, richness threshold equal to the mean
#' svyrich(~eqincome, dbd_eusilc, type_measure = "FGTT1" , g=0, type_thresh= "relm" )
#' # richness gap index, richness threshold equal to the mean
#' svyrich(~eqincome, dbd_eusilc, type_measure = "FGTT1" , g=1, type_thresh= "relm" )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyrich <-
  function(formula, design, ... ) {

    # warning("The svyrich function is experimental and is subject to changes in later versions.")

    if( !( 'g' %in% names(list(...)) ) ) stop( "g= parameter must be specified" )

    if( !( 'type_measure' %in% names(list(...)) ) ) stop( "type_measure= parameter must be specified" )

    if( 'type_measure' %in% names( list( ... ) ) && !( list(...)[["type_measure"]] %in% c( 'Cha' , 'FGTT1' , 'FGTT2' ) ) ) stop( 'type_measure= must be "Cha", "FGTT1" or "FGTT2". See ?svyrich for more detail.' )

    if( 'type_measure' %in% names( list( ... ) ) && ( list(...)[["type_measure"]] == 'Cha' ) && ( list(...)[["g"]] < 0 ) ) stop( 'type_measure="Cha" is defined for g > 0 only.' )

    if( 'type_measure' %in% names( list( ... ) ) && ( list(...)[["type_measure"]] == 'FGTT1' ) && (( list(...)[["g"]] > 1 ) | ( list(...)[["g"]] < 0 ) ) ) stop( 'type_measure="FGTT1" is defined for 0 <= g <= 1 only.' )

    if( 'type_measure' %in% names( list( ... ) ) && ( list(...)[["type_measure"]] == 'FGTT2' ) && ( list(...)[["g"]] <= 1 ) ) stop( 'type_measure="FGTT2" is defined for g > 1 only.' )

    if( 'type_measure' %in% names( list( ... ) ) && ( list(...)[["type_measure"]] == 'FGTT2' ) ) warning( 'Brzezinski (2014) warns about poor inferential performance for convex richness measures. See ?svyrich for reference.' )

    if( 'type_thresh' %in% names( list( ... ) ) && !( list(...)[["type_thresh"]] %in% c( 'relq' , 'abs' , 'relm' ) ) ) stop( 'type_thresh= must be "relq", "relm" or "abs". See ?svyrich for more detail.' )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svyrich", design)

  }

#' @rdname svyrich
#' @export
svyrich.survey.design <-
  function(formula, design, type_measure , g, type_thresh="abs",  abs_thresh=NULL, percent = 1.5 , quantiles = .50, na.rm = FALSE, thresh = FALSE, ...){

    # check for convey_prep
    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    # check for threshold type
    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # set up richness measures auxiliary functions
    if ( type_measure == "Cha" ) {

      #  survey design h function
      h <- function( y , thresh , g ) ifelse( y > thresh ,  1 - ( thresh / y )^g , 0 )

      # ht function
      ht <- function( y , thresh , g ) ifelse( y > thresh , -(g/thresh) * ( thresh / y )^g , 0 )

    } else if ( type_measure == "FGTT1" ) {

      #  survey design h function
      h <- function( y , thresh , g ) ifelse( y > thresh , ( 1 - thresh / y )^g , 0 )

      # ht function
      ht <- function( y , thresh , g ) ifelse( y > thresh , -g/y * ( 1 - thresh / y )^(g - 1) , 0 )
      # ht <- function( y , thresh , g ) ifelse( y > thresh , -g/y^2 * ( 1 - thresh / y )^(g - 1) , 0 )

    } else if ( type_measure == "FGTT2" ) {

      #  survey design h function
      h <- function( y , thresh , g ) ifelse( y > thresh , ( y  / thresh - 1 )^g , 0 )

      # ht function
      ht <- function( y , thresh , g ) ifelse( y > thresh , (-g*y / ( thresh*y - thresh^2 ) ) * ( y/thresh - 1 )^g , 0 )

    }

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    ### richness threshold calculation

    # collect full sample income
    incvec <- model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    # filter missing values
    if(na.rm){
      nas <- is.na( incvec )
      full_design <- full_design[ !nas , ]
      incvec <- model.frame( formula, full_design$variables, na.action = na.pass )[[1]]
    }

    # collect full sample weights
    wf <- 1/full_design$prob

    # add indices
    if( is.null( names( full_design$prob ) ) ) ncom <- names( full_design$prob ) <- rownames( full_design$variables ) else ncom <- names(full_design$prob)

    # branch on threshold type and its influence function
    if( type_thresh == 'relq' ) {

      ARPT <- svyiqalpha(formula = formula, full_design, alpha=quantiles,  na.rm=na.rm, ...)
      th <- percent * coef(ARPT)[[1]]
      arptlin <- percent * attr( ARPT , "influence" )

    } else if( type_thresh == 'relm') {

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

    ### domain richness measure estimate

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

    # domain population size
    N <- sum( w )

    # compute value
    estimate <- sum( w * h( incvar , th , g ) ) / N

    ### linearization and variance estimation

    # compute linearized variable under fixed threshold
    richlin <- ( w > 0 ) * ( h( incvar , th , g ) - estimate ) / N

    # ensure length
    if ( length( richlin ) != length( full_design$prob ) ) {
      names( richlin ) <- rownames( design$variables )[ w > 0 ]
      richlin <- richlin[ pmatch( rownames( full_design$variables ) , names( richlin ) ) ]
      names( richlin ) <- rownames( full_design$variables )
      richlin[ is.na( richlin ) ] <- 0
    }

    # add linearized threshold
    ID <- 1 *( rownames( full_design$variables ) %in% rownames( design$variables )[1/design$prob > 0 ] )
    richlin1 <- ( h( incvec , th , g ) - estimate ) / N
    Fprime <- - densfun( formula , design = design , th , h = NULL , FUN = "F", na.rm = na.rm )
    ahat <- sum( w * ht( incvar , th , g ) ) / N
    ahF <- ahat + h( th , th , g ) * Fprime
    if ( type_thresh %in% c( "relq" , "relm" ) ) richlin2 <- ahF * arptlin else richlin2 <- 0
    richlin <- ID * richlin1 + richlin2

    # compute variance
    variance <- survey::svyrecvar( richlin/full_design$prob, full_design$cluster, full_design$strata, full_design$fpc, postStrata = full_design$postStrata )
    variance[ which( is.nan( variance ) ) ] <- NA
    colnames( variance ) <- rownames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

    # keep necessary influence functions
    richlin <- richlin[ 1/full_design$prob > 0 ]

    # setup result object
    rval <- estimate
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- paste0( type_measure, "-" , g, "-richness measure" )
    attr(rval, "influence") <- richlin
    if(thresh) attr(rval, "thresh") <- th
    rval

  }



#' @rdname svyrich
#' @export
svyrich.svyrep.design <-
  function(formula, design, type_measure, g, type_thresh="abs", abs_thresh=NULL, percent = 1.5 , quantiles = .50, na.rm = FALSE, thresh = FALSE,...) {

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    # svyrep design h function
    if ( type_measure == "Cha" ) {
      h <- function( y , thresh , g ) ifelse( y > thresh ,  1 - ( thresh / y )^g , 0 )
    } else if ( type_measure == "FGTT1" ) {
      h <- function( y , thresh , g ) ifelse( y > thresh , ( 1 - thresh / y )^g , 0 )
    } else if ( type_measure == "FGTT2" ) {
      h <- function( y , thresh , g ) ifelse( y > thresh , ( y  / thresh - 1 )^g , 0 )
    }

    # svyrep design ComputeRich function
    ComputeRich <-
      function( y , w , thresh , g ){
        N <- sum(w)
        sum( w * h( y , thresh , g ) ) / N
      }

    df <- model.frame(design)
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
    }

    ws <- weights(design, "sampling")

    df_full<- model.frame(full_design)
    incvec <- model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      df_full <- model.frame(full_design)
      incvec <- incvec[!nas]
    }

    wsf <- weights(full_design,"sampling")
    names(incvec) <- names(wsf) <- row.names(df_full)
    ind<- row.names(df)

    # poverty threshold
    if(type_thresh=='relq') th <- percent * computeQuantiles( incvec, wsf, p = quantiles)
    if(type_thresh=='relm') th <- percent*sum(incvec*wsf)/sum(wsf)
    if(type_thresh=='abs') th <- abs_thresh


    rval <- ComputeRich(incvar, ws, g = g, th)

    wwf <- weights(full_design, "analysis")

    qq <-
      apply(wwf, 2, function(wi){
        names(wi)<- row.names(df_full)
        wd<-wi[ind]
        incd <- incvec[ind]
        ComputeRich(incd, wd, g = g, th)}
      )
    if(anyNA(qq))variance <- NA
    else variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- paste0( type_measure, "-" , g, "-richness measure" )
    if(thresh) attr(rval, "thresh") <- th
    rval
  }

#' @rdname svyrich
#' @export
svyrich.DBIsvydesign <-
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

    NextMethod("svyrich", design)
  }
