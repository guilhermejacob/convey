#' J-Divergence Decomposition
#'
#' Estimates the group decomposition of the generalized entropy index
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param by.formula a formula specifying the group variable
#' @param na.rm Should cases with missing values be dropped? Observations containing missing values in income or group variables will be dropped.
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvydstat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
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
#' svyjdivdec( ~eqincome , subset(des_eusilc, eqincome > 0) , ~rb090 )
#'
#' # replicate-weighted design
#' svyjdivdec( ~eqincome , subset(des_eusilc_rep, eqincome > 0) , ~rb090 )
#'
#' # linearized design using a variable with missings
#' sub_des_eusilc <- subset(des_eusilc, py010n > 0 | is.na(py010n))
#' svyjdivdec( ~py010n , sub_des_eusilc , ~rb090 )
#' svyjdivdec( ~py010n , sub_des_eusilc , ~rb090 , na.rm = TRUE )
#'
#' # replicate-weighted design using a variable with missings
#' sub_des_eusilc_rep <- subset(des_eusilc_rep, py010n > 0 | is.na(py010n))
#' svyjdivdec( ~py010n , sub_des_eusilc_rep , ~rb090 )
#' svyjdivdec( ~py010n , sub_des_eusilc_rep , ~rb090 , na.rm = TRUE )
#'
#' # library(MonetDBLite) is only available on 64-bit machines,
#' # so do not run this block of code in 32-bit R
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
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' # database-backed linearized design
#' svyjdivdec( ~eqincome , subset(dbd_eusilc, eqincome > 0) , ~rb090 )
#'
#' # database-backed linearized design using a variable with missings
#' sub_dbd_eusilc <- subset(dbd_eusilc, py010n > 0 | is.na(py010n) )
#' svyjdivdec( ~py010n , sub_dbd_eusilc , ~rb090 )
#' svyjdivdec( ~py010n , sub_dbd_eusilc , ~rb090 , na.rm = TRUE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyjdivdec <-
  function( formula, design, by.formula,  ...) {

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    if( length( attr( terms.formula( by.formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `by.formula=` argument" )

    UseMethod("svyjdivdec", design)

  }

#' @rdname svyjdivdec
#' @export
svyjdivdec.survey.design <-
  function ( formula, design, by.formula, na.rm = FALSE, ... ) {

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    w <- 1/design$prob
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[,]
    grpvar <- model.frame( by.formula, design$variables, na.action = na.pass)[,]

    if (na.rm) {
      nas <- ( is.na(incvar) | is.na(grpvar ) )
      design <- design[nas == 0, ]
      w <- 1/design$prob
      incvar <- model.frame(formula, design$variables, na.action = na.pass)[,]
      grpvar <- model.frame( by.formula, design$variables, na.action = na.pass)[,]
    }


    if ( any( incvar[ w != 0 ] <= 0, na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes only." )

    incvar <- incvar[ w > 0 ]
    grpvar <- grpvar[ w > 0 ]
    w <- w[ w > 0 ]

    if ( any( any( is.na(incvar) | is.na(grpvar ) ) & (w > 0) ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( c(NA,NA,NA), dimnames = list( c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "j-divergence decomposition"
      attr(rval,"group")<- as.character( by.formula )[[2]]
      class(rval) <- c( "cvydstat" )

      return(rval)

    }

    grpvar <- interaction( grpvar )

    # total
    U_0 <- list( value = sum( w ), lin = rep( 1, length( incvar ) ) )
    U_1 <- list( value = sum( w * incvar ), lin = incvar )
    T_0 <- list( value = sum( w * log( incvar ) ), lin = log( incvar ) )
    T_1 <- list( value = sum( w * incvar * log( incvar ) ), lin = incvar * log( incvar ) )
    Y_AVG <- contrastinf( quote( U_1 / U_0 ), list(  U_0 = U_0, U_1 = U_1 ) )

    list_all <- list(  U_0 = U_0, U_1 = U_1, T_0 = T_0, T_1 = T_1, Y_AVG = Y_AVG )
    estimate <- contrastinf( quote( ( 1 / U_1 ) * ( T_1 - ( Y_AVG * T_0 ) - ( log( Y_AVG ) * U_1 - Y_AVG * log( Y_AVG ) * U_0 ) ) ) , list_all )

    ttl.jdiv <- estimate$value
    ttl.jdiv.lin <- 1/design$prob
    ttl.jdiv.lin[ ttl.jdiv.lin > 0 ] <- estimate$lin


    ttl.variance <- survey::svyrecvar( ttl.jdiv.lin/design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata )

    # within:

    # Theil T index:
    grp.theilt <- NULL
    grp.theilt.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.theilt.wtd <- NULL
    grp.theilt.wtd.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )

    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      U_0_i <- list( value = sum( w_i ), lin = rep( 1, length( incvar ) ) )
      U_1_i <- list( value = sum( w_i * incvar ), lin = incvar )
      T_0_i <- list( value = sum( w_i * log( incvar ) ), lin = log( incvar ) )
      T_1_i <- list( value = sum( w_i * incvar * log( incvar ) ), lin = incvar * log( incvar ) )
      Y_AVG_i <- contrastinf( quote( U_1_i / U_0_i ), list( U_0_i = U_0_i , U_1_i = U_1_i ) )

      list_all <- list(  U_0 = U_0, U_1 = U_1, T_0 = T_0, T_1 = T_1, Y_AVG_i = Y_AVG_i, U_1_i = U_1_i, T_1_i = T_1_i )
      estimate <- contrastinf( quote( ( 1 / U_1_i ) * ( T_1_i - log( Y_AVG_i ) * U_1_i ) ) , list_all )

      grp.theilt[i] <- estimate$value
      grp.theilt.lin[,i] <- estimate$lin * ( w_i != 0 )

      estimate <- contrastinf( quote( grp_theilt * U_1_i / U_1 ) , list( grp_theilt = estimate, U_1_i = U_1_i, U_1 = U_1 ) )
      grp.theilt.wtd[i] <- estimate$value
      grp.theilt.wtd.lin[,i] <- estimate$lin * ( w_i != 0 )

      rm(i, w_i, estimate)

    }

    wtn.theilt <- sum( grp.theilt.wtd )
    w_teste <- 1/design$prob
    w_teste[ w_teste > 0 ] <- apply( grp.theilt.wtd.lin, 1, sum )
    wtn.theilt.lin <- w_teste ; rm( w_teste )


    # Theil L index:
    grp.theill <- NULL
    grp.theill.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.theill.wtd <- NULL
    grp.theill.wtd.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )

    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      U_0_i <- list( value = sum( w_i ), lin = rep( 1, length( incvar ) ) )
      U_1_i <- list( value = sum( w_i * incvar ), lin = incvar )
      T_0_i <- list( value = sum( w_i * log( incvar ) ), lin = log( incvar ) )
      T_1_i <- list( value = sum( w_i * incvar * log( incvar ) ), lin = incvar * log( incvar ) )
      Y_AVG_i <- contrastinf( quote( U_1_i / U_0_i ), list(  U_0_i = U_0_i, U_1_i = U_1_i ) )

      list_all <- list(  U_0 = U_0, U_1 = U_1, T_0 = T_0, T_1 = T_1, Y_AVG_i = Y_AVG_i, U_0_i = U_0_i, U_1_i = U_1_i, T_0_i = T_0_i, T_1_i = T_1_i )
      estimate <- contrastinf( quote( ( 1 / U_0_i ) * ( log( Y_AVG_i ) * U_0_i - T_0_i ) ) , list_all )

      grp.theill[i] <- estimate$value
      grp.theill.lin[,i] <- estimate$lin * ( w_i != 0 )

      estimate <- contrastinf( quote( grp_theill * U_0_i / U_0 ) , list( grp_theill = estimate, U_0_i = U_0_i, U_0 = U_0 ) )
      grp.theill.wtd[i] <- estimate$value
      grp.theill.wtd.lin[,i] <- estimate$lin * ( w_i != 0 )

      rm(i, w_i, estimate)

    }

    wtn.theill <- sum( grp.theill.wtd )
    w_teste <- 1/design$prob
    w_teste[ w_teste > 0 ] <- apply( grp.theill.wtd.lin, 1, sum )
    wtn.theill.lin <- w_teste ; rm( w_teste )

    # Within component:
    within.jdiv <- wtn.theilt + wtn.theill
    within.jdiv.lin <- wtn.theilt.lin + wtn.theill.lin
    within.variance <- survey::svyrecvar( within.jdiv.lin/design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata )

    # between:

    # Theil T index:
    grp.theilt.btn <- NULL
    grp.theilt.btn.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )

    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      U_0_i <- list( value = sum( w_i ), lin = rep( 1, length( incvar ) ) )
      U_1_i <- list( value = sum( w_i * incvar ), lin = incvar )
      T_0_i <- list( value = sum( w_i * log( incvar ) ), lin = log( incvar ) )
      T_1_i <- list( value = sum( w_i * incvar * log( incvar ) ), lin = incvar * log( incvar ) )

      list_all <- list(  U_0 = U_0, U_1 = U_1, U_0_i = U_0_i, U_1_i = U_1_i, T_0_i = T_0_i, T_1_i = T_1_i )
      estimate <- contrastinf( quote( ( U_1_i / U_1 ) * ( log( U_1_i / U_0_i ) - log( U_1 / U_0 ) ) ) , list_all )

      grp.theilt.btn[i] <- estimate$value
      grp.theilt.btn.lin[,i] <- estimate$lin * ( w_i != 0 )

      rm( i, w_i, estimate )
    }

    btn.theilt <- sum( grp.theilt.btn )
    w_teste <- 1/design$prob
    w_teste[ w_teste > 0 ] <- apply( grp.theilt.btn.lin, 1, sum )
    btn.theilt.lin <- w_teste ; rm( w_teste )


    # Theil L index:
    grp.theill.btn <- NULL
    grp.theill.btn.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )

    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      U_0_i <- list( value = sum( w_i ), lin = rep( 1, length( incvar ) ) )
      U_1_i <- list( value = sum( w_i * incvar ), lin = incvar )
      T_0_i <- list( value = sum( w_i * log( incvar ) ), lin = log( incvar ) )
      T_1_i <- list( value = sum( w_i * incvar * log( incvar ) ), lin = incvar * log( incvar ) )

      list_all <- list(  U_0 = U_0, U_1 = U_1, U_0_i = U_0_i, U_1_i = U_1_i, T_0_i = T_0_i, T_1_i = T_1_i )
      estimate <- contrastinf( quote( -( U_0_i / U_0 ) * ( log( U_1_i / U_0_i ) - log( U_1 / U_0 ) ) ) , list_all )

      grp.theill.btn[i] <- estimate$value
      grp.theill.btn.lin[,i] <- estimate$lin * ( w_i != 0 )

      rm( i, w_i, estimate )
    }

    btn.theill <- sum( grp.theill.btn )
    w_teste <- 1/design$prob
    w_teste[ w_teste > 0 ] <- apply( grp.theill.btn.lin, 1, sum )
    btn.theill.lin <- w_teste ; rm( w_teste )

    # Between component:
    between.jdiv <- btn.theilt + btn.theill
    between.jdiv.lin <- btn.theilt.lin + btn.theill.lin
    between.variance <- survey::svyrecvar( between.jdiv.lin/design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata )


    estimates <- matrix( c( ttl.jdiv, within.jdiv, between.jdiv ), dimnames = list( c( "total", "within", "between" ) ) )[,]
    variance <- matrix( c( ttl.variance, within.variance, between.variance ), dimnames = list( c( "total", "within", "between" ) ) )[,]

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence decomposition"
    attr(rval,"group")<- as.character( by.formula )[[2]]
    class(rval) <- c( "cvydstat" )
    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.svyrep.design <-
  function( formula, design, by.formula, na.rm=FALSE, ...) {

    # J-divergence measure:
    calc.jdiv <-  function( x, weights ) {

      x <- x[ weights > 0]
      weights <- weights[ weights > 0]

      avg <- sum( x * weights ) / sum( weights )
      jdiv = ( ( x - avg ) / avg ) * log( x / avg )

      return( sum( jdiv * weights ) / sum( weights ) )

    }

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

    incvar <- model.frame(formula, design$variables, na.action = na.pass)[,]
    grpvar <- model.frame( by.formula, design$variables, na.action = na.pass)[,]

    if(na.rm){
      nas<-is.na(incvar) | is.na(grpvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
      grpvar <- grpvar[!nas]
    }

    ws <- weights(design, "sampling")

    if ( any( incvar[ ws != 0 ] <= 0, na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes." )

    if ( any( is.na(incvar) | is.na(grpvar) ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( c(NA,NA,NA), dimnames = list( c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "j-divergence decomposition"
      attr(rval,"group")<- as.character( by.formula )[[2]]
      class(rval) <- c( "cvydstat" )

      return(rval)

    }

    grpvar <- interaction( grpvar )

    ww <- weights(design, "analysis")

    # Total
    ttl.jdiv <- calc.jdiv( x = incvar, weights = ws )
    qq.ttl.jdiv <- apply(ww, 2, function(wi) calc.jdiv(incvar, wi ) )

    # within

    # Theil T index:
    grp.wtd.theilt <- NULL
    qq.grp.wtd.theilt <- matrix( NA, nrow = length(qq.ttl.jdiv), ncol = length( levels( grpvar ) ) )
    for ( i in seq_along( levels(grpvar) ) ) {
      ind <- 1*( grpvar == levels(grpvar)[i] )

      grp.theilt <- calc.gei( x = incvar, weights = ws * ind, epsilon = 1 )
      qq.grp.theilt <- apply(ww, 2, function(wi) calc.gei( incvar, wi * ind, epsilon = 1 ) )

      grp.incshr <- sum( incvar * ind * ws ) / sum( incvar * ws )
      qq.grp.incshr <- apply( ww, 2, function(wi) { sum( incvar * ind * wi ) / sum( incvar * wi ) } )

      grp.wtd.theilt[i] <- grp.theilt * grp.incshr
      qq.grp.wtd.theilt[,i] <- qq.grp.theilt * qq.grp.incshr

    }

    # Theil L index:
    grp.wtd.theill <- NULL
    qq.grp.wtd.theill <- matrix( NA, nrow = length(qq.ttl.jdiv), ncol = length( levels( grpvar ) ) )
    for ( i in seq_along( levels(grpvar) ) ) {

      ind <- 1*( grpvar == levels(grpvar)[i] )

      grp.theill <- calc.gei( x = incvar, weights = ws * ind, epsilon = 0 )
      qq.grp.theill <- apply(ww, 2, function(wi) calc.gei( incvar, wi * ind, epsilon = 0 ) )

      grp.popshr <- sum( ind * ws ) / sum( ws )
      qq.grp.popshr <- apply( ww, 2, function(wi) { sum( ind * wi ) / sum( wi ) } )

      grp.wtd.theill[i] <- grp.theill * grp.popshr
      qq.grp.wtd.theill[,i] <- qq.grp.theill * qq.grp.popshr

    }

    # Within component:
    within.jdiv <- sum( grp.wtd.theilt + grp.wtd.theill )
    qq.within.jdiv <- apply( qq.grp.wtd.theilt + qq.grp.wtd.theill, 1, sum )

    # Between:

    # Theil T index:
    grp.avg.theilt <- NULL
    qq.grp.avg.theilt <- matrix( NA, nrow = length(qq.ttl.jdiv), ncol = length( levels( grpvar ) ) )
    for ( i in seq_along( levels(grpvar) ) ) {
      ind <- 1*( grpvar == levels(grpvar)[i] )

      grp.avg.theilt[i] <- ( sum( ws * ind * incvar ) / sum( ws * incvar ) ) * ( log( sum( ws * ind * incvar ) / sum( ws * ind ) ) - log( sum( ws * incvar ) / sum( ws ) ) )
      qq.grp.avg.theilt[,i] <- apply( ww, 2, function(wi) {
        ( sum( wi * ind * incvar ) / sum( wi * incvar ) ) * ( log( sum( wi * ind * incvar ) / sum( wi * ind ) ) - log( sum( wi * incvar ) / sum( wi ) ) )
      } )

    }

    # Theil L index:
    grp.avg.theill <- NULL
    qq.grp.avg.theill <- matrix( NA, nrow = length(qq.ttl.jdiv), ncol = length( levels( grpvar ) ) )
    for ( i in seq_along( levels(grpvar) ) ) {
      ind <- 1*( grpvar == levels(grpvar)[i] )

      grp.avg.theill[i] <- -( sum( ws * ind ) / sum( ws ) ) * ( log( sum( ws * ind * incvar ) / sum( ws * ind ) ) - log( sum( ws * incvar ) / sum( ws ) ) )
      qq.grp.avg.theill[,i] <- apply( ww, 2, function(wi) {
        -( sum( wi * ind ) / sum( wi ) ) * ( log( sum( wi * ind * incvar ) / sum( wi * ind ) ) - log( sum( wi * incvar ) / sum( wi ) ) )
      } )

    }

    # Between Component:
    between.jdiv <- sum(grp.avg.theill + grp.avg.theilt)
    qq.between.jdiv <- apply( qq.grp.avg.theill + qq.grp.avg.theilt, 1, sum )


    if ( any(is.na( c( qq.ttl.jdiv, qq.within.jdiv, qq.between.jdiv ) ) ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( c(NA,NA,NA), dimnames = list( c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "j-divergence decomposition"
      attr(rval,"group")<- as.character( by.formula )[[2]]
      class(rval) <- c( "cvydstat" )

      return(rval)

    } else {

      variance <- c(
        survey::svrVar( qq.ttl.jdiv, design$scale, design$rscales, mse = design$mse, coef = ttl.jdiv),
        survey::svrVar( qq.within.jdiv, design$scale, design$rscales, mse = design$mse, coef = within.jdiv),
        survey::svrVar( qq.between.jdiv, design$scale, design$rscales, mse = design$mse, coef = between.jdiv)
      )
      variance <- matrix( variance, dimnames = list( c( "total", "within", "between" ) ) )[,]

    }

    rval <- list( estimate = matrix( c( ttl.jdiv, within.jdiv, between.jdiv ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence decomposition"
    attr(rval,"group")<- as.character( by.formula )[[2]]
    class(rval) <- c( "cvydstat" )
    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.DBIsvydesign <-
  function (formula, design, by.formula, ...) {


    if (!( "logical" %in% class(attr(design, "full_design"))) ){

      full_design <- attr( design , "full_design" )

      full_design$variables <-
        cbind(
          getvars(formula, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename,updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset),

          getvars(by.formula, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename,updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset)
        )



      attr( design , "full_design" ) <- full_design

      rm( full_design )

    }

    design$variables <-
      cbind(
        getvars(formula, design$db$connection,design$db$tablename, updates = design$updates, subset = design$subset),

        getvars(by.formula, design$db$connection, design$db$tablename,updates = design$updates, subset = design$subset)
      )

    NextMethod("svyjdivdec", design)

  }
