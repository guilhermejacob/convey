context("test-matrix makes sense across all functions' coefficients and standard errors")

# set up functions
these_functions <-
  c( "svygei" , "svygeidec" , "svyjdiv" , "svyjdivdec" , "svyatk" ,
     "svygpg" , "svyqsr" , "svypoormed" , "svyrmpg" , "svyrmir" , "svyisq" , "svyiqalpha" ,
     "svyarpr" , "svyarpt" , "svygini" ,
     "svyfgt" , "svyfgtdec" , "svychu" , "svywatts" , "svyrich" ,  "svylorenz" )

test_that("coef and SE matrix values make sense",{

  # skip test on cran
  skip_on_cran()

  # load libraries
  library(laeken)
  library(survey)

  # load data
  data(eusilc)

  # fix names
  names( eusilc ) <- tolower( names( eusilc ) )

  # declare survey design
  des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
  des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )

  # prepare for convey functions
  des_eusilc <- convey_prep( des_eusilc )
  des_eusilc_rep <- convey_prep( des_eusilc_rep )

  # apply subset
  des_eusilc <- subset( des_eusilc , eqincome > 0 )
  des_eusilc_rep <- subset( des_eusilc_rep , eqincome > 0 )

  # run across functions
  out <- lapply( these_functions , function( this_fun ) {

    # # track process
    # cat( this_fun , "\n")

    # set up general function
    final_fun <- FUN <- get( this_fun )
    if( identical( FUN , svyrmpg ) ) final_fun <- function( ... ) FUN( ... , thresh = TRUE )
    if( identical( FUN , svyrmir ) ) final_fun <- function( ... ) FUN( ... , age = ~ age , med_old = TRUE )
    if( identical( FUN , svyisq ) ) final_fun <- function( ... ) FUN( ... , alpha = 0.2 )
    if( identical( FUN , svyiqalpha ) ) final_fun <- function( ... ) FUN( ... , alpha = 0.5 )
    if( identical( FUN , svyfgt ) ) final_fun <- function( ... ) FUN( ... , g = 0 , abs_thresh = 10000 )
    if( identical( FUN , svyfgtdec ) ) final_fun <- function( ... ) FUN( ... , g = 2 , abs_thresh = 10000 )
    if( identical( FUN , svychu ) ) final_fun <- function( ... ) FUN( ... , g = .5 , abs_thresh = 10000 )
    if( identical( FUN , svywatts ) ) final_fun <- function( ... ) FUN( ... , abs_thresh = 10000 )
    if( identical( FUN , svyrich ) ) final_fun <- function( ... ) FUN( ... , g = 2 , abs_thresh = 15000 , type_measure = "Cha"  )
    if( identical( FUN , svygpg ) ) final_fun <- function( ... ) FUN( ... , sex = ~ rb090 )
    if( identical( FUN , svygei ) ) final_fun <- function( ... ) FUN( ... , epsilon = 0.5 )
    if( identical( FUN , svygeidec ) ) final_fun <- function( ... ) FUN( ... , epsilon = 0.5 , subgroup = ~rb090 )
    if( identical( FUN , svyjdivdec ) ) final_fun <- function( ... ) FUN( ... , subgroup = ~rb090 )
    if( identical( FUN , svyatk ) ) final_fun <- function( ... ) FUN( ... , epsilon = 0.5 )
    if( identical( FUN , svyjdivdec ) ) final_fun <- function( ... ) FUN( ... , subgroup = ~rb090 )
    if( identical( FUN , svylorenz ) ) final_fun <- function( ... ) FUN( ... , quantiles = seq(0,1,.05) )

    # create objects
    estimate_lin <- final_fun( ~ eqincome , des_eusilc )
    estimate_rep <- final_fun( ~ eqincome , des_eusilc_rep )

    # build dataframe
    this_df <-
      data.frame(
        function_name = this_fun ,
        coef_lin = as.numeric( coef( estimate_lin ) ) ,
        se_lin = as.numeric( SE( estimate_lin ) ),
        coef_rep = as.numeric( coef( estimate_lin ) ) ,
        se_rep = as.numeric( SE( estimate_lin ) ) ,
        stringsAsFactors = FALSE )
    rownames( this_df ) <- NULL
    this_df

  } )
  out <- do.call( rbind , out )

  # add descrition of mou
  out$measure_of_uncertainty <- "standard error"

  # reorder columns
  out <- out[ c( "function_name" , "measure_of_uncertainty" , "coef_lin" , "se_lin" , "coef_rep" , "se_rep" ) ]

  # rename columns
  names( out ) <- c( "function_name" , "measure_of_uncertainty__mou" , "linearized_coefficient" , "linearized_mou" , "replication_coefficient" , "replication_mou" )

  # matrix of coefficients and standard errors for all convey functions
  print( out )

  # test that coefficients for all functions are equal!
  isTRUE( expect_true( all.equal( out$linearized_coefficient , out$replication_coefficient ) ) )

  # test that the difference between the measure of uncertainty is a small fraction of replication designs
  expect_true( all( abs( out$replication_mou - out$linearized_mou ) / out$replication_coefficient < 0.05 , na.rm = TRUE ) )

  # test that the difference between the measure of uncertainty is a small fraction of linearized designs
  expect_true( all( abs( out$replication_mou - out$linearized_mou ) / out$linearized_coefficient < 0.05 , na.rm = TRUE ) )

} )

