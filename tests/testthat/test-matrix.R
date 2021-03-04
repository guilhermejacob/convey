context("test-matrix makes sense across all functions' coefficients and standard errors")

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

  # make initial object
  out <- NULL

  # run across functions
  for( this_fun in c( "svygei" , "svygpg" , "svyatk" , "svyqsr" , "svypoormed" , "svyrmpg" , "svyrmir" , "svyisq" , "svyiqalpha" , "svyarpr" , "svyarpt" , "svyfgt" , "svygini" ) ){

    final_fun <- FUN <- get( this_fun )

    if( identical( FUN , svyrmpg ) ) final_fun <- function( ... ) FUN( ... , thresh = TRUE )
    if( identical( FUN , svyrmir ) ) final_fun <- function( ... ) FUN( ... , age = ~ age , med_old = TRUE )
    if( identical( FUN , svyisq ) ) final_fun <- function( ... ) FUN( ... , alpha = 0.2 )
    if( identical( FUN , svyiqalpha ) ) final_fun <- function( ... ) FUN( ... , alpha = 0.5 )
    if( identical( FUN , svyfgt ) ) final_fun <- function( ... ) FUN( ... , g = 0 , abs_thresh = 10000 )
    if( identical( FUN , svygpg ) ) final_fun <- function( ... ) FUN( ... , sex = ~ rb090 )
    if( identical( FUN , svygei ) ) final_fun <- function( ... ) FUN( ... , epsilon = 0.5 )


    this_df <-
      data.frame(
        function_name = this_fun ,
        coef_lin = coef( final_fun( ~ eqincome , des_eusilc ) ) ,
        se_lin = SE( final_fun( ~ eqincome , des_eusilc ) )[1] ,
        coef_rep = coef( final_fun( ~ eqincome , des_eusilc_rep ) ) ,
        se_rep = SE( final_fun( ~ eqincome , des_eusilc_rep ) )[1]
      )

    rownames( this_df ) <- NULL

    out <- rbind( out , this_df )
  }

  # add descrition of mou
  out$measure_of_uncertainty <- "standard error"

  # lorenz curve
  lor_lin <- svylorenz( ~eqincome , des_eusilc, seq(0,1,.05), alpha = .01 , plot = FALSE )
  lor_rep <- svylorenz( ~eqincome , des_eusilc_rep, seq(0,1,.05), alpha = .01 , plot = FALSE )

  this_df <-
    data.frame(
      function_name = "svylorenz" ,
      coef_lin = lor_lin$quantiles[5] ,
      se_lin = lor_lin$quantiles[5] - lor_lin$CIs[9] ,
      coef_rep = lor_rep$quantiles[5] ,
      se_rep = lor_rep$quantiles[5] - lor_rep$CIs[9] ,
      measure_of_uncertainty = "confidence interval length at median" )
  rownames( this_df ) <- NULL
  out <- rbind( out , this_df )

  # gei decomposition
  dec_lin <- svygeidec( ~eqincome , ~rb090 , des_eusilc , epsilon = .5 )
  dec_rep <- svygeidec( ~eqincome , ~rb090 , des_eusilc_rep , epsilon = .5 )

  this_df <-
    data.frame(
      function_name = paste( "svygeidec" , c( "total" , "within" , "between" ) ) ,
      coef_lin = coef( dec_lin ) ,
      se_lin = SE(dec_lin) ,
      coef_rep = coef(dec_rep) ,
      se_rep = SE(dec_rep) ,
      measure_of_uncertainty = "standard error"
    )
  rownames( this_df ) <- NULL
  out <- rbind( out , this_df )

  # reorder columns
  out <- out[ c( "function_name" , "measure_of_uncertainty" , "coef_lin" , "se_lin" , "coef_rep" , "se_rep" ) ]

  # rename columns
  names( out ) <- c( "function_name" , "measure_of_uncertainty__mou" , "linearized_coefficient" , "linearized_mou" , "replication_coefficient" , "replication_mou" )

  # matrix of coefficients and standard errors for all convey functions
  print( out )

  # test that coefficients for all functions are equal!
  isTRUE( expect_true( all.equal( out$linearized_coefficient , out$replication_coefficient ) ) )

  # test that the difference between the measure of uncertainty is a small fraction of replication designs
  expect_true( all( abs( out$replication_mou - out$linearized_mou ) / out$replication_coefficient < 0.05 ) )

  # test that the difference between the measure of uncertainty is a small fraction of linearized designs
  expect_true( all( abs( out$replication_mou - out$linearized_mou ) / out$linearized_coefficient < 0.05 ) )

} )

