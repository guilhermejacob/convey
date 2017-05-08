# presets
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

# J-divergence measure:
calc.kmoi <- function( proportions , medcat = NULL , a = 1 , b = 1 ) {

  k <- length(proportions)
  if (is.null(medcat)) { medcat <- calc.medcat( proportions ) ; m <- match( names( medcat ) , names(proportions) ) }
  else { m <- medcat }
  gamma <- a * sum( proportions[ 1:(m - 1) ] ) - b * sum( proportions[ m:k ] )
  c1 <- b*( k + 1 - m )
  c2 <- ( m - 1 ) * a/2 - ( k + 2 - m )*b/2 + c1
  (gamma + c1)/c2

}

# test
set.seed(1)

art.data <- rmultinom( 10 , size = 100, prob = c(.3,.5,.2) )

art.data <- apply( art.data , 2 , FUN = function(i) {
  i / sum(i)
} )

rep( art. )
