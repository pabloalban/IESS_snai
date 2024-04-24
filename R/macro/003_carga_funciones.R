message( paste( rep( "-", 100 ), collapse = "" ) )
message( "\tCargando funciones macro" )

#Función para reemplazar datos atípicos-------------------------------------------------------------

outliers <- function( x ) {
  n = length( boxplot( x )$out )
  i = 0
  
  while ( ( n > 0 ) || ( i < 2 ) ) {
    x = if_else( x %in% boxplot( x )$out,
                 NA,
                 x )
    x = forecast::tsclean( x, replace.missing = TRUE )
    n = length( boxplot( x )$out )
    i = i + 1
  }
  
  return( x )
}

#Test de hipótesis de coeficientes de un arma-------------------------------------------------------

test_nulidad_arma <- function ( modelo )
{
  t <- abs( modelo$coef / ( sqrt(diag(modelo$var.coef) )  ) )
  p_values <- 1.96 * ( 1 - pt( t, nrow( train ) - length( modelo$coef ) ) )
  
  
  test <- data.frame( coef =  modelo$coef,
                      t = t,
                      p_values = p_values ) %>% 
    filter( t > 0) %>% 
    mutate( se = sqrt( diag( modelo$var.coef ) ) ) %>% 
    mutate( var = row.names( . ) ) %>% 
    dplyr::select( var,
                   coef,
                   se,
                   t, p_values )
  
  return( test )
}

#Función LjungBox multivariante---------------------------------------------------------------------

mq_aumentada <- function ( x, lag = 20, adj = 0 )
{
  if ( !is.matrix( x ) )
    x = as.matrix( x )
  nr = nrow( x )
  nc = ncol( x )
  g0 = var( x )
  ginv = solve( g0 )
  qm = 0
  QM = NULL
  df = 0
  for ( i in 1:lag ) {
    x1 = x[( i + 1 ):nr, ]
    x2 = x[1:( nr - i ), ]
    g = cov( x1, x2 )
    g = g * ( nr - i - 1 ) / ( nr - 1 )
    h = t( g ) %*% ginv %*% g %*% ginv
    qm = qm + nr * nr * sum( diag( h ) ) / ( nr - i )
    df = df + nc * nc
    dff = df - adj
    mindeg = nc ^ 2 - 1
    pv = 1
    if ( dff > mindeg )
      pv = 1 - pchisq( qm, dff )
    QM = rbind( QM, c( i, qm, dff, pv ) )
  }
  pvs = QM[, 4]
  dimnames( QM ) = list( names( pvs ), c( "  m  ", "    Q(   m   ) ",
                                          "   df  ", " p-value" ) )
  #cat( "Ljung-Box Statistics: ", "\n" )
  #printCoefmat( QM, digits = 3 )
  return( QM )
}

#Función de test Arch multivariante de Tsay---------------------------------------------------------

"MarchTest" <- function ( zt, lag = 10 )
{
  if ( !is.matrix( zt ) )
    zt = as.matrix( zt )
  nT = dim( zt )[1]
  k = dim( zt )[2]
  C0 = cov( zt )
  zt1 = scale( zt, center = TRUE, scale = FALSE )
  C0iv = solve( C0 )
  wk = zt1 %*% C0iv
  wk = wk * zt1
  rt2 = apply( wk, 1, sum ) - k
  m1 = acf( rt2, lag.max = lag, plot = F )
  acf = m1$acf[2:( lag + 1 )]
  c1 = c( 1:lag )
  deno = rep( nT, lag ) - c1
  Q = sum( acf ^ 2 / deno ) * nT * ( nT + 2 )
  pv1 = 1 - pchisq( Q, lag )
  #cat( "Q( m ) of squared series( LM test ): ", "\n" )
  #cat( "Test statistic: ", Q, " p-value: ", pv1, "\n" )
  rk = rank( rt2 )
  m2 = acf( rk, lag.max = lag, plot = F )
  acf = m2$acf[2:( lag + 1 )]
  mu = -( rep( nT, lag ) - c( 1:lag ) ) / ( nT * ( nT - 1 ) )
  v1 = rep( 5 * nT ^ 4, lag ) - ( 5 * c( 1:lag ) + 9 ) * nT ^ 3 + 9 *
    ( c( 1:lag ) - 2 ) * nT ^ 2 + 2 * c( 1:lag ) * ( 5 * c( 1:lag ) +
                                                       8 ) * nT + 16 * c( 1:lag ) ^
    2
  v1 = v1 / ( 5 * ( nT - 1 ) ^ 2 * nT ^ 2 * ( nT + 1 ) )
  QR = sum( ( acf - mu ) ^ 2 / v1 )
  pv2 = 1 - pchisq( QR, lag )
  #cat( "Rank-based Test: ", "\n" )
  #cat( "Test statistic: ", QR, " p-value: ", pv2, "\n" )
  #cat( "Q_k( m ) of squared series: ", "\n" )
  x = zt ^ 2
  g0 = var( x )
  ginv = solve( g0 )
  qm = 0
  df = 0
  for ( i in 1:lag ) {
    x1 = x[( i + 1 ):nT,]
    x2 = x[1:( nT - i ),]
    g = cov( x1, x2 )
    g = g * ( nT - i - 1 ) / ( nT - 1 )
    h = t( g ) %*% ginv %*% g %*% ginv
    qm = qm + nT * nT * sum( diag( h ) ) / ( nT - i )
    df = df + k * k
  }
  pv3 = 1 - pchisq( qm, df )
  #cat( "Test statistic: ", qm, " p-value: ", pv3, "\n" )
  cut1 = quantile( rt2, 0.95 )
  idx = c( 1:nT )[rt2 <= cut1]
  x = zt[idx,] ^ 2
  eT = length( idx )
  g0 = var( x )
  ginv = solve( g0 )
  qm2 = 0
  df = 0
  for ( i in 1:lag ) {
    x1 = x[( i + 1 ):eT,]
    x2 = x[1:( eT - i ),]
    g = cov( x1, x2 )
    g = g * ( eT - i - 1 ) / ( eT - 1 )
    h = t( g ) %*% ginv %*% g %*% ginv
    qm2 = qm2 + eT * eT * sum( diag( h ) ) / ( eT - i )
    df = df + k * k
  }
  pv4 = 1 - pchisq( qm2, df )
  #cat( "Robust Test( 5% ) : ", qm, " p-value: ", pv4, "\n" )
  MarchTest <- list( 
    Q = Q,
    pv1 = pv1,
    QR = QR,
    pv2 = pv2,
    qm = qm,
    pv3 = pv3,
    qm2 = qm2,
    pv4 = pv4
  )
}

#Función prueba de nulidad de coeficientes de Tsay--------------------------------------------------

"VARchi" <- function( x,
                      p = 1,
                      include.mean = T,
                      thres ) {
  # Fits a vector AR( p ) model, then performs
  # a chi-square test to zero out insignificant parameters.
  if ( !is.matrix( x ) )
    x = as.matrix( x )
  Tn = dim( x )[1]
  k = dim( x )[2]
  if ( p < 1 )
    p = 1
  ne = Tn - p
  ist = p + 1
  y = x[ist:Tn, ]
  if ( include.mean ) {
    xmtx = cbind( rep( 1, ne ), x[p:( Tn - 1 ), ] )
  }
  else {
    xmtx = x[p:( Tn - 1 ), ]
  }
  if ( p > 1 ) {
    for ( i in 2:p ) {
      xmtx = cbind( xmtx, x[( ist - i ):( Tn - i ), ] )
    }
  }
  ndim = dim( xmtx )[2]   # #perform estimation
  res = NULL
  xm = as.matrix( xmtx )
  xpx = crossprod( xm, xm )
  xpxinv = solve( xpx )
  xpy = t( xm ) %*% as.matrix( y )
  beta = xpxinv %*% xpy
  resi = y - xm %*% beta
  sse = t( resi ) %*% resi / ( Tn - p - ndim )
  C1 = kronecker( sse, xpxinv )
  dd = sqrt( diag( C1 ) )
  #
  bhat = c( beta )
  tratio = bhat / dd
  para = cbind( bhat, dd, tratio )
  npar = length( bhat )
  K = NULL
  omega = NULL
  for ( i in 1:npar ) {
    if ( abs( tratio[i] ) < thres ) {
      idx = rep( 0, npar )
      idx[i] = 1
      K = rbind( K, idx )
      omega = c( omega, bhat[i] )
    }
  }
  v = dim( K )[1]
  K = as.matrix( K )
  #cat( "Number of targeted parameters: ", v, "\n" )
  #####print( K )
  if ( v > 0 ) {
    C2 = K %*% C1 %*% t( K )
    C2inv = solve( C2 )
    tmp = C2inv %*% as.matrix( omega, v, 1 )
    chi = sum( omega * tmp )
    pvalue = 1 - pchisq( chi, v )
    #cat( "Chi-square test and p-value: ", c( chi, pvalue ), "\n" )
  }
  else{
    print( "No contraints needed" )
  }
  VARchi <- list( chi = chi,
                  pvalue = pvalue )
}
