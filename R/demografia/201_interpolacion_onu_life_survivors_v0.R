message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'ONU_life_table_survivors_2019.RData' ) )

# Preparando información ---------------------------------------------------------------------------
# Extrayendo solo la información del Ecuador
onu_ecu_surv <- onu_survivors[ country_code == 218, list( period, sexo, x, lx ) ]
onu_ecu_surv[ , t := as.numeric( substr( period, 1, 4 ) ) ]
onu_ecu_surv[ , lxs := shift( lx, 1, fill = 0, type = 'lag' ), by = list( t, sexo ) ]

# Condición de decrecimiento para kriging
cond_surv <- onu_ecu_surv[ x == 100 ]
cond_surv <- rbind( cond_surv[ , list( period, sexo, x = 105, lx, t, lxs ) ],
                    cond_surv[ , list( period, sexo, x = 110, lx, t, lxs ) ] )

# Condición máxima entropía
cond_surv[ x == 105, lx :=  0.5 * ( lx / lxs ) * lx ]
cond_surv[ x == 110, lx :=  0.125 * ( ( lx / lxs )^2 ) * lx ]

onu_ecu_surv <- rbind( onu_ecu_surv, cond_surv )
setorder( onu_ecu_surv, t, sexo, x )
onu_ecu_surv[ , lxs := NULL ]

onu_ecu_surv[ , p0 := lx / 1e5 ]
onu_ecu_surv[ , u0 := -log( p0 ) ]
onu_ecu_surv[ , log_u0 := log( u0 ) ]
onu_ecu_surv <- onu_ecu_surv[ is.finite( log_u0 ) ]

# Interpolación usando Kriging ---------------------------------------------------------------------
message( '\tPreparando Kriging para interpolación' )
X <- as.matrix( onu_ecu_surv[ sexo == 'F', list( t, x ) ] )
Z_f <- as.matrix( onu_ecu_surv[ sexo == 'F', list( log_u0 ) ] )
Z_m <- as.matrix( onu_ecu_surv[ sexo == 'M', list( log_u0 ) ] )
Y <- as.matrix( expand.grid( t = 1950:2100, x = 0:110 ) )

# Función de distancia
dist <- function( x, y ) {
  return( abs( x[1] - y[1] ) + abs( x[2] - y[2] ) )
}

# Variograma teórico para el kernel
message( '\tUtilizando núcleo gaussiano' )
kernel_variogram <- function( h, s, t ) {
  return( gaussian_kernel( 0, s, t ) - gaussian_kernel( h, s, t ) )
  # return( spherical_kernel( 0, s, t ) - spherical_kernel( h, s, t ) )
  # return( exp_kernel( 0, s, t ) - exp_kernel( h, s, t ) )
}

fit_variogram <- function( p, d, V ) {
  FV <- sapply( d, FUN = kernel_variogram, s = p[1], t = p[2] )
  fit  <-  sum( ( ( FV - V$variogram[,1] ) )^2 )
  return( fit )
}

message( '\tCalculando variogramas empíricos' )
V_f <- variogram( Z_f, X, dist )
d_f <- V_f$distance[ V_f$sort + 1, 1 ]

V_m <- variogram( Z_m, X, dist )
d_m <- V_m$distance[ V_m$sort + 1, 1 ]

message( '\tAjustando variogramas' )
p <- c( 4.7, 12500 )
NLM_f <- nlm( fit_variogram, p = p, d = d_f, V = V_f, 
              gradtol = 1e-6, steptol = 1e-6, iterlim = 1000 )
str( NLM_f )

p <- c( 4.7, 12500 )
NLM_m <- nlm( fit_variogram, p = p, d = d_m, V = V_m, 
              gradtol = 1e-6, steptol = 1e-6, iterlim = 1000 )
str( NLM_m )

Kern_f <- function( x, y ) gaussian_kernel( dist( x, y ), NLM_f$estimate[1], NLM_f$estimate[2] )
# Kern_f <- function( x, y ) gaussian_kernel( dist( x, y ), 4.73, 12500 )
Kern_m <- function( x, y ) gaussian_kernel( dist( x, y ), NLM_m$estimate[1], NLM_m$estimate[2] )
# Kern_m <- function( x, y ) gaussian_kernel( dist( x, y ), 4.73, 12500 )

message( '\tInterpolación por Kriging simple' )
K_f <- Kov( X, X, Kern_f, TRUE )
k_f <- Kov( Y, X, Kern_f )
KRIG_f <- Krig( Z = Z_f,
                K = K_f, 
                k = k_f,
                G = matrix( 0, 1, 1 ),
                g = matrix( 0, 1, 1 ),
                type = "simple", 
                cinv = 'inv' )

K_m <- Kov( X, X, Kern_m, TRUE )
k_m <- Kov( Y, X, Kern_m )
KRIG_m <- Krig( Z = Z_m,
                K = K_m, 
                k = k_m,
                G = matrix( 0, 1, 1 ),
                g = matrix( 0, 1, 1 ),
                type = "simple", 
                cinv = 'inv' )

onu_ecu_surv_int_f <- data.table( cbind( Y, KRIG_f$Z ) )
setnames( onu_ecu_surv_int_f, c( 't', 'x', 'log_u0' ) )
onu_ecu_surv_int_f[ , sexo := 'F' ]

onu_ecu_surv_int_m <- data.table( cbind( Y, KRIG_m$Z ) )
setnames( onu_ecu_surv_int_m, c( 't', 'x', 'log_u0' ) )
onu_ecu_surv_int_m[ , sexo := 'M' ]

onu_ecu_surv_int <- rbind( onu_ecu_surv_int_f, onu_ecu_surv_int_m )
onu_ecu_surv_int <- merge( onu_ecu_surv_int, 
                           onu_ecu_surv[ , list( sexo, x, t, lx_ini = lx, log_u0_ini = log_u0 ) ], 
                           by = c(  't', 'sexo', 'x' ), all.x = TRUE )
setorder( onu_ecu_surv_int, t, sexo, x )

onu_ecu_mort_din <- copy( onu_ecu_surv_int[ , list( t, sexo, x, log_u0 ) ] )
setorder( onu_ecu_mort_din, t, sexo, x )

# Cálculo de variaciones de la mortalidad ----------------------------------------------------------
message( '\tCalculando probabilidad de muerte' )
onu_ecu_mort_din[ , lx := 1e5 * exp( -exp( log_u0 ) ) ]
onu_ecu_mort_din[ x == 0, lx := 1e5 ]
onu_ecu_mort_din[ , lxs := shift( lx, 1, fill = 0, type = 'lead' ), by = list( t, sexo ) ]
onu_ecu_mort_din[ , px := lxs / lx ]
onu_ecu_mort_din[ , ux := -log( px ) ]
onu_ecu_mort_din[ , qx := 1 - px ]

message( '\tCalculando variación de la probabilidad de muerte' )
setorder( onu_ecu_mort_din, sexo, x, t )
onu_ecu_mort_din[ , qxs := shift( qx, 1, fill = 0, type = 'lead' ), by = list( sexo, x ) ]
onu_ecu_mort_din[ , vx := qxs / qx ]
onu_ecu_mort_din[ , qxs := NULL ]
setorder( onu_ecu_mort_din, t, sexo, x )

# Verificaciones
# check <- onu_ecu_surv_int[ !is.na( log_u0_ini ) ]
# check <- check[ abs( log_u0 - log_u0_ini ) > 1e-6 ]

message( '\tGuardando resultados interpolados' )
save( onu_ecu_surv, onu_ecu_surv_int, onu_ecu_mort_din,
      file = paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019.RData' ) )

save( d_f, V_f, KRIG_f, NLM_f,
      d_m, V_m, KRIG_m, NLM_m,
      file = paste0( parametros$RData, 'ONU_kriging_life_table_survivors_2019.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
