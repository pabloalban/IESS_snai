message( paste( rep('-', 100 ), collapse = '' ) )

# Cargando información -----------------------------------------------------------------------------
message( '\tCargando datos' )
load( parametros$demo_rdata_onu_int_life_tab )
load( parametros$demo_rdata_sgo_tasas_tran )
load( parametros$demo_rdata_sgo_din_dec )

# Preparación de variables -------------------------------------------------------------------------
# Horizonte de proyección
# Tiempo
t_min <- 0
t_max <- parametros$demo_horizonte # horizonte de proyección
t_lst <- seq( t_min, t_max, 1 )

# Tiempos de servicio
s_min <- 0
s_max <- parametros$demo_ts_max
s_lst <- seq( s_min, s_max, 1 )

# Edades
x_min <- 0
x_max <- parametros$demo_edad_max
x_lst <- seq( x_min, x_max, 1 )

# Dimensión del número de estados
nd <- 6

# Año inicial de proyección
fec_ini <- parametros$anio

# Año final de proyección
fec_fin <- fec_ini + t_max

nt <- length( t_lst )
ns <- length( s_lst )
nx <- length( x_lst )

# Descripción estados ------------------------------------------------------------------------------
# 1 = Individuos en la PEA no activos
# 2 = Activos cotizantes
# 3 = Activos cesantes
# 4 = Pensionistas de vejez
# 5 = Pensionistas de invalidez
# 6 = Muertos
# 7 = Pensionistas de montepío viudas
# 8 = Pensionistas de montepío huérfanos
# 9 = Dependientes cónyuges
# 10 = Dependientes hijos
# 11 = Dependientes hijos menores de 18 años
# 12 = Activos que no son TNRH, subconjunto del estado 2
# 13 = Activos que son TNRH, subconjunto del estado 2

# (1 -> 2) Tasa de PEA a activo  -------------------------------------------------------------------
message( '\tPreparando tasa de entrada de afililiados' )
u12_m <- merge.data.table(
  data.table( x = x_lst ),
  tas_1_2[ sexo == 'M', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u12_m[ is.na( ux ), ux := 0 ]
setorder( u12_m, x )
u12_m <- as.matrix( u12_m[ , list( ux ) ] )

u12_h <- merge.data.table(
  data.table( x = x_lst ),
  tas_1_2[ sexo == 'H', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u12_h[ is.na( ux ), ux := 0 ]
setorder( u12_h, x )
u12_h <- as.matrix( u12_h[ , list( ux ) ] )

# (1 -> 6) Tasa de PEA a muerto  -------------------------------------------------------------------
message( '\tPreparando tasa de mortalidad de no afililiados' )
u16_m <- onu_ecu_mort_din[ sexo == 'F' & t >= fec_ini & t <= fec_fin & x <= x_max,
                           list( t = t - fec_ini, x, ux ) ]
setorder( u16_m, t, x )
u16_m <- dcast.data.table( data = u16_m, x ~ t, value.var = 'ux' )
setorder( u16_m, x )
u16_m <- as.matrix( u16_m[ , 2:ncol( u16_m ) ] )

u16_h <- onu_ecu_mort_din[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max,
                           list( t = t - fec_ini, x, ux ) ]
setorder( u16_h, t, x )
u16_h <- dcast.data.table( data = u16_h, x ~ t, value.var = 'ux' )
setorder( u16_h, x )
u16_h <- as.matrix( u16_h[ , 2:ncol( u16_h ) ] )

# (2 -> 3) Tasa de activo a cesante/inactivo -------------------------------------------------------
message( '\tPreparando tasa de salida por cesantía de un afiliado' )
u23_m <- merge.data.table(
  data.table( x = x_lst ),
  tas_2_3[ sexo == 'M', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u23_m[ is.na( ux ), ux := 0 ]
setorder( u23_m, x )
u23_m <- as.matrix( u23_m[ , list( ux ) ] )

u23_h <- merge.data.table(
  data.table( x = x_lst ),
  tas_2_3[ sexo == 'H', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u23_h[ is.na( ux ), ux := 0 ]
setorder( u23_h, x )
u23_h <- as.matrix( u23_h[ , list( ux ) ] )

# (2 -> 4) Tasa de activo a pensionista de vejez ---------------------------------------------------
message( '\tPreparando tasa de salida de afiliados por vejez' )
u24_m <- merge.data.table(
  data.table( expand.grid( t = t_lst, x = x_lst ) ),
  tas_2_4[ sexo == 'M', list( x, ux ) ],
  by = 'x', all.x = TRUE )

# fact <- c( 3, 2.5, 2, 1.5, seq( 1, 0.5, length.out = 26 ), rep( 0.4, 12 ) )
fact <- c( 0.9, 1, 1, 1, seq( 1, 0.5, length.out = 37 ) )
u24_m[ is.na( ux ), ux := 0 ]
u24_m[ , r := fact[ t + 1 ] ]
u24_m[ , ux := r * ux ]
u24_m <- dcast.data.table( data = u24_m, x ~ t, value.var = 'ux' )
setorder( u24_m, x )
u24_m <- as.matrix( u24_m[ , 2:ncol( u24_m ) ] )

u24_h <- merge.data.table(
  data.table( expand.grid( t = t_lst, x = x_lst ) ),
  tas_2_4[ sexo == 'H', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u24_h[ is.na( ux ), ux := 0 ]
u24_h[ , r := fact[ t + 1 ] ]
u24_h[ , ux := r * ux ]
u24_h <- dcast.data.table( data = u24_h, x ~ t, value.var = 'ux' )
setorder( u24_h, x )
u24_h <- as.matrix( u24_h[ , 2:ncol( u24_h ) ] )

# (2 -> 5) Tasa de activo a pensionista de invalidez -----------------------------------------------
message( '\tPreparando tasa de salida de afiliados por invalidez' )
u25_m <- merge.data.table(
  data.table( x = x_lst ),
  tas_2_5[ sexo == 'M', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u25_m[ is.na( ux ), ux := 0 ]
setorder( u25_m, x )
u25_m <- as.matrix( u25_m[ , list( ux ) ] )

u25_h <- merge.data.table(
  data.table( x = x_lst ),
  tas_2_5[ sexo == 'H', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u25_h[ is.na( ux ), ux := 0 ]
setorder( u25_h, x )
u25_h <- as.matrix( u25_h[ , list( ux ) ] )

# (2 -> 6) Tasa de activo a muerto -----------------------------------------------------------------
message( '\tPreparando tasa de mortalidad de afililiados' )
u26_m <- tas_din_dec[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_act ) ]
setorder( u26_m, t, x )
u26_m <- dcast.data.table( data = u26_m, x ~ t, value.var = 'ux' )
setorder( u26_m, x )
u26_m <- as.matrix( u26_m[ , 2:ncol( u26_m ) ] )

u26_h <- tas_din_dec[ sexo == 'H' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_act ) ]
setorder( u26_h, t, x )
u26_h <- dcast.data.table( data = u26_h, x ~ t, value.var = 'ux' )
setorder( u26_h, x )
u26_h <- as.matrix( u26_h[ , 2:ncol( u26_h ) ] )

# (3 -> 2) Tasa de cesante/inactivo a activo -------------------------------------------------------
message( '\tPreparando tasa de reingreso a ser activo' )
u32_m <- merge.data.table(
  data.table( x = x_lst ),
  tas_3_2[ sexo == 'M', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u32_m[ is.na( ux ), ux := 0 ]
setorder( u32_m, x )
u32_m <- as.matrix( u32_m[ , list( ux ) ] )

u32_h <- merge.data.table(
  data.table( x = x_lst ),
  tas_3_2[ sexo == 'H', list( x, ux ) ],
  by = 'x', all.x = TRUE )

u32_h[ is.na( ux ), ux := 0 ]
setorder( u32_h, x )
u32_h <- as.matrix( u32_h[ , list( ux ) ] )

# (3 -> 6) Tasa de cesante/inactivo a muerto -------------------------------------------------------
message( '\tPreparando tasa de mortalidad de cesante' )
u36_m <- tas_din_dec[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_act ) ]
setorder( u36_m, t, x )
u36_m <- dcast.data.table( data = u36_m, x ~ t, value.var = 'ux' )
setorder( u36_m, x )
u36_m <- as.matrix( u36_m[ , 2:ncol( u36_m ) ] )

u36_h <- tas_din_dec[ sexo == 'H' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_act ) ]
setorder( u36_h, t, x )
u36_h <- dcast.data.table( data = u36_h, x ~ t, value.var = 'ux' )
setorder( u36_h, x )
u36_h <- as.matrix( u36_h[ , 2:ncol( u36_h ) ] )

# (4 -> 6) Tasa de pensionista de vejez a muerto ---------------------------------------------------
message( '\tPreparando tasa de mortalidad de pensionistas por vejez' )
u46_m <- tas_din_dec[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_vej ) ]
setorder( u46_m, t, x )
u46_m <- dcast.data.table( data = u46_m, x ~ t, value.var = 'ux' )
setorder( u46_m, x )
u46_m <- as.matrix( u46_m[ , 2:ncol( u46_m ) ] )

u46_h <- tas_din_dec[ sexo == 'H' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_vej ) ]
setorder( u46_h, t, x )
u46_h <- dcast.data.table( data = u46_h, x ~ t, value.var = 'ux' )
setorder( u46_h, x )
u46_h <- as.matrix( u46_h[ , 2:ncol( u46_h ) ] )

# (5 -> 6) Tasa de pensionista de invalidez a muerto -----------------------------------------------
message( '\tPreparando tasa de mortalidad de pensionistas por invalidez' )
u56_m <- tas_din_dec[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_inv ) ]
setorder( u56_m, t, x )
u56_m <- dcast.data.table( data = u56_m, x ~ t, value.var = 'ux' )
setorder( u56_m, x )
u56_m <- as.matrix( u56_m[ , 2:ncol( u56_m ) ] )

u56_h <- tas_din_dec[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max,
                      list( t = t - fec_ini, x, ux = ux_inv ) ]
setorder( u56_h, t, x )
u56_h <- dcast.data.table( data = u56_h, x ~ t, value.var = 'ux' )
setorder( u56_h, x )
u56_h <- as.matrix( u56_h[ , 2:ncol( u56_h ) ] )

# Calculo de probabilidades de transición ----------------------------------------------------------
message( '\tPreparando matriz de transición' )

# A continuación están en conformidad con la notación de indices que se utiliza para representar
# cada uno de los estados de transición

cl <- makeCluster( detectCores( all.tests = FALSE, logical = TRUE ) - 1 )
registerDoParallel( cl )

elig_vej <- parametros$elig_vej
elig_inv <- parametros$elig_inv
clusterExport( cl, "elig_vej" )
clusterExport( cl, "elig_inv" )

P <- foreach( t = 1:nt, .combine = 'c', .inorder = FALSE, .multicombine = TRUE ) %:%
  foreach( x = 1:nx, .combine = 'c', .inorder = FALSE, .multicombine = TRUE ) %:%
  foreach( s = 1:ns, .combine = 'c', .inorder = FALSE, .multicombine = TRUE ) %dopar% {
    
    Um <- matrix( 0, nd, nd )
    Um[1,2] <- u12_m[ x, 1 ]
    Um[1,6] <- u16_m[ x, t ]
    Um[2,3] <- u23_m[ x, 1 ]
    Um[2,4] <- elig_vej( s_lst[ s ], x_lst[ x ] ) * u24_m[ x, t ]
    Um[2,5] <- elig_inv( s_lst[ s ], x_lst[ x ] ) * u25_m[ x, 1 ]
    Um[2,6] <- u26_m[ x, t ]
    Um[3,2] <- u32_m[ x, 1 ]
    Um[3,6] <- u36_m[ x, t ]
    Um[4,6] <- u46_m[ x, t ]
    Um[5,6] <- u56_m[ x, t ]
    
    Uh <- matrix( 0, nd, nd )
    Uh[1,2] <- u12_h[ x, 1 ]
    Uh[1,6] <- u16_h[ x, t ]
    Uh[2,3] <- u23_h[ x, 1 ]
    Uh[2,4] <- elig_vej( s_lst[ s ], x_lst[ x ] ) * u24_h[ x, t ]
    Uh[2,5] <- elig_inv( s_lst[ s ], x_lst[ x ] ) * u25_h[ x, 1 ]
    Uh[2,6] <- u26_h[ x, t ]
    Uh[3,2] <- u32_h[ x, 1 ]
    Uh[3,6] <- u36_h[ x, t ]
    Uh[4,6] <- u46_h[ x, t ]
    Uh[5,6] <- u56_h[ x, t ]
    
    Dm <- matrix( 0, nd, nd, byrow = TRUE )
    diag( Dm ) <- -rowSums( Um )
    Um <- Um + Dm
    
    Dh <- matrix( 0, nd, nd, byrow = TRUE )
    diag( Dh ) <- -rowSums( Uh )
    Uh <- Uh + Dh
    
    # Solución exponencial utilizando Cox-Miller
    # Esta solución utiliza la descompisición en valores propios de la matriz de fuerzas de
    # transición U = W^T D V, con D matriz diagonal.
    # exp( U ) = exp( W^T D V ) = W^T exp( D ) V
    Sf <- eigen( Um )
    Dm <- diag( exp( Sf$values ) )
    Pm <- Sf$vectors %*% Dm %*% solve( Sf$vectors )
    Pm <- apply( Pm, c( 1, 2 ), FUN = function( x ) ifelse( x < 1e-10, 0, x ) )
    
    Sm <- eigen( Uh )
    Dh <- diag( exp( Sm$values ) )
    Ph <- Sm$vectors %*% Dh %*% solve( Sm$vectors )
    Ph <- apply( Ph, c( 1, 2 ), FUN = function( x ) ifelse( x < 1e-10, 0, x ) )
    
    R <- list( list( c( t, s, x ), cbind( Um, Uh, Pm, Ph ) ) )
    R
  }

stopCluster( cl )
rm( cl )
gc()


# Unión de probabilidades de transición en arrays --------------------------------------------------
# Esto lo realizamos con la finalidad de facilitar los cálculos posterior al utilizar arrays

Um <- array( 0.0, dim = c( nt, ns, nx, nd, nd ) )
Uh <- array( 0.0, dim = c( nt, ns, nx, nd, nd ) )
Pm <- array( 0.0, dim = c( nt, ns, nx, nd, nd ) )
Ph <- array( 0.0, dim = c( nt, ns, nx, nd, nd ) )

for ( i in 1:length( P ) ) {
  t <- P[[ i ]][[ 1 ]][ 1 ]
  s <- P[[ i ]][[ 1 ]][ 2 ]
  x <- P[[ i ]][[ 1 ]][ 3 ]
  
  Um[ t, s, x, , ] <- P[[ i ]][[ 2 ]][ , 1:nd ]
  Uh[ t, s, x, , ] <- P[[ i ]][[ 2 ]][ , (nd+1):(2*nd) ]
  Pm[ t, s, x, , ] <- P[[ i ]][[ 2 ]][ , (2*nd+1):(3*nd) ]
  Ph[ t, s, x, , ] <- P[[ i ]][[ 2 ]][ , (3*nd+1):(4*nd) ]
}
rm( P )

# Guardando matrices de transición -----------------------------------------------------------------
message( '\tGuardando matrices de transición' )
save( Pm, Ph, Um, Uh,
      file = parametros$demo_rdata_sgo_probs_tran )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
