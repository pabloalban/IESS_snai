# Estadística salarios -----------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga información --------------------------------------------------------------------------------
message( '\tCargando información' )
load( file = paste0( parametros$demo_rdata_sgo_incom_tran_act_anio, parametros$anio_ini, '.RData' ) )

# Parámetros ---------------------------------------------------------------------------------------
message( '\tPreparando parámetros' )

# Horizonte de proyección
t_max <- parametros$demo_horizonte

anio_ini <- parametros$anio_ini

sexo_lst <- factor( c( 'H', 'M' ), levels = c( 'H', 'M' ) )
t_lst <- seq( 0, parametros$demo_horizonte, 1 )
x_lst <- seq( 0, parametros$demo_edad_max, 1 )
s_lst <- seq( 0, parametros$demo_ts_max, 1 )

# Preparando tasa para la proyección
tas_macro_sex_edad <- copy( esc$apo_act[ , list( t, anio, i_r, sbu ) ] )
# No variación en la tasa para años donde se tiene ya información
tas_macro_sex_edad[ anio <= anio_ini, i_r := 0 ]
tas_macro_sex_edad[ , u_r := cumprod( 1 + i_r ) ]

aux <- data.table( expand_grid( 
  t = t_lst,
  sexo = sexo_lst,
  s = s_lst,
  x = x_lst ) )
aux[ , anio := t + parametros$anio_ini ]
aux[ , r := x - s ]
aux[ , u := s - t ]
aux <- merge.data.table(
  aux,
  tas_macro_sex_edad,
  by = c( 't', 'anio' ) )

# Proyección salarios ------------------------------------------------------------------------------
message( '\tGenerando proyección de salarios' )

# Salario por edad y tiempo de servicio
sal_proy <- sgo_comp_tran[ x >= 15, list( sexo, imp, x, ER_act, S ) ]
sal_proy[ , s := round( imp, 0 ) ]
sal_proy[ is.na( s ), s := 0 ]
sal_proy[ x >= 100, x := 100 ]
sal_proy[ , xf := cut( x, breaks = c( 0, 20, seq( 25, 105, 5 ) ), include.lowest = TRUE ) ]
sal_proy[ , ES := sum( S, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ), by = list( sexo, x, s ) ]
sal_proy <- sal_proy[ , list( 
  ER_act = sum( ER_act, na.rm = TRUE ), 
  S = sum( S, na.rm = TRUE ),
  VARS = sum( ( S - ES )^2, na.rm = TRUE ) ),
  by = list( sexo, xf, x, s ) ]
sal_proy[ , ES := 0 ]
sal_proy[ ER_act > 0, ES := S / ER_act ]
sal_proy[ , SDS := 0 ]
sal_proy[ ER_act > 0, SDS := sqrt( VARS ) ]
sal_proy <- sal_proy[ ER_act > 0 ]
sal_proy[ , ES98 := quantile( ES, probs = 0.98, na.rm = TRUE ), by = list( xf ) ]
sal_proy[ ES >= ES98, ES := ES98 ]
sal_proy[ , r := x - s ]
sal_proy[ , u := s ]

sal_proy_x <- sgo_comp_tran[ x >= 15, list( sexo, x, ER_act, S ) ]
sal_proy_x[ x >= 100, x := 100 ]
sal_proy_x[ , xf := cut( x, breaks = c( 0, 20, seq( 25, 105, 5 ) ), include.lowest = TRUE ) ]
sal_proy_x[ , ES := sum( S, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ), by = list( sexo, x ) ]
sal_proy_x <- sal_proy_x[ , list( 
  ER_act = sum( ER_act, na.rm = TRUE ), 
  S = sum( S, na.rm = TRUE ),
  VARS = sum( ( S - ES )^2, na.rm = TRUE ) ),
  by = list( sexo, xf, x ) ]
sal_proy_x[ , ES := 0 ]
sal_proy_x[ ER_act > 0, ES := S / ER_act ]
sal_proy_x[ , SDS := 0 ]
sal_proy_x[ ER_act > 0, SDS := sqrt( VARS ) ]
sal_proy_x <- sal_proy_x[ ER_act > 0 ]
sal_proy_x[ , ES98 := quantile( ES, probs = 0.98, na.rm = TRUE ), by = list( xf ) ]
sal_proy_x[ ES >= ES98, ES := ES98 ]

sal_proy <- merge.data.table(
  aux,
  sal_proy[, list( sexo, r, u, ER_act, S, VARS, ES, SDS, ES98 ) ], 
  by = c( 'sexo', 'r', 'u' ), 
  all.x = TRUE )

sal_proy <- merge.data.table( 
  sal_proy, 
  sal_proy_x[ , list( sexo, x, ESx = ES ) ], 
  by = c( 'sexo', 'x' ), 
  all.x = TRUE )

# Completando salarios faltantes
sal_proy[ , sal := ES ]
sal_proy[ is.na( ES ) & !is.na( ESx ), sal := ESx ]
sal_proy[ is.na( sal ), sal := 0 ]
sal_proy[ x - s < 15, sal := 0 ]
sal_proy[ , sal := u_r * sal ]
setorder( sal_proy, anio, sexo, x, s )
sal_proy <- sal_proy[ , list( t, anio, sexo, s, x, sal ) ]

# Guardando resultados -----------------------------------------------------------------------------
message( '\tGuardando resultados de proyección e salarios' )
save( sal_proy, file = esc$rdata_sal_proy )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista ) ) ] )
gc()
