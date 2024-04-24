# Tasas de transición ------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tIniciando proceso de alisamiento de tablas biométricas' )
message( '\tIncluye: mortalidad, cesantía/salida, ingreso, vejez, invalidez/discapacidad' )

rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )

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

## Carga de funciones ------------------------------------------------------------------------------
source( paste0( parametros$work_dir, "R/demografia/003_carga_funciones.R" ), encoding = 'UTF-8', echo = FALSE )

## Carga de datos ----------------------------------------------------------------------------------
load( parametros$demo_rdata_sgo_tran_prep )
load( parametros$demo_rdata_sgo_pea_proj )

# Parámetros ---------------------------------------------------------------------------------------
x_max <- parametros$demo_edad_max
x_lst <- 0:x_max

# (1 -> 2) Tasas ingreso desde la PEA --------------------------------------------------------------
factor <- exp( -0.1 )
## Hombres -----------------------------------------------------------------------------------------
message( '\t(1 -> 2) Suavizamiento tasa de ingreso a activo desde la PEA hombres' )
ny <- length( parametros$demo_years_analisis )

tas_1_2_h <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, Nx_prim_ing, ERx_act, ERx_ina ) %>%
  dplyr::filter( sexo == 'H' & x >= 18 & x <= 80 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( Nx_prim_ing, na.rm = TRUE ), 
                    ERx = sum( ERx_act, na.rm = TRUE ) )

aux <- PEA_proy[ 
  anio >= min( sgo_act_tran_anio$anio ) & anio <= max( sgo_act_tran_anio$anio ) & sexo == 'H', 
  list( peax = sum( peax, na.rm = TRUE ) ), by = list( x ) ]


tas_1_2_h <- tas_1_2_h %>% 
  merge( aux, by = c( 'x' ), all = TRUE ) %>%
  dplyr::mutate( Nx = ifelse( is.na( Nx ), 0, Nx ), 
                 ERx = ifelse( is.na( ERx ), 0, ERx ),
                 peax = ifelse( is.na( peax ), 0, peax ) ) %>%
  # dplyr::mutate( peax = ifelse( peax <= ERx, 0, peax - ERx ) ) %>%
  dplyr::mutate( ux = factor * ifelse( peax > 0, -log( 1 - ERx / peax / ny ), 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  # dplyr::mutate( ux = ifelse( is.infinite( ux ), -log( 1e-9 ), ux ) ) %>%
  dplyr::filter( is.finite( ux ) & ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_1_2_h, 
  boundary_knots = c( 18, 80 ), 
  knots = list( c( 30, 50 ), 
                c( 20, 25, 50, 60, 65 ) ) 
)

# Estimación de los modelos
mod_tas_1_2_h <- model_selection( list_models )
# print( mod_tas_1_2_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_1_2_h$summary$rellik_BIC == 1 ) )
print( mod_tas_1_2_h$summary[ i ] )
tas_1_2_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 18:80 ), 
  model = list_models[[ i ]],
  result = mod_tas_1_2_h$results[[ i ]] )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(1 -> 2) Suavizamiento tasa de ingreso a activo desde la PEA mujeres' )

tas_1_2_m <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, Nx_prim_ing, ERx_act, ERx_ina ) %>%
  dplyr::filter( sexo == 'M' & x >= 18 & x <= 80 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( Nx_prim_ing, na.rm = TRUE ), 
                    ERx = sum( ERx_act, na.rm = TRUE ) )

aux <- PEA_proy[ 
  anio >= min( sgo_act_tran_anio$anio ) & anio <= max( sgo_act_tran_anio$anio ) & sexo == 'M', 
  list( peax = sum( peax, na.rm = TRUE ) ), by = list( x ) ]

tas_1_2_m <- tas_1_2_m %>% 
  merge( aux, by = c( 'x' ), all = TRUE ) %>%
  dplyr::mutate( Nx = ifelse( is.na( Nx ), 0, Nx ), 
                 ERx = ifelse( is.na( ERx ), 0, ERx ),
                 peax = ifelse( is.na( peax ), 0, peax ) ) %>%
  # dplyr::mutate( peax = ifelse( peax <= ERx, 0, peax - ERx ) ) %>%
  dplyr::mutate( ux = factor * ifelse( peax > 0, -log( 1 - ERx / peax / ny ), 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  # dplyr::mutate( ux = ifelse( is.infinite( ux ), -log( 1e-9 ), ux ) ) %>%
  dplyr::filter( is.finite( ux ) & ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_1_2_m, 
  boundary_knots = c( 18, 80 ), 
  knots = list( c( 20, 50 ), 
                c( 20, 25, 50, 60, 65 ) ) 
)

# Estimación de los modelos
mod_tas_1_2_m <- model_selection( list_models )
# print( mod_tas_1_2_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_1_2_m$summary$rellik_BIC == 1 ) )
print( mod_tas_1_2_m$summary[ i ] )
tas_1_2_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 18:80 ), 
  model = list_models[[ i ]],
  result = mod_tas_1_2_m$results[[ i ]] )

## Total -------------------------------------------------------------------------------------------
message( '\t(1 -> 2) Suavizamiento tasa de ingreso a activo desde la PEA hombres y mujeres' )
tas_1_2_hm <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, Nx_prim_ing, ERx_act, ERx_ina ) %>%
  dplyr::filter( x >= 18 & x <= 80 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( Nx_prim_ing, na.rm = TRUE ), 
                    ERx = sum( ERx_act, na.rm = TRUE ) )

aux <- PEA_proy[ 
  anio >= min( sgo_act_tran_anio$anio ) & anio <= max( sgo_act_tran_anio$anio ), 
  list( peax = sum( peax, na.rm = TRUE ) ), by = list( x ) ]

tas_1_2_hm <- tas_1_2_hm %>% 
  merge( aux, by = c( 'x' ), all = TRUE ) %>%
  dplyr::mutate( Nx = ifelse( is.na( Nx ), 0, Nx ), 
                 ERx = ifelse( is.na( ERx ), 0, ERx ),
                 peax = ifelse( is.na( peax ), 0, peax ) ) %>%
  # dplyr::mutate( peax = ifelse( peax <= ERx, 0, peax - ERx ) ) %>%
  dplyr::mutate( ux = factor * ifelse( peax > 0, -log( 1 - ERx / peax / ny ), 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  # dplyr::mutate( ux = ifelse( is.infinite( ux ), -log( 1e-9 ), ux ) ) %>%
  dplyr::filter( is.finite( ux ) & ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_1_2_hm, 
  boundary_knots = c( 18, 80 ), 
  knots = list( c( 30, 50 ), 
                c( 20, 25, 50, 60, 65 ) ) 
)

# Estimación de los modelos
mod_tas_1_2_hm <- model_selection( list_models )
# print( mod_tas_1_2_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_1_2_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_1_2_hm$summary[ i ] )
tas_1_2_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 18:80 ), 
  model = list_models[[ i ]],
  result = mod_tas_1_2_hm$results[[ i ]] )

# (2 -> 3) Tasas de salidas de activos a cesantes --------------------------------------------------
factor <- exp( 0.22 )
## Hombres -----------------------------------------------------------------------------------------
message( '\t(2 -> 3) Suavizamiento tasa de salidas de activos hombres' )
# Seleccion de datos
tas_2_3_h <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_sal, Nx_dec_act, Nx_vej, Nx_inv, Nx_dis ) %>%
  dplyr::filter( sexo == 'H' & x >= 15 & x <= 90 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( max( Nx_sal - Nx_dec_act - Nx_vej - Nx_inv - Nx_dis, 0 ) ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_3_h, 
  boundary_knots = c( 15, x_max ), 
  knots = list( c( 30, 60 ),
                c( 30, 60, 70 ),
                c( 20, 30, 60, 70 ) ) 
)

# Estimación de los modelos
mod_tas_2_3_h <- model_selection( list_models )
# print( mod_tas_2_3_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_3_h$summary$rellik_BIC == 1 ) )
print( mod_tas_2_3_h$summary[ i ] )
tas_2_3_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_2_3_h$results[[ i ]] )

# extrapolación superior 
range <- c( 86, 105 )
x0 <- range
dx0 <- range
inflex_x <- 104
int <- c( -5, 2 )
tas_2_3_h <- curve_completion( 
  data = tas_2_3_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -4) ), dymax = 0.0, int = int, mono_chk = FALSE )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(2 -> 3) Suavizamiento tasa de salidas de activos mujeres' )
# Seleccion de datos
tas_2_3_m <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_sal, Nx_dec_act, Nx_vej, Nx_inv, Nx_dis ) %>%
  dplyr::filter( sexo == 'M' & x >= 15 & x <= 90 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( max( Nx_sal - Nx_dec_act - Nx_vej - Nx_inv - Nx_dis, 0 ) ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_3_m, 
  boundary_knots = c( 15, x_max ), 
  knots = list( c( 30, 60 ),
                c( 30, 60, 70 ),
                c( 20, 30, 60, 70 ) ) 
)

# Estimación de los modelos
mod_tas_2_3_m <- model_selection( list_models )
# print( mod_tas_2_3_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_3_m$summary$rellik_BIC == 1 ) )
print( mod_tas_2_3_m$summary[ i ] )
tas_2_3_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_2_3_m$results[[ i ]] )

# extrapolación superior 
range <- c( 87, 105 )
x0 <- range
dx0 <- range
inflex_x <- 104
int <- c( -5, 3 )
tas_2_3_m <- curve_completion( 
  data = tas_2_3_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -4) ), dymax = 0.0, int = int, mono_chk = FALSE )

## Total -------------------------------------------------------------------------------------------
message( '\t(2 -> 3) Suavizamiento tasa de salidas de activos hombres y mujeres' )
# Seleccion de datos
tas_2_3_hm <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_sal, Nx_dec_act, Nx_vej, Nx_inv, Nx_dis ) %>%
  dplyr::filter( x >= 15 & x <= 90 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( max( Nx_sal - Nx_dec_act - Nx_vej - Nx_inv - Nx_dis, 0 ) ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_3_hm, 
  boundary_knots = c( 15, x_max ), 
  knots = list( c( 30, 60 ),
                c( 30, 60, 70 ),
                c( 20, 30, 60, 70 ) ) 
)

# Estimación de los modelos
mod_tas_2_3_hm <- model_selection( list_models )
# print( mod_tas_2_3_hm$summary )

# Tabla biométrica
i <- 1 #min( which( mod_tas_2_3_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_2_3_hm$summary[ i ] )
tas_2_3_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_2_3_hm$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 95
int <- c( -5, 3 )
tas_2_3_hm <- curve_completion( 
  data = tas_2_3_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -4) ), dymax = 0.0, int = int, mono_chk = FALSE )

# (2 -> 4) Tasas de retiro por vejez ---------------------------------------------------------------
# Luego de revisar las tasas brutas he llegado a las siguientes conclusiones:
# 1. Suavizarlas por partes en los intervalos [55,59], [60, 64], [65,69] y [70,Inf], pues así es la 
# realidad conforme a las reglas de las condiciones de jubilación
## Hombres -----------------------------------------------------------------------------------------
message( '\t(2 -> 4) Suavizamiento tasa de salida por vejez de activos hombres' )
# Seleccion de datos
tas_2_4_h <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_vej ) %>%
  dplyr::filter( sexo == 'H' & x >= 55 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( Nx_vej ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

aux <- data.table( x = 55:x_max )
aux[ , xf := factor( x, levels = x_lst, ordered = TRUE ) ]
aux <- merge.data.table( aux, as.data.table( tas_2_4_h ), by = c( 'x', 'xf' ), all.x = TRUE )
aux[, piece := cut( x, breaks = c( 55, 60, 65, 70, 105 ), include.lowest = TRUE, labels = FALSE, right = FALSE ) ]
np <- max( aux$piece )
df <- c( 3, 3, 4, 8 )
aux1 <- data.table()
for ( i in 1:np ) {
  aux2 <- aux[ piece == i ]
  mod <- smooth.spline( aux2[ !is.na( ux ) ]$x, log( aux2[ !is.na( ux ) ]$ux ), df = df[ i ] ) # df degrees of freedom
  aux2[ , ux_obs := ux ]
  aux2[ , log_ux := predict( mod, aux2[ , list( x ) ] )$y ]
  aux1 <- rbind( aux1, aux2)
}

# Tabla biométrica
aux1[ , piece := NULL ]
aux1[ , id := '2012-2020']
aux1[ , sexo := 'H' ]
aux1[ , sexo := factor( sexo, levels = c('H', 'M', 'HM' ) ) ]
aux1[ , ux := exp( log_ux ) ]
aux1[ , px := exp( -ux ) ]
aux1[ , qx := 1 - px ]
aux1[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( id, sexo ) ]
aux1[ , lx := 1e5 * cumprod( lx ), by = list( id, sexo ) ]
aux1[ , dx := lx * qx ]
aux1[ , ex := rev( cumsum( rev( lx ) ) ), by = list( id, sexo ) ]
aux1[ , ex := ex / lx - 0.5 ]
aux1[ is.nan( ex ), ex := 0 ]
aux1[ , log_ux := NULL ]

tas_2_4_h <- copy( aux1 )
rm( aux, aux1, aux2 )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(2 -> 4) Suavizamiento tasa de salida por vejez de activos mujeres' )
# Seleccion de datos
tas_2_4_m <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_vej ) %>%
  dplyr::filter( sexo == 'M' & x >= 55 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( Nx_vej ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

aux <- data.table( x = 55:x_max )
aux[ , xf := factor( x, levels = x_lst, ordered = TRUE ) ]
aux <- merge.data.table( aux, as.data.table( tas_2_4_m ), by = c( 'x', 'xf' ), all.x = TRUE )
aux[, piece := cut( x, breaks = c( 55, 60, 65, 70, 105 ), include.lowest = TRUE, labels = FALSE, right = FALSE ) ]
np <- max( aux$piece )
df <- c( 3, 3, 4, 9 )
aux1 <- data.table()
for ( i in 1:np ) {
  aux2 <- aux[ piece == i ]
  mod <- smooth.spline( aux2[ !is.na( ux ) ]$x, log( aux2[ !is.na( ux ) ]$ux ), df = df[ i ] ) # df degrees of freedom
  aux2[ , ux_obs := ux ]
  aux2[ , log_ux := predict( mod, aux2[ , list( x ) ] )$y ]
  aux1 <- rbind( aux1, aux2)
}

# Tabla biométrica
aux1[ , piece := NULL ]
aux1[ , id := '2012-2020']
aux1[ , sexo := 'M' ]
aux1[ , sexo := factor( sexo, levels = c('H', 'M', 'HM' ) ) ]
aux1[ , ux := exp( log_ux ) ]
aux1[ , px := exp( -ux ) ]
aux1[ , qx := 1 - px ]
aux1[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( id, sexo ) ]
aux1[ , lx := 1e5 * cumprod( lx ), by = list( id, sexo ) ]
aux1[ , dx := lx * qx ]
aux1[ , ex := rev( cumsum( rev( lx ) ) ), by = list( id, sexo ) ]
aux1[ , ex := ex / lx - 0.5 ]
aux1[ is.nan( ex ), ex := 0 ]
aux1[ , log_ux := NULL ]

tas_2_4_m <- copy( aux1 )
rm( aux, aux1, aux2 )

## Total -------------------------------------------------------------------------------------------
message( '\t(2 -> 4) Suavizamiento tasa de salida por vejez de activos hombres y mujeres (unisex)' )
# Seleccion de datos 
tas_2_4_hm <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_vej ) %>%
  dplyr::filter( x >= 55 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( Nx_vej ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

aux <- data.table( x = 55:x_max )
aux[ , xf := factor( x, levels = x_lst, ordered = TRUE ) ]
aux <- merge.data.table( aux, as.data.table( tas_2_4_hm ), by = c( 'x', 'xf' ), all.x = TRUE )
aux[, piece := cut( x, breaks = c( 55, 60, 65, 70, 105 ), include.lowest = TRUE, labels = FALSE, right = FALSE ) ]
np <- max( aux$piece )
df <- c( 3, 3, 4, 8 )
aux1 <- data.table()
for ( i in 1:np ) {
  aux2 <- aux[ piece == i ]
  mod <- smooth.spline( aux2[ !is.na( ux ) ]$x, log( aux2[ !is.na( ux ) ]$ux ), df = df[ i ] ) # df degrees of freedom
  aux2[ , ux_obs := ux ]
  aux2[ , log_ux := predict( mod, aux2[ , list( x ) ] )$y ]
  aux1 <- rbind( aux1, aux2)
}

# Tabla biométrica
aux1[ , piece := NULL ]
aux1[ , id := '2012-2020']
aux1[ , sexo := 'HM' ]
aux1[ , sexo := factor( sexo, levels = c('H', 'M', 'HM' ) ) ]
aux1[ , ux := exp( log_ux ) ]
aux1[ , px := exp( -ux ) ]
aux1[ , qx := 1 - px ]
aux1[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( id, sexo ) ]
aux1[ , lx := 1e5 * cumprod( lx ), by = list( id, sexo ) ]
aux1[ , dx := lx * qx ]
aux1[ , ex := rev( cumsum( rev( lx ) ) ), by = list( id, sexo ) ]
aux1[ , ex := ex / lx - 0.5 ]
aux1[ is.nan( ex ), ex := 0 ]
aux1[ , log_ux := NULL ]

tas_2_4_hm <- copy( aux1 )
rm( aux, aux1, aux2 )

# (2 -> 5) Tasas de retiro por invalidez -----------------------------------------------------------
factor <- exp( -0.03 )
## Hombres -----------------------------------------------------------------------------------------
message( '\t(2 -> 5) Suavizamiento tasa de salida por invalidez de activos hombres' )
# Seleccion de datos
tas_2_5_h <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_inv ) %>%
  dplyr::filter( sexo == 'H' & x >= 18 & x <= 80 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( Nx_inv ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_5_h, 
  boundary_knots = c( 18, 80 ), 
  knots = list( c( 60, 65, 80 ),
                c( 55, 60, 65, 70, 80 ) ) 
)


# Estimación de los modelos
mod_tas_2_5_h <- model_selection( list_models )
# print( mod_tas_2_5_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_5_h$summary$rellik_BIC == 1 ) )
print( mod_tas_2_5_h$summary[ i ] )
tas_2_5_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 18:80 ), 
  model = list_models[[ i ]],
  result = mod_tas_2_5_h$results[[ i ]] )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(2 -> 5) Suavizamiento tasa de salida por invalidez de activos mujeres' )
# Seleccion de datos
tas_2_5_m <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_inv ) %>%
  dplyr::filter( sexo == 'M' & x >= 18 & x <= 80 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( Nx_inv ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_5_m, 
  boundary_knots = c( 18, 80 ), 
  knots = list( c( 60, 65, 80 ),
                c( 55, 60, 65, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_2_5_m <- model_selection( list_models )
# print( mod_tas_2_5_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_5_m$summary$rellik_BIC == 1 ) )
print( mod_tas_2_5_m$summary[ i ] )
tas_2_5_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 18:80 ), 
  model = list_models[[ i ]],
  result = mod_tas_2_5_m$results[[ i ]] )

## Total -------------------------------------------------------------------------------------------
message( '\t(2 -> 5) Suavizamiento tasa de salida de activos hombres y mujeres por invalidez' )
# Seleccion de datos
tas_2_5_hm <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, Nx_inv ) %>%
  dplyr::filter( x >= 18 & x <= 80 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_act ), 
                    Nx = sum( Nx_inv ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_5_hm, 
  boundary_knots = c( 18, 80 ), 
  knots = list( c( 60, 65, 80 ),
                c( 55, 60, 65, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_2_5_hm <- model_selection( list_models )
# print( mod_tas_2_5_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_5_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_2_5_hm$summary[ i ] )
tas_2_5_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 18:80 ), 
  model = list_models[[ i ]],
  result = mod_tas_2_5_hm$results[[ i ]] )

# (2,3 -> 6) Tasas mortalidad de activos del SGO ---------------------------------------------------
## Hombres -----------------------------------------------------------------------------------------
message( '\t(2,3 -> 6) Suavizamiento tasa de mortalidad de activos hombres' )
# Seleccion de datos
tas_2_6_h <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx, Nx_dec, ux_dec ) %>%
  dplyr::filter( sexo == 'H' & x >= 15 & x <= 105 ) %>%
  dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx_dec ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_6_h, 
  boundary_knots = c( 15, x_max ), 
  knots = list( c( 15, 25, 40 ), 
                c( 15, 25, 40, 75 ), 
                c( 15, 25, 40, 75 ) )
)

# Estimación de los modelos
mod_tas_2_6_h <- model_selection( list_models )
# print( mod_tas_2_6_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_6_h$summary$rellik_BIC == 1 ) )
print( mod_tas_2_6_h$summary[ i ] )
tas_2_6_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_2_6_h$results[[ i ]] )

# extrapolación superior 
range <- c( 75, 105 )
x0 <- range
dx0 <- range
inflex_x <- 81
int <- c( -10, 2 )
tas_2_6_h <- curve_completion( 
  data = tas_2_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.75, int = int )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(2,3 -> 6) Suavizamiento tasa de mortalidad de activos mujeres' )
# Seleccion de datos
tas_2_6_m <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx, Nx_dec, ux_dec ) %>%
  dplyr::filter( sexo == 'M' & x >= 15 & x <= 105 ) %>%
  dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx_dec ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_6_m, 
  boundary_knots = c( 15, x_max ), 
  knots = list( c( 15, 23, 40 ), 
                c( 15, 23, 40, 75 ), 
                c( 15, 23, 40, 75 ) )
)

# Estimación de los modelos
mod_tas_2_6_m <- model_selection( list_models )
# print( mod_tas_2_6_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_6_m$summary$rellik_BIC == 1 ) )
print( mod_tas_2_6_m$summary[ i ] )
tas_2_6_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_2_6_m$results[[ i ]] )

# extrapolación superior 
range <- c( 75, 105 )
x0 <- range
dx0 <- range
inflex_x <- 81
int <- c( -10, 2 )
tas_2_6_m <- curve_completion( 
  data = tas_2_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.75, int = int )

## Total -------------------------------------------------------------------------------------------
message( '\t(2,3 -> 6) Suavizamiento tasa de mortalidad de activos hombres y mujeres' )
# Seleccion de datos
tas_2_6_hm <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx, Nx_dec, ux_dec ) %>%
  dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::filter( x >= 18 & x <= 95 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx_dec ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_2_6_hm, 
  boundary_knots = c( 15, x_max ), 
  knots = list( c( 15, 24, 40 ), 
                c( 15, 24, 40, 75 ), 
                c( 15, 24, 40, 75 ) )
)

# Estimación de los modelos
mod_tas_2_6_hm <- model_selection( list_models )
# print( mod_tas_2_6_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_2_6_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_2_6_hm$summary[ i ] )
tas_2_6_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_2_6_hm$results[[ i ]] )

# extrapolación superior 
range <- c( 75, 105 )
x0 <- range
dx0 <- range
inflex_x <- 81
int <- c( -10, 2 )
tas_2_6_hm <- curve_completion( 
  data = tas_2_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.75, int = int )

# (3 -> 2) Tasas de ingreso de inactivos -----------------------------------------------------------
factor <- exp( 0.04 )
## Hombres -----------------------------------------------------------------------------------------
message( '\t(3 -> 2) Suavizamiento tasa de ingreso de inactivos hombres' )
# Seleccion de datos
tas_3_2_h <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_ina, Nx_ing, Nx_prim_ing ) %>%
  dplyr::filter( sexo == 'H' & x >= 18 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_ina ), 
                    Nx = sum( Nx_ing - Nx_prim_ing ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_3_2_h, 
  boundary_knots = c( 15, 105 ), 
  knots = list( c( 20, 30, 59, 60, 77, 80 ),
                c( 20, 30, 59, 60, 83, 94, 95 ),
                c( 20, 30, 59, 60, 77, 83, 94, 95 ) ) 
)

# Estimación de los modelos
mod_tas_3_2_h <- model_selection( list_models )
# print( mod_tas_3_2_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_3_2_h$summary$rellik_BIC == 1 ) )
print( mod_tas_3_2_h$summary[ i ] )
tas_3_2_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 15:105 ), 
  model = list_models[[ i ]],
  result = mod_tas_3_2_h$results[[ i ]] )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(3 -> 2) Suavizamiento tasa de ingresos de inactivos mujeres' )
# Seleccion de datos
tas_3_2_m <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_ina, Nx_ing, Nx_prim_ing ) %>%
  dplyr::filter( sexo == 'M' & x >= 18 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_ina ), 
                    Nx = sum( Nx_ing - Nx_prim_ing ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_3_2_m, 
  boundary_knots = c( 15, 105 ), 
  knots = list( c( 20, 30, 59, 60, 70 ),
                c( 20, 30, 59, 60, 70, 80 ),
                c( 20, 30, 59, 60, 70, 80, 95 ) ) 
)

# Estimación de los modelos
mod_tas_3_2_m <- model_selection( list_models )
# print( mod_tas_3_2_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_3_2_m$summary$rellik_BIC == 1 ) )
print( mod_tas_3_2_m$summary[ i ] )
tas_3_2_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 15:105 ), 
  model = list_models[[ i ]],
  result = mod_tas_3_2_m$results[[ i ]] )

## Total -------------------------------------------------------------------------------------------
message( '\t(3 -> 2) Suavizamiento tasa de ingresos de inactivos hombres y mujeres' )
# Seleccion de datos
tas_3_2_hm <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_ina, Nx_ing, Nx_prim_ing ) %>%
  dplyr::filter( x >= 18 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx_ina ), 
                    Nx = sum( Nx_ing - Nx_prim_ing ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_3_2_hm, 
  boundary_knots = c( 15, 105 ), 
  knots = list( c( 20, 25, 70, 80, 85, 90 ),
                c( 20, 25, 45, 70, 80, 85, 90 ),
                c( 20, 25, 45, 60, 70, 80, 85, 90 ) ) 
)

# Estimación de los modelos
mod_tas_3_2_hm <- model_selection( list_models )
# print( mod_tas_3_2_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_3_2_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_3_2_hm$summary[ i ] )
tas_3_2_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 15:105 ), 
  model = list_models[[ i ]],
  result = mod_tas_3_2_hm$results[[ i ]] )

# (4 -> 6) Tasas mortalidad de pensionistas de vejez -----------------------------------------------
## Hombres -----------------------------------------------------------------------------------------
message( '\t(4 -> 6) Suavizamiento tasa de mortalidad de pensionistas de vejez hombres' )
# Seleccion de datos
tas_4_6_h <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'VEJEZ' & x >= 55 & x <= 105 ) %>%
  dplyr::mutate( x = case_when( x < 100 ~ x, x >= 100 ~ 100 ) ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) )

# aux <- sgo_act_tran_anio %>%
#   dplyr::select( anio, sexo, x, ERx_ina, Nx_dec_ina, ux_dec ) %>%
#   dplyr::filter( sexo == 'H' & x >= 55 & x <= 105 ) %>%
#   dplyr::group_by( x ) %>%
#   dplyr::summarise( ERx_ina = sum( ERx_ina ),
#                     Nx_ina = sum( Nx_dec_ina ) )

# tas_4_6_h <- merge( tas_4_6_h, aux, by = 'x', all.x = TRUE, all.y = TRUE )

tas_4_6_h <- tas_4_6_h %>%
  dplyr::mutate( ERx = ifelse( is.na( ERx ), 0, ERx ), 
                 # ERx_ina = ifelse( is.na( ERx_ina ), 0, ERx_ina ),
                 Nx = ifelse( is.na( Nx ), 0, Nx ),
                 # Nx_ina = ifelse( is.na( Nx_ina ), 0, Nx_ina ) 
  ) %>%
  # dplyr::mutate( ux = ifelse( ERx + ERx_ina > 0, ( Nx + Nx_ina ) / ( ERx + ERx_ina ), 0 ), 
  #                xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, ( Nx ) / ( ERx ), 0 ),
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_4_6_h, 
  boundary_knots = c( 55, x_max ), 
  knots = list( c( 55, 70, 80 ),
                c( 55, 65, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_4_6_h <- model_selection( list_models )
# print( mod_tas_4_6_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_4_6_h$summary$rellik_BIC == 1 ) )
print( mod_tas_4_6_h$summary[ i ] )
tas_4_6_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 55:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_4_6_h$results[[ i ]] )

# extrapolación superior 
range <- c( 93, 105 )
x0 <- range
dx0 <- range
inflex_x <- 93
int <- c( -2, 1 )
tas_4_6_h <- curve_completion( 
  data = tas_4_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.9, int = int )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(4 -> 6) Suavizamiento tasa de mortalidad de pensionistas de vejez mujeres' )
# Seleccion de datos
tas_4_6_m <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'VEJEZ' & x >= 55 & x <= 105 ) %>%
  dplyr::mutate( x = case_when( x < 100 ~ x, x >= 100 ~ 100 ) ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) )

# aux <- sgo_act_tran_anio %>%
#   dplyr::select( anio, sexo, x, ERx_ina, Nx_dec_ina, ux_dec ) %>%
#   dplyr::filter( sexo == 'M' & x >= 55 & x <= 105 ) %>%
#   dplyr::group_by( x ) %>%
#   dplyr::summarise( ERx_ina = sum( ERx_ina ),
#                     Nx_ina = sum( Nx_dec_ina ) )

# tas_4_6_m <- merge( tas_4_6_m, aux, by = 'x', all.x = TRUE, all.y = TRUE )

tas_4_6_m <- tas_4_6_m %>%
  dplyr::mutate( ERx = ifelse( is.na( ERx ), 0, ERx ), 
                 # ERx_ina = ifelse( is.na( ERx_ina ), 0, ERx_ina ),
                 Nx = ifelse( is.na( Nx ), 0, Nx ),
                 # Nx_ina = ifelse( is.na( Nx_ina ), 0, Nx_ina ) 
  ) %>%
  # dplyr::mutate( ux = ifelse( ERx + ERx_ina > 0, ( Nx + Nx_ina ) / ( ERx + ERx_ina ), 0 ),
  #                xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, ( Nx ) / ( ERx ), 0 ),
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_4_6_m, 
  boundary_knots = c( 55, x_max ), 
  knots = list( c( 60, 75, 80 ),
                c( 60, 75, 80, 95 ) ) 
)

# Estimación de los modelos
mod_tas_4_6_m <- model_selection( list_models )
# print( mod_tas_4_6_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_4_6_m$summary$rellik_BIC == 1 ) )
print( mod_tas_4_6_m$summary[ i ] )
tas_4_6_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 55:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_4_6_m$results[[ i ]] )

# extrapolación superior 
range <- c( 93, 105 )
x0 <- range
dx0 <- range
inflex_x <- 93
int <- c( -2, 1 )
tas_4_6_m <- curve_completion( 
  data = tas_4_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.9, int = int )

## Total -------------------------------------------------------------------------------------------
message( '\t(4 -> 6) Suavizamiento tasa de mortalidad de pensionistas de vejez hombres y mujeres' )
# Seleccion de datos
tas_4_6_hm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( tipo == 'VEJEZ' & x >= 55 & x <= 105 ) %>%
  dplyr::mutate( x = case_when( x < 100 ~ x, x >= 100 ~ 100 ) ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) )

# aux <- sgo_act_tran_anio %>%
#   dplyr::select( anio, sexo, x, ERx_ina, Nx_dec_ina, ux_dec ) %>%
#   dplyr::filter( x >= 55 & x <= 105 ) %>%
#   dplyr::group_by( x ) %>%
#   dplyr::summarise( ERx_ina = sum( ERx_ina ),
#                     Nx_ina = sum( Nx_dec_ina ) )

# tas_4_6_hm <- merge( tas_4_6_hm, aux, by = 'x', all.x = TRUE, all.y = TRUE )

tas_4_6_hm <- tas_4_6_hm %>%
  dplyr::mutate( ERx = ifelse( is.na( ERx ), 0, ERx ), 
                 # ERx_ina = ifelse( is.na( ERx_ina ), 0, ERx_ina ),
                 Nx = ifelse( is.na( Nx ), 0, Nx ),
                 # Nx_ina = ifelse( is.na( Nx_ina ), 0, Nx_ina ) 
  ) %>%
  # dplyr::mutate( ux = ifelse( ERx + ERx_ina > 0, ( Nx + Nx_ina ) / ( ERx + ERx_ina ), 0 ),
  #                xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, ( Nx ) / ( ERx ), 0 ),
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_4_6_hm, 
  boundary_knots = c( 55, x_max ), 
  knots = list( c( 55, 70, 80 ),
                c( 55, 65, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_4_6_hm <- model_selection( list_models )
# print( mod_tas_4_6_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_4_6_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_4_6_hm$summary[ i ] )
tas_4_6_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 55:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_4_6_hm$results[[ i ]] )

# extrapolación superior 
range <- c( 93, 105 )
x0 <- range
dx0 <- range
inflex_x <- 93
int <- c( -2, 1 )
tas_4_6_hm <- curve_completion( 
  data = tas_4_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.9, int = int )

# (5 -> 6) Tasas mortalidad de pensionistas de invalidez -------------------------------------------
## Hombres -----------------------------------------------------------------------------------------
message( '\t(5 -> 6) Suavizamiento tasa de mortalidad de pensionistas de invalidez hombres' )
# Seleccion de datos
tas_5_6_h <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_5_6_h, 
  boundary_knots = c( 25, x_max ), 
  knots = list( c( 33, 55 ),
                c( 33, 55, 80 ) ) 
)

# Estimación de los modelos
mod_tas_5_6_h <- model_selection( list_models )
# print( mod_tas_5_6_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_5_6_h$summary$rellik_BIC == 1 ) )
print( mod_tas_5_6_h$summary[ i ] )
tas_5_6_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_5_6_h$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 92
int <- c( -2, 1 )
tas_5_6_h <- curve_completion( 
  data = tas_5_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 1, int = int )

# extrapolación inferior
range <- c( 15, 34 )
x0 <- range
dx0 <- range
inflex_x <- 15
int <- c( -3, 3 )
aux <- curve_completion( 
  data = tas_5_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -4.001 ) ), dymax = 0.15, 
  int = int, right = FALSE, mono_chk = FALSE )

tas_5_6_h <- rbindlist( list( aux[ x <= range[2] ], tas_5_6_h[ x > range[2] ] ) )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(5 -> 6) Suavizamiento tasa de mortalidad de pensionistas de invalidez mujeres' )
# Seleccion de datos
tas_5_6_m <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_5_6_m, 
  boundary_knots = c( 25, x_max ), 
  knots = list( c( 45, 70 ),
                c( 45, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_5_6_m <- model_selection( list_models )
# print( mod_tas_5_6_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_5_6_m$summary$rellik_BIC == 1 ) )
print( mod_tas_5_6_m$summary[ i ] )
tas_5_6_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_5_6_m$results[[ i ]] )

# extrapolación superior 
range <- c( 91, 105 )
x0 <- range
dx0 <- range
inflex_x <- 93
int <- c( -3, 4 )
tas_5_6_m <- curve_completion( 
  data = tas_5_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 1.25, int = int )

# extrapolación inferior
range <- c( 15, 34 )
x0 <- range
dx0 <- range
inflex_x <- 19
int <- c( -5, 3 )
aux <- curve_completion( 
  data = tas_5_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -3.999 ) ), dymax = 0.1, 
  int = int, right = FALSE, mono_chk = FALSE )

tas_5_6_m <- rbindlist( list( aux[ x <= range[2] ], tas_5_6_m[ x > range[2] ] ) )


## Total -------------------------------------------------------------------------------------------
message( '\t(5 -> 6) Suavizamiento tasa de mortalidad de pensionistas de invalidez hombres y mujeres' )
# Seleccion de datos
tas_5_6_hm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_5_6_hm, 
  boundary_knots = c( 25, x_max ), 
  knots = list( c( 35, 73 ),
                c( 35, 73, 80 ) ) 
)

# Estimación de los modelos
mod_tas_5_6_hm <- model_selection( list_models )
# print( mod_tas_5_6_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_5_6_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_5_6_hm$summary[ i ] )
tas_5_6_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 15:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_5_6_hm$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 90
int <- c( -6, 3 )
tas_5_6_hm <- curve_completion( 
  data = tas_5_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 1.2, int = int )

# extrapolación inferior
range <- c( 15, 34 )
x0 <- range
dx0 <- range
inflex_x <- 15
int <- c( -3, 3 )
aux <- curve_completion( 
  data = tas_5_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -4.001 ) ), dymax = 0.15, 
  int = int, right = FALSE, mono_chk = FALSE )

tas_5_6_hm <- rbindlist( list( aux[ x <= range[2] ], tas_5_6_hm[ x > range[2] ] ) )

# (7 -> 6) Tasas mortalidad de pensionistas de viudedad --------------------------------------------
## Hombres -----------------------------------------------------------------------------------------
message( '\t(7 -> 6) Suavizamiento tasa de mortalidad de pensionistas de viudedad hombres' )
# Seleccion de datos
tas_7_6_h <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'VIUDEDAD' & x >= 20 & x <= 100 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_7_6_h, 
  boundary_knots = c( 20, x_max ), 
  knots = list( c( 50, 60, 70 ),
                c( 50, 60, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_7_6_h <- model_selection( list_models )
# print( mod_tas_7_6_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_7_6_h$summary$rellik_BIC == 1 ) )
print( mod_tas_7_6_h$summary[ i ] )
tas_7_6_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 20:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_7_6_h$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 105
int <- c( -5, 2 )
tas_7_6_h <- curve_completion( 
  data = tas_7_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.65, int = int )

# extrapolación inferior
range <- c( 20, 35 )
x0 <- range
dx0 <- range
inflex_x <- 21
int <- c( -0, 3 )
tas_7_6_h <- curve_completion( 
  data = tas_7_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -6 ) ), dymax = 0.25, 
  int = int, right = FALSE, mono_chk = FALSE )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(7 -> 6) Suavizamiento tasa de mortalidad de pensionistas de viudedad mujeres' )
# Seleccion de datos
tas_7_6_m <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'VIUDEDAD' & x >= 18 & x <= 100 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_7_6_m, 
  boundary_knots = c( 18, x_max ), 
  knots = list( c( 50, 60, 70 ),
                c( 50, 60, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_7_6_m <- model_selection( list_models )
# print( mod_tas_7_6_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_7_6_m$summary$rellik_BIC == 1 ) )
print( mod_tas_7_6_m$summary[ i ] )
tas_7_6_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 18:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_7_6_m$results[[ i ]] )

# extrapolación superior 
range <- c( 96, 105 )
x0 <- range
dx0 <- range
inflex_x <- 105
int <- c( -5, 3 )
tas_7_6_m <- curve_completion( 
  data = tas_7_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.75, int = int )

# extrapolación inferior
range <- c( 18, 34 )
x0 <- range
dx0 <- range
inflex_x <- 20
int <- c( -0, 3 )
tas_7_6_m <- curve_completion( 
  data = tas_7_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -6 ) ), dymax = 0.001, 
  int = int, right = FALSE, mono_chk = FALSE )

## Total -------------------------------------------------------------------------------------------
message( '\t(7 -> 6) Suavizamiento tasa de mortalidad de pensionistas de viudedad hombres y mujeres' )
# Seleccion de datos
tas_7_6_hm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( tipo == 'VIUDEDAD' & x >= 20 & x <= 100 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_7_6_hm, 
  boundary_knots = c( 20, x_max ), 
  knots = list( c( 50, 60, 70 ),
                c( 50, 60, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_7_6_hm <- model_selection( list_models )
# print( mod_tas_7_6_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_7_6_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_7_6_hm$summary[ i ] )
tas_7_6_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 20:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_7_6_hm$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 105
int <- c( -5, 2 )
tas_7_6_hm <- curve_completion( 
  data = tas_7_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.65, int = int )

# extrapolación inferior
range <- c( 20, 37 )
x0 <- range
dx0 <- range
inflex_x <- 36
int <- c( -2, 2 )
tas_7_6_hm <- curve_completion( 
  data = tas_7_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -6 ) ), dymax = 0.1, 
  int = int, right = FALSE, mono_chk = FALSE )

# (8 -> 6) Tasas mortalidad de pensionistas de orfandad --------------------------------------------
## Hombres -----------------------------------------------------------------------------------------
message( '\t(8 -> 6) Suavizamiento tasa de mortalidad de pensionistas de orfandad hombres' )
# Seleccion de datos
tas_8_6_h <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'ORFANDAD' & x >= 0 & x <= x_max ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_8_6_h, 
  boundary_knots = c( 0, x_max ), 
  knots = list( c( 19, 30, 85 ),
                c( 19, 30, 70, 80 ) ) 
)

# Estimación de los modelos
mod_tas_8_6_h <- model_selection( list_models )
# print( mod_tas_8_6_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_8_6_h$summary$rellik_BIC == 1 ) )
print( mod_tas_8_6_h$summary[ i ] )
tas_8_6_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 0:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_8_6_h$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 105
int <- c( -5, 3 )
tas_8_6_h <- curve_completion( 
  data = tas_8_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.5, int = int )

# extrapolación inferior
range <- c( 0, 18 )
x0 <- range
dx0 <- range
inflex_x <- 0
int <- c( -4, 2 )
tas_8_6_h <- curve_completion( 
  data = tas_8_6_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -6 ) ), dymax = -0.5, 
  int = int, right = FALSE, mono_chk = FALSE )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(8 -> 6) Suavizamiento tasa de mortalidad de pensionistas de orfandad mujeres' )
# Seleccion de datos
tas_8_6_m <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'ORFANDAD' & x >= 0 & x <= x_max ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_8_6_m, 
  boundary_knots = c( 0, x_max ), 
  knots = list( c( 5, 10, 20, 40 ),
                c( 5, 10, 20, 25, 40 ) ) 
)

# Estimación de los modelos
mod_tas_8_6_m <- model_selection( list_models )
# print( mod_tas_8_6_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_8_6_m$summary$rellik_BIC == 1 ) )
print( mod_tas_8_6_m$summary[ i ] )
tas_8_6_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 0:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_8_6_m$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 91
int <- c( -10, 2 )
tas_8_6_m <- curve_completion( 
  data = tas_8_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 1, int = int )

# extrapolación inferior
range <- c( 0, 17 )
x0 <- range
dx0 <- range
inflex_x <- 0
int <- c( -4, 2 )
tas_8_6_m <- curve_completion( 
  data = tas_8_6_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -6 ) ), dymax = -0.5, 
  int = int, right = FALSE, mono_chk = FALSE )

## Total -------------------------------------------------------------------------------------------
message( '\t(8 -> 6) Suavizamiento tasa de mortalidad de pensionistas de orfandad hombres y mujeres' )
# Seleccion de datos
tas_8_6_hm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( tipo == 'ORFANDAD' & x >= 0 & x <= x_max ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( ERx = sum( ERx ), 
                    Nx = sum( Nx ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_8_6_hm, 
  boundary_knots = c( 0, x_max ), 
  knots = list( c( 5, 10, 20, 40 ),
                c( 5, 10, 20, 25, 40 ) ) 
)

# Estimación de los modelos
mod_tas_8_6_hm <- model_selection( list_models )
# print( mod_tas_8_6_hm$summary )

# Tabla biométrica
i <- min( which( mod_tas_8_6_hm$summary$rellik_BIC == 1 ) )
print( mod_tas_8_6_hm$summary[ i ] )
tas_8_6_hm <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'HM', levels = c( 'H', 'M', 'HM' ) ), x = 0:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_8_6_hm$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 91
int <- c( -10, 2 )
tas_8_6_hm <- curve_completion( 
  data = tas_8_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 1, int = int )

# extrapolación inferior
range <- c( 0, 17 )
x0 <- range
dx0 <- range
inflex_x <- 0
int <- c( -4, 2 )
tas_8_6_hm <- curve_completion( 
  data = tas_8_6_hm, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -6 ) ), dymax = -0.5, 
  int = int, right = FALSE, mono_chk = FALSE )

# (7 -> 0) Tasas de terminación de viudedad --------------------------------------------------------
## Hombres -----------------------------------------------------------------------------------------
message( '\t(7 -> 0) Suavizamiento tasa de terminación de hombres viudos' )
factor <- exp(-0.1)
tas_7_0_h <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx, Nx_sal, Nx ) %>%
  dplyr::filter( sexo == 'H' & tipo %in% c( 'VIUDEDAD' ) & x >= 18 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( pmax( Nx_sal, Nx ), na.rm = TRUE ), 
                    ERx = sum( ERx, na.rm = TRUE ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::mutate( ux = ifelse( is.infinite( ux ), -log( 1e-9 ), ux ) ) %>%
  dplyr::filter( ux != 0 )

setDT( tas_7_0_h )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_7_0_h, 
  boundary_knots = c( 18, 105 ), 
  knots = list( c( 25, 55, 70 ),
                c( 25, 55, 70, 85 ) ) 
)

# Estimación de los modelos
mod_tas_7_0_h <- model_selection( list_models )
# print( mod_tas_7_0_h_m18$summary )

# Tabla biométrica
i <- min( which( mod_tas_7_0_h$summary$rellik_BIC == 1 ) )
print( mod_tas_7_0_h$summary[ i ] )
tas_7_0_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 18:105 ), 
  model = list_models[[ i ]],
  result = mod_tas_7_0_h$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 105
int <- c( -5, 2 )
tas_7_0_h <- curve_completion( 
  data = tas_7_0_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.65, int = int )

## Mujeres -----------------------------------------------------------------------------------------
message( '\t(7 -> 0) Suavizamiento tasa de terminación de mujeres viudas' )
factor <- exp( 0.1 )
tas_7_0_m <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx, Nx_sal, Nx ) %>%
  dplyr::filter( sexo == 'M' & tipo %in% c( 'VIUDEDAD' ) & x >= 18 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( pmax( Nx_sal, Nx ), na.rm = TRUE ), 
                    ERx = sum( ERx, na.rm = TRUE ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::mutate( ux = ifelse( is.infinite( ux ), -log( 1e-9 ), ux ) ) %>%
  dplyr::filter( ux != 0 )

setDT( tas_7_0_m )


# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_7_0_m, 
  boundary_knots = c( 18, 105 ), 
  knots = list( c( 25, 55, 70 ),
                c( 25, 55, 70, 85 ) ) 
)

# Estimación de los modelos
mod_tas_7_0_m <- model_selection( list_models )
# print( mod_tas_7_0_m_m18$summary )

# Tabla biométrica
i <- min( which( mod_tas_7_0_m$summary$rellik_BIC == 1 ) )
print( mod_tas_7_0_m$summary[ i ] )
tas_7_0_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 18:105 ), 
  model = list_models[[ i ]],
  result = mod_tas_7_0_m$results[[ i ]] )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 105
int <- c( -5, 2 )
tas_7_0_m <- curve_completion(
  data = tas_7_0_m, range,
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL,
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.65, int = int )

# (8 -> 0) Tasas de terminación de orfandad --------------------------------------------------------
factor <- exp( -0.1 )
## Hombres -----------------------------------------------------------------------------------------
message( '\t(8 -> 0) Suavizamiento tasa de terminación de hombres huérfanos' )
tas_8_0_h <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx, Nx_sal, Nx ) %>%
  dplyr::filter( sexo == 'H' & tipo %in% c( 'ORFANDAD' ) & x >= 0 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( pmax( Nx_sal, Nx ), na.rm = TRUE ), 
                    ERx = sum( ERx, na.rm = TRUE ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::mutate( ux = ifelse( is.infinite( ux ), -log( 1e-9 ), ux ) ) %>%
  dplyr::filter( ux != 0 )

setDT( tas_8_0_h )
tas_8_0_h_m17 <- tas_8_0_h[ x <= 17 ]
tas_8_0_h_M17 <- tas_8_0_h[ x > 17 ]

# Menores a 17: Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_8_0_h_m17, 
  boundary_knots = c( 0, 17 ), 
  knots = list( c( 0, 2, 5, 17 ),
                c( 0, 2, 17, 17 ) ) 
)

# Estimación de los modelos
mod_tas_8_0_h_m17 <- model_selection( list_models )
# print( mod_tas_8_0_h_m17$summary )

# Tabla biométrica
i <- min( which( mod_tas_8_0_h_m17$summary$rellik_BIC == 1 ) )
print( mod_tas_8_0_h_m17$summary[ i ] )
tas_8_0_h_m17 <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 0:17 ), 
  model = list_models[[ i ]],
  result = mod_tas_8_0_h_m17$results[[ i ]] )

# Mayores a 17: Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_8_0_h_M17, 
  boundary_knots = c( 18, 105 ), 
  knots = list( c( 19, 25, 70 ),
                c( 19, 25, 70, 85 ) ) 
)

# Estimación de los modelos
mod_tas_8_0_h_M17 <- model_selection( list_models )
# print( mod_tas_8_0_h_M17$summary )

# Tabla biométrica
i <- min( which( mod_tas_8_0_h_M17$summary$rellik_BIC == 1 ) )
print( mod_tas_8_0_h_M17$summary[ i ] )
tas_8_0_h_M17 <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 18:105 ), 
  model = list_models[[ i ]],
  result = mod_tas_8_0_h_M17$results[[ i ]] )

tas_8_0_h <- rbindlist( list( tas_8_0_h_m17, tas_8_0_h_M17 ) )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 105
int <- c( -5, 3 )
tas_8_0_h <- curve_completion( 
  data = tas_8_0_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 0.5, int = int )

# extrapolación inferior
range <- c( 0, 17 )
x0 <- range
dx0 <- range
inflex_x <- 16
int <- c( -5, 0 )
aux <- curve_completion( 
  data = tas_8_0_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -4) ), dymax = -0.25, 
  int = int, right = FALSE, mono_chk = FALSE )

tas_8_0_h <- rbindlist( list( aux[ x <= range[2] ], tas_8_0_h[ x > range[2] ] ) )


## Mujeres -----------------------------------------------------------------------------------------
message( '\t(8 -> 0) Suavizamiento tasa de terminación de mujeres huérfanas' )
tas_8_0_m <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx, Nx_sal, Nx ) %>%
  dplyr::filter( sexo == 'M' & tipo %in% c( 'ORFANDAD' ) & x >= 0 & x <= 105 ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( pmax( Nx_sal, Nx ), na.rm = TRUE ), 
                    ERx = sum( ERx, na.rm = TRUE ) ) %>%
  dplyr::mutate( ux = factor * ifelse( ERx > 0, Nx / ERx, 0 ), 
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::mutate( ux = ifelse( is.infinite( ux ), -log( 1e-9 ), ux ) ) %>%
  dplyr::filter( ux != 0 )

setDT( tas_8_0_m )
tas_8_0_m_m17 <- tas_8_0_m[ x <= 17 ]
tas_8_0_m_M17 <- tas_8_0_m[ x > 17 ]

# Menores a 17: Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_8_0_m_m17, 
  boundary_knots = c( 0, 17 ), 
  knots = list( c( 0, 2, 5, 17 ),
                c( 0, 2, 17, 17 ) ) 
)

# Estimación de los modelos
mod_tas_8_0_m_m17 <- model_selection( list_models )
# print( mod_tas_8_0_m_m17$summary )

# Tabla biométrica
i <- min( which( mod_tas_8_0_m_m17$summary$rellik_BIC == 1 ) )
print( mod_tas_8_0_m_m17$summary[ i ] )
tas_8_0_m_m17 <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 0:17 ), 
  model = list_models[[ i ]],
  result = mod_tas_8_0_m_m17$results[[ i ]] )

# Mayores a 17: Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_8_0_m_M17, 
  boundary_knots = c( 18, 105 ), 
  knots = list( c( 24, 60, 75 ),
                c( 24, 60, 75, 85 ) ) 
)

# Estimación de los modelos
mod_tas_8_0_m_M17 <- model_selection( list_models )
# print( mod_tas_8_0_m_M17$summary )

# Tabla biométrica
i <- min( which( mod_tas_8_0_m_M17$summary$rellik_BIC == 1 ) )
print( mod_tas_8_0_m_M17$summary[ i ] )
tas_8_0_m_M17 <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 18:105 ), 
  model = list_models[[ i ]],
  result = mod_tas_8_0_m_M17$results[[ i ]] )

tas_8_0_m <- rbindlist( list( tas_8_0_m_m17, tas_8_0_m_M17 ) )

# extrapolación superior 
range <- c( 90, 105 )
x0 <- range
dx0 <- range
inflex_x <- 91
int <- c( -10, 2 )
tas_8_0_m <- curve_completion( 
  data = tas_8_0_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = 1e-9, dymax = 1, int = int )

# extrapolación inferior
range <- c( 0, 17 )
x0 <- range
dx0 <- range
inflex_x <- 17
int <- c( -7, 0 )
aux <- curve_completion( 
  data = tas_8_0_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -2.25 ) ), dymax = -0.75, 
  int = int, right = FALSE, mono_chk = FALSE )

tas_8_0_m <- rbindlist( list( aux[ x <= range[2] ], tas_8_0_m[ x > range[2] ] ) )

# Proporción de TNRH en la población activa --------------------------------------------------------
# Hombres ------------------------------------------------------------------------------------------
message( '\t Suavizamiento tasa de TNRH para hombres' )

tas_tnrh_h <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, ERx_tnrh_act, Nx_prim_ing ) %>%
  dplyr::filter( sexo == 'H' & x >= 18 & x <= 105 ) %>%
  dplyr::mutate( x = case_when( x < 100 ~ x, x >= 100 ~ 100 ) ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( Nx_prim_ing, na.rm = TRUE ), 
                    ERx_act = sum( ERx_act, na.rm = TRUE ), 
                    ERx_tnrh_act = sum( ERx_tnrh_act, na.rm = TRUE ) ) %>%
  dplyr::mutate( ERx = ifelse( is.na( ERx_act ), 0, ERx_act ), 
                 ERx_tnrh_act = ifelse( is.na( ERx_tnrh_act ), 0, ERx_tnrh_act ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, -log( 1 - pmin( ERx_tnrh_act / ERx, 1 ) ), 0 ),
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 & is.finite( ux ) )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_tnrh_h, 
  boundary_knots = c( 18, x_max ), 
  knots = list( c( 20, 30, 60, 62, 75, 80 ), 
                c( 20, 30, 60, 62, 75, 82, 84, 86, 88 ) ) 
)

# Estimación de los modelos
mod_tas_tnrh_h <- model_selection( list_models )
# print( mod_tas_tnrh_h$summary )

# Tabla biométrica
i <- min( which( mod_tas_tnrh_h$summary$rellik_BIC == 1 ) )
print( mod_tas_tnrh_h$summary[ i ] )
tas_tnrh_h <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'H', levels = c( 'H', 'M', 'HM' ) ), x = 18:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_tnrh_h$results[[ i ]] )

# extrapolación superior 
range <- c( 80, 105 )
x0 <- range
dx0 <- range
inflex_x <- 80
int <- c( -5, 10 )
tas_tnrh_h <- curve_completion( 
  data = tas_tnrh_h, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -7.5 ) ), dymax = 0.001, int = int, mono_chk = FALSE )

# Mujeres ------------------------------------------------------------------------------------------
message( '\t Suavizamiento tasa de TNRH para mujeres' )

tas_tnrh_m <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx_act, ERx_tnrh_act, Nx_prim_ing ) %>%
  dplyr::filter( sexo == 'M' & x >= 18 & x <= 105 ) %>%
  dplyr::mutate( x = case_when( x < 100 ~ x, x >= 100 ~ 100 ) ) %>%
  dplyr::group_by( x ) %>% 
  dplyr::summarise( Nx = sum( Nx_prim_ing, na.rm = TRUE ), 
                    ERx_act = sum( ERx_act, na.rm = TRUE ), 
                    ERx_tnrh_act = sum( ERx_tnrh_act, na.rm = TRUE ) ) %>%
  dplyr::mutate( ERx = ifelse( is.na( ERx_act ), 0, ERx_act ), 
                 ERx_tnrh_act = ifelse( is.na( ERx_tnrh_act ), 0, ERx_tnrh_act ) ) %>%
  dplyr::mutate( ux = ifelse( ERx > 0, -log( 1 - pmin( ERx_tnrh_act / ERx, 1 ) ), 0 ),
                 xf = factor( x, levels = x_lst, ordered = TRUE ) ) %>%
  dplyr::filter( ux != 0 & is.finite( ux ) )

# Especificación de los modelos a probar
list_models <- model_definition( 
  data = tas_tnrh_m, 
  boundary_knots = c( 18, x_max ), 
  knots = list( c( 20, 50, 60, 75, 80 ),
                c( 20, 50, 60, 65, 75, 80, 85 ) ) 
)

# Estimación de los modelos
mod_tas_tnrh_m <- model_selection( list_models )
# print( mod_tas_tnrh_m$summary )

# Tabla biométrica
i <- min( which( mod_tas_tnrh_m$summary$rellik_BIC == 1 ) )
print( mod_tas_tnrh_m$summary[ i ] )
tas_tnrh_m <- biometric_table( 
  data = data.table( id = '2012-2020', sexo = factor( 'M', levels = c( 'H', 'M', 'HM' ) ), x = 18:x_max ), 
  model = list_models[[ i ]],
  result = mod_tas_tnrh_m$results[[ i ]] )

# extrapolación superior 
range <- c( 80, 105 )
x0 <- range
dx0 <- range
inflex_x <- 80
int <- c( -5, 10 )
tas_tnrh_m <- curve_completion( 
  data = tas_tnrh_m, range, 
  x0 = x0, dx0 = dx0, y0 = NULL, dy0 = NULL, 
  inflex_x = inflex_x, ymax = exp( -exp( -6 ) ), dymax = 0.001, int = int, mono_chk = FALSE )

# Unión de resultados ------------------------------------------------------------------------------
tas_1_2 <- rbindlist( list( tas_1_2_h, tas_1_2_m, tas_1_2_hm ) )
tas_2_3 <- rbindlist( list( tas_2_3_h, tas_2_3_m, tas_2_3_hm ) )
tas_2_4 <- rbindlist( list( tas_2_4_h, tas_2_4_m, tas_2_4_hm ) )
tas_2_5 <- rbindlist( list( tas_2_5_h, tas_2_5_m, tas_2_5_hm ) )
tas_2_6 <- rbindlist( list( tas_2_6_h, tas_2_6_m, tas_2_6_hm ) )
tas_3_2 <- rbindlist( list( tas_3_2_h, tas_3_2_m, tas_3_2_hm ) )
tas_4_6 <- rbindlist( list( tas_4_6_h, tas_4_6_m, tas_4_6_hm ) )
tas_5_6 <- rbindlist( list( tas_5_6_h, tas_5_6_m, tas_5_6_hm ) )
tas_7_6 <- rbindlist( list( tas_7_6_h, tas_7_6_m, tas_7_6_hm ) )
tas_8_6 <- rbindlist( list( tas_8_6_h, tas_8_6_m, tas_8_6_hm ) )
tas_7_0 <- rbindlist( list( tas_7_0_h, tas_7_0_m ) )
tas_8_0 <- rbindlist( list( tas_8_0_h, tas_8_0_m ) )
tas_tnrh <- rbindlist( list( tas_tnrh_h, tas_tnrh_m ) )

# Guarda resultados --------------------------------------------------------------------------------
message( '\tGuardando suavizamiento de tasas' )
save( tas_1_2, 
      tas_2_3,
      tas_2_4,
      tas_2_5,
      tas_2_6, # Hacer latex
      tas_3_2,
      tas_4_6, # Hacer latex
      tas_5_6, # Hacer latex
      tas_7_6, # Hacer latex
      tas_8_6, # Hacer latex
      tas_7_0,
      tas_8_0,
      tas_tnrh,
      file = parametros$demo_rdata_sgo_tasas_tran )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )
