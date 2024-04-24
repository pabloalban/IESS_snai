message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPreparación de población inicial' )

# Carga de información -----------------------------------------------------------------------------
load( parametros$demo_rdata_sgo_tran_prep )
load( parametros$demo_rdata_ssc_tran_prep )
load( parametros$demo_rdata_inec_pea )

# Parámetros de preparación ------------------------------------------------------------------------
message( '\tEstableciendo parámetros de ejecución' )
# Edades
x_min_pea <- 15
x_min <- 0
x_max <- parametros$demo_edad_max
xlst <- seq( x_min, x_max, 1 )
nx <- length( xlst )

s_min <- 0
s_max <- parametros$demo_ts_max
slst <- seq( s_min, s_max, 1 )
ns <- length( slst )

# Proyección PEA -----------------------------------------------------------------------------------
message( '\tPreparando PEA' )
# Esta preparación de la PEA ( Población económicamente activa ) está sustentada en las siguientes
# hipótesis.
# 1. La PEA contiene a la población con empleo formal es decir, la población afiliada al IESS.
# 2. La población afiliada, es una muestra representativa de la PEA total del país.
PEA <- sgo_act_tran_anio[ x >= x_min_pea & x <= x_max, 
                          list( anio, sexo, x, peax = pmax( ERx - Nx_dec, 0 ) ) ]
PEA[ , xf := factor( x, levels = seq( x_min_pea, x_max, 1 ) ) ]
PEA[ , t := anio - min( anio ) ]
PEA[ , tf := factor( t, levels = seq( min( PEA$t ), max( PEA$t ) + parametros$demo_horizonte, 1 ) ) ]

setorder( PEA, sexo, x, anio )
PEA[ , peaxs := shift( peax, fill = NA ), by = list( sexo, x ) ]
PEA[ is.na( peaxs ), peaxs := peax ]
PEA[ peax > 0 & peaxs > 0, rate := log( peax / peaxs ) ]
PEA[ peax > 0 & peaxs == 0, rate := log( 1e6 ) ]
PEA[ peax == 0 & peaxs > 0, rate := log( 1e-6 ) ]
PEA[ peax == 0 & peaxs == 0, rate := log( 1 ) ]
PEA[ , w := ( t + 1 ) * peax ]
PEA[ , w := peax / sum( peax ) ]

x_len <- length( unique( PEA$x ) )

PEA_mod <- lm( data = PEA,
               formula = rate ~ 0 + t:x,
               weights = w )

# print( summary( PEA_mod ) )

## Proyección de la PEA para años futuros ----------------------------------------------------------
aux <- PEA_inec[ , list( anio, pea_tot = pea ) ]
aux <- merge.data.table( PEA[ , list( peax = sum( peax, na.rm = TRUE ) ), by = list( anio ) ], aux, by = c( 'anio' ) )
aux[ , cal := pea_tot / peax ]
PEA <- merge.data.table( PEA, aux[ , list( anio, cal ) ], by = c( 'anio' ), all.x = TRUE )
PEA[ , peax := cal * peax ]

a_max <- max( PEA$anio )
# t_ini <- max( PEA$t ) + 1
t_ini <- 1
# t_max <- max( PEA$t ) + parametros$demo_horizonte
t_max <- parametros$demo_horizonte + 1
PEA_proy <- data.table( expand.grid( 
  t = seq( t_ini, t_max, 1 ), 
  sexo = factor( c( 'H', 'M' ), levels = c( 'H', 'M', 'HM' ) ), 
  x = seq( x_min_pea, x_max, 1 ) ) )
PEA_proy[ , xf := factor( x, levels = x_min_pea:x_max ) ]
PEA_proy[ , tf := factor( t, levels = seq( min( PEA$t ), max( PEA$t ) + parametros$demo_horizonte, 1 ) ) ]
# PEA_proy[ , anio := a_max + t - max( PEA$t ) ]
PEA_proy[ , anio := a_max + t ]

PEA_proy <- merge.data.table( 
  PEA_proy, 
  PEA[ anio == a_max, list( sexo, x, peax ) ], 
  by = c( 'sexo', 'x' ), all.x = TRUE )
PEA_proy[ is.na( peax ), peax := 0 ]

dat <- data.table( x = 15:105, rate = predict( object = PEA_mod, newdata = data.table( t = 1, x = 15:105 ) )  )
PEA_proy <- merge.data.table( PEA_proy, dat, by = 'x', all.x = TRUE )
setorder( PEA_proy, sexo, x, anio )
PEA_proy[ , rate := exp( rate ) ]
PEA_proy[ , rate := cumprod( rate ), by = list( sexo, x ) ]
PEA_proy[ , peax := rate * peax ]

PEA_proy <- rbind( PEA[ , list( anio, sexo, x, peax, rate = 1 ) ], 
                   PEA_proy[ , list( anio, sexo, x, peax, rate ) ] )

## Preparación de la proporción de la PEA rural ----------------------------------------------------
# Supuesto que PEAr / PEA = SSC / ( SGO + SSC )
PEAr <- merge.data.table( 
  sgo_act_tran_anio[ x >= x_min_pea & x <= x_max, list( anio, sexo, x, ERx ) ],
  ssc_act_tran_anio[ x >= x_min_pea & x <= x_max, list( anio, sexo, x, ERxr = ERx ) ], 
  by = c( 'anio', 'sexo', 'x' ),
  all.x = TRUE )
PEAr[ , pr := 0 ]
PEAr[ ERx + ERxr > 0, pr := ERxr / ( ERx + ERxr ) ]

# Verificación comportamiento de la proporción
# summary( aux$pr )
# quantile( aux$pr, probs = seq( 0, 1, 0.05 ) )
# plot( sort( aux$pr ) )

PEAr <- merge.data.table( 
  PEA_proy[ anio <= max( PEA$anio ), list( anio, sexo, x, peax ) ],
  PEAr,
  by = c( 'anio', 'sexo', 'x' ),
  all.x = TRUE )
PEAr[ , peax_r := peax * pr ]

# Si no se utilizan para estimar, no es necesario definirlas
PEAr[ , xf := factor( x, levels = seq( x_min_pea, x_max, 1 ) ) ]
PEAr[ , t := anio - min( anio ) ]
# PEAr[ , tf := factor( t, levels = seq( min( PEA$t ), max( PEA$t ) + parametros$demo_horizonte, 1 ) ) ]

setorder( PEAr, sexo, x, anio )
PEAr[ , prs := shift( pr, fill = NA ), by = list( sexo, x ) ]
PEAr[ , rate_r := log( pr ) ]
# PEAr[ pr > 0 & prs > 0, rate_r := log( pr / prs ) ]
# PEAr[ pr > 0 & prs == 0, rate_r := log( 1e6 ) ]
# PEAr[ pr == 0 & prs > 0, rate_r := log( 1e-6 ) ]
# PEAr[ pr == 0 & prs == 0, rate_r := log( 1 ) ]
PEAr[ , w := ( t + 1 ) * pr ]
PEAr[ , w := pr / sum( pr, na.rm = T ) ]

x_len <- length( unique( PEAr$x ) )

PEAr_mod <- lm( data = PEAr[ is.finite( rate_r ) ],
                formula = rate_r ~ 0 + sexo:xf,
                weights = ERxr )
# print( summary( PEAr_mod ) )

PEAr_proy <- data.table( expand.grid( x = 15:105, sexo = factor( c( 'H', 'M' ) ) ) )
PEAr_proy[ , xf := factor( x, levels = seq( x_min_pea, x_max, 1 ) ) ]
PEAr_proy[ , rate_r := predict( object = PEAr_mod, newdata = PEAr_proy ) ]
PEAr_proy <- merge.data.table( 
  PEA_proy[ anio > max( PEA$anio ) ], 
  PEAr_proy, 
  by = c( 'sexo', 'x' ), 
  all.x = TRUE )
setorder( PEAr_proy, sexo, x, anio )
PEAr_proy[ , rate_r := exp( rate_r ) ]
PEAr_proy[ anio <= max( PEA$anio ), rate_r := 1 ]
PEAr_proy[ , peax_r := rate_r * peax ]

# summary( PEAr_proy$rate_r )

PEAr_proy <- rbind( PEAr[ , list( anio, sexo, x, peax_r, rate_r = 1 ) ], 
                    PEAr_proy[ , list( anio, sexo, x, peax_r, rate_r ) ] )

PEA_proy  <- merge.data.table( 
  PEA_proy, 
  PEAr_proy, 
  by = c( 'anio', 'sexo', 'x' ), 
  all.x = TRUE )

PEA_proy[ is.na( peax_r ), peax_r := 0 ]
PEA_proy[ is.na( rate_r ), rate_r := 0 ]

## Gráficos PEA y PEA rural ------------------------------------------------------------------------
# source( paste0( parametros$work_dir, "R/401_graf_plantilla.R" ), encoding = 'UTF-8', echo = FALSE )
# 
# dat_tot <- PEA_proy[ , list( peax_r = sum( peax_r, na.rm = TRUE ) ,
#                              peax = sum( peax, na.rm = TRUE ) ), by = list( anio ) ]
# dat <- PEA_proy[ , list( peax_r = sum( peax_r, na.rm = TRUE ) ), by = list( anio, x ) ]
# 
# num_anios <- length( unique( dat$anio ) )
# cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
# cols_graf <- cols_fun( num_anios )


# x_lim <- c( 0, 105 )
# x_brk <- seq( x_lim[1], x_lim[2], 10 )
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( 0, 2e5 )
# y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# plt_pea_r_proy <- ggplot() +
#   geom_line( data = dat, aes( x = x, y = peax_r, group = anio, colour = anio ), linewidth = graf_line_size ) +
#   scale_colour_gradientn( colours = cols_graf ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   xlab( TeX("edad $x$") ) +
#   ylab( TeX("$pea_t$") ) +
#   theme_bw() +
#   plt_theme
# 
# plt_pea_r_proy

# dat <- PEA_proy[ , list( pr = mean( peax_r / peax, na.rm = TRUE ) ), by = list( anio, x ) ]
# 
# num_anios <- length( unique( dat$anio ) )
# cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
# cols_graf <- cols_fun( num_anios )
# 
# x_lim <- c( 0, 105 )
# x_brk <- seq( x_lim[1], x_lim[2], 10 )
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( 0, 1 )
# y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
# y_lbl <- formatC( y_brk, digits = 4, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# plt_pea_amb_proy <- ggplot() +
#   geom_line( data = dat, aes( x = x, y = pr, group = anio, colour = anio ), linewidth = graf_line_size ) +
#   scale_colour_gradientn( colours = cols_graf ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   xlab( TeX("edad $x$") ) +
#   ylab( TeX("$pea_t$") ) +
#   theme_bw() +
#   plt_theme
# 
# plt_pea_amb_proy

# dat_tot <- PEA_proy[ , list( peax = sum( peax, na.rm = TRUE ) ), by = list( anio ) ]
# dat <- PEA_proy[ , list( peax = sum( peax, na.rm = TRUE ) ), by = list( anio, x ) ]
# 
# source( paste0( parametros$work_dir, "R/401_graf_plantilla.R" ), encoding = 'UTF-8', echo = FALSE )
# 
# num_anios <- length( unique( dat$anio ) )
# cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
# cols_graf <- cols_fun( num_anios )
# 
# x_lim <- c( 0, 105 )
# x_brk <- seq( x_lim[1], x_lim[2], 10 )
# x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
# 
# y_lim <- c( 0, 0.8e6 )
# y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
# y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
# 
# plt_pea_proy <- ggplot() +
#   geom_line( data = dat, aes( x = x, y = peax, group = anio, colour = anio ), linewidth = graf_line_size ) +
#   scale_colour_gradientn( colours = cols_graf ) +
#   scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
#   scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
#   xlab( TeX("edad $x$") ) +
#   ylab( TeX("$pea_t$") ) +
#   theme_bw() +
#   plt_theme
# 
# plt_pea_proy

# Guardando PEA ------------------------------------------------------------------------------------
message( '\tGuardando PEA, PEA rural y población inicial' )
save( PEA_proy, file = parametros$demo_rdata_sgo_pea_proj )

# Para el SGO --------------------------------------------------------------------------------------
message( '\tPreparando población inicial para SGO' )

## Población por inicial por tiempo de servicio ----------------------------------------------------
load( paste0( parametros$demo_rdata_sgo_incom_tran_act_anio, parametros$anio_ini, '.RData' ) )
l0xs <- copy( sgo_comp_tran )
rm( sgo_comp_tran )

l0xs[ , s := round( imp, 0 ) ]
l0xs[ is.na( s ), s := 0 ]

l0xs <- l0xs[ , list( 
  l2 = sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, 1, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
  l3 = sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, 1, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ), 
  by = list( x, s, sexo ) ]

aux <- data.table( expand.grid( x = xlst, s = slst, sexo = factor( c( 'H', 'M' ), levels = c( 'H', 'M', 'HM' ) ) ) )
l0xs <- merge.data.table(
  l0xs,
  aux,
  by = c( 'sexo', 'x', 's' ),
  all.y = TRUE )

aux <- PEA_proy[ anio == parametros$anio_ini, list( sexo, x, s = 0, l1 = peax ) ]
l0xs <- merge.data.table( l0xs, aux, by = c( 'sexo', 'x', 's' ), all.x = TRUE )
l0xs[ is.na( l1 ), l1 := 0 ]
l0xs[ is.na( l2 ), l2 := 0 ]
l0xs[ is.na( l3 ), l3 := 0 ]

l0xs[ , sct := mapply( FUN = parametros$elig_vej, s + 4, x + 4 ) ]
l0xs[ sct == 1, l2 := l2 + 0.6 * l3 ]
l0xs[ sct == 1, l3 := 0.4 * l3 ]

l0xs[ , l2s := sum( l2 ), by = list( sexo, x ) ]
l0xs[ s > 0, l2s := 0 ]
l0xs[ , l3s := sum( l3 ), by = list( sexo, x ) ]
l0xs[ s > 0, l3s := 0 ]
l0xs[ s > 0, l1 := 0 ]
l0xs[ , l1 := pmax( l1 - l2s - l3s, 0 ) ]
l0xs[ , l2s := NULL ]
l0xs[ , l3s := NULL ]

# sum( l0xs$l1 ) - ( sum( l0xs$l2 ) + sum( l0xs$l3 ) )
# sum( l0xs$l1 + l0xs$l2 + l0xs$l3 )
# tst <- l0xs[ , list( l1 = sum( l1 ), l2 = sum( l2 ), l3 = sum( l3 ) ), by = list( x ) ]
# sum( tst$l1 ) + sum( tst$l2 ) + sum( tst$l3 )
# sum( aux$l1 )

## Procesando información de activos e cesantes/inactivos ------------------------------------------
message( '\tPreparación de población inicial de activos y cesantes/inactivos' )

l0 <- expand.grid( sexo = factor( c( 'H', 'M' ), levels = c( 'H', 'M', 'HM' ) ), x = xlst )
l0 <- as.data.table( l0 )

lact <- sgo_act_tran_anio[ anio == parametros$anio_ini, list( sexo, x, l2 = l2x, l3 = l3x ) ]
l0 <- merge.data.table( l0, lact, by = c( 'sexo', 'x' ), all.x = TRUE )

aux <- PEA_proy[ anio == parametros$anio_ini, list( sexo, x, l1 = peax ) ]
l0 <- merge.data.table( l0, aux, by = c( 'sexo', 'x' ), all.x = TRUE )
l0[ , l1 := pmax( l1 - l2 - l3, 0 ) ]

## Procesando información de pensionistas ----------------------------------------------------------
message( '\tPreparación de población inicial de pensionistas' )
load( paste0( parametros$demo_rdata_sgo_incom_tran_pen_anio, parametros$anio_ini, '.RData' ) )

sgo_comp_tran[ , s := round( imp, 0 ) ]
sgo_comp_tran[ is.na( s ), s := 0 ]
sgo_comp_tran[ tipo == 'VEJEZ' & s < 10 & x >= 70, s := 10 ]
sgo_comp_tran[ tipo == 'VEJEZ' & s < 15 & x >= 65 & x < 70, s := 15 ]
sgo_comp_tran[ tipo == 'VEJEZ' & s < 30 & x >= 60 & x < 65, s := 30 ]
sgo_comp_tran[ tipo == 'VEJEZ' & s < 40 & x < 60, s := 30 ]
sgo_comp_tran[ tipo == 'VEJEZ' & s < 10, s := 10 ]
sgo_comp_tran[ s < 5 & ( tipo == 'DISCAPACIDAD' | tipo == 'INVALIDEZ' ), s := 5 ]

lpen_xs <- sgo_comp_tran[ , list( 
  lx = sum( ifelse( N_ing - N_sal >= 0 & ER_pen > 0, ER_pen, 0 ) * ( 1 - N ), na.rm = TRUE ) ), 
  by = list( tipo, x, s, sexo ) ]

lpen_xs[ tipo == 'VEJEZ', tip := 'l4' ]
lpen_xs[ tipo == 'DISCAPACIDAD' | tipo == 'INVALIDEZ', tip := 'l5' ]
lpen_xs[ tipo == 'VIUDEDAD', tip := 'l7' ]
lpen_xs[ tipo == 'ORFANDAD', tip := 'l8' ]
lpen_xs <- lpen_xs[ , list( lx = sum( lx, na.rm = TRUE ) ), by = list( tip, sexo, x, s ) ]
lpen_xs <- dcast.data.table( data = lpen_xs, formula = sexo + x + s ~ tip, value.var = 'lx', fill = 0 )

l0xs <- merge.data.table( 
  l0xs, lpen_xs[ , list( sexo, x, s, l4, l5, l7, l8 ) ], 
  by = c( 'sexo', 'x', 's' ), all.x = TRUE )
l0xs[ is.na( l1 ), l1 := 0 ]
l0xs[ is.na( l2 ), l2 := 0 ]
l0xs[ is.na( l3 ), l3 := 0 ]
l0xs[ is.na( l4 ), l4 := 0 ]
l0xs[ is.na( l5 ), l5 := 0 ]
l0xs[ is.na( l7 ), l7 := 0 ]
l0xs[ is.na( l8 ), l8 := 0 ]
setcolorder( l0xs, c( 'sexo', 'x', 's', 'l1', 'l2', 'l3', 'l4', 'l5', 'l7', 'l8' ) )

## Guardando población inicial SGO -----------------------------------------------------------------
message( '\tGuardando población inicial SGO' )
save( l0, l0xs, file = parametros$demo_rdata_sgo_pob_ini )

# Para el SSC --------------------------------------------------------------------------------------
message( '\tPreparando población inicial para SSC' )

## Población por inicial por tiempo de servicio ----------------------------------------------------
load( paste0( parametros$demo_rdata_ssc_incom_tran_act_anio, parametros$anio_ini, '.RData' ) )
l0xs <- copy( ssc_comp_tran )
rm( ssc_comp_tran )

l0xs[ , s := round( imp, 0 ) ]
l0xs[ is.na( s ), s := 0 ]
l0xs[ s >= s_max, s := s_max ]
l0xs[ x >= x_max, x := x_max ]

l0xs <- l0xs[ , list( 
  l2 = sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, 1, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv, 0 ), na.rm = TRUE ),
  l3 = sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, 1, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv, 0 ), na.rm = TRUE ) ), 
  by = list( x, s, sexo ) ]

aux <- data.table( expand.grid( x = xlst, s = slst, sexo = factor( c( 'H', 'M' ), levels = c( 'H', 'M', 'HM' ) ) ) )
l0xs <- merge.data.table(
  l0xs,
  aux,
  by = c( 'sexo', 'x', 's' ),
  all.y = TRUE )
rm( aux )

aux <- PEA_proy[ anio == parametros$anio_ini, list( sexo, x, s = 0, l1 = peax_r ) ]
l0xs <- merge.data.table( l0xs, aux, by = c( 'sexo', 'x', 's' ), all.x = TRUE )
l0xs[ is.na( l1 ), l1 := 0 ]
l0xs[ is.na( l2 ), l2 := 0 ]
l0xs[ is.na( l3 ), l3 := 0 ]

l0xs[ , l2s := sum( l2 ), by = list( sexo, x ) ]
l0xs[ s > 0, l2s := 0 ]
l0xs[ , l3s := sum( l3 ), by = list( sexo, x ) ]
l0xs[ s > 0, l3s := 0 ]
l0xs[ s > 0, l1 := 0 ]
l0xs[ , l1 := pmax( l1 - l2s - l3s, 0 ) ]
l0xs[ , l2s := NULL ]
l0xs[ , l3s := NULL ]

# sum( l0xs$l1 ) - ( sum( l0xs$l2 ) + sum( l0xs$l3 ) )
# sum( l0xs$l1 + l0xs$l2 + l0xs$l3 )
# tst <- l0xs[ , list( l1 = sum( l1 ), l2 = sum( l2 ), l3 = sum( l3 ) ), by = list( x ) ]
# sum( tst$l1 ) + sum( tst$l2 ) + sum( tst$l3 )
# sum( aux$l1 )

## Procesando información de activos e cesantes/inactivos ------------------------------------------
message( '\tPreparación de población inicial de activos y cesantes/inactivos' )

l0 <- expand.grid( sexo = factor( c( 'H', 'M' ), levels = c( 'H', 'M', 'HM' ) ), x = xlst )
l0 <- as.data.table( l0 )

lact <- sgo_act_tran_anio[ anio == parametros$anio_ini, list( sexo, x, l2 = l2x, l3 = l3x ) ]
l0 <- merge.data.table( l0, lact, by = c( 'sexo', 'x' ), all.x = TRUE )

aux <- PEA_proy[ anio == parametros$anio_ini, list( sexo, x, l1 = peax_r ) ]
l0 <- merge.data.table( l0, aux, by = c( 'sexo', 'x' ), all.x = TRUE )
l0[ , l1 := pmax( l1 - l2 - l3, 0 ) ]

## Procesando información de pensionistas ----------------------------------------------------------
message( '\tPreparación de población inicial de pensionistas' )
load( paste0( parametros$demo_rdata_ssc_incom_tran_pen_anio, parametros$anio_ini, '.RData' ) )

ssc_comp_tran[ , s := round( imp, 0 ) ]
ssc_comp_tran[ is.na( s ), s := 0 ]
ssc_comp_tran[ tipo == 'VEJEZ' & s < 5 & x >= 75, s := 5 ]
ssc_comp_tran[ tipo == 'VEJEZ' & s < 6 & x == 74 , s := 6 ]
ssc_comp_tran[ tipo == 'VEJEZ' & s < 7 & x == 73 , s := 7 ]
ssc_comp_tran[ tipo == 'VEJEZ' & s < 8 & x == 72 , s := 8 ]
ssc_comp_tran[ tipo == 'VEJEZ' & s < 9 & x == 71 , s := 9 ]
ssc_comp_tran[ tipo == 'VEJEZ' & s < 10 & x >= 65 & x <= 70, s := 10 ]
ssc_comp_tran[ tipo == 'VEJEZ' & s < 5, s := 5 ]
ssc_comp_tran[ s < 5 & ( tipo == 'DISCAPACIDAD' | tipo == 'INVALIDEZ' ), s := 5 ]

lpen_xs <- ssc_comp_tran[ , list( 
  lx = sum( ifelse( N_ing - N_sal >= 0 & ER_pen > 0, ER_pen, 0 ) * ( 1 - N ), na.rm = TRUE ) ),
  by = list( tipo, x, s, sexo ) ]

lpen_xs[ tipo == 'VEJEZ', tip := 'l4' ]
lpen_xs[ tipo == 'DISCAPACIDAD' | tipo == 'INVALIDEZ', tip := 'l5' ]
# lpen_xs[ tipo == 'VIUDEDAD', tip := 'l7' ]
# lpen_xs[ tipo == 'ORFANDAD', tip := 'l8' ]
lpen_xs <- lpen_xs[ , list( lx = sum( lx, na.rm = TRUE ) ), by = list( tip, sexo, x, s ) ]
lpen_xs <- dcast.data.table( data = lpen_xs, formula = sexo + x + s ~ tip, value.var = 'lx', fill = 0 )
lpen_xs[ , l7 := 0 ]
lpen_xs[ , l8 := 0 ]

l0xs <- merge.data.table( 
  l0xs, lpen_xs[ , list( sexo, x, s, l4, l5, l7, l8 ) ], 
  by = c( 'sexo', 'x', 's' ), all.x = TRUE )
l0xs[ is.na( l1 ), l1 := 0 ]
l0xs[ is.na( l2 ), l2 := 0 ]
l0xs[ is.na( l3 ), l3 := 0 ]
l0xs[ is.na( l4 ), l4 := 0 ]
l0xs[ is.na( l5 ), l5 := 0 ]
l0xs[ is.na( l7 ), l7 := 0 ]
l0xs[ is.na( l8 ), l8 := 0 ]
setcolorder( l0xs, c( 'sexo', 'x', 's', 'l1', 'l2', 'l3', 'l4', 'l5', 'l7', 'l8' ) )

## Guardando población inicial SSC -----------------------------------------------------------------
message( '\tGuardando población inicial SSC' )
save( l0, l0xs, file = parametros$demo_rdata_ssc_pob_ini )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()
