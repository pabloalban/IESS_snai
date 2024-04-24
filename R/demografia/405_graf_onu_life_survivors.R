message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019.RData' ) )

# Gráficos para verficar la interpolación ----------------------------------------------------------
message( '\tGráficos de interpolaciones para ambos sexos' )
x_lim <- c( 0, 110 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

num_anios <- parametros$demo_horizonte
t_min <- parametros$anio_ini
t_max <- t_min + num_anios
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios + 1 )

aux_ini <- onu_ecu_surv[ sexo == 'F' & t >= t_min & t <= t_max, list( t = as.character( t ), x, lx ) ]
aux <- onu_ecu_mort_din[ sexo == 'F' & t >= t_min & t <= t_max, list( t = as.character( t ), x, lx ) ]
plt_lx_f <- ggplot() +
  geom_line( data = aux, aes( x = x, y = lx, color = t ), size = graf_line_size ) +
  geom_point( data = aux_ini, aes( x = x, y = lx, color = t ), size = graf_point_size ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\; l_{x}$")) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_lx_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_onu_int_life_surv_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_ini <- onu_ecu_surv[ sexo == 'M' & t >= t_min & t <= t_max, list( t = as.character( t ), x, lx ) ]
aux <- onu_ecu_mort_din[ sexo == 'M' & t >= t_min & t <= t_max, list( t = as.character( t ), x, lx ) ]
plt_lx_m <- ggplot() +
  geom_line( data = aux, aes( x = x, y = lx, color = t ), size = graf_line_size ) +
  geom_point( data = aux_ini, aes( x = x, y = lx, color = t ), size = graf_point_size  ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\; l_{x}$")) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_lx_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_onu_int_life_surv_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
