message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_estimacion_tasa_entradas.RData' ) )

# Fuerza de entrada mujeres ------------------------------------------------------------------------
message( '\tGraficando tasa de entradas' )

x_lim <- c( 0, 110 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- tasa_ent_esp[ sexo == 'F' ]
plt_ent_f <- ggplot( data = aux_f ) + 
  geom_point( aes( x = x, y = ue_est, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = ue, color = parametros$iess_blue ), size = graf_line_size ) + 
  labs( x = 'edad x', y = 'mujeres ue' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_ent_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_ent_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Fuerza de entrada hombres ------------------------------------------------------------------------
x_lim <- c( 0, 110 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- tasa_ent_esp[ sexo == 'M' ]
plt_ent_m <- ggplot( data = aux_m ) + 
  geom_point( aes( x = x, y = ue_est, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = ue, color = parametros$iess_blue ), size = graf_line_size ) + 
  labs( x = 'edad x', y = 'hombres ue' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_ent_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_ent_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
