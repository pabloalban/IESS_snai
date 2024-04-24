message( paste( rep('-', 100 ), collapse = '' ) )

escenario <- 'escenario_1'
load( paste0( parametros$RData, 'IESS_proyeccion_salarios_', escenario, '.RData' ) )
load( paste0( parametros$RData, 'IESS_salarios_pensiones_iniciales_v3.RData' ) )

source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )


aux_f <- sal_proy[ sexo == 'F' ]
aux_f[ , t := factor( t, levels = 0:40, ordered = TRUE ) ]

num_anios <- parametros$demo_horizonte
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios + 1 )

plt_sal_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = sal, color = t ), linewidth = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  # scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  # scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

plt_sal_f

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
