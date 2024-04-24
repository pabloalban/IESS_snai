message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGráficos de las tasas de transición para activos y pensionistas' )

# Carga template de gráficos -----------------------------------------------------------------------
source( paste0( parametros$work_dir, "R/401_graf_plantilla.R" ), encoding = 'UTF-8', echo = FALSE )
graf_width <- 18
graf_height <- 12

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$demo_rdata_ssc_tasas_tran )
load( file = parametros$demo_rdata_ssc_tran_prep )

# (1 -> 2) Tasa de ingreso desde la PEA ------------------------------------------------------------
# Límites
xlim <- c( 15, 90 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 1 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_1_2 <- ggplot() +
  geom_point( data = tas_1_2[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_1_2[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_1_2[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_1_2[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Tasa de ingreso desde la PEAr a activo',
        subtitle = 'Logaritmo de la tasa de trasición desde la PEA a activo',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{1,2}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_1_2,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_1_2', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (2 -> 3) Salidas de activos ----------------------------------------------------------------------
# Límites
xlim <- c( 15, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 3 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_2_3 <- ggplot() +
  geom_point( data = tas_2_3[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_2_3[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_2_3[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_2_3[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Tasa de salida de activos',
        subtitle = 'Logaritmo de la tasa de trasición de activo a inactivo',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{2,3}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_2_3,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_2_3', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (2 -> 4) Tasa de retiro por vejez ----------------------------------------------------------------
# Límites
xlim <- c( 50, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 3 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

# Hombres y mujeres comparadas
plt_ux_2_4 <- ggplot() +
  geom_point( data = tas_2_4[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_2_4[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_2_4[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_2_4[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Tasa de salida de activo a pensionista por vejez', x = TeX( '$x$' ), 
        subtitle = 'Logaritmo de la tasa de trasición de activo a pensionista por vejez',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{2,4}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_2_4,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_2_4', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (2 -> 5) Tasa de retiro por invalidez ------------------------------------------------------------
# Límites
xlim <- c( 15, 85 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -16, -2 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_2_5 <- ggplot() +
  geom_point( data = tas_2_5[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_2_5[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_2_5[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_2_5[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Tasa de salida de activo a pensionista por invalidez',
        subtitle = 'Logaritmo de la tasa de trasición de activo a pensionista por invalidez',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{2,5}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_2_5,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_2_5', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (2 -> 6) Mortalidad activos e inactivos ----------------------------------------------------------
# ux
# Límites
xlim <- c( 15, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -15, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_act_ux_2_6 <- ggplot() +
  geom_point( data = tas_2_6[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_2_6[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_2_6[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_2_6[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Mortalidad de activos',
        subtitle = 'Logaritmo de la fuerza de mortalidad de activos',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{2,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_act_ux_2_6,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_2_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave( plot = plt_act_ux_2_6,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_3_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# qx
# Límites
xlim <- c( 15, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], 0.1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_qx_2_6 <- ggplot() +
  geom_point( data = tas_2_6[ sexo == 'H' ], aes( x = x, y = ( 1 - exp( -ux_obs ) ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_2_6[ sexo == 'H' ], aes( x = x, y = qx, colour = 'a' ), linewidth = 0.75) +
  geom_point( data = tas_2_6[ sexo == 'M' ], aes( x = x, y = ( 1 - exp( -ux_obs ) ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_2_6[ sexo == 'M' ], aes( x = x, y = qx, colour = 'c' ), linewidth = 0.75) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Mortalidad de activos',
        subtitle = 'Probabilidad de muerte de activos',
        x = TeX( '$x$' ),
        y = TeX( '$q^{2,6}_{i,t,x}$' ) ) +
  plt_theme_legend

ggsave( plot = plt_qx_2_6,
        filename = paste0( parametros$resultado_graficos, 'iess_qx_2_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

ggsave( plot = plt_qx_2_6,
        filename = paste0( parametros$resultado_graficos, 'iess_qx_3_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (3 -> 2) Ingresos de inactivos -------------------------------------------------------------------
# Límites
xlim <- c( 15, 90 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 0 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_3_2 <- ggplot() +
  geom_point( data = tas_3_2[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_3_2[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_3_2[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_3_2[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Tasa de ingreso desde inactivo a activo',
        subtitle = 'Logaritmo de la tasa de trasición de inactivo a activo',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{3,2}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_3_2,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_3_2', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (4 -> 6) Mortalidad vejez ------------------------------------------------------------------------
# ux
# Límites
xlim <- c( 60, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_4_6 <- ggplot() +
  geom_point( data = tas_4_6[ sexo == 'H' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_4_6[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_4_6[ sexo == 'M' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_4_6[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Mortalidad de pensionistas por vejez',
        subtitle = 'Logaritmo de la fuerza de mortalidad de pensionistas por vejez',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_4_6,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_4_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# qx
# Límites
xlim <- c( 50, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], 0.1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_qx_4_6 <- ggplot() +
  geom_point( data = tas_4_6[ sexo == 'H' & x <= 95 ], aes( x = x, y = ( 1 - exp( -ux_obs ) ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_4_6[ sexo == 'H' ], aes( x = x, y = qx, colour = 'a' ), linewidth = 0.75) +
  geom_point( data = tas_4_6[ sexo == 'M' & x <= 95 ], aes( x = x, y = ( 1 - exp( -ux_obs ) ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_4_6[ sexo == 'M' ], aes( x = x, y = qx, colour = 'c' ), linewidth = 0.75) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Mortalidad de pensionistas por vejez', x = TeX( '$x$' ), 
        subtitle = 'Probabilidad de muerte de pensionistas por vejez',
        y = TeX( '$q^{2,6}_{i,t,x}$' ) ) +
  plt_theme_legend

ggsave( plot = plt_qx_4_6,
        filename = paste0( parametros$resultado_graficos, 'iess_qx_4_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (5 -> 6) Mortalidad invalidez --------------------------------------------------------------------
#ux
# Límites
xlim <- c( 20, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_5_6 <- ggplot() +
  geom_point( data = tas_5_6[ sexo == 'H' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_5_6[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_5_6[ sexo == 'M' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_5_6[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Mortalidad de pensionistas de invalidez',
        subtitle = 'Logaritmo de la fuerza de mortalidad',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{5,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_5_6,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_5_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# qx
# Límites
xlim <- c( 50, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], 0.1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_qx_5_6 <- ggplot() +
  geom_point( data = tas_5_6[ sexo == 'H' & x <= 95 ], aes( x = x, y = ( 1 - exp( -ux_obs ) ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_5_6[ sexo == 'H' ], aes( x = x, y = qx, colour = 'a' ), linewidth = 0.75) +
  geom_point( data = tas_5_6[ sexo == 'M' & x <= 95 ], aes( x = x, y = ( 1 - exp( -ux_obs ) ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_5_6[ sexo == 'M' ], aes( x = x, y = qx, colour = 'c' ), linewidth = 0.75) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='Mortalidad de pensionistas de vejez', x = TeX( '$x$' ), 
        subtitle = 'Probabilidad de muerte de pensionistas por invalidez',
        y = TeX( '$q^{2,6}_{i,t,x}$' ) ) +
  plt_theme_legend

ggsave( plot = plt_qx_5_6,
        filename = paste0( parametros$resultado_graficos, 'iess_qx_5_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# (7 -> 6) Mortalidad viudedad ---------------------------------------------------------------------
# Límites
xlim <- c( 20, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_7_6 <- ggplot() +
  geom_point( data = tas_7_6[ sexo == 'H' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_7_6[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_7_6[ sexo == 'M' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_7_6[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Mortalidad de pensionistas de viudedad',
        subtitle = 'Logaritmo de la fuerza de mortalidad',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{7,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_7_6,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_7_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (8 -> 6) Mortalidad orfandad ---------------------------------------------------------------------
# Límites
xlim <- c( 0, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt_ux_8_6 <- ggplot() +
  geom_point( data = tas_8_6[ sexo == 'H' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_8_6[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_8_6[ sexo == 'M' & x <= 95 ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_8_6[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Mortalidad de pensionistas de orfandad',
        subtitle = 'Logaritmo de la fuerza de mortalidad',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu^{8,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plt_ux_8_6,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_8_6', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (7 -> 0) Tasa de salida de viudas --------------------------------------------------------------
# Límites
xlim <- c( 15, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plot_ux_7_0 <- ggplot() +
  geom_point( data = tas_7_0[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_7_0[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_line( data = tas_7_0[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_7_0[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_7_0[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Terminación del estado de montepío por viudedad',
        subtitle = 'Logaritmo de la tasa de terminación de montepío',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plot_ux_7_0,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_sal_mont', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# (8 -> 0) Tasa de salida de orfandad --------------------------------------------------------------
# Límites
xlim <- c( 0, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plot_ux_8_0 <- ggplot() +
  geom_point( data = tas_8_0[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.0 ) +
  geom_line( data = tas_8_0[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_point( data = tas_8_0[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'd' ), size = 1.0 ) +
  geom_line( data = tas_8_0[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title = 'Terminación del estado de montepío por orfandad',
        subtitle = 'Logaritmo de la tasa de terminación de montepío',
        x = TeX( '$x$' ),
        y = TeX( '$\\log( \\mu_{i,t,x} )$' ) ) +
  plt_theme_legend

ggsave( plot = plot_ux_8_0,
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_sal_mont', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiando resultados -----------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )
