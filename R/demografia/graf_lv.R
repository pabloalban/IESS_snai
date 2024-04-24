# Carga template de gráficos -----------------------------------------------------------------------
source( paste0( parametros$work_dir, "R/401_graf_plantilla.R" ), encoding = 'UTF-8', echo = FALSE )
graf_width <- 25
graf_height <- 17

# Límites
xlim <- c( 50, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 0 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt <- ggplot() +
  # geom_point( data = tas_act_hm_vej[ sexo == 'HM' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.5 ) +
  # geom_point( data = tas_act_m_vej[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.5 ) +
  # geom_point( data = tas_act_h_vej[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.5 ) +
  geom_line( data = tas_act_m_vej[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_line( data = tas_act_h_vej[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'b' ), linewidth = 0.75 ) +
  geom_line( data = tas_act_hm_vej[ sexo == 'HM' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='TASAS', x = TeX( '$x$' ), 
        subtitle = 'Logaritmo de la tasa instantánea',
        y = TeX( '$\\log( \\mu^{2,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

plt

####################################################################################################

# Límites
xlim <- c( 15, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -6, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt <- ggplot() +
  # geom_point( data = tas_pen_inv_h_dec[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.5 ) +
  # geom_point( data = tas_pen_inv_m_dec[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'c' ), size = 1.5 ) +
  # geom_point( data = tas_act_h_vej[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'b' ), size = 1.5 ) +
  geom_line( data = tas_pen_inv_m_dec[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'd' ), linewidth = 0.75 ) +
  geom_line( data = tas_pen_inv_h_dec[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_line( data = tas_pen_inv_hm_dec[ sexo == 'HM' ], aes( x = x, y = log( ux ), colour = 'c' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='TASAS', x = TeX( '$x$' ), 
        subtitle = 'Logaritmo de la tasa instantánea',
        y = TeX( '$\\log( \\mu^{2,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

plt



# Límites
xlim <- c( 20, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -10, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )


plt <- ggplot() +
  # geom_point( data = tas_pen_viu_h_dec[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'a' ), size = 1.5 ) +
  geom_line( data = tas_pen_viu_h_dec[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'b' ), linewidth = 0.75 ) +
  # geom_point( data = tas_pen_viu_m_dec[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'c' ), size = 1.5 ) +
  geom_point( data = tas_pen_viu_hm_dec[ sexo == 'HM' ], aes( x = x, y = log( ux_obs ), colour = 'c' ), size = 1.5 ) +
  geom_line( data = tas_pen_viu_m_dec[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'd' ), linewidth = 0.75 ) +
  geom_line( data = tas_pen_viu_hm_dec[ sexo == 'HM' ], aes( x = x, y = log( ux ), colour = 'e' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='TASAS', x = TeX( '$x$' ), 
        subtitle = 'Logaritmo de la tasa instantánea',
        y = TeX( '$\\log( \\mu^{2,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

plt


# Límites
xlim <- c( 0, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -12, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt <- ggplot() +
  # geom_point( data = tas_pen_orf_h_dec[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'a' ), size = 1.5 ) +
  geom_line( data = tas_pen_orf_h_dec[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'b' ), linewidth = 0.75 ) +
  # geom_point( data = tas_pen_orf_m_dec[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'c' ), size = 1.5 ) +
  geom_line( data = tas_pen_orf_m_dec[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'd' ), linewidth = 0.75 ) +
  geom_line( data = tas_pen_orf_hm_dec[ sexo == 'HM' ], aes( x = x, y = log( ux ), colour = 'e' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='TASAS', x = TeX( '$x$' ), 
        subtitle = 'Logaritmo de la tasa instantánea',
        y = TeX( '$\\log( \\mu^{2,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

plt


# Límites
xlim <- c( 15, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -16, -4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt <- ggplot() +
  # geom_point( data = tas_act_m_inv[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'c' ), size = 1.5 ) +
  geom_line( data = tas_act_m_inv[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'a' ), linewidth = 0.75 ) +
  geom_line( data = tas_act_h_inv[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'b' ), linewidth = 0.75 ) +
  geom_line( data = tas_act_hm_inv[ sexo == 'HM' ], aes( x = x, y = log( ux ), colour = 'e' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='TASAS', x = TeX( '$x$' ), 
        subtitle = 'Logaritmo de la tasa instantánea',
        y = TeX( '$\\log( \\mu^{2,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

plt


# Límites
xlim <- c( 0, 105 )
xbrk <- seq( xlim[1], xlim[2], 5 )
xlab <- formatC( xbrk, digits = 0, format = 'f' )

ylim <- c( -12, 4 )
ybrk <- seq( ylim[1], ylim[2], 1 )
ylab <- formatC( ybrk, digits = 2, format = 'f' )

plt <- ggplot() +
  # geom_point( data = tas_pen_viu_orf_h_sal[ sexo == 'H' ], aes( x = x, y = log( ux_obs ), colour = 'a' ), size = 1.5 ) +
  geom_line( data = tas_pen_viu_orf_h_sal[ sexo == 'H' ], aes( x = x, y = log( ux ), colour = 'b' ), linewidth = 0.75 ) +
  # geom_point( data = tas_pen_viu_orf_m_sal[ sexo == 'M' ], aes( x = x, y = log( ux_obs ), colour = 'c' ), size = 1.5 ) +
  geom_line( data = tas_pen_viu_orf_m_sal[ sexo == 'M' ], aes( x = x, y = log( ux ), colour = 'd' ), linewidth = 0.75 ) +
  geom_line( data = tas_pen_viu_orf_hm_sal[ sexo == 'HM' ], aes( x = x, y = log( ux ), colour = 'f' ), linewidth = 0.75 ) +
  scale_x_continuous( breaks = xbrk, labels = xlab, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, labels = ylab, limits = ylim ) +
  scale_colour_manual( 
    labels = c( 'H alisado y completado', 'H observado', 'M alisado y completado', 'M observado',
                'HM alisado y completado', 'HM observado' ),
    breaks = c( 'a', 'b', 'c', 'd', 'e', 'f' ), 
    values = c( 'darkred', 'orange', 'olivedrab4', 'skyblue2', 'dodgerblue4', 'darkgreen'  ) ) +
  labs( title ='TASAS', x = TeX( '$x$' ), 
        subtitle = 'Logaritmo de la tasa instantánea',
        y = TeX( '$\\log( \\mu^{2,6}_{i,t,x} )$' ) ) +
  plt_theme_legend

plt



