message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( parametros$demo_rdata_sgo_pob_proy )
pob_proy <- pob_proy[ t <= parametros$demo_horizonte ]

# Población PEA no afiliados -----------------------------------------------------------------------
message( '\tGraficando proyección PEA no afiliados' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1.7e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l1 ) ]
plt_l1_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l1, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^1$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l1_m,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l1_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l1 ) ]
plt_l1_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l1, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^1$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l1_h,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l1_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población afiliados activos-----------------------------------------------------------------------
message( '\tGraficando proyección afiliados activos' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1.2e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l2 ) ]
plt_l2_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l2, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^2$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l2_m,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l2 ) ]
plt_l2_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l2, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^2$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l2_h,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población afiliados cesantes ---------------------------------------------------------------------
message( '\tGraficando proyección afiliados cesantes' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1.2e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l3 ) ]
plt_l3_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l3, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^3$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l3_m,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l3 ) ]
plt_l3_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l3, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^3$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l3_h,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas vejez (jubilados) ---------------------------------------------------------
message( '\tGraficando proyección pensionistas por vejez' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1.2e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l4 ) ]
plt_l4_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l4, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^4$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l4_m,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l4 ) ]
plt_l4_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l4, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^4$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l4_h,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas invalidez (jubilados por invalidez)----------------------------------------
message( '\tGraficando proyección pensionistas por invalidez' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e4 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l5 ) ]
plt_l5_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l5, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^5$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l5_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l5_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l5 ) ]
plt_l5_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l5, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^5$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l5_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l5_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población de muertos -----------------------------------------------------------------------------
message( '\tGraficando proyección de muertos' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1.2e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l6 ) ]
plt_l6_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l6, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^6$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l6_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l6_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l6 ) ]
plt_l6_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l6, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^6$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l6_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l6_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Unión proyecciones activos -----------------------------------------------------------------------
message( '\tGraficando unión de proyecciones de activos' )
plt_pob <- marrangeGrob( list(
  plt_l1_m, plt_l1_h,
  plt_l2_m, plt_l2_h, 
  plt_l3_m, plt_l3_h, 
  plt_l4_m, plt_l4_h, 
  plt_l5_m, plt_l5_h,
  plt_l6_m, plt_l6_h ),
  nrow = 2, ncol = 6, top = '' )

ggsave( plot = plt_pob,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy', parametros$graf_ext ),
        width = 50, height = 18, units = graf_units, dpi = graf_dpi )

# Población de montepíos viudas --------------------------------------------------------------------
message( '\tGraficando proyección de montepíos viudas' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e4 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l7 ) ]
plt_l7_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l7, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^7$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l7_m,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l7_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l7 ) ]
plt_l7_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l7, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^7$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l7_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l7_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población de montepíos huérfanos -----------------------------------------------------------------
message( '\tGraficando proyección de montepíos huérfanos' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2e3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l8 ) ]
plt_l8_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l8, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^8$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l8_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l8_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l8 ) ]
plt_l8_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l8, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^8$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l8_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l8_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Unión proyecciones montepíos ---------------------------------------------------------------------
message( '\tGraficando unión de proyecciones de montepíos' )
plt_pob_mont <- marrangeGrob( list(
  plt_l7_m, plt_l7_h,
  plt_l8_m, plt_l8_h ),
  nrow = 2, ncol = 2, top = '' )

ggsave( plot = plt_pob_mont,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_mont', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población de dependientes cónyuges ---------------------------------------------------------------
message( '\tGraficando proyección de dependientes cónyuges' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2e4 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l9 ) ]
plt_l9_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l9, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^9$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l9_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l9_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l9 ) ]
plt_l9_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l9, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^9$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l9_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l9_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población de dependientes hijos ------------------------------------------------------------------
message( '\tGraficando proyección de dependientes hijos' )
## Mujeres -----------------------------------------------------------------------------------------
num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2e4 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- pob_proy[ sexo == 'M', list( t, x, l10 ) ]
plt_l10_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l10, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\l^{10}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l10_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l10_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Hombres -----------------------------------------------------------------------------------------
aux_h <- pob_proy[ sexo == 'H', list( t, x, l10 ) ]
plt_l10_h <- ggplot() +
  geom_line( data = aux_h, aes( x = x, y = l10, group = t, colour = t ), linewidth = graf_line_size ) +
  scale_colour_gradientn( colours = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\l^{10}$")) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l10_h, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l10_h', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Unión proyecciones dependientes ------------------------------------------------------------------
message( '\tGraficando unión de proyecciones de dependientes' )
plt_pob_dep <- marrangeGrob( list(
  plt_l9_m, plt_l9_h,
  plt_l10_m, plt_l10_h ),
  nrow = 2, ncol = 2, top = '' )

ggsave( plot = plt_pob_mont,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_dep', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
