# Gráficos de tasas --------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGraficando tasas de fertilidad alisadas' )

source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
load( parametros$demo_rdata_inec_fert_model )

graf_units <- 'cm'
graf_dpi <- 300

cols_fun <- colorRampPalette( c( 
  'white', 'gold', 'orange', 'red', 'darkred', 
  'olivedrab2', 'olivedrab4', 'green', 
  'turquoise2', 'deepskyblue2', 'royalblue2', 'royalblue4' ) )

# Gráficos tasas de fertilidad ---------------------------------------------------------------------
# Probabilidades H y H -----------------------------------------------------------------------------
xbrk <- seq( 15, 105, 10 )
xlim <- range( xbrk )
ybrk <- seq( 0, 80, 10 )
ylim <- range( ybrk )

aux <- fer_dat[ sexo == 'H' & sexo_dep == 'H', list( x, y, u ) ]
aux_obs <- fer_dat[ sexo == 'H' & sexo_dep == 'H' & is.finite( u_obs ) ]
# aux_obs <- aux_obs[ q <= quantile( aux$q, probs = 0.9999 ) ]

u_lst <- unique( quantile( c( aux_obs$u_obs, aux$u ), probs = seq( 0, 1, length.out = 100 ), na.rm = TRUE ) )
u_lst <- c( 0, cumsum( diff( u_lst ) ) / sum( diff( u_lst ) ) )
cols_graf <- cols_fun( length( u_lst ) )

plt_obs_hh <- ggplot() +
  geom_tile( data = aux_obs, aes( x = x, y = y, fill = u_obs ), alpha = 0.8 ) +
  labs( title = 'Probabilidades observadas', x = 'Edad padre x', y = 'Edad hijo y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  plt_theme 

plt_hh <- ggplot() +
  geom_tile( data = aux, aes( x = x, y = y, fill = u ), alpha = 0.8 ) +
  labs( title = 'Probabilidades ajustadas', x = 'Edad padre x', y = 'Edad hijo y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  plt_theme 

aux <- data.table( res = aux_obs[ is.finite( r )]$r )
aux[ , res := ( res - mean( res ) ) / sd( res ) ]

xlim <- c( -4, 4 )
xbrk <- seq( xlim[1], xlim[2], length.out = 11 )

ylim <- c( -4, 4 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_qq_hh <- ggplot( data = aux, aes( sample = res ) ) +
  stat_qq( color = parametros$iess_blue, size = 0.7 ) +
  stat_qq_line( color = parametros$iess_green ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'QQplot de residuos', x = '', y = '' ) + 
  plt_theme

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_hist_hh <- ggplot( data = aux ) +
  geom_histogram( aes( x = res, y = ..density.. ), bins = 113, 
                  fill = parametros$iess_blue, color = 'white' ) +
  geom_area( stat = "function", fun = dnorm, 
             args = list( mean = mean( aux$res ), sd = sd( aux$res ) ), 
             col = parametros$iess_green, fill = parametros$iess_green, alpha = 0.3 ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'Histograma de residuos', x = '', y = '' ) + 
  theme_bw() +
  plt_theme 

plt_prob_hij_hh <- marrangeGrob( 
  list( plt_obs_hh, plt_hh, plt_hist_hh, plt_qq_hh ), 
  layout_matrix = matrix( c( 1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             NA, 3, 3, 4, 4, NA,
                             NA, 3, 3, 4, 4, NA ), 
                          nrow = 5, ncol = 6, byrow = TRUE ),
  top = '' )

ggsave( plot = plt_prob_hij_hh, 
        filename = paste0( parametros$resultado_graficos, 'iess_probabilidades_hijos_hh', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# Probabilidades H y M -----------------------------------------------------------------------------
xbrk <- seq( 15, 105, 10 )
xlim <- range( xbrk )
ybrk <- seq( 0, 80, 10 )
ylim <- range( ybrk )

aux <- fer_dat[ sexo == 'H' & sexo_dep == 'M' ]
aux_obs <- fer_dat[ sexo == 'H' & sexo_dep == 'M' & is.finite( u_obs ) ]

u_lst <- unique( quantile( c( aux_obs$u_obs, aux$u ), probs = seq( 0, 1, length.out = 100 ), na.rm = TRUE ) )
u_lst <- c( 0, cumsum( diff( u_lst ) ) / sum( diff( u_lst ) ) )
cols_graf <- cols_fun( length( u_lst ) )

plt_obs_hm <- ggplot() +
  geom_tile( data = aux_obs, aes( x = x, y = y, fill = u_obs ), alpha = 0.8 ) +
  labs( title = 'Probabilidades observadas', x = 'Edad padre x', y = 'Edad hija y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  plt_theme 

plt_hm <- ggplot() +
  geom_tile( data = aux, aes( x = x, y = y, fill = u ), alpha = 0.8 ) +
  labs( title = 'Probabilidades ajustadas', x = 'Edad padre x', y = 'Edad hija y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  plt_theme 

aux <- data.table( res = aux_obs[ is.finite( r )]$r )
aux[ , res := ( res - mean( res ) ) / sd( res ) ]

xlim <- c( -4, 4 )
xbrk <- seq( xlim[1], xlim[2], length.out = 11 )

ylim <- c( -4, 4 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_qq_hm <- ggplot( data = aux, aes( sample = res ) ) +
  stat_qq( color = parametros$iess_blue, size = 0.7 ) +
  stat_qq_line( color = parametros$iess_green ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'QQplot de residuos', x = '', y = '' ) + 
  theme_bw() +
  plt_theme 

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_hist_hm <- ggplot( data = aux ) +
  geom_histogram( aes( x = res, y = ..density.. ), bins = 113, 
                  fill = parametros$iess_blue, color = 'white' ) +
  geom_area( stat = "function", fun = dnorm, 
             args = list( mean = mean( aux$res ), sd = sd( aux$res ) ), 
             col = parametros$iess_green, fill = parametros$iess_green, alpha = 0.3 ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'Histograma de residuos', x = '', y = '' ) + 
  theme_bw() +
  plt_theme 

plt_prob_hij_hm <- marrangeGrob( 
  list( plt_obs_hm, plt_hm, plt_hist_hm, plt_qq_hm ), 
  layout_matrix = matrix( c( 1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             NA, 3, 3, 4, 4, NA,
                             NA, 3, 3, 4, 4, NA ), 
                          nrow = 5, ncol = 6, byrow = TRUE ),
  top = '' )

ggsave( plot = plt_prob_hij_hm, 
        filename = paste0( parametros$resultado_graficos, 'iess_probabilidades_hijos_hm', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# Probabilidades M y H -----------------------------------------------------------------------------
xbrk <- seq( 15, 105, 10 )
xlim <- range( xbrk )
ybrk <- seq( 0, 80, 10 )
ylim <- range( ybrk )

aux <- fer_dat[ sexo == 'M' & sexo_dep == 'H' ]
aux_obs <- fer_dat[ sexo == 'M' & sexo_dep == 'H' & is.finite( u_obs ) ]

u_lst <- unique( quantile( c( aux_obs$u_obs, aux$u ), probs = seq( 0, 1, length.out = 100 ), na.rm = TRUE ) )
u_lst <- c( 0, cumsum( diff( u_lst ) ) / sum( diff( u_lst ) ) )
cols_graf <- cols_fun( length( u_lst ) )

plt_obs_mh <- ggplot() +
  geom_tile( data = aux_obs, aes( x = x, y = y, fill = u_obs ), alpha = 0.8 ) +
  labs( title = 'Probabilidades observadas', x = 'Edad madre x', y = 'Edad hijo y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  theme_bw() +
  plt_theme 

plt_mh <- ggplot() +
  geom_tile( data = aux, aes( x = x, y = y, fill = u ), alpha = 0.8 ) +
  labs( title = 'Probabilidades ajustadas', x = 'Edad madre x', y = 'Edad hijo y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  theme_bw() +
  plt_theme 

aux <- data.table( res = aux_obs[ is.finite( r )]$r )
aux[ , res := ( res - mean( res ) ) / sd( res ) ]

xlim <- c( -4, 4 )
xbrk <- seq( xlim[1], xlim[2], length.out = 11 )

ylim <- c( -4, 4 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_qq_mh <- ggplot( data = aux, aes( sample = res ) ) +
  stat_qq( color = parametros$iess_blue, size = 0.7 ) +
  stat_qq_line( color = parametros$iess_green ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'QQplot de residuos', x = '', y = '' ) + 
  plt_theme

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_hist_mh <- ggplot( data = aux ) +
  geom_histogram( aes( x = res, y = ..density.. ), bins = 113, 
                  fill = parametros$iess_blue, color = 'white' ) +
  geom_area( stat = "function", fun = dnorm, 
             args = list( mean = mean( aux$res ), sd = sd( aux$res ) ), 
             col = parametros$iess_green, fill = parametros$iess_green, alpha = 0.3 ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'Histograma de residuos', x = '', y = '' ) + 
  plt_theme 

plt_prob_hij_mh <- marrangeGrob( 
  list( plt_obs_mh, plt_mh, plt_hist_mh, plt_qq_mh ), 
  layout_matrix = matrix( c( 1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             NA, 3, 3, 4, 4, NA,
                             NA, 3, 3, 4, 4, NA ), 
                          nrow = 5, ncol = 6, byrow = TRUE ),
  top = '' )

ggsave( plot = plt_prob_hij_mh, 
        filename = paste0( parametros$resultado_graficos, 'iess_probabilidades_hijos_mh', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# Probabilidades M y M -----------------------------------------------------------------------------
xbrk <- seq( 15, 105, 10 )
xlim <- range( xbrk )
ybrk <- seq( 0, 80, 10 )
ylim <- range( ybrk )

aux <- fer_dat[ sexo == 'M' & sexo_dep == 'M' ]
aux_obs <- fer_dat[ sexo == 'M' & sexo_dep == 'M' & is.finite( u_obs ) ]

u_lst <- unique( quantile( c( aux_obs$u_obs, aux$u ), probs = seq( 0, 1, length.out = 100 ), na.rm = TRUE ) )
u_lst <- c( 0, cumsum( diff( u_lst ) ) / sum( diff( u_lst ) ) )
cols_graf <- cols_fun( length( u_lst ) )

plt_obs_mm <- ggplot() +
  geom_tile( data = aux_obs, aes( x = x, y = y, fill = u_obs ), alpha = 0.8) +
  labs( title = 'Probabilidades observadas', x = 'Edad madre x', y = 'Edad hija y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  plt_theme 

plt_mm <- ggplot() +
  geom_tile( data = aux, aes( x = x, y = y, fill = u ), alpha = 0.8 ) +
  labs( title = 'Probabilidades ajustadas', x = 'Edad madre x', y = 'Edad hija y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  plt_theme 

aux <- data.table( res = aux_obs[ is.finite( r )]$r )
aux[ , res := ( res - mean( res ) ) / sd( res ) ]

xlim <- c( -4, 4 )
xbrk <- seq( xlim[1], xlim[2], length.out = 11 )

ylim <- c( -4, 4 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_qq_mm <- ggplot( data = aux, aes( sample = res ) ) +
  stat_qq( color = parametros$iess_blue, size = 0.7 ) +
  stat_qq_line( color = parametros$iess_green ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'QQplot de residuos', x = '', y = '' ) + 
  plt_theme

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_hist_mm <- ggplot( data = aux ) +
  geom_histogram( aes( x = res, y = ..density.. ), bins = 113, 
                  fill = parametros$iess_blue, color = 'white' ) +
  geom_area( stat = "function", fun = dnorm, 
             args = list( mean = mean( aux$res ), sd = sd( aux$res ) ), 
             col = parametros$iess_green, fill = parametros$iess_green, alpha = 0.3 ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'Histograma de residuos', x = '', y = '' ) + 
  plt_theme 

plt_prob_hij_mm <- marrangeGrob( 
  list( plt_obs_mm, plt_mm, plt_hist_mm, plt_qq_mm ), 
  layout_matrix = matrix( c( 1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             NA, 3, 3, 4, 4, NA,
                             NA, 3, 3, 4, 4, NA ), 
                          nrow = 5, ncol = 6, byrow = TRUE ),
  top = '' )

ggsave( plot = plt_prob_hij_mm, 
        filename = paste0( parametros$resultado_graficos, 'iess_probabilidades_hijos_mm', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# Gráficos tasas de nupcialidad --------------------------------------------------------------------
xbrk <- seq( 15, 110, 10 )
xlim <- range( xbrk )
ybrk <- seq( 15, 110, 10 )
ylim <- range( ybrk )

aux <- nup_dat
aux_obs <- nup_dat[ is.finite( u_obs ) ]

u_lst <- unique( quantile( c( aux_obs$u_obs, aux$u ), probs = seq( 0, 1, length.out = 100 ), na.rm = TRUE ) )
u_lst <- c( 0, cumsum( diff( u_lst ) ) / sum( diff( u_lst ) ) )
cols_graf <- cols_fun( length( u_lst ) )

plt_obs_cony <- ggplot() +
  geom_tile( data = aux_obs, aes( x = x, y = y, fill = u_obs ), alpha = 0.8 ) +
  labs( title = 'Probabilidades observadas', x = 'Edad afiliado x', y = 'Edad cónyuge y' ) + 
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( values = u_lst, colours = cols_graf ) +
  plt_theme

plt_cony <- ggplot() +
  geom_tile( data = aux, aes( x = x, y = y, fill = u ), alpha = 0.8 ) +
  labs( title = 'Probabilidades ajustadas', x = 'Edad afiliado x', y = 'Edad cónyuge y' ) +
  scale_x_continuous( breaks = xbrk, expand = c( 0, 0 ), limits = xlim ) +
  scale_y_continuous( breaks = ybrk, expand = c( 0, 0 ), limits = ylim ) +
  scale_fill_gradientn( colours = cols_graf ) +
  plt_theme

aux <- data.table( res = nup_dat[ is.finite( r ) ]$r )
aux[ , res := ( res - mean( res ) ) / sd( res ) ]

xlim <- c( -1, 1 )
xbrk <- seq( xlim[1], xlim[2], length.out = 11 )

ylim <- c( -1, 1 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

plt_qq_cony <- ggplot( data = aux, aes( sample = res ) ) +
  stat_qq( color = parametros$iess_blue, size = 0.7 ) +
  stat_qq_line( color = parametros$iess_green ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'QQplot de residuos', x = '', y = '' ) + 
  plt_theme

xlim <- c( -4, 4 )
xbrk <- seq( xlim[1], xlim[2], length.out = 11 )

ylim <- c( 0, 1 )
ybrk <- seq( ylim[1], ylim[2], length.out = 11 )

ur <- mean( aux$r, na.rm = TRUE )
sdr <- sd( aux$r, na.rm = TRUE )

plt_hist_cony <- ggplot( data = aux ) +
  geom_histogram( aes( x = res, y = ..density.. ), bins = 111,
                  fill = parametros$iess_blue, color = 'white' ) +
  geom_area( stat = "function", fun = dnorm, 
             args = list( mean = ur, sd = 1 ), 
             col = parametros$iess_green, fill = parametros$iess_green, alpha = 0.3 ) +
  scale_x_continuous( breaks = xbrk, limits = xlim ) +
  scale_y_continuous( breaks = ybrk, limits = ylim ) +
  labs( title = 'Histograma de residuos', x = '', y = '' ) +
  plt_theme

plt_prob_cony <- marrangeGrob( 
  list( plt_obs_cony, plt_cony, plt_hist_cony, plt_qq_cony ), 
  layout_matrix = matrix( c( 1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             1, 1, 1, 2, 2, 2,
                             NA, 3, 3, 4, 4, NA,
                             NA, 3, 3, 4, 4, NA ), 
                          nrow = 5, ncol = 6, byrow = TRUE ),
  top = '' )

ggsave( plot = plt_prob_cony, 
        filename = paste0( parametros$resultado_graficos, 'iess_probabilidades_conyuge', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()
