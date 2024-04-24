message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura población inicial' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_tasas_tran )
load( file = parametros$demo_rdata_sgo_tran_prep )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Parámetros ---------------------------------------------------------------------------------------
x_max <- parametros$demo_edad_max
x_lst <- 0:x_max

# Afiliados activos rt -----------------------------------------------------------------------------
message( '\tAfiliados activos rt' )

auxh <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx, Nx_dec ) %>% 
  dplyr::filter( sexo == 'H', x >= 15 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx_dec ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Hombres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

auxm <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx, Nx_dec ) %>% 
  dplyr::filter( sexo == 'M', x >= 15 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx_dec ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Mujeres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

aux <- rbind( auxh, auxm )

x_lim <- c( min( aux$anio), max( aux$anio ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, max( aux$rt ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_afi <- ggplot( data = aux, aes( x = anio, y = rt, fill = Sexo, color = Sexo ) ) + 
  geom_line( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                        rep( parametros$iess_blue, nrow( aux )/2 ) ), 
             size = graf_line_size ) + 
  geom_point( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                         rep( parametros$iess_blue, nrow( aux )/2 ) ), 
              size = graf_line_size, 
              aes( shape = Sexo ) ) + 
  labs( x = 'Año', y = 'Porcentaje' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) + 
  theme_bw( ) +
  plt_theme_legend + 
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_afi, 
        filename = paste0( parametros$resultado_graficos, 'iess_afi_rt', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# Pensionistas vejez rt ----------------------------------------------------------------------------
message( '\tPensionistas vejez rt' )

auxh <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Hombres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

auxm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Mujeres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

aux <- rbind( auxh, auxm )

x_lim <- c( min( aux$anio), max( aux$anio ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, max( aux$rt ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen <- ggplot( data = aux, aes( x = anio, y = rt, fill = Sexo, color = Sexo ) ) + 
  geom_line( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                        rep( parametros$iess_blue, nrow( aux )/2 ) ), 
             size = graf_line_size ) + 
  geom_point( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                         rep( parametros$iess_blue, nrow( aux )/2 ) ), 
              size = graf_line_size, 
              aes( shape = Sexo ) ) + 
  labs( x = 'Año', y = 'Porcentaje' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) + 
  theme_bw( ) +
  plt_theme_legend + 
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_vej_rt', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas invalidez rt ------------------------------------------------------------------------
message( '\tPensionistas invalidez rt' )

auxh <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & 
                   x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Hombres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

auxm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & 
                   x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Mujeres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

aux <- rbind( auxh, auxm )

x_lim <- c( min( aux$anio), max( aux$anio ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, max( aux$rt ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen <- ggplot( data = aux, aes( x = anio, y = rt, fill = Sexo, color = Sexo ) ) + 
  geom_line( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                        rep( parametros$iess_blue, nrow( aux )/2 ) ), 
             size = graf_line_size ) + 
  geom_point( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                         rep( parametros$iess_blue, nrow( aux )/2 ) ), 
              size = graf_line_size, 
              aes( shape = Sexo ) ) + 
  labs( x = 'Año', y = 'Porcentaje' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) + 
  theme_bw( ) +
  plt_theme_legend + 
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_rt', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# Pensionistas viudedad rt -------------------------------------------------------------------------
message( '\tPensionistas viudedad rt' )

auxh <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Hombres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

auxm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Mujeres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

aux <- rbind( auxh, auxm )

x_lim <- c( min( aux$anio), max( aux$anio ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, max( aux$rt ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen <- ggplot( data = aux, aes( x = anio, y = rt, fill = Sexo, color = Sexo ) ) + 
  geom_line( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                        rep( parametros$iess_blue, nrow( aux )/2 ) ), 
             size = graf_line_size ) + 
  geom_point( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                         rep( parametros$iess_blue, nrow( aux )/2 ) ), 
              size = graf_line_size, 
              aes( shape = Sexo ) ) + 
  labs( x = 'Año', y = 'Porcentaje' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) + 
  theme_bw( ) +
  plt_theme_legend + 
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_viu_rt', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas orfandad hombres --------------------------------------------------------------------
message( '\tPensionistas orfandad hombres' )

auxh <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx ) %>% 
  dplyr::filter( sexo == 'H' & tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Hombres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

auxm <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx ) %>% 
  dplyr::filter( sexo == 'M' & tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ), 
                 Sexo = 'Mujeres' ) %>% 
  dplyr::select( - ERt, - Nt ) %>% 
  as.data.frame()

aux <- rbind( auxh, auxm )

x_lim <- c( min( aux$anio), max( aux$anio ) )
x_brk <- seq( x_lim[1], x_lim[2], length.out = 9 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, max( aux$rt ) )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen <- ggplot( data = aux, aes( x = anio, y = rt, fill = Sexo, color = Sexo ) ) + 
  geom_line( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                        rep( parametros$iess_blue, nrow( aux )/2 ) ), 
             size = graf_line_size ) + 
  geom_point( color = c( rep( parametros$iess_green, nrow( aux )/2 ), 
                         rep( parametros$iess_blue, nrow( aux )/2 ) ), 
              size = graf_line_size, 
              aes( shape = Sexo ) ) + 
  labs( x = 'Año', y = 'Porcentaje' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) + 
  theme_bw( ) +
  plt_theme_legend + 
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_orf_rt', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámides afiliados activos ----------------------------------------------------------------------
message( '\tAfiliados activos' )

aux_erx <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx ) %>% 
  dplyr::filter( x >= 15 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 15,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_afi <- ggplot( aux_erx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_erx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_erx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_afi, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_nx <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, Nx_dec ) %>% 
  dplyr::filter( x >= 15 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx_dec ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3 ), as.integer ) %>%
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 15,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_afi_nx <- ggplot( aux_nx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_nx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_nx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_afi_nx, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados_nx', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámide pensionistas vejez ----------------------------------------------------------------------
message( '\tPensionistas vejez' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx ) %>% 
  dplyr::filter( tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  # # dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen <- ggplot( aux_erx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_erx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_erx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_vejez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, Nx ) %>% 
  dplyr::filter( tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3 ), as.integer ) %>%
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen_nx <- ggplot( aux_nx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_nx %>% filter(sexo == 'M'), stat = 'identity', colour='white', size=0.1) +
  geom_bar( data = aux_nx %>% filter(sexo == 'H'), stat = 'identity', colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen_nx, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_vejez_nx', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámide pensionistas invalidez ------------------------------------------------------------------
message( '\tPensionistas invalidez' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx ) %>% 
  dplyr::filter( tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen <- ggplot( aux_erx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_erx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_erx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_invalidez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, Nx ) %>% 
  dplyr::filter( tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3 ), as.integer ) %>%
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen_nx <- ggplot( aux_nx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_nx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_nx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen_nx, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_invalidez_nx', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámide pensionistas viudedad -------------------------------------------------------------------
message( '\tPensionistas viudedad' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx ) %>% 
  dplyr::filter( tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen <- ggplot( aux_erx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_erx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_erx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_viudedad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, Nx ) %>% 
  dplyr::filter( tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3 ), as.integer ) %>%
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen_nx <- ggplot( aux_nx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_nx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_nx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen_nx, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_viudedad_nx', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámide pensionistas orfandad -------------------------------------------------------------------
message( '\tPensionistas orfandad' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, ERx ) %>% 
  dplyr::filter( tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen <- ggplot( aux_erx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_erx %>% filter(sexo == 'M'), stat = 'identity',colour='white', size=0.1) +
  geom_bar( data = aux_erx %>% filter(sexo == 'H'), stat = 'identity',colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_orfandad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( anio, tipo, sexo, x, Nx ) %>% 
  dplyr::filter( tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3 ), as.integer ) %>%
  as.data.frame() %>% 
  mutate( H = -H ) %>%
  gather( .,
          key = 'sexo',
          value = 'n',
          H,
          M ) %>%
  filter( x >= 55,
          x <= 105 ) %>%
  arrange( sexo, x )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), '%')
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen_nx <- ggplot( aux_nx, aes( x = x, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux_nx %>% filter(sexo == 'M'), stat = 'identity', colour='white', size=0.1) +
  geom_bar( data = aux_nx %>% filter(sexo == 'H'), stat = 'identity', colour='white', size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = 'right',
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position='bottom') +
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ),
                    labels = c( 'Hombres', 'Mujeres')) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_pen_nx, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_orfandad_nx', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
