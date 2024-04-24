message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGraficando demografía del SGRT' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_demografia.RData' ) )

# Parámetros----------------------------------------------------------------------------------------
x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

#1.Evolución de beneficiarios de SGRT---------------------------------------------------------------
message( '\tGraficando evolución de beneficiarios de SGRT' )

# 1.1 Evolución de beneficiarios de subsidios-------------------------------------------------------

aux <- tab_evo_ben_subsidios 

y_lim <- c( 0, 50000)
y_brk <- seq( y_lim[1], y_lim[2], 5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_subsidios_rtr <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ben, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = 'round' ) + 
  labs( x = 'Año', y = 'Subsidios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_subsidios_rtr, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_subsidios_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1.2 Evolución de beneficiarios de Indemnizaciones-------------------------------------------------

aux <- tab_evo_ben_indemnizaciones 

y_lim <- c( 0, 1200 )
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_indemnizaciones_rtr <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ben, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = 'round' ) + 
  labs( x = 'Año', y = 'Indemnizaciones' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_indemnizaciones_rtr, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_indemnizaciones_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1.3 Evolución de beneficiarios de incapacidad parcial---------------------------------------------

aux <- tab_evo_ben_pp 

y_lim <- c( 0, 8000 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_pp_rtr <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ben, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = 'round' ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_pp_rtr, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_pp_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1.4 Evolución de beneficiarios de incapacidad permanente total------------------------------------

aux <- tab_evo_ben_pt 

y_lim <- c( 0, 2000 )
y_brk <- seq( y_lim[1], y_lim[2], 400 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_pt_rtr <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ben, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = 'round' ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_pt_rtr, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_pt_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1.5 Evolución de beneficiarios de incapacidad absoluta--------------------------------------------

aux <- tab_evo_ben_pa

y_lim <- c( 0, 200 )
y_brk <- seq( y_lim[1], y_lim[2], 40 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_pa_rtr <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ben, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = 'round' ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_pa_rtr, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_pa_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1.6 Evolución de beneficiarios de viudedad--------------------------------------------------------

aux <- tab_evo_ben_vo

y_lim <- c( 0, 8000 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_vo_rtr <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ben, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = 'round' ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_vo_rtr, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_vo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1.7 Evolución de beneficiarios de orfandad--------------------------------------------------------

aux <- tab_evo_ben_of

y_lim <- c( 0, 6000 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_of_rtr <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = ben, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = 'round' ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_of_rtr, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_of_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2. Piramide poblacional al 31 de diciembre de 2222-------------------------------------------------
message( '\tGraficando Pirámide de beneficiarios de SGRT' )

# 2.1 Pirámide de beneficiarios de subsidios--------------------------------------------------------

aux <- pir_ben_subsidios %>%
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 90 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.005
brks_y <- seq( -0.04, 0.04, salto_x )
lbls_y <- paste0( as.character( c( seq( 0.04, 0, -salto_x )*100, seq( salto_x, 0.04, salto_x )*100 ) ), '%')
brks_x <- seq( 15, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_ben_subsidios <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 15, 75 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_ben_subsidios, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_subsidios',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2.2 Pirámide de beneficiarios de Indemnizaciones--------------------------------------------------

aux <- pir_ben_indemnizaciones %>%
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 100 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.01
brks_y <- seq(-0.06,0.04,salto_x)
lbls_y <- paste0( as.character( c( seq( 0.06, 0, -salto_x )*100, seq( salto_x, 0.04, salto_x )*100 ) ), '%')
brks_x <- seq( 20, 100, salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_ben_indemnizaciones <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 20, 80 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_ben_indemnizaciones, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_indemnizaciones', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2.3 Pirámide de beneficiarios de incapacidad parcial----------------------------------------------

aux <- pir_ben_pp  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.005
brks_y <- seq( -0.04, 0.04, salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100 )), '%')
brks_x <- seq(15, 105,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_ben_pp <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5,
                              reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_ben_pp, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_pp', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2.4 Pirámide de beneficiarios de incapacidad permanente total-------------------------------------

aux <- pir_ben_pt  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.005
brks_y <- seq( -0.04, 0.04, salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100 )), '%')
brks_x <- seq(20, 100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_ben_pt <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 20, 100 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_ben_pt, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_pt', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2.5 Pirámide de beneficiarios de incapacidad absoluta---------------------------------------------

aux <- pir_ben_pa  %>%
  mutate( edad = as.integer( edad ) ) %>% 
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.01
brks_y <- seq( -0.06, 0.04, salto_x)
lbls_y <- paste0(as.character(c(seq(0.06, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100 )), '%')
brks_x <- seq(25, 85,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_ben_pa <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white', width = 1) +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 25, 85 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_ben_pa, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_pa', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2.6 Pirámide de beneficiarios de viudedad---------------------------------------------------------

aux <- pir_ben_vo  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.0025
brks_y <- seq( -0.04, 0.04, salto_x)
lbls_y <- paste0( as.character( c( seq (0.04, 0, -salto_x )*100, seq( salto_x, 0.04, salto_x )*100 ) ), '%' )
brks_x <- seq( 20, 110, salto_y)
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_ben_vo <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 20, 110 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_ben_vo, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_vo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2.7 Pirámide de beneficiarios de orfandad---------------------------------------------------------

aux <- pir_ben_of  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 0 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.01
brks_y <- seq( -0.04, 0.04, salto_x)
lbls_y <- paste0( as.character( c( seq (0.04, 0, -salto_x )*100, seq( salto_x, 0.04, salto_x )*100 ) ), '%' )
brks_x <- seq( 0, 100, salto_y)
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_ben_of <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_ben_of, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_of', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 3. Pirámide de pensiones al 31 de diciembre de 2222-----------------------------------------------
message( '\tGraficando Pirámide de pensiones de SGRT' )

# 3.1 Pirámide de pensiones de subsidios------------------------------------------------------------

aux <- pir_montos_subsidios %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_promedio,
                         pen_promedio ) ) %>%
  filter( edad >= 15,
          edad <= 90 ) %>%
  arrange( sexo, edad )

salto_y <- 10
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 6), seq( 0, max(aux$fdp), length.out = 4 )[-1] ) )
lbls_y <- paste0( '$', formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 15, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_montos_subsidios <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 15, 85 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_montos_subsidios, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_montos_subsidios', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 3.2 Pirámide de pensiones de Indemnizaciones------------------------------------------------------

aux <- pir_montos_indemnizaciones %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_promedio,
                         pen_promedio ) ) %>%
  filter( edad >= 15,
          edad <= 100 ) %>%
  arrange( sexo, edad )

salto_y <- 10
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 4 ), seq( 0, max(aux$fdp), length.out = 6 )[-1] ) )
lbls_y <- paste0( '$', formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 20, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_montos_indemnizaciones <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_montos_indemnizaciones, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_montos_indemnizaciones', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 3.3 Pirámide de pensiones de incapacidad parcial--------------------------------------------------

aux <- pir_pensiones_pp %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_promedio,
                         pen_promedio ) ) %>%
  filter( edad >= 15,
          edad <= 100 ) %>%
  arrange( sexo, edad )

salto_y <- 10
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 5 ), seq( 0, max(aux$fdp), length.out = 5 )[-1] ) )
lbls_y <- paste0( '$', formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 25, 105, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_pensiones_pp <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 25, 105 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_pensiones_pp, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pensiones_pp', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 3.4 Pirámide de pensiones de incapacidad permanente total-----------------------------------------

aux <- pir_pensiones_pt  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_promedio,
                         pen_promedio ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 10
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 5 ), seq( 0, max(aux$fdp), length.out = 5 )[-1] ) )
lbls_y <- paste0( '$', formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 20, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_pensiones_pt <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_pensiones_pt, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pensiones_pt', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 3.5 Pirámide de pensiones de incapacidad absoluta-------------------------------------------------

aux <- pir_pensiones_pa  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_promedio,
                         pen_promedio ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 10
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 5 ), seq( 0, max(aux$fdp), length.out = 3 )[-1] ) )
lbls_y <- paste0( '$', formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 25, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_pensiones_pa <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white', width = 1) +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 25, 85 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_pensiones_pa, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pensiones_pa', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2.6 Pirámide de pensiones de viudedad-------------------------------------------------------------

aux <- pir_pensiones_vo  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_promedio,
                         pen_promedio ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 10
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 8 ), seq( 0, max(aux$fdp), length.out = 3 )[-1] ) )
lbls_y <- paste0( '$', formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 20, 110, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_pensiones_vo <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 20, 110 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_pensiones_vo, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pensiones_vo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 3.7 Pirámide de pensiones de orfandad-------------------------------------------------------------

aux <- pir_pensiones_of  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_promedio,
                         pen_promedio ) ) %>%
  filter( edad >= 0 ) %>%
  arrange( sexo, edad )

salto_y <- 10
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 5 ), seq( 0, max(aux$fdp), length.out = 5 )[-1] ) )
lbls_y <- paste0( '$', formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 0, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_pensiones_of <- ggplot( aux, aes( x = edad, y = fdp, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = c( 20, 100 ) ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend( title = NULL,label.position = 'right',
                              label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = iess_pir_pensiones_of, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pensiones_of',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiar Memoria RAM-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()