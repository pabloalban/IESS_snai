message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGraficando demografía de SNAI' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_SNAI_tablas_demografia.RData' ) )

# Pirámide por edad y sexo -------------------------------------------------------------------------

message( '\tGraficando población de servidores por edad y sexo' )

aux <- pir_porc_edad_sexo %>%
  mutate( porcentaje = if_else( sexo == 'F',
                         -porcentaje,
                         porcentaje ) ) %>%
  arrange( sexo, x )

salto_x <- 1
salto_y <- 5
brks_x <- seq( -69, 69, salto_y )
brks_y <- seq( -6, 6, salto_x )
lbls_y <- paste0( as.character( format(c( seq( 6, 0, -salto_x ), seq( salto_x, 6, salto_x ) ), decimal.mark = ',')), '%')

snai_pir_porc_edad_sexo <- ggplot( aux, aes( x = x, y = porcentaje, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( 'Porcentaje por grupo' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_x_continuous( breaks = brks_x) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom',
         axis.text.y = element_text( size = 7 ) ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = snai_pir_porc_edad_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_snai_pir_porc_edad_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Pirámide por grado y sexo ------------------------------------------------------------------

message( '\tGraficando población de servidores por grado  y sexo' )
aux <- pir_grado_sexo %>%
  mutate( porcentaje = if_else( sexo == 'F',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo, denominacion_puesto_unificado )

salto_x <- 5
brks_y <- seq( -50, 50, salto_x )
lbls_y <- paste0( as.character( format(c( seq( 50, 0, -salto_x ), seq( salto_x, 50, salto_x ) ), decimal.mark = ',')), '%')

snai_pir_grado_sexo <- ggplot( aux, aes( x = denominacion_puesto_unificado, y = porcentaje, fill = sexo ) ) +
  xlab( 'Grado' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 60, hjust = 0.5, vjust=0.5 ) ) +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) ) +
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = snai_pir_grado_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_snai_pir_grado_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Pirámide por provincia y sexo --------------------------------------------------------------------

message( '\tGraficando población de servidores por provincia y sexo' )
aux <- pir_prov_sexo %>%
  mutate( porcentaje = if_else( sexo == 'F',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo, provincia )

salto_x <- 3
brks_y <- seq( -30, 30, salto_x )
lbls_y <- paste0( as.character( format(c( seq( 30, 0, -salto_x ), seq( salto_x, 30, salto_x ) ), decimal.mark = ',')), '%')

snai_pir_prov_sexo <- ggplot( aux, aes( x = provincia, y = porcentaje, fill = sexo ) ) +
  xlab( 'Provincia' ) +
  ylab( 'Porcentaje por grupo') +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5, vjust=0.5 ) ) +
  guides( fill = guide_legend( title = NULL,
                               label.position = 'right', 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE ) )+
  theme( legend.position = 'bottom' ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ),
                     labels = c( 'Mujeres', 'Hombres' ) )

ggsave( plot = snai_pir_prov_sexo, 
        filename = paste0( parametros$resultado_graficos, 'IESS_snai_pir_prov_sexo',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide de salario según edad y sexo-------------------------------------------------

message( '\tGraficando salario según edad y sexo' )
aux <- pir_edad_sal_prom %>%
  mutate( porcentaje = if_else( sexo == 'F',
                                -porcentaje,
                                porcentaje ) ) %>%
  arrange( sexo, x )

salto_x <- 0.5
salto_y <- 5
brks_x <- seq( -69, 69, salto_y )
brks_y <- seq( -6, 6, salto_x )
lbls_y <- paste0( as.character( format(c( seq( 6, 0, -salto_x ), seq( salto_x, 6, salto_x ) ), decimal.mark = ',')), '%')

snai_pir_edad_sal_prom <- ggplot( aux, aes( x = x, y = porcentaje, fill = sexo ) ) +
  xlab( 'Edad' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity', colour = 'white') +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity', colour = 'white') +
  scale_x_continuous( breaks = brks_x ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
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

ggsave( plot = snai_pir_edad_sal_prom, 
        filename = paste0( parametros$resultado_graficos, 'IESS_snai_pir_edad_sal_prom',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

