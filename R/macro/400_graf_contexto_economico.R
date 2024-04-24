message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
message( '\tCargando datos' )
load( file = parametros$macro_rdata_info )
load( file = parametros$macro_rdata_macro_est )
load( file = parametros$macro_rdata_biess_proy )

# 0. Parámetros-------------------------------------------------------------------------------------
if( parametros$seguro %in% c( 'SAL' ) ) {
  anio_fin = 10 + 2020
} else if ( parametros$seguro %in% c( 'SSC' ) ) {
  anio_fin = 20 + 2020
} else {
  anio_fin = 40 + 2020
}

anio_ini <- 2002
anio_corte <- 2022

# 1. Evolución histórica----------------------------------------------------------------------------
# Evolución histórica del índice de precios ( IPC )-------------------------------------------------
message( '\tGraficando análisis de contexto' )
aux <- inflacion %>%
  filter( anio >= '2003',
          anio <= '2020' ) %>%
  dplyr::select( periodo,
                 ipc,
                 inflacion_mensual,
                 inflacion_variacion_anual ) %>%
  arrange( periodo )

y_lim <- c( 0, 130 )
y_brk <- seq( 0, y_lim[2], by = 15 )
y_lbl <-
  formatC( 
    y_brk,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

y_brk_dual <- seq( -2, 10, by = 2 )
y_lbl_dual <- paste0( y_brk_dual, '%' )

iess_inflacion  <- ggplot( data = aux, aes( x = periodo ) ) +
  geom_line( 
    aes( 
      y = 10 * inflacion_variacion_anual + 20 ,
      group = 1L,
      color = 'Inflación Acumulada'
    ),
    linewidth = graf_line_size
  ) +
  geom_line( aes( y = ipc,
                  group = 1L,
                  color = 'aIPC' ),
             linewidth = graf_line_size ) +
  scale_x_date( 
    breaks = seq( as.Date( '2002-12-01' ), as.Date( '2020-12-01' ), by = '24 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2002-12-01', '2020-12-01' ), '%Y-%m-%d' )
  ) +
  scale_y_continuous( 
    breaks = y_brk,
    labels = y_lbl,
    limits = y_lim,
    sec.axis = sec_axis( 
      ~ . * ( 1 / 10 ) - 2,
      name = 'Inflación Acumulada',
      breaks = y_brk_dual,
      labels = y_lbl_dual
    )
  ) +
  scale_color_manual( 
    values =  c( parametros$iess_blue, parametros$iess_green ),
    labels = c( 'IPC', 'Inflación Acumulada' )
  ) +
  theme_bw(  ) +
  plt_theme +
  labs( x = 'Año', y = 'IPC' ) +
  theme( legend.position = 'bottom' )  +
  theme( axis.text.x = element_text( 
    angle = 90,
    hjust = 0.5,
    vjust = 0.5
  ) )

ggsave( 
  plot = iess_inflacion,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_inflacion',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Evolución histórica del desempleo-----------------------------------------------------------------
message( '\tGraficando análisis de desempleo' )

aux <- desempleo %>%
  filter( anio >= '2007',
          anio <= '2020' ) %>%
  dplyr::select( periodo,
                 desempleo_nacional,
                 empleo_adecuado_pleno_n ) %>%
  arrange( periodo )

scl = 0.1  # escala de millones
hmts = 1.1 #homotecia

y_lim <- c( 3, 7 )
y_brk <- seq( 0, y_lim[2], by = 0.5 )
y_lbl <- paste0(
  formatC( 
    y_brk,
    digits = 1,
    format = 'f',
    big.mark = '',
    decimal.mark = ','
  ), 
  '%' )

y_brk_dual <- seq( 20, 60, by = 5 )
y_lbl_dual <- paste0( y_brk_dual, '%' )

iess_desempleo  <- ggplot( data = aux, aes( x = periodo ) ) +
  geom_line( 
    aes( 
      y = scl * hmts * empleo_adecuado_pleno_n + 0.9,
      group = 1L,
      color = 'empleo_adecuado_pleno_n'
    ),
    linewidth = graf_line_size
  ) +
  geom_line( aes( y = desempleo_nacional,
                  group = 1L,
                  color = 'adesempleo_nacional' ),
             linewidth = graf_line_size ) +
  scale_x_date( 
    breaks = seq( as.Date( '2007-12-01' ), as.Date( '2020-12-01' ), by = '12 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2007-12-01', '2020-12-01' ), '%Y-%m-%d' )
  ) +
  scale_y_continuous( 
    breaks = y_brk,
    labels = y_lbl,
    limits = y_lim,
    sec.axis = sec_axis( 
      ~ . / ( scl * hmts ) - 8.181818,
      name = 'Empleo Pleno',
      breaks = y_brk_dual,
      labels = y_lbl_dual
    )
  ) +
  scale_color_manual( 
    values =  c( parametros$iess_green, parametros$iess_blue ),
    labels = c( 'Tasa Desempleo', 'Tasa de empleo pleno' )
  ) +
  theme_bw(  ) +
  plt_theme +
  labs( x = 'Año', y = 'Tasa Desempleo' ) +
  theme( legend.position = 'bottom' ) +
  theme( axis.text.x = element_text( 
    angle = 90,
    hjust = 0.5,
    vjust = 0.5
  ) )

ggsave( 
  plot = iess_desempleo,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_desempleo',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Evolución del salario básico unificado ( SBU )----------------------------------------------------
aux <- sbu %>%
  filter( anio <= '2020' )

x_lim <- c( 2000, 2020 )
x_brk <- seq( x_lim[1] , x_lim[2], 2 )
x_lbl <-
  formatC( 
    x_brk,
    digits = 0,
    format = 'f',
    big.mark = '',
    decimal.mark = ','
  )

y_lim <- c( 0, 450 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 10 )
y_lbl <-
  formatC( 
    y_brk,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

iess_sbu <- ggplot( data = aux ) +
  geom_line( aes( 
    x = anio,
    y = sbu,
    color = parametros$iess_green
  ),
  linewidth = graf_line_size ) +
  labs( x = 'Año', y = 'Salario Básico Unificado ( USD )' ) +
  scale_color_manual( values =  c( parametros$iess_green ),
                      labels = c( '' ) ) +
  scale_x_continuous( breaks = x_brk,
                      labels = x_lbl,
                      limits = x_lim ) +
  scale_y_continuous( breaks = y_brk,
                      labels = y_lbl,
                      limits = y_lim ) +
  theme_bw(  ) +
  plt_theme +
  theme( axis.text.x = element_text( 
    angle = 0,
    hjust = 0.5,
    vjust = 0.5
  ),
  legend.position = 'none' )

ggsave( 
  plot = iess_sbu,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_sbu',
    parametros$graf_ext
  ),
  width = graf_width ,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Evolución histórica del salario promedio----------------------------------------------------------
aux <- salarios %>%
  na.omit( . ) %>%
  filter( periodo >= as.Date( '01/12/2006', '%d/%m/%Y' ) )

x_lim <- c( 2005, 2020 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <-
  formatC( 
    x_brk,
    digits = 0,
    format = 'f',
    big.mark = '',
    decimal.mark = ','
  )

y_lim <- c( 0, 800 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <-
  formatC( 
    y_brk,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

iess_salarios_evo <- ggplot( data = aux,
                             aes( x = periodo ) ) +
  geom_line( aes( y = sal_prom ),
             color = parametros$iess_green,
             linewidth = graf_line_size ) +
  labs( x = 'Año', y = 'Salario promedio ( USD )' ) +
  scale_x_date( 
    breaks = seq( as.Date( '2006-12-01' ), as.Date( '2020-12-01' ), by = '12 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2006-12-01', '2020-12-01' ), '%Y-%m-%d' )
  ) +
  scale_y_continuous( breaks = y_brk,
                      labels = y_lbl,
                      limits = y_lim ) +
  theme_bw(  ) +
  plt_theme +
  theme( axis.text.x = element_text( 
    angle = 90,
    hjust = 0.5,
    vjust = 0.5
  ) )

ggsave( 
  plot = iess_salarios_evo,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_salarios_evo',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Evolución histórica del PIB del Ecuador-----------------------------------------------------------
aux <- pib_real %>%
  filter( anio >= '2000',
          anio <= '2020' ) %>%
  mutate( periodo = ymd( paste0( anio, '/01/01' ) ) ) %>%
  mutate( apib_constantes = pib_constantes  / 1000000 ) %>%
  mutate( var = 'PIB a precios constantes' )

scl = 1000  # escala de millones
hmts = 3 #homotecia

x_lim <- c( 2000, 2020 )
x_brk <- seq( x_lim[1], x_lim[2], by = 2 )
x_lbl <-
  formatC( 
    x_brk,
    digits = 0,
    format = 'f',
    big.mark = '',
    decimal.mark = ','
  )

y_lim <- c( 0, 80000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <-
  formatC( 
    y_brk,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

y_brk_dual <- seq( -9, 15, by = 3 )
y_lbl_dual <- paste0( y_brk_dual, '%' )

iess_pib_real <- ggplot( data = aux,
                         aes( x = periodo,
                              y = apib_constantes ,
                              fill = var ) ) +
  geom_bar( stat = 'identity',
            colour = 'black' ) +
  geom_line( 
    data = aux,
    aes( 
      x = periodo,
      y = crecimiento_pib * hmts * scl + 35000,
      group = 1,
      linetype = 'Tasa de crecimiento PIB'
    ),
    inherit.aes = FALSE,
    size = 1
  ) +
  scale_linetype_manual( NULL, values = 1 ) +
  scale_x_date( 
    breaks = seq( as.Date( '2000-01-01' ), as.Date( '2020-01-01' ), by = '24 months' ),
    date_labels = '%Y',
    limits = as.Date( c( '2000-0-01', '2020-12-31' ), '%Y-%m-%d' )
  ) +
  scale_y_continuous( 
    name = 'PIB a precios constantes ( millones de USD )',
    labels = y_lbl,
    breaks = y_brk,
    limits = y_lim,
    sec.axis = sec_axis( 
      ~ . / ( scl * hmts ) - 11.66667,
      name = 'Tasa de crecimiento PIB real',
      labels = y_lbl_dual,
      breaks = y_brk_dual
    )
  ) +
  scale_color_manual( 
    values =  c( parametros$iess_green, parametros$iess_blue ),
    labels = c( 'PIB a precios constantes', 'Tasa de crecimiento PIB real' )
  ) +
  scale_fill_manual( values = c( parametros$iess_green, parametros$iess_blue ) ) +
  theme_bw(  ) +
  plt_theme +
  guides( color = guide_colorbar( order = 0 ),
          fill = guide_legend( order = 1 ) ) +
  theme( legend.position = 'bottom' ) +
  labs( x = '', y = '' ) +
  theme( 
    legend.background = element_rect( fill = 'transparent' ),
    legend.box.background = element_rect( fill = 'transparent',
                                          colour = NA ),
    legend.key = element_rect( fill = 'transparent' ),
    legend.spacing = unit( -1, 'lines' )
  )

ggsave( 
  plot = iess_pib_real,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_pib_real',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Evolución histórica de las tasas de interés-------------------------------------------------------
message( '\tGraficando tasas de interés' )
aux <- tasas_interes %>%
  filter( anio >= '2003',
          anio <= '2020' ) %>%
  arrange( periodo )

y_lim <- c( 2, 16 )
y_brk <- seq( 0, y_lim[2], by = 2 )
y_lbl <-
  paste0( formatC( 
    y_brk,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  ),
  '%' )

iess_tasas_interes  <- ggplot( data = aux, aes( x = periodo ) ) +
  geom_line( aes( y = tasa_activa,
                  group = 1L,
                  color = 'tasa_activa' ),
             linewidth = graf_line_size ) +
  geom_line( aes( y = tasa_pasiva        ,
                  group = 1L,
                  color = 'tasa_pasiva' ),
             linewidth = graf_line_size ) +
  scale_x_date( 
    breaks = seq( as.Date( '2002-12-01' ), as.Date( '2020-12-01' ), by = '24 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2002-12-01', '2020-12-01' ), '%Y-%m-%d' )
  ) +
  scale_y_continuous( breaks = y_brk,
                      labels = y_lbl,
                      limits = y_lim ) +
  scale_color_manual( 
    values =  c( parametros$iess_blue, parametros$iess_green ),
    labels = c( 'Tasa activa referencial', 'Tasa pasiva referencial' )
  ) +
  theme_bw(  ) +
  plt_theme +
  labs( x = 'Año', y = '' ) +
  theme( legend.position = 'bottom' )  +
  theme( axis.text.x = element_text( angle = 90, hjust = 0 ) )

ggsave( 
  plot = iess_tasas_interes,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_tasas_interes',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Portafolio de inversiones del BIESS---------------------------------------------------------------
message( '\tGraficando evolución histórica de las inversiones del BIESS' )

aux <- rendimiento_biess %>%
  #filter(  fecha <= as.Date( '01/12/2020', '%d/%m/%Y'  )  ) %>%
  mutate( rendimiento = rendimiento * 100,
          instrumento = 'Fondos administrados BIESS' ) %>%
  dplyr::select( fecha,
                 mes,
                 f_adm,
                 rendimiento,
                 instrumento ) %>%
  na.omit( . )

df_bar <- aux %>% dplyr::select( -rendimiento )
df_line = aux %>% dplyr::select( fecha, rendimiento )

scl = 1000000  # escala de millones
hmts = 30 #homotecia

y_lim <- c( 0, 25000000000 )
y_brk <- seq( y_lim[1], y_lim[2], 3000000000 )
y_lbl <-
  formatC( 
    y_brk / scl,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

y_lim_dual <- c( 0, 18 )
ydual_brk <- seq( 0, 18, 2 )
ydual_lbl <- paste0( formatC( 
  ydual_brk,
  digits = 0,
  format = 'f',
  big.mark = '.',
  decimal.mark = ','
),
'%' )

biess_rendimiento <- ggplot( data = df_bar,
                             aes( x = fecha,
                                  y = f_adm,
                                  fill = instrumento ) ) +
  geom_area( alpha = 0.7,
             size = 0.5,
             colour = '#1a5c0c' ) +
  geom_line( 
    data = df_line,
    aes( 
      x = fecha,
      y = rendimiento * hmts * scl * 45,
      group = 1,
      linetype = 'Rendimiento Neto'
    ),
    inherit.aes = FALSE,
    linewidth = graf_line_size
  ) +
  scale_linetype_manual( NULL, values = 1 ) +
  scale_x_date( 
    breaks = seq( as.Date( '2011-12-01' ), as.Date( '2020-12-01' ), by = '12 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2011-12-01', '2020-12-01' ), '%Y-%m-%d' )
  ) +
  scale_y_continuous( 
    name = 'Fondos administrados BIESS ( millones USD )',
    labels = y_lbl,
    breaks = y_brk,
    limits = y_lim,
    sec.axis = sec_axis( 
      ~ . / ( scl * hmts * 45 ),
      name = 'Rendimiento Neto',
      labels = ydual_lbl,
      breaks = ydual_brk
    )
  ) +
  scale_fill_manual( values = c( parametros$iess_green,
                                 parametros$iess_blue ) ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( 
    title = NULL,
    label.position = 'right',
    label.hjust = 0
  ) ) +
  theme( legend.position = 'bottom' ) +
  labs( x = '', y = '' ) +
  theme( 
    legend.background = element_rect( fill = 'transparent' ),
    legend.box.background = element_rect( fill = 'transparent',
                                          colour = NA ),
    legend.key = element_rect( fill = 'transparent' ),
    legend.spacing = unit( -1, 'lines' )
  ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 0 ) )

ggsave( 
  plot = biess_rendimiento,
  filename = paste0( 
    parametros$resultado_graficos,
    'biess_rendimiento',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 2. Predicciones-----------------------------------------------------------------------------------
# Predicciones PIB nominal--------------------------------------------------------------------------
aux <- intervalos_confianza %>% 
  group_by( anio ) %>% 
  mutate( pib_anual = sum( pib, na.rm = TRUE )/1000,
          lim_inf_pib = sum( lim_inf_pib )/1000,
          lim_sup_pib = sum( lim_sup_pib )/1000 ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, .keep_all = TRUE ) %>% 
  dplyr::select( anio, pib_anual, lim_inf_pib, lim_sup_pib ) %>% 
  mutate( lim_inf_pib = if_else( anio == anio_corte,
                                 pib_anual,
                                 lim_inf_pib ),
          lim_sup_pib = if_else( anio == anio_corte,
                                 pib_anual,
                                 lim_sup_pib ) )

aux_his <- aux %>% filter( anio <= anio_corte )
aux_pred <- aux %>% filter( anio >= anio_corte, anio <= anio_fin )

lim_y <- c( 0 , 250000 )
salto_y = 50000
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <-
  formatC( 
    brks_y,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

iess_pib_pred <- ggplot(  ) +
  geom_line( 
    data = aux_his,
    aes( x = anio, y = pib_anual, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green
  ) + 
  geom_ribbon( data = aux_pred,
               aes( x = anio,
                    ymin = lim_inf_pib,
                    ymax = lim_sup_pib ), 
               fill = 'blue',
               alpha = 0.2 ) +
  geom_line( 
    data = aux_pred,
    aes( x = anio, y = pib_anual, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green,
    linetype = 'dashed'
  ) +
  xlab( '' ) +
  ylab( 'PIB nominal ( millones de USD )' ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = seq( 2000, anio_fin, by = 5 ),
                      breaks = seq( 2000, anio_fin, by = 5 ) ) +
  theme_bw(  ) +
  plt_theme

ggsave( 
  plot = iess_pib_pred,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_pib_pred',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Predicciones Salario promedio anual---------------------------------------------------------------
aux <- intervalos_confianza %>% 
  group_by( anio ) %>% 
  mutate( sal_anual = sum( sal_prom, na.rm = TRUE ),
          lim_inf_sal_prom = sum( lim_inf_sal_prom ),
          lim_sup_sal_prom = sum( lim_sup_sal_prom ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sal_anual, lim_inf_sal_prom, lim_sup_sal_prom ) %>% 
  mutate( lim_inf_sal_prom = if_else( anio == anio_corte,
                                      sal_anual,
                                      lim_inf_sal_prom ),
          lim_sup_sal_prom = if_else( anio == anio_corte,
                                      sal_anual,
                                      lim_sup_sal_prom ) )

aux_his <- aux %>% filter( anio <= anio_corte )
aux_pred <- aux %>% filter( anio >= anio_corte, anio <= anio_fin )

lim_y <- c( 0, 25000 )
salto_y = 5000
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <-
  formatC( 
    brks_y,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

iess_sal_pred <- ggplot(  ) +
  geom_line( 
    data = aux_his,
    aes( x = anio, y = sal_anual, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green
  ) +
  geom_ribbon( data = aux_pred,
               aes( x = anio,
                    ymin = lim_inf_sal_prom,
                    ymax = lim_sup_sal_prom ), 
               fill = 'blue',
               alpha = 0.2 ) +
  geom_line( 
    data = aux_pred,
    aes( x = anio, y = sal_anual, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green,
    linetype = 'dashed'
  ) +
  xlab( '' ) +
  ylab( 'Salario promedio anual ( USD )' ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = seq( 2000, anio_fin, by = 5 ),
                      breaks = seq( 2000, anio_fin, by = 5 ) ) +
  theme_bw(  ) +
  plt_theme

ggsave( 
  plot = iess_sal_pred,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_sal_pred',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Predicciones  del salario básico unificado--------------------------------------------------------
message( '\tGraficando salario básico unificado' )

aux <- intervalos_confianza %>% 
  filter( mes == '6' ) %>% 
  dplyr::select( anio, sbu, lim_inf_sbu, lim_sup_sbu ) %>% 
  mutate( lim_inf_sbu = if_else( anio == anio_corte,
                                 sbu,
                                 lim_inf_sbu ),
          lim_sup_sbu = if_else( anio == anio_corte,
                                 sbu,
                                 lim_sup_sbu ) )

aux_his <- aux %>% filter( anio <= anio_corte )
aux_pred <- aux %>% filter( anio >= anio_corte, anio <= anio_fin )

lim_y <- c( 0, 1300 )
salto_y <- 200
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <-
  formatC( 
    brks_y,
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  )

iess_sbu_pred <- ggplot(  ) +
  geom_line( 
    data = aux_his,
    aes( x = anio, y = sbu, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green
  ) +
  geom_ribbon( data = aux_pred,
               aes( x = anio,
                    ymin = lim_inf_sbu,
                    ymax = lim_sup_sbu ), 
               fill = 'blue',
               alpha = 0.2 ) +
  geom_line( 
    data = aux_pred,
    aes( x = anio, y = sbu, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green,
    linetype = 'dashed'
  ) +
  xlab( '' ) +
  ylab( 'SBU ( USD )' ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = seq( 2000, anio_fin, by = 5 ),
                      breaks = seq( 2000, anio_fin, by = 5 ) ) +
  theme_bw(  ) +
  plt_theme

ggsave( 
  plot = iess_sbu_pred,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_sbu_pred',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Predicciones tasa pasiva--------------------------------------------------------------------------
message( '\tGraficando tasa pasiva' )

aux <- intervalos_confianza %>% 
  mutate( fecha = as.Date( paste0( anio, '-', mes, '-01', '%Y-%m-%d' ) ) ) %>% 
  dplyr::select( fecha, tasa_pasiva, lim_inf_tasa_pasiva, lim_sup_tasa_pasiva ) %>% 
  mutate( lim_inf_tasa_pasiva = if_else( year( fecha ) == anio_corte & month( fecha ) == 12,
                                         tasa_pasiva,
                                         lim_inf_tasa_pasiva ),
          lim_sup_tasa_pasiva = if_else( year( fecha ) == anio_corte & month( fecha ) == 12,
                                         tasa_pasiva,
                                         lim_sup_tasa_pasiva ) )

aux_his <- aux %>% filter(  fecha <= as.Date( '2022-12-01' ) )
aux_pred <- aux %>% filter( fecha >= as.Date( '2022-12-01' ), year( fecha ) <= anio_fin )

lim_y <- c( 0, 13 )
salto_y = 2
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <-
  paste0( as.character( c( seq( 
    abs( lim_y[1] ), abs( lim_y[2] ), salto_y
  ) ) ), '%' )

iess_tp_pred <- ggplot(  ) +
  geom_line( 
    data = aux_his,
    aes( x = fecha, y = tasa_pasiva, group = 1 ),
    colour = parametros$iess_green,
    linewidth = graf_line_size
  ) +
  geom_ribbon( data = aux_pred,
               aes( x = fecha,
                    ymin = lim_inf_tasa_pasiva,
                    ymax = lim_sup_tasa_pasiva ), 
               fill = 'blue',
               alpha = 0.2 ) +
  geom_line( 
    data = aux_pred,
    aes( x = fecha,
         y = tasa_pasiva, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green,
    linetype = 'dashed'
  ) +
  xlab( '' ) +
  ylab( 'Tasa pasiva referencial' ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_date( 
    breaks = seq( as.Date( '2000-12-01' ), as.Date( paste0( anio_fin,'-12-01' ) ), by = '60 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2000-12-01', paste0( anio_fin,'-12-01' ) ), '%Y-%m-%d' )
  ) +
  theme_bw(  ) +
  plt_theme  +
  theme( axis.text.x = element_text( 
    angle = 90,
    hjust = 0.5,
    vjust = 0.5
  ) )

ggsave( 
  plot = iess_tp_pred,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_tp_pred',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Predicciones IPC----------------------------------------------------------------------------------
message( '\tGraficando IPC' )

aux <- intervalos_confianza %>% 
  mutate( fecha = as.Date( paste0( anio, '-', mes, '-01', '%Y-%m-%d' ) ) ) %>% 
  dplyr::select( fecha, ipc, lim_inf_ipc, lim_sup_ipc ) %>% 
  mutate( lim_inf_ipc = if_else( year( fecha ) == anio_ini & month( fecha ) == 12,
                                 ipc,
                                 lim_inf_ipc ),
          lim_sup_ipc = if_else( year( fecha ) == anio_ini & month( fecha ) == 12,
                                 ipc,
                                 lim_sup_ipc ) )

aux_his <- aux %>% filter(  fecha <= as.Date( '2022-12-01' ) )
aux_pred <- aux %>% filter( fecha >= as.Date( '2022-12-01' ), year( fecha ) <= anio_fin )

lim_y <- c( 40, 220 )
salto_y = 20
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <-
  paste0( as.character( c( seq( 
    abs( lim_y[1] ), abs( lim_y[2] ), salto_y
  ) ) ) )

iess_ipc_pred <- ggplot(  ) +
  geom_line( 
    data = aux_his,
    aes( x = fecha, y = ipc, group = 1 ),
    colour = parametros$iess_green,
    linewidth = graf_line_size
  ) +
  geom_ribbon( data = aux_pred,
               aes( x = fecha,
                    ymin = lim_inf_ipc,
                    ymax = lim_sup_ipc ), 
               fill = 'blue',
               alpha = 0.2 ) +
  geom_line( 
    data = aux_pred,
    aes( x = fecha, y = ipc, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green,
    linetype = 'dashed'
  ) +
  xlab( '' ) +
  ylab( 'IPC' ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_date( 
    breaks = seq( as.Date( '2000-12-01' ), as.Date( paste0( anio_fin,'-12-01' ) ), by = '60 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2000-12-01', paste0( anio_fin,'-12-01' ) ), '%Y-%m-%d' )
  ) +
  theme_bw(  ) +
  plt_theme  +
  theme( axis.text.x = element_text( 
    angle = 90,
    hjust = 0.5,
    vjust = 0.5
  ) )

ggsave( 
  plot = iess_ipc_pred,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_ipc_pred',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Predicciones del rendimiento del BIESS------------------------------------------------------------
message( '\tGraficando rendimiento del BIESS' )

aux <- predicciones_mensuales_biess %>% 
  mutate( fecha = as.Date( paste0( anio, '-', mes, '-01', '%Y-%m-%d' ) ) ) %>% 
  dplyr::select( fecha, rendimiento_biess, lim_inf, lim_sup ) %>% 
  mutate( lim_inf_ipc = if_else( year( fecha ) == anio_ini & month( fecha ) == 12,
                                 rendimiento_biess,
                                 lim_inf ),
          lim_sup_ipc = if_else( year( fecha ) == anio_ini & month( fecha ) == 12,
                                 rendimiento_biess,
                                 lim_sup ) )

aux_his <- aux %>% filter(  fecha <= as.Date( '2022-12-01' ) )
aux_pred <- aux %>% filter( fecha >= as.Date( '2022-12-01' ), year( fecha ) <= anio_fin )

lim_y <- c( 5, 15 )
salto_y = 2
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <-
  paste0( as.character( c( seq( 
    abs( lim_y[1] ), abs( lim_y[2] ), salto_y
  ) ) ), '%' )

biess_rendimiento_pred <- ggplot(  ) +
  geom_line( 
    data = aux_his,
    aes( x = fecha, y = rendimiento_biess, group = 1 ),
    colour = parametros$iess_green,
    linewidth = graf_line_size
  ) +
  geom_ribbon( data = aux_pred,
               aes( x = fecha,
                    ymin = lim_inf,
                    ymax = lim_sup ), 
               fill = 'blue',
               alpha = 0.2 ) +
  geom_line( 
    data = aux_pred,
    aes( x = fecha, y = rendimiento_biess, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_green,
    linetype = 'dashed'
  ) +
  xlab( '' ) +
  ylab( 'Tasa de rendimiento neto del BIESS' ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_date( 
    breaks = seq( as.Date( '2010-12-01' ), as.Date( paste0( anio_fin,'-12-01' ) ), by = '60 months' ),
    date_labels = '%b %Y',
    limits = as.Date( c( '2010-12-01', paste0( anio_fin,'-12-01' ) ), '%Y-%m-%d' )
  ) +
  theme_bw(  ) +
  plt_theme  +
  theme( axis.text.x = element_text( 
    angle = 90,
    hjust = 0.5,
    vjust = 0.5
  ) )

ggsave( 
  plot = biess_rendimiento_pred,
  filename = paste0( 
    parametros$resultado_graficos,
    'biess_rendimiento_pred',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Predicciones inflación----------------------------------------------------------------------------
message( '\tGraficando inflación' )

aux <- predicciones_anuales
aux_his <- aux %>% filter( anio <= anio_corte )
aux_pred <- aux %>% filter( anio >= anio_corte, anio <= anio_fin )
lim_y <- c( -1, 9 )
salto_y = 1
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <-
  paste0( as.character( seq( lim_y[1], lim_y[2], salto_y ) ), '%' )

iess_inf_pred <- ggplot(  ) +
  geom_line( 
    data = aux_his,
    aes( x = anio, y = inf_anual, group = 1 ),
    colour = parametros$iess_green,
    linewidth = graf_line_size
  ) +
  geom_line( 
    data = aux_pred,
    aes( x = anio, y = inf_anual, group = 1 ),
    linewidth = graf_line_size,
    colour = parametros$iess_blue,
    linetype = 'dashed'
  ) +
  xlab( '' ) +
  ylab( 'Tasa inflación acumulada anual promedio' ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = seq( 2000, anio_fin, by = 5 ),
                      breaks = seq( 2000, anio_fin, by = 5 ) ) +
  theme_bw(  ) +
  plt_theme

ggsave( 
  plot = iess_inf_pred,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_inf_pred',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# Gráfico agrupado----------------------------------------------------------------------------------
plt_var_macro_a <-
  marrangeGrob( 
    list( 
      iess_pib_pred,
      iess_sal_pred
    ),
    nrow = 2,
    ncol = 1,
    top = ''
  )

ggsave( 
  plot = plt_var_macro_a,
  filename = paste0( 
    parametros$resultado_graficos,
    'plt_var_macro_a',
    parametros$graf_ext
  ),
  width = 20,
  height = 14,
  units = graf_units,
  dpi = graf_dpi
)

plt_var_macro_b <-
  marrangeGrob( 
    list( 
      iess_sbu_pred,
      iess_tp_pred
    ),
    nrow = 2,
    ncol = 1,
    top = ''
  )

ggsave( 
  plot = plt_var_macro_b,
  filename = paste0( 
    parametros$resultado_graficos,
    'plt_var_macro_b',
    parametros$graf_ext
  ),
  width = 20,
  height = 14,
  units = graf_units,
  dpi = graf_dpi
)

plt_var_macro_c <-
  marrangeGrob( 
    list( 
      iess_ipc_pred,
      iess_inf_pred
    ),
    nrow = 2,
    ncol = 1,
    top = ''
  )

ggsave( 
  plot = plt_var_macro_c,
  filename = paste0( 
    parametros$resultado_graficos,
    'plt_var_macro_c',
    parametros$graf_ext
  ),
  width = 20,
  height = 14,
  units = graf_units,
  dpi = graf_dpi
)

#  Gráfico de predicciones para el SSC--------------------------------------------------------------
plt_pob_var_macro_ssc <-
  marrangeGrob( 
    list( 
      iess_pib_pred,
      iess_sal_pred,
      iess_sbu_pred,
      iess_tp_pred,
      iess_ipc_pred,
      iess_inf_pred
    ),
    nrow = 2,
    ncol = 3,
    top = ''
  )

ggsave( 
  plot = plt_pob_var_macro_ssc,
  filename = paste0( 
    parametros$resultado_graficos,
    'plt_pob_var_macro_ssc',
    parametros$graf_ext
  ),
  width = 24.5,
  height = 12,
  units = graf_units,
  dpi = graf_dpi
)

# 3. Prueba de Independencia de errores de Box-Ljung------------------------------------------------

# 3.1. Prueba de Box-Ljung aumentada multivariante--------------------------------------------------
aux <- as_tibble( box_ljung )

lim_y <- c( 0, 0.9 )
salto_y = 0.1
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )

iess_box_ljung <- ggplot( aux, aes( x = m, y = p_valor ) ) +
  geom_segment( 
    aes( 
      x = m,
      xend = m,
      y = 0,
      yend = p_valor
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = seq( 1, 10, 1 ), breaks = seq( 1, 10, 1 ) ) +
  theme_bw(  ) +
  plt_theme +
  xlab( '' ) +
  ylab( 'p-valor' )

ggsave( 
  plot = iess_box_ljung,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_box_ljung_aumentada',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 3.2. Prueba de Box-Ljung PIB----------------------------------------------------------------------
aux <- as.data.frame( box_ljung_pib ) %>%
  clean_names( . )

lim_y <-
  c( 0, max( ( max( 
    round( aux$p_value * 10, 0 )
  )  + 1 ) / 10, 0.3 ) )
salto_y = 0.1
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )
lbls_x <- seq( 1, max( aux$lags ), 2 )
brks_x <- seq( 1, max( aux$lags ), 2 )

iess_box_ljung_pib <- ggplot( aux, aes( x = lags, y = p_value ) ) +
  geom_segment( 
    aes( 
      x = lags,
      xend = lags,
      y = 0,
      yend = p_value
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = lbls_x, breaks = brks_x ) +
  theme_bw(  ) +
  plt_theme +
  xlab( '' ) +
  ylab( 'p-valor' ) +
  ggtitle( TeX( 'Residuos de $\\nabla$ PIB' ) )

ggsave( 
  plot = iess_box_ljung_pib,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_box_ljung_pib',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 3.3. Prueba de Box-Ljung salarios-----------------------------------------------------------------
aux <- as.data.frame( box_ljung_sal ) %>%
  clean_names( . )

lim_y <-
  c( 0, max( ( max( 
    round( aux$p_value * 10, 0 )
  )  + 1 ) / 10, 0.3 ) )
salto_y = 0.1
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )
lbls_x <- seq( 1, max( aux$lags ), 2 )
brks_x <- lbls_x

iess_box_ljung_sal <- ggplot( aux, aes( x = lags, y = p_value ) ) +
  geom_segment( 
    aes( 
      x = lags,
      xend = lags,
      y = 0,
      yend = p_value
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = lbls_x, breaks = brks_x ) +
  theme_bw(  ) +
  plt_theme +
  xlab( '' ) +
  ylab( 'p-valor' ) + 
  ggtitle( TeX( 'Residuos de $\\nabla$ Salarios Promedio' ) )

ggsave( 
  plot = iess_box_ljung_sal,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_box_ljung_sal',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 3.4. Prueba de Box-Ljung SBU----------------------------------------------------------------------
aux <- as.data.frame( box_ljung_sbu ) %>%
  clean_names( . )

lim_y <-
  c( 0, max( ( max( 
    round( aux$p_value * 10, 0 )
  )  + 1 ) / 10, 0.3 ) )
salto_y = 0.1
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )
lbls_x <- seq( 1, max( aux$lags ), 2 )
brks_x <- lbls_x

iess_box_ljung_sbu <- ggplot( aux, aes( x = lags, y = p_value ) ) +
  geom_segment( 
    aes( 
      x = lags,
      xend = lags,
      y = 0,
      yend = p_value
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = lbls_x, breaks = brks_x ) +
  theme_bw(  ) +
  plt_theme +
  xlab( '' ) +
  ylab( 'p-valor' ) + 
  ggtitle( TeX( 'Residuos de $\\nabla$ SBU' ) )

ggsave( 
  plot = iess_box_ljung_sbu,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_box_ljung_sbu',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 3.4. Prueba de Box-Ljung tasa pasiva--------------------------------------------------------------
aux <- as.data.frame( box_ljung_tp ) %>%
  clean_names( . )

lim_y <-
  c( 0, max( ( max( 
    round( aux$p_value * 10, 0 )
  )  + 1 ) / 10, 0.3 ) )
salto_y = 0.1
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )
lbls_x <- seq( 1, max( aux$lags ), 2 )
brks_x <- lbls_x

iess_box_ljung_tp <- ggplot( aux, aes( x = lags, y = p_value ) ) +
  geom_segment( 
    aes( 
      x = lags,
      xend = lags,
      y = 0,
      yend = p_value
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = lbls_x, breaks = brks_x ) +
  theme_bw(  ) +
  plt_theme +
  xlab( '' ) +
  ylab( 'p-valor' ) + 
  ggtitle( TeX( 'Residuos de $\\nabla$ Tasa pasiva' ) )

ggsave( 
  plot = iess_box_ljung_tp,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_box_ljung_tp',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 3.5. Prueba de Box-Ljung IPC----------------------------------------------------------------------
aux <- as.data.frame( boc_ljung_ipc ) %>%
  clean_names( . )

lim_y <-
  c( 0, max( ( max( 
    round( aux$p_value * 10, 0 )
  )  + 1 ) / 10, 0.3 ) )
salto_y = 0.1
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )
lbls_x <- seq( 1, max( aux$lags ), 2 )
brks_x <- lbls_x

iess_box_ljung_ipc <- ggplot( aux, aes( x = lags, y = p_value ) ) +
  geom_segment( 
    aes( 
      x = lags,
      xend = lags,
      y = 0,
      yend = p_value
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = lbls_x, breaks = brks_x ) +
  theme_bw(  ) +
  plt_theme +
  xlab( '' ) +
  ylab( 'p-valor' ) + 
  ggtitle( TeX( 'Residuos de $\\nabla$ IPC' ) )

ggsave( 
  plot = iess_box_ljung_tp,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_box_ljung_ipc',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 3.5 Gráfico de todas las pruebas de Box-Ljung-----------------------------------------------------
iess_box_ljung_grupo <-
  ggarrange( 
    ggarrange( 
      iess_box_ljung_pib,
      iess_box_ljung_sal,
      iess_box_ljung_sbu,
      ncol = 3 ),
    ggarrange(
      iess_box_ljung_tp,
      iess_box_ljung_ipc,
      ncol = 2
    ),
    nrow = 2
  )

ggsave( 
  plot = iess_box_ljung_grupo,
  filename = paste0( 
    parametros$resultado_graficos,
    'iess_box_ljung_grupo',
    parametros$graf_ext
  ),
  width = 24.5,
  height = 12,
  units = graf_units,
  dpi = graf_dpi
)

# 4. Pruebas de diagnóstico del modelo del BIESS----------------------------------------------------
# 4.1. Prueba de Box-Ljung--------------------------------------------------------------------------
aux <- as_tibble( box_ljung_biess ) %>% 
  clean_names()

lim_y <- c( 0, 1 )
salto_y = 0.2
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )

biess_box_ljung <- ggplot( aux, aes( x = lags, y = p_value ) ) +
  geom_segment( 
    aes( 
      x = lags,
      xend = lags,
      y = 0,
      yend = p_value
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = seq( 1, 20, 2 ), breaks = seq( 1, 20, 2 ) ) +
  theme_bw(  ) +
  plt_theme +
  #xlab( 'm' ) +
  ylab( 'p-valor' )

ggsave( 
  plot = biess_box_ljung,
  filename = paste0( 
    parametros$resultado_graficos,
    'biess_box_ljung',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# 4.2. Prueba de errores cuadráticos----------------------------------------------------------------
aux <- as_tibble( portmanteau_biess ) %>% 
  clean_names()

lim_y <- c( 0, 0.2 )
salto_y = 0.05
brks_y <- seq( lim_y[1], lim_y[2], salto_y )
lbls_y <- paste0( format( seq( lim_y[1], lim_y[2], salto_y ),         
                          nsmall = 1,
                          decimal.mark = ',',
                          big.mark = '.' ) )

biess_portmanteau <- ggplot( aux, aes( x = lags, y = p_value ) ) +
  geom_segment( 
    aes( 
      x = lags,
      xend = lags,
      y = 0,
      yend = p_value
    ),
    color = parametros$iess_blue,
    linetype = 6
  ) +
  geom_hline( yintercept = 0,
              color = 'black',
              size = 0.5 ) +
  geom_hline( 
    yintercept = 0.05,
    color = 'red',
    size = 0.5,
    linetype = 'dashed'
  ) +
  geom_point( color = parametros$iess_green, size = 2 ) +
  scale_y_continuous( breaks = brks_y,
                      labels = lbls_y,
                      limits = lim_y ) +
  scale_x_continuous( labels = seq( 1, 20, 2 ), breaks = seq( 1, 20, 2 ) ) +
  theme_bw(  ) +
  plt_theme +
  #xlab( 'm' ) +
  ylab( 'p-valor' )

ggsave( 
  plot = biess_portmanteau,
  filename = paste0( 
    parametros$resultado_graficos,
    'biess_portmanteau',
    parametros$graf_ext
  ),
  width = graf_width,
  height = graf_height,
  units = graf_units,
  dpi = graf_dpi
)

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'REP', 'slide' ) ) ] )
gc()