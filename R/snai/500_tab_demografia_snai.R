message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura tabla información finaciera ' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# 1. Activo-----------------------------------------------------------------------------------------
message( '\tLectura activo del fondo' )
aux <- activo_del_fondo %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_activo_del_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

## Análisis Componentes del Activo------------------------------------------------------------------
message( '\tTabla Análisis Componentes del Activo' )
aux <- analisis_componentes_activo %>% 
  clean_names() %>%
  dplyr::select( -x2010,
                 -x2011 )
  
aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_componentes_activo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )

##Análisis Horizontal Activo------------------------------------------------------------------------
message( '\tTabla Análisis Horizontal Activo' )
aux <- analisis_horizontal_activo %>% 
  clean_names() %>%
  dplyr::select( -x2011_2010,
                 -x2012_2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_activo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1,
                       nrow(aux)),
       sanitize.text.function = identity )

##Análisis vertical del activo----------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Activo' )
aux <- analisis_vertical_activo %>% 
  clean_names() %>%
  dplyr::select( -x2010,
                 -x2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_activo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1,
                       nrow(aux)),
       sanitize.text.function = identity )

##Cuentas por Cobrar Fondo RT-----------------------------------------------------------------------
message( '\tTabla Análisis Cuentas por Cobrar' )
aux <- analisis_componentes_cobrar_fondo %>%
  clean_names( ) 

aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 9) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'analisis_componentes_cobrar_fondo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )

#2. Pasivos-----------------------------------------------------------------------------------------
message( '\tTabla Pasivos del Fondo' )
aux <- pasivos_fondo %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0,0,2,2,2))

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_pasivos_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

##Componentes del Pasivo del Fondo------------------------------------------------------------------
message( '\tTabla Componentes del Pasivo del Fondo' )
aux <- componentes_pasivos_fondo %>%
  clean_names( ) %>%
  dplyr::select( -x2010,
                 -x2011 ) %>%
  mutate_if( is.numeric, ~replace_na(., 0))

aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 9 ) ) )
print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_pasivos_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux)-1,
       sanitize.text.function = identity )

## Análisis Horizontal del Pasivo--------------------------------------------------------------------
message( '\tTabla Análisis Horizontal del Pasivo' )
aux <- analisis_horizontal_pasivos %>%
  clean_names( ) %>%
  dplyr::select( -x2011_2010,
                 -x2012_2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 8) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_pasivos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

## Análisis Vertical del Pasivo----------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Pasivo' )

aux <- analisis_vertical_pasivos %>%
  clean_names( ) %>%
  dplyr::select( -x2010_percent,
                 -x2011_percent ) %>%
  mutate_if( is.numeric, ~replace_na(., 0))

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_pasivos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )

## Cuentas por Pagar del Fondo-----------------------------------------------------------------------
message( '\tTabla Cuentas por Pagar del Fondo' )
aux <- cuentas_pagar_fondo %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0,0, rep( 2, 3 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_pagar_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#3. Patrimonio del Fondo----------------------------------------------------------------------------
message( '\tTabla Patrimonio del Fondo' )
aux <- patrimonio_fondo %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 3 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

## Componentes del Patrimonio del Fondo--------------------------------------------------------------
message( '\tTabla Componentes del Patrimonio del Fondo' )
aux <- componentes_patrimonio_fondo %>%
  clean_names( ) %>%
  dplyr::select( -x2010,
                 -x2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 9) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_patrimonio_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux)-1,
       sanitize.text.function = identity )

## Análisis Horizontal del Patrimonio----------------------------------------------------------------
message( '\tTabla  Análisis Horizontal del Patrimonio' )
aux <- analisis_horizontal_patrimonio %>%
  clean_names( ) %>%
  dplyr::select( -x2011_2010,
                 -x2012_2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_patrimonio_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,
                       nrow( aux ) ),
       sanitize.text.function = identity )

## Análisis Vertical del Patrimonio------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Patrimonio' )
aux <- analisis_vertical_patrimonio %>%
  clean_names( ) %>%
  dplyr::select( -x2010,
                 -x2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 9) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_patrimonio_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,
                       nrow( aux ) ),
       sanitize.text.function = identity )

#4. Ingresos del Fondo------------------------------------------------------------------------------
message( '\tTabla Ingresos del Fondo' )
aux <- ingresos_fondo %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 3) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_fondo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

## Componentes de los Ingresos----------------------------------------------------------------------
message( '\tTabla Componentes de los Ingresos' )
aux <- componentes_ingresos %>%
  clean_names( ) %>%
  dplyr::select( -x2010,
                 -x2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2,9) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1,
                       nrow(aux) ),
       sanitize.text.function = identity )

## Análisis Horizontal del Ingreso------------------------------------------------------------------
message( '\tTabla Análisis Horizontal del Ingreso' )
aux <- analisis_horizontal_ingresos %>%
  clean_names( ) %>%
  dplyr::select( -x2011_2010,
                 -x2012_2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1,
                       nrow(aux) ),
       sanitize.text.function = identity )

## Análisis Vertical del Ingreso--------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Ingreso' )

aux <- analisis_vertical_ingresos %>%
  clean_names( ) %>%
  dplyr::select( -x2010,
                 -x2011 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_ingresos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1,
                       nrow(aux) ),
       sanitize.text.function = identity )

## Ingresos por Aportes-----------------------------------------------------------------------------
message( '\tTabla Ingresos por Aportes' )
aux <- ingresos_aportes %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_aportes_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


#5.Gastos-------------------------------------------------------------------------------------------
message( '\tTabla Gastos' )
aux <- gastos %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0,0, rep( 2, 3 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

##Componentes del Gasto------------------------------------------------------------------------------
message( '\tTabla Componentes del Gasto' )

aux <- componentes_gastos %>%
  clean_names( ) %>%
  dplyr::select( -x2010,
                 -x2011 ) %>%
  filter( x2012 != 0 | x2020 != 0 )


aux_xtab <- xtable( aux, digits = c( 0,0, rep(2, 9 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_componentes_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

##Análisis Horizontal del Gasto----------------------------------------------------------------------
message( '\tTabla Análisis Horizontal del Gasto' )

aux <- analisis_horizontal_gastos %>%
  clean_names( ) %>%
  dplyr::select( -x2011_2010,
                 -x2012_2011 ) %>%
  filter( !is.na( x2020_2019!= 0 | x2013_2012!=0  ) )
  
aux_xtab <- xtable( aux, digits = c( 0,0, rep(2,8)))

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_horizontal_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1,
                       nrow( aux ) ),
       sanitize.text.function = identity )

##Análisis Vertical del Gasto------------------------------------------------------------------------
message( '\tTabla Análisis Vertical del Gasto' )
aux <- analisis_vertical_gastos %>%
  clean_names( ) %>%
  dplyr::select( -x2010,
                 -x2011 ) %>%
  filter( x2012 != 0 | x2020 != 0 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_analisis_vertical_gastos_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )

#6.Resultado del ejercicio--------------------------------------------------------------------------
message( '\tTabla resultado del ejercicio' )
aux <- ingresos_vs_gastos %>%
  clean_names( ) %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )


print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_resultado_ejercicio_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )

# Limpiar RAM---------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

