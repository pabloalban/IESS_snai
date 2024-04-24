message( paste( rep( '-', 100 ), collapse = '' ) )

# Configuración específica para demografía ---------------------------------------------------------
message( '\tEstableciendo parámetros de configuración para demografía' )

parametros$demo_lect_tran <- TRUE
parametros$demo_prep_tran <- TRUE
parametros$demo_years_analisis <- 2012:2022
parametros$demo_horizonte <- 40
parametros$demo_ts_max <- 70
parametros$demo_edad_max <- parametros$edad_max

parametros$dem_fec_fin <- ymd( '2020-12-31' )

if ( parametros$prod_conf ) {
  
  parametros$demo_lectura_filas <- Inf
  
} else {
  
  parametros$demo_lectura_filas <- 1e5
  
}

# Ubicación de información -------------------------------------------------------------------------
message( '\tEstableciendo nombre de objetos de entrada y salida para demografía' )

## Objetos SGO -------------------------------------------------------------------------------------
## Objetos de origen, pueden ser en varios formatos .RData, .txt, .csv, .dsv, .xlsx, .dbi, etc -----
message( '\tEstableciendo nombre de objetos de entrada y salida para demografía para el SGO' )

parametros$demo_data_sgo_act_tran <- paste0( parametros$Data_dem,'IESS_SGO_transiciones_activos_2012_2022_V6.dsv' )
parametros$demo_data_sgo_inac <- paste0( parametros$Data_dem, 'datos_ivm.RData' )
parametros$demo_data_sgo_pen_tran <- paste0( parametros$Data_dem, 'IESS_SGO_transiciones_pensionistas_2012_2022_V3.dsv' )
parametros$demo_data_ssc_pen_tran <- paste0( parametros$Data_dem, 'IESS_SSC_transiciones_pensionistas_2012_2022_V1.txt' )

parametros$demo_rdata_sgo_tran <- paste0( parametros$RData_dem, 'IESS_SGO_transiciones_activos_pensionistas_2012_2022.RData' )
parametros$demo_rdata_sgo_tran_prep <- paste0( parametros$RData_dem, 'IESS_SGO_transiciones_activos_pensionistas_preparado_2012_2022.RData' )
parametros$demo_rdata_sgo_tasas_tran <- paste0( parametros$RData_dem,'IESS_SGO_tasas_transicion_preparadas_2012_2022.RData' )
parametros$demo_rdata_sgo_incom_tran_act_anio <- paste0( parametros$RData_dem, 'IESS_SGO_transiciones_activos_preparado_' )
parametros$demo_rdata_sgo_incom_tran_pen_anio <- paste0( parametros$RData_dem, 'IESS_SGO_transiciones_pensionistas_preparado_' )

parametros$demo_rdata_sgo_probs_tran <- paste0( parametros$RData_dem, 'IESS_SGO_probabilidades_transicion_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_sgo_probs_tran_ts <- paste0( parametros$RData_dem, 'IESS_SGO_probabilidades_transicion_edad_tiempo_servicio_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_sgo_pob_ini <- paste0( parametros$RData_dem, 'IESS_SGO_poblacion_inicial_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_sgo_din_dec <- paste0( parametros$RData_dem, 'IESS_SGO_tabla_mortalidad_dinamica_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_sgo_pob_proy <- paste0( parametros$RData_dem, 'IESS_SGO_proyeccion_poblacion_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_sgo_est_dem <- paste0( parametros$RData_dem, 'IESS_SGO_estadisticas_salarios_pensiones_', parametros$anio_ini, '.RData' )
#parametros$demo_rdata_inec_fert_model <- paste0( parametros$RData_dem, 'INEC_censo_iess_fertilidad_alisado_2010.RData' )
parametros$demo_rdata_inec_censo <- paste0( parametros$RData_dem, 'INEC_censo_poblacion_2010.RData' )
parametros$demo_rdata_inec_fert_model_todo <- paste0( parametros$RData_dem, 'INEC_censo_iess_fertilidad_alisado_todo_2020.RData' )
parametros$demo_rdata_inec_fert_model <- paste0( parametros$RData_dem, 'INEC_censo_iess_fertilidad_alisado_2020.RData' )

parametros$demo_rdata_inec_pea <- paste0( parametros$RData_dem, 'INEC_PEA_proyectada.RData' ) 
parametros$demo_rdata_sgo_pea_proj <- paste0( parametros$RData_dem, 'IESS_ECU_pea_proj.RData' )
# parametros$demo_rdata_sgo_pea_proj <- paste0( parametros$RData_dem, 'IESS_onu_pea_ecu_int.RData' )
parametros$demo_rdata_onu_int_life_tab <- paste0( parametros$RData_dem, 'ONU_interpolado_life_table_survivors_2019.RData' ) 
parametros$demo_rdata_onu_pob_proy <- paste0( parametros$RData_dem, 'ONU_proyeccion_poblacion.RData' ) 
parametros$demo_rdata_sgo_est_sal_pen_auto_info <- paste0( parametros$RData_dem, 'IESS_SGO_estadísticas_auto_informacion.RData' ) 
parametros$demo_data_afi_pen_dic <- paste0( parametros$Data_dem, 'IESS_SGO_afi_pen_dic_2020.xlsx' ) 
parametros$demo_rdata_afi_pen_dic_prep <- paste0( parametros$RData_dem, 'IESS_SGO_afi_pen_dic_2020_prep.RData' ) 

## Objetos SSC -------------------------------------------------------------------------------------
message( '\tEstableciendo nombre de objetos de entrada y salida para demografía para el SSC' )

parametros$demo_data_ssc_act_tran <- paste0( parametros$Data_dem,'IESS_SSC_transiciones_activos_2012_2022.dsv' )
parametros$demo_data_ssc_inac <- paste0( parametros$Data_dem, 'datos_ivm.RData' )
parametros$demo_data_ssc_pen_tran <- paste0( parametros$Data_dem, 'IESS_SSC_transiciones_pensionistas_2012_2022.dsv' )

parametros$demo_rdata_ssc_tran <- paste0( parametros$RData_dem, 'IESS_SSC_transiciones_activos_pensionistas_2012_2022.RData' )
parametros$demo_rdata_ssc_tran_prep <- paste0( parametros$RData_dem, 'IESS_SSC_transiciones_activos_pensionistas_preparado_2012_2022.RData' )
parametros$demo_rdata_ssc_tasas_tran <- paste0( parametros$RData_dem,'IESS_SSC_tasas_transicion_preparadas_2012_2022.RData' )
parametros$demo_rdata_ssc_incom_tran_act_anio <- paste0( parametros$RData_dem, 'IESS_SSC_transiciones_activos_preparado_' )
parametros$demo_rdata_ssc_incom_tran_pen_anio <- paste0( parametros$RData_dem, 'IESS_SSC_transiciones_pensionistas_preparado_' )

parametros$demo_rdata_ssc_probs_tran <- paste0( parametros$RData_dem, 'IESS_SSC_probabilidades_transicion_', parametros$anio_ini, '.RData' )
# parametros$demo_rdata_sgo_probs_tran_ts <- paste0( parametros$RData_dem, 'IESS_SGO_probabilidades_transicion_edad_tiempo_servicio_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_ssc_pob_ini <- paste0( parametros$RData_dem, 'IESS_SSC_poblacion_inicial_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_ssc_din_dec <- paste0( parametros$RData_dem, 'IESS_SSC_tabla_mortalidad_dinamica_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_ssc_pob_proy <- paste0( parametros$RData_dem, 'IESS_SSC_proyeccion_poblacion_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_ssc_est_dem <- paste0( parametros$RData_dem, 'IESS_SSC_estadisticas_salarios_pensiones_', parametros$anio_ini, '.RData' )
parametros$demo_rdata_ssc_sal_pen_proy <- paste0( parametros$RData_dem, 'IESS_SSC_proyeccion_salarios_', parametros$anio_ini, '.RData' )
# parametros$demo_rdata_inec_fert_model <- paste0( parametros$RData_dem, 'INEC_censo_iess_fertilidad_alisado_2010.RData' )
# parametros$demo_rdata_inec_pea_rurL <- paste0( parametros$RData_dem, 'INEC_PEA_proyectada.RData' ) #Cambiar a la pea rural
parametros$demo_rdata_ssc_pea_rural_proj <- paste0( parametros$RData_dem, 'IESS_ECU_pea_rural_proj.RData' )
# # parametros$demo_rdata_sgo_pea_proj <- paste0( parametros$RData_dem, 'IESS_onu_pea_ecu_int.RData' )
# parametros$demo_rdata_onu_int_life_tab <- paste0( parametros$RData_dem, 'ONU_interpolado_life_table_survivors_2019.RData' ) 
# parametros$demo_rdata_onu_pob_proy <- paste0( parametros$RData_dem, 'ONU_proyeccion_poblacion.RData' ) 

parametros$coef_inv_ssc <- 0.5
# Función con condiciones de eligibilidad ----------------------------------------------------------
parametros$elig_vej_ssc <- function( s, x ) {
  e <- 10
  if ( x >= 65 & x < 71 ) {
    e <- 10
  } else if ( x == 71) {
    e <- 9
  } else if ( x == 72 ) {
    e <- 8
  } else if ( x == 73 ) {
    e <- 7
  } else if ( x == 74 ) {
    e <- 6
  } else if ( x >= 75 ) {
    e <- 5
  }
  e <- as.numeric( e <= s )
  return( e )
}

parametros$elig_vej <- function( s, x ) {
  e <- 40
  if ( x < 60 ) {
    e <- 40
  } else if ( x >= 60 & x < 65 ) {
    e <- 30
  } else if ( x >= 65 & x < 70 ) {
    e <- 15
  } else if ( x >= 70 ) {
    e <- 10
  }
  e <- as.numeric( e <= s )
  return( e )
}

parametros$elig_inv <- function( s, x ) {
  e <- as.numeric( ( s >= 5 ) & ( ( x - s ) >= 15 ) )
  return( e )
}

# Ubicación de información CES DES -----------------------------------------------------------------
message( '\tEstableciendo nombre de objetos de entrada y salida para demografía para el CES DES' )

parametros$demo_data_ces_des_act_tran <- paste0( parametros$Data_dem,'IESS_CES_DES_transiciones_activos_2012_2022.dsv' )
parametros$demo_rdata_ces_des_tran <- paste0( parametros$RData_dem, 'IESS_CES_DES_transiciones_activos_pensionistas_2012_2022.RData' )
parametros$demo_rdata_ces_des_tran_prep <- paste0( parametros$RData_dem, 'IESS_CES_DES_transiciones_activos_pensionistas_preparado_2012_2022.RData' )
parametros$demo_rdata_des_pob_proy <- paste0( parametros$RData_dem, 'IESS_DES_proyeccion_poblacion_', parametros$anio_ini, '.RData' )


# Reporte-------------------------------------------------------------------------------------------
parametros$dem_rep_gen <- paste0( parametros$work_dir, 'R/demografia/600_reporte_latex_dem.R' )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
