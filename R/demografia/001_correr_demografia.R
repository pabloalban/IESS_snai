# Ejecución Demografía -----------------------------------------------------------------------------
# Script que describe la ejecución de los cálculos para estimaciones demográficas

if ( parametros$anio_ini == 2018 ) {
  
  # Ejecución de análisis demografía 2018 ----------------------------------------------------------
  # Muchos de los scripts aquí citados ya no están presentes, se encuentra respaldados en el 
  # git histórico, esto debido a que muchas funcionalidades y métodos fueron mejorados para los 
  # estudios con corte al 2020.
  ## Lectura de información ------------------------------------------------------------------------
  source( 'R/demografia/101_lectura_onu_life_survivors.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/102_lectura_mortalidad_pensionistas.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/103_lectura_poblacion_inicial.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/104_lectura_tasa_actividad.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/105_lectura_poblacion_jubilada_total.R', encoding = 'UTF-8', echo = FALSE )
  
  source( 'R/demografia/200_interpolacion_onu_pea.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/201_interpolacion_onu_life_survivors.R', encoding = 'UTF-8', echo = FALSE )
  
  ## Análisis demográfico --------------------------------------------------------------------------
  source( 'R/demografia/300_calculo_tabla_decrementos.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/301_estimacion_tasa_entrada.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/302_ajuste_mortalidad_dinamica.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/303_ajuste_mortalidad_pensionistas.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/304_ajuste_tasa_actividad.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/306_probabilidades_transicion.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/309_tablas_mortalidad_estados.R', encoding = 'UTF-8', echo = FALSE )
  
  ## Otros -----------------------------------------------------------------------------------------
  source( 'R/demografia/307_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/310_calibracion_tiempo_servicio.R', encoding = 'UTF-8', echo = FALSE )
  
} else if ( parametros$anio_ini %in% c( 2020, 2023 ) ) {
  
  # Ejecución de análisis demografía 2020 ----------------------------------------------------------
  ## Configuración ---------------------------------------------------------------------------------
  # source( 'R/demografia/002_configurar_demografia.R', encoding = 'UTF-8', echo = FALSE )
  
  ## Carga información -----------------------------------------------------------------------------
  source( 'R/demografia/101_lectura_transiciones_iess.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/106_lectura_pea_inec.R', encoding = 'UTF-8', echo = FALSE )
  
  ## SGO estadísticas y estimaciones ---------------------------------------------------------------
  # script con alto costo de ejecución en memoria y procesamiento
  source( 'R/demografia/102_preparacion_transiciones_iess.R', encoding = 'UTF-8', echo = FALSE )
  
  source( 'R/demografia/103_preparacion_poblacion_inicial.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/204_estadistica_salario_pensiones.R', encoding = 'UTF-8', echo = FALSE )
  
  # script con alto costo de ejecución en memoria y procesamiento
  # source( 'R/demografia/207_modelo_nupcialidad_fertilidad.R', encoding = 'UTF-8', echo = FALSE )
  
  source( 'R/demografia/303_suavizado_tablas.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/305_proyeccion_mortalidad.R', encoding = 'UTF-8', echo = FALSE )
  
  # script con alto costo de ejecución en memoria y procesamiento
  source( 'R/demografia/306_probabilidades_transicion.R', encoding = 'UTF-8', echo = FALSE )
  
  source( 'R/demografia/307_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
  
  ## SSC estadísticas y estimaciones ---------------------------------------------------------------
  source( 'R/demografia/102_preparacion_transiciones_ssc.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/303_suavizado_tablas_ssc.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/305_proyeccion_mortalidad_ssc.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/306_probabilidades_transicion_ssc.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/307_proyeccion_poblacion_ssc.R', encoding = 'UTF-8', echo = FALSE )
  
  ## CES y DES estadísticas y estimaciones ---------------------------------------------------------
  source( 'R/demografia/101_lectura_transiciones_activos_ces_des.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/102_preparacion_transiciones_ces_des.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/demografia/307_proyeccion_beneficiarios_des.R', encoding = 'UTF-8', echo = FALSE ) 
  
  ## RTR estadísticas y estimaciones ---------------------------------------------------------------
  source( 'R/demografia/107_preparacion_transiciones_pensionistas_rtr.R', encoding = 'UTF-8', echo = FALSE ) 
  source( 'R/demografia/205_suavizado_transicion_pensionista_rtr.R', encoding = 'UTF-8', echo = FALSE ) 
  source( 'R/demografia/206_suavizado_pensionistas_iniciales_rtr.R', encoding = 'UTF-8', echo = FALSE ) 
  source( 'R/demografia/308_proyeccion_poblacion_rtr.R', encoding = 'UTF-8', echo = FALSE ) 

}

# Generación reporte -------------------------------------------------------------------------------
source( parametros$dem_rep_gen, encoding = 'UTF-8', echo = FALSE )
