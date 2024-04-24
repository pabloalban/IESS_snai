# Ejecución RTR ------------------------------------------------------------------------------------
source( 'R/rtr/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )

#Cargando datos-------------------------------------------------------------------------------------
source( 'R/rtr/100_lectura_rentas_2022_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/101_lectura_indemnizacion_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/102_lectura_subsidios_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Preparando datos----------------------------------------------------------------------------------
source( 'R/demografia/107_preparacion_transiciones_pensionistas_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Filtros e imputación de datos --------------------------------------------------------------------
source( 'R/demografia/205_suavizado_transicion_pensionista_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/206_suavizado_pensionistas_iniciales_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/201_suavizado_porcentaje_incapacidad_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/202_suavizado_pensiones_iniciales_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Estadísticas y estimaciones ----------------------------------------------------------------------
source( 'R/rtr/200_estadisticas_descriptivas_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Proyección de población beneficiaria--------------------------------------------------------------
source( 'R/demografia/308_proyeccion_poblacion_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Cálculos de escenarios ---------------------------------------------------------------------------
source( 'R/rtr/302_calculo_escenarios_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/303_calculo_prima_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Generación reporte -------------------------------------------------------------------------------
source( parametros$rtr_rep_gen, encoding = 'UTF-8', echo = FALSE )
