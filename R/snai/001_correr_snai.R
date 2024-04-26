# Ejecución RTR ------------------------------------------------------------------------------------
source( 'R/rtr/002_configuracion_seguro.R', encoding = 'UTF-8', echo = FALSE )

#Cargando datos-------------------------------------------------------------------------------------
source( 'R/snai/100_carga_base_snai.R', encoding = 'UTF-8', echo = FALSE )

# Estadísticas y estimaciones ----------------------------------------------------------------------
source( 'R/rtr/200_estadisticas_descriptivas_snai.R', encoding = 'UTF-8', echo = FALSE )

# Generación reporte -------------------------------------------------------------------------------
source( parametros$rtr_rep_gen, encoding = 'UTF-8', echo = FALSE )
