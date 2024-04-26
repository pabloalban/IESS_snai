# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

#Lectura de bases de datos--------------------------------------------------------------------------
source( 'R/snai/100_cargar_base_snai.R', encoding = 'UTF-8', echo = FALSE )

# Estadísticas descriptivas-------------------------------------------------------------------------
source( 'R/snai/200_estadisticas_descriptivas_snai.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de REC ----------------------------------------------------------------------
source('R/snai/400_graf_analisis_demografico_snai.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de REC ------------------------------------------------------------------------
source( 'R/snai/500_tablas_demograficas_snai.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

