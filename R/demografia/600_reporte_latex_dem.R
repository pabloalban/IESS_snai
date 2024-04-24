# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/002_configurar_demografia.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos -----------------------------------------------------------------------------
source( 'R/demografia/400_graf_demografia_dem.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/402_graficos_tasas.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas -------------------------------------------------------------------------------
source( 'R/demografia/500_tab_demografia_dem.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/demografia/501_tab_tasas.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )
