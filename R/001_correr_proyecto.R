# Detalle de secciones -----------------------------------------------------------------------------
# 0X Configuraciones iniciales
# 1X carga de información e imputación
# 2X Estimaciones y estadísticas
# 3X Selección proyecciones y cálculo reservas
# 4X Resultados y gráficos
# 5X Tablas
# 6X Reportes LaTeX y excel
# 7x Pruebas y verificaciones adicionales

# Parámetros y carga -------------------------------------------------------------------------------
# Configuraciones globales y comunes a todos los cálculos de los seguros

# Carga de paquetes
source( 'R/002_cargar_paquetes.R', encoding = 'UTF-8', echo = FALSE )

# Configuración global del proyecto
source( 'R/003_configurar_proyecto.R', encoding = 'UTF-8', echo = FALSE )

# Configuración global de demografía
source( 'R/demografia/002_configurar_demografia.R', encoding = 'UTF-8', echo = FALSE )

# Configuración global del modelo macroeconómico
source( 'R/macro/002_configurar_macro.R', encoding = 'UTF-8', echo = FALSE )