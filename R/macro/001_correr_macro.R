# Ejecución del modelo macro económico -------------------------------------------------------------
# Script que describe los cálculos para obtener las proyecciones de las hipótesis monocromáticas

source( 'R/macro/002_configurar_macro.R', encoding = 'UTF-8', echo = FALSE )

# Lectura de las tasas macro históricas ------------------------------------------------------------
source( 'R/macro/100_lectura_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Interpolación a datos mensuales o perdidos -------------------------------------------------------
source( 'R/macro/200_interpolacion_series.R', encoding = 'UTF-8', echo = FALSE )

# Modelo de series multivariantes para tasas macro -------------------------------------------------
source( 'R/macro/201_hipotesis_macro.R', encoding = 'UTF-8', echo = FALSE )

# Modelo de series de tiempo para tasa de rendimiento del BIESS ------------------------------------
source( 'R/macro/202_proy_rendimiento_BIESS.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos del capitulo de análisis de contexto ----------------------------------------------------
#source( 'R/macro/400_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Tablas del capitulo de análisis de contexto ------------------------------------------------------
#source( 'R/macro/500_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
