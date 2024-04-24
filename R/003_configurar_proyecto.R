# Parámetros globales R ----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tConfiguración global de R' )

options( scipen = 99 )
setNumericRounding( 2 )
options( stringsAsFactors = FALSE )

# Extensión de operadores --------------------------------------------------------------------------
"%notin%" <- Negate("%in%")

# Parámetros ---------------------------------------------------------------------------------------
message( '\tCreando entorno de parámetros' )

# Entorno con parámetros
parametros <- new.env()

# User name
parametros$user <- Sys.getenv( 'USER' )

parametros$fec_eje <- Sys.Date()

#Operating system name
parametros$opsys <- Sys.info()[[1]]

# Condicional para importar el tipo de letra Linux Libertine en diferentes sistemas operativos
if ( 'Linux Libertine' %notin% font_families() ) {
  if ( parametros$opsys == 'Linux' ) {
    file <- "/usr/share/fonts/opentype/linux-libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
  } else if ( parametros$opsys == 'Windows' ) {
    file <- "C:/Program Files/MiKTeX/fonts/opentype/public/libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
    file <- "C:/Program Files (x86)/MiKTeX/fonts/opentype/public/libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
    file <- "C:/MiKTeX/fonts/opentype/public/libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
  }
}
showtext_auto()

# Hostname
parametros$hostname <- Sys.info()[[4]]

# local
#parametros$data_server <- paste0( getwd(), '/' )

# Directorio de trabajo
parametros$work_dir <- paste0( getwd(), '/' )

#Servidor de datos
parametros$data_server <- 'Y:/IESS_snai/'
parametros$risko_conf <- FALSE
parametros$prod_conf <- TRUE

# Setting Time Zone
parametros$time_zone <- "America/Guayaquil"

# Colores IESS
parametros$iess_blue <- rgb( 0, 63, 138, maxColorValue = 255 )
parametros$iess_green <- rgb( 0, 116, 53, maxColorValue = 255 )
parametros$iess_total <- rgb( 138, 5, 81, maxColorValue = 255 )
parametros$col_female <- rgb( 255, 153, 204, maxColorValue = 255 )
parametros$col_male <- rgb( 51, 102, 204, maxColorValue = 255 )

# Calcular balance
parametros$calcular_balance <- FALSE
parametros$mont_prop_afi <- 0.1275

# Direcciones globables  ---------------------------------------------------------------------------
message( '\tEstableciendo directorios globales' )

parametros$empresa <- 'IESS'

parametros$edad_max <- 105
parametros$anio_ini <- 2020
parametros$anio <- 2021 # Año del estudio

# Direcciones globales
parametros$Data <- paste0( parametros$data_server, 'Data/' )
parametros$RData <- paste0( parametros$data_server, 'RData/' )
parametros$SQL <- paste0( parametros$data_server, 'SQL/' )
parametros$RSQL <- paste0( parametros$data_server, 'RSQL/' )

# Direcciones para datos demográficos
parametros$RData_dem <- paste0( parametros$RData, 'demografia/' )
parametros$Data_dem <- paste0( parametros$Data, 'demografia/' )

# Direcciones para datos macroeconómicos
parametros$RData_macro <- paste0( parametros$RData, 'macro/' )
parametros$Data_macro <- paste0( parametros$Data, 'macro/' )

# Direcciones para datos morbilidad
parametros$RData_morb <- paste0( parametros$RData, 'morbilidad/' )
parametros$Data_morb <- paste0( parametros$Data, 'morbilidad/' )

if ( parametros$risko_conf ) { 
  
  source( 'R/006_conf_risko.R', encoding = 'UTF-8', echo = FALSE )
  
}

# Configuraciones particulares por seguro ----------------------------------------------------------
message( '\tConfiguración' )
#Dem corresponde a demografía
#Mor corresponde a mortalidad
#EPI
parametros$seguro <- 'SNAI'

parametros$conf_seg <- paste0( parametros$work_dir, 'R/', tolower( parametros$seguro ), '/', 
                                 '002_configuracion_seguro.R' )

source( parametros$conf_seg, encoding = 'UTF-8', echo = FALSE )

parametros$exe_seg <- paste0( parametros$work_dir, 'R/', tolower( parametros$seguro ), '/', 
                              '001_correr_' , tolower( parametros$seguro ) ,'.R' )

# Configuración de gráficos-------------------------------------------------------------------------
parametros$graf_template <- 'R/401_graf_plantilla.R'
parametros$graf_ext <- '.pdf'

# Variables automáticas ----------------------------------------------------------------------------
parametros$Data_seg <- paste0( parametros$Data, parametros$seguro, '/' )
parametros$RData_seg <- paste0( parametros$RData, parametros$seguro, '/' )
parametros$SQL_seg <- paste0( parametros$SQL, parametros$seguro, '/' )
parametros$RSQL_seg <- paste0( parametros$RSQL, parametros$seguro, '/' )
parametros$Driver <- paste0( parametros$data_server, 'Drivers/oracle', '/' )

parametros$reportes <- paste0( parametros$work_dir, 'Reportes/' )
parametros$resultados <- paste0( parametros$work_dir, 'Resultados/' )

if ( parametros$risko_conf ){
  parametros$reporte_seguro <- paste0( parametros$work_dir, 'Reportes/Reporte_', 
                                       parametros$seguro, '/' )
} else {
  parametros$reporte_seguro <- paste0( parametros$work_dir, 'Reportes/Reporte_', 
                                       toupper( parametros$seguro ), '/' )
}
parametros$reporte_script <- paste0( parametros$reporte_seguro, 'reporte.R' )
parametros$reporte_nombre <- paste0( parametros$empresa, '_', 
                                     parametros$seguro, '_estudio_actuarial' )
parametros$reporte_latex <- paste0( parametros$reporte_nombre, '.tex' )
parametros$resultado_seguro <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/' )
parametros$resultado_tablas <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/tablas/' )
parametros$resultado_graficos <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                         format( parametros$fec_eje, '%Y_%m_%d' ), '/graficos/' )

parametros$rdata_sgo_coef_pen <- paste0( parametros$RData, 'IVM/', 'IESS_IVM_coeficientes_pensiones.RData' )

# Función para preparar tasas en cada estudio ------------------------------------------------------
parametros$prepara_tasas <- function( dat, tas_list ) {
  for ( i in 1:length( tas_list ) ) {
    expr <- expression({
      dat[ , u_N := i_N ]
      dat[ t == 0, u_N := 0 ]
      dat[ , u_N := cumprod( 1 + u_N ) ]
      dat[ , v_N := 1 / u_N ]  
    })
    expr <- gsub( '(N)', tas_list[ i ], deparse( expr ) )
    eval( eval( parse( text = expr ) ) )  
  }
  return( dat )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

