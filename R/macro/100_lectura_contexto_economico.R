message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura del contexto económico' )

# Cargando información del contexto económico ------------------------------------------------------
file<-paste0(parametros$Data, 'IESS_contexto_economico.xlsx' )

inflacion <- read_excel(file,
                        sheet = 'inflacion',
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0) %>% clean_names( ) %>%
  mutate( periodo = as.Date( paste0( "31/", mes, "/", anio ), "%d/%m/%Y") )
  

desempleo <- read_excel( file,
                         sheet = 'desempleo',
                         col_names = TRUE,
                         col_types = NULL,
                         na = "",
                         skip = 0 ) %>% clean_names( ) %>%
  mutate( periodo = as.Date( paste0( "01/", mes, "/", anio ), "%d/%m/%Y") )


pib_real <- read_excel(file,                                  
                       sheet = 'pib_real',
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names() %>%
  dplyr::select( anio,
                 pib := pib_us_a_precios_actuales,
                 pib_constantes := pib_umn_a_precios_constantes,
                 crecimiento_pib )


pib_trimestral <- read_excel(file,                                  
                             sheet = 'pib_trimestral',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names() 

sbu <- read_excel(file,
                  sheet = 'sbu',
                  col_names = TRUE,
                  col_types = NULL,
                  na = "",
                  skip = 0) %>% clean_names()

salarios <- read_excel( file,
                        sheet = 'salarios',
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names( ) %>%
  mutate( periodo = as.Date( paste0( "01/", mes, "/", anio ), "%d/%m/%Y") )

incre_pensiones <- read_excel( file,
                               sheet = 'incre_pensiones',
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0 ) %>% clean_names( )

pension_min <- read_excel( file,
                           sheet = 'pension_min',
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0 ) %>% clean_names( )

pension_max <- read_excel( file,
                           sheet = 'pension_max',
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0 ) %>% clean_names( ) 


tasas_interes <- read_excel( file,
                             sheet = 'tasas_interes',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0 ) %>% clean_names( ) %>%
  mutate( periodo = as.Date( paste0( "01/", mes, "/", anio ), "%d/%m/%Y") ) %>%
  mutate( tasa_activa = 100 * tasa_activa,
          tasa_pasiva = 100 * tasa_pasiva,
          spread = 100 * spread)


roe <- read_excel( file,
                   sheet = 'roe',
                   col_names = TRUE,
                   col_types = NULL,
                   na = "",
                   skip = 0 ) %>% clean_names( )

roa <- read_excel( file,
                   sheet = 'roa',
                   col_names = TRUE,
                   col_types = NULL,
                   na = "",
                   skip = 0 ) %>% clean_names( )


rendimiento_biess <- read_excel( file,
                                 sheet = 'rendimiento_biess',
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = "",
                                 skip = 0 ) %>% clean_names( ) %>%
  mutate( fecha = as.Date( fecha, "%Y/%m/%d" ) )



tasas_macro <- read_excel( file,
                                 sheet = 'datos_modelo',
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = "",
                                 skip = 0 ) %>% clean_names( )

# Guardando en un Rdata ----------------------------------------------------------------------------
message( '\tGuardando en un solo data.frame' )

save( inflacion,
      desempleo,
      pib_real,
      pib_trimestral,
      sbu,
      salarios,
      incre_pensiones,
      pension_min,
      pension_max,
      tasas_interes,
      roe,
      roa,
      rendimiento_biess,
      tasas_macro,
      file = parametros$macro_rdata_info )

# Borrando data.frames -----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
