message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura de datos SNAI' )

#Cargando información financiera--------------------------------------------------------------------
file <-
  paste0( parametros$Data,
          'SERVIDORES CSVP.xlsx' )

load( paste0( parametros$RData, "RC_informacion.RData" ) )

#Carga de recursos administrados por el BIESS-------------------------------------------------------
snai <- read_excel( 
  file,
  sheet = 'Hoja1',
  range = "a7:m2835",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

sal_snai <- read_excel(
  file, 
  sheet = 'Hoja2',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )


#Arreglo de cédulas---------------------------------------------------------------------------------

snai <- snai %>% 
  mutate( cedula = as.character( cc ) ) %>% 
  mutate( cedula = if_else( nchar( cedula ) == 9,
                            paste0( 0, cedula ),
                            cedula ) ) 

#Cruce de base de datos-----------------------------------------------------------------------------

snai <- snai %>% 
  lazy_dt( ) %>% 
  left_join( ., rc, by = 'cedula' ) %>% 
  as_tibble( ) %>% 
  mutate( x = as.integer( round( difftime( as.Date("2024-02-23"), fecha_nacimiento, units = "days") / 365.25, 0 ) ) )

snai <- snai %>% 
  lazy_dt( ) %>% 
  left_join( ., sal_snai, by = 'denominacion_puesto_unificado' ) %>% 
  as_tibble( )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando base de agentes snai' )

save( snai,
      file = paste0( 
        parametros$RData,
        'IESS_agentes_snai.RData'
      )
)

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )
