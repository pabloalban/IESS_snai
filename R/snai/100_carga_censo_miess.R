message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura de causas de desfinanciamiento' )

#Cargando información financiera--------------------------------------------------------------------
file <-
  paste0( parametros$Data,
          'IESS_28-11-2023.xlsx' )

#Carga de recursos administrados por el BIESS-------------------------------------------------------
censo_miess <- read_excel( 
  file,
  sheet = 'Hoja1',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando base de recicladores de MIESS ' )

save( censo_miess,
  file = paste0( 
    parametros$RData,
    'MIESS_censo_recicladores.RData'
  )
)

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )
