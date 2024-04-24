# Lectura de la PEA reportada por el INEC - EDEMNU y proyectada ------------------------------------

file <- paste0( parametros$Data, 'demografia/PEA_INEC_20218_2022.xlsx' )
PEA_inec <- read_xlsx( path = file, sheet = 'Proyec', skip = NULL, range = 'A1:E53', 
                       col_names = TRUE )
PEA_inec <- as.data.table( PEA_inec )
setnames( PEA_inec, c( 'anio', 'pea', 'pea_h', 'pea_m', 'tipo' ) )

message( '\tGuardando PEA - INEC' )
save( PEA_inec, file = parametros$demo_rdata_inec_pea )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
