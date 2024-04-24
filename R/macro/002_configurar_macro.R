message( paste( rep('-', 100 ), collapse = '' ) )

# Configuración específica para modelo macroeconómico ----------------------------------------------
message( '\tEstableciendo parámetros de configuración para modelo macroeconómico' )

# Ubicación de información -------------------------------------------------------------------------
message( '\tEstableciendo nombre de objetos de entrada y salida para modelo macroeconómico' )

parametros$macro_rdata_info <- paste0( parametros$RData_macro, 'IESS_contexto_economico.RData' )
parametros$macro_rdata_series_int <- paste0( parametros$RData_macro, 'IESS_series_macro_interpolacion.RData' )
parametros$macro_rdata_macro_est <- paste0( parametros$RData_macro, 'IESS_macro_estudio.RData' )
parametros$macro_rdata_biess_proy <- paste0( parametros$RData_macro, 'BIESS_proy_tasa_rendimiento.RData' )

message( paste( rep('-', 100 ), collapse = '' ) )
