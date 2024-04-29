message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura tabla información finaciera ' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_SNAI_tablas_demografia.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla de servidores por edad y sexo-------------------------------------------------------------

message( '\tTabla de servidores por edad y sexo' )
aux <- rang_edad_sexo %>%
  mutate( sexo_F = as.integer(sexo_F), 
          sexo_M = as.integer(sexo_M),
          total = as.integer(total) )

aux_xtab <- xtable( aux)

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_snai_rang_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )


#Tabla de servidores por grado y sexo------------------------------------------------------

message( '\tTabla de servidores por grado y sexo' )
aux <- grado_sexo %>%
  mutate( F = as.integer(F), 
          M = as.integer(M),
          total = as.integer(total) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_snai_grado_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )

#Tabla de servidores por provincia y sexo---------------------------------------------------------

message( '\tTabla de reclicladores por provincia y sexo' )
aux <- prov_sexo %>%
  mutate( F = as.integer(F), 
          M = as.integer(M),
          total = as.integer(total) )

aux_xtab <- xtable( aux)

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_snai_prov_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )


#Tabla de salarios por edad y sexo-----------------------------------------------------

message( '\tTabla de salarios por edad y sexo' )
aux <- edad_sal %>%
  mutate( F = as.integer(F), 
          M = as.integer(M),
          total = as.integer(total) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_snai_edad_sal', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow( aux )-1,nrow(aux)),
       sanitize.text.function = identity )

