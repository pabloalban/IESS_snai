message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tTablas de mortalidad' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_tasas_tran )
load( file = parametros$demo_rdata_sgo_tran_prep )

# tas_2_6, # tasa de mortalidad de activos hombres y mujeres
# tas_4_6, # tasa de mortalidad de pensionistas de vejez hombres y mujeres
# tas_5_6, # tasa de mortalidad de pensionistas de invalidez hombres y mujeres
# tas_7_6, # tasa de mortalidad de pensionistas de viudedad hombres y mujeres
# tas_8_6, # tasa de mortalidad de pensionistas de orfandad hombres y mujeres

# Generando tabla de mortalidad de activos hombres y mujeres ---------------------------------------
message( '\tGenerando tabla de mortalidad de activos hombres y mujeres' )

aux <- tas_2_6[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_2_6.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de mortalidad de pensionistas de vejez hombres y mujeres -------------------------
message( '\tGenerando tabla de mortalidad de pensionistas de vejez hombres y mujeres' )

aux <- tas_4_6[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_4_6.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de mortalidad de pensionistas de invalidez hombres y mujeres ---------------------
message( '\tGenerando tabla de mortalidad de pensionistas de invalidez hombres y mujeres' )

aux <- tas_5_6[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_5_6.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de mortalidad de pensionistas de viudedad hombres y mujeres ----------------------
message( '\tGenerando tabla de mortalidad de pensionistas de viudedad hombres y mujeres' )

aux <- tas_7_6[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_7_6.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = (nrow(aux)/2) - 1, sanitize.text.function = identity )

# Generando tabla de mortalidad de orfandad hombres y mujeres --------------------------------------
message( '\tGenerando tabla de mortalidad de orfandad hombres y mujeres' )

aux <- tas_8_6[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_8_6.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de entradas de nuevos afiliados hombres y mujeres --------------------------------
message( '\tGenerando tabla de entradas de nuevos afiliados hombres y mujeres' )

aux <- tas_1_2[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_1_2.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de actividad afiliados hombres y mujeres -----------------------------------------
message( '\tGenerando tabla de actividad afiliados hombres y mujeres' )

aux <- tas_2_3[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_2_3.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de jubilación vejez de hombres y mujeres -----------------------------------------
message( '\tGenerando tabla de jubilación de vejez afiliados hombres y mujeres' )

aux <- tas_2_4[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_2_4.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de jubilación invalidez de hombres y mujeres -------------------------------------
message( '\tGenerando tabla de jubilación de invalidez afiliados hombres y mujeres' )

aux <- tas_2_5[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_2_5.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# Generando tabla de reingreso de afiliados de hombres y mujeres -----------------------------------
message( '\tGenerando tabla de reingreso de afiliados hombres y mujeres' )

aux <- tas_3_2[ sexo != "HM", list( sexo, x, ux, lx, qx, px, ex ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 6, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tas_3_2.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux)/2, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
# message( paste( rep( '-', 100 ), collapse = '' ) )
# rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
# gc()
