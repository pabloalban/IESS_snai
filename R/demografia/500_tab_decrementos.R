message( paste( rep( '-', 100 ), collapse = '' ) )

# Cargando datos -----------------------------------------------------------------------------------
if( isTRUE(parametros$risko_conf) ) {
  load( file = paste0( parametros$RData, 'demografia/IESS_tabla_decrementos.RData' ) )
  load( file = paste0( parametros$RData, 'CES/IESS_tablas_biometricas_mortalidad_todos_estados.RData' ) )
}else{
  load( file = paste0( parametros$RData, 'IESS_tabla_decrementos.RData' ) )
  load( file = paste0( parametros$RData, 'IESS_tablas_biometricas_mortalidad_todos_estados.RData' ) )
}
# Generando tabla de decrementos ------------------------------------------------------------------
message( '\tGenerando tablas de decrementos' )
aux_f <- tab_dec[ sexo == 'F' & x <= 105, list( x, lx_f = lx, dv_f = dv, di_f = di, dd_f = dd ) ]
aux_m <- tab_dec[ sexo == 'M' & x <= 105, list( x, x_m = x, lx_m = lx, dv_m = dv, di_m = di, dd_m = dd ) ]
aux <- merge( aux_f, aux_m, by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 4, 4, 4, 0, 2, 4, 4, 4 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_dec.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla de mortalidad afiliados ---------------------------------------------------------
message( '\tGenerando tabla de mortalidad afiliados' )
aux <- merge( afi_mor[ t == 2018 & sexo == 'F' , list( x, lx, qx, px, ex ) ],
              afi_mor[ t == 2018 & sexo == 'M' , list( x, x_m = x, lx, qx, px, ex ) ], 
              by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 6, 6, 2, 0, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_afi_mort_2018.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla de mortalidad inválidos --------------------------------------------------------
message( '\tGenerando tabla de mortalidad pensionistas inválidos' )
aux <- merge( pen_inv_mor[ x > 19 & t == 2018 & sexo == 'F' , list( x, lx, qx, px, ex ) ],
              pen_inv_mor[ x > 19 & t == 2018 & sexo == 'M' , list( x, x_m = x, lx, qx, px, ex ) ], 
              by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 6, 6, 2, 0, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_pen_inv_mort_2018.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla de mortalidad pensionistas -----------------------------------------------------
message( '\tGenerando tabla de mortalidad pensionistas vejez' )
aux <- merge( pen_vej_mor[ t == 2018 & sexo == 'F' , list( x, lx, qx, px, ex ) ],
              pen_vej_mor[ t == 2018 & sexo == 'M' , list( x, x_m = x, lx, qx, px, ex ) ], 
              by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 6, 6, 2, 0, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_pen_vej_mort_2018.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla de mortalidad montepíos -------------------------------------------------------
message( '\tGenerando tabla de mortalidad pensionistas montepío' )
aux <- merge( noafi_mor[ t == 2018 & sexo == 'F' , list( x, lx, qx, px, ex ) ],
              noafi_mor[ t == 2018 & sexo == 'M' , list( x, x_m = x, lx, qx, px, ex ) ], 
              by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 6, 6, 2, 0, 2, 6, 6, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_mon_mort_2018.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# ------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
