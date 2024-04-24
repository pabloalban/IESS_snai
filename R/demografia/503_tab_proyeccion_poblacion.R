message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_pob_proy )
load( file = parametros$demo_rdata_sgo_pea_proj )

message( '\tGenerando tablas de proyección de la población' )
y_max <- parametros$anio_ini + parametros$demo_horizonte

# Generando tabla proyección población: ------------------------------------------------------------
aux <- pob_proy_tot[ t > 0 & t <= y_max, list( 
  t = t + parametros$anio_ini, l1, l2, l3, l4, l5, l6, l7, l8 ) ]
aux[, t := as.character( t ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_tot.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla: iess_tab_pob_proy ---------------------------------------------------------------
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( 
  t = t + parametros$anio_ini, l1_m = l1, l2_m = l2, l3_m = l3, l4_m = l4, l5_m = l5, l6_m = l6, l7_m = l7, l8_m = l8 ) ]
aux_m <- aux_m[ t <= y_max ]
aux_h <- pob_proy_tot_sex[ sexo == 'H', list( 
  t = t + parametros$anio_ini, l1_h = l1, l2_h = l2, l3_h = l3, l4_h = l4, l5_h = l5, l6_h = l6, l7_h = l7, l8_h = l8 ) ]
aux_h <- aux_h[ t <= y_max ]

aux <- merge( aux_m, aux_h, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux <- aux[ t > parametros$anio_ini ]

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 16 ) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla: iess_tab_pob_proy_tran ----------------------------------------------------------
aux_m <- pob_proy_tot_sex[ sexo == 'M', list(
  t = t + parametros$anio_ini,
  l1_2_m = l1_2, l1_5_m = l1_6,
  l2_3_m = l2_3, l2_4_m = l2_4, l2_5_m = l2_5, l2_6_m = l2_6,
  l3_2_m = l3_2, l3_4_m = l3_4, l3_5_m = l3_5, l3_6_m = l3_6,
  l4_6_m = l4_6,
  l5_6_m = l5_6 ) ]
aux_m <- aux_m[ t <= y_max ]
aux_m[, t := as.character( t ) ]

aux_h <- pob_proy_tot_sex[ sexo == 'H', list(
  t = t + parametros$anio_ini,
  l1_2_h = l1_2, l1_5_h = l1_6,
  l2_3_h = l2_3, l2_4_h = l2_4, l2_5_h = l2_5, l2_6_h = l2_6,
  l3_2_h = l3_2, l3_4_h = l3_4, l3_5_h = l3_5, l3_6_h = l3_6,
  l4_6_h = l4_6,
  l5_6_h = l5_6 ) ]
aux_h <- aux_h[ t <= y_max ]
aux_h[, t := as.character( t ) ]

xtb_aux <- xtable( aux_m, digits = c( 0, 0, rep( 2, 12 ) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_tran_m.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

xtb_aux <- xtable( aux_h, digits = c( 0, 0, rep( 2, 12 ) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_tran_h.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Población montepios viudas -----------------------------------------------------------------------
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, l7_m = l7 ) ]
aux_m <- aux_m[ t <= y_max ]

aux_h <- pob_proy_tot_sex[ sexo == 'H', list( t = t + parametros$anio_ini, l7_h = l7 ) ]
aux_h <- aux_h[ t <= y_max ]

aux <- merge( aux_m, aux_h, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux <- aux[ t > parametros$anio_ini ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_viu.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Población montepios viudas -----------------------------------------------------------------------
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, l8_m = l8 ) ]
aux_m <- aux_m[ t <= y_max ]

aux_h <- pob_proy_tot_sex[ sexo == 'H', list( t = t + parametros$anio_ini, l8_h = l8 ) ]
aux_h <- aux_h[ t <= y_max ]

aux <- merge( aux_m, aux_h, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux <- aux[ t > parametros$anio_ini ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_huer.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Población cónyuges dependientes ------------------------------------------------------------------
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, l9_m = l9 ) ]
aux_m <- aux_m[ t <= y_max ]

aux_h <- pob_proy_tot_sex[ sexo == 'H', list( t = t + parametros$anio_ini, l9_h = l9 ) ]
aux_h <- aux_h[ t <= y_max ]

aux <- merge( aux_m, aux_h, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux <- aux[ t > parametros$anio_ini ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_cony.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Población dependientes hijos ---------------------------------------------------------------------
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, l10_m = l10, l11_m = l11 ) ]
aux_m <- aux_m[ t <= y_max ]

aux_h <- pob_proy_tot_sex[ sexo == 'H', list( t = t + parametros$anio_ini, l10_m = l10, l11_m = l11 ) ]
aux_h <- aux_h[ t <= y_max ]

aux <- merge( aux_m, aux_h, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux <- aux[ t > parametros$anio_ini ]

xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_hij.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Proyección de la PEA ------------------------------------------------------------------------------
aux <- pob_proy_tot[ , list( t = t + parametros$anio_ini, l1, l2, l3 ) ]
aux <- aux[ t <= y_max ]
aux[ , pea := l1 + l2 + l3 ]
aux[ , pea_afiliada := l2 / pea ]

aux_onu <- copy( PEA_proy[ anio >= parametros$anio_ini, list( anio, peax ) ] )
aux_onu <- aux_onu[ anio <= y_max ]
aux_onu <- aux_onu[ , list( pea = sum( peax ) ), by = list( anio ) ]

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
