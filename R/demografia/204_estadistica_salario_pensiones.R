# Estadística salarios y pensiones -----------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )

# Descripción de campos ----------------------------------------------------------------------------
# M2x = Conteo de registros, equivalente al número de activos, no incluye muertos.
# M3x = Conteo de registros, equivalente al número de inactivos, no incluye muertos.
# l2x = Exposición al riesgo de activos no salidos.
# l3x = Exposición al riesgo de inactivos no salidos.
# m2x = Exposición al riesgo de activos no salidos al mes de diciembre.
# m3x = Exposición al riesgo de inactivos no salidos al mes de diciembre.
# l2x_tnrh = Exposición al riesgo de activos TNRH no salidos.
# l3x_tnrh = Exposición al riesgo de inactivos TNRH no salidos.
# l12 = Exposición al riesgo de activos no TNRH no salidos.
# l13 = Exposición al riesgo de activos TNRH no salidos = l2x_tnrh.
# ts = Tiempo de servicio.
# ERx = ERx_act + ERx_ina, Exposición al total, suma de exposición al riesgo de activos e inactivos.
# ERx_act = Exposición al riesgo de activos.
# ERx_ina = Exposición al riesgo de inactivos.
# Nx_sal = Numero de salidas.
# Nx_ing = Número de ingresos.
# Nx_prim_ing = Número de primeros ingresos.
# Nx_dec = Número de decesos de activos e inactivos.
# Nx_dec_act = Número de decesos de activos.
# Nx_dec_ina = Número de decesos de inactivos.
# Nx_vej = Número de salidas por vejez.
# Nx_inv = Número de salidas por invalidez.
# Nx_dis = Número de salidas por discapacidad.
# Nx_viu = Número de salidas por viudedad.
# Nx_orf = Número de salidas por orfandad.
# S = Suma de salarios.
# S2 = Suma de los salarios al cuadrado, es el segundo momento.
# P = Suma de pensiones
# P2 = Suma de los pensiones al cuadrado, es el segundo momento.

# Carga información --------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_tran_prep )

# Estadísticas salarios promedio -------------------------------------------------------------------
message( '\tGenerando estadísticas de activos' )

save_list <- NULL

## Estadísticas de salario promedio por anio, sexo y edad ------------------------------------------
est_sal <- sgo_act_tran_anio[ , list( anio, sexo, x, ER_act = ERx_act, ER_ina = ERx_ina, S, S2 ) ]
est_sal[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal[ , ES := ifelse( ER_act > 0, S / ER_act, 0 ) ]
est_sal[ , ES2 := ifelse( ER_act > 0, S2 / ER_act, 0 ) ]
est_sal[ , ESm := ES / 12 ]
setorder( est_sal, anio, sexo, x )
save_list <- c( save_list, 'est_sal' )

## Estadística de salario promedio por anio y sexo -------------------------------------------------
est_sal_anio_sexo <- sgo_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  M12 = sum( M2x - M2x_tnrh, na.rm = TRUE ),
  M13 = sum( M2x_tnrh, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  l12 = sum( l2x - l2x_tnrh, na.rm = TRUE ),
  l13 = sum( l2x_tnrh, na.rm = TRUE ),
  m12 = sum( m2x - m2x_tnrh, na.rm = TRUE ),
  m13 = sum( m2x_tnrh, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ERx_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ERx_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  N_dis = sum( Nx_dis, na.rm = TRUE ),
  N_viu = sum( Nx_viu, na.rm = TRUE ),
  N_orf = sum( Nx_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST2, na.rm = TRUE ) ),
  by = list( anio, sexo )
]

est_sal_anio_sexo[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo[ , ES := 0 ]
est_sal_anio_sexo[ ER_act > 0, ES := S / ER_act ]
est_sal_anio_sexo[ , ES2 := 0 ]
est_sal_anio_sexo[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio_sexo[ , ESm := 0 ]
est_sal_anio_sexo[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio_sexo[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio_sexo[ , SDSm := SDS / 12 ]
est_sal_anio_sexo[ , EST := 0 ]
est_sal_anio_sexo[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_sal_anio_sexo[ , EST2 := 0 ]
est_sal_anio_sexo[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_sal_anio_sexo[ , ESTm := 0 ]
est_sal_anio_sexo[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_sal_anio_sexo[ , SDST := sqrt( EST2 - EST^2 ) ]
est_sal_anio_sexo[ , SDSTm := SDST / 12 ]

setorder( est_sal_anio_sexo, anio, sexo )
save_list <- c( save_list, 'est_sal_anio_sexo' )

## Estadística de salario promedio por anio, sexo y edad -------------------------------------------
est_sal_anio_sexo_edad <- sgo_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  M12 = sum( M2x - M2x_tnrh, na.rm = TRUE ),
  M13 = sum( M2x_tnrh, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  l12 = sum( l2x - l2x_tnrh, na.rm = TRUE ),
  l13 = sum( l2x_tnrh, na.rm = TRUE ),
  m12 = sum( m2x - m2x_tnrh, na.rm = TRUE ),
  m13 = sum( m2x_tnrh, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ERx_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ERx_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  N_dis = sum( Nx_dis, na.rm = TRUE ),
  N_viu = sum( Nx_viu, na.rm = TRUE ),
  N_orf = sum( Nx_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST2, na.rm = TRUE ) ),
  by = list( anio, sexo, x )
]

est_sal_anio_sexo_edad[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo_edad[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo_edad[ , ES := 0 ]
est_sal_anio_sexo_edad[ ER_act > 0, ES := S / ER_act ]
est_sal_anio_sexo_edad[ , ES2 := 0 ]
est_sal_anio_sexo_edad[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio_sexo_edad[ , ESm := 0 ]
est_sal_anio_sexo_edad[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio_sexo_edad[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio_sexo_edad[ , SDSm := SDS / 12 ]
est_sal_anio_sexo_edad[ , EST := 0 ]
est_sal_anio_sexo_edad[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_sal_anio_sexo_edad[ , EST2 := 0 ]
est_sal_anio_sexo_edad[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_sal_anio_sexo_edad[ , ESTm := 0 ]
est_sal_anio_sexo_edad[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_sal_anio_sexo_edad[ , SDST := sqrt( EST2 - EST^2 ) ]
est_sal_anio_sexo_edad[ , SDSTm := SDST / 12 ]

setorder( est_sal_anio_sexo_edad, anio, sexo, x )
save_list <- c( save_list, 'est_sal_anio_sexo_edad' )

## Estadística de salario promedio por anio y edad -------------------------------------------------
est_sal_anio_edad <- sgo_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  M12 = sum( M2x - M2x_tnrh, na.rm = TRUE ),
  M13 = sum( M2x_tnrh, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  l12 = sum( l2x - l2x_tnrh, na.rm = TRUE ),
  l13 = sum( l2x_tnrh, na.rm = TRUE ),
  m12 = sum( m2x - m2x_tnrh, na.rm = TRUE ),
  m13 = sum( m2x_tnrh, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ERx_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ERx_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  N_dis = sum( Nx_dis, na.rm = TRUE ),
  N_viu = sum( Nx_viu, na.rm = TRUE ),
  N_orf = sum( Nx_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST2, na.rm = TRUE ) ),
  by = list( anio, x )
]

est_sal_anio_edad[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_edad[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_edad[ , ES := 0 ]
est_sal_anio_edad[ ER_act > 0, ES := S / ER_act ]
est_sal_anio_edad[ , ES2 := 0 ]
est_sal_anio_edad[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio_edad[ , ESm := 0 ]
est_sal_anio_edad[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio_edad[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio_edad[ , SDSm := SDS / 12 ]
est_sal_anio_edad[ , EST := 0 ]
est_sal_anio_edad[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_sal_anio_edad[ , EST2 := 0 ]
est_sal_anio_edad[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_sal_anio_edad[ , ESTm := 0 ]
est_sal_anio_edad[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_sal_anio_edad[ , SDST := sqrt( EST2 - EST^2 ) ]
est_sal_anio_edad[ , SDSTm := SDST / 12 ]

setorder( est_sal_anio_edad, anio, x )
save_list <- c( save_list, 'est_sal_anio_edad' )

## Estadística de salario promedio por anio --------------------------------------------------------
est_sal_anio <- sgo_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  M12 = sum( M2x - M2x_tnrh, na.rm = TRUE ),
  M13 = sum( M2x_tnrh, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  l12 = sum( l2x - l2x_tnrh, na.rm = TRUE ),
  l13 = sum( l2x_tnrh, na.rm = TRUE ),
  m12 = sum( m2x - m2x_tnrh, na.rm = TRUE ),
  m13 = sum( m2x_tnrh, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ERx_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ERx_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  N_dis = sum( Nx_dis, na.rm = TRUE ),
  N_viu = sum( Nx_viu, na.rm = TRUE ),
  N_orf = sum( Nx_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST2, na.rm = TRUE ) ),
  by = list( anio )
]
est_sal_anio[ , ES := 0 ]
est_sal_anio[ ER_act > 0, ES := S / ER_act ]
est_sal_anio[ , ES2 := 0 ]
est_sal_anio[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio[ , ESm := 0 ]
est_sal_anio[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio[ , SDSm := SDS / 12 ]
est_sal_anio[ , EST := 0 ]
est_sal_anio[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_sal_anio[ , EST2 := 0 ]
est_sal_anio[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_sal_anio[ , ESTm := 0 ]
est_sal_anio[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_sal_anio[ , SDST := sqrt( EST2 - EST^2 ) ]
est_sal_anio[ , SDSTm := SDST / 12 ]

setorder( est_sal_anio, anio )
save_list <- c( save_list, 'est_sal_anio' )

## Estadística de salario promedio en todo el periodo de observación -------------------------------
est_sal_tot <- sgo_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  M12 = sum( M2x - M2x_tnrh, na.rm = TRUE ),
  M13 = sum( M2x_tnrh, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  l12 = sum( l2x - l2x_tnrh, na.rm = TRUE ),
  l13 = sum( l2x_tnrh, na.rm = TRUE ),
  m12 = sum( m2x - m2x_tnrh, na.rm = TRUE ),
  m13 = sum( m2x_tnrh, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ERx_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ERx_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  N_dis = sum( Nx_dis, na.rm = TRUE ),
  N_viu = sum( Nx_viu, na.rm = TRUE ),
  N_orf = sum( Nx_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST2, na.rm = TRUE ) )
]
est_sal_tot[ , ES := 0 ]
est_sal_tot[ ER_act > 0, ES := S / ER_act ]
est_sal_tot[ , ES2 := 0 ]
est_sal_tot[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_tot[ , ESm := 0 ]
est_sal_tot[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_tot[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_tot[ , SDSm := SDS / 12 ]
est_sal_tot[ , EST := 0 ]
est_sal_tot[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_sal_tot[ , EST2 := 0 ]
est_sal_tot[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_sal_tot[ , ESTm := 0 ]
est_sal_tot[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_sal_tot[ , SDST := sqrt( EST2 - EST^2 ) ]
est_sal_tot[ , SDSTm := SDST / 12 ]

save_list <- c( save_list, 'est_sal_tot' )

# Estadísticas por edad y tiempo de servicio -------------------------------------------------------
message( '\tGenerando estadísticas por edad x y tiempo de servicio' )
y <- parametros$anio_ini
load( paste0( parametros$demo_rdata_sgo_incom_tran_act_anio, y, '.RData' ) )

xlst <- c( seq( 0, 95, 5 ), Inf )
slst <- c( seq( 0, 50, 5 ), Inf )

sgo_comp_tran[ , s := round( imp, 0 ) ]
sgo_comp_tran[ , s_tnrh := round( imp_tnrh, 0 ) ]
sgo_comp_tran[ , xg := cut( x, breaks = xlst, include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ]
sgo_comp_tran[ , sg := cut( s, breaks = slst, include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ]
sgo_comp_tran[ , sg_tnrh := cut( s_tnrh, breaks = slst, include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ]

sgo_comp_tran[ , M2 := sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, 1, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]
sgo_comp_tran[ , M3 := sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, 1, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]
sgo_comp_tran[ , M2_tnrh := sum( ifelse( N_ing - N_sal >= 0 & ER_tnrh_act > 0, 1, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]
sgo_comp_tran[ , M3_tnrh := sum( ifelse( N_sal - N_ing >= 0 & ER_tnrh_ina > 0, 1, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]
sgo_comp_tran[ , l2 := sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, ER_act, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]
sgo_comp_tran[ , l3 := sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, ER_ina, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]
sgo_comp_tran[ , l2_tnrh := sum( ifelse( N_ing - N_sal >= 0 & ER_tnrh_act > 0, ER_tnrh_act, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]
sgo_comp_tran[ , l3_tnrh := sum( ifelse( N_sal - N_ing >= 0 & ER_tnrh_ina > 0, ER_tnrh_ina, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ) ]

# 1: requistos no cumplidos
# 2: cumple requisitos para invalidez y muerte
# 3: cumple requisitos para vejez en 5 años
# 4: cumple requisitos para vejez en 1 años
sgo_comp_tran[ , rsk_vej := mapply( FUN = parametros$elig_vej, s, x ) ]
sgo_comp_tran[ , rsk_inv := mapply( FUN = parametros$elig_inv, s, x ) ]
sgo_comp_tran[ , rsk_vej_5 := mapply( FUN = parametros$elig_vej, s + 5, x + 5 ) ]
sgo_comp_tran[ , rsk_vej_1 := mapply( FUN = parametros$elig_vej, s + 1, x + 1 ) ]
sgo_comp_tran[ rsk_vej == 0 & rsk_inv == 0, riesgo := 1 ]
sgo_comp_tran[ rsk_vej == 0 & rsk_inv == 1, riesgo := 2 ]
sgo_comp_tran[ rsk_vej_5 == 1, riesgo := 3 ]
sgo_comp_tran[ rsk_vej_1 == 1, riesgo := 4 ]

## Estadística por riesgo, edad, tiempo de servicio y sexo -----------------------------------------
est_act_rsk_xs <- sgo_comp_tran[ , list( 
  M2 = sum( M2, na.rm = TRUE ),
  M3 = sum( M3, na.rm = TRUE ),
  M12 = sum( M2 - M2_tnrh, na.rm = TRUE ),
  M13 = sum( M2_tnrh, na.rm = TRUE ),
  l2 = sum( l2, na.rm = TRUE ),
  l3 = sum( l3, na.rm = TRUE ),
  l12 = sum( l2 - l2_tnrh, na.rm = TRUE ),
  l13 = sum( l2_tnrh, na.rm = TRUE ),
  ER = sum( ER, na.rm = TRUE ),
  ER_act = sum( ER_act, na.rm = TRUE ),
  ER_ina = sum( ER_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ER_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ER_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( N_ing - N_prim_ing ), na.rm = TRUE ),
  N_sal = sum( N_sal, na.rm = TRUE ),
  N_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
  N_dec = sum( N_dec, na.rm = TRUE ),
  N_dec_act = sum( N_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
  N_vej = sum( N_vej, na.rm = TRUE ),
  N_inv = sum( N_inv, na.rm = TRUE ),
  N_dis = sum( N_dis, na.rm = TRUE ),
  N_viu = sum( N_viu, na.rm = TRUE ),
  N_orf = sum( N_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S^2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST^2, na.rm = TRUE ) ),
  by = list( riesgo, sexo, x, s ) ]

est_act_rsk_xs[ , PER_act := ER_act / sum( ER_act, na.rm = TRUE ) ]
est_act_rsk_xs[ , ES := 0 ]
est_act_rsk_xs[ ER_act > 0, ES := S / ER_act ]
est_act_rsk_xs[ , ES2 := 0 ]
est_act_rsk_xs[ ER_act > 0, ES2 := S2 / ER_act ]
est_act_rsk_xs[ , ESm := 0 ]
est_act_rsk_xs[ ER_act > 0, ESm := S / ER_act / 12 ]
est_act_rsk_xs[ , SDS := sqrt( ES2 - ES^2 ) ]
est_act_rsk_xs[ , SDSm := SDS / 12 ]
est_act_rsk_xs[ , EST := 0 ]
est_act_rsk_xs[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_act_rsk_xs[ , EST2 := 0 ]
est_act_rsk_xs[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_act_rsk_xs[ , ESTm := 0 ]
est_act_rsk_xs[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_act_rsk_xs[ , SDST := sqrt( EST2 - EST^2 ) ]
est_act_rsk_xs[ , SDSTm := SDST / 12 ]

save_list <- c( save_list, 'est_act_rsk_xs' )

## Estadística por edad, tiempo de servicio y sexo -------------------------------------------------
est_act_xs <- sgo_comp_tran[ , list( 
  M2 = sum( M2, na.rm = TRUE ),
  M3 = sum( M3, na.rm = TRUE ),
  M12 = sum( M2 - M2_tnrh, na.rm = TRUE ),
  M13 = sum( M2_tnrh, na.rm = TRUE ),
  l2 = sum( l2, na.rm = TRUE ),
  l3 = sum( l3, na.rm = TRUE ),
  l12 = sum( l2 - l2_tnrh, na.rm = TRUE ),
  l13 = sum( l2_tnrh, na.rm = TRUE ),
  ER = sum( ER, na.rm = TRUE ),
  ER_act = sum( ER_act, na.rm = TRUE ),
  ER_ina = sum( ER_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ER_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ER_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( N_ing - N_prim_ing ), na.rm = TRUE ),
  N_sal = sum( N_sal, na.rm = TRUE ),
  N_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
  N_dec = sum( N_dec, na.rm = TRUE ),
  N_dec_act = sum( N_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
  N_vej = sum( N_vej, na.rm = TRUE ),
  N_inv = sum( N_inv, na.rm = TRUE ),
  N_dis = sum( N_dis, na.rm = TRUE ),
  N_viu = sum( N_viu, na.rm = TRUE ),
  N_orf = sum( N_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S^2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST^2, na.rm = TRUE ) ),
  by = list( x, s, sexo ) ]

est_act_xs[ , PER_act := ER_act / sum( ER_act, na.rm = TRUE ) ]
est_act_xs[ , ES := 0 ]
est_act_xs[ ER_act > 0, ES := S / ER_act ]
est_act_xs[ , ES2 := 0 ]
est_act_xs[ ER_act > 0, ES2 := S2 / ER_act ]
est_act_xs[ , ESm := 0 ]
est_act_xs[ ER_act > 0, ESm := S / ER_act / 12 ]
est_act_xs[ , SDS := sqrt( ES2 - ES^2 ) ]
est_act_xs[ , SDSm := SDS / 12 ]
est_act_xs[ , EST := 0 ]
est_act_xs[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_act_xs[ , EST2 := 0 ]
est_act_xs[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_act_xs[ , ESTm := 0 ]
est_act_xs[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_act_xs[ , SDST := sqrt( EST2 - EST^2 ) ]
est_act_xs[ , SDSTm := SDST / 12 ]

save_list <- c( save_list, 'est_act_xs' )

## Estadística por grupo de edad, grupo de tiempo de servicio y sexo -------------------------------
est_act_grupo_xs <- sgo_comp_tran[ , list( 
  M2 = sum( M2, na.rm = TRUE ),
  M3 = sum( M3, na.rm = TRUE ),
  M12 = sum( M2 - M2_tnrh, na.rm = TRUE ),
  M13 = sum( M2_tnrh, na.rm = TRUE ),
  l2 = sum( l2, na.rm = TRUE ),
  l3 = sum( l3, na.rm = TRUE ),
  l12 = sum( l2 - l2_tnrh, na.rm = TRUE ),
  l13 = sum( l2_tnrh, na.rm = TRUE ),
  ER = sum( ER, na.rm = TRUE ),
  ER_act = sum( ER_act, na.rm = TRUE ),
  ER_ina = sum( ER_ina, na.rm = TRUE ),
  ER_tnrh_ina = sum( ER_tnrh_ina, na.rm = TRUE ),
  ER_tnrh_act = sum( ER_tnrh_act, na.rm = TRUE ),
  N_ing = sum( pmax( N_ing - N_prim_ing ), na.rm = TRUE ),
  N_sal = sum( N_sal, na.rm = TRUE ),
  N_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
  N_dec = sum( N_dec, na.rm = TRUE ),
  N_dec_act = sum( N_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
  N_vej = sum( N_vej, na.rm = TRUE ),
  N_inv = sum( N_inv, na.rm = TRUE ),
  N_dis = sum( N_dis, na.rm = TRUE ),
  N_viu = sum( N_viu, na.rm = TRUE ),
  N_orf = sum( N_orf, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S^2, na.rm = TRUE ),
  ST = sum( ST, na.rm = TRUE ),
  ST2 = sum( ST^2, na.rm = TRUE ) ),
  by = list( xg, sg, sexo ) ]

est_act_grupo_xs[ , PER_act := ER_act / sum( ER_act, na.rm = TRUE ) ]
est_act_grupo_xs[ , ES := 0 ]
est_act_grupo_xs[ ER_act > 0, ES := S / ER_act ]
est_act_grupo_xs[ , ES2 := 0 ]
est_act_grupo_xs[ ER_act > 0, ES2 := S2 / ER_act ]
est_act_grupo_xs[ , ESm := 0 ]
est_act_grupo_xs[ ER_act > 0, ESm := S / ER_act / 12 ]
est_act_grupo_xs[ , SDS := sqrt( ES2 - ES^2 ) ]
est_act_grupo_xs[ , SDSm := SDS / 12 ]
est_act_grupo_xs[ , EST := 0 ]
est_act_grupo_xs[ ER_tnrh_act > 0, EST := ST / ER_tnrh_act ]
est_act_grupo_xs[ , EST2 := 0 ]
est_act_grupo_xs[ ER_tnrh_act > 0, EST2 := ST2 / ER_tnrh_act ]
est_act_grupo_xs[ , ESTm := 0 ]
est_act_grupo_xs[ ER_tnrh_act > 0, ESTm := ST / ER_tnrh_act / 12 ]
est_act_grupo_xs[ , SDST := sqrt( EST2 - EST^2 ) ]
est_act_grupo_xs[ , SDSTm := SDST / 12 ]

save_list <- c( save_list, 'est_act_grupo_xs' )

## Estadística doble entrada grupo edad y grupo tiempo de servicio ambos sexos ---------------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ , list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ , list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ , list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER > 0, ES := S / ER / 12 ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_sal_xs <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_pob_xs <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_sal_xs <- merge.data.table( est_act_sal_xs, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_pob_xs <- merge.data.table( est_act_pob_xs, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_sal_xs[ , ord := 2 ]
est_act_pob_xs[ , ord := 1 ]
est_act_er_sal_xs <- rbind( est_act_sal_xs, est_act_pob_xs )
setorder( est_act_er_sal_xs, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_er_sal_xs <- rbind( est_act_er_sal_xs, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_er_sal_xs' )

## Estadística doble entrada grupo edad y grupo tiempo de servicio para hombres --------------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ sexo == 'H', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ sexo == 'H', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ sexo == 'H', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER > 0, ES := S / ER / 12 ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_sal_xs_h <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_pob_xs_h <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_sal_xs_h <- merge.data.table( est_act_sal_xs_h, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_pob_xs_h <- merge.data.table( est_act_pob_xs_h, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_sal_xs_h[ , ord := 2 ]
est_act_pob_xs_h[ , ord := 1 ]
est_act_er_sal_xs_h <- rbind( est_act_sal_xs_h, est_act_pob_xs_h )
setorder( est_act_er_sal_xs_h, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_er_sal_xs_h <- rbind( est_act_er_sal_xs_h, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_er_sal_xs_h' )

## Estadística doble entrada grupo edad y grupo tiempo de servicio para mujeres --------------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ sexo == 'M', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ sexo == 'M', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ sexo == 'M', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER > 0, ES := S / ER / 12 ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_sal_xs_m <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_pob_xs_m <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_sal_xs_m <- merge.data.table( est_act_sal_xs_m, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_pob_xs_m <- merge.data.table( est_act_pob_xs_m, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_sal_xs_m[ , ord := 2 ]
est_act_pob_xs_m[ , ord := 1 ]
est_act_er_sal_xs_m <- rbind( est_act_sal_xs_m, est_act_pob_xs_m )
setorder( est_act_er_sal_xs_m, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_er_sal_xs_m <- rbind( est_act_er_sal_xs_m, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_er_sal_xs_m' )

## Estadística TNRH doble entrada grupo edad y grupo tiempo de servicio ambos sexos ----------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ , list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ , list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ , list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux[ ER > 0, ES := S / ER / 12 ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_tnrh_sal_xs <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_tnrh_pob_xs <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_tnrh_sal_xs <- merge.data.table( est_act_tnrh_sal_xs, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_tnrh_pob_xs <- merge.data.table( est_act_tnrh_pob_xs, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_tnrh_sal_xs[ , ord := 2 ]
est_act_tnrh_pob_xs[ , ord := 1 ]
est_act_tnrh_er_sal_xs <- rbind( est_act_tnrh_sal_xs, est_act_tnrh_pob_xs )
setorder( est_act_tnrh_er_sal_xs, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_tnrh_er_sal_xs <- rbind( est_act_tnrh_er_sal_xs, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_tnrh_er_sal_xs' )

## Estadística TNRH doble entrada grupo edad y grupo tiempo de servicio para hombres ---------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ sexo == 'H' , list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ sexo == 'H', list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ sexo == 'H', list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux[ ER > 0, ES := S / ER / 12 ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_tnrh_sal_xs_h <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_tnrh_pob_xs_h <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_tnrh_sal_xs_h <- merge.data.table( est_act_tnrh_sal_xs_h, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_tnrh_pob_xs_h <- merge.data.table( est_act_tnrh_pob_xs_h, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_tnrh_sal_xs_h[ , ord := 2 ]
est_act_tnrh_pob_xs_h[ , ord := 1 ]
est_act_tnrh_er_sal_xs_h <- rbind( est_act_tnrh_sal_xs_h, est_act_tnrh_pob_xs_h )
setorder( est_act_tnrh_er_sal_xs_h, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_tnrh_er_sal_xs_h <- rbind( est_act_tnrh_er_sal_xs_h, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_tnrh_er_sal_xs_h' )

## Estadística TNRH doble entrada grupo edad y grupo tiempo de servicio para mujeres ---------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ sexo == 'M', list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ sexo == 'M', list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ sexo == 'M', list( S = sum( ST, na.rm = TRUE ), ER = sum( ER_tnrh_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux[ ER > 0, ES := S / ER / 12 ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_tnrh_sal_xs_m <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_tnrh_pob_xs_m <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_tnrh_sal_xs_m <- merge.data.table( est_act_tnrh_sal_xs_m, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_tnrh_pob_xs_m <- merge.data.table( est_act_tnrh_pob_xs_m, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_tnrh_sal_xs_m[ , ord := 2 ]
est_act_tnrh_pob_xs_m[ , ord := 1 ]
est_act_tnrh_er_sal_xs_m <- rbind( est_act_tnrh_sal_xs_m, est_act_tnrh_pob_xs_m )
setorder( est_act_tnrh_er_sal_xs_m, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_tnrh_er_sal_xs_m <- rbind( est_act_tnrh_er_sal_xs_m, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_tnrh_er_sal_xs_m' )

#---------------------------------------------------------------------------------------------------
est_act_ina_er <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER2 = ER_act, ER3 = ER_ina ) ]
est_act_ina_er <- est_act_ina_er[ , list( ER2 = sum( ER2, na.rm = TRUE ), ER3 = sum( ER3, na.rm = TRUE ) ), by = list( anio, sexo ) ]
est_act_ina_er <- melt.data.table( est_act_ina_er, id.vars = c( 'anio', 'sexo' ), value.name = 'total', variable.name = 'tipo' )
est_act_ina_er[ , por := total / sum( total ) ]
est_act_ina_er[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_ina_er[ sexo == 'H' & tipo == 'ER2', desc := 'Activos hombres' ]
est_act_ina_er[ sexo == 'M' & tipo == 'ER2', desc := 'Activos mujeres' ]
est_act_ina_er[ sexo == 'H' & tipo == 'ER3', desc := 'Inactivos hombres' ]
est_act_ina_er[ sexo == 'M' & tipo == 'ER3', desc := 'Inactivos mujeres' ]
save_list <- c( save_list, 'est_act_ina_er' )

#---------------------------------------------------------------------------------------------------
est_act_tnrh_ina_er <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER2 = ER_tnrh_act, ER3 = ER_tnrh_ina ) ]
est_act_tnrh_ina_er <- est_act_tnrh_ina_er[ , list( ER2 = sum( ER2, na.rm = TRUE ), ER3 = sum( ER3, na.rm = TRUE ) ), by = list( anio, sexo ) ]
est_act_tnrh_ina_er <- melt.data.table( est_act_tnrh_ina_er, id.vars = c( 'anio', 'sexo' ), value.name = 'total', variable.name = 'tipo' )
est_act_tnrh_ina_er[ , por := total / sum( total ) ]
est_act_tnrh_ina_er[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_tnrh_ina_er[ sexo == 'H' & tipo == 'ER2', desc := 'Activos hombres' ]
est_act_tnrh_ina_er[ sexo == 'M' & tipo == 'ER2', desc := 'Activos mujeres' ]
est_act_tnrh_ina_er[ sexo == 'H' & tipo == 'ER3', desc := 'Inactivos hombres' ]
est_act_tnrh_ina_er[ sexo == 'M' & tipo == 'ER3', desc := 'Inactivos mujeres' ]
save_list <- c( save_list, 'est_act_tnrh_ina_er' )

#---------------------------------------------------------------------------------------------------
est_act_er_risk_m <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_act, riesgo ) ]
est_act_er_risk_m <- est_act_er_risk_m[ !is.na( riesgo ) ]
est_act_er_risk_m <- est_act_er_risk_m[ sexo == 'M' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_act_er_risk_m[ , por := ER / sum( ER ) ]
est_act_er_risk_m[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_er_risk_m[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_act_er_risk_m' )

#---------------------------------------------------------------------------------------------------
est_act_er_risk_h <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_act, riesgo ) ]
est_act_er_risk_h <- est_act_er_risk_h[ !is.na( riesgo ) ]
est_act_er_risk_h <- est_act_er_risk_h[ sexo == 'H' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_act_er_risk_h[ , por := ER / sum( ER ) ]
est_act_er_risk_h[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_er_risk_h[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_act_er_risk_h' )

#---------------------------------------------------------------------------------------------------
est_ina_er_risk_m <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_ina, riesgo ) ]
est_ina_er_risk_m <- est_ina_er_risk_m[ !is.na( riesgo ) ]
est_ina_er_risk_m <- est_ina_er_risk_m[ sexo == 'M' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_ina_er_risk_m[ , por := ER / sum( ER ) ]
est_ina_er_risk_m[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_ina_er_risk_m[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_ina_er_risk_m' )

#---------------------------------------------------------------------------------------------------
est_ina_er_risk_h <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_ina, riesgo ) ]
est_ina_er_risk_h <- est_ina_er_risk_h[ !is.na( riesgo ) ]
est_ina_er_risk_h <- est_ina_er_risk_h[ sexo == 'H' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_ina_er_risk_h[ , por := ER / sum( ER ) ]
est_ina_er_risk_h[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_ina_er_risk_h[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_ina_er_risk_h' )

#---------------------------------------------------------------------------------------------------
est_act_tnrh_er_risk_m <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_tnrh_act, riesgo ) ]
est_act_tnrh_er_risk_m <- est_act_tnrh_er_risk_m[ !is.na( riesgo ) ]
est_act_tnrh_er_risk_m <- est_act_tnrh_er_risk_m[ sexo == 'M' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_act_tnrh_er_risk_m[ , por := ER / sum( ER ) ]
est_act_tnrh_er_risk_m[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_tnrh_er_risk_m[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_act_tnrh_er_risk_m' )

#---------------------------------------------------------------------------------------------------
est_act_tnrh_er_risk_h <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_tnrh_act, riesgo ) ]
est_act_tnrh_er_risk_h <- est_act_tnrh_er_risk_h[ !is.na( riesgo ) ]
est_act_tnrh_er_risk_h <- est_act_tnrh_er_risk_h[ sexo == 'H' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_act_tnrh_er_risk_h[ , por := ER / sum( ER ) ]
est_act_tnrh_er_risk_h[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_tnrh_er_risk_h[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_act_tnrh_er_risk_h' )

#---------------------------------------------------------------------------------------------------
est_ina_tnrh_er_risk_m <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_tnrh_ina, riesgo ) ]
est_ina_tnrh_er_risk_m <- est_ina_tnrh_er_risk_m[ !is.na( riesgo ) ]
est_ina_tnrh_er_risk_m <- est_ina_tnrh_er_risk_m[ sexo == 'M' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_ina_tnrh_er_risk_m[ , por := ER / sum( ER ) ]
est_ina_tnrh_er_risk_m[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_ina_tnrh_er_risk_m[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_ina_tnrh_er_risk_m' )

#---------------------------------------------------------------------------------------------------
est_ina_tnrh_er_risk_h <- sgo_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_tnrh_ina, riesgo ) ]
est_ina_tnrh_er_risk_h <- est_ina_tnrh_er_risk_h[ !is.na( riesgo ) ]
est_ina_tnrh_er_risk_h <- est_ina_tnrh_er_risk_h[ sexo == 'H' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_ina_tnrh_er_risk_h[ , por := ER / sum( ER ) ]
est_ina_tnrh_er_risk_h[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_ina_tnrh_er_risk_h[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_ina_tnrh_er_risk_h' )

# Estadísticas pensiones promedio ------------------------------------------------------------------
message( '\tGenerando estadísticas de pensionistas' )

est_pen_anio_tipo_sexo_sgo <- copy( sgo_pen_tran_anio )

## Estadísticas de pensiones promedio por anio, tipo, sexo y edad ----------------------------------
est_pen <- sgo_pen_tran_anio[ , list( anio, tipo, sexo, x, ER = ERx, P, P2 ) ]
est_pen[ , EP := ifelse( ER > 0, P / ER, 0 ) ]
est_pen[ , EP2 := ifelse( ER > 0, P2 / ER, 0 ) ]
est_pen[ , EPm := EP / 12 ]
setorder( est_pen, anio, tipo, sexo, x )

save_list <- c( save_list, 'est_pen' )

## Estadística de pensiones promedio por anio tipo, y sexo -----------------------------------------
est_pen_anio_tipo_sexo <- sgo_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, tipo, sexo )
]

est_pen_anio_tipo_sexo[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_tipo_sexo[ , EP := 0 ]
est_pen_anio_tipo_sexo[ ER > 0, EP := P / ER ]
est_pen_anio_tipo_sexo[ , EP2 := 0 ]
est_pen_anio_tipo_sexo[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_tipo_sexo[ , EPm := EP / 12 ]
est_pen_anio_tipo_sexo[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_tipo_sexo[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_tipo_sexo, tipo, sexo, anio )
est_pen_anio_tipo_sexo[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( tipo, sexo ) ]
est_pen_anio_tipo_sexo[ , tm := 100 * ( m / shift( m, type = 'lag' ) - 1 ), by = list( tipo, sexo ) ]
est_pen_anio_tipo_sexo[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( tipo, sexo ) ]
est_pen_anio_tipo_sexo[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( tipo, sexo ) ]

setorder( est_pen_anio_tipo_sexo, anio, tipo, sexo )

save_list <- c( save_list, 'est_pen_anio_tipo_sexo' )

## Estadística de pensiones promedio por anio tipo, sexo y edad ------------------------------------
est_pen_anio_tipo_sexo_x <- sgo_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, tipo, sexo, x )
]

est_pen_anio_tipo_sexo_x[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_tipo_sexo_x[ , EP := 0 ]
est_pen_anio_tipo_sexo_x[ ER > 0, EP := P / ER ]
est_pen_anio_tipo_sexo_x[ , EP2 := 0 ]
est_pen_anio_tipo_sexo_x[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_tipo_sexo_x[ , EPm := EP / 12 ]
est_pen_anio_tipo_sexo_x[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_tipo_sexo_x[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_tipo_sexo_x, tipo, sexo, x, anio )
est_pen_anio_tipo_sexo_x[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( tipo, sexo, x ) ]
est_pen_anio_tipo_sexo_x[ , tm := 100 * ( m / shift( m, type = 'lag' ) - 1 ), by = list( tipo, sexo, x ) ]
est_pen_anio_tipo_sexo_x[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( tipo, sexo, x ) ]
est_pen_anio_tipo_sexo_x[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( tipo, sexo, x ) ]

setorder( est_pen_anio_tipo_sexo_x, anio, tipo, sexo, x )

save_list <- c( save_list, 'est_pen_anio_tipo_sexo_x' )

## Estadística de pensiones promedio por anio y sexo -----------------------------------------------
est_pen_anio_sexo <- sgo_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, sexo )
]

est_pen_anio_sexo[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_sexo[ , EP := 0 ]
est_pen_anio_sexo[ ER > 0, EP := P / ER ]
est_pen_anio_sexo[ , EP2 := 0 ]
est_pen_anio_sexo[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_sexo[ , EPm := EP / 12 ]
est_pen_anio_sexo[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_sexo[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_sexo, sexo, anio )
est_pen_anio_sexo[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( sexo ) ]
est_pen_anio_sexo[ , tm := 100 * ( m / shift( m, type = 'lag' ) - 1 ), by = list( sexo ) ]
est_pen_anio_sexo[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( sexo ) ]
est_pen_anio_sexo[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( sexo ) ]

setorder( est_pen_anio_sexo, anio, sexo )

save_list <- c( save_list, 'est_pen_anio_sexo' )

## Estadística de pensiones promedio por anio y tipo -----------------------------------------------
est_pen_anio_tipo <- sgo_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, tipo )
]

est_pen_anio_tipo[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_tipo[ , EP := 0 ]
est_pen_anio_tipo[ ER > 0, EP := P / ER ]
est_pen_anio_tipo[ , EP2 := 0 ]
est_pen_anio_tipo[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_tipo[ , EPm := EP / 12 ]
est_pen_anio_tipo[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_tipo[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_tipo, tipo, anio )
est_pen_anio_tipo[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( tipo ) ]
est_pen_anio_tipo[ , tm := 100 * ( m / shift( m, type = 'lag' ) - 1 ), by = list( tipo ) ]
est_pen_anio_tipo[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( tipo ) ]
est_pen_anio_tipo[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( tipo ) ]

setorder( est_pen_anio_tipo, anio, tipo )

save_list <- c( save_list, 'est_pen_anio_tipo' )

## Estadística de pensiones promedio por anio ------------------------------------------------------
est_pen_anio <- sgo_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio )
]
est_pen_anio[ , EP := 0 ]
est_pen_anio[ ER > 0, EP := P / ER ]
est_pen_anio[ , EP2 := 0 ]
est_pen_anio[ ER > 0, EP2 := P2 / ER ]
est_pen_anio[ , EPm := EP / 12 ]
est_pen_anio[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio[ , SDPm := SDP / 12 ]

setorder( est_pen_anio, anio )
est_pen_anio[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ) ]
est_pen_anio[ , tm := 100 * ( m / shift( m, type = 'lag' ) - 1 ) ]
est_pen_anio[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ) ]
est_pen_anio[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ) ]

setorder( est_pen_anio, anio )

save_list <- c( save_list, 'est_pen_anio' )

## Estadística de pensiones promedio en todo el periodo de observación -----------------------------
est_pen_tot <- sgo_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) )
]
est_pen_tot[ , EP := 0 ]
est_pen_tot[ ER > 0, EP := P / ER ]
est_pen_tot[ , EP2 := 0 ]
est_pen_tot[ ER > 0, EP2 := P2 / ER ]
est_pen_tot[ , EPm := EP / 12 ]
est_pen_tot[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_tot[ , SDPm := SDP / 12 ]

save_list <- c( save_list, 'est_pen_tot' )

# Guardando resultados -----------------------------------------------------------------------------
message( '\tGuardando resultados de estadísticas de salarios y pensiones' )
save( list = save_list, file = parametros$demo_rdata_sgo_est_dem )

# Para el SSC --------------------------------------------------------------------------------------
# Carga información --------------------------------------------------------------------------------
load( parametros$demo_rdata_ssc_tran_prep )

# Estadísticas salarios promedio de jefes de familia -------------------------------------------------------------------
message( '\tGenerando estadísticas de jefes de familia' )

save_list <- NULL

## Estadísticas de salario promedio de jefes por anio, sexo y edad ------------------------------------------
est_sal <- ssc_act_tran_anio[ , list( anio, sexo, x, ER_act = ERx_act, ER_ina = ERx_ina, S, S2 ) ]
est_sal[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal[ , ES := ifelse( ER_act > 0, S / ER_act, 0 ) ]
est_sal[ , ES2 := ifelse( ER_act > 0, S2 / ER_act, 0 ) ]
est_sal[ , ESm := ES / 12 ]
setorder( est_sal, anio, sexo, x )
save_list <- c( save_list, 'est_sal' )

## Estadística de salario promedio de jefes por anio y sexo -------------------------------------------------
est_sal_anio_sexo <- ssc_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ) ),
  by = list( anio, sexo )
]

est_sal_anio_sexo[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo[ , ES := 0 ]
est_sal_anio_sexo[ ER_act > 0, ES := S / ER_act ]
est_sal_anio_sexo[ , ES2 := 0 ]
est_sal_anio_sexo[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio_sexo[ , ESm := 0 ]
est_sal_anio_sexo[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio_sexo[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio_sexo[ , SDSm := SDS / 12 ]

setorder( est_sal_anio_sexo, anio, sexo )
save_list <- c( save_list, 'est_sal_anio_sexo' )

## Estadística de salario promedio de jefes por anio, sexo y edad -------------------------------------------
est_sal_anio_sexo_edad <- ssc_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ) ),
  by = list( anio, sexo, x )
]

est_sal_anio_sexo_edad[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo_edad[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_sexo_edad[ , ES := 0 ]
est_sal_anio_sexo_edad[ ER_act > 0, ES := S / ER_act ]
est_sal_anio_sexo_edad[ , ES2 := 0 ]
est_sal_anio_sexo_edad[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio_sexo_edad[ , ESm := 0 ]
est_sal_anio_sexo_edad[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio_sexo_edad[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio_sexo_edad[ , SDSm := SDS / 12 ]

setorder( est_sal_anio_sexo_edad, anio, sexo, x )
save_list <- c( save_list, 'est_sal_anio_sexo_edad' )

## Estadística de salario promedio de jefes por anio y edad -------------------------------------------------
est_sal_anio_edad <- ssc_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE )),
  by = list( anio, x )
]

est_sal_anio_edad[ , PER2 := ER_act / sum( ER_act, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_edad[ , PER3 := ER_ina / sum( ER_ina, na.rm = TRUE ), by = list( anio ) ]
est_sal_anio_edad[ , ES := 0 ]
est_sal_anio_edad[ ER_act > 0, ES := S / ER_act ]
est_sal_anio_edad[ , ES2 := 0 ]
est_sal_anio_edad[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio_edad[ , ESm := 0 ]
est_sal_anio_edad[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio_edad[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio_edad[ , SDSm := SDS / 12 ]

setorder( est_sal_anio_edad, anio, x )
save_list <- c( save_list, 'est_sal_anio_edad' )

## Estadística de salario promedio de jefes por anio -----------------------------------------------
est_sal_anio <- ssc_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ) ),
  by = list( anio )
]
est_sal_anio[ , ES := 0 ]
est_sal_anio[ ER_act > 0, ES := S / ER_act ]
est_sal_anio[ , ES2 := 0 ]
est_sal_anio[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_anio[ , ESm := 0 ]
est_sal_anio[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_anio[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_anio[ , SDSm := SDS / 12 ]

setorder( est_sal_anio, anio )
save_list <- c( save_list, 'est_sal_anio' )

## Estadística de salario promedio de jefes en todo el periodo de observación ----------------------
est_sal_tot <- ssc_act_tran_anio[ , list(
  M2 = sum( M2x, na.rm = TRUE ),
  M3 = sum( M3x, na.rm = TRUE ),
  l2 = sum( l2x, na.rm = TRUE ),
  l3 = sum( l3x, na.rm = TRUE ),
  m2 = sum( m2x, na.rm = TRUE ),
  m3 = sum( m3x, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  ER_act = sum( ERx_act, na.rm = TRUE ),
  ER_ina = sum( ERx_ina, na.rm = TRUE ),
  N_ing = sum( pmax( Nx_ing - Nx_prim_ing ), na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_prim_ing = sum( Nx_prim_ing, na.rm = TRUE ),
  N_dec = sum( Nx_dec, na.rm = TRUE ),
  N_dec_act = sum( Nx_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( Nx_dec_ina, na.rm = TRUE ),
  N_vej = sum( Nx_vej, na.rm = TRUE ),
  N_inv = sum( Nx_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S2, na.rm = TRUE ) )
]
est_sal_tot[ , ES := 0 ]
est_sal_tot[ ER_act > 0, ES := S / ER_act ]
est_sal_tot[ , ES2 := 0 ]
est_sal_tot[ ER_act > 0, ES2 := S2 / ER_act ]
est_sal_tot[ , ESm := 0 ]
est_sal_tot[ ER_act > 0, ESm := S / ER_act / 12 ]
est_sal_tot[ , SDS := sqrt( ES2 - ES^2 ) ]
est_sal_tot[ , SDSm := SDS / 12 ]

save_list <- c( save_list, 'est_sal_tot' )

# Estadísticas por edad y tiempo de servicio -------------------------------------------------------
message( '\tGenerando estadísticas de jefes por edad x y tiempo de servicio' )
y <- parametros$anio_ini
load( paste0( parametros$demo_rdata_ssc_incom_tran_act_anio, y, '.RData' ) )

xlst <- c( seq( 0, 95, 5 ), Inf )
slst <- c( seq( 0, 50, 5 ), Inf )

ssc_comp_tran[ , s := round( imp, 0 ) ]
ssc_comp_tran[ , xg := cut( x, breaks = xlst, include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ]
ssc_comp_tran[ , sg := cut( s, breaks = slst, include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ]

ssc_comp_tran[ , M2 := sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, 1, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv, 0 ), na.rm = TRUE ) ]
ssc_comp_tran[ , M3 := sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, 1, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv, 0 ), na.rm = TRUE ) ]
ssc_comp_tran[ , l2 := sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, ER_act, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv, 0 ), na.rm = TRUE ) ]
ssc_comp_tran[ , l3 := sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, ER_ina, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv , 0 ), na.rm = TRUE ) ]

# 1: requistos no cumplidos
# 2: cumple requisitos para invalidez y muerte
# 3: cumple requisitos para vejez en 5 años
# 4: cumple requisitos para vejez en 1 años
ssc_comp_tran[ , rsk_vej := mapply( FUN =parametros$elig_vej_ssc, s, x ) ]
ssc_comp_tran[ , rsk_inv := mapply( FUN = parametros$elig_inv, s, x ) ]
ssc_comp_tran[ , rsk_vej_5 := mapply( FUN = parametros$elig_vej_ssc, s + 5, x + 5 ) ]
ssc_comp_tran[ , rsk_vej_1 := mapply( FUN = parametros$elig_vej_ssc, s + 1, x + 1 ) ]
ssc_comp_tran[ rsk_vej == 0 & rsk_inv == 0, riesgo := 1 ]
ssc_comp_tran[ rsk_vej == 0 & rsk_inv == 1, riesgo := 2 ]
ssc_comp_tran[ rsk_vej_5 == 1, riesgo := 3 ]
ssc_comp_tran[ rsk_vej_1 == 1, riesgo := 4 ]

## Estadística por riesgo, edad, tiempo de servicio y sexo -----------------------------------------
est_act_rsk_xs <- ssc_comp_tran[ , list( 
  M2 = sum( M2, na.rm = TRUE ),
  M3 = sum( M3, na.rm = TRUE ),
  l2 = sum( l2, na.rm = TRUE ),
  l3 = sum( l3, na.rm = TRUE ),
  ER = sum( ER, na.rm = TRUE ),
  ER_act = sum( ER_act, na.rm = TRUE ),
  ER_ina = sum( ER_ina, na.rm = TRUE ),
  N_ing = sum( pmax( N_ing - N_prim_ing ), na.rm = TRUE ),
  N_sal = sum( N_sal, na.rm = TRUE ),
  N_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
  N_dec = sum( N_dec, na.rm = TRUE ),
  N_dec_act = sum( N_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
  N_vej = sum( N_vej, na.rm = TRUE ),
  N_inv = sum( N_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S^2, na.rm = TRUE ) ),
  by = list( riesgo, sexo, x, s ) ]

est_act_rsk_xs[ , PER_act := ER_act / sum( ER_act, na.rm = TRUE ) ]
est_act_rsk_xs[ , ES := 0 ]
est_act_rsk_xs[ ER_act > 0, ES := S / ER_act ]
est_act_rsk_xs[ , ES2 := 0 ]
est_act_rsk_xs[ ER_act > 0, ES2 := S2 / ER_act ]
est_act_rsk_xs[ , ESm := 0 ]
est_act_rsk_xs[ ER_act > 0, ESm := S / ER_act / 12 ]
est_act_rsk_xs[ , SDS := sqrt( ES2 - ES^2 ) ]
est_act_rsk_xs[ , SDSm := SDS / 12 ]

save_list <- c( save_list, 'est_act_rsk_xs' )

## Estadística por edad, tiempo de servicio y sexo -------------------------------------------------
est_act_xs <- ssc_comp_tran[ , list( 
  M2 = sum( M2, na.rm = TRUE ),
  M3 = sum( M3, na.rm = TRUE ),
  l2 = sum( l2, na.rm = TRUE ),
  l3 = sum( l3, na.rm = TRUE ),
  ER = sum( ER, na.rm = TRUE ),
  ER_act = sum( ER_act, na.rm = TRUE ),
  ER_ina = sum( ER_ina, na.rm = TRUE ),
  N_ing = sum( pmax( N_ing - N_prim_ing ), na.rm = TRUE ),
  N_sal = sum( N_sal, na.rm = TRUE ),
  N_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
  N_dec = sum( N_dec, na.rm = TRUE ),
  N_dec_act = sum( N_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
  N_vej = sum( N_vej, na.rm = TRUE ),
  N_inv = sum( N_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S^2, na.rm = TRUE ) ),
  by = list( x, s, sexo ) ]

est_act_xs[ , PER_act := ER_act / sum( ER_act, na.rm = TRUE ) ]
est_act_xs[ , ES := 0 ]
est_act_xs[ ER_act > 0, ES := S / ER_act ]
est_act_xs[ , ES2 := 0 ]
est_act_xs[ ER_act > 0, ES2 := S2 / ER_act ]
est_act_xs[ , ESm := 0 ]
est_act_xs[ ER_act > 0, ESm := S / ER_act / 12 ]
est_act_xs[ , SDS := sqrt( ES2 - ES^2 ) ]
est_act_xs[ , SDSm := SDS / 12 ]

save_list <- c( save_list, 'est_act_xs' )

## Estadística por grupo de edad, grupo de tiempo de servicio y sexo -------------------------------
est_act_grupo_xs <- ssc_comp_tran[ , list( 
  M2 = sum( M2, na.rm = TRUE ),
  M3 = sum( M3, na.rm = TRUE ),
  l2 = sum( l2, na.rm = TRUE ),
  l3 = sum( l3, na.rm = TRUE ),
  ER = sum( ER, na.rm = TRUE ),
  ER_act = sum( ER_act, na.rm = TRUE ),
  ER_ina = sum( ER_ina, na.rm = TRUE ),
  N_ing = sum( pmax( N_ing - N_prim_ing ), na.rm = TRUE ),
  N_sal = sum( N_sal, na.rm = TRUE ),
  N_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
  N_dec = sum( N_dec, na.rm = TRUE ),
  N_dec_act = sum( N_dec_act, na.rm = TRUE ),
  N_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
  N_vej = sum( N_vej, na.rm = TRUE ),
  N_inv = sum( N_inv, na.rm = TRUE ),
  S = sum( S, na.rm = TRUE ),
  S2 = sum( S^2, na.rm = TRUE ) ),
  by = list( xg, sg, sexo ) ]

est_act_grupo_xs[ , PER_act := ER_act / sum( ER_act, na.rm = TRUE ) ]
est_act_grupo_xs[ , ES := 0 ]
est_act_grupo_xs[ ER_act > 0, ES := S / ER_act ]
est_act_grupo_xs[ , ES2 := 0 ]
est_act_grupo_xs[ ER_act > 0, ES2 := S2 / ER_act ]
est_act_grupo_xs[ , ESm := 0 ]
est_act_grupo_xs[ ER_act > 0, ESm := S / ER_act / 12 ]
est_act_grupo_xs[ , SDS := sqrt( ES2 - ES^2 ) ]
est_act_grupo_xs[ , SDSm := SDS / 12 ]

save_list <- c( save_list, 'est_act_grupo_xs' )

## Estadística doble entrada grupo edad y grupo tiempo de servicio ambos sexos ---------------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ , list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ , list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ , list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER > 0, ES := S / ER / 12 ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_sal_xs <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_pob_xs <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_sal_xs <- merge.data.table( est_act_sal_xs, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_pob_xs <- merge.data.table( est_act_pob_xs, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_sal_xs[ , ord := 2 ]
est_act_pob_xs[ , ord := 1 ]
est_act_er_sal_xs <- rbind( est_act_sal_xs, est_act_pob_xs )
setorder( est_act_er_sal_xs, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_er_sal_xs <- rbind( est_act_er_sal_xs, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_er_sal_xs' )

## Estadística doble entrada grupo edad y grupo tiempo de servicio para hombres --------------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ sexo == 'H', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ sexo == 'H', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ sexo == 'H', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER > 0, ES := S / ER / 12 ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_sal_xs_h <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_pob_xs_h <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_sal_xs_h <- merge.data.table( est_act_sal_xs_h, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_pob_xs_h <- merge.data.table( est_act_pob_xs_h, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_sal_xs_h[ , ord := 2 ]
est_act_pob_xs_h[ , ord := 1 ]
est_act_er_sal_xs_h <- rbind( est_act_sal_xs_h, est_act_pob_xs_h )
setorder( est_act_er_sal_xs_h, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_er_sal_xs_h <- rbind( est_act_er_sal_xs_h, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_er_sal_xs_h' )

## Estadística doble entrada grupo edad y grupo tiempo de servicio para mujeres --------------------
aux <- est_act_grupo_xs[ !( xg %in% c('[0,5)', '[5,10)', '[10,15)' ) ) ]
aux_r <- aux[ sexo == 'M', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( sg ) ]
aux_c <- aux[ sexo == 'M', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg ) ]
aux <- aux[ sexo == 'M', list( S = sum( S, na.rm = TRUE ), ER = sum( ER_act, na.rm = TRUE ) ), by = list( xg, sg ) ]
aux[ , ES := as.numeric( NA ) ]
aux[ ER > 0, ES := S / ER / 12 ]
aux[ ER <= 1/365, ES := NA ]
aux[ ER <= 1/365, ER := NA ]
aux_r[ , ES := as.numeric( NA ) ]
aux_r[ ER > 0, ES := S / ER / 12 ]
aux_c[ , ES := as.numeric( NA ) ]
aux_c[ ER > 0, ES := S / ER / 12 ]
setorder( aux_r, sg )
setorder( aux_c, xg )
est_act_sal_xs_m <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ES' )
est_act_pob_xs_m <- dcast.data.table( data = aux, formula = xg ~ sg, value.var = 'ER' )
est_act_sal_xs_m <- merge.data.table( est_act_sal_xs_m, aux_c[ , list( xg, TOT = ES ) ], by = 'xg' )
est_act_pob_xs_m <- merge.data.table( est_act_pob_xs_m, aux_c[ , list( xg, TOT = ER ) ], by = 'xg' )
est_act_sal_xs_m[ , ord := 2 ]
est_act_pob_xs_m[ , ord := 1 ]
est_act_er_sal_xs_m <- rbind( est_act_sal_xs_m, est_act_pob_xs_m )
setorder( est_act_er_sal_xs_m, xg, ord )
# aux[ , ord := NULL ]
nam <- as.character( aux_r$sg )
aux_r <- as.data.table( t( aux_r[ , list( ER, ES ) ] ) )
setnames( aux_r, nam )
aux_r[ , ord := c( 1, 2 ) ]
est_act_er_sal_xs_m <- rbind( est_act_er_sal_xs_m, aux_r, fill = TRUE, use.names = TRUE )
rm( aux, aux_r, aux_c )

save_list <- c( save_list, 'est_act_er_sal_xs_m' )

#---------------------------------------------------------------------------------------------------
est_act_ina_er <- ssc_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER2 = ER_act, ER3 = ER_ina ) ]
est_act_ina_er <- est_act_ina_er[ , list( ER2 = sum( ER2, na.rm = TRUE ), ER3 = sum( ER3, na.rm = TRUE ) ), by = list( anio, sexo ) ]
est_act_ina_er <- melt.data.table( est_act_ina_er, id.vars = c( 'anio', 'sexo' ), value.name = 'total', variable.name = 'tipo' )
est_act_ina_er[ , por := total / sum( total ) ]
est_act_ina_er[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_ina_er[ sexo == 'H' & tipo == 'ER2', desc := 'Activos hombres' ]
est_act_ina_er[ sexo == 'M' & tipo == 'ER2', desc := 'Activos mujeres' ]
est_act_ina_er[ sexo == 'H' & tipo == 'ER3', desc := 'Inactivos hombres' ]
est_act_ina_er[ sexo == 'M' & tipo == 'ER3', desc := 'Inactivos mujeres' ]
save_list <- c( save_list, 'est_act_ina_er' )

#---------------------------------------------------------------------------------------------------
est_act_er_risk_m <- ssc_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_act, riesgo ) ]
est_act_er_risk_m <- est_act_er_risk_m[ !is.na( riesgo ) ]
est_act_er_risk_m <- est_act_er_risk_m[ sexo == 'M' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_act_er_risk_m[ , por := ER / sum( ER ) ]
est_act_er_risk_m[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_er_risk_m[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_act_er_risk_m' )

#---------------------------------------------------------------------------------------------------
est_act_er_risk_h <- ssc_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_act, riesgo ) ]
est_act_er_risk_h <- est_act_er_risk_h[ !is.na( riesgo ) ]
est_act_er_risk_h <- est_act_er_risk_h[ sexo == 'H' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_act_er_risk_h[ , por := ER / sum( ER ) ]
est_act_er_risk_h[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_act_er_risk_h[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_act_er_risk_h' )

#---------------------------------------------------------------------------------------------------
est_ina_er_risk_m <- ssc_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_ina, riesgo ) ]
est_ina_er_risk_m <- est_ina_er_risk_m[ !is.na( riesgo ) ]
est_ina_er_risk_m <- est_ina_er_risk_m[ sexo == 'M' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_ina_er_risk_m[ , por := ER / sum( ER ) ]
est_ina_er_risk_m[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_ina_er_risk_m[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_ina_er_risk_m' )

#---------------------------------------------------------------------------------------------------
est_ina_er_risk_h <- ssc_comp_tran[ anio == parametros$anio_ini, list( anio, sexo, ER = ER_ina, riesgo ) ]
est_ina_er_risk_h <- est_ina_er_risk_h[ !is.na( riesgo ) ]
est_ina_er_risk_h <- est_ina_er_risk_h[ sexo == 'H' , list( ER = sum( ER, na.rm = TRUE ) ), by = list( riesgo ) ]
est_ina_er_risk_h[ , por := ER / sum( ER ) ]
est_ina_er_risk_h[ , porf := paste0( formatC( 100 * por, decimal.mark = ',' , digits = 2, format = 'f' ), '%' ) ]
est_ina_er_risk_h[ , riesgo := paste0( 'Grupo ', riesgo ) ]
save_list <- c( save_list, 'est_ina_er_risk_h' )

# Estadísticas pensiones promedio ------------------------------------------------------------------
message( '\tGenerando estadísticas de pensionistas del SSC' )

est_pen_anio_tipo_sexo_sgo <- copy( ssc_pen_tran_anio )

## Estadísticas de pensiones promedio por anio, tipo, sexo y edad ----------------------------------
est_pen <- ssc_pen_tran_anio[ , list( anio, tipo, sexo, x, ER = ERx, P, P2 ) ]
est_pen[ , EP := ifelse( ER > 0, P / ER, 0 ) ]
est_pen[ , EP2 := ifelse( ER > 0, P2 / ER, 0 ) ]
est_pen[ , EPm := EP / 12 ]
setorder( est_pen, anio, tipo, sexo, x )

save_list <- c( save_list, 'est_pen' )

## Estadística de pensiones promedio por anio tipo, y sexo -----------------------------------------
est_pen_anio_tipo_sexo <- ssc_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, tipo, sexo )
]

est_pen_anio_tipo_sexo[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_tipo_sexo[ , EP := 0 ]
est_pen_anio_tipo_sexo[ ER > 0, EP := P / ER ]
est_pen_anio_tipo_sexo[ , EP2 := 0 ]
est_pen_anio_tipo_sexo[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_tipo_sexo[ , EPm := EP / 12 ]
est_pen_anio_tipo_sexo[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_tipo_sexo[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_tipo_sexo, tipo, sexo, anio )
est_pen_anio_tipo_sexo[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( tipo, sexo ) ]
est_pen_anio_tipo_sexo[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( tipo, sexo ) ]
est_pen_anio_tipo_sexo[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( tipo, sexo ) ]

setorder( est_pen_anio_tipo_sexo, anio, tipo, sexo )

save_list <- c( save_list, 'est_pen_anio_tipo_sexo' )

## Estadística de pensiones promedio por anio tipo, sexo y edad ------------------------------------
est_pen_anio_tipo_sexo_x <- ssc_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, tipo, sexo, x )
]

est_pen_anio_tipo_sexo_x[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_tipo_sexo_x[ , EP := 0 ]
est_pen_anio_tipo_sexo_x[ ER > 0, EP := P / ER ]
est_pen_anio_tipo_sexo_x[ , EP2 := 0 ]
est_pen_anio_tipo_sexo_x[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_tipo_sexo_x[ , EPm := EP / 12 ]
est_pen_anio_tipo_sexo_x[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_tipo_sexo_x[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_tipo_sexo_x, tipo, sexo, x, anio )
est_pen_anio_tipo_sexo_x[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( tipo, sexo, x ) ]
est_pen_anio_tipo_sexo_x[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( tipo, sexo, x ) ]
est_pen_anio_tipo_sexo_x[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( tipo, sexo, x ) ]

setorder( est_pen_anio_tipo_sexo_x, anio, tipo, sexo, x )

save_list <- c( save_list, 'est_pen_anio_tipo_sexo_x' )

## Estadística de pensiones promedio por anio y sexo -----------------------------------------------
est_pen_anio_sexo <- ssc_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, sexo )
]

est_pen_anio_sexo[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_sexo[ , EP := 0 ]
est_pen_anio_sexo[ ER > 0, EP := P / ER ]
est_pen_anio_sexo[ , EP2 := 0 ]
est_pen_anio_sexo[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_sexo[ , EPm := EP / 12 ]
est_pen_anio_sexo[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_sexo[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_sexo, sexo, anio )
est_pen_anio_sexo[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( sexo ) ]
est_pen_anio_sexo[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( sexo ) ]
est_pen_anio_sexo[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( sexo ) ]

setorder( est_pen_anio_sexo, anio, sexo )

save_list <- c( save_list, 'est_pen_anio_sexo' )

## Estadística de pensiones promedio por anio y tipo -----------------------------------------------
est_pen_anio_tipo <- ssc_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio, tipo )
]

est_pen_anio_tipo[ , PER := ER / sum( ER, na.rm = TRUE ), by = list( anio ) ]
est_pen_anio_tipo[ , EP := 0 ]
est_pen_anio_tipo[ ER > 0, EP := P / ER ]
est_pen_anio_tipo[ , EP2 := 0 ]
est_pen_anio_tipo[ ER > 0, EP2 := P2 / ER ]
est_pen_anio_tipo[ , EPm := EP / 12 ]
est_pen_anio_tipo[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio_tipo[ , SDPm := SDP / 12 ]

setorder( est_pen_anio_tipo, tipo, anio )
est_pen_anio_tipo[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ), by = list( tipo ) ]
est_pen_anio_tipo[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ), by = list( tipo ) ]
est_pen_anio_tipo[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ), by = list( tipo ) ]

setorder( est_pen_anio_tipo, anio, tipo )

save_list <- c( save_list, 'est_pen_anio_tipo' )

## Estadística de pensiones promedio por anio ------------------------------------------------------
est_pen_anio <- ssc_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) ),
  by = list( anio )
]
est_pen_anio[ , EP := 0 ]
est_pen_anio[ ER > 0, EP := P / ER ]
est_pen_anio[ , EP2 := 0 ]
est_pen_anio[ ER > 0, EP2 := P2 / ER ]
est_pen_anio[ , EPm := EP / 12 ]
est_pen_anio[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_anio[ , SDPm := SDP / 12 ]

setorder( est_pen_anio, anio )
est_pen_anio[ , td := 100 * ( ER / shift( ER, type = 'lag' ) - 1 ) ]
est_pen_anio[ , tp := 100 * ( EP / shift( EP, type = 'lag' ) - 1 ) ]
est_pen_anio[ , tpm := 100 * ( EPm / shift( EPm, type = 'lag' ) - 1 ) ]

setorder( est_pen_anio, anio )

save_list <- c( save_list, 'est_pen_anio' )

## Estadística de pensiones promedio en todo el periodo de observación -----------------------------
est_pen_tot <- ssc_pen_tran_anio[ , list(
  M = sum( Mx, na.rm = TRUE ),
  l = sum( lx, na.rm = TRUE ),
  ER = sum( ERx, na.rm = TRUE ),
  m = sum( mx, na.rm = TRUE ),
  N_sal = sum( Nx_sal, na.rm = TRUE ),
  N_dec = sum( Nx, na.rm = TRUE ),
  P = sum( P, na.rm = TRUE ),
  P2 = sum( P2, na.rm = TRUE ) )
]
est_pen_tot[ , EP := 0 ]
est_pen_tot[ ER > 0, EP := P / ER ]
est_pen_tot[ , EP2 := 0 ]
est_pen_tot[ ER > 0, EP2 := P2 / ER ]
est_pen_tot[ , EPm := EP / 12 ]
est_pen_tot[ , SDP := sqrt( EP2 - EP^2 ) ]
est_pen_tot[ , SDPm := SDP / 12 ]

save_list <- c( save_list, 'est_pen_tot' )

# Guardando resultados -----------------------------------------------------------------------------
message( '\tGuardando resultados de estadísticas de salarios y pensiones del SSC' )
save( list = save_list, file = parametros$demo_rdata_ssc_est_dem )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
