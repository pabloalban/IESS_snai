# Mortalidad dinámica ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( parametros$demo_rdata_onu_int_life_tab )
load( parametros$demo_rdata_sgo_tasas_tran )

# Pre procesamiento --------------------------------------------------------------------------------
# Ajustando sexo al factor adecuado
onu_ecu_mort_din[ sexo == 'M', sex := factor( 'H', levels = c( 'H', 'M', 'HM' ) ) ]
onu_ecu_mort_din[ sexo == 'F', sex := factor( 'M', levels = c( 'H', 'M', 'HM' ) ) ]
onu_ecu_mort_din[ , sexo := sex ]
onu_ecu_mort_din[ , sex := NULL ]

onu_ecu_surv[ sexo == 'M', sex := factor( 'H', levels = c( 'H', 'M', 'HM' ) ) ]
onu_ecu_surv[ sexo == 'F', sex := factor( 'M', levels = c( 'H', 'M', 'HM' ) ) ]
onu_ecu_surv[ , sexo := sex ]
onu_ecu_surv[ , sex := NULL ]

onu_ecu_surv_int[ sexo == 'M', sex := factor( 'H', levels = c( 'H', 'M', 'HM' ) ) ]
onu_ecu_surv_int[ sexo == 'F', sex := factor( 'M', levels = c( 'H', 'M', 'HM' ) ) ]
onu_ecu_surv_int[ , sexo := sex ]
onu_ecu_surv_int[ , sex := NULL ]

aux_onu <- onu_ecu_mort_din[ t >= parametros$anio_ini, list( t, sexo, x, qx_onu = qx, vx ) ]
aux_onu[ , x := as.integer( x ) ]

tas_din_dec <- rbindlist( list(
  tas_2_6[ , list( grupo = 'qx_act', sexo, x, qx ) ],
  tas_4_6[ , list( grupo = 'qx_vej', sexo, x, qx ) ],
  tas_5_6[ , list( grupo = 'qx_inv',sexo, x, qx ) ], 
  tas_7_6[ , list( grupo = 'qx_viu',sexo, x, qx ) ],
  tas_8_6[ , list( grupo = 'qx_orf',sexo, x, qx ) ] ) )

tas_din_dec <- tas_din_dec[ sexo != 'HM' ]
tas_din_dec[ , x := as.integer( x ) ]

tas_din_dec <- dcast.data.table( data = tas_din_dec, formula = sexo + x ~ grupo, value.var = 'qx' )
tas_din_dec <- merge.data.table( aux_onu, tas_din_dec, by = c( 'sexo', 'x' ), all.x = TRUE )

message( '\tAjustando probabilidad por variaciones de la tabla de la ONU' )
tas_din_dec[ t == parametros$anio_ini, vx := 1 ]

setorder( tas_din_dec, sexo, x, t )
tas_din_dec[ , vx := cumprod( vx ), by = list( sexo, x ) ]

tas_din_dec[ !is.na( qx_act ), qx_act := vx * qx_act ]
tas_din_dec[ is.na( qx_act ), qx_act := qx_onu ]

tas_din_dec[ !is.na( qx_vej ), qx_vej := vx * qx_vej ]
tas_din_dec[ is.na( qx_vej ), qx_vej := qx_onu ]

tas_din_dec[ !is.na( qx_inv ), qx_inv := vx * qx_inv ]
tas_din_dec[ is.na( qx_inv ), qx_inv := qx_onu ]

tas_din_dec[ !is.na( qx_viu ), qx_viu := vx * qx_viu ]
tas_din_dec[ is.na( qx_viu ), qx_viu := qx_onu ]

tas_din_dec[ !is.na( qx_orf ), qx_orf := vx * qx_orf ]
tas_din_dec[ is.na( qx_orf ), qx_orf := qx_onu ]

tas_din_dec[ qx_act >= 1, qx_act := 1 - 1e-9 ] 
tas_din_dec[ qx_vej >= 1, qx_vej := 1 - 1e-9 ]
tas_din_dec[ qx_inv >= 1, qx_inv := 1 - 1e-9 ] 
tas_din_dec[ qx_viu >= 1, qx_viu := 1 - 1e-9 ] 
tas_din_dec[ qx_orf >= 1, qx_orf := 1 - 1e-9 ]

tas_din_dec[ , px_act := 1 - qx_act ] 
tas_din_dec[ , px_vej := 1 - qx_vej ]
tas_din_dec[ , px_inv := 1 - qx_inv ] 
tas_din_dec[ , px_viu := 1 - qx_viu ] 
tas_din_dec[ , px_orf := 1 - qx_orf ]

tas_din_dec[ , ux_act := -log( px_act ) ]
tas_din_dec[ , ux_vej := -log( px_vej ) ]
tas_din_dec[ , ux_inv := -log( px_inv ) ]
tas_din_dec[ , ux_viu := -log( px_viu ) ]
tas_din_dec[ , ux_orf := -log( px_orf ) ]

setorder( tas_din_dec, t, sexo, x )

# Actuarial formula: may the rule be with you
tas_din_dec[ , ex_act := cumprod( px_act ), by = list( t, sexo ) ]
tas_din_dec[ , ex_act := rev( cumsum( rev( ex_act ) ) ) / shift( ex_act, n = 1, type = 'lag', fill = 1 ) - 0.5, by = list( t, sexo ) ]
tas_din_dec[ , ex_vej := cumprod( px_vej ), by = list( t, sexo ) ]
tas_din_dec[ , ex_vej := rev( cumsum( rev( ex_vej ) ) ) / shift( ex_vej, n = 1, type = 'lag', fill = 1 ) - 0.5, by = list( t, sexo ) ]
tas_din_dec[ , ex_inv := cumprod( px_inv ), by = list( t, sexo ) ]
tas_din_dec[ , ex_inv := rev( cumsum( rev( ex_inv ) ) ) / shift( ex_inv, n = 1, type = 'lag', fill = 1 ) - 0.5, by = list( t, sexo ) ]
tas_din_dec[ , ex_viu := cumprod( px_viu ), by = list( t, sexo ) ]
tas_din_dec[ , ex_viu := rev( cumsum( rev( ex_viu ) ) ) / shift( ex_viu, n = 1, type = 'lag', fill = 1 ) - 0.5, by = list( t, sexo ) ]
tas_din_dec[ , ex_orf := cumprod( px_orf ), by = list( t, sexo ) ]
tas_din_dec[ , ex_orf := rev( cumsum( rev( ex_orf ) ) ) / shift( ex_orf, n = 1, type = 'lag', fill = 1 ) - 0.5, by = list( t, sexo ) ]

# Guardando resultados -----------------------------------------------------------------------------
message( '\tGuardando resultados' )
save( tas_din_dec, 
      file = parametros$demo_rdata_sgo_din_dec )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
