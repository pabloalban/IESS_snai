# Carga información --------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCalculando tabla decrementos' )
load( paste0( parametros$RData_seg, 'IESS_IVM_tablas_biometricas_2019.RData' ) )

# Construcción tabla de decrementos ----------------------------------------------------------------
tab_dec <- copy( tasas )
tab_dec[ is.na( ER ), ER := 0 ]
tab_dec[ is.na( Nd ), Nd := 0 ]
tab_dec[ is.na( Ni ), Ni := 0 ]
tab_dec[ is.na( Nr ), Nr := 0 ]

# Estimando fuerzas de transición inmediata --------------------------------------------------------
tab_dec[ ER > 0, ud_est := Nd / ER ]
tab_dec[ ER > 0, ui_est := Ni / ER ]
tab_dec[ ER > 0, uv_est := Nr / ER ]
tab_dec[ ER == 0, ud_est := 0 ]
tab_dec[ ER == 0, ui_est := 0 ]
tab_dec[ ER == 0, uv_est := 0 ]

# Alisando tasas brutas ----------------------------------------------------------------------------
tab_dec[ , log_ud_est := log( ud_est ) ]
tab_dec[ , log_ui_est := log( ui_est ) ]
tab_dec[ , log_uv_est := log( uv_est ) ]

tab_dec_proj_f <- data.table( x = 15:110 )
tab_dec_proj_m <- data.table( x = 15:110 )

# Alisando retiro por muerte -----------------------------------------------------------------------
ud_smooth_model_f <- lm( log_ud_est ~ bs( x, df = 3, degree = 3, Boundary.knots = range( tab_dec_proj_f$x ) ), 
                         data = tab_dec[ sexo == 'F' & is.finite( log_ud_est ) & 
                                           x >= 20 & x <= 85 ] )
tab_dec_proj_f[ , log_ud := predict( object = ud_smooth_model_f,
                                     newdata = tab_dec_proj_f ) ]

ud_smooth_model_m <- lm( log_ud_est ~ bs( x, df = 7, degree = 3, Boundary.knots = range( tab_dec_proj_m$x ) ),
                         data = tab_dec[ sexo == 'M' & is.finite( log_ud_est ) & 
                                           x >= 20 & x <= 89  ] )
tab_dec_proj_m[ , log_ud := predict( object = ud_smooth_model_m, 
                                     newdata = tab_dec_proj_m ) ]

# Alisando retiro por invalidez --------------------------------------------------------------------
ui_smooth_model_f <- lm( log_ui_est ~ bs( x, df = 8, degree = 3, Boundary.knots = range( tab_dec_proj_f$x ) ),
                         data = tab_dec[ sexo == 'F' & is.finite( log_ui_est ) & 
                                           x >= 19 & x <= 100 ] )
tab_dec_proj_f[ , log_ui := predict( object = ui_smooth_model_f,
                                     newdata = tab_dec_proj_f[ , list( x ) ] ) ]

ui_smooth_model_m <- lm( log_ui_est ~ bs( x, df = 8, degree = 3, Boundary.knots = range( tab_dec_proj_m$x ) ),
                         data = tab_dec[ sexo == 'M' & is.finite( log_ui_est ) & 
                                           x >= 19 & x <= 100 ] )
tab_dec_proj_m[ , log_ui := predict( object = ui_smooth_model_m, 
                                     newdata = tab_dec_proj_m[ , list( x ) ] ) ]

# Alisando retiro por vejez ------------------------------------------------------------------------
uv_smooth_model_f <- lm( log_uv_est ~ bs( x, df = 8, degree = 3, Boundary.knots = range( tab_dec_proj_f$x ) ), 
                         data = tab_dec[ sexo == 'F' & is.finite( log_uv_est ) ] )
tab_dec_proj_f[ , log_uv := predict( object = uv_smooth_model_f, 
                                     newdata = tab_dec_proj_f[ , list( x ) ] ) ]

uv_smooth_model_m <- lm( log_uv_est ~ bs( x, df = 8, degree = 3, Boundary.knots = range( tab_dec_proj_m$x ) ), 
                         data = tab_dec[ sexo == 'M' & is.finite( log_uv_est ) ] )
tab_dec_proj_m[ , log_uv := predict( object = uv_smooth_model_m,
                                     newdata = tab_dec_proj_m[ , list( x ) ] ) ]

# Colocando columna sexo
tab_dec_proj_f[ , sexo := 'F' ]
tab_dec_proj_m[ , sexo := 'M' ]

aux <- rbind( tab_dec_proj_f, tab_dec_proj_m )
tab_dec <- merge( tab_dec, aux, by = c( 'sexo', 'x' ), all.y = TRUE )

tab_dec <- tab_dec[ , list( sexo, x, ER, 
                            Nd, Ni, Nr, 
                            ud_est, ui_est, uv_est,
                            log_ud, log_ui, log_uv,
                            log_ud_est, log_ui_est, log_uv_est ) ]
setorder( tab_dec, sexo, x )

# Generación de fuerzas de transición inmediata lisas ----------------------------------------------
tab_dec[ , ud := exp( log_ud ) ]
tab_dec[ , ui := exp( log_ui ) ]
tab_dec[ , uv := exp( log_uv ) ]
tab_dec[ x <= 48, uv := 0 ]
tab_dec[ x >= 100, uv := 0 ]
tab_dec[ , u_tau := ud + ui + uv ]

# Calculando probabilidades independientes ---------------------------------------------------------
tab_dec[ is.finite( ud ), qd_ind := 1 - exp( -ud ) ]
tab_dec[ !is.finite( ud ), qd_ind := 1 ]
tab_dec[ is.finite( ui ), qi_ind := 1 - exp( -ui ) ]
tab_dec[ !is.finite( ui ), qi_ind := 1 ]
tab_dec[ is.finite( uv ), qv_ind := 1 - exp( -uv ) ]
tab_dec[ !is.finite( uv ), qv_ind := 1 ]
tab_dec[ , pd_ind := 1 - qd_ind ]
tab_dec[ , pi_ind := 1 - qi_ind ]
tab_dec[ , pv_ind := 1 - qv_ind ]

# Cálculo probabilidades totales -------------------------------------------------------------------
tab_dec[ , p_tau := pd_ind * pi_ind * pv_ind ]
tab_dec[ , q_tau := 1 - p_tau ]

# Cálculo probabilidades dependientes --------------------------------------------------------------
# Asumiendo fuerzas constantes de decrementos
tab_dec[ is.finite( u_tau ), qd := ud * q_tau / u_tau ]
tab_dec[ !is.finite( u_tau ), qd := 0 ]
tab_dec[ is.finite( u_tau ), qi := ui * q_tau / u_tau ]
tab_dec[ !is.finite( u_tau ), qi := 0 ]
tab_dec[ is.finite( u_tau ), qv := uv * q_tau / u_tau ]
tab_dec[ !is.finite( u_tau ), qv := 0 ]
tab_dec[ , pd := 1 - qd ]
tab_dec[ , pi := 1 - qi ]
tab_dec[ , pv := 1 - qv ]

# Calculando esperanzas ----------------------------------------------------------------------------
setorder( tab_dec, sexo, x )
tab_dec[ , ed := cumprod( pd ), by = list( sexo ) ]
tab_dec[ , ed := rev( ed ), by = list( sexo ) ]
tab_dec[ , ed := cumsum( ed ), by = list( sexo ) ]
tab_dec[ , ed := rev( ed ), by = list( sexo ) ]
tab_dec[ , ed := ed + 0.5 ]

tab_dec[ , ev := cumprod( pv ), by = list( sexo ) ]
tab_dec[ , ev := rev( ev ), by = list( sexo ) ]
tab_dec[ , ev := cumsum( ev ), by = list( sexo ) ]
tab_dec[ , ev := rev( ev ), by = list( sexo ) ]
tab_dec[ , ev := ev + 0.5 ]

tab_dec[ , ei := cumprod( pi ), by = list( sexo ) ]
tab_dec[ , ei := rev( ei ), by = list( sexo ) ]
tab_dec[ , ei := cumsum( ei ), by = list( sexo ) ]
tab_dec[ , ei := rev( ei ), by = list( sexo ) ]
tab_dec[ , ei := ei + 0.5 ]

# Generando tabla de decrementos -------------------------------------------------------------------
tab_dec[ , lx := 1e5 ]
tab_dec[ , p_tau_shift := shift( p_tau, type = 'lag', fill = 1 ), by = list( sexo ) ]
tab_dec[ , p_tau_shift := cumprod( p_tau_shift ), by = list( sexo ) ]
tab_dec[ , lx := 1e5  *  p_tau_shift ]
tab_dec[ , dd := lx * qd ]
tab_dec[ , di := lx * qi ]
tab_dec[ , dv := lx * qv ]
tab_dec[ , p_tau_shift := NULL ]

save( tab_dec, 
      ud_smooth_model_f, ud_smooth_model_m, 
      ui_smooth_model_f, ui_smooth_model_m, 
      uv_smooth_model_f, uv_smooth_model_m, 
      file = paste0( parametros$RData, 'IESS_tabla_decrementos.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
