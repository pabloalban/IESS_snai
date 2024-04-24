# Modelos para tasa de fertilidad y nupcialidad ----------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

# Cargando censos ----------------------------------------------------------------------------------
message( '\tCargando objetos' )

load( parametros$demo_rdata_inec_censo )
cen_pob_2010 <- copy( censo_pob )
rm( censo_pob )
gc()

# Preparación censo 2010 ---------------------------------------------------------------------------
message( '\tProcesando censo 2010' )
cen_2010 <- cen_pob_2010[ , list( I01, I02, I03, I04, I05, I09, I10, URP, P00, P01, P02, P03, P34, P35 ) ]
cen_2010 <- merge.data.table( 
   cen_2010[ P02 == 'Jefe o Jefa de hogar', 
             list( I01, I02, I03, I04, I05, I09, I10, URP, sexo = P01, tipo = P02, x = P03, P34, aseg = P35 ) ],
   cen_2010[ , list( I01, I02, I03, I04, I05, I09, I10, URP, P01, P02, P03, P35 ) ], 
   by = c( 'I01', 'I02', 'I03', 'I04', 'I05', 'I09', 'I10', 'URP' ), 
   all.x = TRUE )

setnames( cen_2010, c( 'P01', 'P02', 'P03', 'P34', 'P35' ), c( 'sexo_dep', 'tipo_dep', 'y', 'est_civ', 'aseg_dep' ) )

## Codificando campos ------------------------------------------------------------------------------
cen_2010[ tipo == 'Jefe o Jefa de hogar', tipo := 'J' ]
cen_2010[ tipo == 'Cónyuge o  conviviente', tipo := 'C' ]
cen_2010[ tipo == 'Hijo o HIja', tipo := 'H' ]
cen_2010[ tipo == 'Nieto o nieta', tipo := 'N' ]
cen_2010[ tipo == 'Otro no pariente', tipo := 'ONP' ]
cen_2010[ tipo == 'Otro Pariente', tipo := 'OP' ]
cen_2010[ tipo == 'Yerno o nuera', tipo := 'YN' ]
cen_2010[ tipo == 'Padres o suegros', tipo := 'PS' ]
cen_2010[ tipo == 'Empleado(a) doméstico(a)', tipo := 'E' ]
cen_2010[ tipo == 'Miembro de hogar colectivo', tipo := 'MHC' ]
cen_2010[ tipo == 'Sin vivienda', tipo := 'SV' ]
cen_2010[ , tipo := factor( 
   tipo, levels = c( 'J', 'C', 'H', 'N', 'ONP', 'OP', 'YN', 'PS', 'E', 'MHC', 'SV' ) ) ]

cen_2010[ tipo_dep == 'Jefe o Jefa de hogar', tipo_dep := 'J' ]
cen_2010[ tipo_dep == 'Cónyuge o  conviviente', tipo_dep := 'C' ]
cen_2010[ tipo_dep == 'Hijo o HIja', tipo_dep := 'H' ]
cen_2010[ tipo_dep == 'Nieto o nieta', tipo_dep := 'N' ]
cen_2010[ tipo_dep == 'Otro no pariente', tipo_dep := 'ONP' ]
cen_2010[ tipo_dep == 'Otro Pariente', tipo_dep := 'OP' ]
cen_2010[ tipo_dep == 'Yerno o nuera', tipo_dep := 'YN' ]
cen_2010[ tipo_dep == 'Padres o suegros', tipo_dep := 'PS' ]
cen_2010[ tipo_dep == 'Empleado(a) doméstico(a)', tipo_dep := 'E' ]
cen_2010[ tipo_dep == 'Miembro de hogar colectivo', tipo_dep := 'MHC' ]
cen_2010[ tipo_dep == 'Sin vivienda', tipo_dep := 'SV' ]
cen_2010[ , tipo_dep := factor( 
   tipo_dep, levels = c( 'J', 'C', 'H', 'N', 'ONP', 'OP', 'YN', 'PS', 'E', 'MHC', 'SV' ) ) ]

cen_2010[ sexo_dep == 'Hombre', sexo_dep := 'H' ]
cen_2010[ sexo_dep == 'Mujer', sexo_dep := 'M' ]
cen_2010[ , sexo_dep := factor( sexo_dep, levels = c( 'H', 'M' ) ) ]

cen_2010[ sexo == 'Hombre', sexo := 'H' ]
cen_2010[ sexo == 'Mujer', sexo := 'M' ]
cen_2010[ , sexo := factor( sexo, levels = c( 'H', 'M' ) ) ]

cen_2010[ est_civ == 'Casado/a', est_civ := 'CA' ]
cen_2010[ est_civ == 'Separado/a', est_civ := 'SE' ]
cen_2010[ est_civ == 'Viudo/a', est_civ := 'VI' ]
cen_2010[ est_civ == 'Unido/a', est_civ := 'CA' ]
cen_2010[ est_civ == 'Divorciado/a', est_civ := 'DI' ]
cen_2010[ est_civ == 'Soltero/a', est_civ := 'SO' ]
cen_2010[ , est_civ := factor( est_civ, c( 'CA', 'SE', 'VI', 'DI', 'SO' ) ) ]

cen_2010[ aseg == 'IESS Seguro general', aseg := 'iess_sgo' ]
cen_2010[ aseg == 'No aporta', aseg := 'no_aporta' ]
cen_2010[ aseg == 'IESS Seguro campesino', aseg := 'iess_ssc' ]
cen_2010[ aseg == 'IESS Seguro voluntario', aseg := 'iess_sgo' ]
cen_2010[ aseg == 'Seguro ISSPOL', aseg := 'isspol' ]
cen_2010[ aseg == 'Seguro ISSFA', aseg := 'issfa' ]
cen_2010[ aseg == 'Se ignora', aseg := 'ignora' ]
cen_2010[ aseg == 'Es jubilado del IESS/ISSFA/ISSPOL', aseg := 'pens_iess_issfa_isspol' ]
cen_2010[ , aseg := factor( 
   aseg, levels = c( 'iess_sgo', 'iess_ssc', 'issfa', 'isspol', 'pens_iess_issfa_issplo', 'ignora', 'no_aporta' ) ) ]

cen_2010[ aseg_dep == 'IESS Seguro general', aseg_dep := 'iess_sgo' ]
cen_2010[ aseg_dep == 'No aporta', aseg_dep := 'no_aporta' ]
cen_2010[ aseg_dep == 'IESS Seguro campesino', aseg_dep := 'iess_ssc' ]
cen_2010[ aseg_dep == 'IESS Seguro voluntario', aseg_dep := 'iess_sgo' ]
cen_2010[ aseg_dep == 'Seguro ISSPOL', aseg_dep := 'isspol' ]
cen_2010[ aseg_dep == 'Seguro ISSFA', aseg_dep := 'issfa' ]
cen_2010[ aseg_dep == 'Se ignora', aseg_dep := 'ignora' ]
cen_2010[ aseg_dep == 'Es jubilado del IESS/ISSFA/ISSPOL', aseg_dep := 'pens_iess_issfa_isspol' ]
cen_2010[ , aseg_dep := factor( 
   aseg_dep, 
   levels = c( 'iess_sgo', 'iess_ssc', 'issfa', 'isspol', 'pens_iess_issfa_issplo', 'ignora', 'no_aporta' ) ) ]

cen_aseg_2010 <- copy( cen_2010[ aseg %in% c( 'iess_sgo', 'iess_ssc' ) & 
                                    aseg_dep %in% c( 'iess_sgo', 'iess_ssc' ) ] )

rm( cen_pob_2010 )
gc()

# Estimación tasa de fertilidad --------------------------------------------------------------------
# En el alisamiento no se utiliza los datos del censo 2001, debido a que no se tiene unicidad de 
# invidividuos
message( '\tAlisando tasas de fertilidad' )

## Preparación información -------------------------------------------------------------------------
fer_dat <- copy( cen_aseg_2010 )
fer_dat[ , nj := 0 ]
fer_dat[ tipo_dep == 'J', nj := 1 ]
fer_dat[ , nj := sum( nj ), by = list( tipo, x ) ]
fer_dat[ tipo_dep == 'H', n := 1 ]
fer_dat[ tipo_dep != 'H', n := 0 ]
fer_dat <- fer_dat[ , list( n = sum( n ) ), by = list( tipo, sexo, x, tipo_dep, sexo_dep, y, nj ) ]
fer_dat[ , q := n / nj ]
fer_dat <- fer_dat[ tipo_dep == 'H', list( sexo, x, sexo_dep, y, nj, n, q_obs = q ) ]
fer_dat[ , u_obs := log( -log( q_obs ) ) ]
setorder( fer_dat, x, y )

fer_dat_prep <- fer_dat
# fer_dat_prep <- fer_dat_prep[ sexo == 'M' & x - y > 55 & nj < 10 ]
fer_dat_prep[ x - y <= 15 & !is.na( q_obs ), q_obs := 0 ]
fer_dat_prep[ , u_obs := log( -log( q_obs ) ) ]
fer_dat_prep <- fer_dat_prep[ is.finite( u_obs ) ]
fer_dat_prep[ , uq := quantile( u_obs, 0.97 ), by = list( sexo ) ]
fer_dat_prep <- fer_dat_prep[ u_obs <= uq ]
fer_dat_prep[ , var := var( u_obs ), by = list( abs( x - y ) ) ]
fer_dat_prep[ , varx := mean( var, na.rm = TRUE ), by = list( x ) ]
fer_dat_prep[ is.na( var ), var := varx ]
fer_dat_prep[ , varx := mean( var, na.rm = TRUE ) ]
fer_dat_prep[ is.na( var ), var := varx ]
fer_dat_prep[ , varx := NULL ]

summary( fer_dat_prep$var )
chk <- fer_dat_prep[ , list( q_obs = sum( q_obs, na.rm = TRUE ) ), by = list( x ) ]
plot( chk$x, chk$q_obs, pch = 16, cex = 0.8, col = "darkgreen" )

## Malla de interpolación --------------------------------------------------------------------------
x <- seq( 15, 110, 1 )
y <- seq( 0, 80, 1 )
int_data <- data.table( expand.grid( x = x, y = y ) )
setorder( int_data, x, y )

## Interpolanción, para Jefe Hombre, Hijo Hombre ---------------------------------------------------
fer_dat_prep_hh <- fer_dat_prep[ sexo == 'H' & sexo_dep == 'H' ]

fer_krig_hh <- km( formula = ~1,
                   design = fer_dat_prep_hh[ , list( x, y ) ], 
                   response = fer_dat_prep_hh$u_obs, 
                   estim.method = 'MLE', 
                   covtype = 'matern5_2',
                   optim.method = 'BFGS', 
                   noise.var = fer_dat_prep_hh$var )

fer_krig_hh_pred <- predict( fer_krig_hh, newdata = int_data, type = "SK" )

fer_dat_hh <- copy( int_data )
fer_dat_hh[ , u := fer_krig_hh_pred$mean ]
fer_dat_hh[ , q := exp( -exp( u ) ) ]
fer_dat_hh[ x - y <= 15, q := 0 ]
fer_dat_hh <- merge.data.table( 
   fer_dat_hh,
   fer_dat[ sexo == 'H' & sexo_dep == 'H' ],
   by = c( 'x', 'y' ),
   all.x = TRUE )
fer_dat_hh[ , r := u - u_obs ]

# chk <- fer_dat_hh[ , list( q = sum( q, na.rm = TRUE ), q_obs = sum( q_obs, na.rm = TRUE ) ), by = list( x ) ]
# plot( chk$x, chk$q, type = 'l', lwd = 2, col = "navyblue", ylim = c( 0, 1 ) )
# points( chk$x, chk$q_obs, pch = 16, cex = 0.8, col = "darkgreen" )
# 
# fer_surf <- dcast.data.table( data = fer_dat_hh, formula = x ~ y, value.var = 'q' )
# fer_surf[ , x := NULL ]
# fer_surf <- as.matrix( fer_surf )
# persp3d( x, y, fer_surf, col = 'gold', alpha = 0.7, xlim = range( x ), ylim = range( y ), zlim = c( -0.05, 1 ) )
# points3d( fer_dat_hh$x, fer_dat_hh$y, fer_dat_hh$q_obs, col = 'red', add = TRUE, zlim = c( -0.05, 0.1 ) )

## Interpolanción, para Jefe Hombre, Hijo Mujer ----------------------------------------------------
fer_dat_prep_hm <- fer_dat_prep[ sexo == 'H' & sexo_dep == 'M' ]

fer_krig_hm <- km( formula = ~1, 
                   design = fer_dat_prep_hm[ , list( x, y ) ], 
                   response = fer_dat_prep_hm$u_obs, 
                   estim.method = 'MLE', 
                   covtype = 'matern5_2',
                   optim.method = 'BFGS', 
                   noise.var = fer_dat_prep_hm$var )

fer_krig_hm_pred <- predict( fer_krig_hm, newdata = int_data, type = "SK" )

fer_dat_hm <- copy( int_data )
fer_dat_hm[ , u := fer_krig_hm_pred$mean ]
fer_dat_hm[ , q := exp( -exp( u ) ) ]
fer_dat_hm[ x - y <= 15, q := 0 ]
fer_dat_hm <- merge.data.table( 
   fer_dat_hm,
   fer_dat[ sexo == 'H' & sexo_dep == 'M' ],
   by = c( 'x', 'y' ),
   all.x = TRUE )
fer_dat_hm[ , r := u - u_obs ]

# chk <- fer_dat_hm[ , list( q = sum( q, na.rm = TRUE ), q_obs = sum( q_obs, na.rm = TRUE ) ), by = list( x ) ]
# plot( chk$x, chk$q, type = 'l', lwd = 2, col = "navyblue", ylim = c( 0, 1 ) )
# points( chk$x, chk$q_obs, pch = 16, cex = 0.8, col = "darkgreen" )

## Interpolanción, para Jefe Mujer, Hijo Hombre ----------------------------------------------------
fer_dat_prep_mh <- fer_dat_prep[ sexo == 'M' & sexo_dep == 'H' ]

fer_krig_mh <- km( formula = ~1, 
                   design = fer_dat_prep_mh[ , list( x, y ) ], 
                   response = fer_dat_prep_mh$u_obs, 
                   estim.method = 'MLE', 
                   covtype = 'matern5_2',
                   optim.method = 'BFGS', 
                   noise.var = fer_dat_prep_mh$var )

fer_krig_mh_pred <- predict( fer_krig_mh, newdata = int_data, type = "SK" )

fer_dat_mh <- copy( int_data )
fer_dat_mh[ , u := fer_krig_mh_pred$mean ]
fer_dat_mh[ , q := exp( -exp( u ) ) ]
fer_dat_mh[ x - y <= 15, q := 0 ]
fer_dat_mh <- merge.data.table( 
   fer_dat_mh,
   fer_dat[ sexo == 'M' & sexo_dep == 'H' ],
   by = c( 'x', 'y' ),
   all.x = TRUE )
fer_dat_mh[ , r := u - u_obs ]

# chk <- fer_dat_mh[ , list( q = sum( q, na.rm = TRUE ), q_obs = sum( q_obs, na.rm = TRUE ) ), by = list( x ) ]
# plot( chk$x, chk$q, type = 'l', lwd = 2, col = "navyblue", ylim = c( 0, 1 ) )
# points( chk$x, chk$q_obs, pch = 16, cex = 0.8, col = "darkgreen" )

## Interpolanción, para Jefe Mujer, Hijo Mujer -----------------------------------------------------
fer_dat_prep_mm <- fer_dat_prep[ sexo == 'M' & sexo_dep == 'M' ]

fer_krig_mm <- km( formula = ~1, 
                   design = fer_dat_prep_mm[ , list( x, y ) ], 
                   response = fer_dat_prep_mm$u_obs, 
                   estim.method = 'MLE', 
                   covtype = 'matern5_2',
                   optim.method = 'BFGS', 
                   noise.var = fer_dat_prep_mm$var )

fer_krig_mm_pred <- predict( fer_krig_mm, newdata = int_data, type = "SK" )

fer_dat_mm <- copy( int_data )
fer_dat_mm[ , u := fer_krig_mm_pred$mean ]
fer_dat_mm[ , q := exp( -exp( u ) ) ]
fer_dat_mm[ x - y <= 15, q := 0 ]
fer_dat_mm <- merge.data.table( 
   fer_dat_mm,
   fer_dat[ sexo == 'M' & sexo_dep == 'M' ],
   by = c( 'x', 'y' ),
   all.x = TRUE )
fer_dat_mm[ , r := u - u_obs ]

# chk <- fer_dat_mm[ , list( q = sum( q, na.rm = TRUE ), q_obs = sum( q_obs, na.rm = TRUE ) ), by = list( x ) ]
# plot( chk$x, chk$q, type = 'l', lwd = 2, col = "navyblue", ylim = c( 0, 1 ) )
# points( chk$x, chk$q_obs, pch = 16, cex = 0.8, col = "darkgreen" )

## Uniendo resultados de fertilidad ----------------------------------------------------------------
fer_dat <- rbindlist( list( fer_dat_hh, fer_dat_hm, fer_dat_mh, fer_dat_mm ) )

# Estimación tasa nupcialidad ----------------------------------------------------------------------
message( '\tEstimando tasa de nupcialidad para el iess' )

## Preparación de información ----------------------------------------------------------------------
nup_dat <- copy( cen_aseg_2010 )
nup_dat[ , nj := 0 ]
nup_dat[ tipo_dep == 'J', nj := 1 ]
nup_dat[ , nj := sum( nj ), by = list( tipo, x ) ]
nup_dat[ est_civ == 'CA', n := 1 ]
nup_dat[ est_civ != 'CA', n := 0 ]
nup_dat <- nup_dat[ , list( n = sum( n ) ), by = list( tipo, x, tipo_dep, y, nj ) ]
nup_dat[ , q := n / nj ]
nup_dat <- nup_dat[ tipo_dep == 'C', list( x, y, nj, n, q_obs = q ) ]
nup_dat[ , u_obs := log( -log( q_obs ) ) ]
setorder( nup_dat, x, y )

nup_dat_prep <- nup_dat[ x >= 20 & x <= 90 ]
nup_dat_prep[ abs( x - y ) > 40 & !is.na( q_obs ), q_obs := 0 ]
nup_dat_prep[ , u_obs := log( -log( q_obs ) ) ]
nup_dat_prep <- nup_dat_prep[ is.finite( u_obs ) ]
nup_dat_prep[ , var := var( u_obs ), by = list( x ) ]
nup_dat_prep[ is.na( var ), var := mean( var, na.rm = TRUE ) ]
nup_dat_prep[ , vart := abs( x - y ) * var( u_obs ), by = list( abs( x - y ) ) ]
nup_dat_prep[ is.na( vart ), vart := mean( vart, na.rm = TRUE ) ]
nup_dat_prep[ , var := var^2 + vart^2 ]

# chk <- nup_dat_prep[ , list( q_obs = sum( q_obs, na.rm = TRUE ) ), by = list( x ) ]
# plot( chk$x, chk$q_obs, pch = 16, cex = 0.8, col = "darkgreen" )

## Malla de interpolación --------------------------------------------------------------------------
x <- seq( 15, 110, 1 )
y <- seq( 15, 110, 1 )
int_data <- data.table( expand.grid( x = x, y = y ) )
setorder( int_data, x, y )

## Interpolación utilizando Kriging ----------------------------------------------------------------
# Emplea maximización de verosimilitud, utiliza el método de optimización BFGS.
nup_krig <- km( formula = ~1, 
                design = nup_dat_prep[ , list( x, y ) ], 
                response = nup_dat_prep$u_obs, 
                estim.method = 'MLE', 
                covtype = 'matern5_2',
                optim.method = 'BFGS', 
                noise.var = nup_dat_prep$var )

nup_krig_pred <- predict( nup_krig, newdata = int_data, type = "SK" )

aux <- copy( int_data )
aux[ , u := nup_krig_pred$mean ]
nup_dat <- merge.data.table(
   nup_dat,
   aux,
   by = c( 'x', 'y' ),
   all.y = TRUE
)
rm( aux )
nup_dat[ , q := exp( -exp( u ) ) ]
nup_dat[ abs( x - y ) > 40, q := 0 ]
nup_dat[ , r := u - u_obs ]

## Gráficos de verificación ------------------------------------------------------------------------
chk <- nup_dat[ , list( q = sum( q, na.rm = TRUE ), q_obs = sum( q_obs, na.rm = TRUE ) ), by = list( x ) ]
plot( chk$x, chk$q, type = 'l', lwd = 2, col = "navyblue", ylim = c( 0, 1 ) )
points( chk$x, chk$q_obs, pch = 16, cex = 0.8, col = "darkgreen" )

# hist( nup_dat$r, breaks = 100, xlim = c( -0.1, 0.1 ), probability = TRUE )
# quantile( nup_dat$r, probs = seq( 0, 1, 0.1 ), na.rm = TRUE )
# plot( sort( nup_dat$r ), ylim = c( -0.1, 0.1 ) )

nup_surf <- dcast.data.table( data = nup_dat, formula = x ~ y, value.var = 'q' )
nup_surf[ , x := NULL ]
nup_surf <- as.matrix( nup_surf )
persp3d( x, y, nup_surf, col = 'gold', alpha = 0.7, xlim = range( x ), ylim = range( y ), zlim = c( -0.05, 0.1 ) )
points3d( nup_dat$x, nup_dat$y, nup_dat$q_obs, col = 'red', add = TRUE )

# Guarda resultados --------------------------------------------------------------------------------
save( cen_2010,
      fer_dat,
      fer_krig_hh, fer_krig_hm, fer_krig_mh, fer_krig_mm,
      fer_krig_hh_pred, fer_krig_hm_pred, fer_krig_mh_pred, fer_krig_mm_pred,
      nup_krig,
      nup_krig_pred,
      nup_dat,
      file = parametros$demo_rdata_inec_fert_model_todo )

save( fer_dat,
      nup_dat,
      file = parametros$demo_rdata_inec_fert_model )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()
