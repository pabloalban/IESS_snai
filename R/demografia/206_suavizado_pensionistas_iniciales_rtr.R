message( paste( rep('-', 100 ), collapse = '' ) )

# Descripción de campos ----------------------------------------------------------------------------
# 12 = pensionistas de riesgos del trabajo;
# 13 = indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = montepíos de orfandad de riesgos del trabajo;
# 16 = montepíos de viudedad de riesgos del trabajo.

#0.Cargando datos-----------------------------------------------------------------------------------
message( '\tCargando transiciones de los pensionistas del SGRT' )
load( paste0( parametros$RData_seg, 'IESS_RTR_rentas.RData' ) )

#Eliminando data frames no usado
rm( list = ls()[ !(ls() %in% c( "parametros",
                                "prestaciones_orfandad",
                                "prestaciones_pa",
                                "prestaciones_pp",
                                "prestaciones_pt",
                                "prestaciones_viudez" ) ) ] )

#Parámetros

anio_fin <- 2020
mes_fin <- 12
fecha_corte <- as.Date( paste0( anio_fin, "-", mes_fin, "-12") )
#1. Pensionistas iniciales de PA, PT y PP-----------------------------------------------------------

l_12_ini <- rbind( prestaciones_pa,
                     prestaciones_pp,
                     prestaciones_pt ) %>% 
  filter( anio == anio_fin, mes == mes_fin ) %>% 
  distinct( anio, mes, cedula, .keep_all = TRUE ) %>% 
  mutate( x = round( as.numeric( difftime( fecha_corte, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>%   mutate( t = 0 ) %>% 
  group_by( x, sexo ) %>% 
  mutate( l_12 = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( x, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, x, l_12 ) %>% 
  arrange( sexo, x  ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') ) %>% 
  full_join( expand.grid( t = 0,
             sexo = c( 'H', 'M' ),
             x = c( 15:105 ) ), ., by = c( 't', 'sexo', 'x' ) ) %>% 
  group_by( sexo ) %>% 
  mutate( ERx_incap_2020 = sum( l_12, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  mutate( l_12 = ifelse( is.na( l_12 ),
                           0,
                           l_12 ) )

#Hombres--------------------------------------------------------------------------------------------
l_12_ini_h <- l_12_ini %>% 
  filter( sexo == 'H' )

aux <- l_12_ini_h %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '27', '97' ) ) )

plot( aux$x, aux$l_12 )


mod<-smooth.spline( aux$x,
                    aux$l_12, df = 8 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    l_12_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( l_12_int = if_else( l_12_int < 0,
                                0,
                                l_12_int ) )

l_12_ini_h <- expand.grid( t = 0,
                             sexo = c( 'H' ),
                             x = c( 15:105 ) )%>% 
  full_join( ., l_12_ini_h, by = c( 'x', 'sexo', 't' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( l_12_int = ( l_12_int * ERx_incap_2020 /  sum ( l_12_int, na.rm = TRUE ) ) ) %>% 
  mutate( l_12_int = if_else( is.na( l_12_int ),
                                0,
                                l_12_int ) )

#Gráfico de Control
plot( l_12_ini_h$x, l_12_ini_h$l_12 )
lines( l_12_ini_h$x, l_12_ini_h$l_12_int )

#Comprobación
sum ( l_12_ini_h$l_12_int, na.rm = TRUE )
sum ( l_12_ini_h$l_12, na.rm = TRUE )

#Mujeres--------------------------------------------------------------------------------------------
l_12_ini_m <- l_12_ini %>% 
  filter( sexo == 'M' )

aux <- l_12_ini_m %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '64', '62' ) ) )

plot( aux$x, aux$l_12 )


mod<-smooth.spline( aux$x,
                    aux$l_12, df = 8 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    l_12_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( l_12_int = if_else( l_12_int < 0,
                                0,
                                l_12_int ) )

l_12_ini_m <- expand.grid( t = 0,
                             sexo = c( 'M' ),
                             x = c( 15:105 ) )%>% 
  full_join( ., l_12_ini_m, by = c( 'x', 'sexo', 't' ) ) %>%
  full_join( ., pred, by = 'x' ) %>% 
  mutate( l_12_int = ( l_12_int * ERx_incap_2020 /  sum ( l_12_int, na.rm = TRUE ) ) ) %>% 
  mutate( l_12_int = if_else( is.na( l_12_int ),
                                0,
                                l_12_int ) )

#Gráfico de control
plot( l_12_ini_m$x, l_12_ini_m$l_12 )
lines( l_12_ini_m$x, l_12_ini_m$l_12_int )

#Comprobación
sum ( l_12_ini_m$l_12_int, na.rm = TRUE )
sum ( l_12_ini_m$l_12, na.rm = TRUE )

#2. Pensionistas iniciales de orfandad--------------------------------------------------------------

l_15_ini <- prestaciones_orfandad %>% 
  filter( anio == anio_fin, mes == mes_fin ) %>% 
  distinct( anio, mes, cedula, .keep_all = TRUE ) %>% 
  mutate( x = round( as.numeric( difftime( fecha_corte, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>%   mutate( t = 0 ) %>% 
  group_by( x, sexo ) %>% 
  mutate( l_15 = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( x, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, x, l_15 ) %>% 
  arrange( sexo, x  ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') ) %>% 
  full_join( expand.grid( t = 0,
                          sexo = c( 'H', 'M' ),
                          x = c( 0:105 ) ), ., by = c( 't', 'sexo', 'x' ) ) %>% 
  group_by( sexo ) %>% 
  mutate( ERx_incap_2020 = sum( l_15, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  mutate( l_15 = ifelse( is.na( l_15 ),
                           0,
                           l_15 ) )

#Hombres--------------------------------------------------------------------------------------------
l_15_ini_h <- l_15_ini %>% 
  filter( sexo == 'H' )

aux <- l_15_ini_h %>% 
  na.omit( . )

plot( aux$x, aux$l_15 )

aux_1 <- aux %>%  filter( x <= 18 )
aux_2 <- aux %>%  filter( x > 18 )


mod<-smooth.spline( aux_1$x,
                    aux_1$l_15, df = 6 ) 

pred_1 <- data.frame( x = seq( 0, 18, 1 ), 
                    l_15_int = predict( mod, seq( 0, 18, 1 ), deriv = 0)[["y"]] ) %>% 
  mutate( l_15_int = if_else( l_15_int < 0,
                                0,
                                l_15_int ) )

mod<-smooth.spline( aux_2$x,
                    aux_2$l_15, df = 8 ) 

pred_2 <- data.frame( x = seq( 19, 105, 1 ), 
                    l_15_int = predict( mod, seq( 19, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( l_15_int = if_else( l_15_int < 0,
                                0,
                                l_15_int ) )

pred <- rbind( pred_1, pred_2 )

l_15_ini_h <- left_join( l_15_ini_h, pred, by = 'x' ) %>%
  mutate( l_15_int = if_else( l_15_int < 0,
                                l_15,
                                l_15_int ) ) %>% 
  mutate( l_15_int = ( l_15_int * ERx_incap_2020 /  sum ( l_15_int, na.rm = TRUE ) ) )

#Gráfico de Control
plot( l_15_ini_h$x, l_15_ini_h$l_15 )
lines( l_15_ini_h$x, l_15_ini_h$l_15_int )

#Comprobación
sum ( l_15_ini_h$l_15_int, na.rm = TRUE )
sum ( l_15_ini_h$l_15, na.rm = TRUE )

#Mujeres--------------------------------------------------------------------------------------------
l_15_ini_m <- l_15_ini %>% 
  filter( sexo == 'M' )

aux <- l_15_ini_m %>% 
  na.omit( . )

plot( aux$x, aux$l_15 )

aux_1 <- aux %>%  filter( x <= 18 )
aux_2 <- aux %>%  filter( x > 18 )


mod<-smooth.spline( aux_1$x,
                    aux_1$l_15, df = 6 ) 

pred_1 <- data.frame( x = seq( 0, 18, 1 ), 
                      l_15_int = predict( mod, seq( 0, 18, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( l_15_int = if_else( l_15_int < 0,
                                0,
                                l_15_int ) )

mod<-smooth.spline( aux_2$x,
                    aux_2$l_15, df = 8 ) 

pred_2 <- data.frame( x = seq( 19, 105, 1 ), 
                      l_15_int = predict( mod, seq( 19, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( l_15_int = if_else( l_15_int < 0,
                                0,
                                l_15_int ) )

pred <- rbind( pred_1, pred_2 )

l_15_ini_m <- left_join( l_15_ini_m, pred, by = 'x' ) %>%
  mutate( l_15_int = if_else( l_15_int < 0,
                                l_15,
                                l_15_int ) ) %>% 
  mutate( l_15_int = ( l_15_int * ERx_incap_2020 /  sum ( l_15_int, na.rm = TRUE ) ) )

#Gráficos de control
plot( l_15_ini_m$x, l_15_ini_m$l_15 )
lines( l_15_ini_m$x, l_15_ini_m$l_15_int )

#Comprobación
sum ( l_15_ini_m$l_15_int, na.rm = TRUE )
sum ( l_15_ini_m$l_15, na.rm = TRUE )

#3. Pensionistas iniciales de viudedad----------------------------------------------------------------

l_16_ini <- prestaciones_viudez %>% 
  filter( anio == anio_fin, mes == mes_fin ) %>% 
  distinct( anio, mes, cedula, .keep_all = TRUE ) %>% 
  mutate( x = round( as.numeric( difftime( fecha_corte, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>%   mutate( t = 0 ) %>% 
  group_by( x, sexo ) %>% 
  mutate( l_16 = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( x, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, x, l_16 ) %>% 
  arrange( sexo, x  ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') ) %>% 
  full_join( expand.grid( t = 0,
                          sexo = c( 'H', 'M' ),
                          x = c( 15:105 ) ), ., by = c( 't', 'sexo', 'x' ) ) %>% 
  group_by( sexo ) %>% 
  mutate( ERx_incap_2020 = sum( l_16, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  mutate( l_16 = ifelse( is.na( l_16 ),
                           0,
                           l_16 ) )
#Hombres--------------------------------------------------------------------------------------------
l_16_ini_h <- l_16_ini %>% 
  filter( sexo == 'H' ) 

aux <- l_16_ini_h %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '8' ) ) )

plot( aux$x, aux$l_16 )

mod<-smooth.spline( aux$x,
                    aux$l_16, df = 4 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    l_16_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] ) %>%
  mutate( l_16_int = if_else( l_16_int < 0,
                                0,
                                l_16_int ) ) 

l_16_ini_h <- expand.grid( t = 0,
                             sexo = c( 'H' ),
                             x = c( 15 : 105 ) ) %>% 
  full_join( ., l_16_ini_h, by = c( 't', 'sexo', 'x' ) ) %>%
  full_join( ., pred, by = 'x' ) %>%
  mutate( l_16_int = if_else( l_16_int < 0,
                                l_16,
                                l_16_int ),
          ERx_incap_2020 = mean( ERx_incap_2020, na.rm = TRUE ) ) %>% 
  mutate( l_16_int = ( l_16_int * ERx_incap_2020 /  sum ( l_16_int, na.rm = TRUE ) ) ) 

#Gráfico de control
plot( l_16_ini_h$x, l_16_ini_h$l_16 )
lines( l_16_ini_h$x, l_16_ini_h$l_16_int )

#Comprobación
sum ( l_16_ini_h$l_16_int, na.rm = TRUE )
sum ( l_16_ini_h$l_16, na.rm = TRUE )


#Mujeres--------------------------------------------------------------------------------------------
l_16_ini_m <- l_16_ini %>% 
  filter( sexo == 'M' )

aux <- l_16_ini_m %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '106' ) ) )

plot( aux$x, aux$l_16 )

mod<-smooth.spline( aux$x,
                    aux$l_16, df = 6 ) 

pred <- data.frame( x = c( 15 : 105 ), 
                    l_16_int = predict( mod, c( 15 : 105 ), deriv = 0)[["y"]] ) %>% 
  mutate( l_16_int = if_else( l_16_int < 0,
                                0,
                                l_16_int ) )

l_16_ini_m <- expand.grid( t = 0,
                             sexo = c( 'M' ),
                             x = c( 15 : 105 ) ) %>% 
  full_join( ., l_16_ini_m, by = c( 't', 'sexo', 'x' ) ) %>%
  full_join( ., pred, by = 'x' ) %>%
  mutate( l_16_int = if_else( l_16_int < 0,
                                l_16,
                                l_16_int ) ) %>% 
  mutate( l_16_int = ( l_16_int * ERx_incap_2020 /  sum( l_16_int, na.rm = TRUE ) ) ) %>% 
  mutate( l_16_int = ifelse( is.na( l_16_int ),
                                0,
                                l_16_int ) )

#Gráfico de control
plot( l_16_ini_m$x, l_16_ini_m$l_16 )
lines( l_16_ini_m$x, l_16_ini_m$l_16_int )

#Comprobación
sum ( l_16_ini_m$l_16_int, na.rm = TRUE )
sum ( l_16_ini_m$l_16, na.rm = TRUE )

# 4. Consolidación----------------------------------------------------------------------------------

l_12_ini <- rbind( l_12_ini_h, l_12_ini_m )
l_15_ini <- rbind( l_15_ini_h, l_15_ini_m )
l_16_ini <- rbind( l_16_ini_h, l_16_ini_m )

# Guarda resultados --------------------------------------------------------------------------------
message( '\tGuardando suavizamiento de tasas' )
save( l_12_ini,
      l_15_ini,
      l_16_ini,
      file = paste0( parametros$RData_seg, 'IESS_RTR_pensionistas_iniciales.RData' ) )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )