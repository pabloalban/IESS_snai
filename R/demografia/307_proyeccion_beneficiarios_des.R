message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tProyección del número de beneficiarios por pago de Desempleo' )

#0. Cargando información ---------------------------------------------------------------------------
message( '\tCargando datos' )

load( parametros$demo_rdata_sgo_pob_proy )
load( parametros$des_rdata_tasa_siniestralidad )

#Eliminando data frames no usado
rm( list = ls()[ !(ls() %in% c( "parametros",
                                "p_i_int",
                                "pob_proy" ) ) ] )

## 0.1 Parámetros-----------------------------------------------------------------------------------
age_grid <- seq( 0, 105, 1 )
sex_grid <- c( 'M', 'H' )
hor_grid <- seq( 0, parametros$des_horizonte, 1 )
cal_ben_des <- data.frame( t = hor_grid,
                           cal_ben_des= c( 1, 
                                           1,
                                           1,
                                           rep( 1, 38 ) ) )

## 0.2 Tablas auxiliares----------------------------------------------------------------------------

l_2_3 <- pob_proy %>% 
  dplyr::select( t, sexo, x, l2_3 )

p_i_int <- p_i_int %>% 
  mutate( p_i = pi_int,
          i = as.character( i ) ) %>% 
  dplyr::select( -pi_int, -pi ) %>% 
  pivot_wider( names_from = i, values_from = p_i, names_prefix = "p_" ) %>%
  full_join( ., expand.grid( t = hor_grid,
                             x = age_grid,
                             sexo = sex_grid ), by = c( 'x', 'sexo' ) ) %>% 
  dplyr::select( t, sexo, x, p_1, p_2, p_3, p_4, p_5 )

#1. Proyección de beneficiarios de SD por pago------------------------------------------------------

pob_proy_des <- l_2_3 %>% 
  full_join( ., cal_ben_des, by = c( 't' ) ) %>% 
  full_join( ., p_i_int, by = c( 't', 'sexo', 'x' ) ) %>% 
  mutate( l_p1 = cal_ben_des * l2_3 * p_1,
          l_p2 = cal_ben_des * l2_3 * p_2,
          l_p3 = cal_ben_des * l2_3 * p_3,
          l_p4 = cal_ben_des * l2_3 * p_4,
          l_p5 = cal_ben_des * l2_3 * p_5 )

#2. Proyección de beneficiarios de SD anual---------------------------------------------------------

pob_proy_des_anual <- pob_proy_des %>% 
  group_by( t ) %>% 
  mutate( l_p1 = sum( l_p1, na.rm = TRUE ),
          l_p2 = sum( l_p2, na.rm = TRUE ),
          l_p3 = sum( l_p3, na.rm = TRUE ),
          l_p4 = sum( l_p4, na.rm = TRUE ),
          l_p5 = sum( l_p5, na.rm = TRUE ) ) %>% 
  distinct( t, .keep_all = TRUE ) %>% 
  dplyr::select( t, l_p1, l_p2, l_p3, l_p4, l_p5 )

# Guardo resultados --------------------------------------------------------------------------------
message( '\tGuardando proyección de población' )
save( pob_proy_des,
      file = parametros$demo_rdata_des_pob_proy )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

