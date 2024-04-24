message( paste( rep('-', 100 ), collapse = '' ) )

# Descripción estados ------------------------------------------------------------------------------
# 1 = Individuos en la PEA no activos
# 2 = Activos cotizantes
# 3 = Activos cesantes
# 4 = Pensionistas de vejez
# 5 = Pensionistas de invalidez
# 6 = Muertos
# 7 = Pensionistas de montepío viudas
# 8 = Pensionistas de montepío huérfanos
# 9 = Dependientes cónyuges
# 10 = Dependientes hijos
# 11 = Dependientes hijos menores de 18 años

# Descripción de campos del SGRT--------------------------------------------------------------------
# 12 = pensionistas de riesgos del trabajo;
# 13 = indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = montepíos de orfandad de riesgos del trabajo;
# 16 = montepíos de viudedad de riesgos del trabajo.

#0. Cargando información ----------------------------------------------------------------------------
message( '\tCargando datos' )

load( parametros$demo_rdata_sgo_pob_proy )
load( parametros$demo_rdata_rtr_tasas_tran  )
load( parametros$demo_rdata_rtr_tran_prep )
load( paste0( parametros$RData_seg, 'IESS_RTR_pensionistas_iniciales.RData' ) )
load( parametros$demo_rdata_inec_fert_model )

#Eliminando data frames no usado
rm( list = ls()[ !(ls() %in% c( "parametros",
                                "tas_2_12",
                                "tas_12_6",
                                "tas_2_13",
                                "tas_2_14",
                                "tas_0_15",
                                "tas_15_0",
                                "tas_0_16",
                                "tas_16_0",
                                "pob_proy",
                                "porcentaje_viudas",
                                "porcentaje_pensionistas_hombres",
                                "tas_din_dec",
                                "sgrt_pen_tran_orf_anio",
                                "l_12_ini",
                                "l_15_ini",
                                "l_16_ini",
                                "cen_iess_hij_alis",
                                "cen_iess_cony_alis",
                                "fer_dat",
                                "nup_dat" ) ) ] )

# 0.1. Tablas auxiliares----------------------------------------------------------------------------
cal_ing_viudez <- 1
cal_sal_viudez <- 1

cal_ing_orf <- 1
cal_sal_orf <- 1

cal_ing_pen <- 1
cal_sal_pen <- 1

age_grid <- seq( 0, 105, 1)

l_2 <- pob_proy %>% 
  dplyr::select( t, sexo, x, l2 )

l_12_ini <- l_12_ini %>% 
  dplyr::select( t, sexo, x, l_12 := l_12_int )

l_15_ini <- l_15_ini %>% 
  dplyr::select( t, sexo, x, l_15 := l_15_int )

l_16_ini <- l_16_ini %>% 
  dplyr::select( t, sexo, x, l_16 := l_16_int )

l_10 <- pob_proy %>% 
  dplyr::select( t, sexo, x, l10, l11 )


cen_iess_hij_alis <- fer_dat %>% 
  dplyr::select( -sexo, -sexo_dep ) %>% 
  mutate( sexo = c( rep( 'H' , nrow( . )/4 ),
                    rep( 'H' , nrow( . )/4 ),
                    rep( 'M' , nrow( . )/4 ),
                    rep( 'M' , nrow( . )/4 ) ) ) %>% 
  mutate( sexo_dep = c( rep( 'H' , nrow( . )/4 ),
                        rep( 'M' , nrow( . )/4 ),
                        rep( 'H' , nrow( . )/4 ),
                        rep( 'M' , nrow( . )/4 ) ) ) %>% 
  dplyr::select( -u )


cen_iess_cony_alis <- nup_dat %>%  dplyr::select( -u )

l_11 <- expand.grid( t = c( 0: parametros$horizonte ),
                        sexo = c( 'H', 'M' ),
                        x = seq( 15, 105, 1 ) ) %>% 
  left_join( ., l_2, by = c( 't', 'sexo', 'x' ) ) %>% 
  left_join( ., cen_iess_hij_alis, by = c( 'x' = 'x', 'sexo' = 'sexo' ), relationship = "many-to-many" ) %>%
  mutate( hij = q * l2 ) %>% 
  group_by( t, sexo_dep, y ) %>% 
  mutate( l11 = sum( hij, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, sexo_dep, y, .keep_all = TRUE ) %>% 
  dplyr::select( t,
                 sexo := sexo_dep,
                 x:=y,
                 l11 ) %>% 
  mutate( l11 = if_else( x >= 18,
                             0,
                             l11 ) ) 


l_9 <- expand.grid( t = c( 0: parametros$horizonte ),
                    sexo = c( 'H', 'M' ),
                    x = seq( 15, 105, 1 ) ) %>% 
  left_join( ., l_2, by = c( 't', 'sexo', 'x' ) ) %>% 
  left_join( ., cen_iess_cony_alis, by = c( 'x' = 'x' ), relationship = "many-to-many" ) %>% 
  mutate( cony = q * l2 ) %>% 
  group_by( t, sexo, y ) %>% 
  mutate( l9 = sum( cony, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, sexo, y, .keep_all = TRUE ) %>% 
  dplyr::select( t,
                 sexo,
                 x := y,
                 l9 ) 

#1. Pensionistas de incapacidad PA, PT y PP---------------------------------------------------------
#Ingresos y salidas

l_12 <- l_12_ini %>% 
  full_join( ., expand_grid( x = age_grid,
                             sexo = c( 'H', 'M' ),
                             t = 0:parametros$horizonte ), by = c( 'x', 't', 'sexo' ) ) %>% 
  full_join( ., l_2, by = c( 't', 'x', 'sexo' ) ) %>% 
  dplyr::select( t, x, sexo, l_12, l2 ) %>% 
  full_join( ., tas_12_6 %>% dplyr::select( x, sexo, t_12_6 := t_12_6_int ), by = c( 'x', 'sexo' ) ) %>% 
  full_join( ., tas_2_12 %>% dplyr::select( x, sexo, t_2_12 := t_2_12_int ), by = c( 'x', 'sexo' ) ) %>% 
  arrange( t, sexo, x ) %>%
  mutate( t_2_12 = if_else( t == 0,
                            0,
                            t_2_12 ) ) %>% 
  mutate( l_2_12 = l2 * t_2_12 * cal_ing_pen,
          l_12_6 = 0,
          l_12_12 = 0,
          l_12 = if_else( t > 0,
                          0,
                          l_12 ) ) %>% 
  filter( !is.na( x ) ) %>% 
  replace( is.na( . ), 0 )

t_12_6 <- xtabs(  t_12_6 ~ t + x + sexo, l_12 )

l12 <- xtabs( l_12 ~ t + x + sexo, l_12 )

l_12_12  <- xtabs( l_12_12  ~ t + x + sexo, l_12 )

l_2_12  <- xtabs( l_2_12  ~ t + x + sexo, l_12 )

l_12_6  <- xtabs( l_12_6  ~ t + x + sexo, l_12 )


for ( g in c( 1, 2 ) ) {
  for ( t in c( 1: parametros$horizonte ) ) {
    for ( x in c( 1: 105 ) ) { 
      l_12_12[ t + 1, x + 1, g ] <- l12[ t, x, g ] * ( 1 - cal_sal_pen * t_12_6[ t, x, g ] )
      l_12_6[ t + 1, x + 1, g ] <- l12[ t, x, g ] * cal_sal_pen * t_12_6[ t, x, g ]
      l12[ t + 1, x + 1, g ] <- l_12_12[ t + 1, x + 1, g ] + l_2_12[ t + 1, x + 1, g ]
    }
  }
}

l12 <- data.frame( l12 ) %>% 
  dplyr::select(  t, x, sexo, l_12 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_12_12 <- data.frame( l_12_12 ) %>% 
  dplyr::select(  t, x, sexo, l_12_12 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_12_6 <- data.frame( l_12_6 ) %>% 
  dplyr::select(  t, x, sexo, l_12_6 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_12 <- l_12 %>% 
  dplyr::select( -l_12, -l_12_6, -l_12_12 ) %>% 
  full_join( ., l12, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., l_12_6, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., l_12_12, by = c( 't', 'x', 'sexo' ) ) %>% 
  dplyr::select( t, sexo, x, l_12, l_12_6, l_2_12, l_12_12 )

#Comprobación
l_12_anual <- l_12 %>% 
  group_by( t ) %>% 
  mutate( l_12 = sum( l_12, na.rm = TRUE ),
          l_12_6 = sum( l_12_6, na.rm = TRUE ),
          l_2_12 = sum( l_2_12, na.rm = TRUE ),
          l_12_12 = sum( l_12_12, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, .keep_all = TRUE ) %>% 
  dplyr::select( t, l_12, l_12_6, l_2_12, l_12_12 )

#2. Beneficios de Indemnizaciones-------------------------------------------------------------------
# Hombres, en 2020 hay 303 indemnizaciones
# Mujeres hay  40 indemnizaciones en 2020

l_13 <- tas_2_13 %>% 
  full_join( ., expand_grid( x = age_grid,
                             sexo = c( 'H', 'M' ),
                             t = 0:parametros$horizonte ), by = c( 'x', 'sexo' ) ) %>% 
  dplyr::select( t, x, sexo, t_2_13 := t_2_13_int ) %>% 
  left_join( ., l_2, by = c( 'x', 't', 'sexo' ) ) %>% 
  mutate( l_13 = t_2_13 * l2 ) %>% 
  dplyr::select( t, sexo, x, l_13 )  %>% 
  replace( is.na( . ), 0 )

#Comprobación
aux <- l_13 %>% 
  group_by( t, sexo ) %>% 
  mutate( l_13 = sum( l_13, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, l_13 ) %>% 
  arrange( t, sexo )


#3. Beneficios de Subsidios-------------------------------------------------------------------------
# Hombres hay 208.627 subsidios pagados entre 2012 y 2020; y 11.442 subsidios pagados en 2020   
# Mujeres 43.135 subsidios entre 2012 a 2020, en 2020 hay 2.400

l_14 <- tas_2_14 %>% 
  full_join( ., expand_grid( x = age_grid,
                             sexo = c( 'H', 'M' ),
                             t = 0:parametros$horizonte ), by = c( 'x', 'sexo' ) ) %>% 
  dplyr::select( t, x, sexo, t_2_14 := t_2_14_int ) %>% 
  left_join( ., l_2, by = c( 'x', 't', 'sexo' ) ) %>% 
  mutate( l_14 = t_2_14 * l2 ) %>% 
  dplyr::select( t, sexo, x, l_14 )  %>% 
  replace( is.na( . ), 0 )

#Comprobación
aux <- l_14 %>% 
  group_by( t, sexo ) %>% 
  mutate( l_14 = sum( l_14, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, l_14 ) %>% 
  arrange( t, sexo )

#4. Pensionistas de orfandad------------------------------------------------------------------------

#Ingresos y salidas

l_15 <- l_15_ini %>% 
  full_join( ., expand_grid( x = age_grid,
                             sexo = c( 'H', 'M' ),
                             t = 0:parametros$horizonte ), by = c( 'x', 't', 'sexo' ) ) %>% 
  full_join( ., l_11, by = c( 't', 'x', 'sexo' ) ) %>% 
  dplyr::select( t, x, sexo, l_15, l11 ) %>% 
  full_join( ., tas_15_0 %>% dplyr::select( x, sexo, t_15_0 := t_15_0_int ), by = c( 'x', 'sexo' ) ) %>% 
  full_join( ., tas_0_15 %>% dplyr::select( x, sexo, t_0_15 := t_0_15_int ), by = c( 'x', 'sexo' ) ) %>% 
  arrange( t, sexo, x ) %>%
  mutate( t_0_15 = if_else( t == 0,
                            0,
                            t_0_15 ) ) %>% 
  mutate( l_0_15 = l11 * t_0_15 * cal_ing_orf,
          l_15_0 = 0,
          l_15_15 = 0,
          l_15 = if_else( t > 0,
                          0,
                          l_15 ) ) %>% 
  filter( !is.na( x ) ) %>% 
  replace( is.na( . ), 0 )

t_15_0 <- xtabs(  t_15_0 ~ t + x + sexo, l_15 )

l11 <- xtabs( l11 ~ t + x + sexo, l_15 )

l_0_15  <- xtabs( l_0_15  ~ t + x + sexo, l_15 )

l_15_0  <- xtabs( l_15_0  ~ t + x + sexo, l_15 )

l_15_15  <- xtabs( l_15_15  ~ t + x + sexo, l_15 )

l15 <- xtabs( l_15 ~ t + x + sexo, l_15 )

for ( g in c( 1, 2 ) ) {
  for ( t in c( 1: parametros$horizonte ) ) {
    for ( x in c( 1: 105 ) ) { 
      l_15_15[ t + 1, x + 1, g ] <- l15[ t, x, g ] * ( 1 - cal_sal_orf * t_15_0[ t, x, g ] )
      l_15_0[ t + 1, x + 1, g ] <- l15[ t, x, g ] * cal_sal_orf * t_15_0[ t, x, g ]
      l15[ t + 1, x + 1, g ] <- l_15_15[ t + 1, x + 1, g ] + l_0_15[ t + 1, x + 1, g ]
    }
  }
}

l15 <- data.frame( l15 ) %>% 
  dplyr::select(  t, x, sexo, l_15 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_15_0 <- data.frame( l_15_0 ) %>% 
  dplyr::select(  t, x, sexo, l_15_0 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_15_15 <- data.frame( l_15_15 ) %>% 
  dplyr::select(  t, x, sexo, l_15_15 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_15 <- l_15 %>% 
  dplyr::select( -l_15, -l_15_0, -l_15_15 ) %>% 
  full_join( ., l15, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., l_15_0, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., l_15_15, by = c( 't', 'x', 'sexo' ) ) %>% 
  dplyr::select( t, sexo, x, l_15, l_15_0, l_0_15, l_15_15 )

#Comprobación
l_15_anual <- l_15 %>% 
  group_by( t ) %>% 
  mutate( l_15 = sum( l_15, na.rm = TRUE ),
          l_15_0 = sum( l_15_0, na.rm = TRUE ),
          l_0_15 = sum( l_0_15, na.rm = TRUE ),
          l_15_15 = sum( l_15_15, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, .keep_all = TRUE ) %>% 
  dplyr::select( t, l_15, l_15_0, l_0_15, l_15_15 )

#5. Pensionistas de viudedad------------------------------------------------------------------------

l_16 <- l_16_ini %>% 
  full_join( ., expand_grid( x = c( 15 : 105 ),
                             sexo = c( 'H', 'M' ),
                             t = 0:parametros$horizonte ), by = c( 'x', 't', 'sexo' ) ) %>% 
  left_join( ., l_9, by = c( 't', 'x', 'sexo' ) ) %>% 
  dplyr::select( t, x, sexo, l_16, l9 ) %>% 
  full_join( ., tas_16_0 %>% dplyr::select( x, sexo, t_16_0 := t_16_0_int ), by = c( 'x', 'sexo' ) ) %>% 
  full_join( ., tas_0_16 %>% dplyr::select( x, sexo, t_0_16 := t_0_16_int ), by = c( 'x', 'sexo' ) ) %>% 
  arrange( t, sexo, x ) %>%
  mutate( t_0_16 = if_else( t == 0,
                            0,
                            t_0_16 ) ) %>% 
  mutate( l_0_16 = l9 * t_0_16 * cal_ing_viudez,
          l_16_0 = 0,
          l_16_16 = 0,
          l_16 = if_else( t > 0,
                          0,
                          l_16 ) ) %>% 
  filter( !is.na( x ) ) %>% 
  replace( is.na( . ), 0 )

t_16_0 <- xtabs(  t_16_0 ~ t + x + sexo, l_16 )

l9 <- xtabs( l9 ~ t + x + sexo, l_16 )

l_0_16  <- xtabs( l_0_16  ~ t + x + sexo, l_16 )

l_16_0  <- xtabs( l_16_0  ~ t + x + sexo, l_16 )

l_16_16  <- xtabs( l_16_16  ~ t + x + sexo, l_16 )

l16 <- xtabs( l_16 ~ t + x + sexo, l_16 )

for ( g in c( 1, 2 ) ) {
  for ( t in c( 1: parametros$horizonte ) ) {
    for ( x in c( 1 : 90 ) ) { 
      l_16_16[ t + 1, x + 1, g ] <- l16[ t, x, g ] * ( 1 - cal_sal_viudez * t_16_0[ t, x, g ] )
      l_16_0[ t + 1, x + 1, g ] <- l16[ t, x, g ] * cal_sal_viudez * t_16_0[ t, x, g ]
      l16[ t + 1, x + 1, g ] <- l_16_16[ t + 1, x + 1, g ] + l_0_16[ t + 1, x + 1, g ]
    }
  }
}

l16 <- data.frame( l16 ) %>% 
  dplyr::select(  t, x, sexo, l_16 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_16_0 <- data.frame( l_16_0 ) %>% 
  dplyr::select(  t, x, sexo, l_16_0 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_16_16 <- data.frame( l_16_16 ) %>% 
  dplyr::select(  t, x, sexo, l_16_16 := Freq ) %>% 
  mutate( t = as.integer( t ),
          x = as.integer( x ),
          sexo = as.character( sexo ) ) %>% 
  mutate( t = t - 1,
          x = x - 1 )

l_16 <- l_16 %>% 
  dplyr::select( -l_16, -l_16_0, -l_16_16 ) %>% 
  full_join( ., l16, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., l_16_0, by = c( 't', 'x', 'sexo' ) ) %>% 
  full_join( ., l_16_16, by = c( 't', 'x', 'sexo' ) ) %>% 
  dplyr::select( t, sexo, x, l_16, l_16_0, l_0_16, l_16_16 )

#Comprobación
l_16_anual <- l_16 %>% 
  group_by( t ) %>% 
  mutate( l_16 = sum( l_16, na.rm = TRUE ),
          l_16_0 = sum( l_16_0, na.rm = TRUE ),
          l_0_16 = sum( l_0_16, na.rm = TRUE ),
          l_16_16 = sum( l_16_16, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, .keep_all = TRUE ) %>% 
  dplyr::select( t, l_16, l_16_0, l_0_16, l_16_16 )

# 6. Consolidación junto al Rdata de proyecciones de población de IVM-------------------------------

pob_proy_rtr <- pob_proy %>% 
  left_join( ., l_12, by = c( 't', 'sexo', 'x' ) ) %>% 
  left_join( ., l_13, by = c( 't', 'sexo', 'x' ) ) %>% 
  left_join( ., l_14, by = c( 't', 'sexo', 'x' ) ) %>% 
  left_join( ., l_15, by = c( 't', 'sexo', 'x' ) ) %>% 
  left_join( ., l_16, by = c( 't', 'sexo', 'x' ) ) %>% 
  replace( is.na( . ), 0 )

# 7 Agrupado por sexo y tiempo----------------------------------------------------------------------

pob_proy_tot_sex <- pob_proy_rtr %>% 
  group_by( t, sexo ) %>% 
  mutate( l_12 = sum( l_12, na.rm =  TRUE ),
          l_12_6 = sum( l_12_6, na.rm =  TRUE ),
          l_2_12 = sum( l_2_12, na.rm =  TRUE ),
          l_12_12 = sum( l_12_12, na.rm = TRUE ),
          l_13 = sum( l_13, na.rm =  TRUE ),
          l_14 = sum( l_14, na.rm =  TRUE ),
          l_15 = sum( l_15, na.rm =  TRUE ),
          l_15_0 = sum( l_15_0, na.rm =  TRUE ),
          l_0_15 = sum( l_0_15, na.rm =  TRUE ),
          l_15_15 = sum( l_15_15, na.rm = TRUE ),
          l_16 = sum( l_16, na.rm =  TRUE ),
          l_16_0 = sum( l_16_0, na.rm =  TRUE ),
          l_0_16 = sum( l_0_16, na.rm =  TRUE ),
          l_16_16 = sum( l_16_16, na.rm =  TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, l_12, l_12_6, l_2_12, l_12_12, l_13, l_14, l_14, l_15, l_15_0, l_0_15, l_15_15, l_16, l_16_0, l_0_16, l_16_16 )
  
# Guardo resultados --------------------------------------------------------------------------------
message( '\tGuardando proyección de población' )
save( pob_proy_rtr,
      pob_proy_tot_sex,
      file = parametros$demo_rdata_rtr_pob_proy )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

