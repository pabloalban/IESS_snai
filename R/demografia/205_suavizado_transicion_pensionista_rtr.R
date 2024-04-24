message( paste( rep( '-', 100 ), collapse = '' ) )

# Descripción de campos ----------------------------------------------------------------------------
# 12 = Pensionistas de riesgos del trabajo;
# 13 = Indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = Montepíos de orfandad de riesgos del trabajo;
# 16 = Montepíos de viudedad de riesgos del trabajo.

#0.Cargando datos-----------------------------------------------------------------------------------
message( '\tCargando transiciones de los pensionistas del SGRT' )
load( file = parametros$demo_rdata_rtr_tran_prep )
load( file = parametros$demo_rdata_sgo_tran_prep )
load( file = paste0( parametros$RData_seg, "IESS_RTR_siniestralidad_accidentes_laborales_fatales.RData" ) )
load( file = parametros$demo_rdata_inec_fert_model )
load( file = parametros$demo_rdata_sgo_din_dec )
load( file = parametros$demo_rdata_sgo_pob_proy )

rm( list = ls()[ !(ls() %in% c( "parametros",
                                "sgrt_pen_tran_pa_pt_pp_anio",
                                "sgrt_pen_tran_indem_anio",
                                "sgrt_pen_tran_sub_anio",
                                "sgrt_pen_tran_orf_anio",
                                "sgrt_pen_tran_viu_anio",
                                "sgo_act_tran_anio",
                                "tas_din_dec",
                                "pob_proy",
                                "fer_dat",
                                "nup_dat" ) ) ] )

age_grid <- c(15: 105)
year_grid <- seq( 2012, 2020, 1 )

# 0.1 Tablas auxiliares-----------------------------------------------------------------------------

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

aux_1 <- sgo_act_tran_anio %>% 
  filter( anio == '2020' ) %>% 
  group_by( anio, sexo ) %>% 
  mutate( ERx_act = sum( ERx_act, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, ERx_act ) %>% 
  mutate( t = 0 )

aux_2 <- pob_proy %>% 
  group_by( t, sexo ) %>% 
  mutate( l2 = sum( l2, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( t, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( t, sexo, l2 ) %>%
  filter( t == 0 ) %>% 
  left_join( ., aux_1, by = c( 't', 'sexo' ) ) %>% 
  mutate( factor = l2 / ERx_act ) %>% 
  dplyr::select( sexo, factor )

corector_l2_ERx_act_H <- filter( aux_2, sexo == 'H')$factor
corector_l2_ERx_act_M <- filter( aux_2, sexo == 'M')$factor


ERx_act <- sgo_act_tran_anio %>% 
  dplyr::select( anio,
                 sexo,
                 x,
                 ERx_act :=  ERx_act ) %>% 
  mutate( ERx_act = if_else( sexo == 'H',
                             corector_l2_ERx_act_H * ERx_act,
                             corector_l2_ERx_act_M * ERx_act ) )


tas_din_dec <- tas_din_dec %>% 
  dplyr::select( sexo, x, t, t_15_0 := qx_act) %>% 
  filter( x > 21 ) %>% 
  mutate( t = t - 2020 ) %>% 
  filter( t <= 40, x > 21  )

ERx_hij <- expand.grid( anio = year_grid,
                        sexo = c( 'H', 'M' ),
                        x = seq( 15, 105, 1 ) ) %>% 
  left_join( ., ERx_act, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  left_join( ., cen_iess_hij_alis, by = c( 'x' = 'x', 'sexo' = 'sexo' ), relationship = "many-to-many" ) %>% 
  mutate( hij = q * ERx_act ) %>%
  group_by( anio, sexo_dep, y ) %>% 
  mutate( ERx_hij = sum( hij, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo_dep, y, .keep_all = TRUE ) %>% 
  dplyr::select( anio,
                 sexo := sexo_dep,
                 x:=y,
                 ERx_hij ) %>% 
  mutate( ERx_hij = if_else( x >= 18,
                             0,
                             ERx_hij ) ) 


ERx_jub_rt <- sgrt_pen_tran_pa_pt_pp_anio %>% 
  group_by( sexo, x ) %>% 
  mutate( ERx_jub_rt = sum( ERx_incap, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, ERx_jub_rt )


dist_sexo_viudez  <- sgrt_pen_tran_viu_anio %>% 
  group_by( sexo ) %>% 
  mutate(  N =  sum(Nx_ing, na.rm = TRUE )  ) %>% 
  ungroup( ) %>% 
  mutate( rel_sexo = N/ sum(Nx_ing, na.rm = TRUE ) ) %>% 
  distinct( ., sexo, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, rel_sexo)

ERx_cony <- expand.grid( anio = year_grid,
                         sexo = c( 'H', 'M' ),
                         x = seq( 15, 105, 1 ) ) %>% 
  left_join( ., ERx_act, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  left_join( ., cen_iess_cony_alis, by = c( 'x' = 'x' ), relationship = "many-to-many" ) %>% 
  mutate( cony = q * ERx_act ) %>%
  group_by( anio, sexo, y ) %>% 
  mutate( ERx_cony = sum( cony, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, y, .keep_all = TRUE ) %>% 
  dplyr::select( anio,
                 sexo,
                 x := y,
                 ERx_cony )

#1. Ajuste de la tasa de transición de afiliado a pensionista SGRT----------------------------------

##1.1. (2 ---> 12) Tasa ingreso de pensionistas de incapacidad PA, PT y PP-------------------------------

message( '\t(2 ---> 12) Tasa ingreso de pensionistas de incapacidad PA, PT y PP' )

dist_sexo_pensionistas <- sgrt_pen_tran_pa_pt_pp_anio %>% 
  filter( anio == '2020' ) %>% 
  group_by( sexo ) %>% 
  mutate(  E =  sum( ERx_incap, na.rm = TRUE )  ) %>% 
  ungroup( ) %>% 
  mutate( rel_sexo = E/ sum( ERx_incap, na.rm = TRUE ) ) %>% 
  distinct( ., sexo, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, rel_sexo)

porcentaje_hombres_pen <- filter( dist_sexo_pensionistas, sexo == 'H' )$rel_sexo

dist_sexo_pensionistas <- sgrt_pen_tran_pa_pt_pp_anio %>% 
  filter( anio == '2020' ) %>% 
  group_by( sexo ) %>% 
  mutate(  E =  sum( ERx_incap, na.rm = TRUE )  ) %>% 
  ungroup( ) %>% 
  mutate( rel_sexo = E/ sum( ERx_incap, na.rm = TRUE ) ) %>% 
  distinct( ., sexo, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, rel_sexo)

porcentaje_pensionistas_hombres <- filter( dist_sexo_pensionistas, sexo == 'H' )$rel_sexo

tas_2_12 <- expand.grid( anio = year_grid,
                    sexo = c( 'H', 'M' ),
                    x = age_grid ) %>%
  left_join( ., sgrt_pen_tran_pa_pt_pp_anio, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  left_join( ., ERx_act, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  group_by( x, sexo ) %>%
  mutate( ERx_act_2020 = sum( ifelse( anio == '2020',
                                      ERx_act,
                                      0 ), na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  group_by( sexo ) %>%
  mutate( Nx_ing_2020 = sum( ifelse( anio == '2020',
                                     Nx_ing,
                                     0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( x, sexo ) %>% 
  mutate( Nx_ing = sum( Nx_ing, na.rm = TRUE ),
          ERx_act = sum( ERx_act, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  filter( anio == '2020' ) %>% 
  distinct( x, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( x, 
                 sexo,
                 Nx_ing,
                 ERx_act,
                 Nx_ing_2020,
                 ERx_act_2020 ) %>% 
  mutate( Nx_ing  = if_else( Nx_ing == 0,
                             NA,
                             Nx_ing ) ) %>% 
  mutate( t_2_12 = Nx_ing / ERx_act )


aux <- sgrt_pen_tran_pa_pt_pp_anio %>%
  group_by( anio, sexo ) %>%
  mutate( ing = sum( Nx_ing, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, ing )

# Hombres 545 ingresos 2012 a 2020, y solo 79 ingresos de mujeres, total 624
# Ingresos de 21 en 2020, 20 hombres y una mujer
# Por el bajo número de entradas de pensionistas mujeres se decide hacer solo una
# tabla y multiplicar por el porcentaje de cada sexo

#Hombres
tas_2_12_h <- tas_2_12 %>% 
  filter( sexo == 'H' )

aux <- tas_2_12_h %>% 
  filter( !(x %in% c( '73', '74', '68' ) ) ) %>% 
  filter( is.finite( t_2_12 ) ) 

mod<-smooth.spline( aux$x,
                    aux$t_2_12, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    t_2_12_int = predict( mod, age_grid, deriv = 0)[["y"]] ) %>% 
  mutate( t_2_12_int = if_else( t_2_12_int < 0,
                                0,
                                t_2_12_int ) )


tas_2_12_h <- expand.grid( sexo = c( 'H' ),
                           x = age_grid ) %>% 
  full_join( ., tas_2_12_h, by = c( 'x', 'sexo' ) ) %>% 
  left_join( ., pred, by = 'x' )  %>% 
  mutate( t_2_12_int = ( t_2_12_int * Nx_ing_2020 /  sum ( t_2_12_int * ERx_act_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_2_12 = ( t_2_12 * sum( filter(., x < 60)$t_2_12_int, na.rm = TRUE ) /  sum( filter(., x < 60)$t_2_12, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_2_12_h$x, tas_2_12_h$t_2_12 )
lines( tas_2_12_h$x, tas_2_12_h$t_2_12_int )

#Comprobación: 21 ingresos de pensionistas en 2020
sum ( tas_2_12_h$t_2_12_int * tas_2_12_h$ERx_act_2020, na.rm = TRUE )


#Mujeres
tas_2_12_m <- tas_2_12_h %>% 
  mutate( sexo = 'M',
          t_2_12 = ( 1 - porcentaje_hombres_pen ) * t_2_12,
          t_2_12_int = ( 1 - porcentaje_hombres_pen ) * t_2_12_int ) %>% 
  dplyr::select( sexo,
                 x,
                 Nx_ing,
                 ERx_act,
                 Nx_ing_2020,
                 ERx_act_2020,
                 t_2_12,
                 t_2_12_int )

##1.2. (12 ---> 6) Tasa salida de pensionistas de incapacidad PA, PT y PP---------------------------
message( '\t(12 ---> 6) Tasa salida de pensionistas de incapacidad PA, PT y PP' )

tas_12_6 <- expand.grid( anio = year_grid,
                         sexo = c( 'H', 'M' ),
                         x = age_grid ) %>% 
  left_join( ., sgrt_pen_tran_pa_pt_pp_anio, by = c( 'anio', 'sexo', 'x' ) ) %>%
  group_by( x ) %>% 
  mutate( ERx_incp_2020 = sum( if_else( anio == '2020',
                                      ERx_incap,
                                      0 ), na.rm = TRUE ),
          Nx_sal_2020 = sum( if_else( anio == '2020',
                                    Nx_sal,
                                    0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( x ) %>% 
  mutate( Nx_sal = sum( Nx_sal, na.rm = TRUE ),
          ERx_incap  = sum( ERx_incap , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  filter( anio == 2020 ) %>%
  distinct( x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo,
                 x, 
                 Nx_sal,
                 ERx_incap,
                 Nx_sal_2020,
                 ERx_incp_2020 ) %>% 
  mutate( Nx_sal  = if_else( Nx_sal == 0,
                             NA,
                             Nx_sal ) ) %>% 
  mutate( t_12_6 = Nx_sal / ERx_incap  )

aux <- sgrt_pen_tran_pa_pt_pp_anio %>% 
  group_by( anio,sexo ) %>% 
  mutate( Nx_sal = sum( Nx_sal, na.rm = TRUE ),
          ERx_incap  = sum( ERx_incap , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, .keep_all = TRUE  ) %>% 
  dplyr::select( anio, sexo, Nx_sal, ERx_incap )


#Total 920 salidas entre 2012 a 2020, 170 salidas en 2020 
# Hombres 840 salidas entre 2012 a 2020 y 161 en 2020
# Mujeres 80 salidas entre 2012 a 2020 y 9 en 2020
# Se realiza una sola tabla para ambos sexos

aux <- tas_12_6 %>% 
  filter( !(x %in% c( '17', '22' ,'98', '104', '101',  '100',  '82', '83' ) ) ) %>% 
  filter( is.finite( t_12_6 ) ) 

mod<-smooth.spline( aux$x,
                    aux$t_12_6, df = 11 ) 

pred <- data.frame( x = age_grid, 
                    t_12_6_int = predict( mod, age_grid, deriv = 0)[["y"]] ) %>% 
  mutate( t_12_6_int = if_else( t_12_6_int < 0,
                                0,
                                t_12_6_int ) )


tas_12_6 <- expand.grid( #sexo = c( 'H' ),
                            x = age_grid ) %>% 
  left_join( ., tas_12_6, by = c('x') ) %>% 
  left_join( ., pred, by = 'x' )  %>% 
  mutate( t_12_6_int = ( t_12_6_int * sum( Nx_sal_2020, na.rm = TRUE ) /  sum ( t_12_6_int * ERx_incp_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_12_6 = ( t_12_6 * sum( Nx_sal_2020, na.rm = TRUE ) /  sum ( t_12_6 * ERx_incp_2020, na.rm = TRUE ) ) ) %>% 
  dplyr::select( -sexo )

#Gráfico de control
plot( tas_12_6$x, tas_12_6$t_12_6 )
lines( tas_12_6$x, tas_12_6$t_12_6_int )

#Comprobación:
sum ( tas_12_6$t_12_6_int * tas_12_6$ERx_incp_2020, na.rm = TRUE )

#Hombres
tas_12_6_h <- tas_12_6 %>% 
  mutate( sexo = 'H',
          t_12_6 = t_12_6,
          t_12_6_int = t_12_6_int ) %>% 
  dplyr::select( sexo,
                 x,
                 Nx_sal,
                 ERx_incap,
                 Nx_sal_2020,
                 ERx_incp_2020,
                 t_12_6,
                 t_12_6_int )

#Mujeres
tas_12_6_m <- tas_12_6 %>% 
  mutate( sexo = 'M',
          t_12_6 =  t_12_6,
          t_12_6_int =  t_12_6_int ) %>% 
  dplyr::select( sexo,
                 x,
                 Nx_sal,
                 ERx_incap,
                 Nx_sal_2020,
                 ERx_incp_2020,
                 t_12_6,
                 t_12_6_int )

# 2. (2 ---> 13) Tasa de transición de activo a beneficiario de indemnizaciones---------------------
message( '\t(2 ---> 13) Tasa de transición de activo a beneficiario de indemnizaciones' )

tas_2_13 <- expand.grid( anio = year_grid,
                         sexo = c( 'H', 'M' ),
                         x = age_grid ) %>% 
  left_join( ., sgrt_pen_tran_indem_anio, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  left_join( ., ERx_act, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  group_by( sexo, x ) %>% 
  mutate( ERx_act_2020 = sum( if_else( anio == 2020,
                                       ERx_act,
                                       0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo ) %>% 
  mutate( Nx_ing_2020 = sum( if_else( anio == 2020,
                                    Nx_ing,
                                    0 ), na.rm = TRUE ) ) %>%  
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( Nx_ing = sum( Nx_ing, na.rm = TRUE ),
          ERx_act = sum( ERx_act, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  #distinct( sexo, x, .keep_all = TRUE ) %>% 
  filter( anio == '2020' ) %>% 
  dplyr::select( sexo,
                 x, 
                 Nx_ing,
                 ERx_act,
                 Nx_ing_2020,
                 ERx_act_2020 ) %>% 
  mutate( Nx_ing  = if_else( Nx_ing == 0,
                             NA,
                             Nx_ing ) ) %>% 
  mutate( t_2_13 = Nx_ing / ERx_act ) 

aux <- sgrt_pen_tran_indem_anio %>% 
  group_by( anio, sexo ) %>% 
  mutate( ing = sum( Nx_ing, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, ing ) %>% 
  filter( sexo == 'H' )

# Hombres, 5061 ingresos entre 2012 a 2020, en 2020 hay 303 ingresos
tas_2_13_h <- tas_2_13 %>% 
  filter( sexo == 'H' )

aux <- tas_2_13_h %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( '88', '71', '74' ) ) )

mod<-smooth.spline( aux$x,
                    aux$t_2_13, df = 6 ) 

pred <- data.frame( x = age_grid, 
                    t_2_13_int = predict( mod, age_grid, deriv = 0)[["y"]] )

tas_2_13_h <- left_join( tas_2_13_h, pred, by = 'x' ) %>% 
  mutate( t_2_13 = ( t_2_13 * Nx_ing_2020 /  sum ( t_2_13 * ERx_act_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_2_13_int = ( t_2_13_int * Nx_ing_2020 /  sum ( t_2_13_int * ERx_act_2020, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_2_13_h$x, tas_2_13_h$t_2_13 )
lines( tas_2_13_h$x, tas_2_13_h$t_2_13_int )

#Comprobación: 
sum ( tas_2_13_h$t_2_13_int * tas_2_13_h$ERx_act_2020, na.rm = TRUE )

# Mujeres hay  901 ingresos entre 2012 a 2020 y en 40 ingresos en 2020
tas_2_13_m <- tas_2_13 %>% 
  filter( sexo == 'M' )

aux <- tas_2_13_m %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c( 28, 30, 32, 34, 37, 43, 48 ) ) )

mod<-smooth.spline( aux$x,
                    aux$t_2_13, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    t_2_13_int = predict( mod, age_grid, deriv = 0)[["y"]] )

tas_2_13_m <- left_join( tas_2_13_m, pred, by = 'x' )  %>% 
  mutate( t_2_13 = ( t_2_13 * Nx_ing_2020 /  sum ( t_2_13 * ERx_act_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_2_13_int = ( t_2_13_int * Nx_ing_2020 /  sum ( t_2_13_int * ERx_act_2020, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_2_13_m$x, tas_2_13_m$t_2_13 )
lines( tas_2_13_m$x, tas_2_13_m$t_2_13_int )

#Comprobación: 
sum ( tas_2_13_m$t_2_13_int * tas_2_13_m$ERx_act_2020, na.rm = TRUE )

# 3. (2 ---> 14) Tasa de transiciones de activo a beneficiario de subsidios-------------------------
message( '\t(2 ---> 14) Tasa de transiciones de activo a beneficiario de subsidios' )

tas_2_14 <- expand.grid( anio = year_grid,
                         sexo = c( 'H', 'M' ),
                         x = age_grid ) %>% 
  left_join( ., sgrt_pen_tran_sub_anio, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  left_join( ., ERx_act, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  #filter( ! ( anio %in% c( '2013', '2014', '2015', '2016', '2017', '2018' ) ) ) %>% # Años con altas siniestralidad
  group_by( sexo, x ) %>% 
  mutate( ERx_act_2020 = sum( if_else( anio == 2020,
                                       ERx_act,
                                       0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo ) %>% 
  mutate( Nx_ing_2020 = sum( if_else( anio == 2020,
                                      Nx_ing,
                                      0 ), na.rm = TRUE ) ) %>%  
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( Nx_ing = sum( Nx_ing, na.rm = TRUE ),
          ERx_act = sum( ERx_act, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  #distinct( sexo, x, .keep_all = TRUE ) %>% 
  filter( anio == '2020' ) %>% 
  dplyr::select( sexo,
                 x, 
                 Nx_ing,
                 ERx_act,
                 Nx_ing_2020,
                 ERx_act_2020 ) %>% 
  mutate( Nx_ing  = if_else( Nx_ing == 0,
                             NA,
                             Nx_ing ) ) %>% 
  mutate( t_2_14 = Nx_ing / ERx_act ) 


# Hombres hay 208.627 subsidios pagados entre 2012 y 2020; y 11.442 subsidios pagados en 2020   

tas_2_14_h <- tas_2_14 %>% 
  filter( sexo == 'H' ) 

aux <- tas_2_14_h %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c('17' ) ) )

mod<-smooth.spline( aux$x,
                    aux$t_2_14, df = 12 ) 

pred <- data.frame( x = age_grid, 
                    t_2_14_int = predict( mod, age_grid, deriv = 0)[["y"]] )

tas_2_14_h <- left_join( tas_2_14_h, pred, by = 'x' ) %>% 
  mutate( t_2_14 = ( t_2_14 * Nx_ing_2020 /  sum ( t_2_14 * ERx_act_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_2_14_int = ( t_2_14_int * Nx_ing_2020 /  sum ( t_2_14_int * ERx_act_2020, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_2_14_h$x, tas_2_14_h$t_2_14 )
lines( tas_2_14_h$x, tas_2_14_h$t_2_14_int )

#Comprobación: 
sum( tas_2_14_h$t_2_14_int * tas_2_14_h$ERx_act_2020, na.rm = TRUE )

# Mujeres 43.135 subsidios entre 2012 a 2020, en 2020 hay 2.400

tas_2_14_m <- tas_2_14 %>% 
  filter( sexo == 'M' )

aux <- tas_2_14_m %>% 
  na.omit( . ) %>% 
  filter( !(x %in% c('68', '61', '18', '19', '77', '75', '76', '20', '78', '73', '71' ) ) )

mod<-smooth.spline( aux$x,
                    aux$t_2_14, df = 4 ) 

pred <- data.frame( x = age_grid, 
                    t_2_14_int = predict( mod, age_grid, deriv = 0)[["y"]] )

tas_2_14_m <- left_join( tas_2_14_m, pred, by = 'x' )  %>% 
  mutate( t_2_14 = ( t_2_14 * Nx_ing_2020 /  sum ( t_2_14 * ERx_act_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_2_14_int = ( t_2_14_int * Nx_ing_2020 /  sum ( t_2_14_int * ERx_act_2020, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_2_14_m$x, tas_2_14_m$t_2_14 )
lines( tas_2_14_m$x, tas_2_14_m$t_2_14_int )

#Comprobación: 
sum( tas_2_14_m$t_2_14_int * tas_2_14_m$ERx_act_2020, na.rm = TRUE )

#4. Transiciones de orfandad------------------------------------------------------------------------

##4.1. ( 0 ---> 15) Tasa de ingreso de pensionistas de orfandad-------------------------------------

message( '\t ( 0 ---> 15) Tasa de ingreso de pensionistas de orfandad' )

tas_0_15 <- expand.grid( anio = year_grid,
                          sexo = c( 'H', 'M' ),
                          x = seq( 0, 105, 1 ) ) %>% 
  left_join( ., sgrt_pen_tran_orf_anio, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  left_join( ., ERx_hij, by = c( 'anio' ,'sexo', 'x' ) ) %>% 
  group_by( sexo, x ) %>%
  mutate( ERx_hij_2020 = sum( ifelse( anio == '2020',
                                      ERx_hij,
                                      0 ), na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  group_by( sexo ) %>% 
  mutate( Nx_ing_2020 = sum( if_else( anio == 2020,
                                      Nx_ing,
                                      0 ), na.rm = TRUE ) ) %>%  
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( Nx_ing = sum( Nx_ing, na.rm = TRUE ),
          ERx_hij  = sum( ERx_hij , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  filter( anio == '2020' ) %>% 
  dplyr::select( sexo,
                 x, 
                 Nx_ing,
                 ERx_hij,
                 Nx_ing_2020,
                 ERx_hij_2020 ) %>% 
  filter( x <= 17 ) %>% 
  mutate( t_0_15 = Nx_ing / ( ERx_hij ) ) 


#Hombres 1308 ingresos entre 2012 a 2020, 35 ingresos en 2020 
tas_0_15_h <- tas_0_15 %>% 
  filter( sexo == 'H' ) 

mod<-smooth.spline( tas_0_15_h$x,
                    tas_0_15_h$t_0_15, df = 6 ) 

pred <- data.frame( x = seq( 0, 105, 1 ), 
                    t_0_15_int = predict( mod, seq( 0, 105, 1 ), deriv = 0)[["y"]] )

tas_0_15_h <- left_join( tas_0_15_h, pred, by = 'x' ) %>% 
  mutate( t_0_15 = ( t_0_15 * Nx_ing_2020 /  sum ( t_0_15 * ERx_hij_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_0_15_int = ( t_0_15_int * Nx_ing_2020 /  sum ( t_0_15_int * ERx_hij_2020, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_0_15_h$x, tas_0_15_h$t_0_15 )
lines( tas_0_15_h$x, tas_0_15_h$t_0_15_int )

#Comprobación:
sum ( tas_0_15_h$t_0_15_int * tas_0_15_h$ERx_hij_2020, na.rm = TRUE )

#Mujeres 1231 ingresos entre 2012 a 2020, 33 ingresos en 2020
tas_0_15_m <- tas_0_15 %>% 
  filter( sexo == 'M' ) 

mod<-smooth.spline( tas_0_15_m$x,
                    tas_0_15_m$t_0_15, df = 6 ) 

pred <- data.frame( x = seq( 0, 105, 1 ), 
                    t_0_15_int = predict( mod, seq( 0, 105, 1 ), deriv = 0)[["y"]] )

tas_0_15_m <- left_join( tas_0_15_m, pred, by = 'x' )  %>% 
  mutate( t_0_15 = ( t_0_15 * Nx_ing_2020 /  sum ( t_0_15 * ERx_hij_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_0_15_int = ( t_0_15_int * Nx_ing_2020 /  sum ( t_0_15_int * ERx_hij_2020, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_0_15_m$x, tas_0_15_m$t_0_15 )
lines( tas_0_15_m$x, tas_0_15_m$t_0_15_int )

#Comprobación:
sum ( tas_0_15_m$t_0_15_int * tas_0_15_m$ERx_hij_2020, na.rm = TRUE )

##4.2. (15 ---> 0 ) Tasa de salida de pensionistas de orfandad--------------------------------------

message( '\t (15 ---> 0 ) Tasa salida de pensionistas de orfandad para menores de 21' )

#Se elabora la tasa de salida de huerfanos de menores de 21 años que obtubieron derecho con la CD 100, despues de 2006
#Para los beneficiarios de orfandad, con fecha derecho antes de la cd 100, son pensiones vitalicias y se aplica tabala de mortalidad

tas_15_0 <- expand.grid( anio = year_grid,
                         sexo = c( 'H', 'M' ),
                         x = seq( 0, 105, 1 ) ) %>% 
  left_join( ., sgrt_pen_tran_orf_anio, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  #filter( x <= 21 ) %>% 
  group_by( sexo ) %>% 
  mutate( Nx_sal_2020 = sum( if_else( anio == '2020',
                                      Nx_sal,
                                      0 ), na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( ERx_incap_2020 = sum( if_else( anio == 2020,
                                         ERx_incap,
                                         0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo, x ) %>% 
  mutate( Nx_sal = sum( Nx_sal, na.rm = TRUE ),
          ERx_incap  = sum( ERx_incap , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  mutate( E = sum( ERx_incap , na.rm = TRUE ),
          N = sum( Nx_sal, na.rm = TRUE ) ) %>% 
  dplyr::select( sexo,
                 x, 
                 Nx_sal,
                 ERx_incap,
                 Nx_sal_2020,
                 ERx_incap_2020,
                 E, 
                 N ) %>% 
  mutate( t_15_0 = Nx_sal / ERx_incap  ) %>% 
  mutate( t_15_0 = if_else( t_15_0 == 0,
                            NA,
                            t_15_0 ) ) 

aux <- sgrt_pen_tran_orf_anio %>% 
  group_by( anio, sexo ) %>% 
  mutate( sal = sum( Nx_sal, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, anio, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, sal ) 

# Hombres Salidas de 1117 hombres, 115 salidas en 2020
tas_15_0_h <- tas_15_0 %>% 
  filter( sexo == 'H' ) 

aux <- tas_15_0_h %>% 
  na.omit( . ) %>% 
  filter( !( x %in% c( 18, 21, 7, 3 ) ) )

mod<-smooth.spline( aux$x,
                    aux$t_15_0, df = 6 ) 

pred <- data.frame( x = seq( 0, 105, 1 ), 
                    t_15_0_int = predict( mod, seq( 0, 105, 1 ), deriv = 0)[["y"]] ) %>% 
  mutate( t_15_0_int = if_else( t_15_0_int < 0,
                                0,
                                t_15_0_int ) )

tas_15_0_h <- expand.grid( sexo = c( 'H' ),
                         x = seq( 0, 105, 1 ) ) %>% 
  left_join( ., tas_15_0_h, by = c('sexo', 'x') ) %>% 
  left_join( ., pred, by = 'x' ) %>% 
  mutate( t_15_0_int = if_else( x %in% c( '18', '21' ),
                                t_15_0,
                                t_15_0_int ) ) %>% 
  mutate( t_15_0 = ( t_15_0 * Nx_sal_2020 /  sum ( t_15_0 * ERx_incap_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_15_0_int = ( t_15_0_int * Nx_sal_2020 /  sum ( t_15_0_int * ERx_incap_2020, na.rm = TRUE ) ) )

#Comprobación:
sum( tas_15_0_h$t_15_0_int * tas_15_0_h$ERx_incap_2020, na.rm = TRUE )

#Gráfico de control
plot( tas_15_0_h$x, tas_15_0_h$t_15_0 )
lines( tas_15_0_h$x, tas_15_0_h$t_15_0_int )


# Mujeres 1107 salidas entre 2012 a 2020, en 2020 hay 112 salidas
tas_15_0_m <- tas_15_0 %>% 
  filter( sexo == 'M' ) 

aux <- tas_15_0_m %>% 
  na.omit( . ) %>% 
  filter( !( x %in% c( 18, 21,  14, 17 ) ) )

mod<-smooth.spline( aux$x,
                    aux$t_15_0, df = 4 ) 

pred <- data.frame( x = seq( 0, 105, 1 ), 
                    t_15_0_int = predict( mod, seq( 0, 105, 1 ), deriv = 0)[["y"]] ) %>% 
  mutate( t_15_0_int = if_else( t_15_0_int < 0,
                                0,
                                t_15_0_int ) )

tas_15_0_m <- expand.grid( sexo = c( 'M' ),
                         x = seq( 0, 105, 1 ) ) %>% 
  left_join( ., tas_15_0_m, by = c('sexo', 'x') ) %>% 
  left_join( ., pred, by = 'x' ) %>% 
  mutate( t_15_0_int = if_else( x %in% c( '18', '21' ),
                                t_15_0,
                                t_15_0_int ) ) %>% 
  mutate( t_15_0 = ( t_15_0 * Nx_sal_2020 /  sum ( t_15_0 * ERx_incap_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_15_0_int = ( t_15_0_int * Nx_sal_2020 /  sum ( t_15_0_int * ERx_incap_2020, na.rm = TRUE ) ) )

#Comprobación:
sum( tas_15_0_m$t_15_0_int * tas_15_0_m$ERx_incap_2020, na.rm = TRUE )

#Gráfico de Control
plot( tas_15_0_m$x, tas_15_0_h$t_15_0 )
lines( tas_15_0_m$x, tas_15_0_h$t_15_0_int )

#5. Transiciones de cónyuge a pensionistas de viudedad----------------------------------------------

##5.1. ( 0 ---> 16) Tasa de ingreso de pensionistas de viudedad-------------------------------------
message( '\t ( 0 ---> 16 ) Tasa ingreso de pensionistas de viudedad' )

dist_sexo_viudez <- sgrt_pen_tran_viu_anio %>% 
  filter( anio == '2020' ) %>% 
  group_by( sexo ) %>% 
  mutate(  E =  sum( ERx_incap, na.rm = TRUE )  ) %>% 
  ungroup( ) %>% 
  mutate( rel_sexo = E/ sum( ERx_incap, na.rm = TRUE ) ) %>% 
  distinct( ., sexo, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, rel_sexo)

porcentaje_viudas <- filter( dist_sexo_viudez, sexo == 'M' )$rel_sexo

tas_0_16 <- expand.grid( anio = year_grid,
                         sexo = c( 'M', 'H' ),
                         x = seq( 0, 105, 1 ) ) %>% 
  full_join( ., sgrt_pen_tran_viu_anio, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  full_join( ., ERx_cony, by = c( 'anio' ,'sexo', 'x' ) ) %>% 
  group_by( x, sexo ) %>%
  mutate( ERx_cony_2020 = sum( ifelse( anio == '2020',
                                       ERx_cony,
                                      0 ), na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  group_by( sexo ) %>%
  mutate( Nx_ing_2020 = sum( ifelse( anio == '2020',
                                     Nx_ing,
                                     0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( x, sexo ) %>% 
  mutate( Nx_ing = sum( Nx_ing, na.rm = TRUE ),
          ERx_cony  = sum( ERx_cony , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  filter( anio == '2020' ) %>% 
  distinct( x, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( x,
                 sexo,
                 Nx_ing,
                 ERx_cony,
                 Nx_ing_2020,
                 ERx_cony_2020 ) %>% 
  filter( x >= 15 ) %>% 
  mutate( t_0_16 = Nx_ing / ( ERx_cony ) ) %>%  
  mutate( t_0_16 = if_else( t_0_16 == 0,
                            NA,
                            t_0_16 ) ) 

aux <- sgrt_pen_tran_viu_anio %>% 
  group_by( sexo, anio ) %>% 
  mutate( ing = sum( Nx_ing, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, anio, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, ing )

# Hombres solo hay 60 ingresos entre 2012 y 2020, 1 ingreso en 2020 
# Mujeres hay 1466 ingresos entre 2012 a 2020, 47 ingresos en 2020
# Por bajo número de ingresos de hombres viudos, se realiza solo una tabla
# y después se aplica un porcentaje para separar por sexo

#Mujeres
tas_0_16_m <- tas_0_16 %>% 
  filter( sexo == 'M' )

aux <- tas_0_16_m %>% 
  filter( is.finite( t_0_16 ) ) 

mod<-smooth.spline( aux$x,
                    aux$t_0_16, df = 10 ) 

pred <- data.frame( x = age_grid, 
                    t_0_16_int = predict( mod, age_grid, deriv = 0)[["y"]] ) %>% 
  mutate( t_0_16_int = if_else( t_0_16_int < 0,
                                0,
                                t_0_16_int ) )

tas_0_16_m <- expand.grid( sexo = c( 'M' ),
  x = age_grid ) %>% 
  left_join( ., tas_0_16_m, by = c('x', 'sexo') ) %>% 
  left_join( ., pred, by = c( 'x' ) ) %>% 
  mutate( t_0_16_int = ( t_0_16_int * Nx_ing_2020 /  sum ( t_0_16_int * ERx_cony_2020, na.rm = TRUE ) ) ) %>%
  mutate( t_0_16 = ( t_0_16 * sum( filter(., x < 50 )$t_0_16_int, na.rm = TRUE ) /  sum( filter(., x < 50 )$t_0_16, na.rm = TRUE ) ) )

#Gráfico de control
plot( tas_0_16_m$x, tas_0_16_m$t_0_16 )
lines( tas_0_16_m$x, tas_0_16_m$t_0_16_int )

#Comprobación: 48 ingresos de pensionistas en 2020
sum ( tas_0_16_m$t_0_16_int * tas_0_16_m$ERx_cony_2020, na.rm = TRUE )

#Hombres

tas_0_16_h <- tas_0_16_m %>% 
  mutate( sexo = 'H',
          t_0_16 = ( 1 - porcentaje_viudas ) * t_0_16,
          t_0_16_int = ( 1 - porcentaje_viudas ) * t_0_16_int ) %>% 
  dplyr::select( sexo,
                 x,
                 Nx_ing,
                 ERx_cony,
                 Nx_ing_2020,
                 ERx_cony_2020,
                 t_0_16,
                 t_0_16_int )


#Gráfico de control
plot( tas_0_16_h$x, tas_0_16_h$t_0_16 )
lines( tas_0_16_h$x, tas_0_16_h$t_0_16_int )

##5.2. ( 16 ---> 0 ) Tasa salida de pensionistas de viudedad-----------------------------------------

message( '\t ( 16 ---> 0 ) Tasa de salida de pensionistas de viudedad' )

tas_16_0 <- expand.grid( anio = year_grid,
                        sexo = c( 'H', 'M' ),
                        x = seq( 15, 105, 1 ) ) %>% 
  left_join( ., sgrt_pen_tran_viu_anio, by = c( 'anio', 'sexo', 'x' ) ) %>% 
  group_by( sexo, x ) %>% 
  mutate( ERx_incap_2020 = sum( if_else( anio == 2020,
                                         ERx_incap,
                                       0 ), na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( sexo ) %>% 
  mutate( Nx_sal_2020 = sum( if_else( anio == 2020,
                                      Nx_sal,
                                      0 ), na.rm = TRUE ) ) %>%  
  ungroup( ) %>% 
 group_by( sexo, x ) %>% 
  mutate( Nx_sal = sum( Nx_sal, na.rm = TRUE ),
          ERx_incap = sum( ERx_incap, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  #distinct( sexo, x, .keep_all = TRUE ) %>% 
  filter( anio == '2020' ) %>% 
  dplyr::select( sexo,
                 x, 
                 Nx_sal,
                 ERx_incap,
                 Nx_sal_2020,
                 ERx_incap_2020 ) %>% 
  mutate( Nx_sal  = if_else( Nx_sal == 0,
                             NA,
                             Nx_sal ) ) %>% 
  mutate( t_16_0 = Nx_sal / ERx_incap ) 

aux <- sgrt_pen_tran_viu_anio %>% 
  group_by( sexo, anio ) %>% 
  mutate( sal = sum( Nx_sal, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( sexo, anio, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, sal ) %>% 
  filter( sexo == 'H')


# Hombres 10 salidas entre 2012 a 2020, 2 en 2020
# Mujeres 602 salidas entre 2012 a 2020, 102 en 2020
#Por la poca muestra de hombres se decide hacer una sola tabla

tas_16_0 <- tas_16_0 %>% 
  group_by( x ) %>% 
  mutate( Nx_sal = sum( Nx_sal, na.rm = TRUE ),
          ERx_incap = sum( ERx_incap, na.rm = TRUE ),
          Nx_sal_2020 = sum( Nx_sal_2020, na.rm = TRUE ),
          ERx_incap_2020= sum( ERx_incap_2020 , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( x, .keep_all = TRUE ) %>% 
  mutate( t_16_0 = Nx_sal / ERx_incap ) %>% 
  dplyr::select( -sexo )

aux <- tas_16_0 %>%
  na.omit( . ) %>% 
  filter( !(x %in% c( '88', '71', '74' ) ) )

mod<-smooth.spline( aux$x,
                    aux$t_16_0, df = 6 ) 

pred <- data.frame( x = seq( 15, 105, 1 ), 
                    t_16_0_int = predict( mod, seq( 15, 105, 1 ), deriv = 0)[["y"]] )

tas_16_0 <- left_join( tas_16_0, pred, by = 'x' ) %>% 
  mutate( t_16_0 = ( t_16_0 * Nx_sal_2020 /  sum ( t_16_0 * ERx_incap_2020, na.rm = TRUE ) ) ) %>% 
  mutate( t_16_0_int = ( t_16_0_int * Nx_sal_2020 /  sum ( t_16_0_int * ERx_incap_2020, na.rm = TRUE ) ) )

#Gráfico de Control
plot( tas_16_0$x, tas_16_0$t_16_0 )
lines( tas_16_0$x, tas_16_0$t_16_0_int )

#Comprobación:
sum ( tas_16_0$t_16_0_int * tas_16_0$ERx_incap_2020, na.rm = TRUE )

#Hombres

tas_16_0_h <- tas_16_0 %>% 
  mutate( sexo = 'H',
          t_16_0 =  t_16_0,
          t_16_0_int = t_16_0_int ) %>% 
  dplyr::select( sexo,
                 x,
                 Nx_sal,
                 ERx_incap,
                 Nx_sal_2020 ,
                 ERx_incap_2020,
                 t_16_0,
                 t_16_0_int )

#Mujeres

tas_16_0_m <- tas_16_0 %>% 
  mutate( sexo = 'M',
          t_16_0 =  t_16_0,
          t_16_0_int =  t_16_0_int ) %>% 
  dplyr::select( sexo,
                 x,
                 Nx_sal,
                 ERx_incap,
                 Nx_sal_2020 ,
                 ERx_incap_2020,
                 t_16_0,
                 t_16_0_int )

#Conslidando----------------------------------------------------------------------------------------
tas_2_12 <- rbind( tas_2_12_h, tas_2_12_m )
tas_12_6 <- rbind( tas_12_6_h, tas_12_6_m )
tas_2_13 <- rbind( tas_2_13_h, tas_2_13_m )
tas_2_14 <- rbind( tas_2_14_h, tas_2_14_m )
tas_0_15 <- rbind( tas_0_15_h, tas_0_15_m )
tas_15_0 <- rbind( tas_15_0_h, tas_15_0_m )
tas_0_16 <- rbind( tas_0_16_h, tas_0_16_m )
tas_16_0 <- rbind( tas_16_0_h, tas_16_0_m )

# Guarda resultados --------------------------------------------------------------------------------
message( '\tGuardando suavizamiento de tasas' )
save( tas_2_12,
      tas_12_6,
      tas_2_13,
      tas_2_14,
      tas_0_15,
      tas_15_0,
      tas_0_16,
      tas_16_0,
      porcentaje_viudas,
      porcentaje_pensionistas_hombres,
      file = parametros$demo_rdata_rtr_tasas_tran )

# Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% 'parametros' ) ] )
gc( )

