message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tPreparación de información de transiciones de activos a pensionistas de SGRT' )

# Descripción de campos ----------------------------------------------------------------------------
# 12 = pensionistas de riesgos del trabajo;
# 13 = indemnizaciones por incapacidad permanente parcial;
# 14 = subsidios por incapacidad temporal;
# 15 = montepíos de orfandad de riesgos del trabajo;
# 16 = montepíos de viudedad de riesgos del trabajo.

#Cargando información-------------------------------------------------------------------------------
message( '\tLectura de inventario de pensionistas de SGRT, indemnizaciones y subsidios' )
load( paste0( parametros$RData_seg, 'IESS_RTR_inventario_pensionistas.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_subsidios.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_rentas.RData' ) )
load( parametros$macro_rdata_info )

rm( list = ls()[ !(ls() %in% c( "parametros",
                                "indemnizaciones_rt_2018",
                                "indemnizaciones_rt_2022",
                                "inventario_jubilados",
                                "prestaciones_orfandad",
                                "prestaciones_pa",
                                "prestaciones_pp",
                                "prestaciones_pt",
                                "prestaciones_viudez",
                                "subsidios_rtr",
                                "sbu" ) ) ] )

# 0. Coeficientes de Incapacidad--------------------------------------------------------------------
inventario_jubilados <- inventario_jubilados %>% 
  filter( tipo_seguro == 'RT',
          estado_prestacion %in% c( 'A', 'I' ) ) %>%
  group_by( cedula, tipo_prestacion ) %>% 
  mutate( n = n() ) %>% 
  mutate( coef_incap = max( coeficiente_real, na.rm = TRUE ),
          Sx_prom = max( promedio_sueldo_real, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( cedula, tipo_prestacion, .keep_all = TRUE ) %>% 
  dplyr::select( cedula,
                 #sexo,
                 #fecha_nacimiento,
                 fecha_derecho,
                 fecha_defuncion,
                 tipo_prestacion,
                 coef_incap,
                 Sx_prom )

#1. Transiciones de activo a pensionistas de incapacidad PA, PT y PP--------------------------------

sgrt_pen_tran_pa_pt_pp <- rbind( prestaciones_pa, prestaciones_pt, prestaciones_pp ) %>% #5.170.363
  left_join( ., inventario_jubilados, by = c('cedula'='cedula',  'cod_tipo_prestacion'='tipo_prestacion' ) ) %>% 
  mutate( planilla = as.Date( paste0(anio, '-', mes,'-01'), "%Y-%m-%d" ) ) %>%
  group_by( cedula, anio, mes ) %>% 
  mutate(  tot_ingr =  sum(  ifelse(  rubro %in% c( 1,
                                                    2,
                                                    3,
                                                    5,
                                                    9,
                                                    10,
                                                    22,
                                                    55,
                                                    60,
                                                    69,
                                                    89,
                                                    101,
                                                    102,
                                                    103,
                                                    345,
                                                    374,
                                                    375,
                                                    376  ),
                                      valor,
                                      0 ), na.rm = TRUE  ),
           decimo_tercero = sum(  ifelse(  rubro %in% c( 345, 374, 376, 10  ),
                                           valor,
                                           0 ), na.rm = TRUE  ),
           decimo_cuarto = sum(  ifelse(  rubro %in% c( 9, 375  ),
                                          valor,
                                          0 ), na.rm = TRUE  ),
           renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, mes, cedula, .keep_all = TRUE ) %>% 
  group_by( cedula ) %>% 
  mutate( fecha_ingreso = min( planilla, na.rm = TRUE )  ) %>%
  mutate( fecha_salida = max( planilla, na.rm = TRUE )  ) %>% 
  # mutate( fecha_salida = if_else( !is.na( fecha_defuncion ),
  #                                 fecha_defuncion,
  #                                 fecha_salida )  ) %>% 
  ungroup( ) %>% 
  mutate( fecha_ingreso = if_else( fecha_ingreso == as.Date( '2012-01-01' ),
                                   NA,
                                   fecha_ingreso ) ) %>% 
  mutate( fecha_salida = if_else( fecha_salida == as.Date( '2020-12-01' ),
                                   NA,
                                  fecha_salida ) ) %>% 
  group_by( anio, cedula ) %>% 
  mutate( P = sum( renta_mensual, na.rm = TRUE ),
          Sx_prom = Sx_prom,
          coef_incap = coef_incap,
          P_13 = sum( decimo_tercero, na.rm = TRUE ),
          P_14 = sum( decimo_cuarto, na.rm = TRUE ),
          P_Tot = sum( tot_ingr, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( cedula ) %>% 
  mutate( max_planilla = max( planilla, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( ., anio, cedula, .keep_all = TRUE ) %>% 
  left_join( ., sbu %>% dplyr::select( anio, sbu ), by = 'anio' ) %>% 
  mutate( coef_decima_tercera = P_13 / P,
          coef_decima_cuarta = P_14 / sbu ) %>% 
  filter( tot_ingr > 0 ) %>% 
  dplyr::select( -rubro, -descripcion )

##1.1. (2 ---> 12) Ingreso de pensionistas de incapacidad PA, PT y PP--------------------------------

aux_a <- sgrt_pen_tran_pa_pt_pp %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( anio, '-06-30') ), fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( ERx_incap = n( ),
          P = sum( P, na.rm = TRUE ),
          Sx_prom = mean( Sx_prom, na.rm = TRUE ),
          P_13= sum( P_13, na.rm = TRUE ),
          P_14 = sum( P_14, na.rm = TRUE ),
          P_Tot = sum( P_Tot, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select(  anio, sexo, x,  ERx_incap, P, Sx_prom, P_13, P_14, P_Tot )
  
aux_b <- sgrt_pen_tran_pa_pt_pp %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( year( fecha_ingreso ), "-06-30"), "%Y-%m-%d" ) , fecha_nacimiento, units = "days" ) ) / 365, 0 ) ) %>% 
  filter( cod_tipo_prestacion %in% c('PA', 'PT') ) %>%  #desde 2014 no se permite nuevos ingresos de incapcidad PP
  filter( fecha_ingreso < as.Date( "2020-12-01" ),
          fecha_ingreso > as.Date( "2012-01-01" ) ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso != as.Date( "2012-01-01" ) ) %>% 
  mutate( anio = year( fecha_ingreso ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_ing = n( ) ) %>% 
  mutate( coef_incap = mean( coef_incap, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_ing, coef_incap ) 

aux_ing <- full_join( aux_a, aux_b, by = c('anio', 'sexo', 'x') )

##1.2. (12 ---> 6 ) Salida de pensionistas de incapacidad PA, PT y PP-------------------------------

aux_sal <- sgrt_pen_tran_pa_pt_pp %>% 
  mutate( fecha_salida = max_planilla ) %>% 
  
  mutate( x = round( as.numeric( difftime( as.Date( paste0( year( fecha_salida ), "-06-30" ) ) , fecha_nacimiento, units = "days" ) / 365 ), 0 ) ) %>%
  filter( fecha_salida < as.Date( "2020-12-01" ),
          fecha_salida > as.Date( "2012-01-01" ) ) %>% 
  distinct( cedula, .keep_all = TRUE ) %>% 
  mutate( anio = year( fecha_salida ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_sal = sum( ifelse ( fecha_salida != as.Date( "2020-12-01" ),
                                 1,
                                 0 ) ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_sal )

sgrt_pen_tran_pa_pt_pp_anio <- full_join( aux_ing, aux_sal, by = c('anio', 'sexo', 'x') )  %>% 
  arrange( anio, sexo, x ) %>% 
  mutate( estado = 12 ) %>% 
  dplyr::select( anio,
                 sexo,
                 x, 
                 Nx_ing,
                 Nx_sal,
                 ERx_incap,
                 coef_incap,
                 P,
                 Sx_prom,
                 P_13,
                 P_14,
                 P_Tot, 
                 estado ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') ) %>% 
  arrange( anio, sexo, x )

# 2. (2 ---> 13) Transiciones de activo a beneficiario de indemnizaciones---------------------------
aux_2018 <- indemnizaciones_rt_2018 %>% dplyr::select( cedula,
                                           sexo,
                                           fecha_nacimiento,
                                           fecha_pago = fecha_acu,
                                           P := valor ) 

coef_incap <- inventario_jubilados %>% 
  filter( tipo_prestacion %in% c( 'ID' ) ) %>% #7876
  distinct( cedula, .keep_all = TRUE  ) %>% 
  dplyr::select( cedula,
                 coef_incap,
                 Sx_prom ) 

aux_2018 <- aux_2018 %>%  left_join( ., coef_incap, by = 'cedula' )
  

aux_2022 <- indemnizaciones_rt_2022 %>% 
  mutate( mes = ifelse( mes == 'Enero',
                         1,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Febrero',
                         2,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Marzo',
                         3,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Abril',
                         4,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Mayo',
                         5,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Junio',
                         6,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Julio',
                         7,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Agosto',
                         8,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Septiembre',
                         9,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Octubre',
                         10,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Noviembre',
                         11,
                         mes ) ) %>% 
  mutate( mes = ifelse( mes == 'Diciembre',
                         12,
                         mes ) ) %>% 
  mutate( fecha_pago = as.Date( paste0(anio, '-', mes, '-01') ) ) %>%  
  mutate( Sx_prom = promedio_sueldo_teorico ) %>% 
  mutate( coef_incap = liquido_a_pagar / ( 60 * Sx_prom) ) %>% 
    dplyr::select( cedula,
                   sexo,
                   fecha_nacimiento,
                   fecha_pago,
                   P :=  liquido_a_pagar,
                   coef_incap,
                   Sx_prom )



sgrt_pen_tran_indem <- rbind( aux_2018,
                              aux_2022 ) 


sgrt_pen_tran_indem_anio <- sgrt_pen_tran_indem %>% 
  mutate( coef_incap = P / ( 60 * Sx_prom ),
          anio = year( fecha_pago ) ) %>% 
  mutate( x = round( as.numeric( difftime( fecha_pago, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_ing = n( ),
          P = sum( P, na.rm = TRUE ),
          coef_incap = mean( coef_incap, na.rm = TRUE ),
          Sx_prom = mean( Sx_prom, na.rm = TRUE ) ) %>% 
  group_by( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_ing, P,  coef_incap, Sx_prom ) %>% 
  mutate( estado = 13 ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') )

#3. (2 ---> 14) Transiciones de activo a beneficiario de subsidios----------------------------------

sgrt_pen_tran_sub_anio <- subsidios_rtr %>% 
  mutate( fecha_pago = fecha_transferencia,
          coef_incap = porc_sub / 100 ) %>%
  mutate( anio = year( fecha_pago ) ) %>% 
  mutate( x = round( as.numeric( difftime( fecha_pago, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( P = sum( valor , na.rm = TRUE ),
          coef_incap = mean( coef_incap, na.rm = TRUE ),
          dias_reposo = mean( dias_reposo, na.rm = TRUE ),
          Nx_ing = n( ) ) %>% 
ungroup( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio,
                 sexo,
                 x,
                 Nx_ing,
                 P,
                 coef_incap,
                 dias_reposo )  %>% 
  mutate( estado = 14 ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') )
#4. Transiciones de activo a pensionistas de orfandad-----------------------------------------------

sgrt_pen_tran_orf <- prestaciones_orfandad %>%
  left_join( ., inventario_jubilados, by = c('cedula'='cedula',  'cod_tipo_prestacion'='tipo_prestacion' ) ) %>% 
  mutate( planilla = as.Date( paste0(anio, '-', mes,'-01'), "%Y-%m-%d" ) ) %>%
  group_by( cedula, anio, mes ) %>% 
  mutate(  tot_ingr =  sum(  ifelse(  rubro %in% c( 1,
                                                    2,
                                                    3,
                                                    5,
                                                    9,
                                                    10,
                                                    22,
                                                    55,
                                                    60,
                                                    69,
                                                    89,
                                                    101,
                                                    102,
                                                    103,
                                                    345,
                                                    374,
                                                    375,
                                                    376  ),
                                      valor,
                                      0 ), na.rm = TRUE  ),
           decimo_tercero = sum(  ifelse(  rubro %in% c( 345, 374, 376, 10  ),
                                           valor,
                                           0 ), na.rm = TRUE  ),
           decimo_cuarto = sum(  ifelse(  rubro %in% c( 9, 375  ),
                                          valor,
                                          0 ), na.rm = TRUE  ),
           renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, mes, cedula, .keep_all = TRUE ) %>% 
  group_by( cedula ) %>% 
  mutate( fecha_ingreso = min( planilla, na.rm = TRUE )  ) %>%
  mutate( fecha_salida = max( planilla, na.rm = TRUE )  ) %>% 
  ungroup( ) %>% 
  mutate( fecha_ingreso = if_else( fecha_ingreso == as.Date( '2012-01-01' ),
                                   NA,
                                   fecha_ingreso ) ) %>% 
  mutate( fecha_salida = if_else( fecha_salida == as.Date( '2020-12-01' ),
                                  NA,
                                  fecha_salida ) ) %>% 
  group_by( anio, cedula ) %>% 
  mutate( P = sum( renta_mensual, na.rm = TRUE ),
          Sx_prom = Sx_prom,
          coef_incap = coef_incap,
          P_13 = sum( decimo_tercero, na.rm = TRUE ),
          P_14 = sum( decimo_cuarto, na.rm = TRUE ),
          P_Tot = sum( tot_ingr, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( cedula ) %>% 
  mutate( max_planilla = max( planilla, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( ., anio, cedula, .keep_all = TRUE ) %>% 
  left_join( ., sbu %>% dplyr::select( anio, sbu ), by = 'anio' ) %>% 
  mutate( coef_decima_tercera = P_13 / P,
          coef_decima_cuarta = P_14 / sbu ) %>% 
  filter( tot_ingr > 0 ) %>% 
  dplyr::select( -rubro, -descripcion )

##4.1. (0 ---> 15) Ingreso de pensionistas de orfandad-----------------------------------------

aux_a <- sgrt_pen_tran_orf %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( anio, '-06-30') ), fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( ERx_incap = n( ),
          P = sum( P, na.rm = TRUE ),
          P_13= sum( P_13, na.rm = TRUE ),
          P_14 = sum( P_14, na.rm = TRUE ),
          P_Tot = sum( P_Tot, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select(  anio, sexo, x, coef_incap, ERx_incap, P, P_13, P_14, P_Tot )

aux_b <- sgrt_pen_tran_orf %>% 
  mutate( x = round( as.numeric( difftime( fecha_ingreso, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2012-01-01" ) ) %>% 
  mutate( anio = year( fecha_ingreso ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_ing = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_ing ) 

aux_ing <- full_join( aux_a, aux_b, by = c('anio', 'sexo', 'x') )

##4.2. (15 ---> 0 ) Salida de pensionistas de orfandad----------------------------------------------

aux_sal <- sgrt_pen_tran_orf %>% 
  filter( fecha_salida < as.Date( "2020-12-01" ),
          fecha_salida > as.Date( "2012-01-01" ) ) %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( year( fecha_salida ), '-06-30') ), fecha_nacimiento, units = "days") ) / 365, 0 ) ) %>% 
  distinct( cedula, .keep_all = TRUE ) %>% 
  mutate( anio = year( fecha_salida ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_sal = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_sal )

sgrt_pen_tran_orf_anio <- full_join( aux_ing, aux_sal, by = c('anio', 'sexo', 'x') )  %>% 
  arrange( anio, sexo, x ) %>% 
  mutate( estado = 15 ) %>% 
  dplyr::select( anio,
                 sexo,
                 x, 
                 Nx_ing,
                 Nx_sal,
                 ERx_incap,
                 coef_incap,
                 P,
                 #Sx_prom,
                 P_13,
                 P_14,
                 P_Tot, 
                 estado ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') )


#5. Transiciones de activo a pensionistas de viudedad-----------------------------------------------

sgrt_pen_tran_viu <- prestaciones_viudez %>%
  left_join( ., inventario_jubilados, by = c('cedula'='cedula',  'cod_tipo_prestacion'='tipo_prestacion' ) ) %>% 
  mutate( planilla = as.Date( paste0(anio, '-', mes,'-01'), "%Y-%m-%d" ) ) %>%
  group_by( cedula, anio, mes ) %>% 
  mutate(  tot_ingr =  sum(  ifelse(  rubro %in% c( 1,
                                                    2,
                                                    3,
                                                    5,
                                                    9,
                                                    10,
                                                    22,
                                                    55,
                                                    60,
                                                    69,
                                                    89,
                                                    101,
                                                    102,
                                                    103,
                                                    345,
                                                    374,
                                                    375,
                                                    376  ),
                                      valor,
                                      0 ), na.rm = TRUE  ),
           decimo_tercero = sum(  ifelse(  rubro %in% c( 345, 374, 376, 10  ),
                                           valor,
                                           0 ), na.rm = TRUE  ),
           decimo_cuarto = sum(  ifelse(  rubro %in% c( 9, 375  ),
                                          valor,
                                          0 ), na.rm = TRUE  ),
           renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, mes, cedula, .keep_all = TRUE ) %>% 
  group_by( cedula ) %>% 
  mutate( fecha_ingreso = min( planilla, na.rm = TRUE )  ) %>%
  mutate( fecha_salida = max( planilla, na.rm = TRUE )  ) %>% 
  ungroup( ) %>% 
  group_by( anio, cedula ) %>% 
  mutate( P = sum( renta_mensual, na.rm = TRUE ),
          coef_incap = coef_incap,
          P_13 = sum( decimo_tercero, na.rm = TRUE ),
          P_14 = sum( decimo_cuarto, na.rm = TRUE ),
          P_Tot = sum( tot_ingr, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  group_by( cedula ) %>% 
  mutate( max_planilla = max( planilla, na.rm = TRUE ) ) %>%
  ungroup( ) %>% 
  distinct( ., anio, cedula, .keep_all = TRUE ) %>% 
  left_join( ., sbu %>% dplyr::select( anio, sbu ), by = 'anio' ) %>% 
  mutate( coef_decima_tercera = P_13 / P,
          coef_decima_cuarta = P_14 / sbu ) %>% 
  filter( tot_ingr > 0 ) %>% 
  dplyr::select( -rubro, -descripcion )

##5.1. (0 ---> 16) Ingreso de pensionistas de viudedad-----------------------------------------

aux_a <- sgrt_pen_tran_viu %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( anio, '-06-30') ), fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( ERx_incap = n( ),
          P = sum( P, na.rm = TRUE ),
          P_13= sum( P_13, na.rm = TRUE ),
          P_14 = sum( P_14, na.rm = TRUE ),
          P_Tot = sum( P_Tot, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select(  anio, sexo, x, coef_incap,  ERx_incap, P, P_13, P_14, P_Tot )

aux_b <- sgrt_pen_tran_viu %>% 
  mutate( x = round( as.numeric( difftime( fecha_ingreso, fecha_nacimiento, units = "days") ) / 365 ), 0 ) %>% 
  distinct( ., cedula, .keep_all = TRUE ) %>% 
  filter( fecha_ingreso > as.Date( "2012-01-01" ) ) %>% 
  mutate( anio = year( fecha_ingreso ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_ing = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_ing ) 

aux_ing <- full_join( aux_a, aux_b, by = c('anio', 'sexo', 'x') )

##5.2. (16 ---> 0 ) Salida de pensionistas de viudedad----------------------------------------------

aux_sal <- sgrt_pen_tran_viu %>% 
  mutate( x = round( as.numeric( difftime( as.Date( paste0( year( fecha_salida ), "-06-30" ) ) , fecha_nacimiento, units = "days" ) / 365 ), 0 ) ) %>%
  filter( fecha_salida < as.Date( "2020-12-01" ),
          fecha_salida > as.Date( "2012-01-01" ) ) %>% 
  distinct( cedula, .keep_all = TRUE ) %>%
  mutate( anio = year( fecha_salida ) ) %>% 
  group_by( anio, sexo, x ) %>% 
  mutate( Nx_sal = n( ) ) %>% 
  ungroup( ) %>% 
  distinct( ., anio, sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, x, Nx_sal )

sgrt_pen_tran_viu_anio <- full_join( aux_ing, aux_sal, by = c('anio', 'sexo', 'x') )  %>% 
  arrange( anio, sexo, x ) %>% 
  mutate( estado = 16 ) %>% 
  dplyr::select( anio,
                 sexo,
                 x, 
                 Nx_ing,
                 Nx_sal,
                 ERx_incap,
                 coef_incap,
                 P,
                 #Sx_prom,
                 P_13,
                 P_14,
                 P_Tot, 
                 estado ) %>% 
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M') ) %>% 
  filter( x <= 105 )
#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  sgrt_pen_tran_pa_pt_pp,
       sgrt_pen_tran_orf,
       sgrt_pen_tran_viu,
       sgrt_pen_tran_pa_pt_pp_anio,
       sgrt_pen_tran_indem_anio,
       sgrt_pen_tran_sub_anio,
       sgrt_pen_tran_orf_anio,
       sgrt_pen_tran_viu_anio,
       file = parametros$demo_rdata_rtr_tran_prep ) 
#Limpiar Ram----------------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )
