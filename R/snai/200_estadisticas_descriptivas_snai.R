message( paste( rep( "-", 100 ), collapse = "" ) )
#Cargando Rdatas------------------------------------------------------------------------------------
message( "\tCargando datos" )
load( paste0( parametros$RData, "MIESS_censo_recicladores.RData" ) )


#1. Funciones---------------------------------------------------------------------------------------
message( "\tCargando funciones" )
##1.1. Función evolución de beneficiarios-----------------------------------------------------------

ben_fun <- function( .data22 ) {
  b <- .data22 %>% 
    distinct(  anio, mes, cedula, .keep_all = TRUE  ) %>%
    group_by(  anio, mes, sexo  ) %>%
    mutate(  beneficiarios_12 = n(   )  ) %>%
    ungroup(   ) %>%
    filter(  mes == 12  ) %>% 
    distinct(  anio, sexo, .keep_all = TRUE  ) %>%
    dplyr::select(  anio,
                    sexo,
                    beneficiarios_12  ) %>%
    spread(   ., sexo, value = c( beneficiarios_12  ),  sep = "ben12"  ) %>%
    mutate(  ben12 =  rowSums( .[2:3] )  )
  
  b <- .data22 %>%
    distinct(  anio, cedula, .keep_all = TRUE  ) %>%
    group_by(  anio, sexo  ) %>%
    mutate(  beneficiarios = n(   )  ) %>%
    ungroup(   ) %>%
    distinct(  anio, sexo, .keep_all = TRUE  ) %>%
    dplyr::select(  anio, sexo, beneficiarios ) %>%
    spread(   ., sexo, value = c( beneficiarios  ),  sep = "ben"  ) %>%
    mutate(  ben =  rowSums( .[2:3] )  ) %>%
    left_join( b, ., by = 'anio'  )
  
  aux <- b %>%
    mutate(  incremento = ben - lag(  ben  )  ) %>%
    mutate(  tasa_crecimiento = 100 * incremento / lag(  ben  )  )
  
  return(  aux  )
}

##1.2. Función evolución de beneficios entregados---------------------------------------------------
monto_fun <- function( .data22 ) {
  b <- .data22 %>%
    group_by(  anio  ) %>%
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
             renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto,
             financiamiento_decimas_pensiones = sum(  ifelse(  rubro %in% c( 16 ),
                                                               valor,
                                                               0 ), na.rm = TRUE  ),
             financiamiento_fondo_mortuorio = sum(  ifelse(  rubro %in% c( 17 ),
                                                             valor,
                                                             0 ), na.rm = TRUE  ),
             
             tot_desc = sum(  ifelse(  rubro %in% c(  11,
                                                      12,
                                                      13,
                                                      14,
                                                      15,
                                                      16,
                                                      17,
                                                      111,
                                                      211,
                                                      212,
                                                      213,
                                                      214,
                                                      216,
                                                      217,
                                                      218,
                                                      220,
                                                      221,
                                                      228,
                                                      235,
                                                      328,
                                                      329,
                                                      360,
                                                      361  ),
                                       valor,
                                       0 ), na.rm = TRUE  ),
             otros_descuentos = tot_desc - financiamiento_fondo_mortuorio - financiamiento_decimas_pensiones,
             liq_pagar = tot_ingr - tot_desc  ) %>%
    ungroup(   ) %>%
    distinct(  ., anio, .keep_all = TRUE  ) %>%
    dplyr::select(  anio,
                    renta_mensual,
                    decimo_tercero,
                    decimo_cuarto,
                    tot_ingr,
                    financiamiento_decimas_pensiones,
                    financiamiento_fondo_mortuorio,
                    otros_descuentos,
                    tot_desc,
                    liq_pagar  )
  
  aux <- b 
  
  return(  aux  )
}


##1.3. Función distribución de pensionistas por edad y sexo a 31/12/2021----------------------------


ben_pir_fun <- function( .data22, anio_valor ) {
  a <- .data22 %>% 
    filter(  mes == 12, anio == {{anio_valor}}  ) %>% 
    distinct(   mes, cedula, .keep_all = TRUE  ) %>%
    mutate(  fecha_nacimiento = if_else(  is.na( fecha_nacimiento ),
                                          mean(  fecha_nacimiento, na.rm =TRUE ),
                                          fecha_nacimiento  )  ) %>%
    mutate( edad = round( age_calc( fecha_nacimiento,
                                    enddate = as.Date( "31/12/2021","%d/%m/%Y" ),
                                    units = "years",
                                    precise = FALSE  )  ) ) %>%
    group_by(  sexo, edad  ) %>%
    mutate(  freq = n(   )  ) %>%
    ungroup(   ) %>%
    distinct(  sexo, edad, .keep_all = TRUE  ) %>%
    mutate(  fdp = freq / sum(  freq  )  ) %>% 
    dplyr::select(  sexo,
                    edad,
                    freq, 
                    fdp  ) %>%
    arrange(  sexo, edad  )
  return(  a  )
}


##1.4. Función distribución de pensión promedio por edad y sexo a 31/12/2021------------------------

pension_pir_fun <- function( .data22, anio_valor ) {
  a <- .data22 %>% 
    filter(  anio == {{anio_valor}}  ) %>% 
    group_by(  anio, mes, cedula  ) %>%
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
                                        0  ), na.rm = TRUE  )  ) %>%
    ungroup(   ) %>%
    distinct(   anio, mes, cedula, .keep_all = TRUE  ) %>%
    mutate(  fecha_nacimiento = if_else(  is.na( fecha_nacimiento ),
                                          mean(  fecha_nacimiento, na.rm =TRUE ),
                                          fecha_nacimiento  )  ) %>%
    mutate( edad = round( age_calc( fecha_nacimiento,
                                    enddate = as.Date( "31/12/2021","%d/%m/%Y" ),
                                    units = "years",
                                    precise = FALSE  )  ) ) %>%
    group_by(  sexo, edad  ) %>%
    mutate(  pen_promedio = mean(  tot_ingr, na.rm = TRUE  )  ) %>%
    ungroup(   ) %>%
    distinct(  sexo, edad, .keep_all = TRUE  ) %>%
    dplyr::select(  anio,
                    sexo,
                    edad,
                    pen_promedio  ) %>%
    arrange(  sexo, edad  )
  return(  a  )
}

## 1.5. Función distribución de beneficiarios por rangos de monto de las pensión mensual------------

rango_monto <- function( .data22, anio_valor ) {
  
  a <- .data22 %>% 
    filter(  anio == {{anio_valor}}  ) %>% 
    group_by(  anio, mes, cedula  ) %>%
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
                                        0  ), na.rm = TRUE  )  ) %>%
    ungroup(   ) %>%
    distinct(  cedula, .keep_all = TRUE  )
  
  cortes_monto <- c(  quantile( a$tot_ingr, probs = seq(  0, 1, length.out = 11  )  )  )
  
  etiquetas_monto<-c( paste0( "( \\$",formatC(  cortes_monto[1:length( cortes_monto )-1], 
                                                digits = 0, format = 'f', big.mark = '.', decimal.mark = ','  ),
                              "-\\$",formatC(  cortes_monto[2:length( cortes_monto )], 
                                               digits = 0, format = 'f', big.mark = '.', decimal.mark = ','  ),"]" ) )
  
  a <- a %>%
    mutate(  rango_monto = cut(  tot_ingr, 
                                 breaks = cortes_monto,
                                 labels = etiquetas_monto,
                                 include.lowest = TRUE,
                                 right = TRUE ) ) %>%
    group_by(  rango_monto, sexo  ) %>%
    mutate(  ben = n(  )  ) %>%
    ungroup(   ) %>%
    distinct(  sexo, rango_monto, .keep_all = TRUE  ) %>%
    dplyr::select(  sexo,
                    rango_monto,
                    ben  ) %>%
    arrange(  sexo, rango_monto  ) %>%
    spread(   ., sexo, value = c(  ben  ),  sep = "ben12"  )  %>%
    mutate_if(  is.numeric , replace_na, replace = 0 ) %>%
    mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
    mutate(  rango_monto = as.character(  rango_monto  )  ) %>%
    rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
    mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
    mutate(  por_sexoben12F = 100 * 2 * sexoben12F / sum(  total  ),
             por_sexoben12M = 100* 2 * sexoben12M / sum(  total  ),
             por_total = 100 * 2 * total / sum(  total  )  ) %>%
    dplyr::select(  rango_monto,
                    sexoben12F,
                    por_sexoben12F,
                    sexoben12M,
                    por_sexoben12M,
                    total,
                    por_total  ) %>%
    distinct(  ., rango_monto, .keep_all = TRUE  )
  
  
  return(  a  )
}


#2. Tabla evolución de pensionistas-----------------------------------------------------------------
message( "\tTablas sobre pensiones del SGRT" )
tab_evo_ben_pp <- ben_fun(  prestaciones_pp  )
tab_evo_ben_pt <- ben_fun(  prestaciones_pt  )
tab_evo_ben_pa <- ben_fun(  prestaciones_pa  )
tab_evo_ben_vo <- ben_fun(  prestaciones_viudez  )
tab_evo_ben_of <- ben_fun(  prestaciones_orfandad  )

#3. Tabla evolución de montos entregados------------------------------------------------------------

tab_evo_monto_pp <- monto_fun( prestaciones_pp  )
tab_evo_monto_pt <- monto_fun(  prestaciones_pt  )
tab_evo_monto_pa <- monto_fun(  prestaciones_pa  )
tab_evo_monto_vo <- monto_fun(  prestaciones_viudez  )
tab_evo_monto_of <- monto_fun(  prestaciones_orfandad  )

#4. Tabla de edades para pirámides------------------------------------------------------------------

pir_ben_pp <- ben_pir_fun(  prestaciones_pp, '2020'  )
pir_ben_pt <- ben_pir_fun(  prestaciones_pt, '2020'  )
pir_ben_pa <- ben_pir_fun(  prestaciones_pa, '2020'  )
pir_ben_vo <- ben_pir_fun(  prestaciones_viudez, '2020'  )
pir_ben_of <- ben_pir_fun(  prestaciones_orfandad, '2020'  )

#5. Tablas de pirámides de pensiones----------------------------------------------------------------

pir_pensiones_pp <- pension_pir_fun(  prestaciones_pp, '2020'  )
pir_pensiones_pt <- pension_pir_fun(  prestaciones_pt, '2020'  )
pir_pensiones_pa <- pension_pir_fun(  prestaciones_pa, '2020'  )
pir_pensiones_vo <- pension_pir_fun(  prestaciones_viudez, '2020'  )
pir_pensiones_of <- pension_pir_fun(  prestaciones_orfandad, '2020'  )

#6. Tablas de los beneficiarios por rangos de pensiones---------------------------------------------

tab_rango_monto_pp <- rango_monto(  prestaciones_pp, '2020'  )
tab_rango_monto_pt <- rango_monto(  prestaciones_pt, '2020'  )
tab_rango_monto_pa <- rango_monto(  prestaciones_pa, '2020'  )
tab_rango_monto_vo <- rango_monto(  prestaciones_viudez, '2020'  )
tab_rango_monto_of <- rango_monto(  prestaciones_orfandad, '2020'  )

#7. Subsidios---------------------------------------------------------------------------------------
message( "\tTablas sobre subsidios del SGRT" )
##7.1 Tabla evolución de beneficiarios en subsidios-------------------------------------------------

b <- subsidios_rtr %>% 
  #distinct(  anio, mes, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, mes, sexo  ) %>%
  mutate(  beneficiarios_12 = n(   )  ) %>%
  ungroup(   ) %>%
  filter(  mes == 12  ) %>% 
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  beneficiarios_12  ) %>%
  spread(   ., sexo, value = c( beneficiarios_12  ),  sep = "ben12"  ) %>%
  mutate(  ben12 =  rowSums( .[2:ncol(.)] )  )

b <- subsidios_rtr %>%
  #distinct(  anio, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, sexo  ) %>%
  mutate(  beneficiarios = n(   )  ) %>%
  ungroup(   ) %>%
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio, sexo, beneficiarios ) %>%
  spread(   ., sexo, value = c( beneficiarios  ),  sep = "ben"  ) %>%
  mutate(  ben =  rowSums( .[2:3] )  ) %>%
  mutate(  incremento = ben - lag(  ben  )  ) %>%
  mutate(  tasa_crecimiento = 100 * incremento / lag(  ben  )  ) %>%
  left_join( b, ., by = 'anio'  )

tab_evo_ben_subsidios <- b

##7.2 Tabla evolución de montos entregados----------------------------------------------------------

b <- subsidios_rtr %>% 
  #distinct(  anio, mes, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, mes, sexo  ) %>%
  mutate(  subsidios_12 = sum(  valor, na.rm = TRUE )  ) %>%
  ungroup(   ) %>%
  filter(  mes == 12  ) %>% 
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  subsidios_12  ) %>%
  spread(   ., sexo, value = c( subsidios_12 ),  sep = "ben12"  ) %>%
  mutate(  subsidios_12 =  rowSums( .[2:3] )  )

b <- subsidios_rtr %>%
  #distinct(  anio, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, sexo  ) %>%
  mutate(  subsidios = sum(  valor, na.rm = TRUE )  ) %>%
  ungroup(   ) %>%
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio, sexo, subsidios ) %>%
  spread(   ., sexo, value = c( subsidios  ),  sep = "ben"  ) %>%
  mutate(  subsidios =  rowSums( .[2:3] )  ) %>%
  left_join(  b, ., by = 'anio'  )

tab_evo_monto_subsidios <- b %>%
  mutate(  incremento = subsidios - lag(  subsidios  )  ) %>%
  mutate(  tasa_crecimiento = 100 * incremento / lag(  subsidios  )  )

##7.3 Tabla de edades para pirámides----------------------------------------------------------------

pir_ben_subsidios <- ben_pir_fun(  subsidios_rtr, '2020'  )

##7.4 Tablas de pirámides de pensiones--------------------------------------------------------------

a <- subsidios_rtr %>% 
  filter(  anio == 2020  ) %>% 
  group_by(  anio, cedula  ) %>%
  mutate(  valor = sum(  valor, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  cedula, .keep_all = TRUE  ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "30/06/2020","%d/%m/%Y" ),
                                  units = "years",
                                  precise = FALSE  )  ) ) %>%
  group_by(  sexo, edad  ) %>%
  mutate(  pen_promedio = mean(  valor, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, edad, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  edad,
                  pen_promedio  ) %>%
  arrange(  sexo, edad  )


pir_montos_subsidios <- a

##7.5 Tabla Subsidios entregado por rango de montos-------------------------------------------------

a <- subsidios_rtr %>% 
  filter(  anio == 2020  ) %>% 
  group_by(  anio, cedula  ) %>%
  mutate(  valor = sum(  valor, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  cedula, .keep_all = TRUE  )

cortes_monto <- c(  quantile( a$valor, probs = seq(  0, 1, length.out = 11  )  )  )

etiquetas_monto<-c( paste0( "( \\$",formatC(  cortes_monto[1:length( cortes_monto )-1], 
                                              digits = 0, format = 'f', big.mark = '.', decimal.mark = ','  ),
                            "-\\$",formatC(  cortes_monto[2:length( cortes_monto )], 
                                             digits = 0, format = 'f', big.mark = '.', decimal.mark = ','  ),"]" ) )

a <- a %>%
  mutate( rango_monto=cut( valor, 
                           breaks = cortes_monto,
                           labels = etiquetas_monto,
                           include.lowest = TRUE,
                           right = TRUE ) ) %>%
  group_by(  rango_monto, sexo  ) %>%
  mutate(  ben = n(  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, rango_monto, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo,
                  rango_monto,
                  ben  ) %>%
  arrange(  sexo, rango_monto  ) %>%
  spread(   ., sexo, value = c(  ben  ),  sep = "ben12"  )  %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_monto = as.character(  rango_monto  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  por_sexoben12F = 100 * 2 * sexoben12F / sum(  total  ),
           por_sexoben12M = 100* 2 * sexoben12M / sum(  total  ),
           por_total = 100 * 2 * total / sum(  total  )  ) %>%
  dplyr::select(  rango_monto,
                  sexoben12F,
                  por_sexoben12F,
                  sexoben12M,
                  por_sexoben12M,
                  total,
                  por_total  ) %>%
  distinct(  ., rango_monto, .keep_all = TRUE  )


tab_rango_monto_subsidios <- a

#8. Indemnizaciones---------------------------------------------------------------------------------
message( "\tTablas sobre indemnizaciones del SGRT" )
##8.1 Tabla evolución de beneficiarios en indemnizaciones-------------------------------------------------

a <- indemnizaciones_rt_2018 %>% 
  distinct(  anio, mes, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, mes, sexo  ) %>%
  mutate(  beneficiarios_12 = n(   )  ) %>%
  ungroup(   ) %>%
  filter(  mes == 12  ) %>% 
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  beneficiarios_12  ) %>%
  spread(   ., sexo, value = c( beneficiarios_12  ),  sep = "ben12"  ) %>%
  replace( is.na( . ), 0 ) %>%
  mutate(  ben12 =  rowSums(  .[2:3] )  )


a <- indemnizaciones_rt_2018 %>%
  distinct(  anio, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, sexo  ) %>%
  mutate(  beneficiarios = n(   )  ) %>%
  ungroup(   ) %>%
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio, sexo, beneficiarios ) %>%
  spread(   ., sexo, value = c( beneficiarios  ),  sep = "ben"  ) %>%
  mutate(  ben =  rowSums( .[2:3] )  ) %>%
  left_join(  a, ., by = 'anio'  )

b <- indemnizaciones_rt_2022 %>% 
  distinct(  anio, mes, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, mes, sexo  ) %>%
  mutate(  beneficiarios_12 = n(   )  ) %>%
  ungroup(   ) %>%
  filter(  mes == "Diciembre"  ) %>% 
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  beneficiarios_12  ) %>%
  spread(   ., sexo, value = c( beneficiarios_12  ),  sep = "ben12"  ) %>%
  replace( is.na( . ), 0 ) %>%
  mutate(  ben12 =  rowSums(  .[2:3] )  )


b <- indemnizaciones_rt_2022 %>%
  distinct(  anio, cedula, .keep_all = TRUE  ) %>%
  group_by(  anio, sexo  ) %>%
  mutate(  beneficiarios = n(   )  ) %>%
  ungroup(   ) %>%
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio, sexo, beneficiarios ) %>%
  spread(   ., sexo, value = c( beneficiarios  ),  sep = "ben"  ) %>%
  mutate(  ben =  rowSums( .[2:3] )  ) %>%
  left_join(  b, ., by = 'anio'  )

tab_evo_ben_indemnizaciones <- rbind(  a, b ) %>%
  mutate(  incremento = ben - lag(  ben  )  ) %>%
  mutate(  tasa_crecimiento = 100 * incremento / lag(  ben  )  )

##8.2 Tabla evolución de montos entregados----------------------------------------------------------

a <- indemnizaciones_rt_2018 %>% 
  group_by(  anio, mes, sexo  ) %>%
  mutate(  subsidios_12 = sum(  valor, na.rm = TRUE )  ) %>%
  ungroup(   ) %>%
  filter(  mes == 12  ) %>% 
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  subsidios_12  ) %>%
  spread(   ., sexo, value = c( subsidios_12 ),  sep = "ben12"  ) %>%
  replace( is.na( . ), 0 ) %>%
  mutate(  subsidios_12 =  rowSums( .[2:3] )  )

a <- indemnizaciones_rt_2018 %>%
  group_by(  anio, sexo  ) %>%
  mutate(  subsidios = sum(  valor, na.rm = TRUE )  ) %>%
  ungroup(   ) %>%
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio, sexo, subsidios ) %>%
  spread(   ., sexo, value = c(  subsidios  ),  sep = "ben"  ) %>%
  replace( is.na( . ), 0 ) %>%
  mutate(  subsidios =  rowSums( .[2:3] )  ) %>%
  left_join( a, ., by = 'anio'  )


b <- indemnizaciones_rt_2022 %>% 
  group_by(  anio, mes, sexo  ) %>%
  mutate(  subsidios_12 = sum(  valor_pension_concedida , na.rm = TRUE )  ) %>%
  ungroup(   ) %>%
  filter(  mes == 'Diciembre'  ) %>% 
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  subsidios_12  ) %>%
  spread(   ., sexo, value = c( subsidios_12 ),  sep = "ben12"  ) %>%
  mutate(  subsidios_12 =  rowSums( .[2:3] )  )

b <- indemnizaciones_rt_2022 %>%
  group_by(  anio, sexo  ) %>%
  mutate(  subsidios = sum(  valor_pension_concedida , na.rm = TRUE )  ) %>%
  ungroup(   ) %>%
  distinct(  anio, sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  anio, sexo, subsidios ) %>%
  spread(   ., sexo, value = c(  subsidios  ),  sep = "ben"  ) %>%
  mutate(  subsidios =  rowSums( .[2:3] )  ) %>%
  left_join(  b, ., by = 'anio'  )

tab_evo_monto_indemnizaciones <- rbind(  a, b  ) %>%
  mutate(  incremento = subsidios - lag(  subsidios  )  ) %>%
  mutate(  tasa_crecimiento = 100 * incremento / lag(  subsidios  )  )

##8.3 Tabla de edades para pirámides----------------------------------------------------------------

pir_ben_indemnizaciones <- indemnizaciones_rt_2022 %>% 
  filter(  anio == 2020  ) %>% 
  distinct(   mes, cedula, .keep_all = TRUE  ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "31/12/2020","%d/%m/%Y" ),
                                  units = "years",
                                  precise = FALSE  )  ) ) %>%
  group_by(  sexo, edad  ) %>%
  mutate(  freq = n(   )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, edad, .keep_all = TRUE  ) %>%
  mutate(  fdp = freq / sum(  freq  )  ) %>%
  dplyr::select(  anio,
                  sexo,
                  edad,
                  freq,
                  fdp  ) %>%
  arrange(  sexo, edad  )

##8.4 Tablas de pirámides de pensiones--------------------------------------------------------------

a <- indemnizaciones_rt_2022 %>% 
  filter(  anio == 2020  ) %>% 
  group_by(  anio, cedula  ) %>%
  mutate(  valor = sum(  valor_pension_concedida, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  cedula, .keep_all = TRUE  ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "30/06/2020","%d/%m/%Y" ),
                                  units = "years",
                                  precise = FALSE  )  ) ) %>%
  group_by(  sexo, edad  ) %>%
  mutate(  pen_promedio = mean(  valor, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, edad, .keep_all = TRUE  ) %>%
  dplyr::select(  anio,
                  sexo,
                  edad,
                  pen_promedio  ) %>%
  arrange(  sexo, edad  )


pir_montos_indemnizaciones <- a

##8.5 Tabla indemnizaciones entregado por rango de montos-------------------------------------------

a <- indemnizaciones_rt_2022 %>% 
  filter(  anio == 2020  ) %>% 
  group_by(  anio, cedula  ) %>%
  mutate(  valor = sum(  valor_pension_concedida, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  cedula, .keep_all = TRUE  )

cortes_monto <- c(  quantile( a$valor, probs = seq(  0, 1, length.out = 11  )  )  )

etiquetas_monto<-c( paste0( "( \\$",formatC(  cortes_monto[1:length( cortes_monto )-1], 
                                              digits = 0, format = 'f', big.mark = '.', decimal.mark = ','  ),
                            "-\\$",formatC(  cortes_monto[2:length( cortes_monto )], 
                                             digits = 0, format = 'f', big.mark = '.', decimal.mark = ','  ),"]" ) )

a <- a %>%
  mutate( rango_monto=cut( valor, 
                           breaks = cortes_monto,
                           labels = etiquetas_monto,
                           include.lowest = TRUE,
                           right = TRUE ) ) %>%
  group_by(  rango_monto, sexo  ) %>%
  mutate(  ben = n(  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, rango_monto, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo,
                  rango_monto,
                  ben  ) %>%
  arrange(  sexo, rango_monto  ) %>%
  spread(   ., sexo, value = c(  ben  ),  sep = "ben"  )  %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_monto = as.character(  rango_monto  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  por_sexobenF = 100 * 2 * sexobenF / sum(  total  ),
           por_sexobenM = 100* 2 * sexobenM / sum(  total  ),
           por_total = 100 * 2 * total / sum(  total  )  ) %>%
  dplyr::select(  rango_monto,
                  sexobenF,
                  por_sexobenF,
                  sexobenM,
                  por_sexobenM,
                  total,
                  por_total  ) %>%
  distinct(  ., rango_monto, .keep_all = TRUE  )


tab_rango_monto_indemnizaciones <- a

# 9. Estadísticas para redacción del capítulo-------------------------------------------------------
## 9.1 Subsidios------------------------------------------------------------------------------------
message( "\tEstadísticas Subsidios" )
aux <- subsidios_rtr %>%
  filter(  anio == '2020'  ) %>%
  group_by(  sexo  ) %>%
  mutate(  prom_sub = mean(  valor  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo, prom_sub )


aux <- subsidios_rtr %>%
  filter(  anio == '2020'  ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "30/06/2020","%d/%m/%Y" ),
                                  units = "years",
                                  precise = FALSE  )  ) ) %>%
  group_by(  sexo  ) %>%
  mutate(  edad_prom = mean(  edad, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo, edad_prom ) %>%
  left_join(  ., aux, by = 'sexo'  )

aux <- aux %>%
  mutate_if( is.numeric, round, digits = 2 ) %>%
  mutate_at(  c( 2:ncol( . ) ), as.character )

( aux )

## 9.2 Indemnizaciones-------------------------------------------------------------------------------
message( "\tEstadísticas Indemnizaciones" )
aux <- indemnizaciones_rt_2022 %>%
  filter(  anio == '2020'  ) %>%
  group_by(  sexo  ) %>%
  mutate(  prom_sub = mean(  liquido_a_pagar  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo, prom_sub )


aux <- indemnizaciones_rt_2022 %>%
  filter(  anio == '2020'  ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "30/06/2020","%d/%m/%Y" ),
                                  units = "years",
                                  precise = FALSE  )  ) ) %>%
  group_by(  sexo  ) %>%
  mutate(  edad_prom = mean(  edad, na.rm = TRUE  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo, edad_prom ) %>%
  left_join(  ., aux, by = 'sexo'  )

aux <- aux %>%
  mutate_if( is.numeric, round, digits = 2 ) %>%
  mutate_at(  c( 2:ncol( . ) ), as.character )

( aux )

##Función para pensiones----------------------------------------------------------------------------
estadisticas_fun <- function( .data22 ) {
  b <- .data22 %>%
    lazy_dt(   ) %>% 
    filter(  anio == '2020'  ) %>%
    group_by(  cedula, mes  ) %>%
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
             renta_mensual = tot_ingr - decimo_tercero - decimo_cuarto,
             financiamiento_decimas_pensiones = sum(  ifelse(  rubro %in% c( 16 ),
                                                               valor,
                                                               0 ), na.rm = TRUE  ),
             financiamiento_fondo_mortuorio = sum(  ifelse(  rubro %in% c( 17 ),
                                                             valor,
                                                             0 ), na.rm = TRUE  ),
             
             tot_desc = sum(  ifelse(  rubro %in% c(  11,
                                                      12,
                                                      13,
                                                      14,
                                                      15,
                                                      16,
                                                      17,
                                                      111,
                                                      211,
                                                      212,
                                                      213,
                                                      214,
                                                      216,
                                                      217,
                                                      218,
                                                      220,
                                                      221,
                                                      228,
                                                      235,
                                                      328,
                                                      329,
                                                      360,
                                                      361  ),
                                       valor,
                                       0 ), na.rm = TRUE  ),
             otros_descuentos = tot_desc - financiamiento_fondo_mortuorio - financiamiento_decimas_pensiones,
             liq_pagar = tot_ingr - tot_desc  ) %>%
    ungroup(   ) %>%
    filter(  !is.na( fecha_nacimiento )  ) %>%
    mutate( edad = round( age_calc( fecha_nacimiento,
                                    enddate = as.Date( "31/12/2020","%d/%m/%Y" ),
                                    units = "years",
                                    precise = FALSE  )  ) ) %>%
    group_by(  anio, sexo  ) %>%
    mutate(  prom_pension = mean(  renta_mensual, na.rm = TRUE  )  ) %>%
    mutate(  prom_tot_ingr = mean(  tot_ingr, na.rm = TRUE  )  ) %>%
    ungroup(   ) %>%
    filter(  mes == '12'  ) %>%
    group_by(  sexo  ) %>%
    mutate(  prom_edad = mean(  edad, na.rm = TRUE  )  ) %>%
    ungroup(   ) %>%
    distinct(  ., sexo, .keep_all = TRUE  ) %>%
    dplyr::select(  sexo,
                    prom_edad,
                    prom_pension,
                    prom_tot_ingr  ) %>%
    as_tibble(   )
  
  return(  b  )
}


## 9.2 Pensiones Incapacidad Permanente Parcial-----------------------------------------------------
message( "\tEstadísticas Incapacidad Permanente Parcial" )

aux <- estadisticas_fun( prestaciones_pp ) %>%
  mutate_if( is.numeric, round, digits = 2 ) %>%
  mutate_at(  c( 2:ncol( . ) ), as.character )

( aux )

## 9.3 Pensiones Incapacidad Permanente Total-------------------------------------------------------
message( "\tEstadísticas Incapacidad Permanente Total" )

aux <- estadisticas_fun( prestaciones_pt  ) %>%
  mutate_if( is.numeric, round, digits = 2 ) %>%
  mutate_at(  c( 2:ncol( . ) ), as.character )

( aux )

## 9.4 Pensiones Incapacidad Permanente Absoluta----------------------------------------------------
message( "\tEstadísticas Incapacidad Permanente Absoluta" )

aux <- estadisticas_fun( prestaciones_pa  ) %>%
  mutate_if( is.numeric, round, digits = 2 ) %>%
  mutate_at(  c( 2:ncol( . ) ), as.character )

( aux )


## 9.5 Pensiones viudez-----------------------------------------------------------------------------
message( "\tEstadísticas viudez" )

aux <- estadisticas_fun( prestaciones_viudez  ) %>%
  mutate_if( is.numeric, round, digits = 2 ) %>%
  mutate_at(  c( 2:ncol( . ) ), as.character )

( aux )

## 9.5 Pensiones orfandad---------------------------------------------------------------------------
message( "\tEstadísticas orfandad" )

aux <- estadisticas_fun( prestaciones_orfandad  ) %>%
  mutate_if( is.numeric, round, digits = 2 ) %>%
  mutate_at(  c( 2:ncol( . ) ), as.character )

( aux )

#10 . Estadísticas de cotizantes--------------------------------------------------------------------

aux <- sgo_act_tran_anio %>% 
  filter( anio <= 2020, anio >= 2012 ) %>% 
  group_by( anio, sexo ) %>% 
  mutate( ERx = sum( ERx_act, na.rm = TRUE ),
          masa_sal = sum( S , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( anio, sexo, ERx, masa_sal )

##6.1. Evolución de cotizantes-----------------------------------------------------------------------

evo_er_sgo <- aux %>%
  dplyr::select( anio, sexo, ERx ) %>% 
  pivot_wider( names_from = sexo, values_from = ERx, names_prefix = "ERx_" ) %>% 
  mutate( total = ERx_H + ERx_M ) %>% 
  mutate( incremento = total - lag( total ) ) %>% 
  mutate( porc = incremento / lag( total ) )

##6.2. Evolución de masa salarial--------------------------------------------------------------------

evo_masa_sgo <- aux %>%
  dplyr::select( anio, sexo, masa_sal ) %>% 
  pivot_wider( names_from = sexo, values_from = masa_sal, names_prefix = "masa_" ) %>% 
  mutate( total = masa_H + masa_M ) %>% 
  mutate( incremento = total - lag( total ) ) %>% 
  mutate( porc = incremento / lag( total ) )

##6.3 Distribución de salarios----------------------------------------------------------------------

dist_sal_edad_sexo <- sgo_act_tran_anio %>% 
  filter( anio == 2020 ) %>% 
  filter(  x < 106, x > 14 ) %>% 
  group_by( x, sexo ) %>% 
  mutate( ERx = sum( M2x, na.rm = TRUE ),
          masa_sal = sum( S , na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  mutate( sal_prom = masa_sal / ERx ) %>% 
  distinct( x, sexo, .keep_all = TRUE ) %>% 
  dplyr::select( x, sexo, sal_prom ) %>% 
  filter( sal_prom > 0, is.finite( sal_prom ) )


#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  tab_evo_ben_pt,
       tab_evo_ben_pp,
       tab_evo_ben_pa,
       tab_evo_ben_vo,
       tab_evo_ben_of,
       tab_evo_monto_pp,
       tab_evo_monto_pt,
       tab_evo_monto_pa,
       tab_evo_monto_vo,
       tab_evo_monto_of,
       pir_ben_pp,
       pir_ben_pt,
       pir_ben_pa,
       pir_ben_vo,
       pir_ben_of,
       pir_pensiones_pp,
       pir_pensiones_pt,
       pir_pensiones_pa,
       pir_pensiones_vo,
       pir_pensiones_of,
       tab_rango_monto_pp,
       tab_rango_monto_pt,
       tab_rango_monto_pa,
       tab_rango_monto_vo,
       tab_rango_monto_of,
       tab_evo_ben_subsidios,
       tab_evo_monto_subsidios,
       pir_ben_subsidios,
       pir_montos_subsidios,
       tab_rango_monto_subsidios,
       tab_evo_ben_indemnizaciones,
       tab_evo_monto_indemnizaciones,
       pir_ben_indemnizaciones,
       pir_montos_indemnizaciones,
       tab_rango_monto_indemnizaciones,
       evo_er_sgo,
       evo_masa_sgo,
       dist_sal_edad_sexo,
       file = paste0(  parametros$RData_seg, 'IESS_RTR_tablas_demografia.RData'  )  )
#Limpiar Ram----------------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )

