message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tLectura del contexto económico' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$macro_rdata_info )
load( file = parametros$macro_rdata_macro_est )
load( file = parametros$macro_rdata_biess_proy )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Parámetros-----------------------------------------------------------------------------------------
if( parametros$seguro %in% c( 'SAL' ) ) {
  anio_fin = 10 + 2020
} else if ( parametros$seguro %in% c( 'SSC' ) ) {
  anio_fin = 20 + 2020
} else {
  anio_fin = 40 + 2020
}

anio_ini <- 2021
anio_corte <- 2020

#Tabla del contexto económico-----------------------------------------------------------------------
message( '\tTablas del contexto económico' )

#Tabla de la inflación------------------------------------------------------------------------------

aux <- inflacion %>%
  filter( mes == '12',
          anio <= anio_corte ) %>%
  dplyr::select( periodo,
                 ipc,
                 inflacion_mensual,
                 inflacion_acumulada,
                 inflacion_promedio_acumulada ) %>%
  mutate( periodo = as.character( periodo ) )


aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ,2 ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inflacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla de desempleo---------------------------------------------------------------------------------
aux <- desempleo %>%
  filter( mes == '12',
          anio <= anio_corte ) %>%
  dplyr::select( periodo,
                 desempleo_hombre,
                 desempleo_mujer,
                 desempleo_nacional,
                 empleo_adecuado_pleno_n,
                 subempleo_nacional ) %>%
  mutate( periodo = as.character( format( periodo, "%b %Y" ) ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_desempleo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla de PIB---------------------------------------------------------------------------------------
aux <- pib_real %>%
  filter( anio >= '2000',
          anio <= anio_corte ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 0, 2 ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pib_real', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla de SBU---------------------------------------------------------------------------------------
aux <- sbu %>%
  filter( anio <= anio_corte) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sbu', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla de salarios promedio-------------------------------------------------------------------------
aux <- salarios %>%
  filter( mes == '12', anio <= anio_corte ) %>%
  mutate( anio = as.character( anio ) ) %>%
  arrange( periodo ) %>%
  mutate( incremento = sal_prom - lag( sal_prom ) ) %>%
  mutate( tasa = 100 * (sal_prom - lag( sal_prom )) / lag( sal_prom ) ) %>%
  na.omit( . ) %>%
  dplyr::select( periodo,
                 sal_prom,
                 incremento,
                 tasa ) %>%
  mutate( periodo = as.character( format( periodo, "%b %Y" ) ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_salarios', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla de aumento de pensiones----------------------------------------------------------------------
aux <- incre_pensiones %>%
  dplyr::select( -x2021,
                 -x2022 )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_incre_pensiones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de pensiones mínimas-------------------------------------------------------------------------
aux <- pension_min %>%
  filter( anio <= anio_corte ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, rep( 2, 6 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pension_min', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de pensiones máximas-------------------------------------------------------------------------
aux <- pension_max %>%
  filter( anio <= anio_corte ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, 0, rep( 2, 7 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pension_max', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de tasas de interés---------------------------------------------------------------------------
aux <- tasas_interes %>%
  filter( anio <= anio_corte, mes == '12' ) %>%
  mutate( periodo = as.character(  format( periodo, "%b %Y") ) ) %>%
  dplyr::select( periodo,
                 tasa_activa,
                 tasa_pasiva,
                 spread )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 3 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_tasas_interes', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de ROA---------------------------------------------------------------------------------------
aux <- roa %>%
  filter( anio <= anio_corte ) %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( across( where(is.numeric), ~ .x * 100 ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 5 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_roa', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de ROE---------------------------------------------------------------------------------------
aux <- roe %>%
  filter( anio <= anio_corte ) %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( across( where(is.numeric), ~ .x * 100 ) )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 5 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_roe', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla rendimientos del BIESS-----------------------------------------------------------------------
aux <- rendimiento_biess %>%
  filter( fecha <= as.Date("31/12/2020", "%d/%m/%Y" ),
          mes == '12' ) %>%
  mutate( fecha = as.character( format( fecha, "%b %Y") ) ) %>%
  dplyr::select( -mes ) %>%
  mutate( rendimiento = 100 * rendimiento ) %>%
  na.omit( )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 7 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rendimiento_biess', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de resumen de hipótesis----------------------------------------------------------------------
aux <- hipotesis

aux_xtab <- xtable( aux, digits = c( 0, 0, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_hip_macro', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux <- hipotesis
aux <- rbind( aux, c('Tasa actuarial', 6.25))
aux$tasas <- as.numeric( aux$tasas )

aux_xtab <- xtable( aux, digits = c( 0, 0, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_hip_macro_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


#Tabla de predicciones por año----------------------------------------------------------------------
message( '\tTablas del modelo predictivo de las hipótesis macro-económicas' )

aux_1 <- tasas_macro_anuales %>% 
  dplyr::select( anio,
                 t_sal ) %>% 
  mutate( anio = as.character( anio ) )

aux <- predicciones_anuales %>%
  filter( anio >= anio_corte + 1, anio <= anio_fin ) %>%
  mutate( anio = as.character( anio ),
          pib_anual = pib_anual / 1000 ) %>% 
  left_join(., aux_1, by = 'anio' ) %>% 
  dplyr::select( anio,
                 pib_anual,
                 tp_anual,
                 t_sal,
                 sbu_anual,
                 ipc_dic,
                 inf_anual )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_tasas_macro_pred', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de coeficientes del modelo-------------------------------------------------------------------

# aux <- coeficientes
# 
# aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 5, 6 ) ) )
# 
# aux_xtab <- tildes_a_latex( aux_xtab )
# 
# print( aux_xtab, 
#        file = paste0( parametros$resultado_tablas, 'iess_coeficientes', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE, include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = nrow(aux),
#        sanitize.text.function = identity )

#Tabla de prueba de normalidad----------------------------------------------------------------------
aux <- jarque_bera_test

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 5, 2 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

aux_xtab <- xtable( aux_xtab, digits = c( 0, 0, 5, 5 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_jb_test', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de prueba de homocedasticidad----------------------------------------------------------------
aux <- homocedasticidad

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 5, 2 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_homocedasticidad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla matriz de correlaciones----------------------------------------------------------------------
aux <- ma_correlaciones

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 5, 5 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

aux_xtab <- xtable( aux_xtab, digits = c( 0, 0, rep( 5, 5 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ma_correlaciones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla matriz de p-valores de la prueba de multicolinealidad----------------------------------------
aux <- ma_multi_p_valores

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 5, 5 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

aux_xtab <- xtable( aux_xtab, digits = c( 0, 0, rep( 5, 5 ) ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ma_multi_p_valores', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla matriz de covarianza-------------------------------------------------------------------------

# aux <- covarianza
# 
# aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )
# 
# aux_xtab <- tildes_a_latex( aux_xtab )
# 
# print( aux_xtab, 
#        file = paste0( parametros$resultado_tablas, 'iess_ma_covarianza', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE, include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = nrow(aux),
#        sanitize.text.function = identity )


#Tabla de la prueba de nulidad de coeficientes------------------------------------------------------
aux <- test_nulidad_nulidad

aux_xtab <- xtable( aux, digits = c( 0, 0, 2 ) )
aux_xtab <- tildes_a_latex( aux_xtab )
print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_test_nulidad_nulidad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla del test de Box Ljung------------------------------------------------------------------------
aux <- box_ljung 

aux_xtab <- xtable( aux, digits = c( 0, 0, 5, 0, 5 ) )
print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_test_box_ljung', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de coeficientes del modelo de series multivariante-------------------------------------------
modelo <- coeficientes$varresult

modelo_diff_pib <- modelo$diff_pib$coefficients
modelo_diff_tasa_pasiva <- modelo$diff_tasa_pasiva$coefficients
modelo_diff_sal_prom <- modelo$diff_sal_prom$coefficients
modelo_diff_sbu <- modelo$diff_sbu$coefficients
modelo_diff_ipc <- modelo$diff_ipc$coefficients

test_significancia <- rbind( modelo_diff_pib,
                             modelo_diff_tasa_pasiva,
                             modelo_diff_sal_prom,
                             modelo_diff_sbu,
                             modelo_diff_ipc ) %>% 
  as_tibble( . ) %>% 
  clean_names() %>% 
  mutate( p_value = if_else( pr_t < 0.001,
                             "p < 0,001",
                             format( round( pr_t, 3),
                                     nsmall = 3,
                                     decimal.mark = ",", big.mark = ".") ) ) %>% 
  dplyr::select( -pr_t )


sigma_diff_pib <- format( round( modelo$diff_pib$sigma, 4 ),
                          nsmall = 3,
                          decimal.mark = ",", big.mark = "." ) 

df_diff_pib <- modelo$diff_pib$df

f_diff_pib <- format( round(  modelo$diff_pib$fstatistic[1], 4 ),
                      nsmall = 3,
                      decimal.mark = ",", big.mark = "." ) 

f_value_diff_pib <- pf( modelo$diff_pib$fstatistic[1], 
                        modelo$diff_pib$fstatistic[2], 
                        modelo$diff_pib$fstatistic[3], lower.tail = FALSE)


f_value_diff_pib <- if_else( f_value_diff_pib < 0.001,
                             " <\\,0,001",
                             format( round(  f_value_diff_pib, 4 ),
                                     nsmall = 3,
                                     decimal.mark = ",", big.mark = "." )  )


sigma_diff_tasa_pasiva <- format( round( modelo$diff_tasa_pasiva$sigma, 4 ),
                                  nsmall = 3,
                                  decimal.mark = ",", big.mark = "." ) 

df_diff_tasa_pasiva <- modelo$diff_tasa_pasiva$df

f_diff_tasa_pasiva <- format( round(  modelo$diff_tasa_pasiva$fstatistic[1], 4 ),
                              nsmall = 3,
                              decimal.mark = ",", big.mark = "." ) 

f_value_diff_tasa_pasiva <- pf( modelo$diff_tasa_pasiva$fstatistic[1], 
                                modelo$diff_tasa_pasiva$fstatistic[2], 
                                modelo$diff_tasa_pasiva$fstatistic[3], lower.tail = FALSE)


f_value_diff_tasa_pasiva <- if_else( f_value_diff_tasa_pasiva < 0.001,
                                     " <\\,0,001",
                                     format( round(  f_value_diff_tasa_pasiva, 4 ),
                                             nsmall = 3,
                                             decimal.mark = ",", big.mark = "." )  )

sigma_diff_sal_prom <- format( round( modelo$diff_sal_prom$sigma, 4 ),
                               nsmall = 3,
                               decimal.mark = ",", big.mark = "." ) 

df_diff_sal_prom <- modelo$diff_sal_prom$df

f_diff_sal_prom <- format( round(  modelo$diff_sal_prom$fstatistic[1], 4 ),
                           nsmall = 3,
                           decimal.mark = ",", big.mark = "." ) 

f_value_diff_sal_prom <- pf( modelo$diff_sal_prom$fstatistic[1], 
                             modelo$diff_sal_prom$fstatistic[2], 
                             modelo$diff_sal_prom$fstatistic[3], lower.tail = FALSE)

f_value_diff_sal_prom <- if_else( f_value_diff_sal_prom < 0.001,
                                  " <\\,0,001",
                                  format( round(  f_value_diff_sal_prom, 4 ),
                                          nsmall = 3,
                                          decimal.mark = ",", big.mark = "." )  )

sigma_diff_sbu <- format( round( modelo$diff_sbu$sigma, 4 ),
                          nsmall = 3,
                          decimal.mark = ",", big.mark = "." ) 

df_diff_sbu <- modelo$diff_sbu$df

f_diff_sbu <- format( round(  modelo$diff_sbu$fstatistic[1], 4 ),
                      nsmall = 3,
                      decimal.mark = ",", big.mark = "." ) 

f_value_diff_sbu <- pf( modelo$diff_sbu$fstatistic[1], 
                        modelo$diff_sbu$fstatistic[2], 
                        modelo$diff_sbu$fstatistic[3], lower.tail = FALSE)


f_value_diff_sbu <- if_else( f_value_diff_sbu < 0.001,
                             " <\\,0,001",
                             format( round(  f_value_diff_sbu, 4 ),
                                     nsmall = 3,
                                     decimal.mark = ",", big.mark = "." )  )


sigma_diff_ipc <- format( round( modelo$diff_ipc$sigma, 4 ),
                          nsmall = 3,
                          decimal.mark = ",", big.mark = "." ) 

df_diff_ipc <- modelo$diff_ipc$df

f_diff_ipc <- format( round(  modelo$diff_ipc$fstatistic[1], 4 ),
                      nsmall = 3,
                      decimal.mark = ",", big.mark = "." ) 

f_value_diff_ipc <- pf( modelo$diff_ipc$fstatistic[1], 
                        modelo$diff_ipc$fstatistic[2], 
                        modelo$diff_ipc$fstatistic[3], lower.tail = FALSE )


f_value_diff_ipc <- if_else( f_value_diff_ipc < 0.001,
                             " $<\\,0,001$",
                             format( round(  f_value_diff_ipc, 4 ),
                                     nsmall = 3,
                                     decimal.mark = ",", big.mark = "." )  )

aux <- data.frame( var = c( '$AR(1)\\;\\nabla Pib$',
                            '$AR(1)\\;\\nabla IPC$',
                            '$AR(2)\\;\\nabla Pib$',
                            '$AR(1)\\;\\nabla \\text{Tasa Pasiva}$',
                            '$AR(1)\\;\\nabla Salarios$',
                            '$Constante$',
                            '$AR(1)\\;\\nabla SBU$',
                            '$AR(2)\\;\\nabla SBU$',
                            '$AR(2)\\;\\nabla IPC$',
                            '$Constante$',
                            '$AR(1)\\;\\nabla IPC$',
                            '$Constante$' ), 
                   test_significancia )


xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 6, 0 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_modelo_macro_coef', '.tex' ),
       type = 'latex', 
       comment=FALSE,
       include.colnames = FALSE, 
       include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = c( 3, 4, 6, 10, 12 ),
       sanitize.text.function = identity,
       add.to.row = 
         list( pos = list( 3, 4, 6, 10, 12 ), #, 4, 6, 10, 12), # posición después de 3, 4, 6, 10, 12
               command = c( paste(" \\hline \n", "\\multicolumn{5}{l}{Error estándar de los residuos: ",sigma_diff_pib," sobre ", df_diff_pib[2], " grados de libertad} \\\\ \n", "\\multicolumn{5}{l}{Estadístico - $F$: ", f_diff_pib, " sobre ", df_diff_pib[1], " y ",df_diff_pib[2] , " grados de libertad, p-valor", f_value_diff_pib, "}", " \\\\ \\hline  \n \\multicolumn{5}{c}{\\textbf{Ecuación para $\\nabla \\text{Tasa pasiva}$ } } \\\\  \n"),
                            paste( "\\hline \n \\multicolumn{5}{l}{Error estándar de los residuos: ",sigma_diff_tasa_pasiva," sobre ", df_diff_tasa_pasiva[2], " grados de libertad} \\\\ \n", "\\multicolumn{5}{l}{Estadístico $F$: ", f_diff_tasa_pasiva, " sobre ", df_diff_tasa_pasiva[1], " y ",df_diff_tasa_pasiva[2] , " grados de libertad, p-valor", f_value_diff_tasa_pasiva, "}", "\\\\ \\hline \n \\multicolumn{5}{c}{\\textbf{Ecuación para $\\nabla \\text{Salario Promedio}$}} \\\\ \n"),
                            paste(" \\hline \n", "\\multicolumn{5}{l}{Error estándar de los residuos: ",sigma_diff_sal_prom," sobre ", df_diff_sal_prom[2], " grados de libertad} \\\\ \n", "\\multicolumn{5}{l}{Estadístico $F$: ", f_diff_sal_prom, " sobre ", df_diff_sal_prom[1], " y ",df_diff_sal_prom[2] , " grados de libertad, p-valor", f_value_diff_sal_prom, "}", "\\\\ \\hline \n \\multicolumn{5}{c}{\\textbf{Ecuación para $\\nabla SBU$}} \\\\ \n"),
                            paste(" \\hline \n", "\\multicolumn{5}{l}{Error estándar de los residuos: ",sigma_diff_sbu," sobre ", df_diff_sbu[2], " grados de libertad} \\\\ \n", "\\multicolumn{5}{l}{Estadístico $F$: ", f_diff_sbu, " sobre ", df_diff_sbu[1], " y ",df_diff_sbu[2] , " grados de libertad, p-valor", f_value_diff_sbu, "}", "\\\\ \\hline \n \\multicolumn{5}{c}{\\textbf{Ecuación para $\\nabla IPC$}} \\\\ \n"),
                            paste(" \\hline \n", "\\multicolumn{5}{l}{Error estándar de los residuos: ",sigma_diff_ipc," sobre ", df_diff_ipc[2], " grados de libertad} \\\\ \n", "\\multicolumn{5}{l}{Estadístico $F$: ", f_diff_ipc, " sobre ", df_diff_ipc[1], " y ",df_diff_ipc[2] , " grados de libertad, p-valor", f_value_diff_ipc, "} \\\\" )
               ) ) )


#Tabla de coeficientes del modelo de series de rendimientos del BIESS-------------------------------
aux <- coeficientes_biess %>% 
  mutate( var = c( '$AR(1)$',
                   '$AR(2)$' ) )

sigma_modelo <- format( round( se_mod_biess, 4 ),
                        nsmall = 3,
                        decimal.mark = ",", big.mark = "." ) 

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 6, 6 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_modelo_biess_coef', '.tex' ),
       type = 'latex', 
       comment=FALSE,
       include.colnames = FALSE, 
       include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = c( 2 ),
       sanitize.text.function = identity,
       add.to.row = 
         list( pos = list( 2 ),
               command = c( paste( " \\hline \n", "\\multicolumn{5}{l}{Error estándar de los residuos: ",
                                   sigma_modelo,
                                   " sobre 131 grados de libertad} \\\\ " ) ) ) )

#Borrando data frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )
