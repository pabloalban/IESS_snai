message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )

# REP$corte <- parametros$anio_ini
# 
# REP$cap_ini <- format( 
#   parametros$rtr_reserva_ini, 
#   digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# # Carga información --------------------------------------------------------------------------------
# load( file = parametros$demo_rdata_sgo_est_dem )
# 
# # Afiliados ----------------------------------------------------------------------------------------
# 
# REP$cre_prom_anual <- format( est_sal_anio[ , .(anio, ER_act, st = lag(ER_act) ) ]
#                               [ anio < 2021, var := mean( ( ER_act / st - 1 ) * 100, na.rm = T ) ]
#                               [ anio == REP$corte, var ], digits = 3, big.mark = '.',  
#                               decimal.mark = ',', format = 'f' )
# 
# REP$prom_anual_sgo <- format( 
#   mean( est_sal_anio$ER_act, na.rm = TRUE ), 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$afi_sgo <- format( 
#   est_sal_anio[ anio == parametros$anio_ini, ]$ER_act, 
#   nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$cre_afi_sgo <- format( 
#   100 * ( est_sal_anio[ anio == parametros$anio_ini ]$ER_act / 
#             est_sal_anio[ anio == 2012 ]$ER_act - 1 ), 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$afi_var <- format( 
#   100 * ( est_sal_anio[ anio == parametros$anio_ini ]$ER_act / 
#             est_sal_anio[ anio == 2019 ]$ER_act - 1 ),
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$por_sgo_h <- format( 
#   100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ER_act / 
#     sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_act ),
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$por_sgo_m <- format(
#   100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ER_act / 
#     sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_act ),
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# aux <- est_sal_anio_sexo_edad[ 
#   anio == REP$corte & sexo == 'H', 
#   list( x_mean = sum( x * ER_act, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ) ) ]
# REP$edad_prom_h <- format(
#   aux$x_mean,
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# aux <- est_sal_anio_sexo_edad[ 
#   anio == REP$corte & sexo == 'M', 
#   list( x_mean = sum( x * ER_act, na.rm = TRUE ) / sum( ER_act, na.rm = TRUE ) ) ]
# REP$edad_prom_m <- format( 
#   aux$x_mean,
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# # Afiliados TNRH -----------------------------------------------------------------------------------
# 
# REP$cre_prom_anua_tnrh <- format( est_sal_anio[ anio > 2015 , .(anio, ER_tnrh_act, st = lag(ER_tnrh_act) ) ]
#                                   [ anio < 2021, var := mean( ( ER_tnrh_act / st - 1 ) * 100, na.rm = T ) ]
#                                   [ anio == REP$corte, var ], digits = 3, big.mark = '.',  
#                                   decimal.mark = ',', format = 'f' )
# 
# REP$prom_anual_tnrh <- format(
#   mean( est_sal_anio$ER_tnrh_act, na.rm = TRUE ), 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$afi_tnrh <-format( 
#   est_sal_anio[ anio == parametros$anio_ini, ]$ER_tnrh_act, 
#   nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$cre_afi_tnrh <- format(
#   100 * ( est_sal_anio[ anio == parametros$anio_ini ]$ER_tnrh_act / 
#             est_sal_anio[ anio == 2015 ]$ER_tnrh_act - 1 ), 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$por_tnrh_2019 <- format( 
#   100 * ( est_sal_anio[ anio == 2020 ]$ER_tnrh_act / 
#             est_sal_anio[ anio == 2019 ]$ER_tnrh_act - 1 ),
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$por_tnrh_h <- format( 
#   100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ER_tnrh_act / 
#     sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_tnrh_act ),
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$por_tnrh_m <- format( 
#   100 * est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ER_tnrh_act / 
#     sum( est_sal_anio_sexo[ anio == REP$corte ]$ER_tnrh_act ),
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# aux <- est_sal_anio_sexo_edad[ 
#   anio == REP$corte & sexo == 'H', 
#   list( x_mean = sum( x * ER_tnrh_act, na.rm = TRUE ) / sum( ER_tnrh_act, na.rm = TRUE ) ) ]
# REP$edad_prom_tnrh_h <- format(
#   aux$x_mean,
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# aux <- est_sal_anio_sexo_edad[ 
#   anio == REP$corte & sexo == 'M', 
#   list( x_mean = sum( x * ER_tnrh_act, na.rm = TRUE ) / sum( ER_tnrh_act, na.rm = TRUE ) ) ]
# REP$edad_prom_tnrh_m <- format( 
#   aux$x_mean,
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# # Masa salarial ------------------------------------------------------------------------------------
# 
# REP$masa_salarial <- format(
#   est_sal_anio[ anio == REP$corte ]$S, 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$masa_salarial_var <- format(
#   100 * ( est_sal_anio[ anio == REP$corte ]$S / est_sal_anio[ anio == 2012 ]$S - 1 ), 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$masa_sal_prom_2012_2015 <- format( est_sal_anio[ anio < 2016 , .(anio, S, st = lag(S) ) ]
#                                        [ , var := mean( ( S / st - 1 ) * 100, na.rm = T ) ]
#                                        [ anio == 2015, var ], digits = 3, big.mark = '.',  
#                                        decimal.mark = ',', format = 'f' )
# 
# REP$masa_salarial_prom <- format( 
#   mean( est_sal_anio$S ),
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$masa_salarial_var_2020 <- format( 
#   100 * ( est_sal_anio[ anio == REP$corte ]$S / est_sal_anio[ anio == 2019 ]$S - 1 ), 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$sal_prom_h <- format( 
#   est_sal_anio_sexo[ anio == REP$corte & sexo == 'H' ]$ESm, 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$sal_prom_m <- format( 
#   est_sal_anio_sexo[ anio == REP$corte & sexo == 'M' ]$ESm, 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# REP$sal_prom <- format( 
#   est_sal_anio[ anio == REP$corte ]$ESm, 
#   nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
# 
# # Información por escenarios -----------------------------------------------------------------------
# escenarios <- paste0( 'escenario_', 1:3 )
# esc_nom <- paste0( 'esc_', 1:3 )
# 
# for ( i in 1:length( escenarios ) ) {
#   
#   escenario <- escenarios[ i ]
#   
#   message( '\tGenerando auto información para el ', escenario )
#   
#   load( file = paste0( parametros$rtr_rdata_icomp_balance, escenario, '.RData' ) )
#   load( file = paste0( parametros$rtr_rdata_icomp_conf_esc, escenario, '.RData' ) )
#   load( file = paste0( parametros$rtr_rdata_icomp_prima, escenario, '.RData' ) )
#   
#   expr <- expression({
#     
#     REP$bal_act_ESC <- format( 
#       balance_anual[ t == parametros$rtr_horizonte ]$V, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$bal_cap_ESC <- format( 
#       balance_anual[ t == parametros$rtr_horizonte ]$V_cap, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$duracion_ESC <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini - 1
#     
#     REP$cap_ini_ESC <- format( 
#       esc$V0, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$pri_med_niv_ESC <- format( 
#       100 * prima[ t == parametros$rtr_horizonte ]$pri_med_niv_apo_est_pen,
#       digits = 4, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$apo_est_ESC <- format( 
#       100 * mean( esc$apo_act$por_apo_est[-1] ),
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_act_ESC <- format( 
#       100 * mean( esc$apo_act$i_a[-1] ),
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_sal_ESC <- format( 
#       100 * mean( esc$apo_act$i_r[-1] ),
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_sbu_ESC <- format( 
#       100 * mean( esc$apo_act$i_sbu[-1] ),
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_pen_ESC <- format( 
#       100 * mean( esc$apo_act$i_p[-1] ),
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_apo_jub_ESC <- format( 
#       100 * esc$apo_act$por_apo_pen_incap[2],
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_por_gas_ESC <- format( 
#       100 * esc$apo_act$por_gast[2],
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_aporte_salud_ESC <- format( 
#       100 * esc$apo_act$apo_sal[2],
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_aporte_ESC_2020 <- format( 
#       100 * ( esc$apo_act$por_apo[1] + esc$apo_act$apo_sal[2] ),
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$tasa_aporte_ESC_2021 <- format( 
#       100 * ( esc$apo_act$por_apo[2] + esc$apo_act$apo_sal[2] ),
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$bal_sum_act_ESC <- format( 
#       balance_anual[ t == parametros$rtr_horizonte ]$Act_vap, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$bal_sum_pas_ESC <- format( 
#       balance_anual[ t == parametros$rtr_horizonte ]$Pas_vap, 
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$ing_jubilados_ESC <- format( 
#       balance_anual[ t == parametros$rtr_horizonte ]$A12_vap +
#         balance_anual[ t == parametros$rtr_horizonte ]$A15_vap +
#         balance_anual[ t == parametros$rtr_horizonte ]$A16_vap,
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#     REP$ing_apo_est_ESC <- format( 
#       balance_anual[ t == parametros$rtr_horizonte ]$A_est_vap,
#       digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
#     
#   })
#   expr <- gsub( '(ESC)', esc_nom[ i ], deparse( expr ) )
#   eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
#   
# }
