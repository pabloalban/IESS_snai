# Configuraciones particulares ---------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tConfiguraciones particulares para el seguro SGRT' )

parametros$rtr_horizonte <- 40 # en años  
parametros$rtr_fec_fin <- ymd( '2022-03-04', tz = parametros$time_zone )
parametros$rtr_fec_ini <- ymd( '2012-01-01', tz = parametros$time_zone ) # fecha inicio del periodo de observación
parametros$rtr_reserva_ini <- 1202240160.65 # reserva inicial registrada en el patrimonio del balance
parametros$rtr_anio_max <- year( parametros$rtr_fec_fin )
parametros$rtr_anio_min <- year( parametros$rtr_fec_ini )

# Reporte-------------------------------------------------------------------------------------------
parametros$rtr_rep_gen <- paste0( parametros$work_dir, 'R/rtr/600_reporte_latex_rtr.R' )

# Objetos de salida, usualmente .RData ------------------------------------------------------------
parametros$demo_rdata_rtr_tran_prep <- paste0( parametros$RData_dem, 'IESS_RTR_transiciones_activos_pensionistas_preparado_2012_2022.RData' )
parametros$demo_rdata_rtr_tasas_tran <- paste0( parametros$RData_dem,'IESS_RTR_tasas_transicion_preparadas_2012_2022.RData' )
parametros$demo_rdata_rtr_pob_proy <- paste0( parametros$RData_dem, 'IESS_RTR_proyeccion_poblacion_', parametros$anio_ini, '.RData' )
parametros$rtr_rdata_icomp_balance <- paste0( parametros$RData_seg, 'IESS_RTR_balances_', parametros$anio_ini, '_' )
parametros$rtr_rdata_icomp_proy_benef <- paste0( parametros$RData_seg, 'IESS_RTR_proyeccion_beneficios_', parametros$anio_ini, '_' )
parametros$rtr_rdata_icomp_balance <- paste0( parametros$RData_seg, 'IESS_RTR_balances_', parametros$anio_ini, '_' )
parametros$rtr_rdata_icomp_proy_sal <- paste0( parametros$RData_seg, 'IESS_RTR_proyeccion_salarios_', parametros$anio_ini, '_' )
parametros$rtr_rdata_icomp_conf_esc <- paste0( parametros$RData_seg, 'IESS_RTR_configuracion_2020_' )
parametros$rtr_rdata_icomp_sal_proy <- paste0( parametros$RData_seg, 'IESS_RTR_proyeccion_salarios_', parametros$anio_ini, '_' )
parametros$rtr_rdata_icomp_prima <- paste0( parametros$RData_seg, 'IESS_RTR_primas_', parametros$anio_ini, '_' )

# Variables globales que son sobreescritas por cada seguro -----------------------------------------
parametros$horizonte <- parametros$rtr_horizonte

message( paste( rep( '-', 100 ), collapse = '' ) )


