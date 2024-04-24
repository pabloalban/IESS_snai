# Preparación de transiciones activos SSC ----------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPreparación de información de transiciones' )

# Descripción de campos ----------------------------------------------------------------------------
# M2x = Conteo de registros, equivalente al número de activos, no incluye muertos.
# M3x = Conteo de registros, equivalente al número de inactivos, no incluye muertos.
# l2x = Exposición al riesgo de activos vivos.
# l3x = Exposición al riesgo de inactivos vivos.
# ts = Tiempo de servicio.
# ERx = ERx_act + ERx_ina, Exposición al total, suma de exposición al riesgo de activos e inactivos.
# ERx_act = Exposición al riesgo de activos.
# ERx_ina = Exposición al riesgo de inactivos.
# Nx_sal = Numero de salidas.
# Nx_ing = Número de ingresos.
# Nx_prim_ing = Número de primeros ingresos.
# Nx_dec = Número de decesos de activos e inactivos.
# Nx_dec_act = Número de decesos de activos.
# Nx_dec_ina = Número de decesos de inactivos.
# Nx_vej = Número de salidas por vejez.
# Nx_inv = Número de salidas por invalidez.
# Nx_dis = Número de salidas por discapacidad.
# Nx_viu = Número de salidas por viudedad.
# Nx_orf = Número de salidas por orfandad.
# S = Suma de salarios.
# S2 = Suma de los salarios al cuadrado, es el segundo momento.
# P = Suma de pensiones
# P2 = Suma de los pensiones al cuadrado, es el segundo momento.

if ( parametros$demo_prep_tran ) {
  # Carga de funciones
  source( paste0( parametros$work_dir, "R/demografia/003_carga_funciones.R" ), encoding = 'UTF-8', echo = FALSE )
  
  load( parametros$demo_rdata_ssc_tran )
  
  # Preparando transiciones de activos del SSC--------------------------------------------------------------
  message( '\tPreparando transiciones de activos del SSC' )
  
  years <- rev( parametros$demo_years_analisis )
  idvars <- c( 'cdla', 'sexo', 'fec_nac', 'fec_dec', 'fec_inv', 'fec_vej' )
  mesvars <- names( ssc_act_tran )[ !( names( ssc_act_tran ) %in% idvars ) ] #Todas las variables menos idvars
  
  ssc_act_tran <- ssc_act_tran[ , c( 'cdla', 'sexo', 'fec_nac', 'fec_dec', 
                                     'fec_inv', 'fec_vej',
                                     paste0( 'M', rep(seq(2012,2022,1), each = 12), "_", rep(seq(1,12,1), times = 2) ),
                                     'imp_2020', 
                                     'imp_2022', 
                                     paste0( 'S', seq(2012, 2022, 1) ) ,
                                     paste0( 'A', seq(2012, 2022, 1) )
                                      )]
  
  # Solo se toma en cuenta muertes en el periodo de observación
  #dim(ssc_act_tran)  705692    162
  ssc_act_tran <- ssc_act_tran[ is.na( fec_dec ) | year( fec_dec ) >= min( years ) ] #  705427    162
  
  # Solo se toma en cuenta personas con fecha nacimiento menor a la fecha máxima del periodo de
  # observación
  ssc_act_tran <- ssc_act_tran[ !is.na( fec_nac ) | year( fec_nac ) <= max( years ) ] # 705427    162
  
  # Filtros respecto a las fechas de transición
  ssc_act_tran <- ssc_act_tran[ is.na( fec_dec ) | ( !is.na( fec_dec ) & year( fec_dec ) - year( fec_nac ) >= 15 ) ] # 705427    162
  #Comprobaciones-----------------------
  # a <- ssc_act_tran
  # a[, jub:= (year( fec_vej  ) - year( fec_nac ))]
  # #a[ jub==29,]
  # a <- setorder( a[, .( NumDistintos = uniqueN(.SD[["cdla"]])), by = .(get("jub"))], get)
  # a
  # a[get>=29 & get<=64 , list(sum(NumDistintos, na.rm=T))]
  # a[!is.na(get), list(sum(NumDistintos, na.rm=T))]
  # # Existen jubilados con edades entre 29-64 años // 228 casos en total de 135222
  #--------------------------------------
  ssc_act_tran <- ssc_act_tran[ is.na( fec_vej ) | ( !is.na( fec_vej ) & year( fec_vej ) - year( fec_nac ) >= 65 ) ] # 705199    162
  
  # Preparando fechas
  ssc_act_tran[ , fec_nac := update( fec_nac, hours = 12, minutes = 0, seconds = 0, tz = parametros$time_zone ) ]
  ssc_act_tran[ , fec_dec := update( fec_dec, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
  
  # Indicador si se produjo el deceso cuando era activo o inactivo
  IN_dec <- indicador_transicion( years = years, field = 'fec_dec', data = 'ssc_act_tran' )
  
  # Vector de imposiciones
  IMP <- ssc_act_tran$imp_2022 / 12
  
  cl <- makeCluster( detectCores( all.tests = FALSE, logical = TRUE ) - 1 )
  clusterEvalQ(cl, {
    suppressPackageStartupMessages( library( data.table ) )
    suppressPackageStartupMessages( library( lubridate ) )
  })
  clusterExport( cl, "comp_expo_riesgo" )
  clusterExport( cl, "count_trans_out" )
  clusterExport( cl, "count_trans_ing" )
  clusterExport( cl, "count_trans_prim_ing" )
  
  ssc_act_tran_anio <- NULL
  ER_act <- rep( 0, nrow ( ssc_act_tran ) )
  for ( y in years ) { # y <- 2022
    
    cols <- paste0( 'M', y, '_', 1:12 )
    
    message( '\tProcesando activos para el año ', y, ' para las columnas: ', paste0( cols, collapse = ', ' ) )
    
    idcols <-  c( 'sexo', 'fec_nac', 'fec_dec', 'fec_vej', 'fec_inv', 'imp_2022', paste0( 'S', y ),
                  paste0( 'M', y, '_12' ) ) 
    
    ssc_comp_tran <- ssc_act_tran[ , idcols, with = FALSE ]
    ssc_comp_tran[ , IN_dec := IN_dec ]
    ssc_comp_tran[ , anio := y ]
    
    setnames( ssc_comp_tran, c( paste0( 'S', y ), paste0( 'M', y, '_12' ) ), c( 'S', 'm2' ) ) 
    
    ssc_comp_tran <- cbind( 
      ssc_comp_tran,
      t( parApply( cl = cl,
                   X = ssc_act_tran[ , cols, with = FALSE ],
                   MARGIN = 1,
                   FUN = function( x ) c( 'ER_act' = comp_expo_riesgo( x, 360 ),
                                          'N_sal' = count_trans_out( x ),
                                          'N_ing' = count_trans_ing( x ),
                                          'N_prim_ing' = count_trans_prim_ing( x ) ) ) )
    )
    
    # Cálculo de imposiciones por año
    IMP <- pmax( IMP - ER_act, 0 )
    ER_act <- ssc_comp_tran$ER_act
    ssc_comp_tran[ , imp := IMP ]
    
    ssc_comp_tran <- ssc_comp_tran[ year( fec_nac ) <= anio ] #705199     14
    ssc_comp_tran[ , fec_edad := update( fec_nac, year = anio, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
    ssc_comp_tran[ !is.na( fec_dec ) & fec_dec < fec_nac, fec_dec := fec_nac ]
    ssc_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, fec_edad := fec_dec ]
    ssc_comp_tran[ , x := round( interval( fec_nac, fec_edad ) / dyears( 1 ), 0 ) ]
    ssc_comp_tran[ , ER_act := pmin( ER_act, 1 ) ]
    ssc_comp_tran[ , ER_ina := pmax( 1 - ER_act, 0 ) ]
    ssc_comp_tran[ !is.na( fec_vej ), ER_ina := ifelse( year( fec_vej ) >= anio, 0, ER_ina ) ]
    ssc_comp_tran[ !is.na( fec_inv ), ER_ina := ifelse( year( fec_inv ) >= anio, 0, ER_ina ) ]
    ssc_comp_tran[ !is.na( fec_dec ), ER_ina := ifelse( year( fec_dec ) >= anio, 0, ER_ina ) ]
    ssc_comp_tran[ !is.na( fec_vej ), ER_act := ifelse( year( fec_vej ) > anio, 0, ER_act ) ]
    ssc_comp_tran[ !is.na( fec_inv ), ER_act := ifelse( year( fec_inv ) > anio, 0, ER_act ) ]
    ssc_comp_tran[ !is.na( fec_dec ), ER_act := ifelse( year( fec_dec ) > anio, 0, ER_act ) ]
    ssc_comp_tran[ imp == 0, ER_ina := 0 ]
    ssc_comp_tran[ imp == 0, ER_act := 0 ]
    ssc_comp_tran[ , ER := ER_act + ER_ina ]
  
    # Transiciones de deceso
    ssc_comp_tran[ , N_dec := 0 ]
    ssc_comp_tran[ , N_dec_act := 0 ]
    ssc_comp_tran[ , N_dec_ina := 0 ]
    ssc_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, N_dec := 1 ]
    ssc_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio & IN_dec == 1, N_dec_act := 1 ]
    ssc_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio & IN_dec == 0, N_dec_ina := 1 ]
    
    # Transiciones a pensionista de vejez
    ssc_comp_tran[ , N_vej := 0 ]
    ssc_comp_tran[ !is.na( fec_vej ) & year( fec_vej ) == anio, N_vej := 1 ]
    
    # Transiciones a pensionista de invalidez
    ssc_comp_tran[ , N_inv := 0 ]
    ssc_comp_tran[ !is.na( fec_inv ) & year( fec_inv ) == anio, N_inv := 1 ]
    
    ssc_comp_tran[ , m2 := pmin( m2 / 30.7, 1 ) ]
    ssc_comp_tran[ , m3 := 1 - m2  ]
   
     # Solo cuentan los que estuviero expuestos en ese año
    # ssc_comp_tran <- ssc_comp_tran[ imp - 0.7 * 12 * ( max( years ) - anio + 1 ) >= ER_act / 30 | ER_act > 0 ]
    
    # Guardando objetos de estimación por año, en caso se necesiten para otros cálculos
    save( ssc_comp_tran, file = paste0( parametros$demo_rdata_ssc_incom_tran_act_anio, y, '.RData' ) )
    gc()
    
    # Agregación para determinar estructura por edad y tiempo de servicio
    ssc_comp_tran <- ssc_comp_tran[ , list( 
      M2x = sum( ifelse( ER_act > 0, 1 - N_dec_act, 0 ), na.rm = TRUE ),
      M3x = sum( ifelse( ER_ina > 0, 1 - N_dec_ina, 0 ), na.rm = TRUE ),
      l2x = sum( ER_act * ( 1 - N_dec_act ), na.rm = TRUE ),
      l3x = sum( ER_ina * ( 1 - N_dec_ina ), na.rm = TRUE ),
      m2x = sum( m2, na.rm = TRUE ),
      m3x = sum( m3, na.rm = TRUE ),
      ts = sum( imp, na.rm = TRUE ),
      ERx = sum( ER, na.rm = TRUE ), 
      ERx_act = sum( ER_act, na.rm = TRUE ), 
      ERx_ina = sum( ER_ina, na.rm = TRUE ), 
      Nx_sal = sum( N_sal, na.rm = TRUE ),
      Nx_ing = sum( N_ing, na.rm = TRUE ),
      Nx_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
      Nx_dec = sum( N_dec, na.rm = TRUE ), 
      Nx_dec_act = sum( N_dec_act, na.rm = TRUE ), 
      Nx_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
      Nx_vej = sum( N_vej, na.rm = TRUE ),
      Nx_inv = sum( N_inv, na.rm = TRUE ),
      S = sum( S, na.rm = TRUE ),
      S2 = sum( S^2, na.rm = TRUE ) ), 
      by = list( anio, sexo, x ) ]
    
    ssc_act_tran_anio <- rbindlist( list( ssc_act_tran_anio, ssc_comp_tran ) )
    ssc_act_tran[ , ( cols ) := NULL ]
    
    clusterEvalQ( cl, {
      gc()
    })
    rm( ssc_comp_tran )
    gc()
  }
  stopCluster( cl )
  rm( cl )
  gc()
  
  message( '\tPreparando estimaciones de transiciones para activos del SSC' )
  # Cálculo de tasas
  ssc_act_tran_anio[ , ux_sal := ifelse( ERx_act > 0, Nx_sal / ERx_act, 0 ) ]
  ssc_act_tran_anio[ , ux_ing := ifelse( ERx_ina > 0, Nx_ing / ERx_ina, 0 ) ]
  ssc_act_tran_anio[ , ux_dec := ifelse( ERx > 0, Nx_dec / ERx, 0 ) ]
  ssc_act_tran_anio[ , ux_dec_act := ifelse( ERx_act > 0, Nx_dec_act / ERx_act, 0 ) ]
  ssc_act_tran_anio[ , ux_dec_ina := ifelse( ERx_ina > 0, Nx_dec_ina / ERx_ina, 0 ) ]
  ssc_act_tran_anio[ , ux_vej := ifelse( ERx > 0, Nx_vej / ERx, 0 ) ]
  ssc_act_tran_anio[ , ux_inv := ifelse( ERx > 0, Nx_inv / ERx, 0 ) ]
  
  setorder( ssc_act_tran_anio, anio, sexo, x )
  gc()
  
  # Preparando mortalidad de pensionistas SSC para todos los grupos --------------------------------
  message( '\tPreparando transiciones de pensionistas del SSC' )
  
  years <- rev( parametros$demo_years_analisis )
  idvars <- c( 'cdla', 'tipo', 'sex', 'fec_nac', 'fec_dec' )
  mesvars <- names( ssc_pen_tran )[ !( names( ssc_pen_tran ) %in% idvars ) ]
  
  # Preparando fechas
  ssc_pen_tran[ , fec_nac := update( fec_nac, hours = 12, minutes = 0, seconds = 0, tz = parametros$time_zone ) ]
  ssc_pen_tran[ , fec_dec := update( fec_dec, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
  ssc_pen_tran[ , imp := imp / 12 ]
  
  cl <- makeCluster( detectCores( all.tests = FALSE, logical = TRUE ) - 1 )
  clusterEvalQ(cl, {
    suppressPackageStartupMessages( library( data.table ) )
    suppressPackageStartupMessages( library( lubridate ) )
  })
  clusterExport( cl, "comp_expo_riesgo" )
  clusterExport( cl, "count_trans_out" )
  clusterExport( cl, "count_trans_ing" )
  clusterExport( cl, "count_trans_pen_ing_sal" )
  clusterExport( cl, "count_trans_prim_ing" )
  clusterExport( cl, "count_pen_ing_med_sal" )
  clusterExport( cl, "count_pen_mes" )
  
  ssc_pen_tran_anio <- NULL
  for ( y in years ) { # y <- 2015
    
    cols <- mesvars[ grepl( y, mesvars ) ]
    cols <- cols[ !grepl( '^I', cols ) ]
    message( '\tProcesando pensionistas para el año ', y, ' para las columnas: ', paste0( cols, collapse = ', ' ) )
    
    idcols <- c( 'tipo', 'cdla', 'sexo', 'fec_nac', 'fec_dec', 'imp', paste0( 'I', y ),paste0( 'M', y, '_12' ) )
    ssc_comp_tran <- ssc_pen_tran[ , idcols, with = FALSE ]
    ssc_comp_tran[ , anio := y ]
    setnames( ssc_comp_tran, c( paste0( 'I', y ), paste0( 'M', y, '_12' ) ), c( 'P', 'm' ) )
    
    ssc_comp_tran <- cbind( 
      ssc_comp_tran,
      t( parApply( cl = cl,
                   X = ssc_pen_tran[ , cols, with = FALSE ],
                   MARGIN = 1,
                   FUN = function( x ) c( 'ER_pen' = comp_expo_riesgo( x, 12 ),
                                          'N_sal' = count_trans_out( x ),
                                          'N_ing' = count_trans_ing( x ),
                                          'N_ing_sal' = count_trans_pen_ing_sal( x ),
                                          'N_prim_ing' = count_trans_prim_ing( x ),
                                          'ER_prim' = count_pen_ing_med_sal( x ),
                                          'mes_prim_pag' = count_pen_mes( x )
                                          ) ) )
    )
    
    #Comprobaciones pensionitas --------------------------------------------------------------------
    ssc_comp_tran[ tipo=='VEJEZ' & is.na( fec_dec ) & N_ing_sal > 0 & N_sal >= 1, ER_pen := 1] #2012/24800
    ssc_comp_tran[ tipo=='VEJEZ' & year( fec_dec ) > anio & N_ing_sal > 0 & N_sal >= 1, ER_pen := 1 ] #2012/18486 
    ssc_comp_tran[ tipo=='VEJEZ' & year( fec_dec ) == anio & N_ing_sal > 0 &  N_sal >= 1, ER_pen := month(fec_dec)/12] #2012/329
    ssc_comp_tran[ tipo=='VEJEZ' & year(fec_dec) > anio & N_prim_ing == 1 & N_sal >= 1, ER_pen :=  ER_prim ] #2012/825
    ssc_comp_tran[ tipo=='VEJEZ' & year(fec_dec) == anio & N_prim_ing == 1 & N_sal >= 1, ER_pen :=  ( month(fec_dec) - mes_prim_pag + 1 )/12] #2012/46
    ssc_comp_tran[ tipo=='VEJEZ' & year(fec_dec) == anio & N_prim_ing == 1 & N_sal >= 1 & ( month(fec_dec) < mes_prim_pag ), ER_pen := 0 ] #2012/3
    
    ssc_comp_tran[ tipo=='INVALIDEZ' & is.na( fec_dec ) & N_ing_sal > 0 & N_sal >= 1, ER_pen := 1] #2012/162
    ssc_comp_tran[ tipo=='INVALIDEZ' & year( fec_dec ) > anio & N_ing_sal > 0 & N_sal >= 1, ER_pen := 1 ] #2012/123
    ssc_comp_tran[ tipo=='INVALIDEZ' & year( fec_dec ) == anio & N_ing_sal > 0 &  N_sal >= 1, ER_pen := month(fec_dec)/12] #2012/1
    ssc_comp_tran[ tipo=='INVALIDEZ' & year(fec_dec) > anio & N_prim_ing == 1 & N_sal >= 1, ER_pen :=  ER_prim ] #2012/19
    ssc_comp_tran[ tipo=='INVALIDEZ' & year(fec_dec) == anio & N_prim_ing == 1 & N_sal >= 1, ER_pen :=  ( month(fec_dec) - mes_prim_pag + 1 )/12] #2012/0
    ssc_comp_tran[ tipo=='INVALIDEZ' & year(fec_dec) == anio & N_prim_ing == 1 & N_sal >= 1 & ( month(fec_dec) < mes_prim_pag ), ER_pen := 0 ] #2012/0
    
    # message( '\tPreparando estimaciones de mortalidad para pensionistas del SSC' )
    ssc_comp_tran <- ssc_comp_tran[ year( fec_nac ) <= anio ]
    ssc_comp_tran[ , fec_edad := update( fec_nac, year = anio, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
    ssc_comp_tran[ !is.na( fec_dec ) & fec_dec < fec_nac, fec_dec := fec_nac ]
    ssc_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, fec_edad := fec_dec ]
    ssc_comp_tran[ , x := round( interval( fec_nac, fec_edad ) / dyears( 1 ), 0 ) ]
    # ssc_comp_tran[ , fec_ini := ymd_hms( paste0( anio, '-01', '-01', ' 0:0:0' ), tz = parametros$time_zone ) ]
    # ssc_comp_tran[ , fec_fin := ymd_hms( paste0( anio, '-12', '-31', ' 23:59:59' ), tz = parametros$time_zone ) ]
    # ssc_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, fec_fin := fec_dec ]
    # ssc_comp_tran[ , ER := interval( fec_ini, fec_fin ) / dyears( 1 ) ]
    ssc_comp_tran[ , N := 0 ]
    ssc_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, N := 1 ]
    
    ssc_comp_tran[ , m := pmin( m, 1 ) ]
    
    # Guardando objetos de estimación por año, en caso se necesiten para otros cálculos
    save( ssc_comp_tran, file = paste0( parametros$demo_rdata_ssc_incom_tran_pen_anio, y, '.RData' ) )
    gc()
    
    ssc_comp_tran <- ssc_comp_tran[ , list(
      Mx = sum( ifelse( N_ing - N_sal >= 0 & ER_pen > 0, 1, 0 ) * ( 1 - N ), na.rm = TRUE ),
      lx = sum( ifelse( N_ing - N_sal >= 0 & ER_pen > 0, ER_pen, 0 ) * ( 1 - N ), na.rm = TRUE ),
      mx = sum( m, na.rm = TRUE ),
      ts = sum( imp, na.rm = TRUE ),
      ERx = sum( ER_pen, na.rm = TRUE ), 
      Nx_sal = sum( N_sal, na.rm = TRUE ),
      Nx_ing = sum( N_ing, na.rm = TRUE ),
      Nx = sum( N, na.rm = TRUE ), 
      P = sum( P, na.rm = TRUE ),
      P2 = sum( P^2, na.rm = TRUE ) ), 
      by = list( anio, tipo, sexo, x ) ]
    
    ssc_pen_tran_anio <- rbindlist( list( ssc_pen_tran_anio, ssc_comp_tran ) )
    ssc_pen_tran[ , ( cols ) := NULL ]
    clusterEvalQ(cl, {
      gc()
    })
    rm( ssc_comp_tran )
  }
  stopCluster( cl )
  rm( cl )
  gc()
  
  ssc_pen_tran_anio[ , ux := ifelse( ERx > 0, Nx / ERx, 0 ) ]
  setorder( ssc_pen_tran_anio, anio, tipo, sexo, x )
  gc()
  
  message( '\tGuardando resultados preparados' )
  save( ssc_act_tran_anio,
        ssc_pen_tran_anio, 
        file = parametros$demo_rdata_ssc_tran_prep )
  
}
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()

