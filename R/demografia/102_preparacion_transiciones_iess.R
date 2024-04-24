# Preparación de transiciones activos SGO ----------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tPreparación de información de transiciones' )

# Descripción de campos ----------------------------------------------------------------------------
# M2x = Conteo de registros, equivalente al número de activos, no incluye muertos.
# M3x = Conteo de registros, equivalente al número de inactivos, no incluye muertos.
# l2x = Exposición al riesgo de activos no salidos.
# l3x = Exposición al riesgo de inactivos no salidos.
# m2x = Exposición al riesgo de activos no salidos al mes de diciembre.
# m3x = Exposición al riesgo de inactivos no salidos al mes de diciembre.
# l2x_tnrh = Exposición al riesgo de activos TNRH no salidos.
# l3x_tnrh = Exposición al riesgo de inactivos TNRH no salidos.
# l12 = Exposición al riesgo de activos no TNRH no salidos.
# l13 = Exposición al riesgo de activos TNRH no salidos = l2x_tnrh.
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
  
  load( parametros$demo_rdata_sgo_tran )
  
  # Preparando transiciones de activos -------------------------------------------------------------
  message( '\tPreparando transiciones de activos del SGO' )
  
  years <- rev( parametros$demo_years_analisis )
  idvars <- c( 'cdla', 'sexo', 'fec_nac', 'fec_dec', 'fec_inv', 'fec_vej', 'fec_dis', 'fec_viu', 'fec_orf' )
  mesvars <- names( sgo_act_tran )[ !( names( sgo_act_tran ) %in% idvars ) ]
  
  # Solo se toma en cuenta muertes en el periodo de observación
  sgo_act_tran <- sgo_act_tran[ is.na( fec_dec ) | ( !is.na( fec_dec ) & ( year( fec_dec ) >= min( years ) & year( fec_dec ) <= max( years ) ) ) ]
  
  # Solo se toma en cuenta personas con fecha nacimiento menor a la fecha máxima del periodo de
  # observación
  sgo_act_tran <- sgo_act_tran[ !is.na( fec_nac ) | ( !is.na( fec_nac ) & year( fec_nac ) <= max( years ) ) ]
  
  # Filtros respecto a las fechas de transición
  sgo_act_tran <- sgo_act_tran[ is.na( fec_dec ) | ( !is.na( fec_dec ) & year( fec_dec ) - year( fec_nac ) >= 15 ) ]
  sgo_act_tran <- sgo_act_tran[ is.na( fec_vej ) | ( !is.na( fec_vej ) & year( fec_vej ) - year( fec_nac ) >= 54 ) ]
  
  # Preparando fechas
  sgo_act_tran[ , fec_nac := update( fec_nac, hours = 12, minutes = 0, seconds = 0, tz = parametros$time_zone ) ]
  sgo_act_tran[ , fec_dec := update( fec_dec, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
  
  # Indicador si se produjo el deceso cuando era activo o inactivo
  IN_dec <- indicador_transicion( years = years, field = 'fec_dec', data = 'sgo_act_tran' )
  
  # Vector de imposiciones
  sgo_act_tran[ is.na( imp ), imp := 0 ]
  sgo_act_tran[ is.na( imp_tnrh ), imp_tnrh := 0 ]
  sgo_act_tran[ , imp := imp + imp_tnrh ]
  sgo_act_tran[ , imp := imp / 12 ]
  sgo_act_tran[ , imp_tnrh := imp_tnrh / 12 ]
  IMP <- sgo_act_tran$imp
  IMP_TNRH <- sgo_act_tran$imp_tnrh
  
  cl <- makeCluster( detectCores( all.tests = FALSE, logical = TRUE ) - 1 )
  clusterEvalQ(cl, {
    suppressPackageStartupMessages( library( data.table ) )
    suppressPackageStartupMessages( library( lubridate ) )
  })
  clusterExport( cl, "comp_expo_riesgo" )
  clusterExport( cl, "count_trans_out" )
  clusterExport( cl, "count_trans_ing" )
  clusterExport( cl, "count_trans_prim_ing" )
  
  sgo_act_tran_anio <- NULL
  ER_act <- rep( 0, nrow ( sgo_act_tran ) )
  for ( y in years ) { # y <- 2012
    
    cols <- paste0( 'M', y, '_', 1:12 )
    
    message( '\tProcesando activos para el año ', y, ' para las columnas: ', paste0( cols, collapse = ', ' ) )
    
    if ( y >= 2015 ) {
      idcols <-  c( 'sexo', 'fec_nac', 'fec_dec', 'fec_vej', 'fec_inv', 'fec_dis', 'fec_viu', 
                    'fec_orf', 'imp', 'imp_tnrh', paste0( 'S', y ), paste0( 'ST', y ), paste0( 'M', y, '_12' ) )  
    } else {
      idcols <-  c( 'sexo', 'fec_nac', 'fec_dec', 'fec_vej', 'fec_inv', 'fec_dis', 'fec_viu', 
                    'fec_orf', 'imp', 'imp_tnrh', paste0( 'S', y ), paste0( 'M', y, '_12' ) )  
    }
    
    sgo_comp_tran <- sgo_act_tran[ , idcols, with = FALSE ]
    sgo_comp_tran[ , IN_dec := IN_dec ]
    sgo_comp_tran[ , anio := y ]
    
    if ( y >= 2015 ) {
      setnames( sgo_comp_tran, 
                c( paste0( 'S', y ), paste0( 'ST', y ), paste0( 'M', y, '_12' ) ), 
                c( 'S', 'ST', 'm2' ) )
    } else {
      setnames( sgo_comp_tran, 
                c( paste0( 'S', y ), paste0( 'M', y, '_12' ) ), 
                c( 'S', 'm2' ) )
      sgo_comp_tran[ , ST := 0 ]
    }
    
    sgo_comp_tran <- cbind( 
      sgo_comp_tran,
      t( parApply( cl = cl,
                   X = sgo_act_tran[ , cols, with = FALSE ],
                   MARGIN = 1,
                   FUN = function( x ) c( 'ER_act' = comp_expo_riesgo( x, 360 ),
                                          'N_sal' = count_trans_out( x ),
                                          'N_ing' = count_trans_ing( x ),
                                          'N_prim_ing' = count_trans_prim_ing( x ) ) ) )
    )
    
    # Cálculo de imposiciones por año
    IMP <- pmax( IMP - ER_act, 0 )
    IMP_TNRH <- pmax( IMP_TNRH - ER_act, 0 )
    ER_act <- sgo_comp_tran$ER_act
    sgo_comp_tran[ , imp := IMP ]
    sgo_comp_tran[ , imp_tnrh := IMP_TNRH ]
    
    sgo_comp_tran <- sgo_comp_tran[ year( fec_nac ) <= anio ]
    sgo_comp_tran[ , fec_edad := update( fec_nac, year = anio, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
    sgo_comp_tran[ !is.na( fec_dec ) & fec_dec < fec_nac, fec_dec := fec_nac ]
    sgo_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, fec_edad := fec_dec ]
    sgo_comp_tran[ , x := round( interval( fec_nac, fec_edad ) / dyears( 1 ), 0 ) ]
    sgo_comp_tran[ , ER_act := pmin( ER_act, 1 ) ]
    sgo_comp_tran[ , ER_ina := pmax( 1 - ER_act, 0 ) ]
    sgo_comp_tran[ !is.na( fec_vej ), ER_ina := ifelse( year( fec_vej ) >= anio, 0, ER_ina ) ]
    sgo_comp_tran[ !is.na( fec_inv ), ER_ina := ifelse( year( fec_inv ) >= anio, 0, ER_ina ) ]
    sgo_comp_tran[ !is.na( fec_dec ), ER_ina := ifelse( year( fec_dec ) >= anio, 0, ER_ina ) ]
    sgo_comp_tran[ !is.na( fec_vej ), ER_act := ifelse( year( fec_vej ) > anio, 0, ER_act ) ]
    sgo_comp_tran[ !is.na( fec_inv ), ER_act := ifelse( year( fec_inv ) > anio, 0, ER_act ) ]
    sgo_comp_tran[ !is.na( fec_dec ), ER_act := ifelse( year( fec_dec ) > anio, 0, ER_act ) ]
    sgo_comp_tran[ imp == 0, ER_ina := 0 ]
    sgo_comp_tran[ imp == 0, ER_act := 0 ]
    sgo_comp_tran[ , ER := ER_act + ER_ina ]
    
    # Exposición como TNRH
    sgo_comp_tran[ , ER_tnrh_act := 0 ]
    sgo_comp_tran[ imp_tnrh > 0 & ST > 0 & imp > 0, ER_tnrh_act := ER_act * pmin( imp_tnrh / imp, 1 ) ]
    sgo_comp_tran[ imp_tnrh > 0 & ST > 0 & imp <= 0, ER_tnrh_act := ER_act ]
    
    sgo_comp_tran[ , ER_tnrh_ina := 0 ]
    sgo_comp_tran[ imp_tnrh > 0 & ST > 0 & imp > 0, ER_tnrh_ina := ER_ina * pmin( imp_tnrh / imp, 1 ) ]
    sgo_comp_tran[ imp_tnrh > 0 & ST > 0 & imp <= 0, ER_tnrh_ina := ER_ina ]
    
    # Transiciones de deceso
    sgo_comp_tran[ , N_dec := 0 ]
    sgo_comp_tran[ , N_dec_act := 0 ]
    sgo_comp_tran[ , N_dec_ina := 0 ]
    sgo_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, N_dec := 1 ]
    sgo_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio & IN_dec == 1, N_dec_act := 1 ]
    sgo_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio & IN_dec == 0, N_dec_ina := 1 ]
    
    # Transiciones a pensionista de vejez
    sgo_comp_tran[ , N_vej := 0 ]
    sgo_comp_tran[ !is.na( fec_vej ) & year( fec_vej ) == anio, N_vej := 1 ]
    
    # Transiciones a pensionista de invalidez
    sgo_comp_tran[ , N_inv := 0 ]
    sgo_comp_tran[ !is.na( fec_inv ) & year( fec_inv ) == anio, N_inv := 1 ]
    
    # Transiciones a pensionista de discapacidad
    sgo_comp_tran[ , N_dis := 0 ]
    sgo_comp_tran[ !is.na( fec_dis ) & year( fec_dis ) == anio, N_dis := 1 ]
    
    # Transiciones a pensionista de viudez
    sgo_comp_tran[ , N_viu := 0 ]
    sgo_comp_tran[ !is.na( fec_viu ) & year( fec_viu ) == anio, N_viu := 1 ]
    
    # Transiciones a pensionista de orfandad
    sgo_comp_tran[ , N_orf := 0 ]
    sgo_comp_tran[ !is.na( fec_orf ) & year( fec_orf ) == anio, N_orf := 1 ]
    
    sgo_comp_tran[ , m2 := pmin( m2 / 30.7, 1 ) ]
    sgo_comp_tran[ , m3 := 1 - m2  ]
    
    # Guardando objetos de estimación por año, en caso se necesiten para otros cálculos
    save( sgo_comp_tran, file = paste0( parametros$demo_rdata_sgo_incom_tran_act_anio, y, '.RData' ) )
    gc()
    
    # Agregación para determinar estructura por edad y tiempo de servicio
    sgo_comp_tran <- sgo_comp_tran[ , list( 
      M2x = sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, 1, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      M3x = sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, 1, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      M2x_tnrh = sum( ifelse( N_ing - N_sal >= 0 & ER_tnrh_act > 0, 1, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      M3x_tnrh = sum( ifelse( N_sal - N_ing >= 0 & ER_tnrh_ina > 0, 1, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      l2x = sum( ifelse( N_ing - N_sal >= 0 & ER_act > 0, ER_act, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      l3x = sum( ifelse( N_sal - N_ing >= 0 & ER_ina > 0, ER_ina, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      m2x = sum( m2, na.rm = TRUE ),
      m3x = sum( m3, na.rm = TRUE ),
      l2x_tnrh = sum( ifelse( N_ing - N_sal >= 0 & ER_tnrh_act > 0, ER_tnrh_act, 0 ) * pmax( 1 - N_dec_act - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      l3x_tnrh = sum( ifelse( N_sal - N_ing >= 0 & ER_tnrh_ina > 0, ER_tnrh_ina, 0 ) * pmax( 1 - N_dec_ina - N_vej - N_inv - N_dis, 0 ), na.rm = TRUE ),
      m2x_tnrh = sum( as.numeric( ER_tnrh_act > 0 ) * m2, na.rm = TRUE ),
      m3x_tnrh = sum( as.numeric( ER_tnrh_ina > 0 ) * m3, na.rm = TRUE ),
      ts = sum( imp, na.rm = TRUE ),
      ts_tnrh = sum( imp_tnrh, na.rm = TRUE ),
      ERx = sum( ER, na.rm = TRUE ), 
      ERx_act = sum( ER_act, na.rm = TRUE ), 
      ERx_ina = sum( ER_ina, na.rm = TRUE ), 
      ERx_tnrh_act = sum( ER_tnrh_act, na.rm = TRUE ),
      ERx_tnrh_ina = sum( ER_tnrh_ina, na.rm = TRUE ),
      Nx_sal = sum( N_sal, na.rm = TRUE ),
      Nx_ing = sum( N_ing, na.rm = TRUE ),
      Nx_prim_ing = sum( N_prim_ing, na.rm = TRUE ),
      Nx_dec = sum( N_dec, na.rm = TRUE ), 
      Nx_dec_act = sum( N_dec_act, na.rm = TRUE ), 
      Nx_dec_ina = sum( N_dec_ina, na.rm = TRUE ),
      Nx_vej = sum( N_vej, na.rm = TRUE ),
      Nx_inv = sum( N_inv, na.rm = TRUE ),
      Nx_dis = sum( N_dis, na.rm = TRUE ),
      Nx_viu = sum( N_viu, na.rm = TRUE ),
      Nx_orf = sum( N_orf, na.rm = TRUE ),
      S = sum( S, na.rm = TRUE ),
      ST = sum( ST, na.rm = TRUE ),
      S2 = sum( S^2, na.rm = TRUE ),
      ST2 = sum( ST^2, na.rm = TRUE ) ), 
      by = list( anio, sexo, x ) ]
    
    sgo_act_tran_anio <- rbindlist( list( sgo_act_tran_anio, sgo_comp_tran ) )
    sgo_act_tran[ , ( cols ) := NULL ]
    
    clusterEvalQ( cl, {
      gc()
    })
    rm( sgo_comp_tran )
    gc()
  }
  stopCluster( cl )
  rm( cl )
  gc()
  
  message( '\tPreparando estimaciones de transiciones para activos del SGO' )
  # Cálculo de tasas
  sgo_act_tran_anio[ , ux_sal := ifelse( ERx_act > 0, Nx_sal / ERx_act, 0 ) ]
  sgo_act_tran_anio[ , ux_ing := ifelse( ERx_ina > 0, Nx_ing / ERx_ina, 0 ) ]
  sgo_act_tran_anio[ , ux_dec := ifelse( ERx > 0, Nx_dec / ERx, 0 ) ]
  sgo_act_tran_anio[ , ux_dec_act := ifelse( ERx_act > 0, Nx_dec_act / ERx_act, 0 ) ]
  sgo_act_tran_anio[ , ux_dec_ina := ifelse( ERx_ina > 0, Nx_dec_ina / ERx_ina, 0 ) ]
  sgo_act_tran_anio[ , ux_vej := ifelse( ERx > 0, Nx_vej / ERx, 0 ) ]
  sgo_act_tran_anio[ , ux_inv := ifelse( ERx > 0, Nx_inv / ERx, 0 ) ]
  sgo_act_tran_anio[ , ux_dis := ifelse( ERx > 0, Nx_dis / ERx, 0 ) ]
  sgo_act_tran_anio[ , ux_viu := ifelse( ERx > 0, Nx_viu / ERx, 0 ) ]
  sgo_act_tran_anio[ , ux_orf := ifelse( ERx > 0, Nx_orf / ERx, 0 ) ]
  
  setorder( sgo_act_tran_anio, anio, sexo, x )
  gc()
  
  # Preparando mortalidad de pensionistas SGO para todos los grupos --------------------------------
  message( '\tPreparando transiciones de pensionistas del SGO' )
  
  years <- rev( parametros$demo_years_analisis )
  idvars <- c( 'cdla', 'tipo', 'sex', 'fec_nac', 'fec_dec' )
  mesvars <- names( sgo_pen_tran )[ !( names( sgo_pen_tran ) %in% idvars ) ]
  
  # Preparando fechas
  sgo_pen_tran[ , fec_nac := update( fec_nac, hours = 12, minutes = 0, seconds = 0, tz = parametros$time_zone ) ]
  sgo_pen_tran[ , fec_dec := update( fec_dec, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
  sgo_pen_tran[ , imp := imp / 12 ]
  
  cl <- makeCluster( detectCores( all.tests = FALSE, logical = TRUE ) - 1 )
  clusterEvalQ(cl, {
    suppressPackageStartupMessages( library( data.table ) )
    suppressPackageStartupMessages( library( lubridate ) )
  })
  clusterExport( cl, "comp_expo_riesgo" )
  clusterExport( cl, "count_trans_out" )
  clusterExport( cl, "count_trans_ing" )
  
  sgo_pen_tran_anio <- NULL
  for ( y in years ) { # y <- 2012
    
    cols <- mesvars[ grepl( y, mesvars ) ]
    cols <- cols[ !grepl( '^I', cols ) ]
    message( '\tProcesando pensionistas para el año ', y, ' para las columnas: ', paste0( cols, collapse = ', ' ) )
    
    idcols <- c( 'tipo', 'cdla', 'sexo', 'fec_nac', 'fec_dec', 'imp', paste0( 'I', y ), paste0( 'M', y, '_12' ) )
    sgo_comp_tran <- sgo_pen_tran[ , idcols, with = FALSE ]
    sgo_comp_tran[ , anio := y ]
    setnames( sgo_comp_tran, c( paste0( 'I', y ), paste0( 'M', y, '_12' ) ), c( 'P', 'm' ) )
    
    sgo_comp_tran <- cbind( 
      sgo_comp_tran,
      t( parApply( cl = cl,
                   X = sgo_pen_tran[ , cols, with = FALSE ],
                   MARGIN = 1,
                   FUN = function( x ) c( 'ER_pen' = comp_expo_riesgo( x, 12 ),
                                          'N_sal' = count_trans_out( x ),
                                          'N_ing' = count_trans_ing( x ) ) ) )
    )
    
    # message( '\tPreparando estimaciones de mortalidad para pensionistas del SGO' )
    sgo_comp_tran <- sgo_comp_tran[ year( fec_nac ) <= anio ]
    sgo_comp_tran[ , fec_edad := update( fec_nac, year = anio, hours = 23, minutes = 59, seconds = 59, tz = parametros$time_zone ) ]
    sgo_comp_tran[ !is.na( fec_dec ) & fec_dec < fec_nac, fec_dec := fec_nac ]
    sgo_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, fec_edad := fec_dec ]
    sgo_comp_tran[ , x := round( interval( fec_nac, fec_edad ) / dyears( 1 ), 0 ) ]
    # sgo_comp_tran[ , fec_ini := ymd_hms( paste0( anio, '-01', '-01', ' 0:0:0' ), tz = parametros$time_zone ) ]
    # sgo_comp_tran[ , fec_fin := ymd_hms( paste0( anio, '-12', '-31', ' 23:59:59' ), tz = parametros$time_zone ) ]
    # sgo_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, fec_fin := fec_dec ]
    # sgo_comp_tran[ , ER := interval( fec_ini, fec_fin ) / dyears( 1 ) ]
    sgo_comp_tran[ , N := 0 ]
    sgo_comp_tran[ !is.na( fec_dec ) & year( fec_dec ) == anio, N := 1 ]
    
    sgo_comp_tran[ , m := pmin( m, 1 ) ]

    # Guardando objetos de estimación por año, en caso se necesiten para otros cálculos
    save( sgo_comp_tran, file = paste0( parametros$demo_rdata_sgo_incom_tran_pen_anio, y, '.RData' ) )
    gc()
    
    sgo_comp_tran <- sgo_comp_tran[ , list(
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
    
    sgo_pen_tran_anio <- rbindlist( list( sgo_pen_tran_anio, sgo_comp_tran ) )
    sgo_pen_tran[ , ( cols ) := NULL ]
    clusterEvalQ( cl, {
      gc()
    })
    rm( sgo_comp_tran )
  }
  stopCluster( cl )
  rm( cl )
  gc()
  
  sgo_pen_tran_anio[ , ux := ifelse( ERx > 0, Nx / ERx, 0 ) ]
  setorder( sgo_pen_tran_anio, anio, tipo, sexo, x )
  gc()
  
  message( '\tGuardando resultados preparados' )
  save( sgo_act_tran_anio,
        sgo_pen_tran_anio, 
        file = parametros$demo_rdata_sgo_tran_prep )
  
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()
