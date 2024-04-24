# Preparación de transiciones activos Cesantía Desempleo ----------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tPreparación de información de transiciones' )

# Descripción de campos ----------------------------------------------------------------------------
# M2x_ces = Conteo de registros, equivalente al número de activos, no incluye muertos.
# M3x_ces = Conteo de registros, equivalente al número de inactivos, no incluye muertos.
# l2x_ces = Exposición al riesgo de activos vivos.
# l3x_ces = Exposición al riesgo de inactivos vivos.
# ts = Tiempo de servicio.
# ERx_ces = ERx_act + ERx_ina, Exposición al total, suma de exposición al riesgo de activos e inactivos.
# ERx_act_ces = Exposición al riesgo de activos.
# ERx_ina_ces = Exposición al riesgo de inactivos.
# Nx_sal_ces = Numero de salidas.
# Nx_ing_ces = Número de ingresos.
# Nx_prim_ing_ces = Número de primeros ingresos.
# Nx_dec_ces = Número de decesos de activos e inactivos.
# Nx_dec_act_ces = Número de decesos de activos.
# Nx_dec_ina_ces = Número de decesos de inactivos.
# Nx_vej_ces = Número de salidas por vejez.
# Nx_inv__ces = Número de salidas por invalidez.
# Nx_dis_ces = Número de salidas por discapacidad.
# Nx_viu_ces = Número de salidas por viudedad.
# Nx_orf_ces = Número de salidas por orfandad.
# S_ces = Suma de salarios.
# S2_ces = Suma de los salarios al cuadrado, es el segundo momento.

# Carga de funciones
source( 
  paste0( parametros$work_dir, "R/demografia/003_carga_funciones.R" ),
  encoding = 'UTF-8',
  echo = FALSE
 )

load( parametros$demo_rdata_ces_des_tran )
#sgo_act_tran <- sgo_act_tran[c(1:20000),]
# Preparando transiciones de activos -------------------------------------------------------------
message( '\tPreparando transiciones de activos cotizantes a Cesantía y Desempleo' )

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
IMP <- sgo_act_tran$imp / 12

cl <-
  makeCluster( detectCores( all.tests = FALSE, logical = TRUE ) - 1 )
clusterEvalQ( cl, {
  suppressPackageStartupMessages( library( data.table ) )
  suppressPackageStartupMessages( library( lubridate ) )
} )
clusterExport( cl, "comp_expo_riesgo" )
clusterExport( cl, "count_trans_out" )
clusterExport( cl, "count_trans_ing" )
clusterExport( cl, "count_trans_prim_ing" )

#Expuestos al riesgo--------------------------------------------------------------------------------

sgo_act_tran_anio <- NULL
ER_act <- rep( 0, nrow ( sgo_act_tran ) )
for ( y in years ) {
   #y <- 2012
  
  cols <- paste0( 'M', y, '_', 1:12 )
  
  message( 
    '\tProcesando activos para el año ',
    y,
    ' para las columnas: ',
    paste0( cols, collapse = ', ' )
   )
  
  idcols <-
    c( 
      'sexo',
      'fec_nac',
      'fec_dec',
      'fec_vej',
      'fec_inv',
      'fec_dis',
      'fec_viu',
      'fec_orf',
      'imp',
      paste0( 'S', y )
     )
  
  sgo_comp_tran <- sgo_act_tran[, idcols, with = FALSE]
  sgo_comp_tran[, IN_dec := IN_dec]
  sgo_comp_tran[, anio := y]
  setnames( sgo_comp_tran, paste0( 'S', y ), 'S' )
  
  sgo_comp_tran <- cbind( sgo_comp_tran,
                         t( parApply( 
                           cl = cl,
                           X = sgo_act_tran[, cols, with = FALSE],
                           MARGIN = 1,
                           FUN = function( x )
                             c( 
                               'ER_act' = comp_expo_riesgo( x, 360 ),
                               'N_sal' = count_trans_out( x ),
                               'N_ing' = count_trans_ing( x ),
                               'N_prim_ing' = count_trans_prim_ing( x )
                              )
                          ) ) )
  
  # Cálculo de imposiciones por año
  IMP <- pmax( IMP - ER_act, 0 )
  ER_act <- sgo_comp_tran$ER_act
  sgo_comp_tran[, imp := IMP]
  
  sgo_comp_tran <- sgo_comp_tran[year( fec_nac ) <= anio]
  sgo_comp_tran[, fec_edad := update( 
    fec_nac,
    year = anio,
    hours = 23,
    minutes = 59,
    seconds = 59,
    tz = parametros$time_zone
   )]
  sgo_comp_tran[!is.na( fec_dec ) &
                  fec_dec < fec_nac, fec_dec := fec_nac]
  sgo_comp_tran[!is.na( fec_dec ) &
                  year( fec_dec ) == anio, fec_edad := fec_dec]
  sgo_comp_tran[, x := round( interval( fec_nac, fec_edad ) / dyears( 1 ), 0 )]
  sgo_comp_tran[, ER_act := pmin( ER_act, 1 )]
  sgo_comp_tran[, ER_ina := pmax( 1 - ER_act, 0 )]
  sgo_comp_tran[!is.na( fec_vej ), ER_ina := ifelse( year( fec_vej ) >= anio, 0, ER_ina )]
  sgo_comp_tran[!is.na( fec_inv ), ER_ina := ifelse( year( fec_inv ) >= anio, 0, ER_ina )]
  sgo_comp_tran[!is.na( fec_dec ), ER_ina := ifelse( year( fec_dec ) >= anio, 0, ER_ina )]
  sgo_comp_tran[!is.na( fec_vej ), ER_act := ifelse( year( fec_vej ) > anio, 0, ER_act )]
  sgo_comp_tran[!is.na( fec_inv ), ER_act := ifelse( year( fec_inv ) > anio, 0, ER_act )]
  sgo_comp_tran[!is.na( fec_dec ), ER_act := ifelse( year( fec_dec ) > anio, 0, ER_act )]
  sgo_comp_tran[imp == 0, ER_ina := 0]
  sgo_comp_tran[imp == 0, ER_act := 0]
  sgo_comp_tran[, ER := ER_act + ER_ina]
  
  # Transiciones de deceso
  sgo_comp_tran[, N_dec := 0]
  sgo_comp_tran[, N_dec_act := 0]
  sgo_comp_tran[, N_dec_ina := 0]
  sgo_comp_tran[!is.na( fec_dec ) &
                  year( fec_dec ) == anio, N_dec := 1]
  sgo_comp_tran[!is.na( fec_dec ) &
                  year( fec_dec ) == anio & IN_dec == 1, N_dec_act := 1]
  sgo_comp_tran[!is.na( fec_dec ) &
                  year( fec_dec ) == anio & IN_dec == 0, N_dec_ina := 1]
  
  # Transiciones a pensionista de vejez
  sgo_comp_tran[, N_vej := 0]
  sgo_comp_tran[!is.na( fec_vej ) &
                  year( fec_vej ) == anio, N_vej := 1]
  
  # Transiciones a pensionista de invalidez
  sgo_comp_tran[, N_inv := 0]
  sgo_comp_tran[!is.na( fec_inv ) &
                  year( fec_inv ) == anio, N_inv := 1]
  
  # Transiciones a pensionista de discapacidad
  sgo_comp_tran[, N_dis := 0]
  sgo_comp_tran[!is.na( fec_dis ) &
                  year( fec_dis ) == anio, N_dis := 1]
  
  # Transiciones a pensionista de viudez
  sgo_comp_tran[, N_viu := 0]
  sgo_comp_tran[!is.na( fec_viu ) &
                  year( fec_viu ) == anio, N_viu := 1]
  
  # Transiciones a pensionista de orfandad
  sgo_comp_tran[, N_orf := 0]
  sgo_comp_tran[!is.na( fec_orf ) &
                  year( fec_orf ) == anio, N_orf := 1]
  
  # Solo cuentan los que estuviero expuestos en ese año
  # sgo_comp_tran <- sgo_comp_tran[ imp - 0.7 * 12 * (  max(  years  ) - anio + 1  ) >= ER_act / 30 | ER_act > 0 ]
  
  # Guardando objetos de estimación por año, en caso se necesiten para otros cálculos
  # save( 
  #   sgo_comp_tran,
  #   file = paste0( parametros$demo_rdata_sgo_incom_tran_act_anio, y, '.RData' )
  #  )
  gc(  )
  
  # Agregación para determinar estructura por edad y tiempo de servicio
  sgo_comp_tran <- sgo_comp_tran[, list( 
    M2x_ces = sum( ifelse( ER_act > 0, 1 - N_dec_act, 0 ), na.rm = TRUE ),
    M3x_ces = sum( ifelse( ER_ina > 0, 1 - N_dec_ina, 0 ), na.rm = TRUE ),
    l2x_ces = sum( ER_act * ( 1 - N_dec_act ), na.rm = TRUE ),
    l3x_ces = sum( ER_ina * ( 1 - N_dec_ina ), na.rm = TRUE ),
    ts = sum( imp, na.rm = TRUE ),
    ERx_ces = sum( ER, na.rm = TRUE ),
    ERx_act_ces = sum( ER_act, na.rm = TRUE ),
    ERx_ina_ces = sum( ER_ina, na.rm = TRUE ),
    Nx_sal_ces = sum( N_sal, na.rm = TRUE ),
    Nx_ing_ces = sum( N_ing, na.rm = TRUE ),
    Nx_prim_ing_ces = sum( N_prim_ing, na.rm = TRUE ),
    Nx_dec_ces = sum( N_dec, na.rm = TRUE ),
    Nx_dec_act_ces = sum( N_dec_act, na.rm = TRUE ),
    Nx_dec_ina_ces = sum( N_dec_ina, na.rm = TRUE ),
    Nx_vej_ces = sum( N_vej, na.rm = TRUE ),
    Nx_inv_ces = sum( N_inv, na.rm = TRUE ),
    Nx_dis_ces = sum( N_dis, na.rm = TRUE ),
    Nx_viu_ces = sum( N_viu, na.rm = TRUE ),
    Nx_orf_ces = sum( N_orf, na.rm = TRUE ),
    S_ces = sum( S, na.rm = TRUE ),
    S2_ces = sum( S ^ 2, na.rm = TRUE )
   ),
  by = list( anio, sexo, x )]
  
  sgo_act_tran_anio <-
    rbindlist( list( sgo_act_tran_anio, sgo_comp_tran ) )
  sgo_act_tran[, ( cols ) := NULL]
  
  clusterEvalQ( cl, {
    gc(  )
  } )
  rm( sgo_comp_tran )
  gc(  )
}

stopCluster( cl )
rm( sgo_act_tran )
rm( cl )
gc(  )

#Afiliados a diciembre de cada año------------------------------------------------------------------
load( parametros$demo_rdata_ces_des_tran )
#sgo_act_tran <- sgo_act_tran[c(1:20000),]

# Preparando transiciones de activos -------------------------------------------------------------
message( '\tPreparando transiciones de activos cotizantes a Cesantía y Desempleo' )

years <- rev( parametros$demo_years_analisis )
idvars <- c( 'cdla', 'sexo', 'fec_nac', 'fec_dec', 'fec_inv', 'fec_vej', 'fec_dis', 'fec_viu', 'fec_orf', "imp", "S2020",
             'M2012_12', 'M2013_12', 'M2014_12', 'M2015_12', 'M2016_12' ,'M2017_12', 'M2018_12', 'M2019_12', 'M2020_12', 'M2021_12',  'M2022_12' )
mesvars <- names( sgo_act_tran )[ !( names( sgo_act_tran ) %in% idvars ) ]
sgo_act_tran <- sgo_act_tran[ , idvars, with = FALSE ]

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
#IN_dec <- indicador_transicion( years = years, field = 'fec_dec', data = 'sgo_act_tran' )

# Vector de imposiciones
IMP <- sgo_act_tran$imp / 12

cl <-
  makeCluster( detectCores( all.tests = FALSE, logical = TRUE ) - 1 )
clusterEvalQ( cl, {
  suppressPackageStartupMessages( library( data.table ) )
  suppressPackageStartupMessages( library( lubridate ) )
} )
clusterExport( cl, "comp_expo_riesgo" )
clusterExport( cl, "count_trans_out" )
clusterExport( cl, "count_trans_ing" )
clusterExport( cl, "count_trans_prim_ing" )


message( '\tCotizantes de cesantía a diciembre de 2020' )
des_ces_act_dic_2020 <- sgo_act_tran[ , c( "cdla", "sexo", "fec_nac", "imp", "S2020", "M2020_12" ) ]

message( '\tContando cotizantes de cesantía a diciembre de cada año' )
sgo_act_tran_dic <- NULL
ER_act <- rep( 0, nrow ( sgo_act_tran ) )
for ( y in years ) {
  #y <- 2012
  
  cols <- paste0( 'M', y, '_', 12 )
  
  message( 
    '\tProcesando activos para el año ',
    y,
    ' para las columnas: ',
    paste0( cols, collapse = ', ' )
  )
  
  idcols <-
    c( 
      'sexo',
      'fec_nac',
      'fec_dec',
      'fec_vej',
      'fec_inv',
      'fec_dis',
      'fec_viu',
      'fec_orf'
    )
  
  sgo_comp_tran <- sgo_act_tran[, idcols, with = FALSE]
  #sgo_comp_tran[, IN_dec := IN_dec]
  sgo_comp_tran[, anio := y]

  sgo_comp_tran <- cbind( sgo_comp_tran,
                          t( parApply( 
                            cl = cl,
                            X = sgo_act_tran[, cols, with = FALSE],
                            MARGIN = 1,
                            FUN = function( x )
                              c( 
                                'ER_act' = comp_expo_riesgo( x, 360 ),
                                'N_sal' = count_trans_out( x ),
                                'N_ing' = count_trans_ing( x ),
                                'N_prim_ing' = count_trans_prim_ing( x )
                              )
                          ) ) )
  # Cálculo de edad
  ER_act <- sgo_comp_tran$ER_act
  sgo_comp_tran <- sgo_comp_tran[year( fec_nac ) <= anio]
  sgo_comp_tran[, fec_edad := update( 
    fec_nac,
    year = anio,
    hours = 23,
    minutes = 59,
    seconds = 59,
    tz = parametros$time_zone
  )]
  sgo_comp_tran[!is.na( fec_dec ) &
                  fec_dec < fec_nac, fec_dec := fec_nac]
  sgo_comp_tran[!is.na( fec_dec ) &
                  year( fec_dec ) == anio, fec_edad := fec_dec]
  sgo_comp_tran[, x := round( interval( fec_nac, fec_edad ) / dyears( 1 ), 0 )]
  sgo_comp_tran[, ER_act := pmin( ER_act, 1 )]
  gc(  )
  
  sgo_comp_tran <- sgo_comp_tran[, list(
   M2x_ces_dic = sum( ifelse( ER_act > 0, 1, 0 ), na.rm = TRUE ) ),
  by = list( anio, sexo, x )]
  
  sgo_act_tran_dic <-
    rbindlist( list( sgo_act_tran_dic, sgo_comp_tran ) )
  sgo_act_tran[, ( cols ) := NULL]

  clusterEvalQ( cl, {
    gc(  )
  } )
  rm( sgo_comp_tran )
  gc(  )
}

stopCluster( cl )
rm( cl )
gc(  )

message( '\tPreparando estimaciones de transiciones para activos de Cesantía' )
# Cálculo de tasas
sgo_act_tran_anio[, ux_sal := ifelse( ERx_act_ces > 0, Nx_sal_ces / ERx_act_ces, 0 )]
sgo_act_tran_anio[, ux_ing := ifelse( ERx_ina_ces > 0, Nx_ing_ces / ERx_ina_ces, 0 )]
sgo_act_tran_anio[, ux_dec := ifelse( ERx_ces > 0, Nx_dec_ces / ERx_ces, 0 )]
sgo_act_tran_anio[, ux_dec_act := ifelse( ERx_act_ces > 0, Nx_dec_act_ces / ERx_act_ces, 0 )]
sgo_act_tran_anio[, ux_dec_ina := ifelse( ERx_ina_ces > 0, Nx_dec_ina_ces / ERx_ina_ces, 0 )]
sgo_act_tran_anio[, ux_vej := ifelse( ERx_ces > 0, Nx_vej_ces / ERx_ces, 0 )]
sgo_act_tran_anio[, ux_inv := ifelse( ERx_ces > 0, Nx_inv_ces / ERx_ces, 0 )]
sgo_act_tran_anio[, ux_dis := ifelse( ERx_ces > 0, Nx_dis_ces / ERx_ces, 0 )]
sgo_act_tran_anio[, ux_viu := ifelse( ERx_ces > 0, Nx_viu_ces / ERx_ces, 0 )]
sgo_act_tran_anio[, ux_orf := ifelse( ERx_ces > 0, Nx_orf_ces / ERx_ces, 0 )]

sgo_act_tran_anio <- left_join( sgo_act_tran_anio, sgo_act_tran_dic , by = c('anio', 'sexo', 'x' ) )

setorder( sgo_act_tran_anio, anio, sexo, x )
gc(  )

#Cambio nombre variable
des_ces_act_tran_anio = sgo_act_tran_anio


message( '\tGuardando resultados preparados' )

save( des_ces_act_tran_anio,
      des_ces_act_dic_2020,
     file = parametros$demo_rdata_ces_des_tran_prep )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )
