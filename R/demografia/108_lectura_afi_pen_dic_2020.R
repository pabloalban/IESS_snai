message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLeyendo población afiliada activa diciembre de cada año' )

file <- parametros$demo_data_afi_pen_dic

# Poblacion afiliada -------------------------------------------------------------------------------
message( '\tLeyendo distribucion población afiliada SGO del IESS' )
col_nom <- c( 'anio', 'afi_hom', 'afi_muj', 'total' )
col_tip <- c( 'text', rep( 'numeric', 3 ) )

pob_afi_sgo <- as.data.table( read_excel( file, sheet="num_afi_sgo", col_names = TRUE, 
                                          guess_max = 24000, col_types = col_tip ) )
setnames( pob_afi_sgo, col_nom )

# Poblacion afiliada TNRH --------------------------------------------------------------------------
message( '\tLeyendo distribucion población afiliada TNRH del IESS' )
col_nom <- c( 'anio', 'afi_hom', 'afi_muj', 'total' )
col_tip <- c( 'text', rep( 'numeric', 3 ) )

pob_afi_tnrh <- as.data.table( read_excel( file, sheet="num_afi_tnrh", col_names = TRUE, 
                                          guess_max = 24000, col_types = col_tip ) )
setnames( pob_afi_tnrh, col_nom )

# Pensionistas vejez -------------------------------------------------------------------------------
message( '\tLeyendo pensionistas de vejez del SGO' )
col_nom <- c( 'anio', 'pen_hom', 'pen_muj', 'total' )
col_tip <- c( 'text', rep( 'numeric', 3 ) )

pen_vej_sgo <- as.data.table( read_excel( file, sheet="pen_vejez", col_names = TRUE, 
                                           guess_max = 24000, col_types = col_tip ) )
setnames( pen_vej_sgo, col_nom )

# Pensionistas invalidez ---------------------------------------------------------------------------
message( '\tLeyendo pensionistas de invalidez del SGO' )
col_nom <- c( 'anio', 'pen_hom', 'pen_muj', 'total' )
col_tip <- c( 'text', rep( 'numeric', 3 ) )

pen_inv_sgo <- as.data.table( read_excel( file, sheet="pen_inv", col_names = TRUE, 
                                           guess_max = 24000, col_types = col_tip ) )
setnames( pen_inv_sgo, col_nom )

# Pensionistas discapacidad ------------------------------------------------------------------------
message( '\tLeyendo pensionistas de discapacidad del SGO' )
col_nom <- c( 'anio', 'pen_hom', 'pen_muj', 'total' )
col_tip <- c( 'text', rep( 'numeric', 3 ) )

pen_dis_sgo <- as.data.table( read_excel( file, sheet="pen_discap", col_names = TRUE, 
                                           guess_max = 24000, col_types = col_tip ) )
setnames( pen_dis_sgo, col_nom )

# Pensionistas viudedad ----------------------------------------------------------------------------
message( '\tLeyendo pensionistas de viudedad del SGO' )
col_nom <- c( 'anio', 'pen_hom', 'pen_muj', 'total' )
col_tip <- c( 'text', rep( 'numeric', 3 ) )

pen_viu_sgo <- as.data.table( read_excel( file, sheet="pen_viu", col_names = TRUE, 
                                           guess_max = 24000, col_types = col_tip ) )
setnames( pen_viu_sgo, col_nom )

# Pensionistas orfandad ----------------------------------------------------------------------------
message( '\tLeyendo pensionistas de orfandad del SGO' )
col_nom <- c( 'anio', 'pen_hom', 'pen_muj', 'total' )
col_tip <- c( 'text', rep( 'numeric', 3 ) )

pen_orf_sgo <- as.data.table( read_excel( file, sheet="pen_orf", col_names = TRUE, 
                                           guess_max = 24000, col_types = col_tip ) )
setnames( pen_orf_sgo, col_nom )

# Guardando resultados -----------------------------------------------------------------------------

save( pob_afi_sgo,
      pob_afi_tnrh,
      pen_vej_sgo,
      pen_inv_sgo,
      pen_dis_sgo,
      pen_viu_sgo,
      pen_orf_sgo,
      file = parametros$demo_rdata_afi_pen_dic_prep ) 

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()



