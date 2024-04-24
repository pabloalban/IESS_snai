# Inicialización -----------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCarga de información de transiciones' )

if ( parametros$demo_lect_tran & parametros$seguro == 'IVM' ) {
   
   # Lectura transiciones de activos SGO -----------------------------------------------------------   
   message( '\tLeyendo transiciones de activos del SGO' )
   colclass <- c( rep( 'character', 4 ), rep( 'numeric', 2 ), rep( 'character', 5 ),
                  rep( 'numeric', 151 ) )
   sgo_act_tran <- fread( file = parametros$demo_data_sgo_act_tran, 
                          sep = 'auto',
                          key = 'CEDULA_COD',
                          colClasses = colclass,
                          nrows = parametros$demo_lectura_filas )
   
   setnames( sgo_act_tran, 
             c( 'CEDULA_COD', 'SEXO', 'FECHA_NACIMIENTO', 'FECHA_MUERTE', 'IMPO_SGO_2022_12',
                'IMPO_TNRH_2022_12', 'FD_INV', 'FD_VEJ', 'FD_DIS', 'FD_VIU', 'FD_ORF' ), 
             c( 'cdla', 'sexo', 'fec_nac', 'fec_dec', 'imp', 'imp_tnrh', 'fec_inv', 'fec_vej', 'fec_dis', 'fec_viu', 
                'fec_orf' ) )
   sgo_act_tran[ , sexo := factor( sexo, labels = c( 'H', 'M' ) ) ]
   sgo_act_tran[ , fec_nac := as.Date( fec_nac, format = '%d/%m/%Y' ) ]
   sgo_act_tran[ , fec_dec := as.Date( fec_dec, format = '%d/%m/%Y' ) ]
   sgo_act_tran[ , fec_vej := as.Date( fec_vej, format = '%d/%m/%Y' ) ]
   sgo_act_tran[ , fec_inv := as.Date( fec_inv, format = '%d/%m/%Y' ) ]
   sgo_act_tran[ , fec_dis := as.Date( fec_dis, format = '%d/%m/%Y' ) ]
   sgo_act_tran[ , fec_viu := as.Date( fec_viu, format = '%d/%m/%Y' ) ]
   sgo_act_tran[ , fec_orf := as.Date( fec_orf, format = '%d/%m/%Y' ) ]
   
   # Lectura afiliados inactivos -------------------------------------------------------------------
   message( '\tLeyendo transiciones de inactivos de SGO' )
   load( parametros$demo_data_sgo_inac )
   
   # Preparando datos
   sgo_inac_tran <- datos_comp_inac %>% 
      rename( anio = t, sexo = sexo, x = edad, Ex = n_afi, dx = n_afi_mue )
   
   # Pensionistas y jubilados SGO ------------------------------------------------------------------
   message( '\tLeyendo transiciones de pensionistas de SGO' )
   sgo_pen_tran <- fread( file = parametros$demo_data_sgo_pen_tran,
                          sep = '\t', 
                          key = 'CEDULA_COD' )
   setnames( sgo_pen_tran,
             c( 'CEDULA_COD', 'TIPO', 'SEXO', 'FECHA_NACIMIENTO', 'FECHA_MUERTE', 'IMPOSICIONES' ), 
             c( 'cdla', 'tipo', 'sexo', 'fec_nac', 'fec_dec', 'imp' ) )
   sgo_pen_tran[ , sexo := factor( sexo, labels = c( 'H', 'M' ) ) ]
   sgo_pen_tran[ , fec_nac := as.Date( fec_nac, format = '%d/%m/%Y' ) ]
   sgo_pen_tran[ , fec_dec := as.Date( fec_dec, format = '%d/%m/%Y' ) ]
   
   # Guardando transiciones ------------------------------------------------------------------------
   message( '\tGuardando información original de transiciones del SGO' )
   save( sgo_act_tran, 
         sgo_inac_tran,
         sgo_pen_tran,
         file = parametros$demo_rdata_sgo_tran )
   gc()
   
} else if ( parametros$demo_lect_tran & parametros$seguro == 'SSC' ){
  
  # Lectura transiciones de activos SSC -----------------------------------------------------------
  message( '\tLeyendo transiciones de activos del SSC' )
  colclass <- c( rep( 'character', 4 ), rep('numeric', 2 ), rep( 'character', 2 ),
                 rep( 'numeric', 11 ), rep( 'numeric', 11 ), rep( 'numeric', 132 )
                 )
  ssc_act_tran <- fread( file = parametros$demo_data_ssc_act_tran,
                         sep = 'auto',
                         key = 'CEDULA_COD',
                         colClasses = colclass,
                         nrows = parametros$demo_lectura_filas )

  setnames( ssc_act_tran,
            c( 'CEDULA_COD', 'SEXO', 'FECHA_NACIMIENTO', 'FECHA_MUERTE', 'IMPOSICIONES_2020_12',
               'IMPOSICIONES_2022_12',
               'FD_INV', 'FD_VEJ',
            paste0( 'S', seq(2012, 2022, 1)), # Salarios
            paste0( 'A', seq(2012, 2022, 1)), # Aportes
            paste0( 'M', rep(seq(2012,2022,1), each = 12), "_", rep(seq(1,12,1), times = 2) ) ), #Días trabajados 
            
            c( 'cdla', 'sexo', 'fec_nac', 'fec_dec', 'imp_2020', 
               'imp_2022', 
               'fec_inv', 'fec_vej',
               paste0( 'S', seq(2012, 2022, 1) ) ,
               paste0( 'A', seq(2012, 2022, 1) ),
               paste0( 'M', rep(seq(2012,2022,1), each = 12), "_", rep(seq(1,12,1), times = 2) ) ) )
  

  
  ssc_act_tran[ , sexo := factor( sexo, labels = c( 'H', 'M' ) ) ]
  ssc_act_tran[ , fec_nac := as.Date( fec_nac, format = '%d/%m/%Y' ) ]
  ssc_act_tran[ , fec_dec := as.Date( fec_dec, format = '%d/%m/%Y' ) ]
  ssc_act_tran[ , fec_vej := as.Date( fec_vej, format = '%d/%m/%Y' ) ]
  ssc_act_tran[ , fec_inv := as.Date( fec_inv, format = '%d/%m/%Y' ) ]
  # 
  # # Lectura afiliados inactivos -------------------------------------------------------------------
  # message( '\tLeyendo transiciones de inactivos de SGO' )
  # load( parametros$demo_data_sgo_inac )
  # 
  # # Preparando datos
  # sgo_inac_tran <- datos_comp_inac %>% 
  #   rename( anio = t, sexo = sexo, x = edad, Ex = n_afi, dx = n_afi_mue )
  # 
  # Pensionistas y jubilados SSC ------------------------------------------------------------------
  message( '\tLeyendo transiciones de pensionistas del SSC' )
  ssc_pen_tran <- fread( file = parametros$demo_data_ssc_pen_tran,
                         sep = '\t',
                         key = 'CEDULA_COD' )
  setnames( ssc_pen_tran,
            c( 'CEDULA_COD', 'TIPO', 'SEXO', 'FECHA_NACIMIENTO', 'FECHA_MUERTE', 'IMPOSICIONES',
               paste0( 'I', seq(2012, 2022, 1) ), # Monto Ingreso de la pensión
               paste0( 'M', rep(seq(2012,2022,1), each = 12), "_", rep(seq(1,12,1), times = 2) ) #Meses pagados 
               ),
            c( 'cdla', 'tipo', 'sexo', 'fec_nac', 'fec_dec', 'imp',
            paste0( 'I', seq(2012, 2022, 1) ),
            paste0( 'M', rep(seq(2012,2022,1), each = 12), "_", rep(seq(1,12,1), times = 2) ) ) )
  
  ssc_pen_tran[ , sexo := factor( sexo, labels = c( 'H', 'M' ) ) ]
  ssc_pen_tran[ , fec_nac := as.Date( fec_nac, format = '%d/%m/%Y' ) ]
  ssc_pen_tran[ , fec_dec := as.Date( fec_dec, format = '%d/%m/%Y' ) ]

  # Guardando transiciones -------------------------------------------------------------------------
  message( '\tGuardando información original de transiciones del SSC' )
  save( ssc_act_tran, 
        ssc_pen_tran,
        file = parametros$demo_rdata_ssc_tran )
  gc()
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()
