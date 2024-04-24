message( paste( rep( '-', 100 ), collapse = '' ) )
message( 
  '\tCarga de información de transiciones de los cotizantes al Seguro de Cesantía y Desempleo'
)

# Carga de funciones -------------------------------------------------------------------------------
source( 
  paste0( parametros$work_dir, "R/demografia/003_carga_funciones.R" ),
  encoding = 'UTF-8',
  echo = FALSE
)

#1.Carga de masa salarial---------------------------------------------------------------------------

des_ces_act_tran_1 <-
  fread( 
    file = parametros$demo_data_ces_des_act_tran,
    sep = '\t',
    header = TRUE,
    dec = ".",
    key = 'CEDULA',
    fill = TRUE,
    #nrows = 10000,
    select = c( 
      "CEDULA",
      "SEXO",
      "FECHA_NACIMIENTO",
      "FECHA_MUERTE",
      "IMPOSICIONES_2020_12",
      "FD_VEJ",
      "FD_INV",
      "FD_DIS",
      "FD_VIU",
      "FD_ORF",
      "S2012",
      "S2013",
      "S2014",
      "S2015",
      "S2016",
      "S2017",
      "S2018",
      "S2019",
      "S2020", 
      "S2021",
      "S2022"
    ),
    colClasses = c( 
      CEDULA = "character",
      SEXO = "character",
      FECHA_NACIMIENTO = "character",
      FECHA_MUERTE = "character",
      IMPOSICIONES_2020_12 = "numeric",
      FD_VEJ = "character",
      FD_INV = "character",
      FD_DIS = "character",
      FD_VIU = "character",
      FD_ORF = "character",
      S2012 = 'numeric',
      S2013  = 'numeric',
      S2014 = 'numeric',
      S2015 = 'numeric',
      S2016 = 'numeric',
      S2017 = 'numeric',
      S2018 = 'numeric',
      S2019 = 'numeric',
      S2020 = 'numeric',
      S2021 = 'numeric',
      S2022 = 'numeric'
    )
  )

colclass <- data.frame( col = sapply( des_ces_act_tran_1, class ) )

des_ces_act_tran_2 <-
  fread( 
    file = parametros$demo_data_ces_des_act_tran,
    sep = '\t',
    header = TRUE,
    dec = ".",
    #nrows = 10000,
    #key = 'CEDULA',
    fill = TRUE,
    select = c( 1, 23:154 ),
    colClasses = list( character = 1,
                       numeric = 23:154 )
  )


sgo_act_tran <- full_join( des_ces_act_tran_1, des_ces_act_tran_2, by = 'CEDULA' )

rm( des_ces_act_tran_1, des_ces_act_tran_2 )

setnames( 
  sgo_act_tran,
  c( 
    'CEDULA',
    'SEXO',
    'FECHA_NACIMIENTO',
    'FECHA_MUERTE',
    'IMPOSICIONES_2020_12',
    'FD_INV',
    'FD_VEJ',
    'FD_DIS',
    'FD_VIU',
    'FD_ORF'
  ),
  c( 
    'cdla',
    'sexo',
    'fec_nac',
    'fec_dec',
    'imp',
    'fec_inv',
    'fec_vej',
    'fec_dis',
    'fec_viu',
    'fec_orf'
  )
)
sgo_act_tran[ , sexo := factor( sexo, labels = c( 'H', 'M' ) ) ]
sgo_act_tran[ , fec_nac := as.Date( fec_nac, format = '%d/%m/%Y' ) ]
sgo_act_tran[ , fec_dec := as.Date( fec_dec, format = '%d/%m/%Y' ) ]
sgo_act_tran[ , fec_vej := as.Date( fec_vej, format = '%d/%m/%Y' ) ]
sgo_act_tran[ , fec_inv := as.Date( fec_inv, format = '%d/%m/%Y' ) ]
sgo_act_tran[ , fec_dis := as.Date( fec_dis, format = '%d/%m/%Y' ) ]
sgo_act_tran[ , fec_viu := as.Date( fec_viu, format = '%d/%m/%Y' ) ]
sgo_act_tran[ , fec_orf := as.Date( fec_orf, format = '%d/%m/%Y' ) ]

# Guardando transiciones ---------------------------------------------------------------------------
message( '\tGuardando información original de transiciones' )
save( sgo_act_tran,
      file = parametros$demo_rdata_ces_des_tran )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )
