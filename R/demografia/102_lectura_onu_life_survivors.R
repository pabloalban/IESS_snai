message( paste( rep('-', 100 ), collapse = '' ) )
#link de la descarga
# https://population.un.org/wpp/Download/Standard/Mortality/

message( '\tLeyendo life table survivors de ONU' )

col_nom <- c( 'index', 'variant', 'regions', 'notes', 'country_code', 'type', 'parent_code', 
              'period', 'x0', 'x1', 'x5', 'x10', 'x15', 'x20', 'x25', 'x30', 'x35', 'x40', 'x45', 
              'x50', 'x55', 'x60', 'x65', 'x70', 'x75', 'x80', 'x85', 'x90', 'x95', 'x100' )

col_tip <- c( 'numeric', 'text', 'text', 'text', 'numeric', 'text', 'numeric', 'text', 
              rep( 'text', 22 ) )

file <- paste0( parametros$Data, 'WPP2019_MORT_F15_3_LIFE_TABLE_SURVIVORS_FEMALE.xlsx' )
onu_survivors_f <- read_xlsx( path = file, sheet = 1, skip = 16, range = 'A17:AD3587', 
                              col_names = TRUE, col_types = col_tip )
onu_survivors_f <- as.data.table( onu_survivors_f )
setnames( onu_survivors_f, col_nom )
onu_survivors_f <- melt.data.table( onu_survivors_f, 
                         id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                      'type', 'parent_code', 'period' ),
                         variable.name = 'x', value.name = 'lx' )
onu_survivors_f[ , sexo := 'F' ]

onu_survivors_medium_f <- read_xlsx( path = file, sheet = 2, skip = 16, range = 'A17:AD4097', 
                                     col_names = TRUE, col_types = col_tip )
onu_survivors_medium_f <- as.data.table( onu_survivors_medium_f )
setnames( onu_survivors_medium_f, col_nom )
onu_survivors_medium_f <- melt.data.table( onu_survivors_medium_f, 
                                id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                             'type', 'parent_code', 'period' ),
                                variable.name = 'x', value.name = 'lx' )
onu_survivors_medium_f[ , sexo := 'F' ]

file <- paste0( parametros$Data, 'WPP2019_MORT_F15_2_LIFE_TABLE_SURVIVORS_MALE.xlsx' )
onu_survivors_m <- read_xlsx( path= file, sheet = 1, skip = 16, range = 'A17:AD3587', 
                              col_names = TRUE, col_types = col_tip )
onu_survivors_m <- as.data.table( onu_survivors_m )
setnames( onu_survivors_m, col_nom )
onu_survivors_m <- melt.data.table( onu_survivors_m, 
                         id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                      'type', 'parent_code', 'period' ),
                         variable.name = 'x', value.name = 'lx' )
onu_survivors_m[ , sexo := 'M' ]

onu_survivors_medium_m <- read_xlsx( path = file, sheet = 2, skip = 16, range = 'A17:AD4097', 
                                     col_names = TRUE, col_types = col_tip )
onu_survivors_medium_m <- as.data.table( onu_survivors_medium_m )
setnames( onu_survivors_medium_m, col_nom )
onu_survivors_medium_m <- melt.data.table( onu_survivors_medium_m, 
                                id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                             'type', 'parent_code', 'period' ),
                                variable.name = 'x', value.name = 'lx' )
onu_survivors_medium_m[ , sexo := 'M' ]

onu_survivors <- rbind( onu_survivors_f, onu_survivors_m, 
                        onu_survivors_medium_f, onu_survivors_medium_m )
onu_survivors[ , x := as.numeric( gsub( 'x', '', x ) ) ]
onu_survivors[ , lx := as.numeric( lx ) ]

setorder( onu_survivors, variant, regions, country_code, type, parent_code, sexo, period, x )
onu_survivors <- onu_survivors[ , list( index, variant, regions, notes, country_code, type, 
                                        parent_code, period, sexo, x, lx ) ]


#Leyendo expectancy table survivors de ONU -------------------------------------
message( '\tLeyendo expectancy table survivors de ONU' )

col_nom <- c( 'index', 'variant', 'regions', 'notes', 'country_code', 'type', 'parent_code',
              paste0( 'x',seq(1950, 2015, 5),'-',seq(1955, 2020, 5)))

col_tip <- c( 'numeric', 'text', 'text', 'text', 'numeric', 'text', 'numeric', 
              rep( 'numeric', 14 ) )

file <- paste0( parametros$Data, 'WPP2019_MORT_F07_3_LIFE_EXPECTANCY_0_FEMALE.xlsx' )
onu_ex_f <- read_xlsx( path = file, 
                              sheet = 1, 
                              range = 'A17:U272', 
                              col_names = TRUE, col_types = col_tip )

onu_ex_f <- as.data.table( onu_ex_f )
setnames( onu_ex_f, col_nom )
onu_ex_f <- melt.data.table( onu_ex_f, 
                             id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                                 'type', 'parent_code' ),
                             variable.name = 'x', value.name = 'ex' )
onu_ex_f[ , sexo := 'F' ]

col_nom <- c( 'index', 'variant', 'regions', 'notes', 'country_code', 'type', 'parent_code',
              paste0( 'x',seq(2020, 2095, 5),'-',seq(2025, 2100, 5)))
col_tip <- c( 'numeric', 'text', 'text', 'text', 'numeric', 'text', 'numeric', 
              rep( 'numeric', 16 ) )

onu_ex_medium_f <- read_xlsx( path = file, 
                              sheet = 2, range = 'A17:W272', 
                              col_names = TRUE, col_types = col_tip )
onu_ex_medium_f <- as.data.table( onu_ex_medium_f )
setnames( onu_ex_medium_f, col_nom )
onu_ex_medium_f <- melt.data.table( onu_ex_medium_f, 
                                           id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                                        'type', 'parent_code' ),
                                           variable.name = 'x', value.name = 'ex' )
onu_ex_medium_f[ , sexo := 'F' ]

col_nom <- c( 'index', 'variant', 'regions', 'notes', 'country_code', 'type', 'parent_code',
              paste0( 'x',seq(1950, 2015, 5),'-',seq(1955, 2020, 5)))

col_tip <- c( 'numeric', 'text', 'text', 'text', 'numeric', 'text', 'numeric', 
              rep( 'numeric', 14 ) )

file <- paste0( parametros$Data, 'WPP2019_MORT_F07_2_LIFE_EXPECTANCY_0_MALE.xlsx' )
onu_ex_m <- read_xlsx( path = file, 
                       sheet = 1, 
                       range = 'A17:U272', 
                       col_names = TRUE, col_types = col_tip )

onu_ex_m <- as.data.table( onu_ex_m )
setnames( onu_ex_m, col_nom )
onu_ex_m <- melt.data.table( onu_ex_m, 
                             id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                          'type', 'parent_code' ),
                             variable.name = 'x', value.name = 'ex' )
onu_ex_m[ , sexo := 'M' ]

col_nom <- c( 'index', 'variant', 'regions', 'notes', 'country_code', 'type', 'parent_code',
              paste0( 'x',seq(2020, 2095, 5),'-',seq(2025, 2100, 5)))
col_tip <- c( 'numeric', 'text', 'text', 'text', 'numeric', 'text', 'numeric', 
              rep( 'numeric', 16 ) )

onu_ex_medium_m <- read_xlsx( path = file, 
                              sheet = 2, range = 'A17:W272', 
                              col_names = TRUE, col_types = col_tip )
onu_ex_medium_m <- as.data.table( onu_ex_medium_m )
setnames( onu_ex_medium_m, col_nom )
onu_ex_medium_m <- melt.data.table( onu_ex_medium_m, 
                                    id.vars = c( 'index', 'variant', 'regions', 'notes', 'country_code', 
                                                 'type', 'parent_code' ),
                                    variable.name = 'x', value.name = 'ex' )
onu_ex_medium_m[ , sexo := 'M' ]

onu_ex <- rbind( onu_ex_f, onu_ex_m, 
                 onu_ex_medium_f, onu_ex_medium_m )
onu_ex[ , t := as.character( gsub( 'x', '', x ) ) ]
onu_ex[ , ex := as.numeric( ex ) ]

setorder( onu_ex, variant, regions, country_code, type, parent_code, sexo, t )
onu_ex <- onu_ex[ , list( index, variant, regions, notes, country_code, type, 
                                        parent_code, t, sexo, ex ) ]

message( '\tLeyendo life standar ONU' )
# link tablas estandar ONU
# https://www.un.org/en/development/desa/population/publications/pdf/mortality/EMLT/MLT_UN2011_130_1y_complete.xlsx
file <- paste0( parametros$Data, 'MLT_UN2011_130_1y_complete.xlsx' )
col_nom <- c( 'Type_MLT', 'Family', 'Type', 'Sex', 'E0', 'age', 'mx1', 'qx1',
              'lx1', 'dx1', 'Lx1', 'Tx1', 'sx1', 'ex1', 'ax1')

col_tip <- c( 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 
              'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
              'numeric', 'numeric')

onu_estandar_life <- read_xlsx( path = file,
                                sheet = 1,  
                                col_names = TRUE, 
                                col_types = col_tip )

onu_estandar_life <- as.data.table( onu_estandar_life )
setnames( onu_estandar_life, col_nom )





message( '\tGuardando tablas' )
save( onu_survivors, onu_estandar_life, onu_ex, file = paste0( parametros$RData, 'ONU_life_table_survivors_2019.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
