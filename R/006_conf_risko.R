suppressPackageStartupMessages( library ( Risko ) )

parametros$data_server_global <- get_data_server()
# parametros$data_server <- paste0( parametros$data_server_global, 'IESS/IESS_estudio/' )
# parametros$data_server <- paste0( parametros$work_dir )
parametros$data_server <- paste0( parametros$data_server_global, 'IESS/IESS_2023/' )

# Direcciones globales
parametros$Data <- paste0( parametros$data_server, 'Data/Data/' )
parametros$RData <- paste0( parametros$data_server, 'RData/' )
parametros$SQL <- paste0( parametros$data_server, 'SQL/' )
parametros$RSQL <- paste0( parametros$data_server, 'RSQL/' )

# Direcciones para datos demográficos
parametros$RData_dem <- paste0( parametros$RData, 'demografia/' )
parametros$Data_dem <- paste0( parametros$Data, 'demografia/' )

# Direcciones para datos macroeconómicos
parametros$RData_macro <- paste0( parametros$RData, 'macro/' )
parametros$Data_macro <- paste0( parametros$Data, 'macro/' )

# parametros$RData  <- paste0( '/home/leovelez/Development/IESS_estudio/', 'RData_18MAR2020/' )
# parametros$Data <- paste0( '/home/leovelez/Development/IESS_estudio/', 'Data_18MAR2020/' )
