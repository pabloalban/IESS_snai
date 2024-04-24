load( parametros$demo_rdata_sgo_tran )

# muestreo
colmn <- grep( "_12", names(sgo_act_tran), value = TRUE, fixed = TRUE ) 
muestra <- 100000
act_muestra <- sgo_act_tran[ sample( 1:nrow( sgo_act_tran), muestra, replace = FALSE ), 
                             .SD, .SDcols = colmn ]
alfa <- nrow( sgo_act_tran ) / muestra
conteo <- act_muestra[ , lapply( .SD, function(x) alfa * sum( x, na.rm = TRUE ) / 30 ) ]

# toda la base
colmn <- grep( "_12", names(sgo_act_tran), value = TRUE, fixed = TRUE ) 
act_muestra <- sgo_act_tran[ , .SD, .SDcols = colmn ]
conteo <- act_muestra[ , lapply( .SD, function(x) sum( x, na.rm = TRUE ) / 30 ) ]
conteo$M2018_12
