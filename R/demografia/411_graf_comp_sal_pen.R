load( paste0( parametros$RData, 'IESS_salarios_pensiones_iniciales.RData' )  )

tst <- merge( pen_afi, sal_afi, by = c( 'sexo', 'x' ), all.y = TRUE )
tst_f <- tst[ sexo == 'F' ]
tst_m <- tst[ sexo == 'M' ]

plot( tst_f$x, tst_f$pen, type = 'l', col = parametros$iess_blue, ylim = c( 0, 2000 ) )
points( tst_f$x, tst_f$sal, type = 'l', col = parametros$iess_green )

plot( tst_m$x, tst_m$pen, type = 'l', col = parametros$iess_blue, ylim = c( 0, 2000 ) )
points( tst_m$x, tst_m$sal, type = 'l', col = parametros$iess_green )

load( paste0( parametros$RData, 'IESS_salarios_pensiones_iniciales_v3.RData' )  )

tst <- merge( pen_afi[ tip == 'JV' ], sal_afi, by = c( 'sexo', 'x' ), all.y = TRUE )
tst_f <- tst[ sexo == 'F' & x >= 20 ]
tst_m <- tst[ sexo == 'M' & x >= 20 ]

plot( tst_f$x, tst_f$sal, type = 'l', col = parametros$iess_blue, ylim = c( 0, 2000 ) )
points( tst_f$x, tst_f$pen, type = 'l', col = parametros$iess_green )

plot( tst_m$x, tst_m$sal, type = 'l', col = parametros$iess_blue, ylim = c( 0, 2000 ) )
points( tst_m$x, tst_m$pen, type = 'l', col = parametros$iess_green )

plot( tst_m$x, tst_m$sal, type = 'l', col = parametros$iess_blue, ylim = c( 0, 1500 ) )
points( tst_f$x, tst_f$sal, type = 'l', col = parametros$iess_green )

rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
