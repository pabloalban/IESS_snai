# Verificación ley de conservación del modelo demográfico ------------------------------------------

# Carga de información -----------------------------------------------------------------------------
load( parametros$demo_rdata_sgo_pea_proj )
load( parametros$demo_rdata_sgo_pob_proy )
load( parametros$demo_rdata_sgo_probs_tran )

# Preparación parámetros ---------------------------------------------------------------------------
# Horizonte de proyección
t_min <- 0
t_max <- parametros$demo_horizonte # horizonte de proyección
t_lst <- seq( t_min, t_max, 1 )
nt <- length( t_lst )

# Tiempos de servicio
s_min <- 0
s_max <- parametros$demo_ts_max
s_lst <- seq( s_min, s_max, 1 )
ns <- length( s_lst )

# Edades
x_min <- 0
x_max <- parametros$demo_edad_max
x_lst <- seq( x_min, x_max, 1 )
nx <- length( x_lst )

nd <- 6

pob_chk <- copy( pob_proy_tot )

# Verificiación que las probabilidades suman cada una de sus filas 1 -------------------------------
chk <- NULL
for ( t in 1:length( t_lst ) ) {
  for ( x in 1:length( x_lst ) ) {
    for ( s in 1:length( s_lst ) ) {
      chk <- rbind( chk,
                    c( all( abs( rowSums( Pm[ t, s, x, , ] ) - 1 ) < 1e-10 ), 
                       all( abs( rowSums( Ph[ t, s, x, , ] ) - 1 ) < 1e-10 ) ) )
    }
  }
}
colSums( chk ) - nrow( chk )

# Verificación de la ley de conservación a nivel del array utilizado en el cálculo -----------------
cl <- NULL
for ( t in 1:( nt - 1 ) ) {
  for ( x in 1:( nx - 1 ) ) {
    for ( s in 1:( ns - 1 ) ) {
      for ( i in 1:nd ) {
        
        I <- sapply( 1:nd, function( u ) nd * ( u - 1 ) + i )
        J <- sapply( 1:nd, function( v ) nd * ( i - 1 ) + v )
        
        if ( i == 1 & s == 1 ) {  
          a <- lm[ t + 1, 1, x + 1, i ]
          b <- lm[ t, 1, x, i ] + sum( ltm[ t + 1, 2, x + 1, I[-i] ] ) - sum( ltm[ t + 1, 2, x + 1, J[-i] ] ) + 
            ltm[ t + 1, 1, x + 1, nd * nd + 1 ]
          
        } else {
          a <- lm[ t + 1, s + 1, x + 1, i ]
          b <- lm[ t, s, x, i ] + sum( ltm[ t + 1, s + 1, x + 1, I[-i] ] ) - sum( ltm[ t + 1, s + 1, x + 1, J[-i] ] )    
        }
        r <- abs( a - b )
        if ( r >= 1 ) {
          cl <- rbindlist( list( cl, data.table( t, s, x, i, r ) ) )
        }
      }
    }
  }
}
is.null( cl )

# Conservación para PEA inactivos, estado 1 --------------------------------------------------------
pob_chk[ , l1n := shift( l1 ) + l0_1 + l2_1 + l3_1 + l4_1 + l5_1 - l1_2 - l1_3 - l1_4 - l1_5 - l1_6 ]
# No es cero, corresponde a los jóvenes que ingresan a la PEA y no tienen empleo formal
pob_chk[ , c1 := l1 - l1n ]
print( pob_chk$c1 )

# Conservación para activos, estado 2 --------------------------------------------------------------
pob_chk[ , l2n := shift( l2 ) + l1_2 + l3_2 + l4_2 + l5_2 - l2_1 - l2_3 - l2_4 - l2_5 - l2_6 ]
pob_chk[ , c2 := l2 - l2n ]
print( pob_chk$c2 )

# Conservación para cesantes/inactivos, estado 3 ---------------------------------------------------
pob_chk[ , l3n := shift( l3 ) + l1_3 + l2_3 + l4_3 + l5_3 - l3_1 - l3_2 - l3_4 - l3_5 - l3_6 ]
pob_chk[ , c3 := l3 - l3n ]
print( pob_chk$c3 )

# Conservación para pensionistas de vejez, estado 4 ------------------------------------------------
pob_chk[ , l4n := shift( l4 ) + l1_4 + l2_4 + l3_4 + l5_4 - l4_1 - l4_2 - l4_3 - l4_5 - l4_6 ]
pob_chk[ , c4 := l4 - l4n ]
print( pob_chk$c4 )

# Conservación para pensionistas de invalidez, estado 5 --------------------------------------------
pob_chk[ , l5n := shift( l5 ) + l1_5 + l2_5 + l3_5 + l4_5 - l5_1 - l5_2 - l5_3 - l5_4 - l5_6 ]
pob_chk[ , c5 := l5 - l5n ]
print( pob_chk$c5 )

# El deceso representa un estado absorvente --------------------------------------------------------
# Nadie realiza una transición después de alcanzar este estado
pob_chk[ , s6 := l6_1 + l6_2 + l6_3 + l6_4 + l6_5 ]
print( pob_chk$s6 )

# El estado 1 de la PEA no afiliada representa un estado fuente ------------------------------------
# Después de salir de este estado nadie regresa a este
pob_chk[ , r1 := l2_1 + l3_1 + l4_1 + l5_1 + l6_1 ] 
print( pob_chk$r1 )

# No se produce PEAD no afiliada con tiempo de servicio superior a 0 -------------------------------
pob_chk_ts <- pob_proy_ts[ s > 0, list( l1 = sum( l1 ) ), by = list( t ) ]
print( pob_chk_ts$l1 )

# Proyección de la PEA -----------------------------------------------------------------------------
# Tomando en cuenta la compensación por pérdidad de la misma en proyecciones 
pob_proy_tot[ , list( l = l1 + l2 + l3 )]
PEA_proy[ , list( pea = sum( peax ) ), by = list( anio )]
