message( paste( rep( "-", 100 ), collapse = "" ) )
message( "\tCargando tasas macro" )
load( parametros$macro_rdata_info )

# 1. Interpolación del SBU por meses ---------------------------------------------------------------

## Preparación de la data --------------------------------------------------------------------------

data <- sbu %>%
  mutate( sbu_f = ( sbu + lead( sbu ) ) / 2 ) %>%  
  filter( anio <= 2022 ) %>%
  left_join( expand.grid( mes = c( 1:12 ), anio = c( 2000:2023 ) ), ., by = c( 'anio' ) ) %>%
  arrange( anio, mes ) %>%
  mutate( sbu = if_else( mes == '6',
                         sbu,
                         NA ) ) %>% 
  mutate( sbu = ifelse( mes == '12',
                        sbu_f,
                        sbu ) ) %>% 
  dplyr::select( anio,
                 mes,
                 sbu ) %>% 
  rbind( data.frame( anio = c( 2023 ),
                     mes = c( 6 ),
                     sbu = c( 450 ) ) ) %>% 
  mutate( sbu = if_else( mes == '1' & anio == '2000',
                         57,
                         sbu ) ) %>% 
  mutate( t = c( 1: n( ) ) )

aux <- na.omit( data )

## Interpolación  del SBU --------------------------------------------------------------------------
set.seed( 10 ) #1 11

kalman <- na.kalman( data$sbu, model = "StructTS", smooth = TRUE ) + rnorm( length( data$sbu ), mean=0, sd = 3 )

pred <- data.frame(
  t = c( 1:nrow( data )  ),
  anio = data$anio,
  mes = data$mes,
  sbu = data$sbu,
  sbu_int = kalman ) %>%
  mutate( error_int = 100*( sbu - sbu_int )/ sbu )


## Error porcentual --------------------------------------------------------------------------------

mean( pred$error_int, na.rm = TRUE )


## Gráficos de control -----------------------------------------------------------------------------
plot( pred$t, pred$sbu_int, type = "S",
      col = "red", xlab = "x", ylab = "y" )
points( pred$t, pred$sbu, col = "blue" )


## Creando data frame con los sbu mensual ----------------------------------------------------------

sbu_mensual <- pred %>% 
  dplyr::select( anio,
                 mes,
                 sbu_mensual := sbu_int ) %>% 
  filter( anio <= 2022 )



shapiro.test( ( diff( sbu_mensual$sbu_mensual, 1 ) ) )

a<- diff( sbu_mensual$sbu_mensual, 1 )[-c(1:15)]
shapiro.test( a)

plot.ts( ( diff( sbu_mensual$sbu_mensual, 1 ) ) )

plot.ts( diff( sbu_mensual$sbu_mensual, 2 ) )

# 2. Interpolación de los Salarios promedios -------------------------------------------------------

## Preparación de la data --------------------------------------------------------------------------

data <- salarios %>%
  arrange( anio, mes ) %>%
  dplyr::select( anio,
                 mes,
                 sal_prom )  %>% 
  mutate( t = c( 1: n( ) ) )

aux <- na.omit( data )

## Interpolación  del salarios antes de 2006 -------------------------------------------------------
set.seed( 45 )
sd_sal <- stats::sd( diff( aux$sal_prom, na.rm = TRUE ) ) / 4

kalman <- na_kalman( data$sal_prom, model = "StructTS", smooth = TRUE) +
  rnorm( length( data$sal_prom ),
         mean = 0,
         sd = sd_sal )


pred <- data.frame(
  t = c( 1:nrow( data )  ),
  anio = data$anio,
  mes = data$mes,
  sal_prom = data$sal_prom,
  sal_prom_int = kalman ) %>%
  mutate( error_int = 100*( sal_prom - sal_prom_int )/ sal_prom )

## Error porcentual promedio -----------------------------------------------------------------------

mean( pred$error_int, na.rm = TRUE )

## Gráficos de control -----------------------------------------------------------------------------
plot( pred$t, pred$sal_prom_int, type = "S",
      col = "red", xlab = "x", ylab = "y" )
points( pred$t, pred$sal_prom, col = "blue" )

## Creando data frame con los pib mensual ----------------------------------------------------------

salario_mensual <- pred %>% 
  mutate( sal_prom_int := if_else( anio > 2006,
                                   sal_prom,
                                   sal_prom_int ) ) %>%
  filter( anio <= 2022 ) %>% 
  dplyr::select( anio,
                 mes,
                 sal_prom:= sal_prom_int )


shapiro.test( ( diff( salario_mensual$sal_prom, 1 ) ) )

plot.ts( ( diff( salario_mensual$sal_prom, 1 ) ) )



# 3. Alisamiento de las tasas de interés -----------------------------------------------------------
## Preparación de la data --------------------------------------------------------------------------

data <- tasas_interes %>%
  dplyr::select( anio,
                 mes,
                 tasa_activa,
                 tasa_pasiva )  %>% 
  mutate( t = c( 1: n( ) ) )

aux <- na.omit( data )

## Interpolación  de la tasa activa ----------------------------------------------------------------

mod <- smooth.spline( aux$t, aux$tasa_activa, df = 60 ) # df degrees of freedom

pred <- data.frame(
  t = c( 1:nrow( data ) ),
  tasa_activa_int = predict( mod, c( 1:nrow( data ) ), deriv = 0 )[["y"]]
)

pred_ta <- left_join( data, pred, by = 't' ) %>% 
  mutate( tasa_activa = if_else( is.na( tasa_activa ),
                                 tasa_activa_int,
                                 tasa_activa ) )

## Interpolación  de la tasa activa ----------------------------------------------------------------

mod <- smooth.spline( aux$t, aux$tasa_pasiva, df = 100 ) # df degrees of freedom

pred <- data.frame(
  t = c( 1:nrow( data ) ),
  tasa_pasiva_int = predict( mod, c( 1:nrow( data ) ), deriv = 0 )[["y"]]
)

set.seed( 5 )

pred_tp <- left_join( data, pred, by = 't' ) %>% 
  mutate( tasa_pasiva = if_else( is.na( tasa_pasiva ),
                                 tasa_pasiva_int,
                                 tasa_pasiva + rnorm( length( pred$t ),
                                                      mean = 0,
                                                      sd = 0.005 ) ) )

plot.ts( diff( pred_tp$tasa_pasiva ) )
## Consolidar base ---------------------------------------------------------------------------------

tasas_interes_mensuales <- pred_ta %>% dplyr::select( anio, mes, tasa_activa ) %>% 
  left_join( ., pred_tp %>% dplyr::select( anio, mes, tasa_pasiva ), by = c( 'anio', 'mes' ) ) %>% 
  mutate( spread = tasa_activa - tasa_pasiva )

## Gráficos de control -----------------------------------------------------------------------------


# 4. Interpolación del PIB a meses -----------------------------------------------------------------

## Preparación de la data --------------------------------------------------------------------------

data <- pib_trimestral %>%
  mutate( z_t = cumsum( pib_trim ), 
          anio = as.integer( anio ) ) %>%
  filter( anio <= 2022 ) %>%
  mutate( mes = trimestre * 3 ) %>%
  left_join( expand.grid( anio = c( 2000:2022 ), mes = c( 1:12 ) ), ., by = c( 'anio', 'mes' ) ) %>%
  arrange( anio, mes ) %>%
  mutate( t = c( 1: n( ) ) ) 

aux <- na.omit( data )

## Interpolación por Kalman  del PIB ---------------------------------------------------------------
set.seed(101)

sd_pib_trim <- stats::sd( diff( pib_trimestral$pib_trim, na.rm = TRUE ) ) / 2

kalman <- na_kalman( data$z_t, model = "StructTS", smooth = TRUE) + rnorm( length(data$z_t), mean = 0, sd = sd_pib_trim)

pred <- data.frame(
  t = c( 1:nrow( data ) ),
  z_t = data$z_t,
  zt_int = kalman ) %>%
  mutate( error_int = 100*( z_t - zt_int )/ z_t )

## Error porcentual --------------------------------------------------------------------------------

mean( pred$error_int, na.rm = TRUE )

## Gráficos de control -----------------------------------------------------------------------------
plot( pred$t, pred$zt_int, type = "S",
      col = "red", xlab = "x", ylab = "y" )
points( pred$t, pred$zt, col = "blue" )

## Transformar a meses -----------------------------------------------------------------------------
aux <- diff( pred$zt_int, differences = 1 )
pib_mensual <- c( pred$zt[3]/3, pred$zt[3]/3, pred$zt[3]/3  , aux[ -c( 1, 2 ) ] )
pred['pib_mensual'] <- pib_mensual

## Creando data frame con los pib mensual ----------------------------------------------------------

pib_mensual <- pred %>%
  cbind( data %>% dplyr::select( anio, mes ), . ) %>% 
  dplyr::select( anio,
                 mes,
                 pib := pib_mensual )

shapiro.test( ( diff( pib_mensual$pib, 1 ) ) )

plot.ts( ( diff( pib_mensual$pib, 1 ) ) )

# Guardar en un RData ------------------------------------------------------------------------------
message( '\tGuardando tasas macro interpoladas' )

save( sbu_mensual,
      salario_mensual,
      tasas_interes_mensuales,
      pib_mensual,
      file = parametros$macro_rdata_series_int )

# Borrar elementos restantes -----------------------------------------------------------------------
message( paste( rep( "-", 100 ), collapse = "" ) )
rm( list = ls( )[ !( ls( ) %in% c( "parametros" ) ) ] )
gc( )