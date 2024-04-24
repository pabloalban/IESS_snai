message( paste( rep( "-", 100 ), collapse = "" ) )

#Carga de funciones---------------------------------------------------------------------------------
source( 'R/macro/003_carga_funciones.R', encoding = 'UTF-8', echo = FALSE )

# 1. Cargando Datos --------------------------------------------------------------------------------
message( "\tCargando tasas macro" )

load( parametros$macro_rdata_info )
load( parametros$macro_rdata_series_int )

## 1.1. Parámetros ---------------------------------------------------------------------------------

n = 38 #Número de años a predecir
anio_ini = 2002 # Año inicial
alpha = 0.05 # (1 - alpha )*100% límites de predicciones

# 2. Preparando data -------------------------------------------------------------------------------

tasas_macro <-
  expand.grid( anio = c( 2000:2022 ), mes = c( 1:12 ) ) %>%
  left_join( ., pib_mensual, by = c( 'anio', 'mes' ) ) %>%
  left_join( 
    .,
    tasas_interes_mensuales %>% dplyr::select( anio, mes, tasa_pasiva, spread, tasa_activa ),
    by = c( 'anio', 'mes' )
  ) %>%
  left_join( .,
             inflacion %>% dplyr::select( anio, mes, ipc := ipc ),
             by = c( 'anio', 'mes' ) ) %>%
  left_join( .,
             salario_mensual %>% dplyr::select( anio, mes, sal := sal_prom ),
             by = c( 'anio', 'mes' ) ) %>%
  left_join( .,
             sbu_mensual %>% dplyr::select( anio, mes, sbu := sbu_mensual ),
             by = c( 'anio', 'mes' ) ) %>%
  arrange( anio, mes ) %>%
  mutate( tasa_pasiva = tasa_pasiva,
          spread = spread ) %>%
  mutate( pib = pib / 1000000 ) %>% #escala del PIB en miles de millones de USD
  dplyr::select( -spread ) %>%
  filter( anio >= anio_ini )


## 2.1. Pruebas de estacionariedad de Dickey-Fuller ------------------------------------------------
#Ho: La serie de tiempo es no estacionaria | Si p<0.05 se rechaza la H0
aux_1 <- adfTest( diff( tasas_macro$pib, differences = 1 ) )
aux_3 <- adfTest( diff( tasas_macro$tasa_pasiva, differences = 1 ) )
aux_4 <- adfTest( diff( tasas_macro$sal, differences = 1 ) )
aux_5 <- adfTest( diff( tasas_macro$sbu, differences = 1 ) )
aux_6 <- adfTest( diff( tasas_macro$ipc, differences = 1 ) )

dickey_fuller <- data.frame( 
  variable = c( 
    '$\\nabla$ PIB',
    '$\\nabla$ Tasa pasiva',
    '$\\nabla$ Salarios Promedio',
    '$\\nabla$ SBU',
    '$\\nabla$ Inflación Promedio'
  ),
  estadistico = c( 
    aux_1@test$statistic,
    aux_3@test$statistic,
    aux_4@test$statistic,
    aux_5@test$statistic,
    aux_6@test$statistic
  ),
  p_valor = c( 
    aux_1@test$p.value,
    aux_3@test$p.value,
    aux_4@test$p.value,
    aux_5@test$p.value,
    aux_6@test$p.value
  )
)

## 2.2. Diferenciación de la series ----------------------------------------------------------------
data_diff <- tasas_macro %>%
  na.omit( . ) %>%
  mutate( 
    diff_pib = c( NA,  base::diff( pib, 1 ) ),
    diff_tasa_pasiva = c( NA, base::diff( tasa_pasiva, 1 ) ),
    diff_sal_prom = c( NA, base::diff( sal, 1 ) ),
    diff_sbu = c( NA, base::diff( sbu, 1 ) ),
    diff_ipc = c( NA, base::diff( ipc, 1 ) )
  ) %>%
  dplyr::select( diff_pib,
                 diff_tasa_pasiva,
                 diff_sal_prom,
                 diff_sbu,
                 diff_ipc )

## 2.3. Tratamiento de outliers --------------------------------------------------------------------
data_diff_out <- data_diff %>%
  na.omit( . ) %>%
  mutate( 
    diff_pib = outliers( diff_pib ),
    diff_tasa_pasiva = outliers( diff_tasa_pasiva ),
    diff_sal_prom = outliers( diff_sal_prom ),
    diff_sbu = outliers( diff_sbu ),
    diff_ipc = outliers( diff_ipc )
  )

## 2.4. Conjunto de entrenamiento ------------------------------------------------------------------
train <- data_diff_out


# 3. Estimación del modelo -------------------------------------------------------------------------
## 3.1. Pruebas de causalidad ----------------------------------------------------------------------

lmtest::grangertest( diff_pib ~  diff_tasa_pasiva, order = 6, data = train )
lmtest::grangertest( diff_pib ~  diff_sal_prom, order = 6, data = train )
lmtest::grangertest( diff_pib ~  diff_sbu, order = 6, data = train )
lmtest::grangertest( diff_pib ~  diff_ipc, order = 6, data = train )



lmtest::grangertest( diff_sbu ~  diff_pib, order = 6, data = train ) #
lmtest::grangertest( diff_sbu ~  diff_tasa_pasiva, order = 6, data = train )
lmtest::grangertest( diff_sbu ~  diff_sal_prom, order = 6, data = train )
lmtest::grangertest( diff_sbu ~  diff_ipc, order = 6, data = train ) ##


lmtest::grangertest( diff_tasa_pasiva  ~  diff_pib, order = 6, data = train )
lmtest::grangertest( diff_tasa_pasiva  ~  diff_sbu, order = 6, data = train )
lmtest::grangertest( diff_tasa_pasiva  ~  diff_sal_prom,
                     order = 6,
                     data = train ) #
lmtest::grangertest( diff_tasa_pasiva  ~  diff_ipc, order = 6, data = train )


lmtest::grangertest( diff_sal_prom  ~  diff_pib, order = 6, data = train )
lmtest::grangertest( diff_sal_prom  ~  diff_sbu, order = 6, data = train ) #
lmtest::grangertest( diff_sal_prom  ~  diff_tasa_pasiva,
                     order = 6,
                     data = train ) #
lmtest::grangertest( diff_sal_prom  ~  diff_ipc, order = 6, data = train ) ##

lmtest::grangertest( diff_ipc  ~  diff_pib, order = 6, data = train ) #
lmtest::grangertest( diff_ipc  ~  diff_sbu, order = 6, data = train ) #
lmtest::grangertest( diff_ipc  ~  diff_tasa_pasiva, order = 6, data = train )
lmtest::grangertest( diff_ipc  ~  diff_sal_prom, order = 6, data = train ) #

## 3.2. Selección del modelo VAR -------------------------------------------------------------------

vars::VARselect( train,
                 type = "none",
                 lag.max = 20 )

## 3.3. Estimación del modelo VAR ------------------------------------------------------------------

var.a <- vars::VAR( train,
                    lag.max = 2,
                    #se eligió usando el criterio de Hannan–Quinn
                    ic = "AIC",
                    type = "both" )
summary( var.a )

#Se quita los regresores no significativos del VAR( 2 )

restrict <- matrix( 
  c( 
    1,    0,    0,    0,    0,    # diff_pib.l1
    0,    1,    0,    0,    0,    # diff_tasa_pasiva.l1
    0,    0,    1,    0,    0,    # diff_sal_prom.l1
    0,    0,    0,    1,    0,    # diff_sbu.l1
    1,    0,    0,    0,    1,    # diff_ipc.l1
    
    1,    0,    0,    0,    0,    # diff_pib.l2
    0,    0,    0,    0,    0,    # diff_tasa_pasiva.l2
    0,    0,    0,    0,    0,    # diff_sal_prom.l2
    0,    0,    0,    1,    0,    # diff_sbu.l2
    0,    0,    0,    1,    0,    # diff_ipc.l2
    
    0,    0,    1,    1,    1,    # diff_ipc.l2
    0,    0,    0,    0,    0 # diff_ipc.l2
  ),
  nrow = 5,
  ncol = 12 ,
  byrow = FALSE
)

var.r <- vars::restrict( var.a,
                         method = "man",
                         resmat = restrict )

summary( var.r )

coeficientes <- summary( var.r )

Phi <- Acoef(var.r)

Sigma_2 <- coeficientes$covres

roots(var.r, modulus = TRUE) #el modulo es menor que 1, por lo tanto el proceso VAR(p) es estable

# 4. Predicciones ----------------------------------------------------------------------------------
fcast = predict( var.r, n.ahead = 12 * n )

## 4.1. Transformación inversa ---------------------------------------------------------------------

#Construcción de data frame con las predicciones
forecast <- data.frame( 
  diff_pib = rbind( 
    data.frame( diff_pib = data_diff$diff_pib ),
    data.frame( diff_pib = c( fcast$fcst[["diff_pib"]][, 1] ) )
  ) ,
  diff_tasa_pasiva = rbind( 
    data.frame( diff_tasa_pasiva = data_diff$diff_tasa_pasiva ),
    data.frame( diff_tasa_pasiva = c( fcast$fcst[["diff_tasa_pasiva"]][, 1] ) )
  ) ,
  diff_sal_prom = rbind( 
    data.frame( diff_sal_prom = data_diff$diff_sal_prom ),
    data.frame( diff_sal_prom =  fcast$fcst[["diff_sal_prom"]][, 1] )
  ) ,
  diff_sbu = rbind( 
    data.frame( diff_sbu = data_diff$diff_sbu ),
    data.frame( diff_sbu = c( fcast$fcst[["diff_sbu"]][, 1] ) )
  ),
  diff_ipc = rbind( 
    data.frame( diff_ipc = data_diff$diff_ipc ),
    data.frame( diff_ipc = c( fcast$fcst[["diff_ipc"]][, 1] ) )
  )
) %>%
  dplyr::select( diff_pib,
                 #diff_spread,
                 diff_tasa_pasiva,
                 diff_sal_prom,
                 diff_sbu,
                 diff_ipc ) %>%
  na.omit( . )


#Inversa de la diferenciación
pred <- data.frame( 
  pib = diffinv( 
    forecast$diff_pib,
    lag = 1,
    xi = c( tasas_macro$pib[1] )
  ),
  tasa_pasiva = diffinv( 
    forecast$diff_tasa_pasiva ,
    lag = 1,
    xi = c( tasas_macro$tasa_pasiva[1] )
  ),
  sal_prom = diffinv( 
    forecast$diff_sal_prom ,
    lag = 1,
    xi = c( tasas_macro$sal[1] )
  ),
  sbu = diffinv( 
    forecast$diff_sbu ,
    lag = 1,
    xi = c( tasas_macro$sbu[1] )
  ),
  ipc = diffinv( 
    forecast$diff_ipc ,
    lag = 1,
    xi = c( tasas_macro$ipc[1] )
  )
)

#Cálculo de la inflación mensual y anual
pred <- pred %>%
  mutate( inf_anual = 100 * ( ( ipc - dplyr::lag( ipc, 12 ) ) / dplyr::lag( ipc, 12 ) ),
          inf_mensual = 100 * ( ( ipc - dplyr::lag( ipc, 1 ) ) / dplyr::lag( ipc, 1 ) ) )

#Cálculo de la inflación promedio y las fechas
predicciones_mensuales <- pred %>%
  cbind( expand.grid( mes = c( 1:12 ), anio = c( anio_ini:2060 ) ), . ) %>%
  group_by( anio ) %>%
  mutate( inf_prom = mean( inf_anual ) ) %>%
  mutate( pib = pib * 1000000 ) %>%
  ungroup(  ) %>%
  dplyr::select( anio,
                 mes,
                 pib,
                 tasa_pasiva,
                 sal_prom,
                 sbu,
                 ipc,
                 inf_mensual,
                 inf_anual,
                 inf_prom )

## 4.2. Calculo de las tasas de crecimiento anuales ------------------------------------------------
#Cálculo de tasas anuales
predicciones_anuales <- predicciones_mensuales  %>%
  group_by( anio ) %>%
  mutate( 
    pib_anual = sum( pib, na.rm = TRUE ),
    tp_anual = mean( tasa_pasiva, na.rm = TRUE ),
    sal_anual = sum( sal_prom, na.rm = TRUE ),
    ipc_dic = if_else( mes < 12,
                       NA,
                       ipc ),
    inf_anual = inf_prom
  )  %>%
  mutate( sbu_anual = mean( sbu, na.rm = TRUE ) ) %>% 
  mutate( ipc_dic = sum( ipc_dic, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  filter( mes == 6 ) %>%
  dplyr::select( anio,
                 pib_anual,
                 tp_anual,
                 sal_anual,
                 sbu_anual,
                 ipc_dic,
                 inf_anual )

tasas_macro_anuales <- predicciones_anuales %>%
  dplyr::mutate( t_pib = 100 * ( pib_anual - dplyr::lag( pib_anual ) ) / dplyr::lag( pib_anual ) ) %>%
  dplyr::mutate( t_sal = 100 * ( sal_anual - dplyr::lag( sal_anual ) ) / dplyr::lag( sal_anual ) ) %>%
  dplyr::mutate( t_sbu = 100 * ( sbu_anual - dplyr::lag( sbu_anual ) ) / dplyr::lag( sbu_anual ) ) %>%
  dplyr::select( anio, t_pib, t_sal, t_sbu, tp_anual, inf_anual ) %>%
  dplyr::filter( anio >= 2021, anio <= 2060 ) %>% 
  mutate( t_sal = if_else( anio == '2023',
                          3,
                          t_sal ) ) #Debido a hechos subsecuentes, se ha observado que la tasa de crecimiento del salario promedio es 3,96%; pero siguiendo buenas practicas actuariales, se usa la tasa de crecimiento salaial igual a 3% para el 2023.

#Tasas promedio de crecimiento
( t_pib = mean( tasas_macro_anuales$t_pib, na.rm = TRUE ) )
( t_sal = mean( tasas_macro_anuales$t_sal, na.rm = TRUE ) )
( t_sbu = mean( tasas_macro_anuales$t_sbu, na.rm = TRUE ) )
( t_tp = mean( tasas_macro_anuales$tp_anual, na.rm = TRUE ) )
( t_inf = mean( tasas_macro_anuales$inf_anual, na.rm = TRUE ) )

## 4.3. Tabla resumen de parámetros ----------------------------------------------------------------

hipotesis <- tasas_macro_anuales %>%
  dplyr::filter( anio >= 2021, anio <= 2060 ) %>%
  dplyr::mutate( 
    t_pib =  mean( t_pib,   na.rm = TRUE ),
    tp = mean( tp_anual,   na.rm = TRUE ),
    t_sal = mean( t_sal,   na.rm = TRUE ),
    t_sbu = mean( t_sbu,   na.rm = TRUE ),
    inf = mean( inf_anual,   na.rm = TRUE )
  ) %>%
  distinct( t_pib, .keep_all = TRUE ) %>%
  dplyr::select( t_pib, tp, t_sal, t_sbu, inf ) %>%
  gather( ., key = 'hipotesis', value = 'tasas' ) %>%
  mutate( 
    hipotesis = c( 
      'Crecimiento del PIB ( a precios actuales )',
      'Tasa Pasiva Referencial',
      'Crecimiento Salarial',
      'Crecimiento del SBU',
      'Inflación Promedio Acumulada'
    )
  )

## 4.4. Intervalos de confianza --------------------------------------------------------------------

#Cálculo de las phi de la serie original a la serie original

phi <- rep( matrix( 0, ncol( train ), ncol( train ) ), ( 12 * n ) ) 


phi <- vector( "list", 12 * n )
for (i in 1 : ( 12 * n ) ) {
  phi[[i]] <- matrix( 0, nrow = ncol( train ), ncol = ncol( train ) )
}



phi[[1]] <- matrix( 0, nrow = ncol( train ), ncol = ncol( train ) )

phi[[2]] <- Phi[[1]] +  diag( ncol( train ) )

phi[[3]] <- Phi[[2]] - Phi[[1]]

#Cálculo de los pesos psi

psi <- vector( "list", 12 * n )
for (i in 1 : ( 12 * n ) ) {
  psi[[i]] <- matrix( 0, nrow = ncol( train ), ncol = ncol( train ) )
}


for (i in 1 : ( ( 12 * n ) - 1 ) ) {
  psi[[1]] <-  diag( ncol( train ) )
  for ( j in 1:i ) {
    psi[[i + 1]] <- phi[[j + 1]] %*% psi[[i-j+1]] + psi[[i + 1]]
  }
  
}

#Calculo de la matriz de covarianzas del error predicho
cov <- vector( "list", 12 * n )

for ( i in 1: ( ( 12 * n ) - 1 ) ) {
 
  cov[[1]] <-  Sigma_2
  cov[[i + 1]] <- psi[[i + 1]] %*%  Sigma_2 %*% t( psi[[i + 1]] ) + cov[[i]]
  
}

sigma <- matrix( 0, 12 * n, ncol( train ) )

for ( i in 1: ( ( 12 * n ) ) ) {
  
  sigma[i,] <- diag( sqrt( cov[[i]] ) )
  
}

sigma <- as.data.frame( sigma ) %>% 
  rename( "sigma_pib" = "V1",
          "sigma_tasa_pasiva" = "V2",
          "sigma_sal_prom" = "V3",
          "sigma_sbu" = "V4",
          "sigma_ipc" = "V5" ) %>% 
  cbind( expand.grid( mes = c( 1:12 ), anio = c( 2023:( 2022 + n ) ) ),
         . )

#Construcción de intervalos de confianza

intervalos_confianza <- cbind( expand.grid( mes = c( 1:12 ), anio = c( anio_ini:( 2022 + n ) ) ),
                               pred ) %>%
  dplyr::select( anio, mes, pib, tasa_pasiva, sal_prom, sbu, ipc ) %>% 
  left_join( ., sigma, by = c( 'anio', 'mes' ) ) %>% 
  mutate( pib = 1000000 * pib ) %>% 
  mutate( lim_inf_pib = pib - 1000000 * qnorm( 1 - alpha/2 ) * sigma_pib,
          lim_sup_pib = pib + 1000000 * qnorm( 1 - alpha/2 ) * sigma_pib,
          
          lim_inf_tasa_pasiva = tasa_pasiva - qnorm( 1 - alpha/2 ) * sigma_tasa_pasiva,
          lim_sup_tasa_pasiva = tasa_pasiva + qnorm( 1 - alpha/2 ) * sigma_tasa_pasiva,
          
          lim_inf_sal_prom = sal_prom - qnorm( 1 - alpha/2 ) * sigma_sal_prom,
          lim_sup_sal_prom = sal_prom + qnorm( 1 - alpha/2 ) * sigma_sal_prom,
          
          lim_inf_sbu = sbu - qnorm( 1 - alpha/2 ) * sigma_sbu,
          lim_sup_sbu = sbu + qnorm( 1 - alpha/2 ) * sigma_sbu,
          
          lim_inf_ipc = ipc - qnorm( 1 - alpha/2 ) * sigma_ipc,
          lim_sup_ipc = ipc + qnorm( 1 - alpha/2 ) * sigma_ipc
          ) 
# %>%
#   mutate( lim_sup_inf = 100 * ( ( ipc - dplyr::lag( lim_sup_ipc, 1 ) ) / dplyr::lag( lim_sup_ipc, 1 ) ),
#           lim_inf_inf = 100 * ( ( ipc - dplyr::lag( lim_inf_ipc, 1 ) ) / dplyr::lag( lim_inf_ipc, 1 ) ),
#           )
#   

# 5. Gráficos de control ---------------------------------------------------------------------------
g <- predicciones_mensuales %>%
  dplyr::select( pib, tasa_pasiva , sal_prom , ipc, inf_prom, sbu )

autoplot( ts( g$pib,
              start = c( anio_ini, 1 ),
              frequency = 12 ),
          type = "l",
          ylab = "pib" )

autoplot( ts( 
  g$sal_prom,
  start = c( anio_ini, 1 ),
  frequency = 12
) ,
type = "l",
ylab = "salario anual" )

autoplot( ts( g$sbu,
              start = c( anio_ini, 1 ),
              frequency = 12 ) ,
          type = "l",
          ylab = "sbu" )

autoplot( ts( 
  g$tasa_pasiva,
  start = c( anio_ini, 1 ),
  frequency = 12
),
type = "l",
ylab = "tasa pasiva" )

autoplot( ts( 
  g$inf_prom,
  start = c( anio_ini, 1 ),
  frequency = 12
),
type = "l",
ylab = "inf_prom" )


# 6. Pruebas de diagnostico ------------------------------------------------------------------------

residuos <- as.data.frame( resid( var.r ) )


## 6.1. Prueba de independencia de residuos --------------------------------------------------------

#H0: Independencia de los residuos | Si p>0.05 no se rechaza la H0

box_ljung_pib <-
  portes::portest( residuos$diff_pib, test = "LjungBox", lags = c( 1:20 ) )
box_ljung_sbu <-
  portes::portest( residuos$diff_sbu, test = "LjungBox", lags = c( 1:20 ) )
box_ljung_sal <-
  portes::portest( residuos$diff_sal_prom,
                   test = "Ljung",
                   lags = c( 1:20 ) )
box_ljung_tp <-
  portes::portest( residuos$diff_tasa_pasiva,
                   test = "LjungBox",
                   lags = c( 1:20 ) )
boc_ljung_ipc <-
  portes::portest( residuos$diff_ipc, test = "LjungBox", lags = c( 1:20 ) )


box_ljung <-
  mq_aumentada( residuos, lag = 10,  adj = 0 ) %>%
  `colnames<-`( c( "m", "Q", "df", "p_valor" ) )


## 6.2. Prueba de normalidad de los residuos -------------------------------------------------------
#H0: La variable sigue una distribución normal | Si p>0.05 no se rechaza la H0
#Jarque-Bera test
jarque_bera <-
  vars::normality.test( var.r, multivariate.only = FALSE )

jarque_bera_test <- data.frame( 
  variable = c( 
    '$\\nabla$ PIB',
    '$\\nabla$ Tasa pasiva',
    '$\\nabla$ Salarios Promedio',
    '$\\nabla$ SBU',
    '$\\nabla$ IPC'
  ),
  estadistico_JB = c( 
    jarque_bera$jb.uni$diff_pib$statistic,
    jarque_bera$jb.uni$diff_tasa_pasiva$statistic,
    jarque_bera$jb.uni$diff_sal_prom$statistic,
    jarque_bera$jb.uni$diff_sbu$statistic,
    jarque_bera$jb.uni$diff_ipc$statistic
  ),
  p_valor = c( 
    jarque_bera$jb.uni$diff_pib$p.value,
    jarque_bera$jb.uni$diff_tasa_pasiva$p.value,
    jarque_bera$jb.uni$diff_sal_prom$p.value,
    jarque_bera$jb.uni$diff_sbu$p.value,
    jarque_bera$jb.uni$diff_ipc$p.value
  )
)

## 6.3. Pruebas de homocedasticidad ----------------------------------------------------------------

#H0: Las desviaciones no tienen heterocedasticidad condicional | Si p>0.05 no se rechaza la H0

homocedasticidad <- MarchTest( residuos )

homocedasticidad <- data.frame( 
  prueba = c( 
    'Test LM',
    'Test basado en rango',
    '$Q_{k}(   m   )$ de la serie al cuadrado',
    'Test robustez (   5\\%   )'
  ),
  estadistico = c( 
    homocedasticidad$Q,
    homocedasticidad$QR,
    homocedasticidad$qm,
    homocedasticidad$qm2
  ),
  p_valor = c( 
    homocedasticidad$pv1,
    homocedasticidad$pv2,
    homocedasticidad$pv3,
    homocedasticidad$pv4
  )
)

## 6.4. Prueba de multicolinealidad ----------------------------------------------------------------

correlaciones <- ppcor::pcor( train, method = "kendall" )
ma_correlaciones <- data.frame( 
  variable = c( 
    '$\\nabla$ PIB',
    '$\\nabla$ Tasa pasiva',
    '$\\nabla$ Salarios promedio',
    '$\\nabla$ SBU',
    '$\\nabla$ IPC'
  ),
  correlaciones$estimate
)
ma_multi_p_valores <- data.frame( 
  variable = c( 
    '$\\nabla$ PIB',
    '$\\nabla$ Tasa pasiva',
    '$\\nabla$ Salarios Promedio',
    '$\\nabla$ SBU',
    '$\\nabla$ IPC'
  ),
  correlaciones$p.value
)

## 6.5 Prueba de nulidad de los coeficientes del VAR( 2 ) ------------------------------------------

#H0: Nulidad de los coeficientes

test_nulidad_nulidad = VARchi( train, p = 1, th = 1.96 )

test_nulidad_nulidad <- data.frame( 
  concepto = c( 'Estadístico',
                'P-valor' ),
  valor = c( test_nulidad_nulidad$chi,
             test_nulidad_nulidad$pvalue )
)


# 7. Guardar en un RData ---------------------------------------------------------------------------
message( '\tGuardando tasas macro' )

save( 
  tasas_macro,
  dickey_fuller,
  hipotesis,
  predicciones_anuales,
  tasas_macro_anuales,
  predicciones_mensuales,
  box_ljung_pib,
  box_ljung_sbu,
  box_ljung_sal,
  box_ljung_tp,
  boc_ljung_ipc,
  jarque_bera_test,
  homocedasticidad,
  ma_correlaciones,
  ma_multi_p_valores,
  test_nulidad_nulidad,
  box_ljung,
  coeficientes,
  intervalos_confianza,
  file = parametros$macro_rdata_macro_est
)

# 8. Exportar a excel ------------------------------------------------------------------------------

# list_of_datasets <- list( "hipotesis_anuales" = predicciones_anuales, "tasa_hipotesis_anuales" = tasas_macro_anuales )
# 
# openxlsx::write.xlsx(  list_of_datasets,
#                       file = paste0(  parametros$resultado_seguro , 'IESS_macro_estudio.xlsx'  ),
#                       overwrite = TRUE  )

# 9. Borrar elementos restantes --------------------------------------------------------------------
message( paste( rep( "-", 100 ), collapse = "" ) )
rm( list = ls(  )[!( ls(  ) %in% c( "parametros" ) )] )
gc(  )
