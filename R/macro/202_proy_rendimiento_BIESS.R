message( paste( rep( "-", 100 ), collapse = "" ) )

# 1. Cargando Datos---------------------------------------------------------------------------------
message( "\tCargando tasas macro" )

load( parametros$macro_rdata_info )

## 1.1. Parámetros----------------------------------------------------------------------------------

n = 38 #Número de años a predecir
anio_ini = 2012 # Año inicial

# Cargar funciones----------------------------------------------------------------------------------
source( 'R/macro/003_carga_funciones.R', encoding = 'UTF-8', echo = FALSE )

#2. Preparando data --------------------------------------------------------------------------------

tasa_biess <- rendimiento_biess %>% 
  mutate( anio = year( fecha ),
          mes = month( fecha ),
          rendimiento = 100 * rendimiento ) %>% 
  dplyr::select( anio, mes, rendimiento ) %>% 
  filter( anio >= anio_ini )

##2.1. Pruebas de estacionariedad de Dickey-Fuller--------------------------------------------------
#Ho: La serie de tiempo es no estacionaria | Si p<0.05 se rechaza la H0
aux_1 <- adfTest( diff( tasa_biess$rendimiento, differences = 1 ) )

dickey_fuller_biess <- data.frame( 
  variable = c( 
    '$\\nabla$ Tasa rendimiento BIESS'
  ),
  estadistico = c( 
    aux_1@test$statistic
  ),
  p_valor = c( 
    aux_1@test$p.value
  )
)

## 2.2. Diferenciación de la series-----------------------------------------------------------------
data_diff <- tasa_biess %>%
  na.omit( . ) %>%
  mutate( 
    diff_biess = c( NA,  base::diff( rendimiento, 1 ) )
    ) %>%
  dplyr::select( diff_biess )


boxplot( data_diff$diff_biess )$out
plot.ts( data_diff$diff_biess )
shapiro.test( data_diff$diff_biess )
## 2.3. Tratamiento de outliers---------------------------------------------------------------------
data_diff_out <- data_diff %>%
  na.omit( . ) %>%
  mutate( 
    diff_biess = outliers( diff_biess )
    )

boxplot( data_diff_out$diff_biess )
plot.ts( data_diff_out$diff_biess )
shapiro.test( data_diff_out$diff_biess )

## 2.4. Conjunto de entrenamiento-------------------------------------------------------------------
train <- data_diff_out

# 3. Estimación del modelo--------------------------------------------------------------------------
#ARIMAfit = auto.arima( data_diff_out$diff_biess, approximation=FALSE,trace=FALSE)
#summary(ARIMAfit)

acf( train )
pacf( train )

m <- smoots::critMatrix(
  ts( train ),
  p.max = 5,
  q.max = 5,
  criterion = c("aic"),
  include.mean = TRUE
)

#a <- weakARMA::ARMA.selec(ts( train$diff_biess ), P = 5, Q = 5, c = 0) #usando el criterio de BIC se elige ARMA(2,0)



(mod_biess <- forecast::Arima( data_diff_out$diff_biess,
                               order=c( 2, 0, 0 ),
                               fixed = c( NA, NA ),
                               include.mean = FALSE
) )

coeficientes_biess <- test_nulidad_arma( mod_biess )

se_mod_biess <- sqrt( mod_biess$sigma2 )

Phi <- c( mod_biess$coef ) * ( -1 )

sigma_2 <- ( mod_biess$sigma2 )

plot(mod_biess) # todas las raíces características  están dentro del circulo; por lo tanto el modelo es estacionario e invertible

#Pruebas de diagnostico-----------------------------------------------------------------------------
residuos_biess <- mod_biess$residuals %>%  na.omit( . )

#Prueba de normalidad
aux <- shapiro.test( residuos_biess )

shapiro_test_biess <- data.frame( 
  variable = c( 
    '$\\nabla$ Rendimiento neto BIESS' ),
  estadistico_W = c( 
    aux$statistic
  ),
  p_valor = c( 
    aux$p.value )
)

#Prueba de independencia

box_ljung_biess <-
  portes::portest( residuos_biess,
                   test = "LjungBox",
                   lags = c( 1:20 ) )

#Pruebas de heterocesatcidad------------------------------------------------------------------------
#Prueba portmanteau
portmanteau_biess <- portes::portest( residuos_biess^2, test = "LjungBox", lags = c( 1:20 ) )

#4. Predicciones------------------------------------------------------------------------------------
fcast <- forecast::forecast( mod_biess, h = 12 * n )

#Gráfico de control---------------------------------------------------------------------------------
ts.plot( fcast$mean,
         fcast$lower,
         fcast$upper )

#4.1. Transformación inversa------------------------------------------------------------------------

#Construcción de data frame con las predicciones
diff_forecast <- data.frame( 
  diff_rendimiento_biess = rbind( 
    data.frame( diff_rendimiento_biess = c( data_diff$diff_biess ) ),
    data.frame( diff_rendimiento_biess = c( fcast$mean ) ) )
  ) %>% 
  na.omit( . )


#Inversa de la diferenciación-----------------------------------------------------------------------
forecast <- data.frame( 
  rendimiento_biess = diffinv( 
    c( diff_forecast$diff_rendimiento_biess ) ,
    lag = 1,
    xi = c( tasa_biess$rendimiento[1] )
  )
  ) 

plot.ts( forecast$rendimiento_biess )

#Intervalos de confianza----------------------------------------------------------------------------

#Cálculo de las phi de la serie original

phi <- c( rep( 0, ( 12 * n ) ) )

phi[1] <- 0

phi[2] <- c( Phi[1] ) + 1 

phi[3] <- Phi[2] - Phi[1]

#Cálculo de los pesos psi

psi <- c( rep( 0, ( 12 * n ) ) )


for (i in 1 : ( (12*n) - 1 ) ) {
  psi[ 1 ] <- 1
  for ( j in 1:i ) {
    psi[i + 1] <- phi[j + 1] * psi[i-j+1] + psi[i + 1]
  }
  
}

#Construcción de Intervalos 

forecast <- forecast %>% 
  mutate( psi = c( rep( 0, nrow( tasa_biess ) ), psi ) ) %>% 
  mutate( sum_psi = cumsum( psi ) ) %>% 
  mutate( V = sum_psi * sigma_2 ) %>% 
  mutate( lim_inf = lag( rendimiento_biess ) - qnorm(1-.05/2) * sqrt( V ),
          lim_sup = lag( rendimiento_biess ) + qnorm(1-.05/2) * sqrt( V ) ) %>%
  mutate( lim_inf = ifelse( psi > 0, 
                            lim_inf,
                            NA ),
          lim_sup = ifelse( psi > 0, 
                            lim_sup,
                            NA ) )

#Gráfico de control
ts.plot( ts(forecast$rendimiento_biess),
         ts(forecast$lim_inf),
         ts(forecast$lim_sup ) )


#Tasas promedio
( t_biess = mean( forecast$rendimiento_biess, na.rm = TRUE ) )
( t_biess_inf = mean( forecast$lim_inf, na.rm = TRUE ) )
( t_biess_sup = mean( forecast$lim_sup, na.rm = TRUE ) )

#Predicciones mensuales

predicciones_mensuales_biess <-   cbind( expand.grid( mes = c( 1:12 ), anio = c( anio_ini:2060 ) ),
                                   forecast ) %>% 
  mutate( fecha = as.Date( paste0( anio, "-", mes, "-01", "%Y-%m-%d" ) ) ) %>% 
  dplyr::select( mes, anio, fecha, rendimiento_biess, lim_inf, lim_sup )

# 7. Guardar en un RData----------------------------------------------------------------------------
message( '\tGuardando tasas macro' )

save( 
  shapiro_test_biess,
  dickey_fuller_biess,
  box_ljung_biess,
  coeficientes_biess,
  predicciones_mensuales_biess,
  portmanteau_biess,
  se_mod_biess,
  file = parametros$macro_rdata_biess_proy
)

# 8. Exportar a excel-------------------------------------------------------------------------------

# list_of_datasets <- list( "predicciones_mensuales" = predicciones_mensuales )
#
# openxlsx::write.xlsx(  list_of_datasets,
#                       file = paste0(  parametros$resultado_seguro , 'BIESS_proy_tasa_rendimiento.xlsx'  ),
#                       overwrite = TRUE  )

#9. Borrar elementos restantes --------------------------------------------------------------------
message( paste( rep( "-", 100 ), collapse = "" ) )
rm( list = ls(  )[!( ls(  ) %in% c( "parametros" ) )] )
gc(  )
