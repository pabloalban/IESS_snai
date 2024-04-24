# Para estimar mux ---------------------------------------------------------------------------------
estimador <- function(temp00, xmin, tmin, tmax, w){
  # Preprando base
  temp01 <- temp00 %>% 
    filter(x >= xmin, t >= tmin, t <= tmax) %>% 
    group_by(SEXO, x) %>% 
    summarise(Ex = sum(Ex), dx = sum(dx)) %>% 
    ungroup()
  # Menor o igual a w
  temp02 <- temp01 %>% filter(x <= w)
  # Mayor o igual a w
  temp03 <- temp01 %>% filter(x >= w) %>% group_by(SEXO) %>% summarise(Ex = sum(Ex), dx = sum(dx))
  # Ew para H
  temp02$Ex[temp02$SEXO == 'H' & temp02$x == w] <- temp03$Ex[temp03$SEXO == 'H']
  # Ew para M
  temp02$Ex[temp02$SEXO == 'M' & temp02$x == w] <- temp03$Ex[temp03$SEXO == 'M']
  # Ew para T
  temp02$Ex[temp02$SEXO == 'T' & temp02$x == w] <- temp03$Ex[temp03$SEXO == 'T']
  # dw para H
  temp02$dx[temp02$SEXO == 'H' & temp02$x == w] <- temp03$dx[temp03$SEXO == 'H']
  # dw para M
  temp02$dx[temp02$SEXO == 'M' & temp02$x == w] <- temp03$dx[temp03$SEXO == 'M']
  # dw para T
  temp02$dx[temp02$SEXO == 'T' & temp02$x == w] <- temp03$dx[temp03$SEXO == 'T']
  # Base final
  temp04 <- temp02 %>% mutate(mux = dx/Ex, qx = 1 - exp(-mux)) %>% as.data.frame()
  # Ajuste final
  temp04$mux[temp04$Ex == 0 & temp04$dx > 0] <- NA
  temp04$qx[temp04$Ex == 0 & temp04$dx > 0] <- NA
  # Return
  return(structure(temp04))
}

# Definición de modelos ----------------------------------------------------------------------------
model_definition <- function( data, boundary_knots = NULL, knots = NULL ) {
  
  if ( is.null( boundary_knots ) ) {
    boundary_knots <- range( data$x )
  }
  
  list_models <- list()
  
  n = 0
  for ( i in 1:length( knots ) ) {
    
    expr <- substitute( 
      expression({ 
        frml <- formula( log( ux ) ~ bs( 
          x, 
          degree = 2, 
          Boundary.knots = bound_knots,
          knots = given_knots ) )
      }), 
      list( bound_knots = boundary_knots, 
            given_knots = knots[[i]] ) )
    eval( eval( expr ) )
    
    # Spline de orden 2
    list_models[[ n + i ]] <- list(
      type = 'csp',
      modname = paste0( 'csp_ord_2_knots' ), 
      data = data, 
      formula = frml,
      singular.ok = TRUE )
    
  }
  
  n = length( list_models )
  for ( i in 1:length( knots ) ) {
    
    expr <- substitute( 
      expression({ 
        frml <- formula( log( ux ) ~ bs( 
          x, 
          degree = 3, 
          Boundary.knots = bound_knots,
          knots = given_knots ) )
      }), 
      list( bound_knots = boundary_knots, 
            given_knots = knots[[i]] ) )
    eval( eval( expr ) )
    
    # Spline de orden 3
    list_models[[ n + i ]] <- list(
      type = 'csp',
      modname = paste0( 'csp_ord_3_knots' ), 
      data = data, 
      formula = frml,
      singular.ok = TRUE )
    
  }
  
  n = length( list_models )
  for ( i in 1:length( knots ) ) {
    
    expr <- substitute(
      expression({
        frml <- formula( log( ux ) ~ bs(
          x,
          degree = 3,
          Boundary.knots = bound_knots,
          knots = given_knots ) )
      }),
      list( bound_knots = boundary_knots,
            given_knots = knots[[i]] ) )
    eval( eval( expr ) )
    
    # Spline de orden 3
    list_models[[ n + i ]] <- list(
      type = 'csp',
      modname = paste0( 'csp_ord_3_wER_knots' ),
      data = data,
      formula = frml,
      weights = as.data.table( data )$ERx + 1/365,
      singular.ok = TRUE )
    
  }
  
  n = length( list_models )
  
  # GLM, regresión de Poisson, edad numérica
  list_models[[ n + 1 ]] <-  list( 
    type = 'glm',
    modname = 'rp_x', 
    data = data, 
    formula = formula( 'Nx ~ 0 + x + offset( log( ERx ) )' ), 
    family = poisson( link = "log" ),
    control = glm.control( epsilon = 1e-10, maxit = 1000 ) )
  
  # GLM, regresión de Poisson, edad como factor
  list_models[[ n + 2 ]] <-  list(
    type = 'glm',
    modname = 'rp_xf', 
    data = data, 
    formula = formula( 'Nx ~ 0 + xf + offset( log( ERx ) )' ), 
    family = poisson( link = "log" ),
    control = glm.control( epsilon = 1e-10, maxit = 1000 ) )
  
  # GLM, regresión de Poisson, edad como factor, pesos ER
  list_models[[ n + 3 ]] <-  list(
    type = 'glm',
    modname = 'rp_wER_xf', 
    data = data, 
    formula = formula( 'Nx ~ 0 + xf + offset( log( ERx ) )' ), 
    family = poisson( link = "log" ),
    control = glm.control( epsilon = 1e-10, maxit = 1000 ), 
    weights = as.data.table( data )$ERx )
  
  return( list_models )
}

# Estimación y selección del modelo ----------------------------------------------------------------
model_selection <- function( list_models ) {
  
  # Cálculo de cada uno de los modelos de ajuste
  summ_results <- NULL
  list_results <- list()
  for ( i in 1:length( list_models ) ) {
    
    cat( '\r\tProcesando ', i, ' de ', length( list_models ), ' modelos' )
    
    args <- names( list_models[[i]] )
    
    if ( list_models[[ i ]]$type == 'csp' ) {
      
      args <- args[ !( args %in% c( 'type', 'modname' ) ) ]
      list_results[[ i ]] <- do.call( lm, args = list_models[[ i ]][ args ] )
      
    } else if ( list_models[[ i ]]$type == 'glm' ) {
      
      args <- args[ !( args %in% c( 'type', 'name' ) ) ]
      list_results[[ i ]] <- do.call( glm, args = list_models[[ i ]][ args ] )
      
    }
    
    list_results[[ i ]][ 'type' ] <- list_models[[ i ]][ 'type' ]
    list_results[[ i ]][ 'modname' ] <- list_models[[ i ]][ 'modname' ]
    
    # Comparasión de residuos y calidad del modelo
    summ_results <- rbindlist( list(
      summ_results,
      data.table( 
        'type' = list_results[[ i ]]$type,
        'modname' = list_results[[ i ]]$modname,
        'error' = sqrt( sum( residuals( list_results[[ i ]] )^2, na.rm = TRUE ) ),
        'df_residual' = df.residual( list_results[[ i ]] ),
        'loglik' = as.numeric( logLik( list_results[[ i ]] ) ),
        'AIC' = AIC( list_results[[ i ]] ),
        'BIC' = BIC( list_results[[ i ]] ) )
    ) )
    
  }
  
  summ_results[ , rellik_AIC := exp( ( min( AIC ) - AIC ) / 2 ) ]
  summ_results[ , rellik_BIC := exp( ( min( BIC ) - BIC ) / 2 ) ]
  
  return( list( 'summary' = summ_results, 'results' = list_results ) )
  
}
model_selection <- cmpfun( 
  model_selection, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

# Tabla biometrica ---------------------------------------------------------------------------------
biometric_table <- function( data, model, result ) {
  
  # Creacion tabla
  bm <- copy( data )
  aux <- as.data.table( model$data )
  aux <- aux[ , list( x, ERx, Nx, ux_obs = ux ) ]
  bm <- merge.data.table( data, aux, by = 'x', all.x = TRUE )
  
  bm[ , log_ux := predict( result, newdata = data ) ]
  bm[ , ux := exp( log_ux ) ]
  bm[ , log_ux := NULL ]
  bm[ , px := exp( -ux ) ]
  bm[ , qx := 1 - px ]
  
  # Creacion lx
  bm[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( id, sexo ) ]
  bm[ , lx := 1e5 * cumprod( lx ), by = list( id, sexo ) ]
  
  # Creacion dx
  bm[ , dx := lx * qx ]
  
  # Creacion ex
  bm[ , ex := rev( cumsum( rev( lx ) ) ), by = list( id, sexo ) ]
  bm[ , ex := ex / lx - 0.5 ]
  
  # Salida
  return( structure( bm ) )
}
biometric_table <- cmpfun( 
  biometric_table, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

# Métodos de completación de para curvas de tasas --------------------------------------------------
CompletionDG2005 <- function( q, x, y, RangeStart, RangeCompletion, NameMethod, omega ){
  
  x <- min(x):pmin(max(x),100)
  RangeStart <- RangeStart - min(x)
  RangeCompletion <- RangeCompletion - min( x )
  CompletionValues <- matrix( , length(y), 3 )
  rownames(CompletionValues) <- y
  colnames(CompletionValues) <- c("OptAge", "OptR2", "OptCt")
  CompletionMat <- matrix(, RangeCompletion[2] - RangeCompletion[1] + 1, length( y ) )
  QxtFinal <- matrix(, RangeCompletion[2] + 1, length( y ) )
  colnames( QxtFinal ) <- y
  rownames( QxtFinal ) <- min(x):omega
  
  for( j in 1:length(y) ) {
    
    R2Mat <- matrix(,RangeStart[2] - RangeStart[1] + 1, 2)
    colnames(R2Mat) <- c("Age", "R2")
    quadratic.q <- vector("list", RangeStart[2] - RangeStart[1] + 1)
    
    for ( i in 0:( RangeStart[2] - RangeStart[1] ) ){
      
      AgeVec <- ((RangeStart[1] + i):(max(x) - min(x)))
      R2Mat[i + 1, 1] <- AgeVec[1]
      FitLM <- lm(log(q[AgeVec + 1, j]) ~ AgeVec + I(AgeVec^2))
      R2Mat[i + 1, 2] <- summary(FitLM)$adj.r.squared
      quadratic.q[[i + 1]] <- as.vector(exp(fitted(FitLM)))
      
    }
    
    if( any( is.na( R2Mat[,2] ) ) == F ){
      
      OptR2 <- max( R2Mat[, 2], na.rm = T )
      OptAge <- as.numeric( R2Mat[ which( R2Mat[, 2] == OptR2 ), 1 ] )
      quadratic.q.opt <- quadratic.q[[ which(R2Mat[, 2]==OptR2 ) ]]
      
    }
    
    if( any( is.na( R2Mat[,2] ) ) ) {
      
      OptR2 <- NA
      OptAge <- AgeVec[1]
      quadratic.q.opt <- quadratic.q[[i + 1]]
      
    }
    
    OptCt <- coef( lm( log( quadratic.q.opt ) ~ I( ( RangeCompletion[2] -(OptAge : (max(x) - min(x))))^2) - 1))
    CompletionVal <- vector(, RangeCompletion[2] - RangeCompletion[1] + 1 )
    
    for ( i in 0:( RangeCompletion[2] - RangeCompletion[1] ) ){
      
      CompletionVal[i + 1] <- exp( OptCt * ( RangeCompletion[2] - ( RangeCompletion[1] + i ) )^2 )
      
    }
    CompletionValues[j, 1] <- OptAge + min(x)
    CompletionValues[j, 2] <- OptR2
    CompletionValues[j, 3] <- OptCt
    CompletionMat[, j] <- CompletionVal
    QxtFinal[, j] <- c(q[1 : RangeCompletion[1], j], CompletionVal)
    k <- 5
    QxtSmooth <- vector(,2 * k + 1)
    for( i in ( RangeCompletion[1] - k ):( RangeCompletion[1] + k ) ) {
      
      QxtSmooth[1 + i - (RangeCompletion[1] - k)] <- prod(QxtFinal[(i - k):(i + k), j])^(1 / (2 * k + 1))
      
    }
    QxtFinal[(RangeCompletion[1] - k):(RangeCompletion[1] + k), j] <- QxtSmooth
  }
  return( list( CompletionValues = CompletionValues,
                CompletionMat = CompletionMat,
                QxtFinal = QxtFinal,
                NameMethod = NameMethod ) )
}

curve_completion <- function( data, range, x0, dx0, y0, dy0, inflex_x, ymax, dymax, int, right = TRUE, mono_chk = TRUE ) {
  
  x <- seq( range[1], range[2], 1 )
  if ( is.null( y0 ) & !is.null( ymax ) ) {
    
    if ( right ) {
      
      y0 <- c( log( data[ x == range[ 1 ] ]$ux ), 
               log( -log( ymax ) ) )
      
    } else {
      
      y0 <- c( log( -log( ymax ) ), 
               log( data[ x == range[ 2 ] ]$ux ) )
      
    }
    
    
  } else if ( is.null( y0 ) & is.null( ymax ) ) {
    
    y0 <- c( log( data[ x == range[ 1 ] ]$ux ), 
             log( data[ x == range[ 2 ] ]$ux ) )
    
  }
  
  if ( is.null( dy0 ) ) {
    
    if ( right ) {
      
      dy0 <- c( mean( diff( log( data[ x >= range[ 1 ] + int[ 1 ] & x <= range[ 1 ] + int[ 2 ] ]$ux_obs ) ), na.rm = TRUE ), dymax )
      
    } else {
      
      dy0 <- c( dymax, mean( diff( log( data[ x >= range[ 2 ] + int[ 1 ] & x <= range[ 2 ] + int[ 2 ] ]$ux_obs ) ), na.rm = TRUE ) )
      
    }
    
  }
  
  n <- length( x0 ) + length( dx0 )
  if ( n == length( y0 ) + length( dy0 ) ) {
    A <- rbind(
      t( sapply( x0, FUN = function( x ) x^(0:n) ) ),
      t( sapply( dx0, FUN = function( x ) 0:n * x^( c( 0, 0:(n-1) ) ) ) ),
      t( sapply( inflex_x, FUN = function( x ) 0:n * c( 0, 0:(n-1) ) * x^( c( 0, 0, 0:(n-2) ) ) ) ) 
    )
    b <- c( y0, dy0, 0 )
    a <- solve( A, b )
    y <- function( x ) return( sapply( x, function( x ) return( sum( a * x^(0:n) ) ) ) )
    
    dat <- copy( data )
    dat[ , ux_sua := ux ]
    dat[ , px_sua := px ]
    dat[ , qx_sua := qx ]
    
    dat[ x >= range[ 1 ] & x <= range[ 2 ], ux := exp( y( x ) ) ]
    dat[ , px := exp( -ux ) ]
    dat[ , qx := pmax( 1 - px, 0 ) ]
    
    if( mono_chk & !all( diff( dat[ x >= max( range[ 1 ] - 2, 0 ), ux ] ) >= 0 ) ) {
      
      message( 'La solución no mantiene la monotonía' )
      
    }
    
    # Creacion lx
    dat[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( id, sexo ) ]
    dat[ , lx := 1e5 * cumprod( lx ), by = list( id, sexo ) ]
    
    # Creacion dx
    dat[ , dx := lx * qx ]
    
    # Creacion ex
    dat[ , ex := rev( cumsum( rev( lx ) ) ), by = list( id, sexo ) ]
    dat[ , ex := ex / lx - 0.5 ]
    
    return( dat )
    
  } else {
    
    return( NULL )
    
  }
}
curve_completion <- cmpfun( 
  curve_completion, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

# Para proyectar mortalidad ------------------------------------------------------------------------
dinamica <- function(v_x, q_x, x){
  # Periodo
  t <- colnames(v_x)
  # Matriz
  Q_x <- matrix(data = NA, nrow = length(x), ncol = ncol(v_x) + 1)
  # Filas
  rownames(Q_x) <- x
  # Columnas
  colnames(Q_x) <- c(2020,as.numeric(colnames(v_x))+1)
  # Base
  Q_x[, "2020"] <- q_x
  # Dinamica
  for(k in 2021:2099){
    Q_x[, as.character(k)] <- v_x[, as.character(k-1)]*Q_x[, as.character(k-1)]
  }
  # Errores
  Q_x[Q_x>1] <- 1
  # Salida
  return(structure(Q_x))
}

# Para cambiar dias de servicio por 1 o 0 -----------------------------------------------------
dias_servicio <- function(data){
  # Reemplaza NA por 0
  data <- mutate_all(data, ~replace(., is.na(.), 0))
  # Solo servicio
  temp <- data[, 5:136]
  # Cambia dias por 1 o 0
  temp <- mutate_all(temp, ~replace(., .>0, 1))
  # Base
  data <- cbind(data[,1:4], temp)
  # Return
  return(structure(data))
}


# Cálculo de exposición al riesgo de transiciones --------------------------------------------------
comp_expo_riesgo <- function ( x, factor = 365 ) {
  return( sum( x, na.rm = TRUE ) / factor )
}
comp_expo_riesgo <- cmpfun( 
  comp_expo_riesgo, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

# Funciones para cálculo de transiciones -----------------------------------------------------------
count_trans_out <- function( x ) {
  y <- x
  y[ is.na( y ) ] <- 0
  y[ y <= 0 ] <- 0
  y[ y > 0 ] <- 1
  return( sum( diff( y ) < 0 ) )
}
## Compilación de la función para mejorar su rendimiento
count_trans_out <- cmpfun( 
  count_trans_out, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

count_trans_ing <- function( x ) {
  y <- x
  y[ is.na( y ) ] <- 0
  y[ y <= 0 ] <- 0
  y[ y > 0 ] <- 1
  return( sum( diff( y ) > 0 ) )
}
## Compilación de la función para mejorar su rendimiento
count_trans_ing <- cmpfun( 
  count_trans_ing, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

count_trans_prim_ing <- function( x ) {
  y <- x
  y[ is.na( y ) ] <- 0
  y[ y <= 0 ] <- 0
  y[ y > 0 ] <- 1
  z <- as.numeric( ( sum( diff( y ) > 0 ) > 0 ) & ( y[1] == 0 ) )
  return( z )
}
## Compilación de la función para mejorar su rendimiento
count_trans_prim_ing <- cmpfun( 
  count_trans_prim_ing, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

count_trans_pen_ing_sal <- function( x ) {
  y <- x
  y[ is.na( y ) ] <- 0
  y[ y <= 0 ] <- 0
  y[ y > 0 ] <- 1
  z <- as.numeric( ( y[ length(y) ] == 1 ) & ( y[1] == 1 ) )
  return( z )
}
## Compilación de la función para mejorar su rendimiento
count_trans_pen_ing_sal <- cmpfun( 
  count_trans_pen_ing_sal, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

count_pen_ing_med_sal <- function( x ) {
  y <- x
  y[ is.na( y ) ] <- 0
  y[ y <= 0 ] <- 0
  y[ y > 0 ] <- 1
  
  if( sum( y )==0 ){
    z <- as.numeric( -1 )
  } 
  if( sum( y ) > 0 ){
    z <- as.numeric( ( y[ length(y) ] == 1 ) & ( y[1] == 0 ) ) * as.numeric( 1 - ( which(y == 1)[1]-1)/12 )
    #z <- as.numeric(1)
  }
  return( z )
}
## Compilación de la función para mejorar su rendimiento
count_pen_ing_med_sal <- cmpfun( 
  count_pen_ing_med_sal, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

count_pen_mes <- function( x ) {
  y <- x
  y[ is.na( y ) ] <- 0
  y[ y <= 0 ] <- 0
  y[ y > 0 ] <- 1
  
  if( sum( y )==0 ){
    z <- as.numeric( -1 )
  } 
  if( sum( y ) > 0 ){
    z <- as.numeric( which(y == 1)[1] )
  }
  return( z )
}
## Compilación de la función para mejorar su rendimiento
count_pen_mes <- cmpfun( 
  count_pen_mes, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

# Función indicatriz -------------------------------------------------------------------------------
# Determina si una transición se relación mientras estaba activo
indicador_transicion <- function( years, field, data ) {
  
  # Esta expresión asume que data es el nombre de un objeto global
  expr <- expression({
    
    # Selección del primer campo posterior al inicio de los campos que contienen todas las 
    # exposiciones al riesgo de todos los periodos.
    num_vars <- min( which( grepl( '^M', names( DATA ) ) ) ) - 1
    
    IN <- sapply( 1:nrow( DATA ), FUN = function( i ) {
      fec <- DATA$FIELD_DATE[ i ]
      # n <- ncol( DATA )
      # Ejemplo:
      # month( ymd( '2022-12-01' ) ) + 12 * ( year( ymd( '2022-12-01' ) ) - min( years ) ) + num_vars
      I <- ifelse( is.na( fec ), NA, month( fec ) + 12 * ( year( fec ) - min( years ) ) + num_vars )
      # I[ I > n ] <- NA
      ER <- DATA[[ I ]][ i ]
      return( ifelse( is.na( I ), as.integer( -1 ), ifelse( is.na( ER ), as.integer( 0 ), as.integer( ER > 0 ) ) ) ) 
    })
  })
  expr <- gsub( '(FIELD_DATE)', field, deparse( expr ) )
  expr <- gsub( '(DATA)', data, expr )
  eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
  gc()

  return( IN )
}

## Compilación de la función para mejorar su rendimiento
indicador_transicion <- cmpfun( 
  indicador_transicion, 
  options = list( 
    optimize = 3,
    suppressAll = FALSE,
    suppressUndefined = TRUE,
    suppressNoSuperAssignVar = TRUE ) 
)

# Para determinar valores de exposicion y expuestos para vivos --------------------------------
funct01 <- function(temp00, t){
  # Creando variable edad
  temp01 <- melt(data = temp00, id.vars = c('CEDULA_COD','SEXO','FECHA_NACIMIENTO'), variable.name = 'PERIODO', value.name = 'VALOR') %>% 
    filter(VALOR != 0) %>% 
    mutate(Mes_Nac = as.integer(substr(x = as.character(FECHA_NACIMIENTO), start = 4, stop = 5)),
           Ani_Nac = as.integer(substr(x = as.character(FECHA_NACIMIENTO), start = 7, stop = 10)),
           Mes_Exp = as.integer(substr(x = PERIODO, start = 7, stop = 10)),
           x = t - Ani_Nac - 1+(Mes_Nac < Mes_Exp)*1)
  # Para reportar tabla
  Edad <- data.table(t = rep(t,242), x = rep(0:120,2), SEXO = c(rep('H',121),rep('M',121)))
  # Exposion al riesgo por edad
  Exposicion <- temp01 %>% group_by(SEXO, x) %>% summarise(Ex = sum(VALOR)) %>% ungroup() %>% mutate(Ex = Ex/12) %>% filter(x >= 0)
  # Expuestos al riesgo por edad
  Expuestos <- temp01 %>% select(CEDULA_COD, SEXO, x) %>% group_by(CEDULA_COD, SEXO, x) %>% slice(1) %>% group_by(SEXO, x) %>% summarise(Nx = n()) %>% ungroup() %>% filter(x >= 0)
  # Merge edad y exposicion
  temp02 <- merge(x = Edad, y = Expuestos, by = c('SEXO','x'), all = TRUE)
  # Merge edad, exposicion y expuestos
  temp02 <- merge(x = temp02, y = Exposicion, by = c('SEXO','x'), all = TRUE) %>% as.data.frame() %>% select(t,SEXO,x,Ex,Nx)
  # Reemplazar NA por 0
  temp02[is.na(temp02)] <- 0
  # Creando tabla para toda la poblacion
  temp03 <- temp02 %>% group_by(t,x) %>% summarise(Ex = sum(Ex), Nx = sum(Nx)) %>% ungroup() %>% mutate(SEXO = 'T') %>% select(t,SEXO,x,Ex,Nx)
  # Creando tabla final
  temp04 <- rbind(temp02, temp03)
  # Return
  return(structure(temp04))
}
# Para determinar valores de exposicion, expuestos y numero de muertes para muertos -----------
funct02 <- function(temp00, t){
  # Muertes mayores o iguales a t
  temp00 <- temp00 %>% 
    mutate(Ani_Mue = as.integer(substr(x = as.character(FECHA_MUERTE), start = 7, stop = 10))) %>% 
    filter(Ani_Mue >= t)
  # Muertes mayores a t
  temp01 <- temp00 %>% filter(Ani_Mue > t) %>% select(-FECHA_MUERTE, -Ani_Mue)
  # Exposicion muertes despues de t
  datos_muertos_despues_t <- funct01(temp00 = temp01, t = t)
  # Muertes ocurridas en t
  temp02 <- temp00 %>% filter(Ani_Mue == t) %>% select(-Ani_Mue)
  # Variables para edad
  temp03 <- melt(data = temp02, id.vars = c('CEDULA_COD','SEXO','FECHA_NACIMIENTO','FECHA_MUERTE'), variable.name = 'PERIODO', value.name = 'VALOR') %>% 
    mutate(Mes_Nac = as.integer(substr(x = as.character(FECHA_NACIMIENTO), start = 4, stop = 5)),
           Ani_Nac = as.integer(substr(x = as.character(FECHA_NACIMIENTO), start = 7, stop = 10)),
           Mes_Mue = as.integer(substr(x = as.character(FECHA_MUERTE), start = 4, stop = 5)),
           Ani_Mue = as.integer(substr(x = as.character(FECHA_MUERTE), start = 7, stop = 10)),
           Mes_Exp = as.integer(substr(x = PERIODO, start = 7, stop = 10))) %>% 
    select(-FECHA_NACIMIENTO, -FECHA_MUERTE, -PERIODO)
  # Numero de muertes
  Muertes <- temp03 %>% 
    select(CEDULA_COD,SEXO,Mes_Nac,Ani_Nac,Mes_Mue,Ani_Mue) %>% 
    group_by(CEDULA_COD,SEXO,Mes_Nac,Ani_Nac,Mes_Mue,Ani_Mue) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(x = Ani_Mue - Ani_Nac - 1 + (Mes_Nac < Mes_Mue)*1) %>% 
    group_by(SEXO, x) %>% 
    summarise(dx = n()) %>% 
    ungroup()
  # Expocision muertos
  temp04 <- temp03 %>% filter(VALOR != 0, Mes_Exp <= Mes_Mue) %>% mutate(x = t - Ani_Nac - 1+(Mes_Nac < Mes_Exp)*1)
  # Para reportar tabla
  Edad <- data.table(t = rep(t,242), x = rep(0:120,2), SEXO = c(rep('H',121),rep('M',121)))
  # Exposion al riesgo por edad
  Exposicion <- temp04 %>% group_by(SEXO, x) %>% summarise(Ex = sum(VALOR)) %>% ungroup() %>% mutate(Ex = Ex/12) %>% filter(x >= 0)
  # Expuestos al riesgo por edad
  Expuestos <- temp04 %>% select(CEDULA_COD, SEXO, x) %>% group_by(CEDULA_COD, SEXO, x) %>% slice(1) %>% group_by(SEXO, x) %>% summarise(Nx = n()) %>% ungroup() %>% filter(x >= 0)
  # Merge edad y exposicion
  temp05 <- merge(x = Edad, y = Expuestos, by = c('SEXO','x'), all = TRUE)
  # Merge edad, exposicion y expuestos
  temp05 <- merge(x = temp05, y = Exposicion, by = c('SEXO','x'), all = TRUE) %>% as.data.frame() %>% select(t,SEXO,x,Ex,Nx)
  # Reemplazar NA por 0
  temp05[is.na(temp05)] <- 0
  # Creando tabla para toda la poblacion
  temp06 <- temp05 %>% group_by(t,x) %>% summarise(Ex = sum(Ex), Nx = sum(Nx)) %>% ungroup() %>% mutate(SEXO = 'T') %>% select(t,SEXO,x,Ex,Nx)
  # Creando tabla exposicion y expuestos
  datos_muertos_antes_t <- rbind(temp05, temp06)
  # Merge edad y muerte
  temp08 <- merge(x = Edad, y = Muertes, by = c('SEXO','x'), all = TRUE) %>% select(t,SEXO,x,dx)
  # Reemplazar NA por 0
  temp08[is.na(temp08)] <- 0
  # Creando tabla para toda la poblacion
  temp09 <- temp08 %>% group_by(t,x) %>% summarise(dx = sum(dx)) %>% ungroup() %>% mutate(SEXO = 'T') %>% select(t,SEXO,x,dx)
  # Creando tabla muertes
  datos_muertos_t <- rbind(temp08, temp09)
  # Creando tabla final
  datos_muertos <- merge(x = datos_muertos_antes_t, y = datos_muertos_t, by = c('t','SEXO','x'), all = TRUE)
  # Return
  return(structure(datos_muertos))
}
# Para determinar valores de exposicion, expuestos y numero de muertes ------------------------
funct03 <- function(vida, muerte, t){
  # Vivos
  datos_vivos <- funct01(temp00 = vida[, c(1:2,3:14)], t = t)
  # Muertos
  datos_muertos <- funct02(temp00 = muerte[, c(1:3,4:16)], t = t)
  # dx para vivos
  datos_vivos$dx <- 0
  # Base final
  datos <- rbind(datos_vivos, datos_muertos) %>% group_by(t, SEXO, x) %>% summarise(Ex = sum(Ex), Nx = sum(Nx), dx = sum(dx)) %>% ungroup()
  # Return
  return(structure(datos))
}
# Para determinar tabulados de valores de exposicion, expuestos y numero de muertos -----------
funct <- function(vivos, muertos){
  # 2012
  data_2012 <- funct03(vida = vivos[, c(1:3,4:15)], muerte = muertos[,c(1:4,5:16)], t = 2012)
  # 2013
  data_2013 <- funct03(vida = vivos[, c(1:3,16:27)], muerte = muertos[,c(1:4,17:28)], t = 2013)
  # 2014
  data_2014 <- funct03(vida = vivos[, c(1:3,28:39)], muerte = muertos[,c(1:4,29:40)], t = 2014)
  # 2015
  data_2015 <- funct03(vida = vivos[, c(1:3,40:51)], muerte = muertos[,c(1:4,41:52)], t = 2015)
  # 2016
  data_2016 <- funct03(vida = vivos[, c(1:3,52:63)], muerte = muertos[,c(1:4,53:64)], t = 2016)
  # 2017
  data_2017 <- funct03(vida = vivos[, c(1:3,64:75)], muerte = muertos[,c(1:4,65:76)], t = 2017)
  # 2018
  data_2018 <- funct03(vida = vivos[, c(1:3,76:87)], muerte = muertos[,c(1:4,77:88)], t = 2018)
  # 2019
  data_2019 <- funct03(vida = vivos[, c(1:3,88:99)], muerte = muertos[,c(1:4,89:100)], t = 2019)
  # 2020
  data_2020 <- funct03(vida = vivos[, c(1:3,100:111)], muerte = muertos[,c(1:4,101:112)], t = 2020)
  # 2021
  data_2021 <- funct03(vida = vivos[, c(1:3,112:123)], muerte = muertos[,c(1:4,113:124)], t = 2021)
  # 2022
  data_2022 <- funct03(vida = vivos[, c(1:3,124:135)], muerte = muertos[,c(1:4,125:136)], t = 2022)
  # Base final
  data_final <- rbind(data_2012,data_2013,data_2014,data_2015,data_2016,data_2017,data_2018,data_2019,data_2020,data_2021,data_2022) %>% as.data.table()
  # Return
  return(structure(data_final))
}
# Para determinar numero de otras causas ------------------------------------------------------
functSx <- function(temp00){
  # Creando base
  temp00 <- melt(data = temp00, id.vars = c('CEDULA_COD','SEXO','FECHA_NACIMIENTO'), variable.name = 'PERIODO', value.name = 'VALOR')
  # Creando contador
  nombre <- data.frame(PERIODO = as.character(unique(temp00$PERIODO)), contador = 1:132)
  # Seleccion de ultima aparicion
  temp01 <- merge(x = temp00, y = nombre, by = 'PERIODO', all = TRUE) %>% 
    mutate(identificador = VALOR*contador) %>% 
    group_by(CEDULA_COD, SEXO, FECHA_NACIMIENTO) %>% 
    summarise(ultima = max(identificador)) %>% 
    ungroup() %>% 
    filter(ultima != 132)
  # Merge edad y otras causas
  temp02 <- merge(x = temp01, y = nombre, by.x = 'ultima', by.y = 'contador') %>% 
    mutate(Mes_Nac = as.integer(substr(x = as.character(FECHA_NACIMIENTO), start = 4, stop = 5)),
           Ani_Nac = as.integer(substr(x = as.character(FECHA_NACIMIENTO), start = 7, stop = 10)),
           Mes_Ult = as.integer(substr(x = PERIODO, start = 7, stop = 10)),
           t = as.integer(substr(x = PERIODO, start = 2, stop = 5)),
           x = t - Ani_Nac - 1+(Mes_Nac < Mes_Ult)*1) %>% 
    group_by(t, SEXO, x) %>% 
    summarise(Sx = n()) %>% 
    ungroup() %>% 
    filter(x >= 0)
  # Creando tabla para toda la poblacion
  temp03 <- temp02 %>% group_by(t,x) %>% summarise(Sx = sum(Sx)) %>% ungroup() %>% mutate(SEXO = 'T') %>% select(t,SEXO,x,Sx)
  # Creando tabla final
  temp04 <- rbind(temp02, temp03)
  # Return
  return(structure(temp04))
}
# Determina tabulados Ex, Nx, dx, Sx, Mx ------------------------------------------------------
tabulados <- function(data){
  # Creando data table
  data <- as.data.table(data)
  # Poblacion viva
  vivos <- data %>% filter(FECHA_MUERTE == '') %>% select(- FECHA_MUERTE) %>% as.data.table()
  # Poblacion muerta
  muertos <- data %>% filter(FECHA_MUERTE != '') %>% as.data.table()
  # Tabulados vivos y muertos
  aux1 <- funct(vivos = vivos, muertos = muertos)
  # # Tabulados salidas
  # aux2 <- functSx(temp00 = vivos)
  # # Tabulado por anio
  # aux <- merge(x = aux1,  y = aux2, by = c('t','SEXO','x'), all = TRUE) %>% as.data.frame()
  aux <- aux1 %>% as.data.frame()
  # Reemplza NA por 0
  aux[is.na(aux)] <- 0
  # # Creando Mx
  # aux <- aux %>% mutate(Mx = dx + Sx) %>% as.data.table()
  aux <- as.data.table(aux)
  # Return
  return(structure(aux))
}
