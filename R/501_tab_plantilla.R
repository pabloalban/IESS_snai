#Primera etapa de correción de NA ------------------------------------------------------------------
NA_process_inicial <- function(A,ncol){
  A <- as.data.frame(A)
  for (j in ncol:dim(A)[2]) {
    for (i in 1:dim(A)[1]) {
      A[i,j] <-ifelse(is.na(A[i,j])==TRUE,"", A[i,j])
    }
  }
  return(data.frame(A))
}

#Segunda etapa de correcion de NA ------------------------------------------------------------------
NA_gsub <- function(A, ncol){
  A <- as.data.frame( A )
  for (i in ncol:dim(A)[2]) {
    A[ , i] <- gsub("NA", "", A[ , i])
    A[ , i] <- gsub(" NA", "", A[ , i])
    A[ , i] <- gsub("  NA", "", A[ , i])
    A[ , i] <- gsub("   NA", "", A[ , i])
    A[ , i] <- gsub("    NA", "", A[ , i])
    A[ , i] <- gsub("     NA", "", A[ , i])
    A[ , i] <- gsub("      NA", "", A[ , i])
    A[ , i] <- gsub("        NA", "", A[ , i])
    A[ , i] <- gsub("         NA", "", A[ , i])
  }
  return( data.frame(A))
}

# Coloreando tablas
table_color_cuerpo <- function( A, col, col_f, fila_ini, fila_end, col_ini, col_end, posi_col ){
  A <- as.data.frame( A )
  for (j in col_ini:col_end ) {
    for (i in fila_ini:fila_end) {
      if ( j %in% c(col_ini,posi_col)) {
        A[ i, j] <- paste0("\\cellcolor{Azul!", col[i],"}"," ", A[ i , j])
      } 
      if(j %notin% c(col_ini, posi_col) ){
        A[ i, j] <- paste0("\\cellcolor{gray!", col_f[i],"}"," ", A[ i , j] )
      }
    }
  }
  return( data.frame(A) )
}

var_num <- function( A, col_ini ){
  A <- as.data.frame( A )
  for (i in col_ini:dim(A)[2]) {
    A[ , i ] <- as.numeric( A[ , i ])
  }
  return( data.frame( A ) )
}

NA_gsub <- function(A, ncol){
  A <- as.data.frame( A )
  for (i in ncol:dim(A)[2]) {
    A[ , i] <- gsub("NA", "", A[ , i])
    A[ , i] <- gsub(" NA", "", A[ , i])
    A[ , i] <- gsub("  NA", "", A[ , i])
    A[ , i] <- gsub("   NA", "", A[ , i])
    A[ , i] <- gsub("    NA", "", A[ , i])
    A[ , i] <- gsub("     NA", "", A[ , i])
    A[ , i] <- gsub("      NA", "", A[ , i])
    A[ , i] <- gsub("        NA", "", A[ , i])
    A[ , i] <- gsub("         NA", "", A[ , i])
  }
  return( data.frame(A))
}

esp_gsub <- function(A, ncol){
  A <- as.data.frame( A )
  for (i in ncol:dim(A)[2]) {
    A[ , i] <- gsub(" ", "", A[ , i])
    A[ , i] <- gsub("  ", "", A[ , i])
    A[ , i] <- gsub("   ", "", A[ , i])
    A[ , i] <- gsub("    ", "", A[ , i])
    A[ , i] <- gsub("     ", "", A[ , i])
    A[ , i] <- gsub("      ", "", A[ , i])
    A[ , i] <- gsub("       ", "", A[ , i])
    A[ , i] <- gsub("         ", "", A[ , i])
    A[ , i] <- gsub("         ", "", A[ , i])
  }
  return( data.frame(A))
}

