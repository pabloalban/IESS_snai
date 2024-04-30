message( paste( rep( "-", 100 ), collapse = "" ) )

#Cargando Rdatas--------------------------------------------------------------------------------

message( "\tCargando datos" )
load( paste0( parametros$RData, "IESS_agentes_snai.RData" ) )

#Estadísticas descriptivas----------------------------------------------------------------------

#Rango de Número de servidores por edad y sexo-------------------------------------------------- 

cortes_x <- c( 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf )

etiquetas_x <- c(paste0("(\ ", formatC(cortes_x[1:9],
                                            digits = 0, format = 'f', big.mark = '.', decimal.mark = ','),
                            "-", formatC(cortes_x[2:10],
                                            digits = 0, format = 'f', big.mark = '.', decimal.mark = ','), "]"), 
                     "Mayor a 65") 

rang_edad_sexo <- snai %>%
  mutate(  rango_x = cut(  x, 
                           breaks = cortes_x,
                           labels = etiquetas_x,
                           include.lowest = TRUE,
                           right = TRUE ) ) %>%
  group_by(  rango_x, sexo ) %>%
  mutate(  servidores = n( )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, rango_x, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo,
                  rango_x,
                  servidores ) %>%
  arrange(  sexo, rango_x  ) %>%
  spread(   ., sexo, value = c(  servidores  ),  sep = "_"  )  %>%
  mutate_if(  is.numeric , replace_na, replace = 0 ) %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_x = as.character(  rango_x  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  porc_mujer = 100 * sexo_F / total[11] , 
           porc_hombre = 100* sexo_M / total[11] , 
           porc_total = 100 * total / total[11] ) %>%
  dplyr::select(  rango_x, sexo_F, porc_mujer, 
                  sexo_M, porc_hombre, total, porc_total ) %>%
  distinct(  ., rango_x, .keep_all = TRUE  )


#Número de servidores por puesto unificado y sexo-----------------------------------------------

grado_sexo <- snai %>%
  group_by( sexo, denominacion_puesto_unificado ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, denominacion_puesto_unificado ) %>% 
  distinct( sexo, denominacion_puesto_unificado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, denominacion_puesto_unificado, servidores ) %>% 
  pivot_wider( ., names_from = sexo, values_from = servidores, values_fill = 0) %>% 
  arrange( denominacion_puesto_unificado ) %>% 
  mutate( total = M + F,
          denominacion_puesto_unificado = as.character( denominacion_puesto_unificado ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = F * 100 / total[7],
         porcentaje_hombres = M * 100 / total[7],
         porc = total * 100 / total[7]) %>%
  dplyr::select( denominacion_puesto_unificado, F, porcentaje_mujeres, M, porcentaje_hombres, total, porc) 

#Número de servidores por provincia y sexo------------------------------------------------------

prov_sexo <- snai %>%
  group_by( sexo, provincia ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, provincia ) %>% 
  distinct( sexo, provincia, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, provincia, servidores ) %>% 
  pivot_wider( ., names_from = sexo, values_from = servidores, values_fill = 0) %>%
  arrange( provincia ) %>% 
  mutate( total = M + F,
          provincia = as.character( provincia ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = F * 100 / total[21],
         porcentaje_hombres = M * 100 / total[21],
         porc = total * 100 / total[21]) %>%
  dplyr::select( provincia, F, porcentaje_mujeres, M, porcentaje_hombres, total, porc)

#Salario por grado------------------------------------------------------------------------------
file <-
  paste0( parametros$Data,
          'SERVIDORES CSVP.xlsx' )
sal_snai <- read_excel(
  file, 
  sheet = 'Hoja2',
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) %>% clean_names(  )

sal_snai$denominacion_puesto_unificado <- str_to_title( sal_snai$denominacion_puesto_unificado )


#Salario por rango edad y sexo------------------------------------------------------------------ 

rang_edad_sal_prom <- snai %>%
  mutate(  rango_x = cut(  x, 
                           breaks = cortes_x,
                           labels = etiquetas_x,
                           include.lowest = TRUE,
                           right = TRUE ) ) %>%
  group_by(  rango_x, sexo ) %>%
  mutate(  servidores = mean( salario )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo, rango_x, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo,
                  rango_x,
                  servidores ) %>%
  arrange(  sexo, rango_x  ) %>%
  spread(   ., sexo, value = c(  servidores  ),  sep = "_"  )  %>%
  mutate_if(  is.numeric , replace_na, replace = 0 ) %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_x = as.character(  rango_x  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  porc_mujer = 100 * sexo_F / total[11] , 
           porc_hombre = 100* sexo_M / total[11] , 
           porc_total = 100 * total / total[11] ) %>%
  dplyr::select(  rango_x, sexo_F, porc_mujer, 
                  sexo_M, porc_hombre, total, porc_total ) %>%
  distinct(  ., rango_x, .keep_all = TRUE  )


#Tablas para elaboración de pirámides----------------------------------------------------------

#Pirámide del porcentaje de servidores base según edad y sexo----------------------------------
pir_porc_edad_sexo <- snai %>%
  group_by( sexo, x ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, x ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, x, porcentaje )

#Pirámide según grado y sexo-------------------------------------------------------------------
pir_grado_sexo <- snai %>%
  group_by( sexo, denominacion_puesto_unificado ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, denominacion_puesto_unificado ) %>% 
  distinct( sexo, denominacion_puesto_unificado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, denominacion_puesto_unificado, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, denominacion_puesto_unificado, porcentaje )

#Pirámide según provincia y sexo---------------------------------------------------------------
pir_prov_sexo <- snai %>%
  group_by( sexo, provincia ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, provincia ) %>% 
  distinct( sexo, provincia, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, provincia, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, provincia, porcentaje ) %>% 
  arrange( provincia )

#Pirámide de salario según grado y sexo--------------------------------------------------------
pir_edad_sal_prom <- snai %>%
  group_by( sexo, x ) %>% 
  mutate( servidores = mean(salario) ) %>% 
  ungroup( sexo, x ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, x, porcentaje )

#Guardar en Rdatas-----------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  grado_sexo,
       pir_edad_sal_prom,
       pir_grado_sexo,
       pir_porc_edad_sexo,
       pir_prov_sexo,
       prov_sexo,
       rang_edad_sal_prom,
       rang_edad_sexo,
       sal_snai,
       file = paste0(  parametros$RData, 'IESS_SNAI_tablas_demografia.RData'  )  )

#Limpiar Ram-----------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )

