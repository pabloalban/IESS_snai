message( paste( rep( "-", 100 ), collapse = "" ) )
#Cargando Rdatas------------------------------------------------------------------------------------
message( "\tCargando datos" )
load( paste0( parametros$RData, "IESS_agentes_snai.RData" ) )

#Estadísticas descriptivas--------------------------------------------------------------------------

#Número de servidores por edad y sexo-------------------------------------------------------------

edad_sexo <- snai %>%
  group_by( sexo, x ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, x ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, servidores ) %>% 
  pivot_wider( ., names_from = sexo, values_from = servidores, values_fill = 0) %>% 
  arrange( x ) %>% 
  mutate( total = M + F,
          x = as.character( x ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Porcentaje de servidores por edad y sexo---------------------------------------------------------

porc_edad_sexo <- edad_sexo %>%
  mutate(porcentaje_mujeres = F * 100 / total[50],
         porcentaje_hombres = M * 100 / total[50],
         porcentaje_total = total * 100 / total[50] ) %>%
  dplyr::select( x, porcentaje_mujeres, porcentaje_hombres, porcentaje_total )

#Número de servidores por puesto unificado y sexo------------------------------------------------------

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

#Número de servidores por provincia y sexo--------------------------------------------------------

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


#Salario por edad y sexo--------------------------------------------------------------- 

edad_sal <- snai %>%
  group_by(x, sexo) %>%
  mutate(salario = sum(salario)) %>%
  ungroup(x, sexo) %>%
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, salario ) %>% 
  pivot_wider( ., names_from = sexo, values_from = salario, values_fill = 0) %>% 
  arrange( x ) %>% 
  mutate( total = M + F,
          x = as.character( x ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = F * 100 / total[50],
         porcentaje_hombres = M * 100 / total[50],
         porc = total * 100 / total[50]) %>%
  dplyr::select( x, F, porcentaje_mujeres, M, porcentaje_hombres, total, porc)



#Tablas para elaboración de pirámides---------------------------------------------------------------

#Pirámide del porcentaje de servidores base según edad y sexo---------------------------------------
pir_porc_edad_sexo <- snai %>%
  group_by( sexo, x ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, x ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, x, porcentaje )

#Pirámide según grado y sexo------------------------------------------------------------------
pir_grado_sexo <- snai %>%
  group_by( sexo, denominacion_puesto_unificado ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, denominacion_puesto_unificado ) %>% 
  distinct( sexo, denominacion_puesto_unificado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, denominacion_puesto_unificado, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, denominacion_puesto_unificado, porcentaje )

#Pirámide según provincia y sexo--------------------------------------------------------------------
pir_prov_sexo <- snai %>%
  group_by( sexo, provincia ) %>% 
  mutate( servidores = n( ) ) %>% 
  ungroup( sexo, provincia ) %>% 
  distinct( sexo, provincia, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, provincia, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, provincia, porcentaje ) %>% 
  arrange( provincia )

#Pirámide de salario según grado y sexo------------------------------------------
pir_edad_sal <- snai %>%
  group_by( sexo, x ) %>% 
  mutate( servidores = sum(salario) ) %>% 
  ungroup( sexo, x ) %>% 
  distinct( sexo, x, .keep_all = TRUE ) %>% 
  dplyr::select( sexo, x, servidores ) %>%
  mutate(porcentaje = servidores * 100/ sum(servidores) ) %>%
  dplyr::select( sexo, x, porcentaje )


#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  edad_sal,
       edad_sexo,
       grado_sexo,
       pir_edad_sal,
       pir_grado_sexo,
       pir_porc_edad_sexo,
       pir_prov_sexo,
       porc_edad_sexo,
       prov_sexo,
       file = paste0(  parametros$RData, 'IESS_SNAI_tablas_demografia.RData'  )  )

#Limpiar Ram----------------------------------------------------------------------------------------
message(  paste(  rep( '-', 100  ), collapse = ''  )  )
rm(  list = ls(  )[ !(  ls(  ) %in% c(  'parametros'  )  ) ]  )
gc(  )

