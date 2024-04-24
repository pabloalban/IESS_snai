message( paste( rep( "-", 100 ), collapse = "" ) )
#Cargando Rdatas------------------------------------------------------------------------------------
message( "\tCargando datos" )
load( paste0( parametros$RData, "MIESS_censo_recicladores.RData" ) )

censo_miess <- censo_miess %>%
  filter(!(edad_mies %in% c('De 7 a 11 años'))) %>%
  filter(!(instruccion %in% c('Postgrado, Doctorado, PHD')))
censo_miess$instruccion <- gsub( 'Superior Universitario Técnico Tecnologico', 'Técnico', censo_miess$instruccion )
censo_miess[ which(censo_miess$instruccion == 'Superior Universitario (Universidades y Escuelas Politécnicas)'), ]$instruccion <- 'Universitario' 
censo_miess[ which(censo_miess$caracteristica_social_afiliado == 'Seguro Ministerio de la Salud Pública (MSP)'), ]$caracteristica_social_afiliado <- 'Seguro Ministerio de la Salud Pública'
censo_miess[ which(censo_miess$caracteristica_social_estuvo_afiliado == 'Seguro Ministerio de la Salud Pública (MSP)'), ]$caracteristica_social_estuvo_afiliado <- 'Seguro Ministerio de la Salud Pública'
censo_miess[ which(censo_miess$caracteristica_social_estuvo_afiliado == 'Aseguramiento Universal de la Salud (AUS)'), ]$caracteristica_social_estuvo_afiliado <- 'Aseguramiento Universal de la Salud'


#Estadísticas descriptivas--------------------------------------------------------------------------

#Número de recicladores por edad y sexo-------------------------------------------------------------

edad_sexo <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) 

#Porcentaje de recicladores por edad y sexo---------------------------------------------------------

porc_edad_sexo <- edad_sexo %>%
  mutate(porcentaje_mujeres = Mujer * 100 / total[5],
         porcentaje_hombres = Hombre * 100 / total[5],
         porcentaje_total = total * 100 / total[5] ) %>%
  dplyr::select( edad_mies, porcentaje_mujeres, porcentaje_hombres, porcentaje_total )


#Número de recicladores por instrucción y sexo------------------------------------------------------

instr_sexo <- censo_miess %>%
  group_by( sexo_reciclador, instruccion ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, instruccion ) %>% 
  distinct( sexo_reciclador, instruccion, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, instruccion, recicladores ) %>% 
  mutate( instruccion = factor( instruccion,  levels = c( 'Centro de alfabetización',
                                                          'Educación Básica',
                                                          'Educación Media/Bachillerato',
                                                          'Jardín de Infantes',
                                                          'Ninguno',
                                                          'Postgrado, Doctorado, PHD',
                                                          'Primaria',
                                                          'Secundaria',
                                                          'Técnico',
                                                          'Universitario') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>% 
  arrange( instruccion ) %>% 
  mutate( total = Mujer + Hombre,
          instruccion = as.character( instruccion ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / total[10],
         porcentaje_hombres = Hombre * 100 / total[10],
         porc = total * 100 / total[10]) %>%
  dplyr::select( instruccion, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total, porc) 

#Número de recicladores por provincia y sexo--------------------------------------------------------

prov_sexo <- censo_miess %>%
  group_by( sexo_reciclador, provincia ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, provincia ) %>% 
  distinct( sexo_reciclador, provincia, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, provincia, recicladores ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = recicladores, values_fill = 0) %>%
  arrange( provincia ) %>% 
  mutate( total = Mujer + Hombre,
          provincia = as.character( provincia ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / total[25],
         porcentaje_hombres = Hombre * 100 / total[25],
         porc = total * 100 / total[25]) %>%
  dplyr::select( provincia, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total, porc)


#Ingreso promedio de reciclaje por edad y sexo------------------------------------------------------

edad_sal_prom <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingreso_promedio = mean(ingresos_reciclaje) ) %>%
  ungroup(edad_mies, sexo_reciclador)%>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  group_by( edad_mies) %>% 
  mutate( total = mean(ingresos_reciclaje) ) %>% 
  ungroup() %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingreso_promedio, total ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingreso_promedio, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  dplyr::select( edad_mies, Mujer, Hombre, total )

levels(edad_sal_prom$edad_mies) <- c(levels(edad_sal_prom$edad_mies), "Total")
edad_sal_prom <- rbind( edad_sal_prom, c("Total", mean(censo_miess$ingresos_reciclaje[censo_miess$sexo_reciclador=="Mujer"]), mean(censo_miess$ingresos_reciclaje[censo_miess$sexo_reciclador=="Hombre"]), mean(censo_miess$ingresos_reciclaje) ) ) 
edad_sal_prom <- edad_sal_prom %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric )


#Ingreso promedio total por edad y sexo-------------------------------------------------------------

edad_sal_total_prom <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingreso_promedio = mean(ingreso_total)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  group_by( edad_mies) %>% 
  mutate( total = mean(ingreso_total) ) %>% 
  ungroup() %>%
  dplyr::select( sexo_reciclador, edad_mies, ingreso_promedio, total ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingreso_promedio, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  dplyr::select( edad_mies, Mujer, Hombre, total )

levels(edad_sal_total_prom$edad_mies) <- c(levels(edad_sal_total_prom$edad_mies), "Total")
edad_sal_total_prom <- rbind( edad_sal_total_prom, c("Total", mean(censo_miess$ingreso_total[censo_miess$sexo_reciclador=="Mujer"]), mean(censo_miess$ingreso_total[censo_miess$sexo_reciclador=="Hombre"] ), mean(censo_miess$ingreso_total)) ) 
edad_sal_total_prom <- edad_sal_prom %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric )

#Ingreso de reciclaje por edad y sexo--------------------------------------------------------------- 

edad_sal_reciclaje <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingreso_reciclaje = sum(ingresos_reciclaje)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingreso_reciclaje ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingreso_reciclaje, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / total[5],
         porcentaje_hombres = Hombre * 100 / total[5],
         porc = total * 100 / total[5]) %>%
  dplyr::select( edad_mies, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total, porc)


#Rango de ingreso de reciclaje e ingreso total por sexo--------------------------------------------- 

cortes_monto <- c(0, 50, 90, 106.25, 212.5, 425, Inf)

etiquetas_monto <- c(paste0("(\\$", formatC(cortes_monto[1:5],
                                            digits = 2, format = 'f', big.mark = '.', decimal.mark = ','),
                            "-\\$", formatC(cortes_monto[2:6],
                                            digits = 2, format = 'f', big.mark = '.', decimal.mark = ','), "]"), 
                     "Mayor a 425") 

rang_sal_total <- censo_miess %>%
  mutate(  rango_monto = cut(  ingreso_total, 
                               breaks = cortes_monto,
                               labels = etiquetas_monto,
                               include.lowest = TRUE,
                               right = TRUE ) ) %>%
  group_by(  rango_monto, sexo_reciclador ) %>%
  mutate(  recicladores = n(  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo_reciclador, rango_monto, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo_reciclador,
                  rango_monto,
                  recicladores ) %>%
  arrange(  sexo_reciclador, rango_monto  ) %>%
  spread(   ., sexo_reciclador, value = c(  recicladores  ),  sep = "_"  )  %>%
  mutate_if(  is.numeric , replace_na, replace = 0 ) %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_monto = as.character(  rango_monto  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  porc_mujer = 100 * sexo_reciclador_Mujer / sexo_reciclador_Mujer[7] , 
           porc_hombre = 100* sexo_reciclador_Hombre / sexo_reciclador_Hombre[7] , 
           porc_total = 100 * total / total[7] ) %>%
  dplyr::select(  rango_monto, sexo_reciclador_Mujer, porc_mujer, 
                  sexo_reciclador_Hombre, porc_hombre, total, porc_total ) %>%
  distinct(  ., rango_monto, .keep_all = TRUE  )

rang_sal_rec <- censo_miess %>%
  mutate(  rango_monto = cut(  ingresos_reciclaje, 
                               breaks = cortes_monto,
                               labels = etiquetas_monto,
                               include.lowest = TRUE,
                               right = TRUE ) ) %>%
  group_by(  rango_monto, sexo_reciclador ) %>%
  mutate(  recicladores = n(  )  ) %>%
  ungroup(   ) %>%
  distinct(  sexo_reciclador, rango_monto, .keep_all = TRUE  ) %>%
  dplyr::select(  sexo_reciclador,
                  rango_monto,
                  recicladores ) %>%
  arrange(  sexo_reciclador, rango_monto  ) %>%
  spread(   ., sexo_reciclador, value = c(  recicladores  ),  sep = "_"  )  %>%
  mutate_if(  is.numeric , replace_na, replace = 0 ) %>%
  mutate(  total = rowSums( .[2:ncol( . )] )  ) %>%
  mutate(  rango_monto = as.character(  rango_monto  )  ) %>%
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) )  %>%
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(  porc_mujer = 100 * sexo_reciclador_Mujer / total[7] , 
           porc_hombre = 100* sexo_reciclador_Hombre / total[7] , 
           porc_total = 100 * total / total[7] ) %>%
  dplyr::select(  rango_monto, sexo_reciclador_Mujer, porc_mujer, 
                  sexo_reciclador_Hombre, porc_hombre, total, porc_total ) %>%
  distinct(  ., rango_monto, .keep_all = TRUE  )



#Ingreso total por edad y sexo----------------------------------------------------------------------

edad_sal_total <- censo_miess %>%
  group_by(edad_mies, sexo_reciclador) %>%
  mutate(ingresos_total = sum(ingreso_total)) %>%
  ungroup(edad_mies, sexo_reciclador) %>%
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, ingresos_total ) %>% 
  mutate( edad_mies = factor( edad_mies,  levels = c( 'De 12 a 17 años',
                                                      'De 18 a 29 años',
                                                      'De 30 a 64 años',
                                                      'Mayor a 65 años') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = ingresos_total, values_fill = 0) %>% 
  arrange( edad_mies ) %>% 
  mutate( total = Mujer + Hombre,
          edad_mies = as.character( edad_mies ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / total[5],
         porcentaje_hombres = Hombre * 100 / total[5],
         porc = total * 100 / total[5]) %>%
  dplyr::select( edad_mies, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total, porc)


#Número de afiliados por sexo-----------------------------------------------------------------------

afiliados_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  mutate( afiliados = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, afiliados ) %>% 
  mutate( caracteristica_social_afiliado = factor( caracteristica_social_afiliado,  levels = c( 'IESS, Seguro General',
                                                                           'IESS, Seguro Voluntario',
                                                                           'Ninguno',
                                                                           'Seguro Campesino',
                                                                           'Seguro de salud privado con hospitalización',
                                                                           'Seguro de salud privado sin hospitalización',
                                                                           'Seguro del ISSFA ó ISSPOL',
                                                                           'Seguro Ministerio de la Salud Pública',
                                                                           'Seguro Municipales y de Consejos Provinciales') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = afiliados, values_fill = 0) %>% 
  arrange( caracteristica_social_afiliado ) %>% 
  mutate( total = Mujer + Hombre,
          caracteristica_social_afiliado = as.character( caracteristica_social_afiliado ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / total[10],
         porcentaje_hombres = Hombre * 100 / total[10],
         porc = total * 100 / total[10]) %>%
  dplyr::select( caracteristica_social_afiliado, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total,porc)

#Número de personas que estuvieron afiliadas por sexo-----------------------------------------------

afiliados_antiguos_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  mutate( afiliados = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_estuvo_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, afiliados ) %>% 
  mutate( caracteristica_social_estuvo_afiliado = factor( caracteristica_social_estuvo_afiliado,  levels = c( 'Aseguramiento Universal de la Salud',
                                                                                                              'IESS, Seguro General',
                                                                                                              'IESS, Seguro Voluntario',
                                                                                                              'Ninguno',
                                                                                                              'Seguro Campesino',
                                                                                                              'Seguro de salud privado con hospitalización',
                                                                                                              'Seguro de salud privado sin hospitalización',
                                                                                                              'Seguro del ISSFA ó ISSPOL',
                                                                                                              'Seguro Ministerio de la Salud Pública',
                                                                                                              'Seguro Municipales y de Consejos Provinciales') ) ) %>% 
  pivot_wider( ., names_from = sexo_reciclador, values_from = afiliados, values_fill = 0) %>% 
  arrange( caracteristica_social_estuvo_afiliado ) %>% 
  mutate( total = Mujer + Hombre,
          caracteristica_social_estuvo_afiliado = as.character( caracteristica_social_estuvo_afiliado ) ) %>% 
  rbind(  ., c( "Total", as.character( colSums( .[,2:ncol( . )],  na.rm =TRUE  ) ) ) ) %>% 
  mutate_at(  c( 2:ncol( . ) ), as.numeric ) %>%
  mutate(porcentaje_mujeres = Mujer * 100 / total[11],
         porcentaje_hombres = Hombre * 100 / total[11],
         porc = total * 100 / total[11]) %>%
  dplyr::select( caracteristica_social_estuvo_afiliado, Mujer, porcentaje_mujeres, Hombre, porcentaje_hombres, total, porc)



#Tablas para elaboración de pirámides---------------------------------------------------------------
#Pirámide del porcentaje de recicladores base según edad y sexo-------------------------------------
pir_porc_edad_sexo <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores ) %>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#Pirámide según instrucción y sexo------------------------------------------------------------------
pir_instr_sexo <- censo_miess %>%
  group_by( sexo_reciclador, instruccion ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, instruccion ) %>% 
  distinct( sexo_reciclador, instruccion, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, instruccion, recicladores ) %>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, instruccion, porcentaje )

#Pirámide según provincia y sexo--------------------------------------------------------------------
pir_prov_sexo <- censo_miess %>%
  group_by( sexo_reciclador, provincia ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, provincia ) %>% 
  distinct( sexo_reciclador, provincia, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, provincia, recicladores ) %>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, provincia, porcentaje ) %>% 
  arrange( provincia )

#Pirámide de ingreso promedio de reciclaje según instrucción y sexo---------------------------------
pir_edad_sal_prom <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( promedio = mean(ingresos_reciclaje)) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, promedio )

#Pirámide de ingreso de reciclaje según instrucción y sexo------------------------------------------
pir_edad_sal_reciclaje <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = sum(ingresos_reciclaje) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores ) %>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#Pirámide de ingreso total según instrucción y sexo-------------------------------------------------
pir_edad_sal_total <- censo_miess %>%
  group_by( sexo_reciclador, edad_mies ) %>% 
  mutate( recicladores = sum(ingreso_total) ) %>% 
  ungroup( sexo_reciclador, edad_mies ) %>% 
  distinct( sexo_reciclador, edad_mies, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, edad_mies, recicladores )%>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, edad_mies, porcentaje )

#Pirámide de afiliados por sexo---------------------------------------------------------------------
pir_afiliados_sexo <- censo_miess %>%
  group_by( caracteristica_social_afiliado, sexo_reciclador ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( caracteristica_social_afiliado, sexo_reciclador ) %>% 
  distinct( caracteristica_social_afiliado, sexo_reciclador, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, recicladores )%>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, caracteristica_social_afiliado, porcentaje )

#Pirámide de antiguos afiliados por sexo------------------------------------------------------------
pir_afiliados_antiguos_sexo <- censo_miess %>%
  group_by( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  mutate( recicladores = n( ) ) %>% 
  ungroup( sexo_reciclador, caracteristica_social_estuvo_afiliado ) %>% 
  distinct( sexo_reciclador, caracteristica_social_estuvo_afiliado, .keep_all = TRUE ) %>% 
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, recicladores )%>%
  mutate(porcentaje = recicladores * 100/ sum(recicladores) ) %>%
  dplyr::select( sexo_reciclador, caracteristica_social_estuvo_afiliado, porcentaje )


#Guardar en Rdatas----------------------------------------------------------------------------------
message( "\tGuardando Rdatas" )
save(  afiliados_antiguos_sexo,
       afiliados_sexo,
       edad_sal_prom,
       edad_sal_reciclaje,
       edad_sal_total,
       edad_sal_total_prom,
       edad_sexo,
       instr_sexo,
       pir_afiliados_antiguos_sexo,
       pir_afiliados_sexo,
       pir_edad_sal_prom,
       pir_edad_sal_reciclaje,
       pir_edad_sal_total,
       pir_instr_sexo,
       pir_porc_edad_sexo,
       pir_prov_sexo,
       prov_sexo,
       rang_sal_rec,
       rang_sal_total,
       file = paste0(  parametros$RData, 'IESS_REC_tablas_demografia.RData'  )  )

