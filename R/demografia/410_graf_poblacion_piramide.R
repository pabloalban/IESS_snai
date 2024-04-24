message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( parametros$graf_modelo_1, encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Gráficos de la pirámide de Afiliados Activos------------------------------------------------------
message( '\tGraficando población afiliada activa inicial por edad y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pob_afi_edad_sexo_ini.RData' ) )

aux <- copy( pob_afi_edad_sexo_ini )
max_edad <- 100
min_edad <- 15
aux <- aux[cat=="afi" & edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
# M
salto_y <- 5
salto_x <- 0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(as.character(c(seq(0.06, 0, -salto_x)*100, seq(salto_x, 0.06, salto_x)*100) ), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x) )

iess_pir_afiliados<-ggplot( aux, aes(x = edad, y = n, fill=sexo) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5) ) +
  theme( legend.position = "bottom" ) +    #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_afiliados, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Masa Salarial ------------------------------------------------------------------------------------
message( '\tGraficando masa salarial por monto y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_masa_sal_edad_monto_ini.RData' ) )

# Graficos de la pirámide de Masa Salarial----------------------------------------------------------
aux <- copy( masa_sal_edad_monto_ini )
aux <- aux[cat=="afi"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
salto_y <- 100
salto_x <- 0.05
brks_y <- seq(-0.40,0.40,salto_x)
lbls_y <- paste0(as.character(c(seq(0.40, 0, -salto_x)*100, seq(salto_x, 0.40, salto_x)*100) ), "%")
brks_x <- seq(100,1600,salto_y)
nb <- length(brks_x)-1   
lbls_x <- c(paste0(formatC(c(brks_x[1:nb]),
                           digits = 0, 
                           format = 'f', 
                           big.mark = '.', 
                           decimal.mark = ',') ),"mayor a 1.500")

iess_pir_masa_salarial<-ggplot( aux, aes(x = monto, y = n, fill=sexo) ) +
  xlab( 'Salario (USD)' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white",  size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white",  size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_masa_salarial, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Jubilados de vejez -------------------------------------------------------------------------------
message( '\tGraficando población jubilada de vejez por edad y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pob_afi_edad_sexo_ini.RData' ) )

# Gráficos de la pirámide de jubilados de vejez ----------------------------------------------------
aux <- copy( pob_afi_edad_sexo_ini )
max_edad<-105
min_edad<-46
aux <- aux[cat=="pvej"]
aux <- aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y <- 5
salto_x <- 0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(as.character(c(seq(0.06, 0, -salto_x)*100, seq(salto_x, 0.06, salto_x)*100) ), "%")
brks_x <- seq(45,105,salto_y)
lbls_x <- paste0(as.character(brks_x) )

iess_pir_jub_vejez<-ggplot( aux, aes(x = edad, y = n, fill=sexo) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green), 
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_jub_vejez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_jub_vejez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensión de jubilados por vejez -------------------------------------------------------------------
message( '\tGraficando pensionistas por monto y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pen_sexo_monto_ini.RData' ) )

# Gráficos de la pirámide de la pension de junilados por vejez--------------------------------------
aux <- copy( pen_sexo_monto_ini )
aux <- aux[cat=="pvejez" & sexo!="D"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y<-100
salto_x<-0.03
brks_y <- seq(-0.18,0.18,salto_x)
lbls_y <- paste0(as.character(c(seq(0.18, 0, -salto_x)*100, seq(salto_x, 0.18, salto_x)*100) ), "%")
brks_x <- seq(100,1600,salto_y)
nb <- length(brks_x)-1   
lbls_x <- paste0(as.character(c(brks_x[1:nb],"1500,01+") ) )

iess_pir_monto_jub_vejez <- ggplot( aux, aes(x = monto, y = n, fill=sexo) ) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_monto_jub_vejez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_jub_vejez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Jubilados de invalidez ---------------------------------------------------------------------------
message( '\tGraficando población jubilada de invalidez por edad y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pob_afi_edad_sexo_ini.RData' ) )

# Gráficos de la pirámide de jubilados de vejez ----------------------------------------------------
aux <- copy( pob_afi_edad_sexo_ini )
max_edad<-105
min_edad<-24
aux <- aux[cat=="pinv" & sexo!="D"]
aux <- aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y <- 5
salto_x <- 0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(as.character(c(seq(0.06, 0, -salto_x)*100, seq(salto_x, 0.06, salto_x)*100) ), "%")
brks_x <- seq(20,105,salto_y)
lbls_x <- paste0(as.character(brks_x) )

iess_pir_jub_invalidez <- ggplot( aux, aes(x = edad, y = n, fill=sexo) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_jub_invalidez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_jub_invalidez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensión de jubilados por invalidez ---------------------------------------------------------------
message( '\tGraficando pensionistas de invalidez por monto y sexo SGO del IESS' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_pen_sexo_monto_ini.RData' ) )

# Graficos de la pirámide de la pension de junilados por vejez--------------------------------------
aux <- copy( pen_sexo_monto_ini )
aux <- aux[cat=="pinv" & sexo!="D"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y<-100
salto_x<-0.05
brks_y <- seq(-0.30,0.30,salto_x)
lbls_y <- paste0(as.character(c(seq(0.30, 0, -salto_x)*100, seq(salto_x, 0.30, salto_x)*100) ), "%")
brks_x <- seq(100,900,salto_y)
nb <- length(brks_x)-1   
lbls_x <- paste0(as.character(c(brks_x[1:nb],"800,01+") ) )


iess_pir_monto_jub_invalidez<-ggplot( aux, aes(x = monto, y = n, fill=sexo) ) +
  xlab( 'monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )


ggsave( plot = iess_pir_monto_jub_invalidez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_jub_invalidez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Jubilados de Discapacidad ------------------------------------------------------------------------
message( '\tGraficando población jubilada de discapacidad por edad y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pob_afi_edad_sexo_ini.RData' ) )

# Gráficos de la pirámide de jubilados de vejez ----------------------------------------------------
aux <- copy( pob_afi_edad_sexo_ini )
max_edad<-69
min_edad<-28
aux <- aux[cat=="pdis" & sexo!="D"]
aux <- aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y <- 5
salto_x<-0.02
brks_y <- seq(-0.12,0.12,salto_x)
lbls_y <- paste0(as.character(c(seq(0.12, 0, -salto_x)*100, seq(salto_x, 0.12, salto_x)*100) ), "%")
brks_x <- seq(25,70,salto_y)
lbls_x <- paste0(as.character(brks_x) )

iess_pir_jub_discapacidad <- ggplot( aux, aes(x = edad, y = n, fill=sexo) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_jub_discapacidad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_jub_discapacidad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pension de jubilados por discapacidad ------------------------------------------------------------
message( '\tGraficando pensionistas de discapacidad por monto y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pen_sexo_monto_ini.RData' ) )

# Gráficos de la pirámide de la pension de jubilados por vejez--------------------------------------
aux <- copy( pen_sexo_monto_ini )
aux <- aux[cat=="pdis" & sexo!="D"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
# M
salto_y <- 100
salto_x <- 0.05
brks_y <- seq(-0.45,0.45,salto_x)
lbls_y <- paste0(as.character(c(seq(0.45, 0, -salto_x)*100, seq(salto_x, 0.45, salto_x)*100) ), "%")
brks_x <- seq(300,900,salto_y)
nb <- length(brks_x)-1   
lbls_x <- paste0(as.character(c(brks_x[1:nb],"800,01+") ) )

iess_pir_monto_jub_discapacidad <- ggplot( aux, aes(x = monto, y = n, fill=sexo) ) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_monto_jub_discapacidad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_jub_discapacidad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas de Montepio Viudedad ----------------------------------------------------------------
message( '\tGraficando población pensionista de viudedad por edad y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pob_afi_edad_sexo_ini.RData' ) )

# Gráficos de la pirámide de jubilados de vejez ----------------------------------------------------
aux <- copy( pob_afi_edad_sexo_ini )
aux <- aux[cat=="pviu" & sexo!="D"]
max_edad <- 105
min_edad <- 7
aux <- aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y <- 5
salto_x <- 0.01
brks_y <- seq(-0.03,0.03,salto_x)
lbls_y <- paste0(as.character(c(seq(0.03, 0, -salto_x)*100, seq(salto_x, 0.03, salto_x)*100) ), "%")
brks_x <- seq(5,105,salto_y)
lbls_x <- paste0(as.character(brks_x) )

iess_pir_pen_viudedad <- ggplot( aux, aes(x = edad, y = n, fill=sexo) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_pen_viudedad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pen_viudedad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensión de pensionistas por viudedad -------------------------------------------------------------
message( '\tGraficando pensionistas de viudedad por monto y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pen_sexo_monto_ini.RData' ) )

# Gráficos de la pirámide de la pension de pensionistas de viudedad-------------------------------------
aux <- copy( pen_sexo_monto_ini )
aux <- aux[cat=="pviud" & sexo!="D"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y <- 50
salto_x<-0.05
brks_y <- seq(-0.45,0.45,salto_x)
lbls_y <- paste0(as.character(c(seq(0.45, 0, -salto_x)*100, seq(salto_x, 0.45, salto_x)*100) ), "%")
brks_x <- seq(50,450,salto_y)
nb <- length(brks_x) -1  
lbls_x <- paste0(as.character(c(brks_x[1:nb],"400,01+") ) )


iess_pir_monto_pen_viudedad <- ggplot( aux, aes(x = monto, y = n, fill=sexo) ) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_monto_pen_viudedad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_pen_viudedad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensionistas de Montepio orfandad ----------------------------------------------------------------
message( '\tGraficando población pensionista de orfandad por edad y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pob_afi_edad_sexo_ini.RData' ) )

# Gráficos de la pirámide de jpensionistas de orfandad----------------------------------------------------
aux <- copy( pob_afi_edad_sexo_ini )
aux <- aux[cat=="porfa" & sexo!="D"]
max_edad <- 105
min_edad <- 0
aux <- aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
M
salto_y <- 5
salto_x <- 0.01
brks_y <- seq(-0.10,0.10,salto_x)
lbls_y <- paste0(as.character(c(seq(0.10, 0, -salto_x)*100, seq(salto_x, 0.10, salto_x)*100) ), "%")
brks_x <- seq(0,105,salto_y)
lbls_x <- paste0(as.character(brks_x) )

iess_pir_pen_orfandad <- ggplot( aux, aes(x = edad, y = n, fill=sexo) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_pen_orfandad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pen_orfandad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pensión de pensionistas por orfandad -------------------------------------------------------------
message( '\tGraficando pensionistas de viudedad por monto y sexo SGO del IESS' )
load( file = paste0( parametros$RData, 'IESS_pen_sexo_monto_ini.RData' ) )

# Gráficos de la pirámide de la pension de junilados por vejez -------------------------------------
aux <- copy( pen_sexo_monto_ini )
aux <- aux[cat=="porfa" & sexo!="D"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame( (aux[,sum(n,na.rm = TRUE),by=sexo]) )  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame( (aux[,max(abs(n),na.rm = TRUE),by=sexo]) ) # En base a este valor poner los límites del eje x
# M
salto_y <- 50
salto_x <- 0.1
brks_y <- seq(-0.40,0.40,salto_x)
lbls_y <- paste0(as.character(c(seq(0.40, 0, -salto_x)*100, seq(salto_x, 0.40, salto_x)*100) ), "%")
brks_x <- seq(50,450,salto_y)
nb <- length(brks_x) -1  
lbls_x <- paste0(as.character(c(brks_x[1:nb],"400,01+") ) )

iess_pir_monto_pen_orfandad <- ggplot( aux, aes(x = monto, y = n, fill=sexo) ) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour = "white", size = 0.1 ) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity', colour = "white", size = 0.1 ) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0) ) +
  theme( legend.position = "bottom" ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") )

ggsave( plot = iess_pir_monto_pen_orfandad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_pen_orfandad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
