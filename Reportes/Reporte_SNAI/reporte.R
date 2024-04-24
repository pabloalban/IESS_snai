# Script generado para compilar el informe
message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga información --------------------------------------------------------------------------------
message('\tCargando información' )
setwd( parametros$work_dir )

# Entorno para el reporte
REP <- new.env()

# Control ------------------------------------------------------------------------------------------
REP$copy_final <- FALSE
REP$knit_quiet <- TRUE
REP$hacer_graficos <- TRUE
REP$hacer_tablas <- TRUE
REP$latex_clean <- TRUE
REP$latex_aux_clean <- FALSE
REP$latex_quiet <- TRUE

# Parámetros ---------------------------------------------------------------------------------------
message('\tEstableciendo parámetros')
REP$rep_nom <- parametros$reporte_nombre
REP$empresa <- parametros$empresa
REP$fec_eje <- format( parametros$fec_eje, '%Y_%m_%d' )
REP$rep_dir <- parametros$resultado_seguro
REP$rep_tab <- parametros$resultado_tablas
REP$rep_gra <- parametros$resultado_graficos
REP$rep_latex <- parametros$reporte_latex
REP$hor <- parametros$horizonte
REP$style <- 'style.tex'
REP$style_math <- 'comandosEPN.sty'
REP$bib_lib <- 'bibliografia_libros.bib'
REP$bib_art <- 'bibliografia_articulos.bib'
REP$bib_ley <- 'bibliografia_leyes.bib'

REP$tit <- 'Estudio para determinar la base presuntiva de aportación del trabajador autónomo reciclador de base'
REP$nom_seg <- 'Seguro de Invalidez, Vejez y Muerte'
REP$seg <- switch( parametros$seguro,
                   'ivm' = 'Seguro IVM',
                   'ces' = 'Seguro de Cesantía',
                   'des' = 'Seguro de Desempleo',
                   'rtr' = 'Seguro de Riesgos del Trabajo',
                   'sal' = 'Seguro de Salud',
                   'ssc' = 'SSC') 
REP_seg <- 'Seguro General de Riesgos del Trabajo'

paste( 'Seguro', parametros$seguro )
REP$fec_fin <- format( parametros$rtr_fec_fin, '%Y-%m-%d' )
REP$fec_val <- format( ymd( '2024-01-29' ), '%Y-%m-%d' )
REP$watermark <- paste0( 'Borrador ', parametros$fec_eje, ' ', format( Sys.time(), '%H:%M:%S' ) )
REP$version <- digest( paste0( 'IESSDAIE', format( Sys.time(), '%Y%m%d%H' ) ), algo = 'sha256', file = FALSE )

# Copia de resultados  -----------------------------------------------------------------------------
#Busqueda de TNRH
setwd(paste0( parametros$resultados))
arch <- list.files()
arch <- arch[grep("TNRH", arch) ]
tnrh <- file.info( arch )
tnrh <- arch[order(tnrh$mtime, decreasing = TRUE)][1]

resul_tnrh <- paste0( parametros$resultados, tnrh, "/")

setwd( parametros$work_dir )

REP$file_latex_org <- c( 
  paste( parametros$work_dir, 'Reportes/bibliografia_libros.bib', sep = '' ),
  paste( parametros$work_dir, 'Reportes/bibliografia_articulos.bib', sep = '' ),
  paste( parametros$work_dir, 'Reportes/bibliografia_leyes.bib', sep = '' ),
  paste( parametros$work_dir, 'Reportes/style.tex', sep = '' ),
  paste( parametros$work_dir, 'Reportes/logo_iess_azul.png', sep = '' ),
  paste( parametros$work_dir, 'Reportes/caratula.png', sep = '' ),
  paste( parametros$work_dir, 'Reportes/caratula_v2.png', sep = '' ),
  paste( parametros$work_dir, 'Reportes/sumilla_actuario.png', sep = '' ),
  paste( parametros$work_dir, 'Reportes/firma_actuario.png', sep = '' ),
  paste( parametros$work_dir, 'Reportes/comandosEPN.sty', sep = '' ),
  paste( resul_tnrh, 'IESS_TNRH_estudio_actuarial.pdf', sep = '' ),
  paste( parametros$work_dir, 'Reportes/modelo.pdf', sep = '' ),
  paste( parametros$work_dir, 'Reportes/modelo_sal.pdf', sep = '' ) )

REP$file_latex_des <- c( 
  paste( REP$rep_dir, 'bibliografia_libros.bib', sep = '' ), 
  paste( REP$rep_dir, 'bibliografia_articulos.bib', sep = '' ),
  paste( REP$rep_dir, 'bibliografia_leyes.bib', sep = '' ),
  paste( REP$rep_dir, 'style.tex', sep = '' ),
  paste( REP$rep_dir, 'graficos/logo_iess_azul.png', sep = '' ),
  paste( REP$rep_dir, 'graficos/caratula.png', sep = '' ),
  paste( REP$rep_dir, 'graficos/caratula_v2.png', sep = '' ),
  paste( REP$rep_dir, 'graficos/sumilla_actuario.png', sep = '' ),
  paste( REP$rep_dir, 'graficos/firma_actuario.png', sep = '' ),
  paste( REP$rep_dir, 'comandosEPN.sty', sep = '' ),
  paste( REP$rep_dir, 'graficos/IESS_TNRH_estudio_actuarial.pdf', sep = '' ),
  paste( REP$rep_dir, 'graficos/modelo.pdf', sep = '' ),
  paste( REP$rep_dir, 'graficos/modelo_sal.pdf', sep = '' ) )

REP$file_latex_clean <- c( 
  paste( REP$rep_dir, 'bibliografia_libros.bib', sep = '' ), 
  paste( REP$rep_dir, 'bibliografia_articulos.bib', sep = '' ),
  paste( REP$rep_dir, 'bibliografia_leyes.bib', sep = '' ),
  paste( REP$rep_dir, 'style.tex', sep = '' ),
  paste( REP$rep_dir, 'comandosEPN.sty', sep = '' ) )

file.copy( REP$file_latex_org, REP$file_latex_des, overwrite = TRUE  )

# Compilación reporte ------------------------------------------------------------------------------
message('\tInicio compilación')

# Genera información automática --------------------------------------------------------------------
source( paste0( parametros$reporte_seguro, 'auto_informacion.R' ), encoding = 'UTF-8', echo = FALSE )

# Kniting reporte ----------------------------------------------------------------------------------
setwd( parametros$reporte_seguro ) 
knit( input = "reporte.Rnw", 
      output = paste0( REP$rep_dir, REP$rep_latex ),
      quiet = REP$knit_quiet, encoding = 'utf8' )

# Compilacion LaTeX --------------------------------------------------------------------------------
message('\tInicio compilación LaTeX')
setwd( REP$rep_dir )
tools::texi2pdf( REP$rep_latex, quiet = REP$latex_quiet, clean = REP$latex_clean )  
setwd( parametros$work_dir )
message('\tFin compilación LaTeX')

if( REP$latex_aux_clean ) {
  unlink( REP$file_latex_clean, recursive = TRUE )
}

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
