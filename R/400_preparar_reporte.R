# Creación estructura reporte ----------------------------------------------------------------------
if ( dir.exists( parametros$resultado_seguro ) ) {
  unlink( parametros$resultado_tablas, recursive = TRUE, force = TRUE )
  unlink( parametros$resultado_graficos, recursive = TRUE, force = TRUE )
  unlink( parametros$resultado_seguro, recursive = TRUE, force = TRUE )
} 
dir.create( parametros$resultado_seguro )
dir.create( parametros$resultado_tablas )
dir.create( parametros$resultado_graficos )

REP_file_latex_org <- c( paste( parametros$work_dir, 'Reportes/hombre.png', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/mujer.png', sep = '' )
                         # paste( parametros$work_dir, 'Reportes/modelo.pdf', sep = '' ),
                         # paste( parametros$work_dir, 'Reportes/modelo_sal.pdf', sep = '' )
)

REP_file_latex_des <- c( paste( parametros$resultado_seguro, 'graficos/hombre.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/mujer.png', sep = '' )
                         # paste( parametros$resultado_seguro, 'graficos/modelo.pdf', sep = '' ),
                         # paste( parametros$resultado_seguro, 'graficos/modelo_sal.pdf', sep = '' )
)

file.copy( REP_file_latex_org, REP_file_latex_des, overwrite = TRUE  )