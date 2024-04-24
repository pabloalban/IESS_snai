message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura población inicial' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = parametros$demo_rdata_sgo_tasas_tran )
load( file = parametros$demo_rdata_sgo_tran_prep )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Parámetros ---------------------------------------------------------------------------------------
x_max <- parametros$demo_edad_max
x_lst <- 0:x_max

# Afiliados activos hombres ---------------------------------------------------------------------------
message( '\tAfiliados activos hombres' )

aux <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx, Nx_dec ) %>% 
  dplyr::filter( sexo == 'H', x >= 15 & x <= 105, anio <= 2020 ) %>%
  # dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx_dec ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_afi_act_est_h_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Afiliados activos mujeres ---------------------------------------------------------------------------
message( '\tAfiliados activos mujeres' )

aux <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx, Nx_dec ) %>% 
  dplyr::filter( sexo == 'M', x >= 15 & x <= 105, anio <= 2020 ) %>%
  # dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx_dec ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_afi_act_est_m_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas vejez hombres ---------------------------------------------------------------------------
message( '\tPensionistas vejez hombres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  # dplyr::mutate( x = case_when( x < 100 ~ x, x >= 100 ~ 100 ) ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_vej_est_h_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas vejez mujeres ---------------------------------------------------------------------------
message( '\tPensionistas vejez mujeres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  # dplyr::mutate( x = case_when( x < 100 ~ x, x >= 100 ~ 100 ) ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_vej_est_m_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas invalidez hombres ---------------------------------------------------------------------------
message( '\tPensionistas invalidez hombres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_inv_est_h_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas invalidez mujeres ---------------------------------------------------------------------------
message( '\tPensionistas invalidez mujeres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_inv_est_m_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas viudedad hombres ---------------------------------------------------------------------------
message( '\tPensionistas viudedad hombres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_viu_est_h_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas viudedad mujeres ---------------------------------------------------------------------------
message( '\tPensionistas viudedad mujeres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_viu_est_m_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas orfandad hombres ---------------------------------------------------------------------------
message( '\tPensionistas orfandad hombres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'H' & tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_orf_est_h_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Pensionistas orfandad mujeres ---------------------------------------------------------------------------
message( '\tPensionistas orfandad mujeres' )

aux <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx, Nx, ux ) %>%
  dplyr::filter( sexo == 'M' & tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( anio ) %>% 
  dplyr::summarise( ERt = sum( ERx ), 
                    Nt = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( rt = round( 100 * Nt/ERt, digits = 2 ) ) %>% 
  dplyr::rename( "Año" = anio, 
                 "Exposición" = ERt, 
                 "Contingencias" = Nt, 
                 "Porcentaje" = rt ) %>% 
  as.data.table() %>% 
  mutate_at( c( 1, 3 ), as.integer ) %>%
  mutate_at( c( 2, 4 ), as.numeric ) %>% 
  as.data.frame()

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_orf_est_m_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Consolidado afiliados activos exposicion ----------------------------------------------------
message( '\tAfiliados activos' )

aux_erx <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, ERx ) %>% 
  dplyr::filter( x >= 15 & x <= 105, anio <= 2020 ) %>%
  # # dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  dplyr::rename( ERxH = H, ERxM = M)

aux_nx <- sgo_act_tran_anio %>% 
  dplyr::select( anio, sexo, x, Nx_dec ) %>% 
  dplyr::filter( x >= 15 & x <= 105, anio <= 2020 ) %>%
  # # dplyr::mutate( x = case_when( x < 80 ~ x, x >= 80 ~ 80 ) ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx_dec ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3 ), as.integer ) %>%
  as.data.frame() %>% 
  dplyr::rename( NxH = H, NxM = M)

aux <- merge( x = aux_erx, aux_nx, by.x = "x", by.y = "x", all.x = TRUE )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 0, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_afi_act_cons_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Consolidado pensionistas vejez exposicion ---------------------------------------------------------------------------
message( '\tPensionistas vejez' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx ) %>%
  dplyr::filter( tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  dplyr::rename( ERxH = H, ERxM = M)

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, Nx ) %>%
  dplyr::filter( tipo == 'VEJEZ' & x >= 55 & x <= 105, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3), as.integer ) %>%
  as.data.frame() %>% 
  dplyr::rename( NxH = H, NxM = M)

aux <- merge( x = aux_erx, aux_nx, by.x = "x", by.y = "x", all.x = TRUE )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 0, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_vej_cons_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Consolidado pensionistas invalidez exposicion ---------------------------------------------------------------------------
message( '\tPensionistas invalidez' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx ) %>%
  dplyr::filter( tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  dplyr::rename( ERxH = H, ERxM = M)

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, Nx ) %>%
  dplyr::filter( tipo %in% c( 'INVALIDEZ', 'DISCAPACIDAD' ) & x >= 25 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3), as.integer ) %>%
  as.data.frame() %>% 
  dplyr::rename( NxH = H, NxM = M)

aux <- merge( x = aux_erx, aux_nx, by.x = "x", by.y = "x", all.x = TRUE )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 0, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_inv_cons_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Consolidado pensionistas viudedad exposicion ---------------------------------------------------------------------------
message( '\tPensionistas viudedad' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx ) %>%
  dplyr::filter( tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  dplyr::rename( ERxH = H, ERxM = M)

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, Nx ) %>% 
  dplyr::filter( tipo == 'VIUDEDAD' & x >= 20 & x <= 100, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3), as.integer ) %>%
  as.data.frame() %>% 
  dplyr::rename( NxH = H, NxM = M)

aux <- merge( x = aux_erx, aux_nx, by.x = "x", by.y = "x", all.x = TRUE )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 0, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_viu_cons_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )
# Consolidado pensionistas orfandad exposicion ---------------------------------------------------------------------------
message( '\tPensionistas viudedad' )

aux_erx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, ERx ) %>% 
  dplyr::filter( tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( ERx = sum( ERx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'ERx' ) %>% 
  mutate_at( c( 1), as.integer ) %>%
  mutate_at( c( 2, 3 ), as.numeric ) %>% 
  as.data.frame() %>% 
  dplyr::rename( ERxH = H, ERxM = M)

aux_nx <- sgo_pen_tran_anio %>% 
  dplyr::select( tipo, anio, sexo, x, Nx ) %>% 
  dplyr::filter( tipo == 'ORFANDAD' & x >= 0 & x <= x_max, anio <= 2020 ) %>%
  dplyr::group_by( x, sexo ) %>% 
  dplyr::summarise( Nx = sum( Nx ) ) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table() %>% 
  reshape2::dcast( formula = x ~ sexo, value.var = 'Nx' ) %>% 
  mutate_at( c( 1, 2, 3), as.integer ) %>%
  as.data.frame() %>% 
  dplyr::rename( NxH = H, NxM = M)

aux <- merge( x = aux_erx, aux_nx, by.x = "x", by.y = "x", all.x = TRUE )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 0, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sgo_pen_orf_cons_2012_2020', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )

# Edad promedio -------------------------------------------------------------------------------
# Afi_act
aux <- tas_2_6 %>% 
  as.data.frame() %>% 
  dplyr::group_by( sexo ) %>% 
  summarise( TERx = sum( ERx, na.rm = TRUE ), 
             TNx = sum( Nx, na.rm = TRUE ) ) %>% 
  ungroup()
temp <- tas_2_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "H" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "H" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "H" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "afi_act" )
temp0 <- tas_2_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "M" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "M" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "M" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "afi_act" )
temp <- rbind( temp, temp0 )
# Pen_vej
aux <- tas_4_6 %>% 
  as.data.frame() %>% 
  dplyr::group_by( sexo ) %>% 
  summarise( TERx = sum( ERx, na.rm = TRUE ), 
             TNx = sum( Nx, na.rm = TRUE ) ) %>% 
  ungroup()
temp0 <- tas_4_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "H" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "H" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "H" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_vej" )
temp <- rbind( temp, temp0 )
temp0 <- tas_4_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "M" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "M" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "M" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_vej" )
temp <- rbind( temp, temp0 )
# Pen_inv
aux <- tas_5_6 %>% 
  as.data.frame() %>% 
  dplyr::group_by( sexo ) %>% 
  summarise( TERx = sum( ERx, na.rm = TRUE ), 
             TNx = sum( Nx, na.rm = TRUE ) ) %>% 
  ungroup()
temp0 <- tas_5_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "H" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "H" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "H" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_inv" )
temp <- rbind( temp, temp0 )
temp0 <- tas_5_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "M" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "M" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "M" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_inv" )
temp <- rbind( temp, temp0 )
# Pen_viu
aux <- tas_7_6 %>% 
  as.data.frame() %>% 
  dplyr::group_by( sexo ) %>% 
  summarise( TERx = sum( ERx, na.rm = TRUE ), 
             TNx = sum( Nx, na.rm = TRUE ) ) %>% 
  ungroup()
temp0 <- tas_7_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "H" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "H" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "H" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_viu" )
temp <- rbind( temp, temp0 )
temp0 <- tas_7_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "M" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "M" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "M" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_viu" )
temp <- rbind( temp, temp0 )
# Pen_orf
aux <- tas_8_6 %>% 
  as.data.frame() %>% 
  dplyr::group_by( sexo ) %>% 
  summarise( TERx = sum( ERx, na.rm = TRUE ), 
             TNx = sum( Nx, na.rm = TRUE ) ) %>% 
  ungroup()
temp0 <- tas_8_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "H" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "H" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "H" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_orf" )
temp <- rbind( temp, temp0 )
temp0 <- tas_8_6 %>% 
  as.data.frame() %>% 
  dplyr::filter( sexo == "M" ) %>% 
  dplyr::mutate( we = ERx/aux$TERx[ aux$sexo == "M" ], 
                 wn = Nx/aux$TNx[ aux$sexo == "M" ], 
                 pe = x * we, 
                 pn = x * wn ) %>% 
  group_by( sexo ) %>% 
  summarise( pe = sum( pe, na.rm = TRUE ), 
             pn = sum( pn, na.rm = TRUE ) ) %>% 
  ungroup() %>% 
  dplyr::mutate( pob = "pen_orf" )
temp <- rbind( temp, temp0 )
