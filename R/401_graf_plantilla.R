message( '\tEstableciendo plantilla de gráficos' )

# Preliminares -------------------------------------------------------------------------------------
tipo_letra <- "Linux Libertine"
base_family_2 <- "Linux Libertine"
tam_letra <- 9
tam_letra_tit <- 9
tam_letra_subtit <- 9
tam_letra_lab <- 9

graf_point_size <- 0.15
graf_line_size <- 0.3
graf_grid_major_size <- 0.25
graf_grid_minor_size <- 0.20
#graf_width <- 11
graf_width <- 14
graf_height <- 9
graf_units <- 'cm'
graf_dpi <- 200

## color definitions
main_color <- "white"
grid_color <- "grey60"
line_color <- parametros$iess_blue
text_color <- "black"

## color pie ambos sexos
pie_1_color <-rgb(37/256, 116/256, 64/256, .5)
pie_2_color <- rgb(0/256, 68/256, 148/256, .5)
pie_3_color <-rgb(37/256, 116/256, 64/256, .2)
pie_4_color <-rgb(0/256, 68/256, 148/256, .2)

## color pie femenino
pie_1_color_F <-rgb(256/256, 3/256, 0/256, .5)
pie_2_color_F <- rgb(200/256, 0/256, 0/256, .2)
pie_3_color_F <-rgb(90/256, 1/256, 1/256, .3)
pie_4_color_F <-rgb(12/256, 1/256, 3/256, .3)


## color pie masculino
pie_1_color_M <-rgb(60/256, 90/256, 256/256, .9)
pie_2_color_M <- rgb(2/256, 80/256, 150/256, .5)
pie_3_color_M <-rgb(60/256, 100/256, 190/256, .6)
pie_4_color_M <-rgb(0/256, 1/256, 3/256, .1)

## plot size definitions
custom_base_size <- 10
plot_margin <- c( 2, 2, 2, 2 )
panel_border_size <- rel( 0.8 )
strip_text_size <- rel( 0.8 )

#Estilo para pie con leyenda
plt_theme_legend_pie <- theme( 
  text = element_text( color = 'black', family = tipo_letra, size = tam_letra ),
  panel.grid.major.x = element_line( colour = "white", linewidth = graf_grid_major_size, linetype = 3 ),
  panel.grid.major.y = element_line( colour = "white", linewidth = graf_grid_major_size, linetype = 3 ),
  panel.grid.minor.x = element_line( colour = "white", linewidth = graf_grid_minor_size, linetype = 3 ),
  panel.grid.minor.y = element_line( colour = "white", linewidth = graf_grid_minor_size, linetype = 3 ),
  plot.margin = unit( plot_margin, "mm" ),
  legend.title = element_blank(),
  legend.position = 'bottom',
  legend.text = element_text( size = rel( 0.9), colour = 'black', face = 'plain', family = tipo_letra ),
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)

# Estilo sin leyenda -------------------------------------------------------------------------------
plt_theme <- theme( 
  text = element_text( color = 'black', family = tipo_letra, size = tam_letra ),
  
  panel.border = element_rect( colour = 'black', fill = NA, linetype = 'solid' ),
  panel.background = element_rect( linetype = "solid", fill = 'white' ),
  panel.spacing = unit( 0.25, "lines" ), # this controls spacing between graphs when faceting
  
  panel.grid.major.x = element_line( colour = "grey65", linewidth = graf_grid_major_size, linetype = 3 ),
  panel.grid.major.y = element_line( colour = "grey65", linewidth = graf_grid_major_size, linetype = 3 ),
  panel.grid.minor.x = element_line( colour = "grey65", linewidth = graf_grid_minor_size, linetype = 3 ),
  panel.grid.minor.y = element_line( colour = "grey65", linewidth = graf_grid_minor_size, linetype = 3 ),
  
  legend.title = element_blank(),
  legend.position = 'none',
  legend.text = element_text( size = tam_letra_lab, colour = 'black',
                              face = 'plain', family = tipo_letra ),
  # legend.margin = margin(t = 0.2,b=0.2, unit='cm'),
  # legend.key.size = unit( 0.25, "cm" ),
  plot.title = element_text( size = tam_letra_tit, colour = 'black',
                             face = 'bold', family = tipo_letra,
                             hjust = 0.5 , vjust = 0.5 ),
  plot.subtitle = element_text( size = tam_letra_subtit, colour = 'black',
                                face = 'plain', family = tipo_letra,
                                hjust = 0 , vjust = 0.5 ),
  axis.title.x = element_text( face = 'plain', angle = 0, colour = 'black', 
                               size = tam_letra_lab, family = tipo_letra, 
                               vjust = 0, hjust = 0.5,
                               margin = margin( t = 4, r = 0, b = 0, l = 0 ) ),
  axis.title.y = element_text( face = 'plain', angle = 90, colour = 'black', 
                               size = tam_letra_lab, family = tipo_letra, 
                               vjust = 0, hjust = 0.5,
                               margin = margin(t = 0, r = 10, b = 0, l = 0) ),
  axis.text.x = element_text( face = 'plain', angle = 0, colour = 'black', 
                              size = tam_letra, family = tipo_letra, 
                              vjust = 0, hjust = 0.5 ),
  axis.text.y = element_text( face = 'plain', angle = 0, colour = 'black', 
                              size = tam_letra, family = tipo_letra, 
                              vjust = 0, hjust = 0.5 ),
  
  plot.background = element_rect( color = main_color, fill = main_color  ),
  plot.margin = unit( plot_margin, "mm" ),
  
  legend.box.margin = margin(-20,0,-6,0)
)

# Estilo con leyenda -------------------------------------------------------------------------------
plt_theme_legend <- theme( 
  text = element_text( color = 'black', family = tipo_letra, size = tam_letra ),
  
  panel.border = element_rect( colour = 'black', fill = NA, linetype = 'solid' ),
  panel.background = element_rect( linetype = "solid", fill = 'white' ),
  panel.spacing = unit( 0.25, "lines" ), # this controls spacing between graphs when faceting
  
  panel.grid.major.x = element_line( colour = "grey65", linewidth = graf_grid_major_size, linetype = 3 ),
  panel.grid.major.y = element_line( colour = "grey65", linewidth = graf_grid_major_size, linetype = 3 ),
  panel.grid.minor.x = element_line( colour = "grey65", linewidth = graf_grid_minor_size, linetype = 3 ),
  panel.grid.minor.y = element_line( colour = "grey65", linewidth = graf_grid_minor_size, linetype = 3 ),
  
  plot.background = element_rect( color = main_color, fill = main_color  ),
  plot.margin = unit( plot_margin, "mm" ),
  
  legend.title = element_blank(),
  legend.direction = "horizontal",
  legend.position = 'bottom',
  legend.text = element_text( size = tam_letra_lab, colour = 'black', 
                              face = 'plain', family = tipo_letra ),
  # legend.margin = unit( 0.5, "cm" ),
  # legend.key.size = unit( 0.25, "cm" ),
  plot.title = element_text( size = tam_letra_tit, colour = 'black',
                             face = 'bold', family = tipo_letra,
                             hjust = 0.5 , vjust = 0.5 ),
  plot.subtitle = element_text( size = tam_letra_subtit, colour = 'black',
                                face = 'plain', family = tipo_letra,
                                hjust = 0 , vjust = 0.5 ),
  axis.title.x = element_text( face = 'plain', angle = 0, colour = 'black', 
                               size = tam_letra_lab, family = tipo_letra, 
                               vjust = 0, hjust = 0.5,
                               margin = margin( t = 10, r = 0, b = 0, l = 0 ) ),
  axis.title.y = element_text( face = 'plain', angle = 90, colour = 'black', 
                               size = tam_letra_lab, family = tipo_letra, 
                               vjust = 0, hjust = 0.5,
                               margin = margin(t = 0, r = 10, b = 0, l = 0) ),
  axis.text.x = element_text( face = 'plain', angle = 0, colour = 'black', 
                              size = tam_letra, family = tipo_letra, 
                              vjust = 0, hjust = 0.5 ),
  axis.text.y = element_text( face = 'plain', angle = 0, colour = 'black', 
                              size = tam_letra, family = tipo_letra, 
                              vjust = 0, hjust = 0.5 )
)

# Theme Time-Series --------------------------------------------------------------------------------
# Almost the same as scatterplot theme but legend title is blank considering that the variables are
# often of different types (legend would plot the generic term "variable")

iess_timeseries_theme <-
  ggthemes::theme_foundation( base_size = custom_base_size, base_family = tipo_letra ) +
  theme(
    # Plot -----------------------------------------------------------------------------------------
    text = element_text( color = 'black', family = tipo_letra, size = tam_letra ),
    line = element_line( color = line_color ),
    plot.title = element_text( hjust = 0, size = tam_letra_tit, family = tipo_letra, face = 'bold' ),
    plot.subtitle = element_text( hjust = 0, size = tam_letra_subtit, family = tipo_letra ),
    # plot.caption = element_text( size = plot_caption_size ), # center
    plot.background = element_rect( color = main_color, fill = main_color  ),
    rect = element_rect( fill = main_color, color = NA, linetype = "solid" ),
    plot.margin = unit( plot_margin, "mm" ),
    
    # Panel ----------------------------------------------------------------------------------------
    panel.border = element_rect( colour = line_color, fill = NA, linetype = 'solid' ),
    panel.background = element_rect( linetype = "blank" ),
    panel.spacing = unit( 0.25, "lines" ), # this controls spacing between graphs when faceting
    
    # Grid -----------------------------------------------------------------------------------------
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line( color = grid_color, linewidth = graf_grid_major_size, linetype = 3 ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line( color = grid_color, linewidth = graf_grid_major_size, linetype = 3 ),
    
    # Axis -----------------------------------------------------------------------------------------
    axis.text = element_text( size = tam_letra, family = tipo_letra ),
    # axis.text.x = element_text( vjust = 0, margin = margin( t = tam_letra_lab, unit = "pt" ) ),
    # axis.text.y = element_text( hjust = 0, margin = margin( r = tam_letra_lab, unit = "pt" ) ),
    axis.title = element_text( size = tam_letra_lab, family = tipo_letra ),
    axis.title.x = element_text(),
    axis.title.y = element_text( angle = 90 ),
    # axis.line = element_line( linewidth = axis_line_size ),
    axis.line.y = element_blank( ),
    axis.ticks = element_line( ),
    axis.ticks.y = element_blank( ),
    # axis.ticks.length = unit(  -1/32, "in" ),
    # axis.ticks.length = unit(  -custom_base_size * 0.5, "points" ),
    
    # Legend ---------------------------------------------------------------------------------------
    legend.position = 'bottom',
    # legend.margin = unit( 0.5, "cm" ),
    # legend.justification = "right",
    legend.direction = NULL,
    # legend.background = element_rect( linetype = "blank" ),
    legend.spacing = unit( custom_base_size * 1.5, "points" ),
    legend.key = element_rect( linetype = "blank" ),
    # legend.key.size = unit( 0.25, "cm" ),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text( size = tam_letra_lab, colour = 'black', 
                                face = 'plain', family = tipo_letra ),
    legend.text.align = NULL,
    legend.title = element_blank(),
    legend.title.align = NULL,
    
    # Strip ----------------------------------------------------------------------------------------
    strip.background = element_rect( fill = parametros$iess_green,
                                     color = "red",
                                     linetype = "blank" ),
    strip.text = element_text( size = strip_text_size, color = "white", family = tipo_letra ),
    strip.text.x = element_text( ),
    strip.text.y = element_text( angle = -90 ),
    
    complete = TRUE
  )

Gris = 'gray85'
Azul = rgb(0/256, 68/256, 148/256)
Verde = rgb(37/256, 116/256, 64/256)
NegroTransparente = rgb(0, 0, 0, .5)
textwidth = (210-30-25)/25.4

# Colores para los gráficos
if( parametros$hostname %in% c('PCUIOMTDAIE6382' #Cristian G
                               
)){
  Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
  
}else if ( parametros$hostname %in% c('PCUIOMTDAID1P5' #Jimmy Montoya
) ){
  Sys.setenv(R_GSCMD = "C:/Program Files (x86)/gs10.02.1/bin/gswin64c.exe")
}