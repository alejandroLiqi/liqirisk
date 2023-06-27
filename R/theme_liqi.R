#' Liqi's ggpolt2 Theme
#'
#' Contains the parameters and colour for a standarized plot theme.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#'
#' @export
#'
#' @importFrom grid unit
#' @importFrom data.table data.table
#' @importFrom sysfonts font_add_google
#' @import ggplot2
#'
#'
#'
theme_liqi = function(base_size = 12, base_family = 'open-sans', base_line_size = base_size/22, base_rect_size = base_size/22) {

half_line = base_size / 2

t =

   theme(


    line =               element_line(
                           colour = "black", size = base_line_size,
                           linetype = 1, lineend = "butt"
                         ),
    rect =               element_rect(
                           fill = "white", colour = "black",
                           size = base_rect_size, linetype = 1
                         ),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),



    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = "#000729"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_blank(),
    axis.ticks.length =  unit(half_line / 3, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x =       element_text(
                           colour = '#000729',
                           family = base_family,
                           size = rel(0.8),
                           margin = margin(t = half_line / 2),
                           vjust = 1,
                           hjust = 1
                         ),
    axis.title.x.top =   element_text(
                           margin = margin(b = half_line / 2),
                           vjust = 0
                         ),
    axis.title.y =       element_text(
                           colour = '#000729',
                           family = base_family,
                           size = rel(0.8),
                           angle = 90,
                           margin = margin(r = half_line / 2),
                           vjust = 1,
                           hjust = 1
                         ),
    axis.title.y.right = element_text(
                           angle = -90,
                           margin = margin(l = half_line / 2),
                           vjust = 0
                         ),



    legend.background =  element_rect(fill = "#f6f6f6", colour = NA),
    legend.spacing =     unit(2 * half_line, "pt"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      margin(half_line, half_line, half_line, half_line),
    legend.key =         element_rect(fill = '#f6f6f6', colour = '#f6f6f6'),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(family = base_family, size = rel(0.8), colour = '#000729'),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0, colour = '#000729'),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_rect(fill = "#f6f6f6", colour = NA),
    legend.box.spacing = unit(2 * half_line, "pt"),




    panel.background =   element_rect(fill = "#f6f6f6", colour = NA),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "#DEDEDE", size = rel(0.85)),
    panel.grid.minor =   element_line(colour = "#DEDEDE", size = rel(0.45)),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,



    strip.background =   element_blank(),
    strip.text =         element_text(
                           colour = "grey10",
                           size = rel(0.8),
                           margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
                         ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),




    plot.background =    element_rect(fill = "#f6f6f6", color = NA),
    plot.title =         element_text( # font size "large"
                           colour = '#ffbe00',
                           family = base_family,
                           face = 'plain',
                           size = rel(1.65),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line)
                         ),
    plot.title.position = "panel",
    plot.subtitle =      element_text( # font size "regular"
                           colour = '#000729',
                           family = base_family,
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line * 3)
                         ),
    plot.caption =       element_text( # font size "small"
                           colour = '#000729',
                           family = base_family,
                           size = rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption.position = "panel",
    plot.tag =           element_text(
                           size = rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',
    plot.margin =        margin(half_line * 3, half_line * 3, half_line * 3, half_line * 3),




    complete = TRUE

  )

}
