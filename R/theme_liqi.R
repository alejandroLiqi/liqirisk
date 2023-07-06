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

    ggplot2::theme(


    line =               ggplot2::element_line(
                           colour = "black", size = base_line_size,
                           linetype = 1, lineend = "butt"
                         ),
    rect =               ggplot2::element_rect(
                           fill = "white", colour = "black",
                           size = base_rect_size, linetype = 1
                         ),
    text =               ggplot2::element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = ggplot2::margin(), debug = FALSE
                         ),



    axis.line =          ggplot2::element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          ggplot2::element_text(size = ggplot2::rel(0.8), colour = "#000729"),
    axis.text.x =        ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         ggplot2::element_blank(),
    axis.ticks.length =  ggplot2::unit(half_line / 3, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x =       ggplot2::element_text(
                           colour = '#000729',
                           family = base_family,
                           size = ggplot2::rel(0.8),
                           margin = ggplot2::margin(t = half_line / 2),
                           vjust = 1,
                           hjust = 1
                         ),
    axis.title.x.top =   ggplot2::element_text(
                           margin = ggplot2::margin(b = half_line / 2),
                           vjust = 0
                         ),
    axis.title.y =       ggplot2::element_text(
                           colour = '#000729',
                           family = base_family,
                           size = ggplot2::rel(0.8),
                           angle = 90,
                           margin = ggplot2::margin(r = half_line / 2),
                           vjust = 1,
                           hjust = 1
                         ),
    axis.title.y.right = ggplot2::element_text(
                           angle = -90,
                           margin = ggplot2::margin(l = half_line / 2),
                           vjust = 0
                         ),



    legend.background =  ggplot2::element_rect(fill = "#f6f6f6", colour = NA),
    legend.spacing =     ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.key =         ggplot2::element_rect(fill = '#f6f6f6', colour = '#f6f6f6'),
    legend.key.size =    ggplot2::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        ggplot2::element_text(family = base_family, size = ggplot2::rel(0.8), colour = '#000729'),
    legend.text.align =  NULL,
    legend.title =       ggplot2::element_text(hjust = 0, colour = '#000729'),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_rect(fill = "#f6f6f6", colour = NA),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),




    panel.background =   ggplot2::element_rect(fill = "#f6f6f6", colour = NA),
    panel.border =       ggplot2::element_blank(),
    panel.grid.major =   ggplot2::element_line(colour = "#DEDEDE", size = ggplot2::rel(0.85)),
    panel.grid.minor =   ggplot2::element_line(colour = "#DEDEDE", size = ggplot2::rel(0.45)),
    panel.spacing =      ggplot2::unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,



    strip.background =   ggplot2::element_blank(),
    strip.text =         ggplot2::element_text(
                           colour = "grey10",
                           size = ggplot2::rel(0.8),
                           margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
                         ),
    strip.text.x =       NULL,
    strip.text.y =       ggplot2::element_text(angle = -90),
    strip.text.y.left =  ggplot2::element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),




    plot.background =    ggplot2::element_rect(fill = "#f6f6f6", color = NA),
    plot.title =         ggplot2::element_text( # font size "large"
                           colour = '#ffbe00',
                           family = base_family,
                           face = 'plain',
                           size = ggplot2::rel(1.65),
                           hjust = 0, vjust = 1,
                           margin = ggplot2::margin(b = half_line)
                         ),
    plot.title.position = "panel",
    plot.subtitle =      ggplot2::element_text( # font size "regular"
                           colour = '#000729',
                           family = base_family,
                           hjust = 0, vjust = 1,
                           margin = ggplot2::margin(b = half_line * 3)
                         ),
    plot.caption =       ggplot2::element_text( # font size "small"
                           colour = '#000729',
                           family = base_family,
                           size = ggplot2::rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = ggplot2::margin(t = half_line)
                         ),
    plot.caption.position = "panel",
    plot.tag =           ggplot2::element_text(
                           size = ggplot2::rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',
    plot.margin =        ggplot2::margin(half_line * 3, half_line * 3, half_line * 3, half_line * 3),




    complete = TRUE

  )

}
