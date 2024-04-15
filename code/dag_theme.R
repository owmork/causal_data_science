theme_dag_cds <- function (base_size = 12, base_family = "", ...) {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    ggplot2::theme(axis.text = ggplot2::element_blank(), 
                   axis.title = ggplot2::element_blank(), 
                   axis.ticks = ggplot2::element_blank(), 
                   panel.grid.major = ggplot2::element_line(colour = "#1E2938"), 
                   panel.grid.minor = ggplot2::element_line(colour = "#1E2938"),
                   panel.background = element_rect(fill = "#1E2938"),
                   plot.background = element_rect(fill = "#1E2938"), 
                   line = element_line(color = "white"), 
                   complete = TRUE)
}
