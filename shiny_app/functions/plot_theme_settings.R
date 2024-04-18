plot_theme_settings <- function() {
  theme(
    axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )
}
