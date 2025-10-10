# Presentation-ready capability distribution plot
capability_dist_presentation <- phys_cap %>%
  separate(capability_id, into = c("movement", "quality", "expression"), sep = "_", remove = FALSE) %>%
  mutate(movement = str_to_title(movement)) %>%  # Capitalize movement names
  ggplot(aes(x = benchmarkPct, fill = movement)) +
  geom_histogram(bins = 25, alpha = 0.8, color = "white", size = 0.3) +
  facet_wrap(~movement, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Physical Capability Score Distributions by Movement Type",
       subtitle = "9,839 observations across 20 capability combinations",
       x = "Capability Score (% of baseline)", 
       y = "Count",
       caption = "Data: Chelsea FC Physical Capability Testing") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Remove legend since facets show movement types
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.caption = element_text(size = 10, hjust = 1, color = "gray50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

print(capability_dist_presentation)

# Export at presentation quality
ggsave("capability_distributions_presentation.png", 
       capability_dist_presentation, 
       width = 12, height = 8, dpi = 300, bg = "white")





# Create flowchart data
flowchart_data <- data.frame(
  step = 1:5,
  x = c(1, 2, 3, 4, 5),
  y = c(1, 1, 1, 1, 1),
  label = c("Raw Data\n(Capability Scores)", 
            "Time Series\nConversion", 
            "Model Fitting\n(NAIVE, Drift,\n ARIMA, ETS)", 
            "Forecasting\n(7-day horizon)", 
            "Validation\n(80/20 split)")
)

# Create flowchart
methodology_flowchart <- ggplot(flowchart_data, aes(x = x, y = y)) +
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, ymin = y - 0.3, ymax = y + 0.3),
            fill = "#E8F4FD", color = "#1F78B4", size = 1) +
  geom_text(aes(label = label), size = 3, fontface = "bold") +
  # Add arrows
  geom_segment(aes(x = x + 0.4, y = y, xend = x + 0.6, yend = y),
               arrow = arrow(length = unit(0.2, "cm")), 
               size = 1, color = "#1F78B4",
               data = flowchart_data[1:4, ]) +
  xlim(0.5, 5.5) + ylim(0.5, 1.5) +
  labs(title = "Time Series Forecasting Methodology",
       subtitle = "Systematic approach using fpp3 framework") +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"))

print(methodology_flowchart)

install.packages("DiagrammeR")
library(DiagrammeR)

methodology_diagram <- grViz("
digraph flowchart {
  # Node definitions
  node [fontname = Arial, fontsize = 10, shape = box, style = filled, fillcolor = lightblue]
  
  A [label = 'Raw Data\\nCapability Scores']
  B [label = 'Time Series\\nConversion']
  C [label = 'Model Fitting\\nNAIVE, Drift, ARIMA, ETS']
  D [label = 'Forecasting\\n7-day Horizon']
  E [label = 'Validation\\n80/20 split']
  
  # Edge definitions
  A -> B -> C -> D -> E
}
")

print(methodology_diagram)



# Create comparison framework diagram
framework_data <- data.frame(
  approach = rep(c("Time Series", "External Predictors"), each = 3),
  step = rep(c("Input", "Method", "Output"), 2),
  x = c(1, 1, 1, 2.2, 2.2, 2.2),  # CHANGED: 2.2 instead of 3
  y = c(3, 2, 1, 3, 2, 1),
  label = c(
    "Historical\nCapability Scores", 
    "NAIVE, Drift\nARIMA, ETS", 
    "Capability\nForecast",
    "GPS/HR Data +\nCapability Scores", 
    "ARIMA with\nExternal Regressors",
    "Capability\nForecast"
  )
)

comparison_framework <- ggplot(framework_data, aes(x = x, y = y)) +
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, ymin = y - 0.3, ymax = y + 0.3,
                fill = approach), alpha = 0.7, color = "black", linewidth = 1) +
  geom_text(aes(label = label), size = 3.5, fontface = "bold", lineheight = 0.9) +
  geom_segment(aes(x = x, y = y - 0.3, xend = x, yend = y - 0.7),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"), 
               linewidth = 1, color = "black",
               data = framework_data[framework_data$step != "Output", ]) +
  scale_fill_manual(values = c(
    "External Predictors" = "#FFE8E8",
    "Time Series" = "#E8F4FD"
  )) +
  labs(title = "Forecasting Method Comparison Framework",
       subtitle = "Pure time series vs ARIMA with GPS/HR external regressors") +
  xlim(0.3, 2.9) +  # CHANGED: tighter x limits
  ylim(0.5, 3.5) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(comparison_framework)
