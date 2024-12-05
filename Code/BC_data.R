library(openxlsx)

df <- read.xlsx("~/OneDrive - University of Cambridge/Turner Lab/Data/Ramos_RTX_DP/Barcode Abundance/barcode_data.xlsx")

df <- df %>%
  filter(dose_code %in% c(0,4)) %>%
  filter(sample_no %in% c(3, 44)) %>%
  filter(rawcounts >= 10)

df <- df %>%
  group_by(sample) %>%
  mutate(normalised_counts = (rawcounts/sum(rawcounts) *100))


#################### clone count comp ###########
histo <- function(x){
  p <- ggplot(x, aes(x = normalised_counts)) + geom_histogram(color="black",
                                                fill="#CC9900") + labs(title='') +
    theme(panel.background = element_rect(fill = "gray98", colour = "white", size
                                          = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'dotted', colour =
                                            "black"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour =
                                            "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          plot.title = element_text(size=10, hjust = 0.5),
          legend.title = element_blank()) +
    scale_x_log10() +
    facet_wrap(~ condition)
}

histo_plots <- histo(df)
quartz()
histo_plots


############ Diversity index ############
data_summary <- df %>%
  group_by(condition) %>%
  summarize(shannon_entropy = diversity(normalised_counts, index = "shannon"))

bp <- ggbarplot(
  data_summary, x = "condition", y = "shannon_entropy", 
  color= "condition", palette = c("#00AFBB", "#E7B800"),
  position = position_dodge(0.8)
)

quartz()
bp


############## Clones of interest comparison ########

select <- df %>%
  filter(dose_code == 4) %>%
  filter(normalised_counts >= 1)

bc_check <- function(x) {
  
  # Determine order of barcode_ID based on mean_prop for "Rituximab" condition
  rituximab_order <- x %>%
    filter(condition == "Rituximab") %>%
    filter(normalised_counts >= 1) %>%
    arrange(normalised_counts) %>%  # Sort by mean_prop
    pull(barcode_ID)  # Extract the ordered barcode_ID
  
  # Ensure that rituximab_order contains unique barcode_IDs
  rituximab_order <- unique(rituximab_order)
  
  # Apply the ordering to the full dataset
  summarized_data <- x %>%
    mutate(barcode_ID = factor(barcode_ID, levels = rituximab_order))
  
  # Plotting
ggplot(summarized_data %>% 
           filter(barcode_ID != NA),
         aes(x = barcode_ID, y = normalised_counts)) +
    geom_col(color = "black", fill = "#CC9900") +  # Bars for mean proportion
    coord_flip() +  # Flip the coordinates for readability
    labs(title = '', x = 'Barcode ID', y = 'Mean Proportion') +  # Axis labels
    theme(
      panel.background = element_rect(fill = "gray98", colour = "white", linewidth = 2, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'dotted', colour = "black"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.text.x = element_text(size = 10, angle = 90, hjust = 1),  # Rotate x-axis labels
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 10, hjust = 0.5),
      legend.title = element_blank()
    ) +
    facet_wrap(~ condition, scales = "fixed")  # Fix the x-axis across all conditions
}

quartz()
bc_check(df)




