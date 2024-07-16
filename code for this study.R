setwd("E:/AUDPC calculation in R")
Bikas<-read.csv("E:/AUDPC calculation in R/audp.csv", header = TRUE)
Bikas
colnames(Bikas)
library(agricolae)
library(dplyr)
library(ggplot2)
library(tidyr)
###Bikas$Duration <- as.numeric(Bikas$Duration)# Pivot the data to long format
Bikas_long <- Bikas %>%
  pivot_longer(cols = c("Yellow.mosaic.disease..YMD.", "Leaf.curl..LC.", "Leaf.crinkle..LCrD."),
               names_to = "Disease", values_to = "Index")

# Create the line plot with facets
FB <- ggplot(Bikas_long, aes(x = Duration, y = Index, color = Disease, group = interaction(Disease, Replication))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Variety) +
  theme_minimal() +
  labs(title = "Disease Indices Over Time by Variety",
       x = "Duration (Weeks)",
       y = "Disease Score") +
  scale_x_continuous(breaks = 1:13, labels = c("1st week", "2nd week", "3rd week", "4th week", 
                                               "5th week", "6th week", "7th week", "8th week", 
                                               "9th week", "10th week", "11th week", "12th week", 
                                               "13th week")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot the graph
plot(FB)

#####calculation of the AUDPC
Bikas$DOS <- as.factor(Bikas$DOS)
Bikas$Duration <- as.factor(Bikas$Duration)
Bikas$Variety <- as.factor(Bikas$Variety)
Bikas$Replication <- as.factor(Bikas$Replication)
colnames(Bikas)
# Create a line plot
disease_vars <- c("Yellow.mosaic.disease..YMD.", "Leaf.curl..LC.", "Leaf.crinkle..LCrD.")
library(agricolae)
library(dplyr)
library(tidyr)

# Calculate absolute AUDPC for each disease, factor, and their interactions
library(dplyr)
library(tidyr)

# Reshape the data to long format
Bikas_long <- Bikas %>%
  pivot_longer(cols = c(`Yellow.mosaic.disease..YMD.`, `Leaf.curl..LC.`, `Leaf.crinkle..LCrD.`),
               names_to = "Disease", values_to = "Severity")
Bikas_long
# Calculate AUDPC manually
Bikas_audpc <- Bikas_long %>%
  group_by(DOS, Duration, Variety, Replication, Disease) %>%
  mutate(Severity_lag = lag(Severity, default = 0)) %>%
  summarize(AUDPC = sum((Severity + Severity_lag) / 2 * 1), .groups = "drop") # Trapezoidal rule
library(writexl)
write_xlsx(Bikas_audpc, "E:/AUDPC calculation in R/Bikas_audpc.xlsx")
print(Bikas_audpc)
library(ggplot2)


# Plot the AUDPC values
ggplot(Bikas_audpc, aes(x = Disease, y = AUDPC, fill = Disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(DOS ~ Duration + Variety + Replication) +
  labs(x = "Disease", y = "AUDPC", title = "AUDPC by Disease, DOS, Duration, Variety, and Replication") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
####replication removed plot 
ggplot(Bikas_audpc, aes(x = Disease, y = AUDPC, fill = Disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(DOS ~ Duration + Variety) +
  labs(x = "Disease", y = "AUDPC", title = "AUDPC by Disease, DOS, Duration, and Variety") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##########To reduced the Text size of all aspects 
ggplot(Bikas_audpc, aes(x = Disease, y = AUDPC, fill = Disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(DOS ~ Duration + Variety) +
  labs(x = "Disease", y = "AUDPC based on Trapezoidal Rule", title = "AUDPC by Disease, Days of sowing, Duration(weeks), and Variety") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Reduced axis text size
    axis.text.y = element_text(size = 8),  # Reduced axis text size
    axis.title.x = element_text(size = 8),  # Reduced axis title size
    axis.title.y = element_text(size = 8),  # Reduced axis title size
    plot.title = element_text(size = 8),  # Reduced plot title size
    strip.text = element_text(size = 8, angle = 90)  # Reduced facet label size and changed text angle
  )


#######To create the Box and wisker plot 
ggplot(Bikas_audpc, aes(x = Disease, y = AUDPC, fill = Disease)) +
  geom_boxplot() +
  facet_grid(DOS ~ Duration + Variety) +
  labs(x = "Disease", y = "AUDPC", title = "AUDPC by Disease, DOS, Duration, and Variety") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Reduced axis text size
    axis.text.y = element_text(size = 8),  # Reduced axis text size
    axis.title.x = element_text(size = 10),  # Reduced axis title size
    axis.title.y = element_text(size = 10),  # Reduced axis title size
    plot.title = element_text(size = 12)  # Reduced plot title size
  )


##### to add the mutate function for actual varibale changed calculation
library(dplyr)
library(tidyr)
library(ggplot2)

# Pivot longer and rename diseases
Bikas_long <- Bikas %>%
  pivot_longer(cols = c(`Yellow.mosaic.disease..YMD.`, `Leaf.curl..LC.`, `Leaf.crinkle..LCrD.`),
               names_to = "Disease", values_to = "Severity") %>%
  mutate(Disease = recode(Disease,
                          `Yellow.mosaic.disease..YMD.` = "Yellow mosaic disease",
                          `Leaf.curl..LC.` = "Leaf curl",
                          `Leaf.crinkle..LCrD.` = "Leaf crinkle"))

# Calculate AUDPC
Bikas_audpc <- Bikas_long %>%
  group_by(DOS, Duration, Variety, Replication, Disease) %>%
  mutate(Severity_lag = lag(Severity, default = 0)) %>%
  summarize(AUDPC = sum((Severity + Severity_lag) / 2 * 1), .groups = "drop")

# Plotting
Biku<-ggplot(Bikas_audpc, aes(x = Disease, y = AUDPC, fill = Disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(DOS ~ Duration + Variety) +
  labs(x = "Disease", y = "AUDPC based on Trapezoidal Rule", title = "AUDPC by Disease, Days of Sowing, Duration (weeks), and Variety") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Reduced axis text size
    axis.text.y = element_text(size = 8),  # Reduced axis text size
    axis.title.x = element_text(size = 8),  # Reduced axis title size
    axis.title.y = element_text(size = 8),  # Reduced axis title size
    plot.title = element_text(size = 8),  # Reduced plot title size
    strip.text = element_text(size = 8, angle = 90)  # Reduced facet label size and changed text angle
  )
ggsave("AUDPC_plot.jpg", plot = Biku, width = 15, height = 12, dpi = 600)





