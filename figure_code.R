# Libraries
library(readxl) # importing .xlsx files
library(tidyverse) # general data manipulation, visualisation, etc
library(lubridate) # working with date-data

# Data import and transformation
df_full = read_excel("df_tv.xlsx")
df_full = df_full %>% 
  filter(n_vm != 0, m_vm != 0) %>% # exclude rows with zeroes
  mutate(aeg = date(aeg), # into date format
         kuu = month(aeg), # extract month (nr)
         aasta = year(aeg), # extract year
         maakond = str_replace(maakond, " maakond", "") # remove " maakond" from county name
         ) %>%
  filter(aasta != 2016) # exclude 2016, because it has incomplete data

# Data that have (1) selected counties and (2) aggregated work capability allowance data
df = df_full %>% filter(maakond %in% c("Harju", "Tartu", "Ida-Viru", "Pärnu"),
                        tv_ulatus == "Kokku") 

# Data across (selected) counties and years
df_aasta = df %>% select(maakond, aasta, n_vm, m_vm) %>% 
  group_by(maakond, aasta) %>%
  summarize(N = n_vm, # N of recipients for each month per year per county
            M = m_vm, # average allowance per each month per year per county
            N_nvm = mean(n_vm), # average number of recipients per county and year
            M_mvm = mean(m_vm) # average allowance per county and year
            ) %>%
  mutate(maakond = as.factor(maakond),
         maakond = fct_relevel(maakond, c("Harju", "Ida-Viru", "Tartu", "Pärnu")))

# Palette - something that works well with the poster
palette = c("#35382f", "#3f779d", "#499f14", "#b4cce0")

# Plot
pdf(file = "figure1.pdf", width = 7, height = 4)
ggplot(df_aasta, aes(x = M, y = N, col = maakond)) + 
  geom_vline(xintercept = c(250, 300), size = 0.5, color = "#d9d9d9", linetype = "dashed") +
  geom_point(alpha = 0.2) + # the averages per year/county
  scale_color_manual(values = palette) +
  geom_point(data = df_aasta, aes(x = M_mvm, y = N_nvm, col = maakond), 
             size = 3, shape = 3) + # each month per year per county
  labs(x = NULL, y = NULL) + # axes and title produced in Canva
#  labs(x = "Average size of allowance (€)", 
 #      y = "(Average) Number of recipients",
     #  title = "The increase of the average work capability allowance\nand the number of recipients over time",
     #    "Töövõimetustoetuste suuruse ja toetuse saajate arvu kasv ajas",
     #  subtitle = "The number of recipients is less dependent on month (individual dot) of the year"
  #   ) + 
  scale_y_continuous(limits = c(0, 26000), breaks = seq(0, 26000, 5000)) +
  theme_classic() +
 facet_grid(.~aasta) +
  theme(legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        strip.background = element_rect(colour = "white",
                                        fill = "#e9f0f0"), # remove boxes from facet labels
        axis.line = element_line(size = .13, color = "#40545d"),
        axis.ticks.x = element_line(size = 0.5, color = "#40545d"),
        axis.ticks.y = element_line(size = 0.5, color = "#40545d"),
        axis.text = element_text(color = "#40545d"), 
        panel.spacing.x = unit(1, "lines"), # panel spacing
        plot.margin=unit(c(t = 0.5, r = 1.5, b = 0.5,l = 0.5),"cm") # margin (whitespace around the plot)
        )
dev.off()

# the BIG number value (% increase of monthly work allowance recipients in 2021 compared to 2017)
df_value = df_aasta %>% filter(aasta %in% c(2017, 2021)) %>%
  group_by(aasta) %>%
  summarize(N_avg = mean(N))

(df_value$N_avg[2] - df_value$N_avg[1])/df_value$N_avg[1]*100 # % of monthly recipient increase
