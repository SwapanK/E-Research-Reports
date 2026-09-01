########################################## setting working directory ########################################
rm(list = ls()) # clear the workspace
source_dir <- "J:/Property_Research/Projects/North America/USA/Hurricane/2026_NAHU_OlderVersion_RMSVerisk/HU_FL_comparisons/Verisk/"
setwd(source_dir) # if you have this directory
##############################################################################################################

# R script to reproduce catastrophe modeling plots
# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(ggrepel)
library(data.table)

# Read data
TIV <- fread('TIV_v13_byState_GReIED_USHU.csv')
AAL <- fread('AAL_v13_byPerils_byLOB_byState_GReIED_USHU.csv')
EP <- fread('EP_v13_byPerils_byLOB_byState_GReIED_USHU.csv')

# Summaries
TIV_tot <- sum(TIV$Total, na.rm = TRUE)
Exposure <- TIV %>% select(State, Commercial, Personal, Total)
Exposure <- Exposure %>% mutate(ExposureShare = Total / TIV_tot)

AAL_state <- AAL %>% group_by(State) %>% summarise(AAL = sum(Total, na.rm = TRUE))
AAL_tot <- sum(AAL_state$AAL)
AAL_state <- AAL_state %>% mutate(AALShare = AAL / AAL_tot)

metrics <- Exposure %>% left_join(AAL_state, by = 'State') %>% mutate(
  ExposureShare = Total / TIV_tot,
  AALShare = ifelse(is.na(AAL), 0, AAL) / AAL_tot,
  BurnCost = ifelse(Total > 0, AAL / Total, NA),
  EfficiencyIndex = ifelse(ExposureShare > 0, AALShare / ExposureShare, NA),
  AALminusExp = AALShare - ExposureShare
)

# Bubble plot
p1 <- ggplot(metrics, aes(x = ExposureShare, y = AALShare, size = EfficiencyIndex, label = State)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_text_repel(size = 3) +
  scale_size(range = c(3, 10)) +
  labs(title = 'Exposure Share vs AAL Share', x = 'Exposure Share', y = 'AAL Share') +
  theme_minimal()
print(p1)

# Burn cost by state
p2 <- ggplot(metrics, aes(x = reorder(State, BurnCost), y = BurnCost)) +
  geom_col() + coord_flip() +
  labs(title = 'Burn Cost by State', x = 'State', y = 'Burn Cost (AAL/TIV)') +
  theme_minimal()
print(p2)

# Efficiency index by state
p3 <- ggplot(metrics, aes(x = reorder(State, EfficiencyIndex), y = EfficiencyIndex)) +
  geom_col() + coord_flip() +
  labs(title = 'Risk Efficiency Index by State', x = 'State', y = 'Efficiency Index') +
  theme_minimal()
print(p3)

# AAL share vs exposure share gap
p4 <- ggplot(metrics, aes(x = reorder(State, AALminusExp), y = AALminusExp)) +
  geom_col() + coord_flip() +
  labs(title = 'AAL Share vs Exposure Share Gap', x = 'State', y = 'AALShare - ExposureShare') +
  theme_minimal()
print(p4)

# Tail leverage and amplification require EP data; similar computations using dplyr
# We skip detailed EP metrics in this script due to complexity, but similar approach can be used
