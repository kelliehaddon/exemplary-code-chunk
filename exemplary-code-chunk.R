# Exemplary Code Chunk

# * Preparation * ----
## Load libraries ----
library(tidyverse)
library(haven)
library(patchwork)

## Load data ----
la = read_dta('la_turnout_basic.dta')


# * Prepare data * ----
## White treatment ----
white_treatment = la %>%
  filter(year >= 1950 & year <= 1970, # filter for years of interest
         understandingclause2 == 1) %>% # filter for treatment status of interest
  group_by(year) %>% # group observations by year
  summarize(reg_rate = mean(whiteregrate, na.rm = T)) # calculate mean

white_treatment = white_treatment %>%
  mutate(status = 'Treated', # create defining variables for appended data
         race = 'White')

## White control ----
white_control = la %>%
  filter(year >= 1950 & year <= 1970,
         understandingclause2 == 0) %>%
  group_by(year) %>%
  summarize(reg_rate = mean(whiteregrate, na.rm = T))

white_control = white_control %>%
  mutate(status = 'Control',
         race = 'White')

## Black treatment ----
black_treatment = la %>%
  filter(year >= 1950 & year <= 1970,
         understandingclause2 == 1) %>%
  group_by(year) %>%
  summarize(reg_rate = mean(blackregrate, na.rm = T))

black_treatment = black_treatment %>%
  mutate(status = 'Treated',
         race = 'Black')

## Black control ----
black_control = la %>%
  filter(year >= 1950 & year <= 1970,
         understandingclause2 == 0) %>%
  group_by(year) %>%
  summarize(reg_rate = mean(blackregrate, na.rm = T))

black_control = black_control %>%
  mutate(status = 'Control',
         race = 'Black')

## Combine data ----
df = bind_rows(white_treatment, white_control, black_treatment, black_control) # append data


# * Create plot * ----
df %>%
  ggplot(aes(x = year, y = reg_rate, # identify independent and dependent variables
             color = status, shape = status, linetype = status)) + # create visual elements based on treatment status
  facet_wrap(~ race) + # separate plots by race
  geom_rect(aes(xmin=1954, xmax=1965, ymin=-Inf, ymax=Inf), # highlight years that the Understanding Clause was in affect
            color = 'white', fill = 'grey95') +
  geom_line(lwd = .7) + # add line to plot
  geom_point(color = 'black') + # add points to plot
  scale_shape_manual(values = c(16, 17)) + # choose shape type for points
  scale_color_manual(values = c('orange', 'turquoise3')) + # choose line color for lines
  scale_y_continuous(limits = c(0, 1), # scale plot from 0% to 100%
                     labels = scales::label_percent()) + # display y-axis as percentages rather than proportions
  guides(color = guide_legend(override.aes = list(size = 2))) + # increase size of legend elements
  labs(x = 'Year', y = 'Registration Rate', # add labels
       title = 'FIGURE 2.   Proportion of Registered Voters by Race and by Understanding Clause Status. Treated \nParishes Enforced the Understanding Clause and Control Parishes Did Not',
       caption = 'Note: Treated parishes enforced the Understanding Clause and control parishes did not. Gray shading indicates years \nwhen the Understanding Clause was potentially enforced in treated parishes.') +
  theme_minimal() + # add theme
  theme(legend.title = element_blank(), # remove legend title
        panel.grid = element_blank(), # remove gridlines
        panel.border = element_rect(color = 'black', fill = NA), # format panel border
        panel.spacing = unit(2, "lines"), # add spacing between facets
        axis.text = element_text(size = 11, color = 'black'), # format axis text
        axis.title = element_text(size = 11), # format axis title
        legend.text = element_text(size = 11), # format legend text
        strip.text.x = element_text(size = 11, face = 'bold'), # format titles of facets
        plot.title = element_text(face = 'bold', size = 14, hjust = 0), # format title of plot
        plot.caption = element_text(size = 11, hjust = 0, face = 'italic')) # format caption of plot
