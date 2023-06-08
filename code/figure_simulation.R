library(tidyverse)
experimental_results <- readRDS("G:/.shortcut-targets-by-id/101rwrFdbPyfMrdcJRlP6qlTOIx9sXTe_/sensitivity/results/experimental_results_0605.rds")
experimental_results <- unnest(experimental_results)

exp_results <- experimental_results %>% filter(
  num_classes == 2, num_observations == 100, 
  first_class_perc == 0.1
)



exp_results %>%
  pivot_longer(
    names_to = 'metric',
    values_to = 'value',
    cols = accuracy:f_measure
  ) %>% 
  dplyr::mutate(
    pred_approach = factor(pred_approach,
                           levels = c('uniform', 'proportional', "most_frequent"))) %>% 
  dplyr::filter(metric != 'recall', metric != "g_mean", pred_approach !='most_frequent') -> df

df_summary = df %>% 
  group_by(pred_approach, metric) %>% 
  summarise(
    med = median(value, na.rm=T),
    p95 = quantile(value, 0.95,  na.rm=T),
    std = sd(value, na.rm=T)
  )

df %>% 
  ggplot(
  aes(x = value)
) + 
  geom_histogram() +
  facet_grid(metric ~pred_approach) +
  theme_classic(base_size = 18) +
  geom_vline(
    data = df_summary,
    aes(xintercept = med),
    color = '#E7298A',
    linewidth = 1.5,
    linetype = 'solid'
  ) +
  geom_vline(
    data = df_summary,
    aes(xintercept = p95),
    color = '#66A61E',
    linewidth = 1.5,
    linetype = 'dotted'
  ) +
  geom_text(
    data = df_summary,
    aes(x = med-0.12, y = 4000),
    label = 'median',
    color = '#E7298A',
    size = 4
  ) +
  geom_text(
    data = df_summary,
    aes(x = p95+0.08, y = 4200),
    label = '95th',
    color = '#66A61E'
  ) +
  geom_curve(
    data = df_summary,
    aes(x = med, y = 3800, xend = med-0.12, yend = 4300),
    arrow = arrow(length = unit(0.05, 'npc') ),
    color = '#E7298A',
    size = 1.25
  ) +
  geom_curve(
    data = df_summary,
    aes(x = p95, y = 3600, xend = p95+0.07, yend = 3800),
    arrow = arrow(length = unit(0.05, 'npc') ),
    color = '#66A61E',
    size = 1.25
  ) +
  labs(x = '', y = '', caption = 'Results based on 10,000 simulation runs')

ggsave(filename = "../results/Figure3.jpg", width = 280, height = 180, units="mm", dpi = 700)


