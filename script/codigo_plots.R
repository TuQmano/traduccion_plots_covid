library(tidyverse)
library(covdata)
library(prismatic)
library(ggsci)
library(paletteer)
library(ggrepel)

data <- covnat


## Countries to highlight
focus_cn <- c("CHN", "DEU", "GBR", "USA", "IRN", "JPN",
              "KOR", "ITA", "FRA", "ESP", "CHE", "TUR", "ARG")

## Colors
cgroup_cols <- c(prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)[1:length(focus_cn)], "gray70")



# CASOS POSITIVOS
plot_casos <- data %>%
  filter(cu_cases > 99) %>%
  mutate(days_elapsed = date - min(date),
         end_label = ifelse(date == max(date), cname, NA),
         end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran",
                            `Korea, Republic of` = "South Korea",
                            `United Kingdom` = "UK"),
         cname = recode(cname, `United States` = "USA",
                        `Iran, Islamic Republic of` = "Iran",
                        `Korea, Republic of` = "South Korea",
                        `United Kingdom` = "UK"),
         end_label = case_when(iso3 %in% focus_cn ~ end_label,
                               TRUE ~ NA_character_),
         cgroup = case_when(iso3 %in% focus_cn ~ iso3,
                            TRUE ~ "ZZOTHER")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases,
                       color = cgroup, label = end_label,
                       group = cname)) +
  geom_line(size = 0.5) +
  geom_text_repel(nudge_x = 0.75,
                  segment.color = NA) +
  guides(color = FALSE) +
  scale_color_manual(values = cgroup_cols) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     breaks = 2^seq(4, 19, 1),
                     trans = "log2") +
  labs(x = "Días desde el 100º caso confirmado",
       y = "Número acumulado de casos reportados (log2 scale)",
       title = "Número acumulado de casos reportados por COVID-19",
       subtitle = paste("Países seleccionados | Datos de ECDC al", format(max(covnat$date), "%A %e de %B de %Y")),
       caption = "Código: Kieran Healy @kjhealy / Data: https://www.ecdc.europa.eu/") +
  hrbrthemes::theme_ipsum()


plot_casos

dev.off()

ggsave(plot= last_plot(), filename = "plots/casos_cum.png", width = 10, height = 7)


interactive_casos <- plotly::ggplotly(plot_casos)


htmlwidgets::saveWidget(interactive_casos, file = "interactive_casos.html")



# MUERTES


plot_muertes <- data %>%
  filter(cu_deaths > 0) %>%
  mutate(days_elapsed = date - min(date),
         end_label = ifelse(date == max(date), cname, NA),
         end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran",
                            `Korea, Republic of` = "South Korea",
                            `United Kingdom` = "UK"),
         cname = recode(cname, `United States` = "USA",
                        `Iran, Islamic Republic of` = "Iran",
                        `Korea, Republic of` = "South Korea",
                        `United Kingdom` = "UK"),
         end_label = case_when(iso3 %in% focus_cn ~ end_label,
                               TRUE ~ NA_character_),
         cgroup = case_when(iso3 %in% focus_cn ~ iso3,
                            TRUE ~ "ZZOTHER")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_deaths,
                       color = cgroup, label = end_label,
                       group = cname)) +
  geom_line(size = 0.5) +
  geom_text_repel(nudge_x = 0.75,
                  segment.color = NA) +
  guides(color = FALSE) +
  scale_color_manual(values = cgroup_cols) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     breaks = 2^seq(4, 19, 1),
                     trans = "log2") +
  labs(x = "Días desde la primera muerte confirmada",
       y = "Número acumulado de muertes reportadas (log2 scale)",
       title = "Número acumulado de muertes reportadas por COVID-19",
       subtitle = paste("Países seleccionados | Datos de ECDC al", format(max(covnat$date), "%A %e de %B de %Y")),
       caption = "Código: Kieran Healy @kjhealy / Data: https://www.ecdc.europa.eu/") +
  hrbrthemes::theme_ipsum()


plot_muertes

dev.off()

ggsave(plot= last_plot(), filename = "plots/muertes_cum.png", width = 10, height = 7)

interactive_muertes <- plotly::ggplotly(plot_muertes)


htmlwidgets::saveWidget(interactive_muertes, file = "interactive_decesos.html")
