
### Como calcular a taxa de juros real da economia brasileira?


# Feito por Fernando da Silva (GitHub: @schoulten)


# Iremos calcular a taxa de juro real (juro nominal deflacionado pelo IPCA) por dois conceitos:

# 1) Ex-post: deflacionamento da SELIC acumulada dos últimos 12 meses pelo IPCA do mesmo período

# 2) Ex-ante: deflacionamento da taxa do swap DI-Pré 360 dias pelo IPCA esperado para o mesmo período (Focus/BCB)


# Pacotes -----------------------------------------------------------------

library(ecoseries)  # coletar dados do IPEADATA
library(rbcb)       # coletar dados do BCB
library(sidrar)     # coletar dados do SIDRA/IBGE
library(tidyverse)  # família de pacotes para tratar e manipular dados
library(ggrepel)    # pacote gráfico



# Coletar dados -----------------------------------------------------------

# Taxa do swap DI-Pré 360 dias
dados_swap <- series_ipeadata("1900214364", periodicity = "M")$serie_1900214364


# Expectativa média de Inflação - IPCA - taxa acumulada para os próximos doze meses
dados_inf_expec_m <- series_ipeadata("1693254712", periodicity = "M")$serie_1693254712


# SELIC acumulada dos últimos 12 meses
dados_selic_aa <- get_series(c("selic_aa" = 4189), start_date = "2001/07/01") %>%
  rename(data = date)


# IPCA - Variação acumulada em 12 meses
dados_ipca <- get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202") %>%
  select(data = "Mês (Código)", valor = "Valor") %>%
  mutate(data = paste0(data, "01") %>% as.Date(format = "%Y%m%d")) %>%
  drop_na()



# Cálculo -----------------------------------------------------------------

# Ex-ante
ex_ante <- inner_join(dados_swap, dados_inf_expec_m, by = "data") %>%
  rename(swap = valor.x, inf_expec = valor.y) %>%
  mutate(valor = (((1+(swap/100))/(1+(inf_expec/100)))-1)*100,
         id    = "Ex-ante")


# Ex-post
ex_post <- dados_ipca %>%
  select(data, ipca_12m = valor) %>%
  inner_join(dados_selic_aa, by = "data") %>%
  mutate(valor = (((1+(selic_aa/100))/(1+(ipca_12m/100)))-1)*100,
         id    = "Ex-post")


# Juntar os dados em um objeto
juros_real <- bind_rows(ex_ante[c(1,4:5)], ex_post[c(1,4:5)])



# Gráfico -----------------------------------------------------------------

juros_real %>%
  ggplot(aes(x = data, y = valor, colour = id)) +
  geom_hline(yintercept = 0, size = .8, color = "gray50") +
  geom_line(size = 1.8) + 
  geom_label_repel(data        = subset(juros_real, data == max(data)),
                   aes(label   = paste(scales::comma(valor, decimal.mark = ",", accuracy = 0.01))),
                   show.legend = FALSE,
                   nudge_x     = 50,
                   nudge_y     = 7,
                   force       = 10,
                   size        = 5) +
  labs(title    = "Taxa de juros real - Brasil",
       subtitle = "Taxa mensal (em % a.a.)",
       x        = "",
       y        = NULL,
       caption  = "Fonte: GECE/FURG com dados de B3, BCB e IBGE") +
  scale_color_manual(values = c("#233f91", "red4")) +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
  theme_minimal() +
  theme(plot.title         = element_text(color = "#233f91", size = 26, face = "bold"),
        plot.subtitle      = element_text(face = "bold", colour = "gray20", size = 16),
        plot.background    = element_rect(fill = "#eef1f7", colour = "#eef1f7"),
        plot.caption       = element_text(size = 10, face = "bold", colour = "gray20"),
        legend.background  = element_blank(),
        legend.position    = c(0.9, 0.72),
        legend.title       = element_blank(),
        legend.text        = element_text(face = "bold", colour = "gray20", size = 16),
        panel.background   = element_rect(fill = "#eef1f7", colour = "#eef1f7"),
        panel.grid         = element_line(colour = "gray85"),
        panel.grid.minor.x = element_blank(),
        axis.text          = element_text(size = 13, face = "bold"))


ggsave("./img/juro_real.jpg", units = "in", width = 7.875, height = 4.431, dpi = 1200)
