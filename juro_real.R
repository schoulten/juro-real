
### Como calcular a taxa de juros real da economia brasileira?


# Feito por Fernando da Silva (GitHub: @schoulten)


# Iremos calcular a taxa de juro real, com base na equação de Fisher, por dois conceitos:

# 1) Ex-post: deflacionamento da SELIC acumulada dos últimos 12 meses pelo IPCA do mesmo período;

# 2) Ex-ante: deflacionamento da taxa do swap DI-Pré 360 dias pelo IPCA esperado para o mesmo período (Focus/BCB);


# Pacotes -----------------------------------------------------------------

library(ecoseries)  # coletar dados do IPEADATA
library(rbcb)       # coletar dados do BCB
library(sidrar)     # coletar dados do SIDRA/IBGE
library(tidyverse)  # família de pacotes para tratar dados
library(ggrepel)    # pacote gráfico


# Alternativa:

if(!require(pacman)) install.packages("pacman")
pacman::p_load("ecoseries", "sidrar", "tidyverse", "ggrepel", "scales")
if(!require(rbcb)) devtools::install_github("wilsonfreitas/rbcb")


# Coletar dados -----------------------------------------------------------

# Parâmetros da API IPEADATA
swap = 1900214364       # Taxa do swap DI-Pré 360 dias
expec_inf = 1693254712  # Expectativa média de Inflação (IPCA), tx. acum. p/ os próx. 12 meses


# Coletar dados do IPEADATA
dados_ipea <- series_ipeadata(swap, expec_inf, periodicity = c("M", "M")) %>%
  reduce(inner_join, by = "data") %>%
  rename(swap = valor.y, expec_inf = valor.x)


# SELIC acumulada dos últimos 12 meses
dados_selic <- get_series(c("selic" = 4189), start_date = min(dados_ipea$data)) %>%
  rename(data = date)


# IPCA - Variação acumulada em 12 meses
dados_ipca <- get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202") %>%
  select(data = "Mês (Código)", valor = "Valor") %>%
  mutate(data = paste0(data, "01") %>% as.Date(format = "%Y%m%d")) %>%
  drop_na()



# Cálculo -----------------------------------------------------------------

# Ex-ante
ex_ante <- dados_ipea %>%
  mutate(valor = (((1+(swap/100))/(1+(expec_inf/100)))-1)*100,
         id    = "Ex-ante")


# Ex-post
ex_post <- dados_ipca %>%
  select(data, ipca = valor) %>%
  inner_join(dados_selic, by = "data") %>%
  mutate(valor = (((1+(selic/100))/(1+(ipca/100)))-1)*100,
         id    = "Ex-post")


# Juntar os dados em um objeto
juros_real <- bind_rows(ex_ante[c(1,4:5)], ex_post[c(1,4:5)])



# Gráfico -----------------------------------------------------------------

# Eu gosto de personalizar meus gráficos :)
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
                   size        = 5,
                   fontface = "bold") +
  labs(title    = "Taxa de juros real - Brasil",
       subtitle = paste0("Taxa mensal (em % a.a.)", ", dados até ", format(tail(juros_real$data, 1), "%b/%Y")),
       x        = "",
       y        = NULL,
       caption  = "Fonte: B3, BCB e IBGE") +
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


# Salvar imagem
ggsave("./img/juro_real.jpg", units = "in", width = 7.875, height = 4.431, dpi = 1200)
