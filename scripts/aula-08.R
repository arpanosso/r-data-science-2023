# Data: 14/12/2023
# Programado por: Alan R Panosso
# Carregar os pacotes
library(tidyverse)

# Para a obtenção da análise de variância de um experimento em
# parcelas subdivididas, vamos utilizar os dados obtidos do
# trabalho intitulado “Efeito de épocas de plantio, sobre
# várias características agronômicas na cultura da soja
# (Glycine max. (L.) Merril), variedades Santa Rosa e Viçoja, em
# Jaboticabal, SP”, realizado por K. YUYAMA (1976).
# Foram utilizadas 8 épocas de plantio no ano de 1974
# (20/10, 30/10, 10/11, 20/11, 30/11, 10/12, 20/12 e 30/12)
# e duas variedade de soja (V1 = Viçoja e V2 = Santa Rosa).
# O ensaio foi montado de acordo com o delineamento em parcelas
# subdivididas, com as épocas de plantio nas parcelas, e as
# variedades nas subparcelas.
# Os resultados obtidos para produção de grãos (t/ha)

# Entrada de dados
dados_soja <- read_rds("data/soja_dbc.rds") %>%
  mutate(
    trat = interaction(epoca, variedade)
  )
glimpse(dados_soja)

# Análise de Variância
epoca <- dados_soja %>% pull(epoca) %>% as_factor()
variedade <- dados_soja %>% pull(variedade) %>% as_factor()
bloco <- dados_soja %>% pull(bloco) %>% as_factor()
y <- dados_soja %>% pull(y)

ExpDes.pt::psub2.dbc(epoca,variedade,bloco,y,
                     fac.names = c("Epoca","Varie"))

# Diagnóstico
# Criar tabela de médias
tab_media <- dados_soja %>%
  group_by(epoca,variedade) %>%
  summarise(
    media = mean(y,na.rm = TRUE)
  )

# criar gráfico da interação
tab_media %>%
  ggplot(aes(x=epoca, y =media,
             color=as_factor(variedade))) +
  geom_point()+
  geom_line()+
  theme_bw()


tab_media %>%
  ggplot(aes(x=variedade, y =media,
             color=as_factor(epoca))) +
  geom_point()+
  geom_line()+
  theme_bw()

# Realizar o Diagnostico da ANOVa utilizando o delineamento
# de tratamentos (DIC no caso)
dados_soja %>%
  ggplot(aes(x=trat, y=y)) +
  geom_boxplot(fill="gray") +
  geom_jitter(size=1.3) + # adiciona os pontos no boxplot
  theme_bw()

# Realizar a análise de variância para um modelo
# inteiramente casualizado para estudar o efeito
# de trat nos valores de y.
modelo <- aov(y ~ trat,
              data = dados_soja %>%
                mutate(
                  trat = as_factor(trat),
                  # y_t = y^(1-1.5423/2),
                  # y_bc = log(y)
                )
)
modelo
anova(modelo) # sem efeito.

## Normalidade dos erros
diag <- dados_soja %>%
  mutate(
    rs_y = rstudent(modelo), # extrair residuos
    pred_y = predict(modelo) # calcular preditos
  ) #%>%
#select(sup, argila, rs_argila, pred_argila)

## Construa o QQ plot
diag %>%
  ggplot(aes(sample=rs_y)) +
  stat_qq() +
  stat_qq_line(color="blue") +
  theme_bw()

## Estudo de outliers
diag %>%
  ggplot(aes(x = pred_y, y= rs_y)) +
  geom_point() +
  # ylim(-4,4) +
  geom_hline(yintercept = c(-3,3),
             color="red",
             linetype = 2) +
  theme_bw()

## Histograma dos resíduos
diag %>%
  ggplot(aes(x=rs_y, y=..density..)) +
  geom_histogram(bins = 10,color="black",fill="gray") +
  theme_bw()
diag %>% pull(rs_y) %>% shapiro.test()
diag %>% pull(rs_y) %>% nortest::ad.test()
diag %>% pull(rs_y) %>% nortest::cvm.test()
diag %>% pull(rs_y) %>% nortest::lillie.test()

## Homocedasticidade
y <- diag %>% pull(y)
trat <- diag %>% pull(trat) %>% as_factor()

lawstat::levene.test(y, trat)
lawstat::levene.test(y, trat,location = "mean")
bartlett.test(y, trat)
# Conclusão:



# Correlação na Subparcelas
dados_soja %>%
  pivot_wider(names_from = variedade,
              values_from = y) %>%
  ggplot(aes(x=v1,y=v2)) +
  geom_point()

dados_soja %>%
  pivot_wider(names_from = variedade,
              values_from = y) %>%
  cor.test(~v1 + v2,
           data=.)




