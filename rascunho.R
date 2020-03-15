library(tidyverse)

# Floripa -----------------------------------------------------------------
flp <- read_csv("Industrias_em_Florianopolis_2003-2017.csv")

flp_admp <- flp %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Administração Pública") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_flp = median(average_age),
    salario_mediano_flp = median(average_wage),
    montante_salarial_flp = sum(wage),
    total_empregos_flp = sum(jobs)
  )

flp_ti <- flp %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Serviços de Tecnologia da Informação") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_flp = median(average_age),
    salario_mediano_flp = median(average_wage),
    montante_salarial_flp = sum(wage),
    total_empregos_flp = sum(jobs)
  )

  # bc -----------------------------------------------------------------
bc <- read_csv("Industrias_em_Balneario_Camboriu_2003-2017.csv")

bc_admp <- bc %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Administração Pública") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_flp = median(average_age),
    salario_mediano_flp = median(average_wage),
    montante_salarial_flp = sum(wage),
    total_empregos_flp = sum(jobs)
  )

bc_ti <- bc %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Serviços de Tecnologia da Informação") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_bc = median(average_age),
    salario_mediano_bc = median(average_wage),
    montante_salarial_bc = sum(wage),
    total_empregos_bc = sum(jobs)
  )


# Sao jose ----------------------------------------------------------------

sj <- read_csv("Industrias_em_Sao_Jose_2003-2017.csv")

sj_admp <- sj %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Administração Pública") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_sj = median(average_age),
    salario_mediano_sj = median(average_wage),
    montante_salarial_sj = sum(wage),
    total_empregos_sj = sum(jobs)
  )


sj_ti <- sj %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Serviços de Tecnologia da Informação") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_sj = median(average_age),
    salario_mediano_sj = median(average_wage),
    montante_salarial_sj = sum(wage),
    total_empregos_sj = sum(jobs)
  )

# Palhoça -----------------------------------------------------------------
ph <- read_csv("Industrias_em_Palhoca_2003-2017.csv")

ph_admp <- ph %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Administração Pública") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_ph = median(average_age),
    salario_mediano_ph = median(average_wage),
    montante_salarial_ph = sum(wage),
    total_empregos_ph = sum(jobs)
  )

ph_ti <- ph %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Serviços de Tecnologia da Informação") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_ph = median(average_age),
    salario_mediano_ph = median(average_wage),
    montante_salarial_ph = sum(wage),
    total_empregos_ph = sum(jobs)
  )


# Biguaçu -----------------------------------------------------------------


sj <- read_csv("Industrias_em_Sao_Jose_2003-2017.csv")

sj_admp <- sj %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Administração Pública") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_sj = median(average_age),
    salario_mediano_sj = median(average_wage),
    montante_salarial_sj = sum(wage),
    total_empregos_sj = sum(jobs)
  )


sj_ti <- sj %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Serviços de Tecnologia da Informação") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_sj = median(average_age),
    salario_mediano_sj = median(average_wage),
    montante_salarial_sj = sum(wage),
    total_empregos_sj = sum(jobs)
  )

# Palhoça -----------------------------------------------------------------

bg <- read_csv("Industrias_em_Biguacu_2003-2017.csv")

bg_admp <- bg %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Administração Pública") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_bg = median(average_age),
    salario_mediano_bg = median(average_wage),
    montante_salarial_bg = sum(wage),
    total_empregos_bg = sum(jobs)
  )

bg_ti <- bg %>% 
  filter(year >= 2010 & year <= 2017) %>% 
  filter(industry_division == "Serviços de Tecnologia da Informação") %>% 
  group_by(year) %>% 
  summarise(
    idade_mediana_bg = median(average_age),
    salario_mediano_bg = median(average_wage),
    montante_salarial_bg = sum(wage),
    total_empregos_bg = sum(jobs)
  )



# Emprego em TI -----------------------------------------------------------

emp <- data.frame(
  ano = ph_ti[, "year"],
  total_empregos_flp = flp_ti[, "total_empregos_flp"],
  total_empregos_sj = sj_ti[, "total_empregos_sj"],
  total_empregos_ph = ph_ti[, "total_empregos_ph"],
  total_empregos_ph = bg_ti[, "total_empregos_bg"]
)

empg <- emp %>%
  rename(
    "Florianópolis" = total_empregos_flp,
    "São José" = total_empregos_sj,
    "Palhoça" = total_empregos_ph,
    "Biguaçu" = total_empregos_bg
  ) %>% 
  tidyr::gather("cidade", "total_de_empregos", 2:5) %>% 
  arrange(year)


ggplot(empg, aes(x = empg$year, y = empg$total_de_empregos))+
  geom_bar(stat = "identity", aes(fill=factor(empg$cidade, levels = c("Biguaçu", "Palhoça", "São José", "Florianópolis"))), position = "dodge")+
  labs(
    title = "Evolução da quantidade de empregos na divisão de Serviços de Tecnologia da Informação",
    subtitle = "2010 a 2017",
    x = "Tempo",
    y = "Quantidade de empregos formados",
    fill = "Cidade"
  )+
  theme_minimal()+
  theme(legend.position="bottom")

ggplot(empg, aes(x = empg$year, y = empg$total_de_empregos, fill=empg$cidade))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(
    title = "Evolução da quantidade de empregos na divisão de Serviços de Tecnologia da Informação",
    subtitle = "2010 a 2017",
    x = "Tempo",
    y = "Quantidade de empregos formados",
    fill = "Cidade"
  )+
  facet_grid(rows = vars(factor(empg$cidade, levels = c("Biguaçu", "Palhoça", "São José", "Florianópolis"))), scales = "free_y")+
  theme_minimal()+
  theme(legend.position="bottom")




# araquari ----------------------------------------------------------------

# Exportações
destino_exportacoes_araquari <- read_csv("destino_exportacoes_araquari.csv")

dea <- destino_exportacoes_araquari %>% 
  filter(year >= 2010) %>% 
  group_by(year, country) %>% 
  summarise(
    valor_exp = sum(value),
    kg_exp = sum(kg)
  )

dea_eua <- dea %>% 
  filter(country == "Estados Unidos")

dea_neua <- dea %>% 
  filter(country != "Estados Unidos") %>% 
  group_by(year) %>% 
  summarise(
    country = "Outros",
    valor_exp = sum(valor_exp),
    kg_exp = sum(kg_exp)
  )

araq_exp <- bind_rows(dea_neua, dea_eua) %>% 
  arrange(year)

ggplot(araq_exp, aes(x = araq_exp$year, y = araq_exp$valor_exp, fill=araq_exp$country))+
  geom_bar(stat="identity")+
  labs(
    title = "Exportações de Araquarí, SC",
    subtitle = "2010 a 2018",
    x = "Tempo",
    y = "R$ exportados em u.m.c",
    fill = "País"
  )+
  theme_minimal()+
  theme(legend.position="bottom")

tf <- emp %>% filter(year==2017) %>% select("total_empregos_flp")
tsj <- emp %>% filter(year==2017) %>% select("total_empregos_sj")
tph <- emp %>% filter(year==2017) %>% select("total_empregos_ph")

perc_tfsj <- tsj/tf
perc_tfph <- tph/tf
perc_tfph <- tph/tf

emp$total_empregos_flp

coef_angular <- lm(emp$total_empregos_flp ~ c(1:8))
ca <- coef(coef_angular)[[2]]
ss <- summary(coef_angular)
pv_ca <- ss$coefficients[8]



# bc ----------------------------------------------------------------------


