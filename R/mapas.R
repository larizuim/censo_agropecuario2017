# Bibliotecas

require(tidyverse)
require(sidrar)
require(geobr)


# mapa: Carregando mapa

rondonia_map <- read_municipality(code_muni=11, year=2010) 

#### Estabelecimentos

{
  
aux_map <- get_sidra(geo="City",
                     x=6845,
                     geo.filter = list("State" = 11),
                     variable=183,
                     classific= c("c12564", "c218"))

q1_map <- aux_map %>% 
  filter(`Condição do produtor em relação às terras (Código)` 
         %in% c(46503, 46504)) %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  select(`Sexo do produtor`, Valor, `Município (Código)`) %>% 
  group_by(`Sexo do produtor`, `Município (Código)`) %>% 
  summarise(Valor = sum(Valor, na.rm = TRUE)) %>% 
  rename(code_muni = `Município (Código)`) %>% 
  group_by(code_muni) %>% 
  mutate("Estabelecimentos agropecuários (%)" = 100*round(Valor / sum(Valor), 3)) %>% 
  rename("Número de estabelecimentos agropecuários"  = Valor) %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  mutate(code_muni = as.numeric(code_muni))


rondonia_map %>% 
  left_join(q1_map) %>% 
  ggplot() +
  geom_sf(aes(fill=`Estabelecimentos agropecuários (%)`), 
          color= "black", size=.15)+
  labs(title="% de Estabelecimentos agropecuários com mulheres 
  proprietárias ou concessionárias no estado de Rondônia",
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuário 2017', size= 2.5)+
  scale_fill_gradient(low = "white", high = "darkgreen", 
                      limits = c(5,25), 
                      breaks = seq(5, 25, by = 5)) +
  theme_minimal()

### Área média

aux_map2 <- get_sidra(geo="City",
                      x=6883,
                      geo.filter = list("State" = 11),
                      variable= 184,
                      classific= c("c12564", "c222", "c829"))

q2 <- aux_map2 %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>% 
  group_by(`Município (Código)`) %>% 
  summarise(`Área Total` = sum(Valor, na.rm = TRUE)) %>% 
  na.omit() %>% 
  rename(code_muni = `Município (Código)`) %>% 
  mutate(code_muni = as.numeric(code_muni)) %>% 
  mutate(`% de Área` = 100* round(`Área Total` / sum(`Área Total`),3)) %>% 
  left_join(q1_map) %>% 
  select(1,2,4,3, 5) %>% 
  mutate(`Área média por propriedade` = round(`Área Total` / 
                                                `Número de estabelecimentos agropecuários`, 2))

rondonia_map %>% 
  left_join(q2) %>% 
  rename("Área média dos estabelecimentos (ha)" = 
           `Área média por propriedade`) %>% 
  ggplot() +
  geom_sf(aes(fill=`Área média dos estabelecimentos (ha)`), 
          color= "black", size=.15)+
  labs(title="Área média dos estabelecimentos (ha) 
       com mulheres proprietárias ou concessionárias",
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuários 2017', size= 2.5)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()


### Área média mulheres AF

q3 <- aux_map2 %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - sim")) %>% 
  group_by(`Município (Código)`) %>% 
  summarise(`Área Total` = sum(Valor, na.rm = TRUE)) %>% 
  na.omit() %>% 
  rename(code_muni = `Município (Código)`) %>% 
  mutate(code_muni = as.numeric(code_muni)) %>% 
  mutate(`% de Área` = 100* round(`Área Total` / sum(`Área Total`),3)) %>% 
  left_join(q1_map) %>% 
  select(1,2,4,3, 5) %>% 
  mutate(`Área média por propriedade` = round(`Área Total` / 
                                                `Número de estabelecimentos agropecuários`, 2))


rondonia_map %>% 
  left_join(q3) %>% 
  rename("Área média dos estabelecimentos (ha)" = 
           `Área média por propriedade`) %>% 
  ggplot() +
  geom_sf(aes(fill=`Área média dos estabelecimentos (ha)`), 
          color= "black", size=.15)+
  labs(title="Área média dos estabelecimentos (ha) 
       com mulheres (AF) proprietárias ou concessionárias",
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuários 2017', size= 2.5)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()

#### Área Média mulheres que não praticam agricultura familiar

q4 <- aux_map2 %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não")) %>% 
  group_by(`Município (Código)`) %>% 
  summarise(`Área Total` = sum(Valor, na.rm = TRUE)) %>% 
  na.omit() %>% 
  rename(code_muni = `Município (Código)`) %>% 
  mutate(code_muni = as.numeric(code_muni)) %>% 
  mutate(`% de Área` = 100* round(`Área Total` / sum(`Área Total`),3)) %>% 
  left_join(q1_map) %>% 
  select(1,2,4,3, 5) %>% 
  mutate(`Área média por propriedade` = round(`Área Total` / 
                                                `Número de estabelecimentos agropecuários`, 2))


rondonia_map %>% 
  left_join(q4) %>% 
  rename("Área média dos estabelecimentos (ha)" = 
           `Área média por propriedade`) %>% 
  ggplot() +
  geom_sf(aes(fill=`Área média dos estabelecimentos (ha)`), 
          color= "black", size=.15)+
  labs(title="Área média dos estabelecimentos (ha) 
       com mulheres (NAF) proprietárias ou concessionárias",
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuários 2017', size= 2.5)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()
}

###  Agrotóxicos

aux_map <- get_sidra(geo="City",
                     x=6851,
                     geo.filter = list("State" = 11),
                     variable=183,
                     classific= c("c12564", "c12521", "c218"))

q5 <- aux_map %>% 
  filter(`Condição do produtor em relação às terras (Código)` 
         %in% c(46503, 46504)) %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Uso de agrotóxicos` == "Utilizou") %>% 
  select(`Sexo do produtor`, Valor, `Município (Código)`) %>% 
  group_by(`Sexo do produtor`, `Município (Código)`) %>% 
  summarise(Valor = sum(Valor, na.rm = TRUE)) %>% 
  rename(code_muni = `Município (Código)`) %>% 
  group_by(code_muni) %>% 
  mutate("Estabelicementos agropecuários (%)" = 100*round(Valor / sum(Valor), 3)) %>% 
  rename("Número de estabelecimentos agropecuários"  = Valor) %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  mutate(code_muni = as.numeric(code_muni))



rondonia_map %>% 
  left_join(q5) %>% 
  ggplot() +
  geom_sf(aes(fill=`Estabelicementos agropecuários (%)`), 
          color= "black", size=.15)+
  labs(title="% de Estabelecimentos com 
       mulheres proprietárias ou concessionárias 
       que utilizam agrotóxico",
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuários 2017', size= 2.5)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()


### Uso agrotóxico por mulheres que praticam Agricultura Familiar

aux_map <- get_sidra(geo="City",
                     x=6851,
                     geo.filter = list("State" = 11),
                     variable=183,
                     classific= c("c12564", "c12521", "c829"))

q6 <- aux_map %>% 
  # filter(`Condição do produtor em relação às terras (Código)` 
  #        %in% c(46503, 46504)) %>% 
  filter(`Uso de agrotóxicos` == "Utilizou") %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - sim")) %>% 
  select(`Sexo do produtor`, Valor, `Município (Código)`) %>% 
  group_by(`Sexo do produtor`, `Município (Código)`) %>% 
  summarise(Valor = sum(Valor, na.rm = TRUE)) %>% 
  rename(code_muni = `Município (Código)`) %>% 
  group_by(code_muni) %>% 
  mutate("Estabelicementos agropecuários (%)" = 100*round(Valor / sum(Valor), 3)) %>% 
  rename("Número de estabelecimentos agropecuários"  = Valor) %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  mutate(code_muni = as.numeric(code_muni))

rondonia_map %>% 
  left_join(q6) %>% 
  ggplot() +
  geom_sf(aes(fill=`Estabelicementos agropecuários (%)`), 
          color= "black", size=.15)+
  labs(title="% de Estabelecimentos com 
       mulheres (AF) proprietárias ou concessionárias 
       que utilizam agrotóxico",
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuários 2017', size= 2.5)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()


### q7 -> Mulheres uso de agrotóxico Agricultura Não Familiar:

q7 <- aux_map %>% 
  filter(`Uso de agrotóxicos` == "Utilizou") %>% 
  # filter(`Condição do produtor em relação às terras (Código)` 
  #        %in% c(46503, 46504)) %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não")) %>% 
  select(`Sexo do produtor`, Valor, `Município (Código)`) %>% 
  group_by(`Sexo do produtor`, `Município (Código)`) %>% 
  summarise(Valor = sum(Valor, na.rm = TRUE)) %>% 
  rename(code_muni = `Município (Código)`) %>% 
  group_by(code_muni) %>% 
  mutate("Estabelicementos agropecuários (%)" = 100*round(Valor / sum(Valor), 3)) %>% 
  rename("Número de estabelecimentos agropecuários"  = Valor) %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  mutate(code_muni = as.numeric(code_muni))


rondonia_map %>% 
  left_join(q7) %>% 
  ggplot() +
  geom_sf(aes(fill=`Estabelicementos agropecuários (%)`), 
          color= "black", size=.15)+
  labs(title="% de Estabelecimentos com 
       mulheres (NAF) proprietárias ou concessionárias 
       que utilizam agrotóxico",
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuários 2017', size= 2.5)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()


# write.csv(q7, "data/mulheres_estabelecimentos_AGRO_NAF.csv")

