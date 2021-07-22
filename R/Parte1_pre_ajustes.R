# Bibliotecas

require(tidyverse)
require(sidrar)
require(geobr)

### para mudar o Estado é só mudar o geo.filter para o de interesse.

### Neste código há rascunhos de possíveis tabelas para o artigo:

### BLOCO 1

{

# Busca informações contidas na tabela 6845 do SIDRA: 
  
info_sidra(6845)

# Tabela sobre o número de estabelecimentos agropecuários: 
  
aux <- get_sidra(geo="State",x=6845,geo.filter = 11,
          variable=183,classific= c("c12564", "c218"))

# Questão 1: Número de estabelecimentos por sexo:

q1 <- aux %>% 
  filter(`Condição do produtor em relação às terras (Código)` 
         %in% c(46503, 46504)) %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  select(`Sexo do produtor`, Valor) %>% 
  group_by(`Sexo do produtor`) %>% 
  summarise(Valor = sum(Valor, na.rm = TRUE)) %>% 
  mutate("Estabelicmentos agropecuários (%)" = 100*round(Valor / sum(Valor), 3)) %>% 
  rename("Número de estabelecimentos agropecuários"  = Valor)

### Mapa

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
  mutate("Estabelicementos agropecuários (%)" = 100*round(Valor / sum(Valor), 3)) %>% 
  rename("Número de estabelecimentos agropecuários"  = Valor) %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  mutate(code_muni = as.numeric(code_muni))

rondonia_map <- read_municipality(code_muni=11, year=2010) %>% 
  left_join(q1_map)

# Mapa com porcentagem de mulheres donas de estabelecimentos por municipio de RO


rondonia_map %>% 
  ggplot() +
  geom_sf(aes(fill=`Estabelicementos agropecuários (%)`), 
          color= "black", size=.15)+
  labs(title="Q1) Censo Agropecuário (2017)",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()


# Questão 2 : Estabelecimentos - Agricultura Familiar - Sexo

aux2 <- get_sidra(geo="State",x=6845, geo.filter = 11,
                       variable=183,
                       classific= c("c829","c12564","c218"))

q2 <- aux2 %>%  
  filter(`Condição do produtor em relação às terras (Código)` 
         %in% c(46503, 46504)) %>% 
  filter(`Unidade da Federação` == "Rondônia") %>%
  select(`Sexo do produtor`, Valor, Tipologia) %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>%
  filter(`Sexo do produtor` != "Total") %>%
  # group_by(`Sexo do produtor`) %>% 
  mutate(`Valor (%)` = 100*round(Valor/sum(Valor, na.rm = TRUE), 4)) %>%
  filter(`Sexo do produtor` != "Não se aplica") %>% 
  group_by(`Sexo do produtor`, Tipologia) %>% 
  summarise(Valor = sum(Valor), 
            Valor2 = sum(`Valor (%)`))

q2_names <- c("Sexo do produtor", 
              "Tipologia", "Estabelecimentos", 
              "% Estabelecimentos")

colnames(q2) <- q2_names

q2

# Mapa para os municípios de RO: 

aux_map <- get_sidra(geo="City",
                     x=6845,
                     geo.filter = list("State" = 11),
                     variable=183,
                     classific= c("c829","c12564","c218"))

q2_map <- aux_map %>% 
  filter(`Condição do produtor em relação às terras (Código)` 
         %in% c(46503, 46504)) %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>%
  select(`Sexo do produtor`, Valor, `Município (Código)`, Tipologia) %>% 
  group_by(`Sexo do produtor`, `Município (Código)`, Tipologia) %>% 
  summarise(Valor = sum(Valor, na.rm = TRUE)) %>% 
  rename(code_muni = `Município (Código)`) %>% 
  group_by(code_muni) %>% 
  mutate("Estabelicementos agropecuários (%)" = 100*round(Valor / sum(Valor), 3)) %>% 
  rename("Número de estabelecimentos agropecuários"  = Valor) %>% 
  filter(`Sexo do produtor` == "Mulheres")  %>% 
  filter(Tipologia == "Agricultura familiar - sim") %>% 
  mutate(code_muni = as.numeric(code_muni))

rondonia_map <- read_municipality(code_muni=11, year=2010) %>% 
  left_join(q2_map)


rondonia_map %>% 
  ggplot() +
  geom_sf(aes(fill=`Estabelicementos agropecuários (%)`), 
          color= "black", size=.15)+
  labs(title="Q2) Censo Agropecuário (2017)",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()




## Questão 3 : Práticas agrícolas por sexo considerando agricultura familiar: 

aux3 <- get_sidra(geo="State", geo.filter = 11, 
                  x=6845,
                 variable=183,
                 classific= c("c829","c12564", "c12568", 
                              "c218"))

q3 <- aux3 %>% 
  filter(`Condição do produtor em relação às terras (Código)` 
         %in% c(46503, 46504)) %>% 
  filter(`Unidade da Federação` == "Rondônia") %>%
  select(`Sexo do produtor`, Valor, 
         Tipologia, `Tipo de prática agrícola`) %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>%
  # filter(`Sexo do produtor` != "Total") %>% 
  filter(`Tipo de prática agrícola` != "Total") %>%
  mutate(Valor = ifelse(is.na(Valor), 0, Valor))



q3_mulheres <- q3 %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  pivot_wider(names_from = Tipologia, 
              values_from = Valor, 
              values_fn = sum)

q3_mulheres_porcentagem <- q3_mulheres %>% 
  mutate_if(is.numeric, funs(100*round(./sum(., na.rm = TRUE), 3)))

q3_homens <- q3 %>% 
  filter(`Sexo do produtor` == "Homens") %>% 
  pivot_wider(names_from = Tipologia, 
              values_from = Valor, 
              values_fn = sum)

q3_homens_porcentagem <- q3_homens %>% 
  mutate_if(is.numeric, funs(100*round(./sum(., na.rm = TRUE), 3)))


q3_mulheres_AF <- q3_mulheres_porcentagem %>% 
  select(`Sexo do produtor`, `Agricultura familiar - sim`, 
         `Tipo de prática agrícola`)


q3_homens_AF <- q3_homens_porcentagem %>% 
  select(`Sexo do produtor`, `Agricultura familiar - sim`, 
         `Tipo de prática agrícola`)

q3_AF <- bind_rows(q3_mulheres_AF, q3_homens_AF)

q3_AF %>% 
  pivot_wider(names_from = `Sexo do produtor`, 
              values_from = `Agricultura familiar - sim`)

}


### BLOCO 2: Relações com o uso de agrotóxico:

info_sidra(6851)


aux4 <- get_sidra(geo="State",x=6851,
                  geo.filter = 11,
                 variable=183,
                 classific= c("c12564", "c12521", "c829", "c218"))

# Questão 1

q4 <- aux4 %>% 
  filter(`Condição do produtor em relação às terras (Código)` 
         %in% c(46503, 46504)) %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>% 
  select(`Sexo do produtor`, Valor, `Uso de agrotóxicos`, Tipologia) %>% 
  filter(`Uso de agrotóxicos` != "Total") %>% 
  filter(`Uso de agrotóxicos` %in% c("Utilizou", "Não utilizou")) 

## Uso de agrotóxico e sexo: 

q4 %>% 
  group_by(`Sexo do produtor`, `Uso de agrotóxicos`) %>% 
  filter(`Sexo do produtor` != "Não se aplica") %>% 
  summarise("Número de estabelecimentos agropecuários" = sum(Valor)) %>% 
  pivot_wider(names_from = `Sexo do produtor`, 
              values_from = `Número de estabelecimentos agropecuários`, 
              values_fn = sum) %>% 
  mutate_if(is.numeric, funs(./sum(.))) %>% 
  janitor::adorn_totals() %>% 
  janitor::adorn_pct_formatting()
  

# Sexo - Uso de Agrotóxico - Agricultura familiar

q4_mulheres <- q4 %>% 
  filter(`Sexo do produtor` == "Mulheres") %>% 
  pivot_wider(names_from = Tipologia, 
              values_from = Valor, 
              values_fn = sum)

q4_mulheres_porcentagem <- q4_mulheres %>% 
  ungroup() %>% 
  mutate_if(is.numeric, funs(100*round(./sum(., na.rm = TRUE), 3)))

q4_homens <- q4 %>% 
  filter(`Sexo do produtor` == "Homens") %>% 
  pivot_wider(names_from = Tipologia, 
              values_from = Valor, 
              values_fn = sum)

q4_homens_porcentagem <- q4_homens %>% 
  ungroup() %>% 
  mutate_if(is.numeric, funs(100*round(./sum(., na.rm = TRUE), 3)))

q4_AF <- bind_rows(q4_mulheres_porcentagem, 
                   q4_homens_porcentagem) %>% 
  filter(`Uso de agrotóxicos` == "Utilizou") 


#### q5: Relação Estabelecimentos agro. e área destes


info_sidra(6883)


aux5 <- get_sidra(geo="State",x=6883,
                  geo.filter = 11,
                  variable= 184,
                  classific= c("c12564", "c222", "c829"))

# Tipos de utilização das terras de interesse: 

codigos_matas <- c(40679, 40680, 40681, 113476)

# Criando tabela com a % de área por tipo de utilização: 

q5_a <- aux5 %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>% 
  group_by(`Sexo do produtor`) %>% 
  summarise(`Área Total` = sum(Valor)) %>% 
  na.omit() %>% 
  mutate(`% de Área` = 100* round(`Área Total` / sum(`Área Total`),3)) %>% 
  left_join(q1) %>% 
  select(1,2,4,3, 5) %>% 
  mutate(`Área média por propriedade` = round(`Área Total` / 
           `Número de estabelecimentos agropecuários`, 2)) %>% 
  mutate(`Modulos Fiscais médio por propriedade` = 
           round(`Área média por propriedade` / 60,2)) %>% 
  select(1,2,3,6, 7, 4, 5) 
  

q5_b <- aux5 %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>% 
  group_by(`Sexo do produtor`, Tipologia) %>% 
  summarise(`Área Total` = sum(Valor)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(`% de Área` = 100* round(`Área Total` / sum(`Área Total`),3)) %>% 
  left_join(q2) %>% 
  mutate(`Área média por propriedade` = round(`Área Total` / 
                                                `Estabelecimentos`, 2)) %>% 
  mutate(`Modulos Fiscais médio por propriedade` = 
           round(`Área média por propriedade` / 60,2)) %>% 
  filter(`Sexo do produtor` != "Não se aplica") %>% 
  select(c(1,2,3,5,7,8,4, 6))

q5 <- aux5 %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>%
  mutate("% de Área por tipo utilização da terra" = 
           Valor/sum(Valor, na.rm = TRUE)) %>% 
  filter(`Utilização das terras (Código)` %in% codigos_matas) %>% 
  filter(`Sexo do produtor` != "Não se aplica") %>%
  # filter(Tipologia == "Agricultura familiar - sim") %>% 
  select(`Sexo do produtor`, 
         Tipologia, `Utilização das terras`, 
         `% de Área por tipo utilização da terra`) %>% 
  group_by(`Utilização das terras`) %>% 
  summarise(`% de Área por tipo utilização da terra` = 
           sum(`% de Área por tipo utilização da terra`)) %>% 
  janitor::adorn_pct_formatting(digits = 2)

q5

#### Relação de áreas de interesse - Área - Estabelecimentos: 


aux_join <- get_sidra(geo="State",x=6883,
                      geo.filter = 11,
                      variable= 9587,
                      classific= c("c12564", "c222", "c829"))

q5_c2 <- aux5 %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>%
  mutate("% de Área por tipo utilização da terra" = 
           100* round(Valor/sum(Valor, na.rm = TRUE),3)) %>% 
  filter(`Utilização das terras (Código)` %in% codigos_matas) %>% 
  filter(`Sexo do produtor` != "Não se aplica") %>%
  # filter(Tipologia == "Agricultura familiar - sim") %>% 
  select(`Sexo do produtor`, 
         Tipologia, `Utilização das terras`, 
         `% de Área por tipo utilização da terra`, 
         Valor) %>% 
  group_by(`Utilização das terras`) %>% 
  summarise(`Área por tipo de utilização da terra` = sum(Valor, na.rm = TRUE),
            `% de Área por tipo utilização da terra` = 
              sum(`% de Área por tipo utilização da terra`)) %>% 
  arrange(-`% de Área por tipo utilização da terra`) 

join_aux5 <- aux_join %>% 
  filter(`Sexo do produtor` != "Total") %>% 
  filter(`Utilização das terras` != "Total") %>% 
  filter(Tipologia %in% c("Agricultura familiar - não", 
                          "Agricultura familiar - sim")) %>%
  mutate("% de Estabelecimentos por tipo utilização da terra" = 
           Valor/sum(Valor, na.rm = TRUE)) %>% 
  #filter(`Utilização das terras (Código)` %in% codigos_matas) %>% 
  filter(`Sexo do produtor` != "Não se aplica") %>%
  # filter(Tipologia == "Agricultura familiar - sim") %>% 
  select(`Sexo do produtor`, 
         Tipologia, `Utilização das terras`, 
         `% de Estabelecimentos por tipo utilização da terra`, 
         Valor) %>% 
  group_by(`Utilização das terras`) %>% 
  summarise(`Estabelecimentos por tipo de utilização da terra` = 
              sum(Valor, na.rm = TRUE),
            `% de Estabelecimentos por tipo utilização da terra` = 
              sum(`% de Estabelecimentos por tipo utilização da terra`)) %>% 
  arrange(-`% de Estabelecimentos por tipo utilização da terra`) 

q5_c2 %>% 
  left_join(join_aux5) %>% 
  mutate(`Área média por estabelecimento` = 
           `Área por tipo de utilização da terra` / 
           `Estabelecimentos por tipo de utilização da terra`) %>% 
  mutate(`Módulos fiscais médios` = 
           `Área média por estabelecimento` / 60)
