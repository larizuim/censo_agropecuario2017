# Criando donnut

require(tidyverse)

df <- read.delim("donuts.txt", 
                 header = FALSE)

df$area_frac <- df$V3 / sum(df$V3)

# Compute the cumulative percentages (top of each rectangle)
df$ymax <- cumsum(df$area_frac)

# Compute the bottom of each rectangle
df$ymin <- c(0, head(df$ymax, n=-1))

# # Compute label position
df$labelPosition <- (df$ymax + df$ymin) / 2

# Compute a good label
df$label <- paste0(df$V2, ": ", 100*round(df$area_frac,3)," %")

###

p1 <- ggplot(df, 
       aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= V2)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Proporção da área dos estabelecimentos rurais dirigidos por mulheres em RO", 
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuário 2017')

p1

### estabelecimento

df$estab_frac <- df$V4 / sum(df$V4)

# Compute the cumulative percentages (top of each rectangle)
df$ymax <- cumsum(df$estab_frac)

# Compute the bottom of each rectangle
df$ymin <- c(0, head(df$ymax, n=-1))

# # Compute label position
df$labelPosition <- (df$ymax + df$ymin) / 2

# Compute a good label
df$label <- paste0(df$V2, ": ", 100*round(df$estab_frac,3)," %")

###

p2 <- ggplot(df, 
             aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= V2)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Proporção de estabelecimentos rurais dirigidos por mulheres em RO", 
       caption='Fonte: Elaboração própria adaptado
       do Censo Agropecuário 2017')

p2

### 

gridExtra::grid.arrange(p2, p1, ncol = 2)


# ggsave("donuts.jpeg", width = 4, height = 4)
