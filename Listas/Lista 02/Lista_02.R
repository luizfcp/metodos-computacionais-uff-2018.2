
# Carregando pacotes

library(readxl)
library(magrittr)
library(janitor)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(gridExtra)


# 1 -----------------------------------------------------------------------

data1 <- read_excel("G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento.xls", na = "999") %>% clean_names()

#a)
data1$grupo %<>% factor(labels=c("Adesão", "Não Adesão"))

data1$situacao_atual_no_trabalho %<>% factor(labels=c("Empregado", "Desempregado"))

data1$frequenta_algum_grupo_religioso %<>% factor(labels=c("Não","Sim"))

data1$escolaridade %<>% factor(labels=c("analfabeto", "ensino fundamental", "ensino medio", "ensino superior"))

#b)
data1 %<>% 
  mutate(faixa_etaria = cut(data1$idade, breaks=c(0,18,60,150), labels=c("Jovem", "Adulto", "3ª Idade")))

#c)
class(data1)

#d)
summary(data1)

#e)
write_delim(data1, "G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q1.txt")
write_csv(data1, "G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q1.csv")
write_rds(data1, "G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q1.rds")

files <- c("G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q1.txt",
           "G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q1.csv",
           "G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q1.rds")

for (i in 1:2) {
  ifelse (((file.size(files[i])) < (file.size(files[i+1]))), j <- 1, j <- i+1)
}; cat("O arquivo com menor tamanho é ", 
       str_sub((files[j]), start = 17), 
       ", pois ao salvar o arquivo com a extensão .rds o R salva-o com a compressão gzip, o que diminui o tamanho e o tempo necessário para a leitura do mesmo.")

# 2 -----------------------------------------------------------------------

data2 <- read_rds("G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q1.rds")

#a)
data2 %>% select(idade, ansiedade, depressao) %>% write_delim("G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q2a.txt")

#b)
data2 %>% 
  group_by(escolaridade) %>% 
  filter(escolaridade=="ensino fundamental", idade>50, frequenta_algum_grupo_religioso=="Não") %>% 
  write_csv("G:/Meu Drive/UFF/Met. Comp. para Est. II/Listas/Lista 02/Tratamento_Q2b.csv")

#c)
data2 %>% arrange(idade)

#d)
data2 %>% rename("pont.ansiedade" = "ansiedade")

#e)
data2 %<>% mutate(pont.total = ansiedade+depressao)

#f)
data2 %>% 
  group_by(grupo, situacao_atual_no_trabalho) %>% 
  summarise(med_ansiedade = mean(ansiedade, na.rm = T),
            med_depressao = mean(depressao, na.rm = T))

#g)
data2 %>% 
  group_by(grupo, escolaridade, frequenta_algum_grupo_religioso) %>% 
  summarise(maximo = max(pont.total, na.rm = T),
            mediana = median(pont.total, na.rm = T),
            "5o_decil" = quantile(pont.total, 0.5, na.rm = T))

# 3 -----------------------------------------------------------------------

#a)
grafa <- 
  data1 %>% 
  ggplot() +
  geom_bar(aes(x=grupo), fill="#8cb8ff", color="black") +
  theme_bw() +
  labs(x="Grupo", y="", title="Grupos")

#b)
grafb <- 
  data1 %>% 
  ggplot() +
  geom_bar(aes(x=situacao_atual_no_trabalho), fill="#a570d1", color="black") +
  theme_bw() +
  labs(x="Situação", y="", title="Situação atual do trabalho")

#c)
grafc <- 
  data1 %>% 
  ggplot() +
  geom_boxplot(aes(x=grupo, y=idade, color=grupo)) +
  theme_bw() +
  labs(x="Grupo", y="Idade", title="Idade por Grupos") +
  theme(legend.position = "none") ##ou guides(color=F)

#d)
data1 %>% 
  filter(frequenta_algum_grupo_religioso=="Sim") %>% 
  ggplot() +
  geom_boxplot(aes(x=ansiedade, y=depressao, group=frequenta_algum_grupo_religioso)) +
  theme_bw() +
  labs(x="Ansiedade", y="Depressão", title="Ansiedade e Depressão segundo frequenta algum grupo religioso")

#e)
data1 %>% 
  ggplot() +
  geom_histogram(aes(x=idade), fill="#8cb8ff") +
  theme_bw() +
  labs(x="Idade", y="", title="Idade")
  
#f)
summary(data1)

graf1 <- 
  data1 %>% 
  ggplot(aes(x=idade)) +
  geom_histogram(aes(y=..density..), bins=10) +
  stat_function(fun=dnorm, args=list(mean=mean(data1$idade), sd=sd(data1$idade)), color="red") +
  theme_bw() +
  labs(x="Idade", y="", title="Idade")
# Idade é uma variável quantitativa discreta

## Escolaridade é uma variável qualitativa, logo não possui densidade

graf2 <- 
  data1 %>%  
  ggplot(aes(x=depressao, na.rm=T)) +
  geom_histogram(aes(y=..density..), bins=10) +
  stat_function(fun=dnorm, args=list(mean=mean(na.omit(data1$depressao)), sd=sd(na.omit(data1$depressao))), color="red") +
  theme_bw() +
  labs(x="Depressão", y="", title="Depressão")
# Depressão é uma variável quantitativa discreta

graf3 <-
  data1 %>%  
  na.omit(.$ansiedade) %>% 
  ggplot(aes(x=ansiedade, na.rm=T)) +
  geom_histogram(aes(y=..density..), bins=10) +
  stat_function(fun=dnorm, args=list(mean=mean(na.omit(data1$ansiedade)), sd=sd(na.omit(data1$ansiedade))), color="red") +
  theme_bw() +
  labs(x="Ansiedade", y="", title="Ansiedade")
# Ansiedade é uma variável quantitativa discreta

grid.arrange(graf1, graf2, graf3, ncol=3, nrow=2)

#g)
table(data1$situacao_atual_no_trabalho, data1$grupo) %>% prop.table(2)

data1 %>% 
  ggplot(aes(x=situacao_atual_no_trabalho)) +
  geom_bar(aes(fill=grupo), position = "fill") +
  labs(y="", x="Situação atual no trabalho")
  

#h)
grid.arrange(grafa, grafb, grafc, ncol=3, nrow=2)

# 4 -----------------------------------------------------------------------

#1
data1$depressao %>% summary()
a4 <- data1$depressao %>% na.omit()

data1 %>% 
  ggplot() +
  geom_boxplot(aes(y=depressao)) +
  scale_y_continuous(breaks = seq(min(a4), max(a4), 2)) +
  theme_bw()

data1$depressao %>% boxplot()

#2
data1$frequenta_algum_grupo_religioso %>% summary()

data1 %>% filter(!is.na(frequenta_algum_grupo_religioso)) %>% 
  ggplot() +
  geom_bar(aes(x=frequenta_algum_grupo_religioso, fill=frequenta_algum_grupo_religioso)) +
  labs(x="Frequenta algum grupo religioso", y="", title="Frequenta algum grupo religioso") +
  theme_bw() +
  guides(fill=F)

# 5 -----------------------------------------------------------------------

#a)
data1 %<>% mutate(cat_idade = cut(idade, breaks=c(-Inf,70,90,+Inf), labels=c(1,2,3)))

#b)
data1 %<>% mutate(cat_depre = cut(depressao, breaks=c(0,10,20,30), labels=c(1,2,3)))

# 6 -----------------------------------------------------------------------

#a)
a1 <- function(x, lambda) {
  lambda*exp(-lambda*x) * I(x>=0)
}

#b)
a2 <- function(x) {
  x^2 * I(x>-5 & x<5)
}

#c)
a3 <- function(x) {
  (1/10)*exp(-1)*(x-10) * I(x>2 & x<18)
}

#d)
a4 <- function(alfa, beta, x) {
  ((beta^alfa)/gamma(alfa)) * x^(alfa-1) * exp(-beta * x) * I(x>=0)
}

#e)
a5 <- function(alfa, beta, x) {
  (gamma(alfa+beta)/(gamma(alfa)*gamma(beta))) * x^(alfa-1) * (1-x)^(beta-1) * I(0<=x & x<=1)
}

# 7 -----------------------------------------------------------------------

ggplot(data.frame(x=c(0,10)), aes(x=x)) +
  stat_function(fun = a1, args = list(lambda=1)) +
  labs(y="", title="Destribuição exponencial com lambda=1") +
  theme_minimal()

ggplot(data.frame(x=c(-5,5)), aes(x=x)) +
  stat_function(fun = a2) +
  labs(y="f(x)", title="Função x^2 no intervalo (-5,5)") +
  theme_minimal()

ggplot(data.frame(x=c(2,18)), aes(x=x)) +
  stat_function(fun = a3) +
  labs(y="f(x)") +
  theme_minimal()

ggplot(data.frame(x=c(0,30)), aes(x=x)) +
  stat_function(fun = a4, args = list(alfa=2, beta=0.5)) +
  labs(y="", title="Distribuição Gama com alfa=2 e beta=0.5")

ggplot(data.frame(x=c(0,1))) +
  stat_function(fun = a5, args = list(alfa=2, beta=7))

# 8 -----------------------------------------------------------------------

dev_pad <- function(x) {
  sqrt(var(x))
}

vet <- c(1:5)
sd(vet)
dev_pad(vet)

# 9 -----------------------------------------------------------------------

f9 <- function(x) {
  cat(paste0("Soma = ", sum(x), ". Média = ", mean(x), "."))
}
f9(vet)

# 10 ----------------------------------------------------------------------

IMC <- function(peso, altura) {
  peso/((altura)^2)
}
IMC(80,1.80)

# 11 ----------------------------------------------------------------------

IMC <- function(peso, altura) {
  imc <- peso/((altura)^2)
  
  if (imc>=0 && imc<=18.5) {
    cat("IMC: ", imc %>% round(), "- Desnutrido")
  }else{ 
    if(imc>18.5 && imc<=25){
      cat("IMC: ", imc %>% round(), "- Normal")
    }else{
      cat("IMC: ", imc %>% round(), "- Obeso")
    }
  }
  
}
IMC(80,1.80)

# 12 ----------------------------------------------------------------------

#a)
{
  soma=1
  for (i in seq(32,20*100,20)) {
    soma=soma+(1/i)
  }
  soma
}

#b)
{
  soma=1
  vet <- seq(11,10*100,10)
  for (j in seq(1,50,2)) {
    soma=soma-(1/vet[j])+(1/vet[j+1])
  }
  soma
}

#c)
{
  soma=0
  for (i in 1:100) {
    soma=soma+(i/15)
  }
  soma
}

#d)
{
  soma=1/10
  for (i in seq(10,10*99,10)) {
    soma=soma+(i/10)
  }
  soma
}

#e)
{
  soma=0
  for (i in 1:100) {
    soma=soma+sqrt(log(i))
  }
  soma
}

# 13 ----------------------------------------------------------------------

#a)
(function(x) x^3) %>% 
  integrate(lower = 0, upper = 10)

#b)
fb <- function(x) (3/5)*(x^3 + x)
integrate(fb, lower = 0, upper = 5)[[1]] + integrate(fb, lower = 7, upper = 10)[[1]] + integrate(fb, lower = 11, upper = 15)[[1]]

#c)
(function(x) x^12 * (1-8)^8) %>% 
  integrate(lower = 0, upper = 1)

#d)
(function(x) 3*exp(-3*x)) %>% 
  integrate(lower = 0, upper = 100)

# 14 ----------------------------------------------------------------------

#a)
quote(x^3) %>% D("x")

#b)
quote(cos(2*x)+exp(-3*x)) %>% D("x")

#c)
quote(3*x+log(x+y)) %>% D("x")

#d)
quote(x*exp(-2*x) + log(1/x)) %>% D("x")

# 15 ----------------------------------------------------------------------

#a)
quote(3*x^3 - cos(x)) %>% D("x") %>% D("x")

#b)
quote(3*x^3 - y*cos(x)) %>% D("x") %>% D("y")