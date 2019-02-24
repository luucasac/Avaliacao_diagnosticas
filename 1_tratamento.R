#
#  ---- AVALIAÇOES DIAGNÓSTICAS ---- #
# 
# Manipulacao dos dados e tratamento
#
# importando as bibliotecas necessarias

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# importando os dados xlsx

r <- read_xlsx("../9F826100.xlsx", sheet = 1)

# selecionando apenas a colunas necessarias

r2 <- r %>% select(id_materia,
                    id_turma,
                    id_rede,
                    id_escola,
                    id_matricula,
                    tipo,
                    id_serie,
                    Q1,
                    Q2,
                    Q3,
                    Q4,
                    Q5,
                    Q6,
                    Q7,
                    Q8,
                    Q9,
                    Q10,
                    Q11,
                    Q12,
                    Q13,
                    Q14,
                    Q15,
                    Q16,
                    Q17,
                    Q18,
                    Q19,
                    Q20,
                    Q21,
                    Q22,
                    Q23,
                    Q24,
                    Q25,
                    Q26,
                    Q27,
                    Q28,
                    Q29,
                    Q30)

rm(r)

# convertendo o tipo das colunas

r2$id_materia <- as.numeric(r2$r2$id_materia)
r2$id_turma <- as.numeric(r2$r2$id_turma)
r2$id_rede <- as.numeric(r2$r2$id_rede)
r2$id_escola <- as.numeric(r2$r2$id_escola)
r2$id_matricula <- as.numeric(r2$r2$id_matricula)
r2$tipo <- as.numeric(r2$r2$tipo)
r2$id_serie <- as.numeric(r2$r2$id_serie)
r2$Q1 <- as.numeric(r2$Q1)
r2$Q2 <- as.numeric(r2$Q2)
r2$Q3 <- as.numeric(r2$Q3)
r2$Q4 <- as.numeric(r2$Q4)
r2$Q5 <- as.numeric(r2$Q5)
r2$Q6 <- as.numeric(r2$Q6)
r2$Q7 <- as.numeric(r2$Q7)
r2$Q8 <- as.numeric(r2$Q8)
r2$Q9 <- as.numeric(r2$Q9)
r2$Q10 <- as.numeric(r2$Q10)
r2$Q11 <- as.numeric(r2$Q11)
r2$Q12 <- as.numeric(r2$Q12)
r2$Q13 <- as.numeric(r2$Q13)
r2$Q14 <- as.numeric(r2$Q14)
r2$Q15 <- as.numeric(r2$Q15)
r2$Q16 <- as.numeric(r2$Q16)
r2$Q17 <- as.numeric(r2$Q17)
r2$Q18 <- as.numeric(r2$Q18)
r2$Q19 <- as.numeric(r2$Q19)
r2$Q20 <- as.numeric(r2$Q20)
r2$Q21 <- as.numeric(r2$Q21)
r2$Q22 <- as.numeric(r2$Q22)
r2$Q23 <- as.numeric(r2$Q23)
r2$Q24 <- as.numeric(r2$Q24)
r2$Q25 <- as.numeric(r2$Q25)
r2$Q26 <- as.numeric(r2$Q26)
r2$Q27 <- as.numeric(r2$Q27)
r2$Q28 <- as.numeric(r2$Q28)
r2$Q29 <- as.numeric(r2$Q29)
r2$Q30 <- as.numeric(r2$Q30)

# transformando colunos em linhas

r3 <- r2 %>% gather(questao, acerto, starts_with("Q"))

rm(r2) 

# consolidando os dados por escola

acertos_por_questao <- r3 %>% 
  group_by(id_escola, id_serie, id_materia, questao) %>% 
  summarise(Acertos = mean(acerto)) %>% 
  filter(!is.na(Acertos))



acertos_por_questao %>% filter(id_escola == 1, id_serie == 9) %>% 
  ggplot(aes(y = Acertos, x = questao, col = id_materia))+
  geom_jitter()
