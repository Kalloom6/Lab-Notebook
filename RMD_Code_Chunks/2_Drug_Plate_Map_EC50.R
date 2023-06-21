library(tidyverse)
library(ggforce)
library(scales)

pbs <- "PBS"
drug1 <- "Drug 1"
drug2 <- "Drug 2"
EC50_ID <-"EC50 ID"

dil_ID = matrix(c(pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,
                pbs, "3","3","3","2","2","2","1","1","1",pbs,pbs,
                pbs,"6","6","6","5","5","5","4","4","4",pbs,pbs,
                pbs,"Control","Control","Control","8","8","8","7","7","7",pbs,pbs,
                pbs,"3","3","3","2","2","2","1","1","1",pbs,pbs,
                pbs,"6","6","6","5","5","5","4","4","4",pbs,pbs,
                pbs,"Control","Control","Control","8","8","8","7","7","7",pbs,pbs,
                pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs), ncol = 12, nrow = 8, byrow = TRUE)

drug_list = matrix(c(pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,
           pbs, drug1,drug1,drug1,drug1,drug1,drug1,drug1,drug1,drug1,pbs,pbs,
           pbs, drug1,drug1,drug1,drug1,drug1,drug1,drug1,drug1,drug1,pbs,pbs,
           pbs,"DMSO","DMSO","DMSO",drug1,drug1,drug1,drug1,drug1,drug1,pbs,pbs,
           pbs, drug2,drug2,drug2,drug2,drug2,drug2,drug2,drug2,drug2,pbs,pbs,
           pbs,drug2,drug2,drug2,drug2,drug2,drug2,drug2,drug2,drug2,pbs,pbs,
           pbs,"DMSO","DMSO", "DMSO",drug2,drug2,drug2,drug2,drug2,drug2,pbs,pbs,
           pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs,pbs), ncol = 12, nrow = 8, byrow = TRUE)



df1 <- as.data.frame(drug_list)
df2 <- as.data.frame(dil_ID)
df1 <- df1 %>% 
  mutate(row = 1:8) %>% 
  pivot_longer(-row, names_to = "col", values_to = "value") %>%
  mutate(col = as.integer(str_remove(col, "V"))) %>%
  mutate(well = paste0(LETTERS[row], col))

df2 <- df2 %>% 
  mutate(row = 1:8) %>% 
  pivot_longer(-row, names_to = "col", values_to = "value") %>%
  mutate(col = as.integer(str_remove(col, "V"))) %>%
  mutate(well = paste0(LETTERS[row], col))

glimpse(df1)
glimpse(df2)

#Plot
p <- ggplot(data = df1) + 
  geom_circle(aes(x0 = col, y0 = row, r = 0.45, fill = value))
p <- p + coord_equal()
p <- p + 
  scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01)), trans = reverse_trans()) +
  scale_y_continuous(breaks = 1:8, labels = LETTERS[1:8], expand = expansion(mult = c(0.01, 0.01)), trans = reverse_trans())
p <- p + geom_text(data = df2, aes(x = col, y = row, label = paste0(value)), size = 2,)
p <- p +
  labs(title = "96 Well Plate Map", subtitle = EC50_ID, x = "Col", y = "Row") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p)

