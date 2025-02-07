#install.packages(c('ggplot2','tidyr','gridExtra))
source('CV_Gabriel.R')
source('CV_EM.R')
source('CV_Matrix.R')
source('create_data.R')
library(ggplot2)
library(tidyr)
library(gridExtra)

set.seed(891)

n <- 100
p <- 20
R_true <- 5
Rvalues <- 1:9
folds <- 10

noise <- 'heavy'

df <- data.frame(R = Rvalues)

df_2 <- data.frame(R = Rvalues)

df_3 <- data.frame(R = Rvalues)

times <- 100

for (i in 1:times){
  singolar_values <- runif(R_true,2,23)
  l <- create_data(n, p, singolar_values, noise)
  X <- l$X
  E <- l$E
  X_centered <- scale(X, center=TRUE, scale=FALSE) 
  print(norm(X-E, type = "F") / norm(E, type = "F"))
  
  CVg <- sapply(Rvalues, function(R) CV_PCA_repaired(X_centered, R, folds)$CV)
  col_name <- paste0("CV_", i)
  df[[col_name]] <- CVg
  
  CVem <- sapply(Rvalues, function(R) EM_CV(X, R, folds)$CV)
  df_2[[col_name]] <- CVem
  
  CVm <- sapply(Rvalues, function(R) CV_hardtresholding(X, R, folds)$CV)
  df_3[[col_name]] <- CVm
  
}

df_long <- df %>%
  pivot_longer(
    cols = -R, 
    names_to = "Group", 
    values_to = "Error" 
  )

g1 <- ggplot(df_long, aes(x = R, y = Error, group = Group)) +
  geom_line(color = "#20A7F3", alpha = 1) + 
  labs(
    title = "Gabriel CV",
    x = NULL,
    y = "CV error",
  ) +
  ylim(0, 60) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 13,               
      family = "sans",         
      face = "bold"            
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill='NA',size = 1)
  )

df_frequencies <- data.frame(R = Rvalues, Freq = rep(0, length(Rvalues)))

for (i in 2:ncol(df)) {
  min_value <- min(df[[i]], na.rm = TRUE)
  min_index <- which(df[[i]] == min_value)
  df_frequencies[['Freq']][min_index] <- df_frequencies[['Freq']][min_index] + 1
}

df_frequencies[['Freq']] <- df_frequencies[['Freq']] / times

g2 <- ggplot(df_frequencies, aes(x = factor(R), y = Freq)) + 
  geom_bar(stat = "identity", fill = "#20A7F3", color = "black") +
  geom_text( aes(label = ifelse(Freq > 0, sprintf("%.2f", Freq), ""), 
                 vjust = ifelse(Freq > 0.80, 1.5 , -0.3)), size = 3)  +
  labs(
    title = NULL,
    x = "Dimension",
    y = "Frequency",
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill='NA',size = 1)
  )

##


df_long_2 <- df_2 %>%
  pivot_longer(
    cols = -R, 
    names_to = "Group", 
    values_to = "Error" 
  )

g3 <- ggplot(df_long_2, aes(x = R, y = Error, group = Group)) +
  geom_line(color = "#C22445", alpha = 1) + 
  labs(
    title = "EM CV",
    x = NULL,
    y = NULL,
  ) +
  ylim(0, 60) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 13,               
      family = "sans",         
      face = "bold"            
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill='NA',size = 1)
  )

df_frequencies_2 <- data.frame(R = Rvalues, Freq = rep(0, length(Rvalues)))

for (i in 2:ncol(df_2)) {
  min_value <- min(df_2[[i]], na.rm = TRUE)
  min_index <- which(df_2[[i]] == min_value)
  df_frequencies_2[['Freq']][min_index] <- df_frequencies_2[['Freq']][min_index] + 1
}

df_frequencies_2[['Freq']] <- df_frequencies_2[['Freq']] / times

g4 <- ggplot(df_frequencies_2, aes(x = factor(R), y = Freq)) + 
  geom_bar(stat = "identity", fill = "#C22445", color = "black") +
  geom_text( aes(label = ifelse(Freq > 0, sprintf("%.2f", Freq), ""), 
                 vjust = ifelse(Freq > 0.80, 1.5 , -0.3)), size = 3)  +
  labs(
    title = NULL,
    x = "Dimension",
    y = NULL,
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill='NA',size = 1)
  )

##

df_long_3 <- df_3 %>%
  pivot_longer(
    cols = -R, 
    names_to = "Group", 
    values_to = "Error" 
  )

g5 <- ggplot(df_long_3, aes(x = R, y = Error, group = Group)) +
  geom_line(color = "#2BA061", alpha = 1) + 
  labs(
    title = "Matrix Completion",
    x = NULL,
    y = NULL,
  ) +  
  ylim(0, 60) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 13,               
      family = "sans",         
      face = "bold"            
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill='NA',size = 1)
  )

df_frequencies_3 <- data.frame(R = Rvalues, Freq = rep(0, length(Rvalues)))

for (i in 2:ncol(df_3)) {
  min_value <- min(df_3[[i]], na.rm = TRUE)
  min_index <- which(df_3[[i]] == min_value)
  df_frequencies_3[['Freq']][min_index] <- df_frequencies_3[['Freq']][min_index] + 1
}

df_frequencies_3[['Freq']] <- df_frequencies_3[['Freq']] / times

g6 <- ggplot(df_frequencies_3, aes(x = factor(R), y = Freq)) + 
  geom_bar(stat = "identity", fill = "#2BA061", color = "black") +
  geom_text( aes(label = ifelse(Freq > 0, sprintf("%.2f", Freq), ""), 
                 vjust = ifelse(Freq > 0.80, 1.5 , -0.3)), size = 3)  +
  labs(
    title = NULL,
    x = "Dimension",
    y = NULL,
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill='NA',size = 1)
  )



g <- grid.arrange(g1, g3, g5, g2, g4, g6, ncol = 3)

ggsave(file.path("Report_folder", "100heavy.png"), g, width = 25, height = 12, units = "cm")

