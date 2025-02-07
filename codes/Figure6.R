# install.packages(c('ggplot2', 'tidyr', 'gridExtra'))

source('CV_Gabriel.R')
source('CV_EM.R')
source('CV_Matrix.R')
source('create_data.R')
library(ggplot2)
library(tidyr)
library(gridExtra)

set.seed(698)

n_values <- c(100,300,500,700,1000)
p <- 30
R_true <- 5
singular_values <- runif(R_true, 8, 16)
R_values <- 1:10
folds <- 10

## CV with gaussian

df_1_time <- data.frame(n = n_values, 
                        time_repaired = rep(0, length(n_values)), 
                        time_EM = rep(0, length(n_values)),
                        time_matrix = rep(0, length(n_values)))

noise <- 'gaussian'

for (i in 1:length(n_values)) {
  n <- n_values[i]
  
  l <- create_data(n, p, singular_values, noise)
  X <- l$X
  X_centered <- scale(X, center=TRUE, scale=FALSE) 
  
  for (j in 1:length(R_values)) {
    R <- R_values[j]
    
    l_EM <- EM_CV(X, R, folds)
    df_1_time$time_EM[i] <- df_1_time$time_EM[i] + l_EM$runtime
    
    l_matrix_1 <- CV_hardtresholding(X, R, folds)
    df_1_time$time_matrix[i] <- df_1_time$time_matrix[i] + l_matrix_1$runtime
    
    l_repaired_1 <- CV_PCA_repaired(X_centered, R, folds)
    df_1_time$time_repaired[i] <- df_1_time$time_repaired[i] + l_repaired_1$runtime

  }
  
  df_1_time$time_EM[i] <- df_1_time$time_EM[i] 
  df_1_time$time_matrix[i] <- df_1_time$time_matrix[i] 
  df_1_time$time_repaired[i] <- df_1_time$time_repaired[i]   
}

df_long_1_time <- df_1_time %>%
  pivot_longer(cols = starts_with("time"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_1_time$Technique <- factor(df_long_1_time$Technique, 
                                   levels = c("time_repaired", "time_EM", "time_matrix"))


g1time <- ggplot(df_long_1_time, aes(x = n, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Gaussian Noise",
       x = "Sample size", 
       y = "Runtime (s)",
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"),
                     labels=c('Gabriel CV','EM CV','Matrix Completion'))+
  theme_minimal()+
  scale_x_continuous(breaks = n_values)+
  theme(legend.position = c(0.35,0.8),
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
        panel.border = element_rect(color = "black", fill='NA')
  )


df_2_time <- data.frame(n = n_values, 
                        time_repaired = rep(0, length(n_values)), 
                        time_EM = rep(0, length(n_values)),
                        time_matrix = rep(0, length(n_values)))

noise <- 'heavy'

for (i in 1:length(n_values)) {
  n <- n_values[i]
  
  l <- create_data(n, p, singular_values, noise)
  X <- l$X
  X_centered <- scale(X, center=TRUE, scale=FALSE) 
  
  for (j in 1:length(R_values)) {
    R <- R_values[j]
    
    l_EM <- EM_CV(X, R, folds)
    df_2_time$time_EM[i] <- df_2_time$time_EM[i] + l_EM$runtime
    
    l_matrix_2 <- CV_hardtresholding(X, R, folds)
    df_2_time$time_matrix[i] <- df_2_time$time_matrix[i] + l_matrix_2$runtime
    
    l_repaired_2 <- CV_PCA_repaired(X_centered, R, folds)
    df_2_time$time_repaired[i] <- df_2_time$time_repaired[i] + l_repaired_2$runtime
    
  }
  
  df_2_time$time_EM[i] <- df_2_time$time_EM[i] 
  df_2_time$time_matrix[i] <- df_2_time$time_matrix[i] 
  df_2_time$time_repaired[i] <- df_2_time$time_repaired[i]   
}

df_long_2_time <- df_2_time %>%
  pivot_longer(cols = starts_with("time"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_2_time$Technique <- factor(df_long_2_time$Technique, 
                                   levels = c("time_repaired", "time_EM", "time_matrix"))


g2time <- ggplot(df_long_2_time, aes(x = n, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Heavy Noise",
       x = "Sample size", 
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"))+
  theme_minimal()+
  scale_x_continuous(breaks = n_values)+
  theme(legend.position = 'none',
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
        panel.border = element_rect(color = "black", fill='NA')
  )


df_3_time <- data.frame(n = n_values, 
                        time_repaired = rep(0, length(n_values)), 
                        time_EM = rep(0, length(n_values)),
                        time_matrix = rep(0, length(n_values)))

noise <- 'colored'

for (i in 1:length(n_values)) {
  n <- n_values[i]
  
  l <- create_data(n, p, singular_values, noise)
  X <- l$X
  X_centered <- scale(X, center=TRUE, scale=FALSE) 
  
  for (j in 1:length(R_values)) {
    R <- R_values[j]
    
    l_EM <- EM_CV(X, R, folds)
    df_3_time$time_EM[i] <- df_3_time$time_EM[i] + l_EM$runtime
    
    l_matrix_1 <- CV_hardtresholding(X, R, folds)
    df_3_time$time_matrix[i] <- df_3_time$time_matrix[i] + l_matrix_1$runtime
    
    l_repaired_1 <- CV_PCA_repaired(X_centered, R, folds)
    df_3_time$time_repaired[i] <- df_3_time$time_repaired[i] + l_repaired_1$runtime
    
  }
  
  df_3_time$time_EM[i] <- df_3_time$time_EM[i] 
  df_3_time$time_matrix[i] <- df_3_time$time_matrix[i] 
  df_3_time$time_repaired[i] <- df_3_time$time_repaired[i]   
}

df_long_3_time <- df_3_time %>%
  pivot_longer(cols = starts_with("time"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_3_time$Technique <- factor(df_long_3_time$Technique, 
                                   levels = c("time_repaired", "time_EM", "time_matrix"))


g3time <- ggplot(df_long_3_time, aes(x = n, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Colored Noise",
       x = "Sample size", 
       y =  NULL,
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"))+
  theme_minimal()+
  scale_x_continuous(breaks = n_values)+
  theme(legend.position = 'none',
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
        panel.border = element_rect(color = "black", fill='NA')
  )

g <- grid.arrange(g1time, g2time, g3time, ncol=3,
             widths=c(1.1,1,1))

ggsave(file.path("Report_folder", "runtime_versus_n.png"), g, width = 25, height = 9.5, units = "cm")

