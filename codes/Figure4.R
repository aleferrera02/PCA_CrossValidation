source('CV_Gabriel.R')
source('CV_EM.R')
source('CV_Matrix.R')
source('create_data.R')
library(latex2exp)
library(ggplot2)
library(tidyr)
library(gridExtra)

set.seed(4)

n <- 1000
p <- 30
R_true <- 5
singular_values <- runif(R_true, 7,8)
R_values <- 1:10
folds <- 10

noise <- 'gaussian'
l_1 <- create_data(n, p, singular_values, noise)
X_1 <- l_1$X
E_1 <- l_1$E
X_centered_1 <- scale(X_1, center=TRUE, scale=FALSE) 

SNR_gaussian_1 <- norm(X_1-E_1, type = "F") / norm(E_1, type = "F")

df_1_time <- data.frame(R = R_values, 
                        time_repaired = rep(0, length(R_values)), 
                        time_EM = rep(0, length(R_values)),
                        time_matrix = rep(0, length(R_values)))

for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_EM_1 <- EM_CV(X_1, R, folds)
  run_time_EM_1 <- l_EM_1$runtime
  
  l_matrix_1 <- CV_hardtresholding(X_1, R, folds)
  run_time_matrix_1 <- l_matrix_1$runtime
  
  l_repaired_1 <- CV_PCA_repaired(X_centered_1, R, folds)
  run_time_repaired_1 <- l_repaired_1$runtime

  df_1_time$time_EM[i] <- run_time_EM_1
  df_1_time$time_matrix[i] <- run_time_matrix_1
  df_1_time$time_repaired[i] <- run_time_repaired_1
  
}

df_long_1_time <- df_1_time %>%
  pivot_longer(cols = starts_with("time"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_1_time$Technique <- factor(df_long_1_time$Technique, 
                                   levels = c("time_repaired", "time_EM", "time_matrix"))

g1time <- ggplot(df_long_1_time, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Gaussian Noise",
       x = "Number of principal components", 
       y = "Runtime (s)",
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"))+
  theme_minimal()+
  scale_x_continuous(breaks = R_values)+
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



noise <- 'heavy'
l_2 <- create_data(n, p, singular_values, noise)
X_2 <- l_2$X
E_2 <- l_2$E
X_centered_2 <- scale(X_2, center=TRUE, scale=FALSE) 

SNR_heavy_1 <- norm(X_2-E_2, type = "F") / norm(E_2, type = "F")

df_2_time <- data.frame(R = R_values, 
                        time_repaired = rep(0, length(R_values)), 
                        time_EM = rep(0, length(R_values)),
                        time_matrix = rep(0, length(R_values)))

for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_EM_2 <- EM_CV(X_2, R, folds)
  run_time_EM_2 <- l_EM_2$runtime
  
  l_matrix_2 <- CV_hardtresholding(X_2, R, folds)
  run_time_matrix_2 <- l_matrix_2$runtime
  
  l_repaired_2 <- CV_PCA_repaired(X_centered_2, R, folds)
  run_time_repaired_2 <- l_repaired_2$runtime
  
  df_2_time$time_EM[i] <- run_time_EM_2
  df_2_time$time_matrix[i] <- run_time_matrix_2
  df_2_time$time_repaired[i] <- run_time_repaired_2
  
}

df_long_2_time <- df_2_time %>%
  pivot_longer(cols = starts_with("time"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_2_time$Technique <- factor(df_long_2_time$Technique, 
                                   levels = c("time_repaired", "time_EM", "time_matrix"))

g2time <- ggplot(df_long_2_time, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Heavy Noise",
       x = "Number of principal components", 
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"))+
  theme_minimal()+
  scale_x_continuous(breaks = R_values)+
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


noise <- 'colored'
l_3 <- create_data(n, p, singular_values, noise)
X_3 <- l_3$X
E_3 <- l_3$E
X_centered_3 <- scale(X_3, center=TRUE, scale=FALSE) 

SNR_heavy_3 <- norm(X_3-E_3, type = "F") / norm(E_3, type = "F")

df_3_time <- data.frame(R = R_values, 
                        time_repaired = rep(0, length(R_values)), 
                        time_EM = rep(0, length(R_values)),
                        time_matrix = rep(0, length(R_values)))

for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_EM_3 <- EM_CV(X_3, R, folds)
  run_time_EM_3 <- l_EM_3$runtime
  
  l_matrix_3 <- CV_hardtresholding(X_3, R, folds)
  run_time_matrix_3 <- l_matrix_3$runtime
  
  l_repaired_3 <- CV_PCA_repaired(X_centered_3, R, folds)
  run_time_repaired_3 <- l_repaired_3$runtime
  
  df_3_time$time_EM[i] <- run_time_EM_3
  df_3_time$time_matrix[i] <- run_time_matrix_3
  df_3_time$time_repaired[i] <- run_time_repaired_3
  
}

df_long_3_time <- df_3_time %>%
  pivot_longer(cols = starts_with("time"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_3_time$Technique <- factor(df_long_3_time$Technique, 
                                   levels = c("time_repaired", "time_EM", "time_matrix"))

g3time <- ggplot(df_long_3_time, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Colored Noise",
       x = "Number of principal components", 
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"),
                     labels = c("Gabriel CV", "EM CV", "Matrix Completion"))+
  theme_minimal()+
  scale_x_continuous(breaks = R_values)+
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

g<-grid.arrange(g1time, g2time, g3time, ncol=3,
                widths= c(1.1,1,1))

ggsave(file.path("Report_folder", "runtime.png"), g, width = 25, height = 9.5, units = "cm")
