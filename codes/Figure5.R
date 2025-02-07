source('CV_Gabriel.R')
source('CV_EM.R')
source('CV_Matrix.R')
source('create_data.R')
library(latex2exp)
library(ggplot2)
library(tidyr)
library(gridExtra)

set.seed(125)

n <- 1000
p <- 30
R_true <- 17
singular_values <- runif(R_true, 15, 17)
R_values <- 15:19  
folds <- 10


## CV with gaussian

noise <- 'gaussian'
l_1 <- create_data(n, p, singular_values, noise)
X_1 <- l_1$X
E_1 <- l_1$E
X_centered_1 <- scale(X_1, center=TRUE, scale=FALSE) 

SNR_gaussian_1 <- norm(X_1-E_1, type = "F") / norm(E_1, type = "F")

df_1_cv <- data.frame(R = R_values, 
                      CV_repaired = rep(0, length(R_values)), 
                      CV_EM = rep(0, length(R_values)),
                      CV_matrix = rep(0, length(R_values)))

for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_EM_1 <- EM_CV(X_1, R, folds)
  CV_EM_1 <- l_EM_1$CV
  
  l_matrix_1 <- CV_hardtresholding(X_1, R, folds)
  CV_matrix_1 <- l_matrix_1$CV
  
  l_repaired_1 <- CV_PCA_repaired(X_centered_1, R, folds)
  CV_repaired_1 <- l_repaired_1$CV
  
  df_1_cv$CV_EM[i] <- CV_EM_1
  df_1_cv$CV_matrix[i] <- CV_matrix_1
  df_1_cv$CV_repaired[i] <- CV_repaired_1
  
}


df_long_1_cv <- df_1_cv %>%
  pivot_longer(cols = starts_with("CV"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_1_cv$Technique <- factor(df_long_1_cv$Technique, 
                                 levels = c("CV_repaired", "CV_EM", "CV_matrix"))

g1cv <- ggplot(df_long_1_cv, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Gaussian Noise",
       x = "Number of principal components", 
       y = "CV Error",
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


## CV with heavy

noise <- 'heavy'

l_2 <- create_data(n, p, singular_values, noise)
X_2 <- l_2$X
E_2 <- l_2$E
X_centered_2 <- scale(X_2, center=TRUE, scale=FALSE) 

SNR_heavy_1 <- norm(X_2-E_2, type = "F") / norm(E_2, type = "F")

df_2_cv <- data.frame(R = R_values, 
                      CV_repaired = rep(0, length(R_values)), 
                      CV_EM = rep(0, length(R_values)),
                      CV_matrix = rep(0, length(R_values)))


for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_EM_2 <- EM_CV(X_2, R, folds)
  CV_EM_2 <- l_EM_2$CV
  
  l_matrix_2 <- CV_hardtresholding(X_2, R, folds)
  CV_matrix_2 <- l_matrix_2$CV
  
  l_repaired_2 <- CV_PCA_repaired(X_centered_2, R, folds)
  CV_repaired_2 <- l_repaired_2$CV
  
  df_2_cv$CV_EM[i] <- CV_EM_2
  df_2_cv$CV_matrix[i] <- CV_matrix_2
  df_2_cv$CV_repaired[i] <- CV_repaired_2
  
}

df_long_2_cv <- df_2_cv %>%
  pivot_longer(cols = starts_with("CV"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_2_cv$Technique <- factor(df_long_2_cv$Technique, 
                                 levels = c("CV_repaired", "CV_EM", "CV_matrix"))


g2cv <- ggplot(df_long_2_cv, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Heavy Noise",
       x = "Number of principal components", 
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"), 
                     labels = c("Gabriel CV", "EM CV", "Matrix Completion"))+
  theme_minimal()+
  scale_x_continuous(breaks = R_values)+
  theme(legend.position = c(0.4,0.8),
        legend.text = element_text(size = 9, family = "sans"),
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

## CV with colored

noise <- 'colored'

singular_values <- runif(R_true, 15.5, 16.5)

l_3 <- create_data(n, p, singular_values, noise)
X_3 <- l_3$X
E_3 <- l_3$E
X_centered_3 <- scale(X_3, center=TRUE, scale=FALSE) 

SNR_colored_1 <- norm(X_3-E_3, type = "F") / norm(E_3, type = "F")

df_3_cv <- data.frame(R = R_values, 
                      CV_repaired = rep(0, length(R_values)), 
                      CV_EM = rep(0, length(R_values)),
                      CV_matrix = rep(0, length(R_values)))


for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_EM_3 <- EM_CV(X_3, R, folds)
  CV_EM_3 <- l_EM_3$CV
  
  l_matrix_3 <- CV_hardtresholding(X_3, R, folds)
  CV_matrix_3 <- l_matrix_3$CV
  
  l_repaired_3 <- CV_PCA_repaired(X_centered_3, R, folds)
  CV_repaired_3 <- l_repaired_3$CV
  
  df_3_cv$CV_EM[i] <- CV_EM_3 
  df_3_cv$CV_matrix[i] <- CV_matrix_3 
  df_3_cv$CV_repaired[i] <- CV_repaired_3
  
}

df_long_3_cv <- df_3_cv %>%
  pivot_longer(cols = starts_with("CV"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_3_cv$Technique <- factor(df_long_3_cv$Technique, 
                                 levels = c("CV_repaired", "CV_EM", "CV_matrix"))

SNR_1 <- c(SNR_gaussian_1, SNR_heavy_1, SNR_colored_1)

g3cv <- ggplot(df_long_3_cv, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Colored Noise",
       x = "Number of principal components", 
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"))+
  #  scale_y_continuous(
  #    sec.axis = dup_axis(name = TeX(paste0("$SNR \\in (",round(min(SNR_1), 2) ,",",round(max(SNR_1), 2),")", "$")),
  #                        breaks = numeric(0))  
  #  ) +
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

g <- grid.arrange(g1cv, g2cv, g3cv, ncol=3,
                  widths = c(1.15, 1, 1))

ggsave(file.path("Report_folder", "accuracy_high_rank.png"), g, width = 25, height = 9.5, units = "cm")

