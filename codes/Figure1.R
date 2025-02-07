# install.packages(c('ggplot2', 'tidyr', 'gridExtra','latex2exp'))

source('CV_Gabriel.R')
source('CV_EM.R')
source('CV_Matrix.R')
source('create_data.R')
library(ggplot2)
library(tidyr)
library(gridExtra)
library(latex2exp)

set.seed(98)

p <- 20
R_true <- 5
singular_values <- runif(R_true, 4, 9)
R_values <- 1:10
folds <- 10

P1 <- 0.6
P2 <- 0.7
P3 <- 0.8

n <- 1000

noise <- 'gaussian'
l_1 <- create_data(n, p, singular_values, noise)
X_1 <- l_1$X
E_1 <- l_1$E
X_centered_1 <- scale(X_1, center=TRUE, scale=FALSE) 


df_1_cv <- data.frame(R = R_values, 
                      CV_repaired_1 = rep(0, length(R_values)), 
                      CV_repaired_2 = rep(0, length(R_values)),
                      CV_repaired_3 = rep(0, length(R_values)))


for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_1 <- CV_PCA_repaired(X_centered_1, R, folds, P1)
  CV_1 <- l_1$CV
  
  l_2 <- CV_PCA_repaired(X_centered_1, R, folds, P2)
  CV_2 <- l_2$CV
  
  l_3 <- CV_PCA_repaired(X_centered_1, R, folds, P3)
  CV_3 <- l_3$CV
  
  df_1_cv$CV_repaired_1[i] <- CV_1
  df_1_cv$CV_repaired_2[i] <- CV_2
  df_1_cv$CV_repaired_3[i] <- CV_3
  
}


df_long_1_cv <- df_1_cv %>%
  pivot_longer(cols = starts_with("CV"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_1_cv$Technique <- factor(df_long_1_cv$Technique, 
                                 levels = c("CV_repaired_1", "CV_repaired_2", "CV_repaired_3"))

g7cv <- ggplot(df_long_1_cv, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = NULL,
       x = "Number of principal components", 
       y = "CV Error",
       color = NULL) +
  scale_color_manual(values = c("#773719", "#EE9E01", "#EECE4D"))+
  theme_minimal()+
  scale_x_continuous(breaks = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10))+
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

SNR_heavy <- norm(X_2-E_2, type = "F") / norm(E_2, type = "F")

df_2_cv <- data.frame(R = R_values, 
                      CV_repaired_1 = rep(0, length(R_values)), 
                      CV_repaired_2 = rep(0, length(R_values)),
                      CV_repaired_3 = rep(0, length(R_values)))


for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_1 <- CV_PCA_repaired(X_centered_2, R, folds, P1)
  CV_1 <- l_1$CV
  
  l_2 <- CV_PCA_repaired(X_centered_2, R, folds, P2)
  CV_2 <- l_2$CV
  
  l_3 <- CV_PCA_repaired(X_centered_2, R, folds, P3)
  CV_3 <- l_3$CV
  
  df_2_cv$CV_repaired_1[i] <- CV_1
  df_2_cv$CV_repaired_2[i] <- CV_2
  df_2_cv$CV_repaired_3[i] <- CV_3
  
}

df_long_2_cv <- df_2_cv %>%
  pivot_longer(cols = starts_with("CV"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_2_cv$Technique <- factor(df_long_2_cv$Technique, 
                                 levels = c("CV_repaired_1", "CV_repaired_2", "CV_repaired_3")
)


g8cv <- ggplot(df_long_2_cv, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = NULL,
       x = "Number of principal components", 
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("#773719", "#EE9E01", "#EECE4D"),
                     labels = c(
                       TeX(paste0("$\\pi = ", P1, "$")),
                       TeX(paste0("$\\pi = ", P2, "$")),
                       TeX(paste0("$\\pi = ", P3, "$"))))+
  theme_minimal()+
  scale_x_continuous(breaks = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(legend.position = c(0.4, 0.75),
        legend.text = element_text(size = 11, family = "sans"),
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
l_3 <- create_data(n, p, singular_values, noise)
X_3 <- l_3$X
E_3 <- l_3$E
X_centered_3 <- scale(X_3, center=TRUE, scale=FALSE) 

SNR_colored <- norm(X_3-E_3, type = "F") / norm(E_3, type = "F")
print(SNR_colored)

## CV with gaussian

df_3_cv <- data.frame(R = R_values, 
                      CV_repaired_1 = rep(0, length(R_values)), 
                      CV_repaired_2 = rep(0, length(R_values)),
                      CV_repaired_3 = rep(0, length(R_values)))


for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_1 <- CV_PCA_repaired(X_centered_3, R, folds, P1)
  CV_1 <- l_1$CV
  
  l_2 <- CV_PCA_repaired(X_centered_3, R, folds, P2)
  CV_2 <- l_2$CV
  
  l_3 <- CV_PCA_repaired(X_centered_3, R, folds, P3)
  CV_3 <- l_3$CV
  
  df_3_cv$CV_repaired_1[i] <- CV_1
  df_3_cv$CV_repaired_2[i] <- CV_2
  df_3_cv$CV_repaired_3[i] <- CV_3
  
  
}

df_long_3_cv <- df_3_cv %>%
  pivot_longer(cols = starts_with("CV"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_3_cv$Technique <- factor(df_long_3_cv$Technique, 
                                 levels = c("CV_repaired_1", "CV_repaired_2", "CV_repaired_3"))


g9cv <- ggplot(df_long_3_cv, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = NULL,
       x = "Number of principal components", 
       y = NULL,
       color = NULL) +
  scale_color_manual(values = c("#773719", "#EE9E01", "#EECE4D"))+
  scale_y_continuous(
    sec.axis = dup_axis(name = TeX(paste0("$n = ", n, "$")),
    breaks = numeric(0))  
  ) +
  theme_minimal()+
  scale_x_continuous(breaks = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10))+
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


g<-grid.arrange(g7cv, g8cv, g9cv,ncol = 3,
                widths = c(1.15, 1, 1.15))

ggsave(file.path("Report_folder", "Gabriel.png"), g, width = 25, height = 9, units = "cm")
