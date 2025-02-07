# install.packages(c('ggplot2', 'tidyr', 'gridExtra','latex2exp','MVN'))

source('CV_Gabriel.R')
source('CV_EM.R')
source('CV_Matrix.R')
source('create_data.R')
library(ggplot2)
library(tidyr)
library(gridExtra)
library(latex2exp)
library(MVN)


set.seed(5032)

#5032

X <- read.csv("data_real_2.csv", header = FALSE)
X <- as.matrix(X)
X[,12] <-log(X[,12])
X[,13] <-log(X[,13])
X[,14] <-log(X[,14])
X_centered <- scale(X, center=TRUE, scale=TRUE) 

R_values <- 1:6
folds <- 5

# result <- mvn(data = X_centered, mvnTest = "mardia")
# print(result$multivariateNormality)
# result <- mvn(data = X_centered, mvnTest = "hz")
# print(result$multivariateNormality)
# result <- mvn(data = X_centered, mvnTest = "royston")
# print(result$multivariateNormality)
# result <- mvn(data = X_centered, mvnTest = "dh")
# print(result$multivariateNormality)
# result <- mvn(data = X_centered, mvnTest = "energy")
# print(result$multivariateNormality)


df_cv <- data.frame(R = R_values, 
                    CV_repaired = rep(0, length(R_values)), 
                    CV_em = rep(0, length(R_values)),
                    CV_mc = rep(0, length(R_values)))


for (i in 1:length(R_values)) {
  R <- R_values[i]
  
  l_1 <- CV_PCA_repaired(X_centered, R, folds, 0.7)
  CV_1 <- l_1$CV
  
  l_2 <- EM_CV(X_centered, R, folds)
  CV_2 <- l_2$CV
  
  l_3 <- CV_hardtresholding(X_centered, R, folds)
  CV_3 <- l_3$CV
  
  df_cv$CV_repaired[i] <- CV_1
  df_cv$CV_em[i] <- CV_2
  df_cv$CV_mc[i] <- CV_3
  
}


df_long_cv <- df_cv %>%
  pivot_longer(cols = starts_with("CV"), 
               names_to = "Technique", 
               values_to = "Value")

df_long_cv$Technique <- factor(df_long_cv$Technique, 
                               levels = c("CV_repaired", "CV_em", "CV_mc"))

g <- ggplot(df_long_cv, aes(x = R, y = Value, color = Technique)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = NULL,
       x = "Number of principal components", 
       y = "CV Error",
       color = NULL) +
  scale_color_manual(values = c("#20A7F3", "#C22445", "#2BA061"),
                     labels = c("Gabriel CV", "EM CV", "Matrix Completion"))+
  theme_minimal()+
  scale_x_continuous(breaks = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme(legend.position = c(0.25,0.8),
        legend.text = element_text(size = 13, family = "sans"),
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
print(g)
ggsave(file.path("Report_folder", "pollution.png"), g, width = 25, height = 12, units = "cm")
