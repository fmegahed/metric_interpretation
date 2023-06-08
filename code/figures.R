setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(mltools, RColorBrewer, tidyverse, caret)

# conflicted::conflict_prefer(confusionMatrix, caret)
source("dummy_classifier_tessa.R")

colorPal = brewer.pal(n = 3, 'Dark2')

set.seed(6)
M <- 30
m <- 10
cell_train <- data.frame(State = c(rep("normal", M), 
                                   rep("diseased", m)))
cell_train$State <- sample(cell_train$State, (M+m), replace=F)
cell_train$State <- as.factor(cell_train$State)

result_uniform <- dummy_classifier(strategy = "uniform", random_state = 2024)(cell_train, cell_train$State)
result_proportional <- dummy_classifier(strategy = "proportional", random_state = 2024)(cell_train, cell_train$State)
result_constant <- dummy_classifier(strategy = "most_frequent")(cell_train, cell_train$State)


cell_train$uniform <- factor(predict_dummy_classifier(result_uniform, cell_train), levels = levels(cell_train$State))
cell_train$proportional <- factor(predict_dummy_classifier(result_proportional, cell_train), levels = levels(cell_train$State))
cell_train$constant <- factor(predict_dummy_classifier(result_constant, cell_train), levels = levels(cell_train$State))

cell_train$Index <- 1:nrow(cell_train)

p1 <- cell_train %>% 
  ggplot(aes(x=Index, y=uniform, color=State)) +
  geom_point(size=1) +
  theme_classic() +
  scale_color_manual(values = c("normal" = "#A0A0A0", 
                                "diseased" = "red")) +
  labs(title = "Figure 1a: Uniform Random Predictions") +
  ylab("Predicted Class") +
  theme(legend.position = "top", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        plot.margin=unit(c(5.5, 12.5, 5.5, 12.5), "points"))

p2 <- cell_train %>% 
  ggplot(aes(x=Index, y=proportional, color=State)) +
  geom_point(size=1) +
  theme_classic() +
  scale_color_manual(values = c("normal" = "#A0A0A0", 
                                "diseased" = "red")) +
  labs(title = "Figure 1b: Proportional Random Predictions") +
  ylab("Predicted Class") +
  theme(legend.position = "top", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        plot.margin=unit(c(5.5, 12.5, 5.5, 12.5), "points"))

p3 <- cell_train %>% 
  ggplot(aes(x=Index, y=constant, color=State)) +
  geom_point(size=1) +
  theme_classic() +
  scale_color_manual(values = c("normal" = "#A0A0A0", 
                                "diseased" = "red")) +
  labs(title = "Figure 1c: Most Frequent Random Predictions") +
  ylab("Predicted Class") +
  theme(legend.position = "top", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        plot.margin=unit(c(5.5, 12.5, 5.5, 12.5), "points")) +
  scale_y_discrete(drop = FALSE)


Figure1 <- ggpubr::ggarrange(p1, p2, p3, nrow=1, ncol=3, common.legend=TRUE,
                             legend = "top")
ggsave("../results/Figure1.pdf", plot=Figure1, width = 210, height = 61, units="mm", dpi = 700)


n <- 100
M <- n*0.5
m <- n*0.5

set.seed(1234)
cell_train1 <- data.frame(State = c(rep("normal", M), 
                                   rep("diseased", m)), 
                          X = 1:n)
cell_train1$State <- sample(cell_train1$State, n, replace=F)
cell_train1$State <- as.factor(cell_train1$State)

model1 <- train(State~., data = cell_train1,
                method = dummy_classifier_caret, 
                strategy = "uniform", random_state = 2023)

model2 <- train(State~., data = cell_train1,
                                method = dummy_classifier_caret, 
                                strategy = "proportional", random_state = 2023)

model3 <- train(State~., data = cell_train1,
                method = dummy_classifier_caret, 
                strategy = "constant", 
                constant = "normal")

# Make predictions using the trained dummy classifier

classifier1 <- predict(model1, cell_train1)
classifier2 <- predict(model2, cell_train1)
classifier3 <- predict(model3, cell_train1)


# Evaluate the performance of the dummy classifier

classifier_confusion_matrix1 <- caret::confusionMatrix(classifier1, cell_train1$State)
classifier_confusion_matrix2 <- caret::confusionMatrix(classifier2, cell_train1$State)
classifier_confusion_matrix3 <- caret::confusionMatrix(classifier3, cell_train1$State)

Metric <- rep(c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1", "MCC"), 3)
Perform_Metric <- data.frame(Metric = Metric, 
                             Value = rep(NA, length(Metric)), 
                             Strategy = rep(c("Uniform", "Proportional", "Most Frequent"), each=6))

Perform_Metric[Perform_Metric$Strategy == "Uniform", "Value"] <- 
  c(classifier_confusion_matrix1$overall["Accuracy"],
    classifier_confusion_matrix1$byClass[c("Sensitivity", "Specificity", "Precision", "F1")],
    mltools::mcc(classifier1, cell_train1$State))

Perform_Metric[Perform_Metric$Strategy == "Proportional", "Value"] <- 
  c(classifier_confusion_matrix2$overall["Accuracy"],
    classifier_confusion_matrix2$byClass[c("Sensitivity", "Specificity", "Precision", "F1")],
    mltools::mcc(classifier2, cell_train1$State))

Perform_Metric[Perform_Metric$Strategy == "Most Frequent", "Value"] <- 
  c(classifier_confusion_matrix3$overall["Accuracy"],
    classifier_confusion_matrix3$byClass[c("Sensitivity", "Specificity", "Precision", "F1")],
    mltools::mcc(classifier3, cell_train1$State))

Perform_Metric$Strategy <- factor(Perform_Metric$Strategy, levels = c("Uniform", "Proportional", "Most Frequent"))
Perform_Metric$Metric <- factor(Perform_Metric$Metric, levels = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1", "MCC"))

set.seed(2023)

p4 <- Perform_Metric %>% ggplot(aes(x = Metric, y = Value, color = Strategy)) +
  geom_point(size=1, position=position_jitter(w=0.1)) +
  theme_classic() +
  scale_color_manual(values = colorPal[1:3]) +
  labs(title = "Figure 2a: Balanced Data") +
  xlab("Performance Metric") +
  ylim(c(-0.2, 1))+
  theme(legend.position = "top", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        plot.margin=unit(c(5.5, 12.5, 5.5, 12.5), "points"),
        axis.text.x = element_text(angle = 30, vjust = 0.5)) 


n <- 100
M <- n*0.9
m <- n*0.1

set.seed(5678)
cell_train2 <- data.frame(State = c(rep("normal", M), 
                                    rep("diseased", m)),
                          X = 1:n)
cell_train2$State <- sample(cell_train2$State, n, replace=F)
cell_train2$State <- as.factor(cell_train2$State)


model4 <- train(State~., data = cell_train2,
                method = dummy_classifier_caret, 
                strategy = "uniform", random_state = 2023)

model5 <- train(State~., data = cell_train2,
                method = dummy_classifier_caret, 
                strategy = "proportional", random_state = 2023)

model6 <- train(State~., data = cell_train2,
                method = dummy_classifier_caret, 
                strategy = "most_frequent")

# Make predictions using the trained dummy classifier

classifier4 <- predict(model4, cell_train2)
classifier5 <- predict(model5, cell_train2)
classifier6 <- predict(model6, cell_train2)


# Evaluate the performance of the dummy classifier

classifier_confusion_matrix4 <- caret::confusionMatrix(classifier4, cell_train2$State)
classifier_confusion_matrix5 <- caret::confusionMatrix(classifier5, cell_train2$State)
classifier_confusion_matrix6 <- caret::confusionMatrix(classifier6, cell_train2$State)

Perform_Metric_imbalanced <- data.frame(Metric = Metric, 
                             Value = rep(NA, length(Metric)), 
                             Strategy = rep(c("Uniform", "Proportional", "Most Frequent"), each=6))

Perform_Metric_imbalanced[Perform_Metric_imbalanced$Strategy == "Uniform", "Value"] <- 
  c(classifier_confusion_matrix4$overall["Accuracy"],
    classifier_confusion_matrix4$byClass[c("Sensitivity", "Specificity", "Precision", "F1")],
    mltools::mcc(classifier4, cell_train2$State))

Perform_Metric_imbalanced[Perform_Metric_imbalanced$Strategy == "Proportional", "Value"] <- 
  c(classifier_confusion_matrix5$overall["Accuracy"],
    classifier_confusion_matrix5$byClass[c("Sensitivity", "Specificity", "Precision", "F1")],
    mltools::mcc(classifier5, cell_train2$State))

Perform_Metric_imbalanced[Perform_Metric_imbalanced$Strategy == "Most Frequent", "Value"] <- 
  c(classifier_confusion_matrix6$overall["Accuracy"],
    classifier_confusion_matrix6$byClass[c("Sensitivity", "Specificity", "Precision", "F1")],
    mltools::mcc(classifier6, cell_train2$State))

Perform_Metric_imbalanced$Strategy <- factor(Perform_Metric_imbalanced$Strategy, levels = c("Uniform", "Proportional", "Most Frequent"))
Perform_Metric_imbalanced$Metric <- factor(Perform_Metric_imbalanced$Metric, levels = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1", "MCC"))

set.seed(2023)

p5 <- Perform_Metric_imbalanced %>% ggplot(aes(x = Metric, y = Value, color = Strategy)) +
  geom_point(size=1, position=position_jitter(w=0.1))+
  theme_classic() +
  scale_color_manual(values = colorPal[1:3]) +
  labs(title = "Figure 2b: Imbalanced Data") +
  xlab("Performance Metric") +
  ylim(c(-0.2, 1))+
  theme(legend.position = "top", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 7),
        plot.margin=unit(c(5.5, 12.5, 5.5, 12.5), "points"),
        axis.text.x = element_text(angle = 30, vjust = 0.5)) 



Figure2 <- ggpubr::ggarrange(p4, p5, nrow=1, ncol=2, common.legend=TRUE,
                             legend = "top")
ggsave("../results/Figure2.pdf", plot=Figure2, width = 140, height = 61, units="mm", dpi = 700)


