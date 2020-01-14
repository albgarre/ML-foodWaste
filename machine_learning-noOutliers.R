
## Load libraries

library(tidyverse)
library(GGally)
library(readxl)
library(corrplot)
library(rpart)
library(rpart.plot)
library(ipred)
library(caret)
library(rsample)
library(ggExtra)

## Import data

# data1 <- read_excel("eloy/Análisis merma ProduccOK_v3 (3).xlsx", sheet = "datos1")
# data2 <- read_excel("eloy/Análisis merma ProduccOK_v3 (3).xlsx", sheet = "datos2")
# 
# 
# setNames(data2, my_names)
# names(data1)[which(names(data1) != names(data2))]
# names(data2)[which(names(data1) != names(data2))]

my_names <- c("escandallo", "descrip", "recipe", "date", "out_1",
              "llenadora", "plan_prod", "N_mezclas", "plan_peso",
              "real_peso", "increm_extra", "real_peso_TQ", 
              "prod_salida",
              "prod_valida", "retales", "out_2",
              "merma_prod_val_pctg", "merma_prod_val_peso",
              "out_3", "merma_total_pctg", "merma_total_peso",
              "out_4", "exceso_prod_peso", "exceso_prod_pctg",
              "out_5", "prod_OK", "bidones_extra", "bidones_corto",
              "out_6", "OrdCant", "peso", "formato", "rango",
              "grupo_mezclas", "out_7", "familia_prod",
              "tipo_prod", "filtro_llenadora", "filtro_linea",
              "homo", "celdillas", "bx_min", "bx_max", "ac_min",
              "ac_max", "ph_max", "linea", "desc_linea")

# data_frame(my_names, names(data1)) %>% View()

all_data <- list(datos1 = "datos1", datos2 = "datos2") %>%
    map(., ~ read_excel("eloy/Análisis merma ProduccOK_v3 (3).xlsx", sheet = .)) %>%
    map(., ~ set_names(., my_names)) %>%
    map(., ~ select(., -starts_with("out_"))) %>%
    map(., ~ mutate(., recipe = as.character(recipe))) %>%
    map_dfr(., ~ select(., -descrip)) %>%
    select(-desc_linea)

# prod_valida ~ real_peso + .

# all_data <- list(datos1 = "datos1", datos2 = "datos2") %>%
#     map(., ~ read_excel("eloy/Análisis merma ProduccOK_v3 (3).xlsx", sheet = .)) %>%
#     map(., ~ set_names(., my_names)) %>%
#     map(., ~ select(., -starts_with("out_"))) %>%
#     # map(., ~ dplyr::mutate(., recipe = as.character(recipe))) %>%
#     map_dfr(., ~ select(., -descrip))
# 
# all_data$recipe <- as.character(all_data$recipe)

all_data %>%
    mutate_if(is.character, as.factor) %>%
    summary()

## Figure 2

all_data %>% names()

# ggplot(all_data) +
#     geom_point(aes(x = plan_prod, y = merma_prod_val_peso))


ggplot(all_data, aes(x = real_peso, y = real_peso - prod_valida)) +
    geom_point() +
    geom_smooth() +
    cowplot::theme_cowplot() +
    xlab("Input weight (Tn)") + ylab("Production loss (Kg)")

ggplot(all_data) +
    geom_point(aes(x = real_peso, y = prod_valida/real_peso))

# ## Look for outliers

# filtered_data <- all_data %>%
#     mutate(y = prod_valida - real_peso) %>%
#     select(y,
#            real_peso,
#            llenadora, N_mezclas,
#            formato,
#            "familia_prod",
#            "tipo_prod", "filtro_llenadora", "filtro_linea",
#            "homo",
#            # "celdillas",
#            "bx_min", "bx_max", "ac_min",
#            "ac_max", "ph_max"# , # "linea",
#            # "desc_linea"
#     ) %>%
#     filter(!is.na(y)) %>%
#     filter(!is.na(real_peso))


ggplot(all_data) +
    geom_boxplot(aes(x = 1, y = prod_valida - real_peso))

IQR.outliers <- function(x) {
    Q3<-quantile(x,0.75)
    Q1<-quantile(x,0.25)
    IQR<-(Q3-Q1)
    left<- Q1-1.5*IQR
    right<- Q3+1.5*IQR
    c(x[x <left],x[x>right])
}

# filtered_data <- all_data %>%
#     select(merma_prod_val_peso, plan_prod, llenadora, N_mezclas,
#            formato,
#            "familia_prod",
#            "tipo_prod", "filtro_llenadora", "filtro_linea",
#            "homo",
#            # "celdillas",
#            "bx_min", "bx_max", "ac_min",
#            "ac_max", "ph_max"# , # "linea",
#            # "desc_linea"
#            ) %>%
#     na.omit() %>%
#     # mutate(Q1 = quantile(merma_prod_val_peso/plan_prod, 0.25),
#     #        Q3 = quantile(merma_prod_val_peso/plan_prod, 0.75),
#     #        IQR = Q3 - Q1,
#     #        above = merma_prod_val_peso/plan_prod > Q3 + 1.5*IQR,
#     #        below = merma_prod_val_peso/plan_prod < Q1 - 1.5*IQR,
#     #        outlier = above + below) %>%
# 
#     mutate(Q1 = quantile(merma_prod_val_peso, 0.25),
#            Q3 = quantile(merma_prod_val_peso, 0.75),
#            IQR = Q3 - Q1,
#            above = merma_prod_val_peso > Q3 + 1.5*IQR,
#            below = merma_prod_val_peso < Q1 - 1.5*IQR,
#            outlier = above + below) %>%
# 
#     # .$outlier %>% table()
#     filter(!outlier) %>%
#     # ggplot() +
#     #     geom_boxplot(aes(x = 1, y = merma_prod_val_peso))
#     select(-Q1, -Q3, -IQR, -above, -below, -outlier) # %>% names()

filtered_data <- all_data %>%
    mutate(y = prod_valida/real_peso) %>%
    # mutate(y = prod_valida - real_peso) %>%
    select(y, 
           real_peso, llenadora, N_mezclas,
           formato,
           "familia_prod",
           "tipo_prod", "filtro_llenadora", "filtro_linea",
           "homo",
           # "celdillas",
           "bx_min", "bx_max", "ac_min",
           "ac_max", "ph_max"# , # "linea",
           # "desc_linea"
           ) %>%
    na.omit() %>%
    # mutate(Q1 = quantile(merma_prod_val_peso/plan_prod, 0.25),
    #        Q3 = quantile(merma_prod_val_peso/plan_prod, 0.75),
    #        IQR = Q3 - Q1,
    #        above = merma_prod_val_peso/plan_prod > Q3 + 1.5*IQR,
    #        below = merma_prod_val_peso/plan_prod < Q1 - 1.5*IQR,
    #        outlier = above + below) %>%

    mutate(Q1 = quantile(y, 0.25),
           Q3 = quantile(y, 0.75),
           IQR = Q3 - Q1,
           above = y > Q3 + 1.5*IQR,
           below = y < Q1 - 1.5*IQR,
           outlier = above + below) %>%

    # .$outlier %>% table()
    filter(!outlier) %>%
    # ggplot() +
    #     geom_boxplot(aes(x = 1, y = merma_prod_val_peso))
    select(-Q1, -Q3, -IQR, -above, -below, -outlier) # %>% names()

# ## Correlations between the selected variables
# 
# filtered_data %>%
#     # select(increm_extra, plan_prod, llenadora, N_mezclas,
#     #        formato,
#     #        "familia_prod",
#     #        "tipo_prod", "filtro_llenadora", "filtro_linea",
#     #        "homo", "celdillas", "bx_min", "bx_max", "ac_min",
#     #        "ac_max", "ph_max", "linea", "desc_linea") %>%
#     # glimpse()
#     mutate_if(is.character, factor) %>%
#     mutate_if(is.factor, as.numeric) %>%
#     cor(., method = "kendall", use = "complete.obs") %>%
#     corrplot()
# 
# filtered_data %>%
#     # select(increm_extra, plan_prod, llenadora, N_mezclas,
#     #        formato,
#     #        "familia_prod",
#     #        "tipo_prod", "filtro_llenadora", "filtro_linea",
#     #        "homo", "celdillas", "bx_min", "bx_max", "ac_min",
#     #        "ac_max", "ph_max", "linea", "desc_linea") %>%
#     # na.omit() %>%
#     lm(merma_prod_val_peso ~ ., data = .) %>%
#     summary()  # Puff
# 
# # ## The "linea" col has many 1s. Group them as "Other"
# #
# # filtered_data <- filtered_data %>%
# #     mutate(linea = as.factor(linea))
#     # group_by(linea) %>%
#     # mutate(n_linea = n()) %>%
#     # ungroup() %>%
#     # mutate(linea = ifelse(n_linea > 5, as.character(linea), "Other")) %>%
#     # select(-n_linea)

## Split betwen training and test

set.seed(123)

make_other_stuff <- function(x, threshold = 10) {
    
    data_frame(x = x) %>%
        group_by(x) %>%
        mutate(n = n()) %>%
        mutate(y = ifelse(n < threshold, "Other", x)) %>%
        .$y
}

my_split <- filtered_data %>%
    # select(merma_prod_val_peso, plan_prod, llenadora, N_mezclas,
    #        formato,
    #        "familia_prod",
    #        "tipo_prod", "filtro_llenadora", "filtro_linea",
    #        "homo", "celdillas", "bx_min", "bx_max", "ac_min",
    #        "ac_max", "ph_max", "linea", "desc_linea") %>%
    mutate_if(is.character, make_other_stuff) %>%
    mutate_if(is.character, factor) %>%
    # na.omit() %>%
    initial_split(., prop = .7)

train_set <- training(my_split)
test_set  <- testing(my_split)

## Regression tree

my_tree <- rpart(y ~ ., data = train_set,
                 method = "anova")

printcp(my_tree) # display the results 
plotcp(my_tree) # visualize cross-validation results 
summary(my_tree) # detailed summary of splits

rpart.plot(my_tree)
my_tree$cptable
my_tree$variable.importance    

pred <- predict(my_tree, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)

## Bagging

bag_model <- bagging(y ~ ., data = train_set, 
                     coob = TRUE)

pred <- predict(bag_model, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)

## Bagging with caret

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

set.seed(9872)

bag2_model <- train(y ~ ., data = train_set,
                    method = "treebag",
                    trControl = ctrl,
                    importance = TRUE
)

# bag2_model
# plot(varImp(bag2_model), 20)

pred <- predict(bag2_model, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)


step_model <- train(y ~ ., data = train_set,
                    method = "lmStepAIC"
                    # trControl = ctrl
)

pred <- predict(step_model, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)

# nnet_model <- train(merma_prod_val_peso ~ ., data = train_set,
#                     method = "nnet",
#                     trControl = ctrl
# )
# 
# pred <- predict(nnet_model, newdata = test_set)
# RMSE(pred = pred, obs = test_set$merma_prod_val_peso)

set.seed(1241)

randforest_model <- train(y ~ ., data = train_set,
                          method = "ranger",
                          trControl = ctrl,
                          tuneGrid = expand.grid(min.node.size = 40:50,
                                                 mtry = 12:15,
                                                 splitrule = "variance"),
                          seed = 12412,
                          importance = "impurity"
)

# ncol(train_set) %>% sqrt()

# library(ranger)

# ranger(merma_prod_val_peso ~ ., data = train_set,
#        min.node.size = 80) %>%
#     predict(., data = test_set) %>%
#     .$predictions %>%
#     RMSE(pred = ., obs = test_set$merma_prod_val_peso)

randforest_model$results %>%
    ggplot() +
        geom_tile(aes(x = min.node.size, y = mtry, fill = Rsquared))

defaultSummary(data.frame(pred = predict(randforest_model, newdata = test_set),
               obs = test_set$y))

# pred <- predict(randforest_model, newdata = test_set)
# resampleSummary(pred = pred, obs = test_set$merma_prod_val_peso)

##

set.seed(1243)

gradboost_model <- train(y ~ ., data = train_set,
                         method = "gbm",
                         trControl = ctrl#,
                         # tuneGrid = expand.grid(n.trees = seq(80, 100, by = 5),
                         #                        interaction.depth = 1,
                         #                        shrinkage = .1,
                         #                        n.minobsinnode = seq(25, 35, by = 5)
                         #                        )
)

pred <- predict(gradboost_model, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)

lasso_model <- train(y ~ ., data = train_set,
                     method = "lasso",
                     trControl = ctrl
                     # preProc = c("center", "scale")
)

pred <- predict(lasso_model, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)

ridge_model <- train(y ~ ., data = train_set,
                     method = "ridge",
                     trControl = ctrl
                     # preProc = c("center", "scale")
)

pred <- predict(ridge_model, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)

enet_model <- train(y ~ ., data = train_set,
                    method = "enet",
                    trControl = ctrl
                    # preProc = c("center", "scale")
)

pred <- predict(enet_model, newdata = test_set)
RMSE(pred = pred, obs = test_set$y)


spline_model <- train(y ~ ., data = train_set,
                      method = "earth",
                      trControl = ctrl
)


# bagspline_model <- train(merma_prod_val_peso ~ ., data = train_set,
#                          method = "bagEarth",
#                          trControl = ctrl
# )
# 
# pred <- predict(bagspline_model, newdata = test_set)
# RMSE(pred = pred, obs = test_set$merma_prod_val_peso)

## Model comparison

train_pars <- list(
    stepAIC = step_model,
    tree = my_tree,
    treebag = bag2_model,
    randforest = randforest_model,
    gradboost = gradboost_model,
    lasso = lasso_model,
    ridge = ridge_model,
    enet = enet_model,
    spline = spline_model
    # neurnet = nnet_model,
    ) %>%
    map(., ~ postResample(pred = predict(., newdata = train_set),
                          obs = train_set$y)) %>%
    imap_dfr(., ~ data_frame(par = names(.x), value = .x, model = .y)) %>%
    mutate(set = "train") # %>%
# spread(par, value) %>%
# arrange(desc(Rsquared))

test_pars <- list(
    stepAIC = step_model,
    tree = my_tree,
    treebag = bag2_model,
    randforest = randforest_model,
    gradboost = gradboost_model,
    lasso = lasso_model,
    ridge = ridge_model,
    enet = enet_model,
    spline = spline_model
    # neurnet = nnet_model,
    ) %>%
    map(., ~ postResample(pred = predict(., newdata = test_set),
                          obs = test_set$y)) %>%
    imap_dfr(., ~ data_frame(par = names(.x), value = .x, model = .y)) %>%
    mutate(set = "test") # %>%
# spread(par, value) %>%
# arrange(desc(Rsquared))

bind_rows(train_pars, test_pars) %>%
    ggplot() +
        geom_bar(aes(x = set, y = value, fill = model),
                 position = "dodge", stat = "identity") +
        facet_wrap(~ par, scales = "free") +
        xlab("") + ylab("")

bind_rows(train_pars, test_pars) %>%
    filter(par == "Rsquared") %>%
    ggplot() +
    geom_bar(aes(x = model, y = value, fill = set),
             position = "dodge", stat = "identity") +
    xlab("") + ylab("R-squared") +
    theme_minimal() +
    theme(legend.title = element_blank())

bind_rows(train_pars, test_pars) %>%
    filter(par == "RMSE") %>%
    select(-par) %>%
    spread(set, value)


bind_rows(train_pars, test_pars) %>%
    filter(model == "spline") %>%
    spread(set, value)


## Variable importance

my_importances <- summary(gradboost_model)

# randforest_model$finalModel$variable.importance
# plot(varImp(randforest_model), top = 20)

names_map <- tibble(var = c("llenadoraNO ASEP-2", "llenadoraSCHOLLE", "plan_prod",
                            "llenadoraASTEPO", "desc_lineaFTN.2 - CRUSHER- NO ASEP  -BIDON",
                            "N_mezclas", "bx_min", "ac_min", "ac_max", "bx_max", "llenadoraCISTERNA",
                            "ph_max", "llenadoraBIB", "formatoCISTERNA", 
                            "desc_lineaFTN.2 - CRUSHER-ASEP HRS.1-BIDON",
                            "desc_lineaCRUSHER-HOMO-SCHOLLE-BIDON",
                            "tipo_prodCOMINUTE", 
                            "desc_lineaFTN.2 - CRUSHER-HOMO- NO ASEP - BIDON",
                            "desc_lineaCRUSHER-SCHOLLE-BIDON",
                            "filtro_llenadora0,5 mm",
                            "real_peso",
                            "filtro_llenadora2 mm",
                            "homoS"
                            ),
                    my_label = c("Filler - I", "Filler - II", "plan_prod",
                                 "Filler - III", "AA",
                                 "N_tanks", "bx_min", "Minimum aw", "ac_max", "bx_max", "Filler - IV",
                                 "pH_max", "Filler - V", "BB",
                                 "CC",
                                 "DD",
                                 "EE",
                                 "FF",
                                 "GG",
                                 "Filter - 0.5mm",
                                 "input_weight",
                                 "filter - Type I",
                                 "Homogeneizer"
                                 )
                    )

# randforest_model$finalModel$variable.importance %>% 
#     tibble(par = names(.), importance = .) %>%
#     top_n(10) %>%
#     arrange(importance) %>%
#     left_join(., names_map) %>%
#     mutate(my_label = factor(my_label, levels = my_label)) %>%
#     ggplot(aes(x = importance, y = my_label)) +
#         geom_point() +
#         geom_segment(aes(xend = 0, yend = my_label)) +
#         xlab("Importance") + ylab("")

my_importances %>%
    top_n(6) %>%
    left_join(., names_map) %>%
    mutate(my_label = factor(my_label, levels = my_label)) %>%
    ggplot(aes(x = rel.inf, y = my_label)) +
        geom_point() +
        geom_segment(aes(xend = 0, yend = my_label)) +
        xlab("Importance (Reduction in squared error)") + ylab("") +
        theme_bw()

## Distribution of the residuals

my_sd <- data_frame(
    obs = c(train_set$y, test_set$y),
    pred = c(predict(gradboost_model, newdata = train_set), 
             predict(gradboost_model, newdata = test_set))
) %>%
    mutate(error = obs - pred,
           index = 1:nrow(.)) %>%
    .$error %>%
    sd()

print(my_sd)

p <- data_frame(
    obs = c(train_set$y, test_set$y),
    pred = c(predict(gradboost_model, newdata = train_set), 
             predict(gradboost_model, newdata = test_set))
    ) %>%
    mutate(error = obs - pred,
           index = 1:nrow(.)) %>%
    # .$error %>%
    # sd()
    ggplot() +
        geom_point(aes(x = pred, y = error)) +
        xlab("Predicted loss (Kg)") +
        ylab("Residual (Kg)") +
        theme_minimal()
        
ggMarginal(p, type = "histogram", margins = "y")

## Plots initial

filtered_data %>%
    mutate_if(is.character, as.factor) %>%
    summary()

# all_data %>%
#     summarize(mean(merma_prod_val_peso, na.rm = TRUE),
#               sd(merma_prod_val_peso, na.rm = TRUE))
# 
# all_data %>%
#     mutate(loss = merma_prod_val_peso < 0) %>%
#     group_by(loss) %>%
#     summarize(mean(merma_prod_val_peso), n())

# all_data %>%
#     # filter(!is.na(formato)) %>%
#     ggplot(aes(x = plan_prod, y = merma_prod_val_peso)) +
#         geom_point() +
#         geom_smooth(method = "loess") +
#         # geom_smooth()
#         # facet_wrap("formato", scales = "free_x") +
#         # theme(legend.position = "bottom") +
#         geom_hline(yintercept = 0, linetype = 2, colour = "red") +
#         xlab("Planned production (Tons)") + ylab("Production loss (Tons)") +
#         theme_bw()

## Production planning based on the model

filtered_data[85,] %>%
    sample_n(100, replace = TRUE) %>%
    mutate(real_peso = seq(10, 30, length = 100)) %>%
    mutate(prop_loss = predict(gradboost_model, newdata = .)) %>% 
    mutate(prop_loss_95 = prop_loss + my_sd,
           prop_loss_05 = prop_loss - my_sd) %>%
    mutate(actual_prod = real_peso * prop_loss,
           actual_prod_96 = real_peso * prop_loss_95,
           actual_prod_05 = real_peso * prop_loss_05) %>%
    ggplot() +
    geom_line(aes(real_peso, y = (1 - prop_loss)*real_peso)) +
    geom_line(aes(real_peso, y = (1 - prop_loss_95)*real_peso), linetype = 2) +
    geom_line(aes(real_peso, y = (1 - prop_loss_05)*real_peso), linetype = 2) +
    ylab("Predicted production lossess (kg)") +
    xlab("Input weight (Tn)")


filtered_data[985,] %>%
    sample_n(100, replace = TRUE) %>%
    mutate(real_peso = seq(10, 30, length = 100)) %>%
    mutate(prop_loss = predict(gradboost_model, newdata = .)) %>% 
    mutate(prop_loss_95 = prop_loss + my_sd,
           prop_loss_05 = prop_loss - my_sd) %>%
    mutate(actual_prod = real_peso * prop_loss,
           actual_prod_96 = real_peso * prop_loss_95,
           actual_prod_05 = real_peso * prop_loss_05) %>%
    ggplot() +
    geom_line(aes(real_peso, y = (1 - prop_loss)*real_peso)) +
    geom_line(aes(real_peso, y = (1 - prop_loss_95)*real_peso), linetype = 2) +
    geom_line(aes(real_peso, y = (1 - prop_loss_05)*real_peso), linetype = 2)

## Comparison between different conditions

p1 <- filtered_data[85,] %>%
    sample_n(100, replace = TRUE) %>%
    mutate(real_peso = seq(10, 30, length = 100)) %>%
    mutate(prop_loss = predict(gradboost_model, newdata = .)) %>%
    # ggplot() +
    geom_line(aes(real_peso, y = (1 - prop_loss)*real_peso), data = .)

p2 <- filtered_data[985,] %>%
    sample_n(100, replace = TRUE) %>%
    mutate(real_peso = seq(10, 30, length = 100)) %>%
    mutate(prop_loss = predict(gradboost_model, newdata = .)) %>%
    # ggplot() +
    geom_line(aes(real_peso, y = (1 - prop_loss)*real_peso), data = .)


ggplot() + p1 + p2

list(filtered_data[85,],
     # filtered_data[1091,]
     # filtered_data[985,],
     filtered_data[1301,]
     # filtered_data[1221,]
     ) %>%
    map(., 
        ~ sample_n(., 100, replace = TRUE)
    ) %>%
    map(.,
        ~ mutate(., real_peso = seq(10, 60, length = 100))
    ) %>%
    map(.,
        ~ mutate(., prop_loss = predict(gradboost_model, newdata = .)) 
        ) %>%
    imap_dfr(., ~ mutate(.x, condition = .y)) %>%
    mutate(prop_loss_up = prop_loss - my_sd,
           prop_loss_down = prop_loss + my_sd) %>%
    mutate(waste = (1-prop_loss)*real_peso,
           waste_up = (1-prop_loss_up)*real_peso,
           waste_down = (1-prop_loss_down)*real_peso) %>%
    ggplot() +
        geom_ribbon(aes(x = real_peso, ymin = waste_up, ymax = waste_down,
                        colour = factor(condition),
                        fill = factor(condition)), alpha = .2) +
        geom_line(aes(x = real_peso, y = waste, colour = factor(condition)),
                  linetype = 2) +
        # geom_line(aes(x = real_peso, y = (1-prop_loss)*real_peso, 
        #               colour = factor(condition),
        #               linetype = factor(condition)), size = 1) +
    # geom_line(aes(x = real_peso, y = (1-(prop_loss + my_sd))*real_peso, 
    #               colour = factor(condition),
    #               linetype = factor(condition)), size = 1) +
    cowplot::theme_cowplot() +
    xlab("Input weight (Tn)") + ylab("Predicted losses (Tn)") +
    theme(legend.position = "none")

list(filtered_data[85,],
     # filtered_data[1091,]
     # filtered_data[985,],
     filtered_data[1301,]
     # filtered_data[1221,]
) %>%
    map(., 
        ~ sample_n(., 100, replace = TRUE)
    ) %>%
    map(.,
        ~ mutate(., real_peso = seq(10, 60, length = 100))
    ) %>%
    map(.,
        ~ mutate(., prop_loss = predict(gradboost_model, newdata = .)) 
    ) %>%
    map(., 
        ~ mutate(., prop_loss_up = prop_loss - my_sd,
                 prop_loss_down = prop_loss + my_sd)
        ) %>%
    map(., 
        ~ mutate(., waste = (1-prop_loss)*real_peso,
                 waste_up = (1-prop_loss_up)*real_peso,
                 waste_down = (1-prop_loss_down)*real_peso) 
        ) %>%
    map(.,
        ~ ggplot(.) +
            geom_ribbon(aes(x = real_peso, ymin = waste_up, ymax = waste_down), alpha = .2) +
            geom_line(aes(x = real_peso, y = waste),
                      linetype = 2)  +
            ylim(-.1, 2.6) +
            xlab("Input weight (Tn)") + ylab("Predicted losses (Tn)") +
            cowplot::theme_cowplot()
        ) %>%
    cowplot::plot_grid(plotlist = ., labels = "AUTO")
    

## Prediction of one thingy

which(test_set$y < 0)

aa <- filtered_data[84,]
aa <- filtered_data[1168,]
aa <- filtered_data[1171,]
aa <- filtered_data[650,]
# aa <- filtered_data[421,]

# x <- sample(1:nrow(filtered_data), 1) 
# aa <- filtered_data[x,]

c(1:100) %>%
    map(., ~ aa) %>%
    do.call(bind_rows, .) %>%
    mutate(., real_peso = seq(0, .$real_peso[1]*8, length = 100)) %>%
    mutate(prop_loss = predict(gradboost_model, newdata = .)) %>%
    select(real_peso, prop_loss) %>%
    ggplot(aes(x = real_peso, y = prop_loss)) +
        geom_line() +
        geom_ribbon(aes(ymin = prop_loss - my_sd*1.96, ymax = 1), alpha = 0.5) +
        geom_ribbon(aes(ymin = prop_loss - my_sd*1.64, ymax = 1), alpha = 0.5) +
        geom_ribbon(aes(ymin = prop_loss - my_sd*2.58, ymax = 1), alpha = 0.5) +
        # geom_hline(aes(yintercept = 0), linetype = 2, colour = "red") +
        theme_minimal() +
        xlab("Planned production (Tn)") + ylab("Predicted losses (Kg)")
        # ylim(0, 1)

# filtered_data %>%
#     group_by(linea) %>%
#     summarize(n = n()) %>%
#     ggplot() + geom_histogram(aes(n)) + scale_x_log10()


## Check if it predicts that they are negative

# test_set %>%
#     mutate(pred_loss = predict(randforest_model, newdata = .)) %>%
#     mutate(lower90 = pred_loss - .31*1.64, 
#            lower95 = pred_loss - .31*1.96,
#            lower99 = pred_loss - .31*2.58) %>%
#     mutate(actual_neg = merma_prod_val_peso < 0,
#            neg_90 = lower90 < 0,
#            neg_95 = lower95 < 0,
#            neg_99 = lower99 < 0) %>%
#     select(actual_neg, neg_90, neg_95, neg_99) %>%
#     gather(prob, neg, -actual_neg) %>%
#     group_by(actual_neg, prob, neg) %>%
#     summarize(n()) %>%
#     arrange(prob, actual_neg)
# 
# train_set %>%
#     mutate(pred_loss = predict(randforest_model, newdata = .)) %>%
#     mutate(lower90 = pred_loss - .31*1.64, 
#            lower95 = pred_loss - .31*1.96,
#            lower99 = pred_loss - .31*2.58) %>%
#     mutate(actual_neg = merma_prod_val_peso < 0,
#            neg_90 = lower90 < 0,
#            neg_95 = lower95 < 0,
#            neg_99 = lower99 < 0) %>%
#     select(actual_neg, neg_90, neg_95, neg_99) %>%
#     gather(prob, neg, -actual_neg) %>%
#     group_by(actual_neg, prob, neg) %>%
#     summarize(n()) %>%
#     arrange(prob, actual_neg)













