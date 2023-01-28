###############################################################################
###############################################################################
###############################################################################

## učím binární logistickou regresi -------------------------------------------

#### rekóduji hodnoty protilátek na 0 = negativní a 1 = pozitivní -------------

my_data[, "IgG"] <- ifelse(
    
    my_data[, "IgG"] == "pozitivni",
    1,
    0
    
)

my_data[, "IgM"] <- ifelse(
    
    my_data[, "IgM"] == "pozitivni",
    1,
    0
    
)


setwd(
    paste(
        mother_working_directory,
        "vystupy",
        sep = "/"
    )
)

if(
    file.exists("logit_regression.txt")
){
    invisible(
        file.remove("logit_regression.txt")
    )
}

for(my_antibodies in c(
    
    "IgG",
    "IgM"
    
)){
    
    sink(
        file = "logit_regression.txt",
        append = TRUE,
        type = "output"
    )
    
    cat(
        paste(
            "####################",
            "####################",
            "####################",
            "####################\n",
            "####################",
            "####################",
            "####################",
            "####################\n",
            "####################",
            "####################",
            "####################",
            "####################\n",
            sep = ""
        )
    )
    
    #cat("\n")
    
    cat("\n")
    cat(
        paste(
            "#### Výpočty nad ",
            my_antibodies,
            " protilátkami. ####",
            sep = ""
        )
    )
    cat("\n")    
    cat("\n")
    cat("\n")
    
    sink()
    
    my_accuracies <- NULL
    my_precisions <- NULL
    my_recalls <- NULL
    my_f1_scores <- NULL
    my_confusion_matrices_list <- list()
    
    for(i in 1:k){
        
        #### dělím data do trénovací množiny obsahující (k - 1)/k všech dat
        #### a testovací množiny obsahující vždy jednu k-tinu dat -------------
        
        set.seed(i)
        test_set_indices <- c(
            (
                (i - 1) * floor(dim(my_data)[1] / k) + 1
            ):(
                i * floor(dim(my_data)[1] / k)
            )
        )
        
        train_set <- my_data[-test_set_indices, ]
        test_set <- my_data[test_set_indices, ]
        
        
        #### spedicifikuji model ----------------------------------------------
        
        my_model <- glm(
            
            formula = paste(
                my_antibodies,
                " ~ ",
                paste(
                    "vek",
                    "pohlavi",
                    "BMI",
                    "vaha",
                    "vyska",
                    "bolest_hlavy",
                    "bolest_v_krku",
                    "bolest_ci_tlak_na_hrudi",
                    "bolest_svalu_ci_tela",
                    "dusnost",
                    "horecka",
                    "subfebrilie",
                    "kasel",
                    "nechutenstvi",
                    "prujem",
                    "ryma",
                    "unava",
                    "vyrazka",
                    "ztrata_chuti",
                    "ztrata_cichu",
                    "zvraceni",
                    "psychicke_potize",
                    "nespavost",
                    "pocet_pritomnych_symptomu",
                    "intenzita_symptomu",
                    "delka_trvani_symptomu",
                    "interval_mezi_odberem_protilatek_a_prukazem_covid",
                    sep = " + "
                ),
                sep = ""
            ),
            data = train_set,
            family = binomial(link = "logit")
            
        )
        
        
        ## predikce -----------------------------------------------------------
        
        my_predictions <- predict(
            object = my_model,
            newdata = test_set,
            type = "response"
        )
        
        
        ## matice záměn -------------------------------------------------------
        
        sink(
            file = "logit_regression.txt",
            append = TRUE,
            type = "output"
        )
        
        cat(
            paste(
                "## -----------------",
                "--------------------",
                "--------------------",
                "--------------------\n",
                sep = ""
            )
        )
        
        cat("\n")
        
        cat(
            paste(
                "k = ",
                i,
                sep = ""
            )
        )
        
        cat("\n")
        cat("\n")
        
        my_confusion_table <- table(
            factor(
                ifelse(
                    test_set[, my_antibodies] == 1,
                    paste(
                        my_antibodies,
                        " pozitivni",
                        sep = ""
                    ),
                    paste(
                        my_antibodies,
                        " negativni",
                        sep = ""
                    )
                ),
                levels = c(
                    paste(
                        my_antibodies,
                        " negativni",
                        sep = ""
                    ),
                    paste(
                        my_antibodies,
                        " pozitivni",
                        sep = ""
                    )
                )
            ),
            factor(
                ifelse(
                    my_predictions > 0.5,
                    paste(
                        my_antibodies,
                        " pozitivni",
                        sep = ""
                    ),
                    paste(
                        my_antibodies,
                        " negativni",
                        sep = ""
                    )
                ),
                levels = c(
                    paste(
                        my_antibodies,
                        " negativni",
                        sep = ""
                    ),
                    paste(
                        my_antibodies,
                        " pozitivni",
                        sep = ""
                    )
                )
            ),
            dnn = list(
                "pozorovane hodnoty",
                "predikovane hodnoty"
            )
        )
        
        print(
            my_confusion_table
        )
        
        cat("\n")
        cat(
            "predikční přesnost: "
        )
        cat(
            format(
                round(
                    getMyAccuracy(
                        my_confusion_table
                    ),
                    digits = 3
                ),
                nsmall = 3
            )
        )
        
        cat("\n")
        cat(
            "preciznost: "
        )
        cat(
            format(
                round(
                    getMyPrecision(
                        my_confusion_table
                    ),
                    digits = 3
                ),
                nsmall = 3
            )
        )
        
        cat("\n")
        cat(
            "recall: "
        )
        cat(
            format(
                round(
                    getMyRecall(
                        my_confusion_table
                    ),
                    digits = 3
                ),
                nsmall = 3
            )
        )
        
        cat("\n")
        cat(
            "F1 skóre: "
        )
        cat(
            format(
                round(
                    getMyF1Score(
                        my_confusion_table
                    ),
                    digits = 3
                ),
                nsmall = 3
            )
        )
        
        cat("\n")
        cat("\n")
        cat("\n")
        
        sink()
        
        
        my_confusion_matrices_list[[
            length(my_confusion_matrices_list) + 1
        ]] <- my_confusion_table
        
        
        ## predikční přesnost -------------------------------------------------
        
        my_accuracies <- c(
            
            my_accuracies,
            getMyAccuracy(
                my_confusion_table
            )
            
        )
        
        my_precisions <- c(
            
            my_precisions,
            getMyPrecision(
                my_confusion_table
            )
            
        )
        
        my_recalls <- c(
            
            my_recalls,
            getMyRecall(
                my_confusion_table
            )
            
        )
        
        my_f1_scores <- c(
            
            my_f1_scores,
            getMyF1Score(
                my_confusion_table
            )
            
        )
        
        
        ## logovací hlášky ----------------------------------------------------
        
        flush.console()
        cat(
            paste(
                my_antibodies,
                " | ",
                " provedena ",
                i,
                "-tá iterace ",
                k,
                "-foldové křížové validace",
                sep = ""
            )
        )
        cat("\n")
        #cat("\n")
        
        
        ## --------------------------------------------------------------------
        
    }
    
    cat(
        paste(
            "## -----------------",
            "--------------------",
            "--------------------",
            "--------------------\n",
            sep = ""
        )
    )
    cat("\n")
    
    
    ## předikční přesnosti ----------------------------------------------------
    
    # my_accuracies
    
    assign(
        paste(
            "logit_regression_",
            my_antibodies,
            "_accuracy",
            sep = ""
        ),
        mean(my_accuracies)
    )
    
    assign(
        paste(
            "logit_regression_",
            my_antibodies,
            "_precision",
            sep = ""
        ),
        mean(my_precisions)
    )
    
    assign(
        paste(
            "logit_regression_",
            my_antibodies,
            "_recall",
            sep = ""
        ),
        mean(my_recalls)
    )
    
    assign(
        paste(
            "logit_regression_",
            my_antibodies,
            "_f1_score",
            sep = ""
        ),
        mean(my_f1_scores)
    )
    
    
    ## mediánová matize záměn -------------------------------------------------
    
    my_confusion_matrix <- my_confusion_matrices_list[[1]]
    
    for(i in 1:dim(my_confusion_matrix)[1]){
        
        for(j in 1:dim(my_confusion_matrix)[2]){
            
            my_confusion_matrix[i, j] <- 0
            
        }
        
    }
    
    for(i in 1:dim(my_confusion_matrix)[1]){
        
        for(j in 1:dim(my_confusion_matrix)[2]){
            
            my_row <- NULL
            
            for(l in 1:length(my_confusion_matrices_list)){
                
                my_row <- c(
                    my_row,
                    my_confusion_matrices_list[[l]][i, j]
                )
                
            }
            
            my_confusion_matrix[i, j] <- median(my_row)
            
        }
        
    }
    
    
    ## ukládám mediánovou matici záměn ----------------------------------------
    
    sink(
        file = "logit_regression.txt",
        append = TRUE,
        type = "output"
    )
    
    cat(
        paste(
            "## -----------------",
            "--------------------",
            "--------------------",
            "--------------------\n",
            sep = ""
        )
    )
    
    cat("\n")
    cat(
        paste(
            "MEDIÁNOVÁ MATICE ZÁMĚN (nad k = 1 až k = ",
            k,            
            ")",
            sep = ""
        )
    )
    
    cat("\n")
    cat("\n")
    
    print(
        my_confusion_matrix
    )
    
    cat("\n")
    cat("\n")
    cat("\n")
    
    sink()
    
    
    ## vytvářím objekt s konfutní maticí --------------------------------------
    
    assign(
        paste(
            "logit_regression_",
            my_antibodies,
            "_median_confusion_table",
            sep = ""
        ),
        my_confusion_matrix
    )
    
    
    ## ------------------------------------------------------------------------
    
}

setwd(
    mother_working_directory
)


#### rekóduji hodnoty protilátek zpět na negativní = 0 a pozitivní = 1 --------

my_data[, "IgG"] <- ifelse(
    
    my_data[, "IgG"] == 1,
    "pozitivni",
    "negativni"
    
)

my_data[, "IgM"] <- ifelse(
    
    my_data[, "IgM"] == 1,
    "pozitivni",
    "negativni"
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





