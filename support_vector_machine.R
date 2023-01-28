###############################################################################
###############################################################################
###############################################################################

## učím podpůrné vektory ------------------------------------------------------

setwd(
    paste(
        mother_working_directory,
        "vystupy",
        sep = "/"
    )
)

if(
    file.exists("support_vector_machine.txt")
){
    invisible(
        file.remove("support_vector_machine.txt")
    )
}

for(my_antibodies in c(
    
    "IgG",
    "IgM"
    
)){
    
    sink(
        file = "support_vector_machine.txt",
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
        
        train_set[, my_antibodies] <- as.factor(train_set[, my_antibodies])
        test_set[, my_antibodies] <- as.factor(test_set[, my_antibodies])
        
        train_set <- train_set[
            complete.cases(train_set)
            ,
        ]
        test_set <- test_set[
            complete.cases(test_set)
            ,
        ]
        
        #### spedicifikuji model ----------------------------------------------
        
        my_model <- e1071::svm(
            
            x = train_set[
                ,
                c(
                    "vek",
                    "pohlavi",
                    "BMI",
                    "vek",
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
                    "zvraceni", # je problém, že obsahuje pouze hodnoty "ne"
                    "psychicke_potize",
                    "nespavost",
                    "pocet_pritomnych_symptomu",
                    "intenzita_symptomu",
                    "delka_trvani_symptomu",
                    "interval_mezi_odberem_protilatek_a_prukazem_covid"
                )
            ],
            y = train_set[, my_antibodies],
            type = "C-classification"
            
        )
        
        
        ## predikce -----------------------------------------------------------
        
        my_predictions <- suppressWarnings(
            predict(
                object = my_model,
                newdata = test_set[
                    ,
                    c(
                        "vek",
                        "pohlavi",
                        "BMI",
                        "vek",
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
                        "zvraceni", # je problém, že obsahuje pouze hodnoty "ne"
                        "psychicke_potize",
                        "nespavost",
                        "pocet_pritomnych_symptomu",
                        "intenzita_symptomu",
                        "delka_trvani_symptomu",
                        "interval_mezi_odberem_protilatek_a_prukazem_covid"
                    )
                ]
            )
        )
        
        
        ## matice záměn -------------------------------------------------------
        
        sink(
            file = "support_vector_machine.txt",
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
            test_set[, my_antibodies][
                complete.cases(
                    test_set
                )
            ],
            my_predictions,
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
    
    # (
        # multilogit_regression_accuracy <- mean(my_accuracies)
    # )
    
    assign(
        paste(
            "support_vector_machine_",
            my_antibodies,
            "_accuracy",
            sep = ""
        ),
        mean(my_accuracies)
    )
    
    assign(
        paste(
            "support_vector_machine_",
            my_antibodies,
            "_precision",
            sep = ""
        ),
        mean(my_precisions)
    )
    
    assign(
        paste(
            "support_vector_machine_",
            my_antibodies,
            "_recall",
            sep = ""
        ),
        mean(my_recalls)
    )
    
    assign(
        paste(
            "support_vector_machine_",
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
        file = "support_vector_machine.txt",
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
            "support_vector_machine_",
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


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





