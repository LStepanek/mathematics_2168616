###############################################################################
###############################################################################
###############################################################################

## učím neuronové sítě --------------------------------------------------------

setwd(
    paste(
        mother_working_directory,
        "vystupy",
        sep = "/"
    )
)


#### standarziduji všechny numerické proměnné na interval <0, 1> --------------

scaled_data <- my_data

for(
    my_variable in colnames(scaled_data)
){
    
    if(
        class(scaled_data[, my_variable]) == "numeric"
    ){
        
        scaled_data[, my_variable] <- (
            scaled_data[, my_variable] - min(
                scaled_data[, my_variable],
                na.rm = TRUE
            )
        ) / (
            max(
                scaled_data[, my_variable],
                na.rm = TRUE
            ) - min(
                scaled_data[, my_variable],
                na.rm = TRUE
            )
        )
        
    }
    
}


if(
    file.exists("neural_networks.txt")
){
    invisible(
        file.remove("neural_networks.txt")
    )
}

for(my_antibodies in c(
    
    "IgG",
    "IgM"
    
)){
    
    #### kateogrické hodnoty targetové proměnné kóduji
    #### na dummy 0 vs. 1 hodnoty ---------------------------------------------
    
    temp_data <- scaled_data
    
    temp_data <- cbind(
        
        temp_data,
        nnet::class.ind(temp_data[, my_antibodies])
        
    )
    
    temp_data <- temp_data[
        ,
        -which(colnames(temp_data) == my_antibodies)
    ]
    
    
    sink(
        file = "neural_networks.txt",
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
                (i - 1) * floor(dim(temp_data)[1] / k) + 1
            ):(
                i * floor(dim(temp_data)[1] / k)
            )
        )
        
        train_set <- temp_data[-test_set_indices, ]
        test_set <- temp_data[test_set_indices, ]
        
        model_matrix <- model.matrix(
            
            eval(
                parse(
                    text = paste(
                        " ~ ",
                        paste(
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
                            ),
                            collapse = " + "
                        ),
                        " + negativni + pozitivni",
                        sep = ""
                    )
                )
            ),
            data = train_set
            
        )
        
        
        #### spedicifikuji model ----------------------------------------------
        
        my_model <- neuralnet::neuralnet(
            
            formula = eval(
                parse(
                    text = paste(
                        "negativni + pozitivni",
                        " ~ ",
                        paste(
                            setdiff(
                                colnames(
                                    model_matrix,
                                ),
                                c(
                                    "(Intercept)",
                                    "negativni",
                                    "pozitivni"
                                )
                            ),
                            collapse = " + "
                        ),
                        sep = ""
                    )
                )
            ),
            data = model_matrix,
            hidden = c(10, 5),# c(10, 10) # c(20, 15), #c(15, 10), # 5
            lifesign = "minimal",
            linear.output = FALSE,
            threshold = 0.05,
            learningrate.factor = c(
                minus = 1.10, #1.05, # 1.10
                plus = 1.30 #1.20   # 1.30
            ),
            algorithm = "rprop+"
            
        )
        
        
        ## predikce -----------------------------------------------------------
        
        model_matrix <- model.matrix(
            
            eval(
                parse(
                    text = paste(
                        " ~ ",
                        paste(
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
                            ),
                            collapse = " + "
                        ),
                        " + negativni + pozitivni",
                        sep = ""
                    )
                )
            ),
            data = test_set
            
        )
        
        my_predictions <- compute(
            my_model,
            model_matrix[
                ,
                setdiff(
                    colnames(model_matrix),
                    c(
                        "(Intercept)",
                        "negativni",
                        "pozitivni"
                    )
                )
            ]
        )[["net.result"]]
        
        
        my_predictions <- factor(
            unlist(
                
                lapply(
                    1:length(max.col(my_predictions)),
                    function(i){
                        
                        c(
                            "negativni",
                            "pozitivni"
                        )[
                            max.col(my_predictions)[i]
                        ]
                        
                    }
                )
                
            ),
            levels = c(
                "negativni",
                "pozitivni"
            )
        )
        
        my_observations <- factor(
            unlist(
                
                lapply(
                    1:dim(model_matrix)[1],
                    function(i){
                        
                        c(
                            "negativni",
                            "pozitivni"
                        )[
                            which(
                                model_matrix[
                                    i,
                                    c(
                                        "negativni",
                                        "pozitivni"
                                    )
                                ] == 1
                            )
                        ]
                        
                    }
                )
                
            ),
            levels = c(
                "negativni",
                "pozitivni"
            )
        )
        
        
        ## matice záměn -------------------------------------------------------
        
        sink(
            file = "neural_networks.txt",
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
            my_observations,
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
            "neural_networks_",
            my_antibodies,
            "_accuracy",
            sep = ""
        ),
        mean(my_accuracies, na.rm = TRUE)
    )
    
    assign(
        paste(
            "neural_networks_",
            my_antibodies,
            "_precision",
            sep = ""
        ),
        mean(my_precisions, na.rm = TRUE)
    )
    
    assign(
        paste(
            "neural_networks_",
            my_antibodies,
            "_recall",
            sep = ""
        ),
        mean(my_recalls, na.rm = TRUE)
    )
    
    assign(
        paste(
            "neural_networks_",
            my_antibodies,
            "_f1_score",
            sep = ""
        ),
        mean(my_f1_scores, na.rm = TRUE)
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
        file = "neural_networks.txt",
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
    
    
    ## vytvářím objekt s konfuzní maticí --------------------------------------
    
    assign(
        paste(
            "neural_networks_",
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





