###############################################################################
###############################################################################
###############################################################################

## vytvářím tabulku s predikčními přesnostmi ----------------------------------

closeAllConnections()

for(
    my_antibodies in c(
        
        "IgG",
        "IgM"
        
    )
){
    
    my_table <- NULL
    
    for(
        my_method in c(
            
            "survival_regression",
            "logit_regression",
            "naive_bayes",
            "support_vector_machine",
            "decision_trees",
            "random_forests",
            "neural_networks"
            
        )
    ){
        
        my_row <- NULL
        
        for(
            my_metric in c(
                
                "accuracy",
                "precision",
                "recall",
                "f1_score"
                
            )
        ){
            
            my_row <- c(
                
                my_row,
                get(
                    paste(
                        my_method,
                        my_antibodies,
                        my_metric,
                        sep = "_"
                    )
                )
                
            )
            
            names(my_row)[length(my_row)] <- my_metric
            
        }
        
        my_table <- rbind(
            
            my_table,
            my_row
            
        )
        
        rownames(my_table)[dim(my_table)[1]] <- gsub("_", " ", my_method)
        
        
        if(
            my_method == "survival_regression"
        ){
            
            
            cat(
                my_antibodies
            )
            cat(" survival threshold: ")
            cat(
                get(
                    paste(
                        my_method,
                        my_antibodies,
                        "threshold",
                        sep = "_"
                    )
                )
            )
            cat("\n")
            
        }
        
    }
    
    
    assign(
        x = paste(
            my_antibodies,
            "my_table",
            sep = "_"
        ),
        value = my_table
    )
    
}

closeAllConnections()


## ukládám tabulku ------------------------------------------------------------

setwd(
    paste(
        mother_working_directory,
        "vystupy",
        sep = "/"
    )
)

if(
    file.exists("accuracy_table.txt")
){
    invisible(
        file.remove("accuracy_table.txt")
    )
}

for(
    my_antibodies in c(
        
        "IgG",
        "IgM"
        
    )
){
    
    sink(
        file = "accuracy_table.txt",
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
    
    cat("\n")
    cat(
        paste(
            "## průměrné predikční metriky pro daný algoritmus ",
            "a predikované protilátky ",
            my_antibodies,
            " ##",
            sep = ""
        )
    )
    cat("\n")    
    cat("\n")
    cat("\n")
    
    print(
        format(
            round(
                get(
                    paste(
                        my_antibodies,
                        "my_table",
                        sep = "_"
                    )
                ),
                digits = 3
            ),
            nsmall = 3
        ),
        quote = FALSE
    )
    
    cat("\n")
    cat("\n")
    
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
    
    cat("\n")
    cat("\n")
    
    sink()
    
}

setwd(
    mother_working_directory
)

closeAllConnections()


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





