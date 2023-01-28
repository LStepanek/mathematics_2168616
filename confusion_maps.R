###############################################################################
###############################################################################
###############################################################################

## vykresluji mediánové konfuzní matice jako heat mapy ------------------------

setwd(
    paste(
        mother_working_directory,
        "vystupy",
        sep = "/"
    )
)

for(
    my_antibodies in c(
        
        "IgG",
        "IgM"
        
    )
){
    
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
        
        
        my_table <- get(
            paste(
                my_method,
                "_",
                my_antibodies,
                "_",
                "median_confusion_table",
                sep = ""
            )
        )
        
        cairo_ps(
            file = paste(
                my_method,
                "_",
                my_antibodies,
                "_",
                "median_confusion_table",
                ".eps",
                sep = ""
            ),
            width = 5.0,
            height = 5.0,
            pointsize = 16
        )
        
        par(mar = c(0.1, 6.5, 6.5, 0.1), xpd = TRUE)
        
        plot(
            0,
            xlim = c(0, 2),
            ylim = c(0, 2),
            type = "n",
            axes = FALSE,
            ann = FALSE
        )
        
        rect(
            xleft = 0,
            ybottom = 1,
            xright = 1,
            ytop = 2,
            col = grey(
                0.9 - 0.5 * my_table[1, 1] / sum(my_table)
            ),
            border = NA
        )
        
        text(
            x = 0.5,
            y = 1.5,
            labels = format(
                round(
                    my_table[1, 1],
                    digits = 1
                ),
                nsmall = 1
            )
        )
        
        rect(
            xleft = 1,
            ybottom = 1,
            xright = 2,
            ytop = 2,
            col = grey(
                0.9 - 0.5 * my_table[1, 2] / sum(my_table)
            ),
            border = NA
        )
        
        text(
            x = 1.5,
            y = 1.5,
            labels = format(
                round(
                    my_table[1, 2],
                    digits = 1
                ),
                nsmall = 1
            )
        )
        
        rect(
            xleft = 0,
            ybottom = 0,
            xright = 1,
            ytop = 1,
            col = grey(
                0.9 - 0.5 * my_table[2, 1] / sum(my_table)
            ),
            border = NA
        )
        
        text(
            x = 0.5,
            y = 0.5,
            labels = format(
                round(
                    my_table[2, 1],
                    digits = 1
                ),
                nsmall = 1
            )
        )
        
        rect(
            xleft = 1,
            ybottom = 0,
            xright = 2,
            ytop = 1,
            col = grey(
                0.9 - 0.5 * my_table[2, 2] / sum(my_table)
            ),
            border = NA
        )
        
        text(
            x = 1.5,
            y = 0.5,
            labels = format(
                round(
                    my_table[2, 2],
                    digits = 1
                ),
                nsmall = 1
            )
        )
        
        text(
            x = -0.6,
            y = 0.9,
            labels = "true class",
            adj = 0.5,
            pos = 3,
            srt = 90,
            cex = 1.0
        )
        
        text(
            x = -0.1,
            y = 1.4,
            labels = "decrease",
            adj = 0.5,
            pos = 3,
            srt = 90,
        )
        
        text(
            x = -0.1,
            y = 0.4,
            labels = "non-decrease",
            adj = 0.5,
            pos = 3,
            srt = 90,
        )
        
        text(
            x = 1.0,
            y = 2.3,
            labels = "predicted class",
            adj = 0.5,
            pos = 3,
            cex = 1.0
        )
        
        text(
            x = 0.5,
            y = 2.03,
            labels = "decrease",
            adj = 0.5,
            pos = 3
        )
        
        text(
            x = 1.5,
            y = 2.03,
            labels = "non-decrease",
            adj = 0.5,
            pos = 3
        )
        
        text(
            x = -0.8,
            y = 2.7,
            labels = switch(
                my_method,
                "survival_regression" = {"Cox proportional hazard model"},
                "logit_regression" = {"multivariate logistic regression"},
                "naive_bayes" = {"naïve Bayes classifier"},
                "support_vector_machine" = {"support vector machines"},
                "decision_trees" = {"decision trees"},
                "random_forests" = {"random forests"},
                "neural_networks" = {"artificial neural networks"}
            ),
            adj = 0.5,
            pos = 4,
            cex = 1.1
        )
        
        dev.off()
        
    }
    
}

setwd(
    mother_working_directory
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





