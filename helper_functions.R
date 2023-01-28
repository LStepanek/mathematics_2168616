###############################################################################
###############################################################################
###############################################################################

## definuji pomocné funkce ----------------------------------------------------

getMyAverage <- function(
    
    x,
    alpha = 0.05,
    x_is_lognormal = FALSE,
    classical_format = FALSE,
    multiplier = 1,
    digits = 2,
    scientific_notation = FALSE,
    na.rm = TRUE
    
){
    
    # '''
    # Vrací pro numerický vektor "x" buďto:
    #
    # (i) jeho průměr a směrodatnou odchylku ve tvaru
    #
    #     "průměr ± násobek * směrodatná odchylka",
    #
    # je-li parametr "classical_format" = TRUE, kde násobek
    # odpovídá hodnotě parametru "multiplier", anebo
    #
    # (ii) 100 * (1 - "alpha" / 2) %-ní konfidenční interval
    # jeho průměru v závorce; před závorkou pak bodový odhad
    # průměru, je-li parametr "classical_format" = FALSE.
    # Sleduje-li vektor "x" lognormální nebo jiné zprava zešikmené
    # rozdělení, je třeba volit "x_is_lognormal" = TRUE.
    # V případě "classical_format" = TRUE je parametr "x_is_lognormal"
    # ignorován.
    #
    # Čísla jsou zaokrouhlena na počet desetinných míst udaných
    # argumentem "digits"; defaultně není použit vědecký zápis
    # ("scientific_notation" = FALSE) a jsou ignorovány chybějící
    # hodnoty ("na.rm" = TRUE).
    # '''
    
    if(classical_format){
        
        return(
            paste(
                format(
                    round(
                        mean(x, na.rm = na.rm),
                        digits = digits
                    ),
                    nsmall = digits,
                    digits = digits,
                    scientific = scientific_notation,
                    trim = TRUE            
                ),
                " ± ",
                format(
                    round(
                        multiplier * sd(x, na.rm = na.rm),
                        digits = digits
                    ),
                    nsmall = digits,
                    digits = digits,
                    scientific = scientific_notation,
                    trim = TRUE            
                ),
                sep = ""
            )
        )
        
    }else{
        
        if(x_is_lognormal){
            
            log_x_wo_Inf <- suppressWarnings(
                log(x)[log(x) != -Inf]
            )
            
            return(
                paste(
                    format(
                        round(
                            exp(mean(log_x_wo_Inf, na.rm = na.rm)),
                            digits = digits
                        ),
                        nsmall = digits,
                        digits = digits,
                        scientific = scientific_notation,
                        trim = TRUE            
                    ),
                    " (",
                    format(
                        round(
                            exp(
                                mean(log_x_wo_Inf, na.rm = na.rm) -
                                qnorm(1 - alpha / 2) *
                                sd(log_x_wo_Inf, na.rm = na.rm) /
                                sqrt(length(na.omit(log_x_wo_Inf)))
                            ),
                            digits = digits
                        ),
                        nsmall = digits,
                        digits = digits,
                        scientific = scientific_notation,
                        trim = TRUE            
                    ),
                    "; ",
                    format(
                        round(
                            exp(
                                mean(log_x_wo_Inf, na.rm = na.rm) +
                                qnorm(1 - alpha / 2) *
                                sd(log_x_wo_Inf, na.rm = na.rm) /
                                sqrt(length(na.omit(log_x_wo_Inf)))
                            ),
                            digits = digits
                        ),
                        nsmall = digits,
                        digits = digits,
                        scientific = scientific_notation,
                        trim = TRUE            
                    ),
                    ")",
                    sep = ""
                )
            )
            
        }else{
            
            return(
                paste(
                    format(
                        round(
                            mean(x, na.rm = na.rm),
                            digits = digits
                        ),
                        nsmall = digits,
                        digits = digits,
                        scientific = scientific_notation,
                        trim = TRUE            
                    ),
                    " (",
                    format(
                        round(
                            (
                                mean(x, na.rm = na.rm) -
                                qnorm(1 - alpha / 2) *
                                sd(x, na.rm = na.rm) /
                                sqrt(length(na.omit(x)))
                            ),
                            digits = digits
                        ),
                        nsmall = digits,
                        digits = digits,
                        scientific = scientific_notation,
                        trim = TRUE            
                    ),
                    "; ",
                    format(
                        round(
                            (
                                mean(x, na.rm = na.rm) +
                                qnorm(1 - alpha / 2) *
                                sd(x, na.rm = na.rm) /
                                sqrt(length(na.omit(x)))
                            ),
                            digits = digits
                        ),
                        nsmall = digits,
                        digits = digits,
                        scientific = scientific_notation,
                        trim = TRUE            
                    ),
                    ")",
                    sep = ""
                )
            )
            
        }
        
    }
    
}


## ----------------------------------------------------------------------------

getMyPrintableLinearModel <- function(
    
    my_summary_of_lm,
    which_to_omit = NULL,
    digits = rep(
        3,
        dim(
            as.data.frame(my_summary_of_lm$coefficients)[,
                setdiff(
                    colnames(as.data.frame(my_summary_of_lm$coefficients)),
                    which_to_omit                     
                )
            ]
        )[2]
    ),
    scientific_notation = FALSE,
    decimal_separator = ".",
    category_separator = " = "
    
){
    
    # '''
    # Vrací tisknutelný výstup sumáře lineárního modelu;
    # rovněž ve výstupu vynechává parametry uvedené v "which_to_omit",
    # digits je vektor délky počtu sloupců výsledného výstupu,
    # který každému sloupci určuje počet cifer, na které budou
    # hodnoty v něm zaokrouhleny.
    # '''
    
    output <- as.data.frame(my_summary_of_lm$coefficients)[,
            setdiff(
                colnames(
                    as.data.frame(my_summary_of_lm$coefficients)
                ),
                which_to_omit                     
            )
        ]
    
    for(i in 1:dim(output)[2]){
        output[, i] <- format(
            round(
                as.data.frame(my_summary_of_lm$coefficients)[,
                    setdiff(
                        colnames(
                            as.data.frame(
                                my_summary_of_lm$coefficients
                            )
                        ),
                        which_to_omit                     
                    )
                ][, i],
                digits = digits[i]
            ),
            nsmall = digits[i],
            digits = digits[i],
            scientific = scientific_notation,
            trim = TRUE
        )
    }
    
    for(i in 1:length(rownames(output))){
        
        for(
            var_label in sort(
                attributes(my_summary_of_lm$terms)$term.labels,
                decreasing = TRUE
            )
        ){
            
            if(
                grepl(
                    paste("^", var_label, sep = ""),
                    rownames(output)[i]
                ) &
                rownames(output)[i] != var_label &
                ! grepl(
                    category_separator,
                    rownames(output)[i]
                )
            ){
                
                rownames(output)[i] <- paste(
                    var_label,
                    category_separator,
                    gsub(
                        paste("^", var_label, sep = ""),
                        "",
                        rownames(output)[i]
                    ),
                    sep = ""
                )
                
            }
            
        }
        
    }
    
    for(i in 1:dim(output)[2]){
        
        output[, i] <- gsub(
            "\\.",
            decimal_separator,
            output[, i]
        )
        
    }
    
    return(output)
    
}


## ----------------------------------------------------------------------------

getMyAccuracy <- function(
    
    my_table
    
){
    
    # '''
    # Vrací přesnost pro konfuzní matici "my_table".
    # '''
    
    return(
        sum(diag(my_table)) / sum(my_table)
    )
    
}


## ----------------------------------------------------------------------------

getMyPrecision <- function(
    
    my_table
    
){
    
    # '''
    # Vrací preciznost pro konfuzní matici "my_table", kde
    # v prvním řádku jsou skutečné nehativní hodnoty,
    # ve druhém řádku jsou skutečné pozitivní hodnoty,
    # v prvním sloupci jsou predikované negativní hodnoty
    # a ve druhém sloupci jsou predikované pozitivní hodnoty.
    # '''
    
    return(
        my_table[2, 2] / sum(my_table[, 2])
    )
    
}


## ----------------------------------------------------------------------------

getMyRecall <- function(
    
    my_table
    
){
    
    # '''
    # Vrací recall pro konfuzní matici "my_table", kde
    # v prvním řádku jsou skutečné nehativní hodnoty,
    # ve druhém řádku jsou skutečné pozitivní hodnoty,
    # v prvním sloupci jsou predikované negativní hodnoty
    # a ve druhém sloupci jsou predikované pozitivní hodnoty.
    # '''
    
    return(
        my_table[2, 2] / sum(my_table[2, ])
    )
    
}


## ----------------------------------------------------------------------------

getMyF1Score <- function(
    
    my_table
    
){
    
    # '''
    # Vrací F1 skóre pro konfuzní matici "my_table", kde
    # v prvním řádku jsou skutečné nehativní hodnoty,
    # ve druhém řádku jsou skutečné pozitivní hodnoty,
    # v prvním sloupci jsou predikované negativní hodnoty
    # a ve druhém sloupci jsou predikované pozitivní hodnoty.
    # '''
    
    return(
        2 * (
            my_table[2, 2] / sum(my_table[, 2])
        ) * (
            my_table[2, 2] / sum(my_table[2, ])
        ) / (
            (
                my_table[2, 2] / sum(my_table[, 2])
            ) + (
                my_table[2, 2] / sum(my_table[2, ])
            )
        )
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





