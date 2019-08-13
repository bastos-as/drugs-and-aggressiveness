# These are the functions I developed to create plots for my dissertation

# Multiple scatterplot function (continuous x continuous)
multiple.scatterPlot <- function(df = MAIN, cont_vars, guide_names = character(),
                                 var2, x_lab = "Scores", y_lab = NULL, guides = F){
    if(!all(cont_vars %in% colnames(df))){stop("At least one variable is wrong!")}
    if(is.null(y_lab)){y_lab <- var2}
    if(length(guide_names) == 0){guide_names <- cont_vars}
    if(length(guide_names) != length(cont_vars)){
        stop(paste(length(cont_vars), "variables and", length(guide_names),
                   "names were given. They must be the same length."))
    }
    library(ggplot2)
    guide_names <- guide_names[order(cont_vars)]
    cont_vars <- sort(cont_vars)
    plot_colors <- c("blue", "green3", "yellow3", "red", "purple", "orange")
    
    # Melt df
    cont_df <- data.frame(var1 = NA, var2 = NA, stringsAsFactors = F)
    cont_df[1:(nrow(df)*length(cont_vars)), "var1"] <- unlist(df[, cont_vars])
    cont_df[1:(nrow(cont_df)), "var2"] <- rep(df[, var2], length(cont_vars))
    cont_df[1:nrow(cont_df), "colors"] <-
        rep(plot_colors, each = nrow(df), length.out = nrow(cont_df))
    
    cont_df[1:nrow(cont_df), "Variables"] <-
        rep(guide_names, each = nrow(df), length.out = nrow(cont_df))
    if(guides){ xplot <-
        ggplot(cont_df, aes(var2, var1)) +
        theme_bw() +
        labs(x = y_lab, y = x_lab) +
        geom_point(data = cont_df, aes(color = Variables)) +
        geom_point(color = cont_df[, "colors"]) +
        geom_smooth(data = cont_df, method = "lm", fullrange=T, se = F,
                    aes(color = Variables)) +
        scale_color_manual(values = plot_colors[1:length(cont_vars)])
    } else { xplot <-
        ggplot(cont_df, aes(var2, var1)) +
        theme_bw() +
        labs(x = y_lab, y = x_lab) +
        geom_point(color = cont_df[, "colors"]) +
        geom_smooth(data = cont_df, method = "lm", fullrange=T, se = F,
                    aes(color = Variables)) +
        scale_color_manual(values = plot_colors[1:length(cont_vars)]) +
        guides(color = F)
    }
    return(xplot)
}

# Plot with lines and whiskers, based on the mean of each variable
# (continuous x categorical)
linesWhiskers <- function(df = MAIN, cont_vars, guide_names = character(),
                          var2, categories, x_lab = "Items", y_lab = "Scores",
                          guides = F){
    if(!all(cont_vars %in% colnames(df))){stop("At least one variable is wrong!")}
    if(length(guide_names) == 0){guide_names <- cont_vars}
    if(length(guide_names) != length(cont_vars)){
        stop(paste(length(cont_vars), "variables and", length(guide_names),
                   "names were given. They must be the same length."))
    }
    if(!is.integer(var2) || length(var2) < 2){stop("'var2' must be an integer array!")}
    if(nrow(df) != length(var2)){stop("The arrays must be the same size!")}
    if(range(na.omit(var2))[2] > length(categories)){
        stop("Need more categories!")
    }
    library(ggplot2)
    plot_colors <- c("blue", "green3", "yellow3", "red", "purple", "orange")
    cor_df <- data.frame(means = NA, se = NA,
                         var2 = rep(1:length(categories), length(cont_vars)),
                         color1 =
                             rep(plot_colors, each = length(categories),
                                 length.out = length(categories) * length(cont_vars)),
                         color2 = rep(plot_colors[length(cont_vars):1],
                                      length(categories)))
    for(variable in cont_vars){
        for(i in 1:length(categories)){
            df[, variable] <- rank(df[, variable])
            cor_df[min(which(is.na(cor_df[, 'means']))), 'means'] <-
                mean(df[which(var2 == i), variable], na.rm=T)
            cor_df[min(which(is.na(cor_df[, 'se']))), 'se'] <-
                sd(df[which(var2 == i), variable], na.rm=T) /
                length(grep(i, var2))
        }
    }
    cor_df[, "Variables"] <- rep(cont_vars, each = length(categories))
    pd <- position_dodge(0.1)
    if(guides){ xplot <- 
        ggplot(data = cor_df, aes(var2, means, color = Variables, group = Variables)) +
        theme_bw() +
        geom_line(position=pd, color = cor_df[,"color2"]) +
        geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.1, position=pd) +
        geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.4,
                      color=cor_df[,"color2"], position=pd) +
        scale_x_continuous(x_lab, breaks = 1:length(categories),
                           labels = categories) +
        scale_y_continuous(y_lab) +
        scale_colour_discrete("Variables") +
        guides(color = guide_legend(override.aes = list(
            color = plot_colors[1:length(cont_vars)])))
    } else { xplot <- 
        ggplot(data = cor_df, aes(var2, means, color = Variables, group = Variables)) +
        theme_bw() +
        geom_line(position=pd, color = cor_df[,"color2"]) +
        geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.4,
                      color=cor_df[,"color2"], position=pd) +
        scale_x_continuous(x_lab, breaks = 1:length(categories),
                           labels = categories) +
        scale_y_continuous(y_lab) +
        scale_colour_discrete("Variables")
    }
    return(xplot)
}
