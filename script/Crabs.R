# sp : Blue or Orange
# sex ...
# index : within groups
# FL : frontal lobe
# RW : rear width
# CL : carapace length
# CW : carapace width
# BD : body depth
library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]

plot_crabs <- function()
{
    library(grid)
    library(gridExtra)
    library(reshape2)
    library(ggplot2)
    cormat_original <- cor(crabsquant)
    melted_cormat_o <- melt(cormat_original)
    p1 <- ggplot(data = melted_cormat_o, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + ggtitle("matrice de correlation")
    # Plotting FL and RW as they are correlated to see if we can identify a difference between sex and specie
    p2 <- ggplot(data = crabs, aes(x=FL, y=RW, color=sex)) + geom_point() + ggtitle("FL for RW") + geom_line()
    p7 <- ggplot(data = crabs, aes(x=FL, y=RW, color=sp)) + geom_point() + ggtitle("FL for RW") + geom_line()
    # Here we can see that we can determine sex but not specie
    p3 <- ggplot(data = crabs, aes(x=sex,FL)) + geom_boxplot(outlier.colour="red") + ggtitle("Boxplot for sex under front lobe")
    p4 <- ggplot(data = crabs, aes(x=sex,CL)) + geom_boxplot(outlier.colour="red") + ggtitle("Boxplot for sex under carace length")
    # We can tryto plot carapce dimension in order to determine the specie
    p5 <- ggplot(data = crabs, aes(x=CL, y=CW, color=sex)) + geom_point() + ggtitle("Carapace dimension")
    p8 <- ggplot(data = crabs, aes(x=CL, y=CW, color=sp)) + geom_point() + ggtitle("Carapace dimension")
    # We can determine the specie this time but not the sex
    p6 <- ggplot(crabs, aes(x=CW, color=sex)) + geom_histogram(binwidth=0.5) + ggtitle("Carapace lenth histogram")
    grid.arrange(p1, p2, p3, p7, p6, ncol=3, top="Correlation and covariance")

}

