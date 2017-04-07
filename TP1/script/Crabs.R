# sp : Blue or Orange
# sex ...
# index : within groups
# FL : frontal lobe
# RW : rear width
# CL : carapace length
# CW : carapace width
# BD : body depth
library(MASS)
library(grid)
library(gridExtra)
library(reshape2)
library(ggplot2)
data(crabs)
crabsquant <- crabs[,4:8]

melt_crabs <- function()
{
    cormat_original <- cor(crabsquant)
    melted_cormat_o <- melt(cormat_original)
    ggplot(data = melted_cormat_o, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + ggtitle("matrice de correlation")
}
find_sex  <-  function()
{
    # Plotting FL and RW as they are correlated to see if we can identify a difference between sex and specie
    # ggplot(data = crabs, aes(x=FL, y=RW, color=sex)) + geom_point() + ggtitle("FL for RW")
    # Creating the regression line :
    require(stats)
    # reg<-lm(FL ~ RW, data = crabs)
    # coeff=coefficients(reg)
    # Equation :
    #eq = paste0("y = ", coeff[2], "*x + ", coeff[1])
    # Graph
    sp <- ggplot(data=crabs, aes(x=FL, y=RW, color=sex)) + geom_point() + ggtitle("Influence of sex")
    sp + stat_smooth(method="lm",se=FALSE)
}
find_specie <- function(){
    # p7 <- ggplot(data = crabs, aes(x=FL, y=RW, color=sp)) + geom_point() + ggtitle("FL for RW") + geom_line()
    # Here we can see that we can determine sex but not specie
    # p3 <- ggplot(data = crabs, aes(x=sex,FL)) + geom_boxplot(outlier.colour="red") + ggtitle("Boxplot for sex under front lobe")
    # p4 <- ggplot(data = crabs, aes(x=sex,CL)) + geom_boxplot(outlier.colour="red") + ggtitle("Boxplot for sex under carace length")
    # We can try to plot carapace dimension in order to determine the specie
    # p5 <- ggplot(data = crabs, aes(x=CL, y=CW, color=sex)) + geom_point() + ggtitle("Carapace dimension")
    sp <- ggplot(data = crabs, aes(x=CL, y=CW, color=sp)) + geom_point() + ggtitle("Carapace dimension")
    sp + stat_smooth(method="lm", se=FALSE)
    # We can determine the specie this time but not the sex
    # p6 <- ggplot(crabs, aes(x=CW, color=sex)) + geom_histogram(binwidth=0.5) + ggtitle("Carapace lenth histogram")
}

