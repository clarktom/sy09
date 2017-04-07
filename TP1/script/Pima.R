library(grid)
library(gridExtra)
library(ggplot2)
library(reshape2)
Pima <- read.csv("dataset/Pima.csv", header=T)
Pima$z <- factor(Pima$z)
Pimaquant = Pima[1:7]

cor_matrix <-  function()
{
    c_matrix  <- cor(Pimaquant)
    melted  <-  melt(c_matrix)
    ggplot(data = melted, aes(x=Var1, y=Var2, fill=value, colour="grey")) + geom_tile() + ggtitle("matrice de correlation")
}
hist_age <- function()
{
    p <- ggplot(data = Pimaquant, aes(x = npreg, y = age)) +
        geom_bar(stat="identity") +
        ggtitle("npreg per age")
    p
}
diabete_age  <- function()
{
    ggplot(data=Pima, aes(x = age, fill = z)) +
   geom_bar(stat='count', position='dodge') +
   ggtitle("age VS diabetes") +
   labs(x = 'Age')
}

influ_diabete  <- function()
{
  p1 <- ggplot(Pima, aes(x=glu, color=z)) +
    geom_histogram(fill="white", alpha=0.5, position="identity")
  p2 <- ggplot(Pima, aes(x=bp, color=z)) +
    geom_histogram(fill="white", alpha=0.5, position="identity")
  p3 <- ggplot(Pima, aes(x=skin, color=z)) +
    geom_histogram(fill="white", alpha=0.5, position="identity")
  p4 <- ggplot(Pima, aes(x=bmi, color=z)) +
    geom_histogram(fill="white", alpha=0.5, position="identity")
  p5 <- ggplot(Pima, aes(x=ped, color=z)) +
    geom_histogram(fill="white", alpha=0.5, position="identity")
  grid.arrange(p1, p3, p2, p4, p5, ncol=3, top="Influence diabete")

}
