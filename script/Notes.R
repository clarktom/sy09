# Opening document
notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
# Morphing quantitave values into qualitative ones
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
ordered=T)
notes <- na.omit(notes)
# Plotting different information
plot_notes <- function()
{
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(reshape2)
    # Data representation
    p0 <- ggplot(notes, aes(x=note.totale)) + geom_histogram(aes(fill= ..count..)) + scale_fill_gradient("Count", low="green", high="red") + ggtitle("Grades density")
    p1 <- ggplot(notes, aes(x=specialite, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Speciality influence")
    p2 <- ggplot(notes, aes(x=dernier.diplome.obtenu, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Last diploma influence")
    p3 <- ggplot(notes, aes(x=niveau, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Level influence")
    p4 <- ggplot(notes, aes(x=correcteur.final, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Marker influence")
    grid.arrange(p0, p1, p3, p2, p4, ncol=3, top="Notes plots")
}
