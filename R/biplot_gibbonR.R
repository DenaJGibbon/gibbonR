#' Function to visualize feature data
#' @param mfcc.dataframe Data object from function calc_MFCC.
#' @param classification.type Either "LDA" or "PCA". Whether to use a linear discriminant function analysis or principal component analysis
#' @return
#' @seealso \code{\link{MASS}} which this function wraps
#' @seealso \code{\link{ggplot2}} which this function wraps
#' @export
#' @examples
#'

biplot_gibbonR <- function(mfcc.dataframe, classification.type, class.labs) {


  if (any(is.na(mfcc.dataframe))==TRUE)
    stop("Error: Data cannnot have NA values")

if(classification.type=="LDA"){

  if(class.labs==TRUE){

## Define for function
fit.lda <- MASS::lda(class~.,mfcc.dataframe)
class.lda.values <- predict(fit.lda)
newdata <- data.frame(class = mfcc.dataframe[,1], lda = class.lda.values$x)

lda.plot <- ggplot(newdata, aes(lda.LD1, lda.LD2, colour = class)) +
  geom_point() +
  geom_text(aes(label=class))+
  stat_ellipse()+
  xlab("LD1")+
  ylab("LD2")+
  stat_ellipse(aes(lda.LD1, lda.LD2))+
  theme(legend.position = "")+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  viridis::scale_color_viridis(discrete=T,end=0.9)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line=element_blank())

return(lda.plot)
  }


 else{

    ## Define for function
    fit.lda <- MASS::lda(class~.,mfcc.dataframe)
    class.lda.values <- predict(fit.lda)
    newdata <- data.frame(class = mfcc.dataframe[,1], lda = class.lda.values$x)

    lda.plot <- ggplot(newdata, aes(lda.LD1, lda.LD2, colour = class)) +
      geom_point() +
      stat_ellipse()+
      xlab("LD1")+
      ylab("LD2")+
      stat_ellipse(aes(lda.LD1, lda.LD2))+
      theme(legend.position = "")+
      theme(axis.text.x = element_text(size=20))+
      theme(axis.text.y = element_text(size=20))+
      theme(axis.title.x = element_text(size=20))+
      theme(axis.title.y = element_text(size=20))+
      viridis::scale_color_viridis(discrete=T,end=0.9)+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line=element_blank())

    return(lda.plot)
  }
}

  if(classification.type=="PCA"){
    fit.pca <- princomp(mfcc.dataframe[,-c(1)])

    newdata <- data.frame(class = mfcc.dataframe[,1], pca = fit.pca$scores)

    pca.plot <- ggplot(newdata, aes(pca.Comp.1, pca.Comp.2, colour = class)) +
      geom_point() +
      stat_ellipse()+xlab("PC1")+
      ylab("PC2")+
      theme(legend.position = "")+
      theme(axis.text.x = element_text(size=20))+
      theme(axis.text.y = element_text(size=20))+
      theme(axis.title.x = element_text(size=20))+
      theme(axis.title.y = element_text(size=20))+
      viridis::scale_color_viridis(discrete=T,end=0.9)+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line=element_blank())

    return(pca.plot)
  }

}


