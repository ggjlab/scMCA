##------------------------------------------------------------------------------------------------------
##cytosee

#' Mouse Cell Atlas mapping
#' description hello world
#' @param scdata data.frame or matrix, col correspond to cells and rows correspond to genes
#' @param plot_heat default TRUE, boolean, this will return the result alongwith a heatmap, else will return in a list
#' @param interactive_plot defalut is TRUE, boolean, IF TRUE, a plotly package will be used for interactive plotting
#' @param numbers_plot default is 3, number, it will return top "numbers_plot" records in the plot
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom plotly ggplotly
#' @export

scMCA <- function(scdata,plot_heat=TRUE,interactive_plot=TRUE,numbers_plot=3){
  tst.expr <- data.frame(matrix(nrow = dim(ref.expr)[1],ncol=dim(scdata)[2]))
  rownames(tst.expr)<-rownames(ref.expr)
  colnames(tst.expr)<-colnames(scdata)
  for (i in rownames(tst.expr)) {
    if(i%in%rownames(scdata))tst.expr[i,]<- scdata[i,]
  }
  tst.expr[is.na(tst.expr)]<-0
  cors <- cor(ref.expr,tst.expr)

  cors_index <- apply(cors,2,gettissue)
  cors_index <- sort(unique(as.integer(cors_index)))
  scblast.result <- apply(cors,2,function(x) rownames(cors)[which.max(x)])
  cors_in = cors[cors_index,]
  colnames(cors_in)<-colnames(scdata)
  result<-list()
  if(isTRUE(plot_heat)){
    p=plotMCA(cors_in)
    if(isTRUE(interactive_plot)){
      return(ggplotly(p))
    }
    else{
    }
    return(p)

  }
  else{
    result[['scMCA']]<-scblast.result
    result[['scMCA_probility']]<-cors_in
    return(result)
  }




}

#' @export
gettissue <- function(x,Num=3){
  top_markers <-order(x,decreasing = T)[1:Num]
  return(top_markers)
}

#' @export
plotMCA <- function(data){
  df_heatmap1 <- melt(t(data))
  colnames(df_heatmap1)<-c('cell','type','value')
  p<-ggplot(df_heatmap1, aes(cell,type )) +
    geom_tile(aes(fill = value), color = "white") +
    scale_fill_gradientn(colours = c('grey','white','red')) +
    ylab("Types") +
    xlab("Cells") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 12),
          plot.title = element_text(size=16),
          axis.title=element_text(size=14,face="bold"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(fill = "level")
    return(p)
}
