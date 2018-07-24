##------------------------------------------------------------------------------------------------------

#' Mouse Cell Atlas mapping
#' description
#' @param scdata data.frame or matrix, col correspond to cells and rows correspond to genes
#' @param numbers_plot default is 3, number, it will return top "numbers_plot" records in the plot
#' @importFrom reshape2 melt
#' @export

scMCA <- function(scdata,numbers_plot=3){
  tst.expr <- data.frame(matrix(nrow = dim(ref.expr)[1],ncol=dim(scdata)[2]))
  rownames(tst.expr)<-rownames(ref.expr)
  colnames(tst.expr)<-colnames(scdata)
  for (i in rownames(tst.expr)) {
    if(i%in%rownames(scdata))tst.expr[i,]<- scdata[i,]
  }
  tst.expr[is.na(tst.expr)]<-0
  tst.expr<-as.matrix(t(t(tst.expr)/colSums(tst.expr))*100000)
  tst.expr<-log(tst.expr+1)
  cors <- cor(log(ref.expr+1),tst.expr)

  cors_index <- apply(cors,2,gettissue,numbers_plot)
  cors_index <- sort(unique(as.integer(cors_index)))
  scblast.result <- apply(cors,2,function(x) rownames(cors)[which.max(x)])
  cors_in = cors[cors_index,]
  colnames(cors_in)<-colnames(scdata)
  cors_out = reshape2::melt(cors_in)[c(2,1,3)]
  colnames(cors_out)<- c("Cell","Cell type","Score")
  cors_out <- as.data.frame(cors_out %>% group_by(Cell) %>% top_n(n=numbers_plot,wt=Score))


  result <- list()
  result[["cors_matrix"]] <- cors
  result[['top_cors']]<-numbers_plot
  result[['scMCA']]<-scblast.result
  result[['scMCA_probility']]<-cors_out
  return(result)
}

#' @export
gettissue <- function(x,Num=3){
  top_markers <-order(x,decreasing = T)[1:Num]
  return(top_markers)
}

#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom  plotly plot_ly
#' @importFrom pheatmap pheatmap
#' @export
plotMCA <- function(mca_result,interactive_plot=F, numbers_plot=3, col_font_size = 1, row_font_size=8, show_col=T,show_bar=T, show_tree = T){
  data(ref.expr)
  cors <- mca_result$cors_matrix
  cors_index <- apply(cors,2,gettissue,numbers_plot)
  cors_index <- sort(unique(as.integer(cors_index)))
  data = cors[cors_index,]
  cors_out = reshape2::melt(data)[c(2,1,3)]
  colnames(cors_out)<- c("Cell","Cell type","Score")
  cors_out <- as.data.frame(cors_out %>% group_by(Cell) %>% top_n(n=numbers_plot,wt=Score))
  mca_result$scMCA_probility <- cors_out
  mca_result$top_cors <- numbers_plot
  height=dim(data)[1]*10+230
  tree_height = 0
  if(isTRUE(show_tree)){tree_height=50}

    p<-pheatmap(
      data,
      clustering_distance_rows = "euclidean",
      clustering_distance_cols = "euclidean",
      clustering_method = "ward.D",
      fontsize_col = col_font_size,
      fontsize_row = row_font_size,
      color = colorRampPalette(c("grey", "white", "red"))(100),
      cellheight = 10,
      show_colnames = show_col,
      border_color = NA,
      height = height,
      legend = show_bar,
      treeheight_col = tree_height,
      treeheight_row = tree_height
      )
    if(isTRUE(interactive_plot)){

      inter_data<-data[rev(p$tree_row$order),][,p$tree_col$order]
      height= length(p$tree_row$order)*10+230
      plot_ly(x=colnames(inter_data),y=rownames(inter_data),z = inter_data, colors = colorRamp(c("grey", "white","red")),height=height, type = "heatmap", showscale=show_bar) %>% layout(autosize=T,  margin=list(l=0,r=230,b=180,t=20,pad=4),font=list(size=row_font_size),xaxis=list(showticklabels=show_col),yaxis=list(side="right"))
    }
    else{
      p
    }

}
