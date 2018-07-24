# scMCA

####  A tool defines cell types in mouse based on single-cell digital expression

At first scMCA  is a breif R package for large scale data(large DGE) from scMCA online function [Mouse Cell Atlas](http://bis.zju.edu.cn/MCA)  ,to alleviate burdens of our main Server. 

Now we add a UI for visulizing the scMCA reuslt. 
### Installation

```R
#This require devtools  
install.packages('devtools')
library(devtools)
# scMCA requires ggplot2/reshape2/plotly/shiny/shinythemes/shiny
install_github("ggjlab/scMCA")
```

### Quick Start

```R
library(scMCA)
# mca_lung is an example expression matrix from MCA project.
> data(mca_lung)
> dim(mca_lung)
[1] 2884   80
# 2884 genes expression value of 80 cells

# scMCA has two parameters , single cell expression matrix(scdata) and 
# the number of most similar cell types
> mca_result <- scMCA(scdata = mca_lung, numbers_plot = 3)

```
The return of scMCA() is a list which contains 4 parts.
* cors_matrix: Pearson correlation coefficient matrix of each cell and cell type.
* top_cors: equals to numbers_plot 
* scMCA: the most relevant cell type for each query cell
* scMCA_probility: the top n relevant cell types for each query cell

```R
# open shiny for visualize result for scMCA
scMCA_vis(mca_result)
```

scMCA_vis() provides a bref function for visualizing and downloading of scMCA results
![scMCA_vis](http://bis.zju.edu.cn/MCA/assets/img/scMCA_vis_demo.png)
