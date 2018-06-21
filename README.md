# scMCA

####  A tool defines cell types in mouse based on single-cell digital expression

At first scMCA  is a breif R package for large scale data(large DGE) from scMCA online function [Mouse Cell Atlas](http://bis.zju.edu.cn/MCA)  ,to alleviate burdens of our main Server. 

Now we add a UI for visulizing the scMCA reuslt. 
### Installation

```R
#This require devtools  
install.package('devtools')
library(devtools)
# scMCA requires ggplot2/reshape2/plotly/shiny/shinythemes/shiny
install_github("ggjlab/scMCA")
```

### Quick Start

```R
library(scMCA)
data(mca_lung)
```



