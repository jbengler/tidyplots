
#' Expression data from RNA-Seq study.
#'
#' This data was taken form a RNA-Seq study investigating the regulation of genes
#' in response to central nervous system inflammation.
#'
#' @format A data frame with 800 rows and 9 variables:
#' \describe{
#'   \item{ensembl_gene_id}{Ensembl gene id}
#'   \item{external_gene_name}{Gene symbol}
#'   \item{sample}{Sample name}
#'   \item{expression}{Normalized RNA-Seq expression value}
#'   \item{group}{Experimental group}
#'   \item{sample_type}{Sample type. Either `input` or `IP`.}
#'   \item{condition}{Condition of sampling. Either `healthy` or `EAE`. }
#'   \item{is_immune_gene}{Gene is annotated as immune cell gene. Either `yes` or `no`.}
#'   \item{direction}{Direction of regulation. Either `up` or `down`. }
#' }
#' @source
#' `data_exprs` represents just a small subset of the data aquired in the study.
#'
#' More details about the study can be found here
#' * Nature Neuroscience, [Bassoon proteinopathy drives neurodegeneration in multiple sclerosis](https://www.nature.com/articles/s41593-019-0385-4)
#'
#' The complete raw data can be downloaded here
#' * Gene Expression Omnibus, study accession [GSE104899](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE104899)
"data_exprs"

#' @export
df_demo <-
  dplyr::tibble(category = rep(c("D", "E", "M", "O"), each = 5),
         group = rep(c("red", "blue"), each = 10),
         group2 = rep(c("green", "yellow","green", "yellow"), each = 5),
         group3 = rep(c("unity"), each = 20),
         value = c(2,4,5,4,6,9,8,12,15,16,32,35,24,45,56,23,25,21,22,23),
         value2 = c(8,4,8,9,8,9,0,8,8,9,9,8,7,7,78,8,9,8,8,9))
