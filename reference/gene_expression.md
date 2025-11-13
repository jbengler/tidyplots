# RNA-Seq expression data

RNA-Seq expression data

## Usage

``` r
gene_expression
```

## Format

A data frame.

## Source

[Bassoon proteinopathy drives neurodegeneration in multiple
sclerosis](https://www.nature.com/articles/s41593-019-0385-4), Nature
Neuroscience 2019

[GSE104899](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE104899),
Gene Expression Omnibus

## Examples

``` r
dplyr::glimpse(gene_expression)
#> Rows: 800
#> Columns: 11
#> $ ensembl_gene_id    <chr> "ENSMUSG00000033576", "ENSMUSG00000033576", "ENSMUS…
#> $ external_gene_name <chr> "Apol6", "Apol6", "Apol6", "Apol6", "Apol6", "Apol6…
#> $ sample             <chr> "Hin_1", "Hin_2", "Hin_3", "Hin_4", "Hin_5", "Ein_1…
#> $ expression         <dbl> 2.203755, 2.203755, 2.660558, 2.649534, 3.442740, 5…
#> $ group              <chr> "Hin", "Hin", "Hin", "Hin", "Hin", "Ein", "Ein", "E…
#> $ sample_type        <chr> "input", "input", "input", "input", "input", "input…
#> $ condition          <chr> "healthy", "healthy", "healthy", "healthy", "health…
#> $ is_immune_gene     <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no…
#> $ direction          <chr> "up", "up", "up", "up", "up", "up", "up", "up", "up…
#> $ log2_foldchange    <dbl> 9.395505, 9.395505, 9.395505, 9.395505, 9.395505, 9…
#> $ padj               <dbl> 3.793735e-28, 3.793735e-28, 3.793735e-28, 3.793735e…
```
