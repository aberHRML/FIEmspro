### NOTES

__The following are the details of `R CMD check` (2017-06-26)__

__As `FIEmspro` is no longer actively developed; these NOTES are simply for reference.__

```R
R CMD check FIEmspro_1.2-0.tar.gz

<--truncated-->

* checking examples ... OK
* checking PDF version of manual ... OK
* DONE
Status: 3 WARNINGs, 3 NOTEs
```

#### WARNINGs

```R
* checking S3 generic/method consistency ... WARNING
mean:
  function(x, ...)
mean.shift:
  function(x, y, method, log.f)
```

```R
* checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'mean.shift'
  ‘x’ ‘y’ ‘method’ ‘log.f’

Undocumented arguments in documentation object 'panel.elli'
  ‘x’ ‘y’ ‘ep’ ‘conf.level’ ‘...’

Undocumented arguments in documentation object 'pca.comp'
  ‘x’ ‘scale’ ‘pcs’ ‘...’
```

```R
* checking data for ASCII and uncompressed saves ... WARNING

  Note: significantly better compression could be obtained
        by using R CMD build --resave-data
             old_size new_size compress
  abr1.RData    1.5Mb    1.1Mb       xz
```

#### NOTES

```R
* checking package dependencies ... NOTE
Depends: includes the non-default packages:
  ‘e1071’ ‘randomForest’ ‘MASS’ ‘KernSmooth’ ‘RColorBrewer’ ‘xtable’
  ‘plotrix’ ‘impute’
Adding so many packages to the search path is excessive and importing
selectively is preferable.
```

```R
* checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘KernSmooth’ ‘MASS’ ‘RColorBrewer’ ‘impute’ ‘plotrix’ ‘xtable’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
':::' calls which should be '::':
  ‘MASS:::eqscplot’ ‘MASS:::ldahist’
```

```R
no visible global function definition (multiple)
```
