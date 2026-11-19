# Print an aligned table of pre-formatted display strings

Prints a data frame of already formatted strings with one common width
per column across all rows: the columns named in `right_cols` are right
aligned, the others left aligned; headers are left aligned. Unlike
[`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html),
which pads and trims the columns itself, this keeps the alignment of the
numeric columns (the decimal points line up). Tables wider than `width`
are split into blocks of columns that are printed one below the other,
as [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)
does.

## Usage

``` r
printAlignedTable(df, right_cols = character(0), width = getOption("width"))
```

## Arguments

- df:

  data frame of display strings

- right_cols:

  character vector of column names to right align

- width:

  maximal line width; defaults to `getOption("width")`

## Value

`df`, invisibly
