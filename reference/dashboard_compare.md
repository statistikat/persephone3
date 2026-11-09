# Compare two single series dashboards

Creates a side-by-side comparison view of two hierarchical time series,
displaying their individual diagnostics alongside a comparison panel.

## Usage

``` r
dashboard_compare(x, y, title = "Series Comparison", file = NULL)
```

## Arguments

- x:

  First hierarchical time series object

- y:

  Second hierarchical time series object

- title:

  Main title for the comparison (default: "Series Comparison")

- file:

  Optional file path to save HTML output

## Value

browsable HTML UI for comparison

## Note

This is an internal function; use `createDashboard(x, y)` instead for
the public API.

## Examples

``` r
obj1 <- perX13(AirPassengers, "rsa3")
obj1$run()
obj2 <- perX13(AirPassengers, "rsa1")
obj2$run()
createDashboard(obj1, obj2)  # preferred

  


  
    
      Series A - ARIMA (0 1 1)(0 1 1)
      
        Log transformation
        Yes
      
    
    
      Diagnostics
      
        
          Normality
          
            
              0.1698
            
            
              Good
            
          
        
        
          Independence
          
            
              0.3545
            
            
              Good
            
          
        
        
          Seasonality
          
            
              NA
            
            
              Undefined
            
          
        
        
          TD F-test
          
            
              NA
            
            
              Undefined
            
          
        
      
    
  
  
    
      Series B - ARIMA (0 1 1)(0 1 1)
      
        Log transformation
        Yes
      
    
    
      Diagnostics
      
        
          Normality
          
            
              0.1698
            
            
              Good
            
          
        
        
          Independence
          
            
              0.3545
            
            
              Good
            
          
        
        
          Seasonality
          
            
              NA
            
            
              Undefined
            
          
        
        
          TD F-test
          
            
              NA
            
            
              Undefined
            
          
        
      
    
  
  
    Comparison Summary
    

Metric
```
