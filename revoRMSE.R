#` Calculate the root mean squared error for a model on an XDF dataset.
#`
#` This function applies rxPredict independently within each chunk in a transform function
#` and accumulates the sum of squared errors and row counts from which it calculates RMSE.
#`
#` @param with_model a trained rxLinMod model
#` @param xdfdata an XDF data source on which the model can be applied.

revoRMSE <- function(with_model, xdfdata){
  predict_SSE_transform <- function(data_list){
    if (.rxChunkNum == 1){
      .rxSet("SSE", 0)
      .rxSet("rowCount", 0)
    }
    SSE <- .rxGet("SSE")
    residual <- rxPredict(model, as.data.frame(data_list), 
                          computeResiduals=TRUE, residVarNames="residual")$residual
    SSE <- SSE + sum(residual^2, na.rm=TRUE)
    rowCount <- sum(!is.na(residual))
    .rxSet("SSE", SSE)
    .rxSet("rowCount", rowCount)
    return(data_list)
  }
  xformObjs <- rxDataStep(inData=xdfdata, 
                          transformFunc=predict_SSE_transform, 
                          transformVars=rxGetVarNames(xdfdata), 
                          transformObjects=list(SSE=0, rowCount=0, model=with_model), 
                          returnTransformObjects=TRUE)
  with(xformObjs, sqrt(SSE/rowCount))
}

# with_formula <- ArrDelay ~ Origin:Dest
# fit <- rxLinMod(with_formula, training_set, cube=TRUE)
# revoRMSE(fit, test_set)
