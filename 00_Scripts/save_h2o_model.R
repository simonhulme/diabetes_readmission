# save_h2o_model

save_h2o_model <- function(model, filename, path) {
    
    h2o.saveModel(object = model, 
                  path = path,
                  filename = filename)
}