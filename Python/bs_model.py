#!/usr/bin/python

models = {\
'model': "modelname", \
'predictors': "preds", \
'response': "resp" \
}

print """
lm.fitted <- fitted({model})
lm.resid <- residuals({model})
lm.model <- model.matrix({model})
lm.boot <- RTSB({response}, {predictors}, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=1)
""".format(**models) 

# print """
# # Step 3.4 Bootstrap the above time series model. 
# # Are the coefficients significant?
# # Get the fitted values from the regression model
# lm.fitted <- fitted({model})
# # Get the residuals from the regression model
# lm.resid <- residuals({model})
# # Get the regression model
# lm.model <- model.matrix({model})
# # Use the RTSB function to obtain the bootstrap
# lm.boot <- RTSB({response}, {predictors}, lm.fitted, lm.resid, lm.model, 50000)
# # The estimates
# lm.boot
# # C.I.
# boot.ci(lm.boot,0.95,type=c('bca','perc'))
# """.format(**models) 
