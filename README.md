
<link rel="stylesheet" href="/github_img/myCSS.css">

# Machine learning classification models for nocturnal dipping of blood pressure

  In recent years, home blood pressure monitoring (HBPM) is the most recommended
monitoring method for blood pressure tracking in Taiwan's hypertension treatment guidelines.
Still, this method has the disadvantage of monitoring blood pressure during nighttime sleep.
However, an irregular nocturnal dipping rate would increase the risk of cardiovascular events
and death.

  Therefore, this study uses the Taiwan Consortium of Hypertension associated
Cardiac Disease (TCHCD) database and <b>uses machine learning methods to predict nocturnal
dipping status with home blood pressure and other variables and provide model
recommendations analysis strategies to assist clinical physicians in early assess the patient's
condition.</b>

  This study predicted the model from three different perspectives: <b>multi-wave repeated
measures, single-wave repeated measures, and cross-sectional data. Uses Binary mixed model
forest (BiMM Forest) series models, random forest, Logistic Regression, and XGBoost model
to compare and select the most suitable model under different data forms.</b> In terms of the
confounding effects, we implemented the analysis with and without covariates.

  At the end of the study, we found out that for the TCHCD database, the performance of
the included covariates was better than that of the non-included covariates in all cases, and the
repeated measures data also performed better than the cross-section data. According to the
data, the best prediction ability is the case of multi-wave repeated measures, and the inclusion
of covariates in Visit1 predicts Visit2. We were using one iteration of the BiMM Forest series
model, <b>with 81.7% of accuracy</b>, indicates that home blood pressure and other variables can
predict. Predict nocturnal blood pressure drop.

<b>Keywords: Nocturnal dipping、Machine learning、Repeated measure、Random Forest、Binary Mixed Model Forest</b>
