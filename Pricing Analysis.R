#   Load Library
    library(arm);
    library(caret);
    library(ggplot2);
    library(MASS);
    library(MCMCpack);

    
#   Enable Multi-Core Processing
    library(doParallel);
    cluster <- makeCluster(detectCores());
    registerDoParallel(cluster);    
    

#   Import CSV Dataset
    setwd("P:/Business_Analytics/Personnel Folders/Philip/01 Projects/Goal Post Analysis - 2015.10.27");
    dataset <- read.csv("Analysis 201508-201510 m95.csv", header=T);
    head(dataset, n=10);
    summary(dataset);

    
#   Preprocessing: Principal Component Anaysis
    preProc <- preProcess(dataset[, 3:9], method="pca", thresh=0.90);
    preProcTraining <- predict(preProc, dataset[, 3:9]);
    preProcTest <- predict(preProc, dataset[, 3:9]);
    round(preProc$rotation, 4);

    
#   Creating Training & Test Datasets
    set.seed(777);
    inTrain <- createDataPartition(y=dataset$Grade, p=0.60, list=F);
    training <- dataset[inTrain,];
    testing <- dataset[-inTrain,];
    head(training, n=10);
    summary(training);


#   Pre-Processing & Cross Validation Settings
    trControl <- trainControl(method="cv", number=7, verboseIter=F, preProcOptions="pca",
        allowParallel=T, savePredictions=T);   

    
#   Model Development - Bayes Generalized Linear Model
    bayesglm <- bayesglm(Yield~., data=training, family=gaussian, prior.mean=0, 
        prior.scale=Inf, prior.df=Inf, na.action=na.omit);  # str(bayesglm);
    step <- stepAIC(bayesglm, direction="both");
    summary(bayesglm);
    varImp(bayesglm);
    
    simulates <- coef(sim(bayesglm));
    head(simulates, n=10);
    posterior.open <- simulates[,2];
    head(posterior.open, n=10);

    hist(posterior.open);
    plot(density(posterior.open), main="", xlab="posterior.open", ylab="density");
    quantile(posterior.open, c(.025, .975));


#   Markov Chain Monte Carlo Regression
    MCMC <- MCMCregress(Yield~., data=training, burnin=3000, mcmc=10000, thin=1, verbose=0, seed=777, beta.start=NA);
    summary(MCMC);
    
    raftery.diag(MCMC);

    
#   Residual Analysis
    residual.plot(Expected=bayesglm$fitted, Residuals=bayesglm$residuals, sigma=sigma.hat(bayesglm));
    mean.residual <- round(mean(bayesglm$residuals), 4);
    print(mean.residual);

     
#   Correlation Analysis    
    correlation_testing <- data.frame(Actual=training$Yield, Estimate=bayesglm$fitted.values);
    RSquare_testing <- (cor(correlation_testing)[1, 2])^2;
    print(RSquare_testing);
    
    correlation_training <- data.frame(Actual=testing$Yield, Estimate=predict(bayesglm, newdata=testing));
    RSquare_training <- (cor(correlation_training)[1, 2])^2;
    print(RSquare_training);
    
    
#   Final Model
    model <- as.data.frame(bayesglm$coefficients);
    confidence <- confint(bayesglm);
    model$Lower <- confidence[, 1];
    model$Upper <- confidence[, 2];
    colnames(model) <- c("Expected", "Lower", "Upper");
    print(model);
    write.csv(model, "model.csv");

    
    
    
