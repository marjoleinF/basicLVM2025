
## Lavaan
library(lavaan)

## observed data
obsnames <- c("year1","year2","year3","year4","gender","famstat")

## This example is taken from Duncan and Duncan (1996), who conducted
## a longitudinal study of alcohol use among adolescents. 
## A sample of 321 adolescents were surveyed annually over a 4-year period

values <- c(1.000,
     .640, 1.000,
     .586,	.670, 1.000,
     .454,	.566,	.621, 1.000,
     .001,	.038,	.118,	.091, 1.000,
     -.214, -.149, -.135, -.163, -.025, 1.000)

SD <- c(1.002,.960,.912,.920,.504,.498) 

duncanmeans <- c(2.271,2.560,2.694,2.965,.573,.554) 

duncancor <- lav_matrix_lower2full(values)
duncancov <- diag(SD)%*%duncancor%*%diag(SD)
dimnames(duncancov) <- list(obsnames,obsnames)
names(duncanmeans) <- obsnames


## define model
duncanmodel1 <- '

      # Random intercept
      RI =~ 1*year1 + 1*year2 + 1*year3 + 1*year4

      # Create within-person centered variables
      year1_within =~ 1*year1
      year2_within =~ 1*year2
      year3_within =~ 1*year3
      year4_within =~ 1*year4
      
      # Estimate lagged effects between within-person centered variables
			# regression equations
			year4_within ~ b43*year3_within
			year3_within ~ b32*year2_within
			year2_within ~ b21*year1_within

			# residual variances
			year4_within ~~ p44*year4_within
			year3_within ~~ p33*year3_within
			year2_within ~~ p22*year2_within

			# variance exogenous variable
			year1_within ~~ p11*year1_within

      # restrict variances observed indicators
      year1 ~~ 0*year1
      year2 ~~ 0*year2
      year3 ~~ 0*year3
      year4 ~~ 0*year4

      # variance intercept
      RI ~~ RI
      
      # MEANS
      year1 ~ 0*1
      year2 ~ 0*1
      year3 ~ 0*1
      year4 ~ 0*1

      year1_within ~ 0*1
      year2_within ~ 1
      year3_within ~ 1
      year4_within ~ 1
      
      RI ~ 1
      
		      '

## run model
duncanmodel1Out <- lavaan(duncanmodel1, 
                       sample.cov=duncancov,
                       sample.mean=duncanmeans, 
                       sample.nobs=321)	

## output
summary(duncanmodel1Out, standardized=TRUE)


## add second order effects
duncanmodel2 <- '

      # Random intercept
      RI =~ 1*year1 + 1*year2 + 1*year3 + 1*year4

      # Create within-person centered variables
      year1_within =~ 1*year1
      year2_within =~ 1*year2
      year3_within =~ 1*year3
      year4_within =~ 1*year4
      
      # Estimate lagged effects between within-person centered variables
			# regression equations
			year4_within ~ b43*year3_within + b42*year2_within
			year3_within ~ b32*year2_within + b31*year1_within
			year2_within ~ b21*year1_within

			# residual variances
			year4_within ~~ p44*year4_within
			year3_within ~~ p33*year3_within
			year2_within ~~ p22*year2_within

			# variance exogenous variable
			year1_within ~~ p11*year1_within

      # restrict variances observed indicators
      year1 ~~ 0*year1
      year2 ~~ 0*year2
      year3 ~~ 0*year3
      year4 ~~ 0*year4

      # variance intercept
      RI ~~ RI
      
      # MEANS
      year1 ~ 0*1
      year2 ~ 0*1
      year3 ~ 0*1
      year4 ~ 0*1

      year1_within ~ 0*1
      year2_within ~ 1
      year3_within ~ 1
      year4_within ~ 1
      
      RI ~ 1
      
		      '

## run model
duncanmodel2Out <- lavaan(duncanmodel2, 
                       sample.cov=duncancov,
                       sample.mean=duncanmeans, 
                       sample.nobs=321)	

## output
summary(duncanmodel2Out, standardized=TRUE)
anova(duncanmodel1Out,duncanmodel2Out)




## test equality of first order effects
duncanmodel3 <- '
      # Random intercept
      RI =~ 1*year1 + 1*year2 + 1*year3 + 1*year4

      # Create within-person centered variables
      year1_within =~ 1*year1
      year2_within =~ 1*year2
      year3_within =~ 1*year3
      year4_within =~ 1*year4
      
      # Estimate lagged effects between within-person centered variables
			# regression equations
			year4_within ~ b1*year3_within + b42*year2_within
			year3_within ~ b1*year2_within + b31*year1_within
			year2_within ~ b1*year1_within

			# residual variances
			year4_within ~~ p44*year4_within
			year3_within ~~ p33*year3_within
			year2_within ~~ p22*year2_within

			# variance exogenous variable
			year1_within ~~ p11*year1_within

      # restrict variances observed indicators
      year1 ~~ 0*year1
      year2 ~~ 0*year2
      year3 ~~ 0*year3
      year4 ~~ 0*year4

      # variance intercept
      RI ~~ RI
      
      # MEANS
      year1 ~ 0*1
      year2 ~ 0*1
      year3 ~ 0*1
      year4 ~ 0*1

      year1_within ~ 0*1
      year2_within ~ 1
      year3_within ~ 1
      year4_within ~ 1
      
      RI ~ 1

		      '

## run model
duncanmodel3Out <- lavaan(duncanmodel3, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel3Out,standardized=TRUE)
anova(duncanmodel3Out,duncanmodel2Out)






## test equality of second order effects
duncanmodel4 <- '
      # Random intercept
      RI =~ 1*year1 + 1*year2 + 1*year3 + 1*year4

      # Create within-person centered variables
      year1_within =~ 1*year1
      year2_within =~ 1*year2
      year3_within =~ 1*year3
      year4_within =~ 1*year4
      
      # Estimate lagged effects between within-person centered variables
			# regression equations
			year4_within ~ b1*year3_within + b2*year2_within
			year3_within ~ b1*year2_within + b2*year1_within
			year2_within ~ b1*year1_within

			# residual variances
			year4_within ~~ p44*year4_within
			year3_within ~~ p33*year3_within
			year2_within ~~ p22*year2_within

			# variance exogenous variable
			year1_within ~~ p11*year1_within

      # restrict variances observed indicators
      year1 ~~ 0*year1
      year2 ~~ 0*year2
      year3 ~~ 0*year3
      year4 ~~ 0*year4

      # variance intercept
      RI ~~ RI
      
      # MEANS
      year1 ~ 0*1
      year2 ~ 0*1
      year3 ~ 0*1
      year4 ~ 0*1

      year1_within ~ 0*1
      year2_within ~ 1
      year3_within ~ 1
      year4_within ~ 1
      
      RI ~ 1

		      '

## run model
duncanmodel4Out <- lavaan(duncanmodel4, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel4Out, standardized=TRUE)
anova(duncanmodel4Out,duncanmodel3Out)


## test equality of observed indicator means


## adding predictors of first measurement
duncanmodel6 <- '

      # Random intercept
      RI =~ 1*year1 + 1*year2 + 1*year3 + 1*year4

      # Create within-person centered variables
      year1_within =~ 1*year1
      year2_within =~ 1*year2
      year3_within =~ 1*year3
      year4_within =~ 1*year4
      
      # Estimate lagged effects between within-person centered variables
			# regression equations
			year4_within ~ b1*year3_within + b42*year2_within
			year3_within ~ b1*year2_within + b31*year1_within
			year2_within ~ b1*year1_within
			RI ~ gender + famstat

			# residual variances
			year4_within ~~ p44*year4_within
			year3_within ~~ p33*year3_within
			year2_within ~~ p22*year2_within

			# (co)variance exogenous variable
			year1_within ~~ p11*year1_within
			gender ~~ p55*gender
			famstat ~~ p66*famstat
			gender ~~ p65*famstat

      # restrict variances observed indicators
      year1 ~~ 0*year1
      year2 ~~ 0*year2
      year3 ~~ 0*year3
      year4 ~~ 0*year4

      # variance intercept
      RI ~~ RI
      
      # MEANS
      year1 ~ 0*1
      year2 ~ 0*1
      year3 ~ 0*1
      year4 ~ 0*1

      year1_within ~ 0*1
      year2_within ~ 1
      year3_within ~ 1
      year4_within ~ 1
      
      RI ~ 1
      gender ~ 1
      famstat ~ 1

		      '

## run model
duncanmodel6Out <- lavaan(duncanmodel6, sample.cov=duncancov,
                  sample.mean=duncanmeans, sample.nobs=321)	

## output
summary(duncanmodel6Out,standardized=TRUE)




