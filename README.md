# MechaCar AnalysisThis analysis was performed to understand how different car characteristics such as vehicle length, vehicle weight, spoiler angle, drivetrain, and ground clearance affect the MPG characteristic of the vehicle. Also in the present analysis were considered suspension coil test results from multiple production lots to determine if the manufacturing process is consistent across lots.Moreover further analysis recommendation were summarized at the end of the report.## MPG RegressionTo understand which factors have an impact on MPG car characteristics was build correlation matrix. As a result was observed moderate correlation between MPG and vehicle length (0.61) and weak correlation between MPG and ground clearance (0.33). Other variables have very weak or none correlation to MPG (less than 0.3). Thus vehicle length and ground clearance were chosen for further multiple linear regression.H0 for our multiple linear regression sounds like: There is no linear correlation between MPG and vehicle length and ground clearance. If we look into p-value of our multiple regression model we see that it much less than 0.05 so we can reject our null hypothesis, it means that our slope parameter is not equal to 0.After getting summary of multiple linear regression was find out that our adjusted R-square is equal 0.66 (we used adjusted R-square because it gives us accuracy of our prediction model adjusted by the number of used independent variables). According Radj our model is accurate for 66%. It’s not perfect accuracy but it’s moderate and we can work with this model further and use it to predict further observations.Our predictive model will look as follows:Mpg = 6.08*Vehicle length +3.57*ground clearance – 91.56 ## Suspension Coil SummaryTo perform statistical summary for the suspension coil’s pounds-per-inch (PSI) was used summary() function (to get mean and median), sd() function to get standart deviation and var() to get the variance.According made analysis for all lots Mean values are close to Median values, it means that we have roughly symmetric data.The highest standard deviation is observed for Lot3. It means that PSI for each vehicle is more differ from the mean of group. Variance (average of the squared differences from the mean) is also the highest for the Lot3 and equal to 170. As design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per inch, Lot3 doesn’t meet this design specification.## Suspension Coil T-Test To perform suspension coil-t-test was chosen one sample t-test as we have only one observed group.H0 for T-Test: there is no statistical difference between suspension coil’s pound-per-inch results and the mean population results of 1,500 pounds per inch.Calculated p-value is 0.06 (larger than 0.05) so we keep our null hypothesis, thus there is no statistical difference between suspension coil’s pound-per-inch sample results and the mean population results of 1,500 pounds per inch.## Further studyAs MechaCar has multiple design specification it might be interesting to conduct focus groups for A/B testing. Several groups of customers will use different prototypes.After using prototypes customers have to fill survey to evaluate own satisfaction with the used vehicle.As a dependent variable will be used customer satisfaction (5-points likert scale) - categorical.As a Independent Variables will be used:	0-60 acceleration (numerical)	MPG (numerical)	Ground Clearance (numerical)	Trunk volume (numerical)	After collecting data the regression analysis (multiple linear regression) will be performed to see what characteristics have a larger impact on customer satisfaction. So H0 : observed characteristics like 0-60 acceleration, MPG, Ground Clearance and Trunk Volume have no significant impact on customer satisfaction.Using characteristic that have an impact on customer satisfaction can be built the predictive model to predict customer satisfaction if in one vehicle will be implemented the most important characteristics. For model evaluation will be considered R-squared (adjusted R-squared).	   