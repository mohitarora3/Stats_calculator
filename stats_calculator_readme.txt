				_________________________________________

				   Statistics		   Calculator
				_________________________________________


This Readme explains the R-code for various statistical operations.

__________________

Packages Installed
__________________

qcc package is to be installed in order to plot the pareto graph.


_______________________

In-Built Functions Used
_______________________

cat			:	Outputs the objects, concatenating the representations

read.table		:	function reads a file into data frame in table format. The file can be comma delimited or tab 
				or any other delimiter specified by parameter "sep=". If the parameter "header=" is "TRUE", 
				then the first row will be treated as the row names.

readline		:	Reads a line from the terminal (in interactive use)

prompt			:	Passed as an argument to readline function. The string printed when prompting the user for input

gamma(x)		:	returns the gamma value of any real number x.	


___________________________

User-Defined Functions Used
___________________________

Descriptive_Analysis	:	describe the basic features of the data in a study. They provide simple summaries about the sample 
				and the measures. It contains the definition of several other functions. They are:

				
		1.	 Mean			:	finds the average of a given data by adding all the elements of a given sample
							one by one using for loop and then dividing it by the number of elements.
		

		2.	 Sort			:	arranges the elements of the sample space in increasing order by comparing 
							them using for loop.


		3.	 Median			:	defined as to find the middle item of all given observations arranged in order. 


		4.	 Mode			:	Displays the value which occurs most frequently


		5.	 Variance		:	It gives the measure of variation. It's formulations is categorized into
							whether to evaluate from a population or from a sample.

							_______________

							 Formulae Used
							_______________

										     							
								                ? (xi-µ)*(xi-µ)
								Var(x)	=	_______________

										      N

								where,

										xi is the value of the ith item; 
   
										µ is the population arithmetic mean; 
			
										N is the population size.
 

		6.	 Standard Deviation	:	It also gives the measure of variation. It is calculated by square root of variance.	


		7.	 Mean Absolute Deviation:	Returns the mean of the absolute values of all deviations from the mean.

							_______________

							 Formulae Used
							_______________

										     							
								              		  ? |xi-µ|
								Mean Deviation	  =	  _________

										   	     N

								where,

										xi is the value of the ith item; 
   
										µ is the population arithmetic mean; 
			
										N is the population size.
 


		7.	 Range			:	 returns the difference between two extreme values.


		8.	 Quartiles		:	 it divides distribution into four equal parts such that 25% of the data are = Q1;
							 50% of the data are = Q2; 75% of the data are = Q3. 


		9.	 IQR			:	 It is a measure of statistical dispersion, being equal to the difference between 
							 75th and 25th percentiles, or between upper and lower quartiles.
						
	
							 Mathematically,

								IQR = Q3 -  Q1

		
		10.	 Minimum		:	 returns the least value of the data


		11.	 Maximum		:	 returns the greatest value of the data


		12.	 Skewness		:	 shows how data piled-up.If the coefficient of skewness is positive, we say that
							 data is skewed to the right and If the coefficient of skewness is negative, we 
							 say that data is skewed to the left.


		13.	 Kurtosis		:	 measure of the "tailedness" of the probability distribution of a real-valued 
							 random variable

	
		14.	 Moments		:	 returns constant values in a given distribution which help us to ascertain the
							 nature and form of distribution

					

Predictive_Analysis	: 	Predictive analytics is an area of statistics that deals with extracting information from data and 
				using it to predict trends and behavior patterns. It contains several other functions, they are:


		1.	Correlation		:	 describe how two variables vary together, so it can be computed and 
							 interpreted for any two variables. It's value ranges between -1 to 1.


		2.	Linear Regression	:	



 Probability Analysis 


		1.	Permutations		:	returns the arrangement of all the members of a set into some sequence or order


		2.	Combinations		:	returns the selection of items from a collection, such that the order of selection 
							does not matter


		3.	Probability		:	returns the  measure of the likelihood that an event will occur.


							Formula Used
							_____________

									observed value
							prob	=	______________
	
									 total value



		4.	intersection(x,y)	:	returns the count of common elements of set x and set y



		5.	conditional_prob(x,y)	:	returns the probability of an event A to occur provided that another event B has occurred.

							
							Formulae Used 
							_____________


									Intersection(A&B)
							P(A|B)	=	_________________

									     P(B)


		6.	Bayes_Theorem		:	returns the probability of an hypothesis to occur given that event A has already occurred


							Formulae Used 
							_____________

									
									  		 P(A|Hi)*P(Hi)
							P(Hi|A)	=	___________________________________________________

									P(A|H1)*P(H1) + P(A|H2)*P(H2) + .... + P(A|Hn)*P(Hn)


							where, 

								i		=	1 to n(i.e total number of hypothesis)
								P(A|Hi) 	=	probability of event A to occur given that hypothesis Hi has already occurred


 Discrete_Distribution


		1.	Uniform			:	simple distribution that puts equal weight on the integers from one to N


							Formulae Used 
							_____________

											 1
							y	=	f(x|N)	=	___	where, x = 0,1,...,N

											 N


		2.	Bernoulli		:	This distribution best describes all situations where a "trial" is made resulting in either "success" or
							"failure," such as when tossing a coin, or when modeling the success or failure of a surgical procedure.


							Formulae Used 
							_____________

							f(x) = [p^(x)]*[(1-p)^(1-x)],    for x = 0, 1

											where, p is the probability that a particular event (e.g., success) will occur.


		3.	Binomial		:	Suppose we repeat a Bernouilli p experiment n times and count the number X of successes, the distribution of X
							is called the Binomial B(n,p) random variable.  

							
							Formulae Used 
							_____________

							P(X=k)	=	nCk * [p^(k)]*[(1-p)^(n-x)],     for k=0, 1, 2, ..., n.

											where, p is the probability that a particular event (e.g., success) will occur.



		4.	Geometric		:	The geometric distribution models the number of failures before one success in a series of independent trials,
							where each trial results in either success or failure, and the probability of success in any individual trial is constant.


							Formulae Used 
							_____________

							f(x) = p*[(1-p)^x]	for	 x = 0,1,2,... . 

											where, p is the probability that a particular event (e.g., success) will occur.



		5.	Hyper_geometric		:	The probability of obtaining x successes based on a random sample of size n from a population of size N is given by


							Formulae Used 
							_____________


									kCx * (N-k)C(n-x)
							P(x)	=	_________________	where,  k is the number of successes in the population.

									      NCn



		6.	Negative_Binomial	:	This represents the number of failures which occur in a sequence of Bernoulli trials before a target number 
							of successes is reached. In a sequence of independent Bernoulli(p) trials, let the random variable X denote
							the trial at which the rth success occurs, where r is a ?xed integer.


							Formulae Used 
							_____________

							
							P(X=x|r,p)	=	(x-1)C(r-1) * p^r * (1-p)^(x-r)		where, x  =  r,r+1,r+2....



		7.	Poisson			:	The probability distribution of a Poisson random variable X representing the number of successes occurring in a given 
							time interval or a specified region of space is given by the formula:

							
							Formulae Used 
							_____________

									e^(-µ) * µ^x
							P(X)	=	____________
		
									      x!
?

							where,

								x=0,1,2,3…

								e=2.71828

								µ= mean number of successes in the given time interval or region of space



		8.	Multinomial		:	Let x1 …, xk be discrete random variables whose values are the number of times outcome Ei occurs in n trials.
							Then the probability distribution function for x1 …, xk is called the multinomial distribution.


							Formulae Used 
							_____________


											n!
							f(x1,x2,...xk)	=	__________________ * [(p1)^x1].....[(pk)^xk]

										x1!*x2!*.......xk!




		9.	Multivariate_Hypergeometric:	extension of the Hypergeometric distribution where more than two different states of individuals in a group exist.



 Continuous_Distribution


		1.	Uniform			:	A continuous random variable X has a uniform distribution, denoted U(a, b), if its probability density function is:



							Formulae Used 
							_____________


									  1
							f(x)	=	______		for two constants a and b, such that a < x < b.

									 b-a


		2.	Normal			:	The normal distribution is a continuous probability distribution. This has several implications for probability.
							The total area under the normal curve is equal to 1. The probability that a normal random variable X equals any 
							particular value is 0.

							This function returns the probability density of the normal distribution.


							Formulae Used 
							_____________


							f ( x | µ , Var(x) ) = 	 e^( - (( x - µ )^ 2)/ (2 * Var(x))) 
										_____________________________________________
									
											sqrt(2* p * Var(x))

							Where:

									µ is the mean or expectation of the distribution (and also its median and mode).
								
									Var(x) is the variance


		3.	Bivariate_Normal	:	


		4.	Gamma			:	gamma distribution is a two-parameter family of continuous probability distributions. It has a scale
							parameter ? and a shape parameter k. If k is an integer then the distribution represents the sum of k 
							exponentially distributed random variables, each of which has parameter 1/?.

							This function returns the probability density of the gamma distribution.	


							Formulae Used 
							_____________



							f(x;k,?)	=	x^(k-1)*e^(-x/?)
										________________		for   x > 0,  and k,? > 0

										 ?^k * gamma(k)



		5.	Exponential		:	 exponential distribution (also known as negative exponential distribution) is the probability distribution 
							 that describes the time between events in a Poisson process, i.e. a process in which events occur continuously
							 and independently at a constant average rate.


							 Formulae Used 
							 _____________

										?*e^(-?x)	for	x>=0

							  f(x;?)	=	

										0		otherwise

								
									where, ? > 0 is the parameter of the distribution, often called the rate parameter.



 Distribution_Test 


		1.	Chi_Square		:	tests whether to accept or reject null hypothesis on the basis of chi square table.


		2.	t_test			:	tests whether to accept or reject null hypothesis on the basis of student's t - table.


		3.	F_test			:	tests whether to accept or reject null hypothesis on the basis of f table.


		4.	Z_test			:	tests whether to accept or reject null hypothesis on the basis of z table.



 Interval_Estimation


		1.	Estimation_Means


		2.	Mean_Diff_Estimation


		3.	Proportion_Estimation


		4.	Proportions_Diff_Estimation


		5.	Estimation_Variances

		
		6.	Ratio_Two_Variances


 Non_Parametric_Analysis


		1.	Sign Test


		2.	Wilcoxon_test


		3.	Mann_Whitney_Test


		4.	Kruskal_Wallis_Test


 Visualizations


		1.	Histograms		:	a diagram consisting of rectangles whose area is proportional to the frequency
							of a variable and whose width is equal to the class interval.


							Function Used
							______________
							
							hist(x, ...)




		2.	Line_Graph		:	 displays information as a series of data points called 'markers' connected
							 by straight line segments


							
							  Function Used
							  ______________	

							  lines(x, y, type=) 


		3.	Bar_Graph		:	 presents categorical data with rectangular bars with heights or lengths
							 proportional to the values that they represent

							
							  Function Used
							  ______________	
					
							  barplot(x,main="",xlab="")
	


		4.	Pie_Chart		:	  Pie charts are created with the function pie(x, labels=) where x is a 
							  non-negative numeric vector indicating the area of each slice and 
							  labels= notes a character vector of names for the slices


		5.	Scatter_plot		:	 creates a scatter plot with circles at the locations specified by the 
							 vectors x and y 

								
							  Function Used
							  ______________	
					

							   plot(x, y)

								 where x and y are numeric vectors denoting the (x,y) points to plot. 


		6.	Box_plot		:	  depicts groups of numerical data through their quartiles


							  Function Used
							  ______________	
					
							  boxplot(x, data=)

								where x is a formula and data= denotes the data frame providing the data

							  

		7.	q_q_plot		:	  plots quantiles of two distributions against each other, or a plot based
							  on estimates of the quantiles


							  Function Used
							  ______________
	
							  qqplot(x, y, plot.it = TRUE, ...)
					
							   where,

								x	:	The first sample for qqplot

								y	:	The second or only data sample


		8.	Stem_leaf_plot		:	

		
		9.	Pareto_Chart







_______________________________

Stat_Calc() : The Main Function 
_______________________________

	Variables Defined:

		choice		:	 User's choice to choose any of the 9 functions
		
	______________
	
	Algorithm Used
	______________

	
		1. Calculator Starts! Main Menu is displayed to the user and the user is asked to choose any of the 9 functions.
	
		2. User inputs his/her choice and enters into the function of his/her choice.

		3. Again a Menu is displayed to the user and is asked to choose the operation he/she wants to perform.

		4. In this Menu, the user have a choice to switch back to the Main Menu by inputting '0'.

		5. If the user inputs any of the choice available, except 0, then the user is asked to input the required data 
		   and the output is displayed to the user.
		
__________________

Project Challenges
__________________

	The main challenge in this project was to access and update the initialised variables  globally.

	By using '<<-' operator the defined variables becomes global and can be accessed anywhere you wish to use them.

_______

Results
_______

	This calculator can be used for most of the common functionalities of statistics and can help the statisticians to save their time.
 

							THANK YOU!

______________________________________________________________________________________________________________________________________________________________________________________________ 