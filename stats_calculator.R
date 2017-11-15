Descriptive_Analysis <<- function()
 {
  
  Mean<<-function()
  {
    sum<-0				#initializing sum to 0
    for(i in 1:col_length)
    {
      sum<-sum+coll[i]
    }
    return(sum/col_length)
  }
  
  Sort<<-function()		#selection sort
  {
    for(i in 1:col_length)
    {
      small<-coll[i]
      index<-i
      
      for(j in i:col_length)
      {
        if(coll[j]<small)		#finding smallest element
        {
          small<-coll[j]
          index<-j
        }
        else
        {
          
        }
        
      }
      
      
      if(i!=index)
      {
        temp<-coll[i]			#swapping 
        coll[i]<<-coll[index]
        coll[index]<<-temp
      }
    }
    return (coll)
  }

  Variance <<- function()
  {
    mean1 <-Mean()	#calculating mean
    sum<-0
    for(i in 1:col_length)
    {
      difference<-coll[i]-mean1
      difference_square<-difference*difference
      sum<-sum+difference_square
    }
    return (sum/col_length)
  }
  
  Standard_Deviation<<-function()
  {
    x<-Variance()
    return (sqrt(x))	#square root of variance
  }
  
  Median<<-function()
  {
    sorted_col<-Sort()		#performing sort
    if(col_length%%2!=0)
    {
      return(sorted_col[(col_length+1)/2])
    }
    else
    {
      a<-sorted_col[col_length/2]
      b<-sorted_col[(col_length+1)/2]
      c<-(a+b)/2
      return(c)
    }
  }
  
  Quartile<<-function(i)
  {
    sorted_col<-Sort()
    qi<-i*(col_length+1)%/%4
    qf<-i*(col_length+1)/4
    difference<-qf-qi
    
    if(qi!=qf)
    {
      return (sorted_col[qi]+difference*(sorted_col[qf]-sorted_col[qi]))
    }
    else
    {
      return (sorted_col[qi])
    }
  }
  
  IQR1 <<-function()
  {
    q1<-Quartile(1)	#first quartile
    q3<-Quartile(3)	#third quartile
    return (q3-q1)
  }
  
  Mean_Deviation<<-function()
  {
    sum<-0
    mean1<-Mean()	#calculating mean
    for(i in 1:col_length)
    {
      if(coll[i]>mean1)
      {
        sum<-sum+(coll[i]-mean1)		#taking sum of mod of (coll[i]-mean) 
      }
      else
      {
        sum<-sum+(mean1-coll[i])
      }
    }
    return (sum/col_length)
  }
  
  Moment<<-function(j)
  {
    m<-Mean()		#calculating mean
    sum<-0
    for(i in 1:col_length)
    { 
      sum<-sum+((coll[i]-m)^j)
      
    }
    return (sum/col_length)
  }
  
  Minimum<<-function()
  {
    min<-coll[1]
    for(i in 2:col_length)
    {
      if(coll[i]<min)
      {
        min<-coll[i]
      }
    }
    return(min)
  }
  
  
  
  Maximum<<-function()
  {
    max<-coll[1]
    for(i in 2:col_length)
    {
      if(coll[i]>max)
      {
        max<-coll[i]
      }
    }
    return(max)
  }
  
  Range<<-function()
  {
    return(Maximum()-Minimum())	
  }
  
  Skewness<<-function()
  {
    meu3<-Moment(3)	#3rd moment
    meu2<-Moment(2)     #2nd moment
    return ((meu3^2)/(meu2^3))
  }
  
  Kurtosis<<-function()
  {
    meu4<-Moment(4)	#4th moment
    meu2<-Moment(2)	#2nd moment
    return((meu4/meu2^2)-3)
  }
  
  Mode<<-function()
  {
    sorted_col<-Sort()
    p_c<-0
    c<-1
    temp<-coll[i]
    for( i in 2:col_length)
    {
      if(temp==sorted_col[i])
      {
        c<-c+1
      }
      else 
      {
        if(c>p_c)
        {
          p_c<-c	#if current count is greater than previous count update the previous count
          max<-temp
        }
        c<-1
        temp<-sorted_col[i]
        
      }
    }
    
    if(c>p_c)
    {
      max<-temp
    }
    return (max)
  }
  
}

Predictive_Analysis <<-function()
{

  Correlation<<-function()
  {
    n<-length(x)
    sum_x<-0
    sum_y<-0
    sum_x_sq<-0
    sum_y_sq<-0
    sum_xy<-0
    for(i in 1:n)
    {
      sum_x<-sum_x+x[i]
      sum_y<-sum_y+y[i]
      sum_xy<-sum_xy+x[i]*y[i]
      sum_x_sq<-sum_x_sq+x[i]^2
      sum_y_sq<-sum_y_sq+y[i]^2
      
    }
    return((n*sum_xy-sum_x*sum_y)/sqrt((n*sum_x_sq-sum_x^2)*(n*sum_y_sq-sum_y^2)))
  }
  
  Multiple_Linear_Regression<<-function()
  {
    n<-length(x1)
    sum_x1<-0
    sum_x2<-0
    sum_y<-0
    sum_x1_sq<-0
    sum_x2_sq<-0
    sum_x2_y<-0
    sum_x1_y<-0
    sum_x1x2<-0
    for(i in 1:n)
    {
      sum_x1<-sum_x1+x1[i]
      sum_x2<-sum_x2+x2[i]
      sum_y<-sum_y+y[i]
      sum_x1x2<-sum_x1x2+x1[i]*x2[i]
      sum_x1_sq<-sum_x1_sq+x1[i]^2
      sum_x2_sq<-sum_x2_sq+x2[i]^2
      sum_x1_y<-sum_x1_y+x1[i]*y[i]
      sum_x2_y<-sum_x2_y+x2[i]*y[i]
    }
    m1<-matrix(c(n,sum_x1,sum_x2,sum_x1,sum_x1_sq,sum_x1x2,sum_x2,sum_x1x2,sum_x2_sq),nrow=3,ncol=3)
    minv<-solve(m1)		#calculating inverse of matrix m1
    m2<-matrix(c(sum_y,sum_x1_y,sum_x2_y),nrow=3,ncol=1)
    result<-minv%*%m2		#multiplying matrices minv and m2
    return(result)
    
  }
		
}


Probability_Analysis <<- function()
{
	Factorial <<- function(n)
	{	
		fact <- 1

		if(n==0)
		{
		  
		  fact <- 1
		  
		}
		  
		else
		{
		  
		  for(i in (1:n))
		  {
		  	fact <-(fact*i)
	   	}
		
		} 
		
		return(fact)
	}
	
	Permutation <<- function(n,x)
	{	
		permutation <<- Factorial(n)/Factorial(n-x)		
		return(permutation)
	}

	Combination <<- function(n,x)
	{	
		combination <<- Factorial(n)/(Factorial(n-x)*Factorial(x))
		return(combination)
	}


################################################################################################

########			Basic Probability Function		#############
########			__________________________		#############


########		Variables Used:

########				   obs_value   :	observed values of an event	   ######
########				   total_value :  total number of possible cases   #####
########				   prob        :  probability of the event to occur

#################################################################################################


	Probability <<- function()
	{
		obs_value <- as.numeric(readline(prompt = "\nEnter observed value : \n"))
		total_value <- as.numeric(readline(prompt = "\nEnter total value : \n"))
		probab <- obs_value/total_value
		return(probab)
	}

	intersection <<- function(data1,data2)
	{
	  count<-0
	  len_data1<-length(data1)
	  len_data2<-length(data2)
	  
	  for (i in 1:len_data1)
	  {
	    for (j in 1:len_data2)
	    {
	      if (data1[i]==data2[j])
	        count<-count+1
	    }
	  }	
  
	  return(count)
  
	}


	conditional_prob <<- function(data1,data2)
	{
	  data_intersection <- intersection(data1,data2)
	  len_data1 <- length(data1)
	  len_data2 <- length(data2)
	
	  n <- as.numeric(readline(prompt="\nEnter\n 1. to find, P(A|B)\n 2. to find, P(B|A) \t"))
	
	  if(n==1)
	  {
	     prob <- data_intersection / len_data2
	     cat('\n P(A|B):',prob,'\n')
	  }
	
	  else if(n==2)
	  {
	     prob <- data_intersection / len_data1
	     cat('\n P(B|A):',prob,'\n')
	  }
	  else
	     cat("\nError!\n") 
	  
	  return(prob)
	}


	Bayes_Theorem <<- function()
	{
		n <- as.numeric(readline(prompt="Enter the number of Hypothesis: "))
		P_A_H<- data.frame("")
		P_H <- data.frame("")
		P_A_HH <- data.frame("")
		for(i in 1:n)
		{
			P_A_H[i] <- as.numeric(readline(prompt=cat("\nEnter the conditional probability of event A given that hypothesis H",i," has happened: ")))
			P_H[i] <- as.numeric(readline(prompt=cat("\nEnter the probability of hypothesis H",i," : ")))
			P_A_HH[i] <- P_A_H[i]*P_H[i]
		}

		P_A <- 0		
		for(i in 1:n)
		{
			P_A <- P_A + P_A_HH[i]
		}

		i <- as.numeric(readline(prompt="\nEnter the hypothesis who's conditional probability you have to find out given that event A happened: "))
		P_Hi_A <-round((P_A_H[i]*P_H[i])/P_A,6) 
		
		cat("\nThe probability of the event to occur is: ",P_Hi_A,"\n")
		
	}

}

 Discrete_Distribution_Functions <<- function()
{
####################################################################################

##########		Discrete Uniform Distribution
########## 		_____________________________

#######################################################################################


     	 Uniform <<- function()
       {
		   
     	   k <- as.numeric(readline(prompt = "\nEnter the number of variables\t"))
     	   Fx <- 1/k
     	   cat("\nf(x) = ",Fx,"\n")
     	   
     	 }

####################################################################################

##########		Bernoulli  Distribution					#############
########## 		________________________				#############


##########	      Variables Used	:					#############

##########			Q	:	Probability of success		#############

##########			x	:	if 1, success			#############
##########					if 0, failure			#############

#######################################################################################



     	 
     	 Bernoulli <<- function()
     	 {
     	   Q <- as.numeric(readline(prompt = "\nEnter the probability of success\t"))
     	   x <- as.numeric(readline(prompt = c("\nEnter '1', to find the probability of success\nEnter '0', to find the probability of failure\n")))
     	   Fx_Q <- ((Q^x)*(1-Q)^(1-x))
     	   
     	   if(x==1)
     	     cat("\nThe probability of success is : ",Fx_Q,"\n")
     	   else
     	     cat("\nThe probability of failure is : ",Fx_Q,"\n")
     	 }


####################################################################################

##########		Binomial  Distribution					#############
########## 		_______________________					#############


##########	      Variables Used	:					#############

##########			Q	:	Probability of success		#############

##########			n	:	number of trials			##############

##########			x	:	ranges from 0 to n		#############

#######################################################################################

     	 
     	 Binomial <<- function()
     	 {
     	 
     	   Q <- as.numeric(readline(prompt = "\nEnter the probability of success\t"))
     	   x <- as.numeric(readline(prompt = "\nEnter any number from '0 to n', to find the probability of 'x' successes in 'n' trials\t"))
     	   n <- as.numeric(readline(prompt = "\nEnter the number of trials\t"))
     	   
     	   nCx <- Combination(n,x)
     	   
     	   Bx_n_Q <- nCx*((Q^x)*(1-Q)^(n-x))
     	   
     	   cat("\nThe probability of ",x," successes in ",n," trials is ",Bx_n_Q,"\n")
     	   
     	 }
     	 
     	 Geometric <<- function()
     	 {
     	   
     	   Q <- as.numeric(readline(prompt = "\nEnter the probability of success on any given trial\t"))
     	   x <- as.numeric(readline(prompt = "\nEnter the trial (1,2,3...) in which you have to find success\t"))
     	   
     	   Gx_Q <- Q*((1-Q)^(x-1))
     	   
     	   cat("\nThe probability of success in ",x,"th trial is ",Gx_Q,"\n")
     	   
     	 } 
     	 
     	 Hyper_Geometric <<- function()
     	 {
     	   
     	   N <- as.numeric(readline(prompt = "\nEnter the total number of elements\t"))
     	   M <- as.numeric(readline(prompt = "\nEnter the number of successes\t"))
     	   n <- as.numeric(readline(prompt = "\nEnter the number of trials\t"))
     	   x <- as.numeric(readline(prompt = "\nEnter the number of successes (0,1,2,...,n)\t"))
     	   
     	   if(x<=M && (n-x)<=(N-M))
     	   {
     	     
     	     MCx <- Combination(M,x)
     	     N_MCn_x <- Combination((N-M),(n-x))
     	     NCn <- Combination(N,n)
     	     
     	     Hx_n_N_M <- (MCx*N_MCn_x)/NCn
     	     
     	     cat("\nThe probability of ",x," successes in ",n," trials is ",Hx_n_N_M,"\n")
     	     
     	   }
     	   
     	   else
     	   {
     	     
     	     cat("\nError! Check whether x<=M and n-x<=N-M and Re-Enter the values\n")
     	     Hyper_Geometric()
     	     
     	   }
     	 } 
     	 
     	 Negative_Binomial <<- function()
     	 {
     	   
     	   k <- as.numeric(readline(prompt = "\nEnter the number of success\t"))
     	   x <- as.numeric(readline(prompt = "\nEnter the number of trials on which the kth success (x=k+1,k+2,k+3,...)\t"))
     	   Q <- as.numeric(readline(prompt = c("\nEnter the probability of success on xth trial\t")))
     	   xCk <- Combination((x-1),(k-1))
     	   
     	   Bx_k_Q <- xCk*(Q^k)*((1-Q)^(x-k))
     	   
     	   cat("\nThe probability that ",x,"th on which the ",k,"th success will occur is ",Bx_k_Q,"\n")
     	   
     	 }
     	 
     	 Poisson <<- function()
     	 {
     	   n <- as.numeric(readline(prompt = c("\nEnter '1', to find the probability using 'Poisson Parameter'\nEnter '2', to find the probability by first calculating poisson parameter and thereby using it\n")))
     	   
     	   if(n==1)
     	   {
     	     
     	     l <- as.numeric(readline(prompt = "\nEnter the value of Poisson parameter\t"))
     	     x <- as.numeric(readline(prompt = "\nEnter x (0,1,2,...)\t"))
     	     
     	     P_x_l <- (l^x)*exp(-l)/Factorial(x)
     	     
     	     cat("\nThe poisson probability is ",P_x_l,"\n")
     	     
     	   }
     	   
     	   else if (n==2)
     	   {
     	     
     	     n <- as.numeric(readline(prompt = "\nEnter the number of trials\t"))
     	     Q <- as.numeric(readline(prompt = "\nEnter the probability of success\t"))
     	     l <- n*Q
     	     x <- as.numeric(readline(prompt = "\nEnter x (0,1,2,...)\t"))
     	     
     	     P_x_l <- (l^x)*exp(-l)/Factorial(x)
     	     
     	     cat("\nThe poisson probability is ",P_x_l,"\n")
     	     
     	   }
     	   
     	   else
     	   {
     	     
     	     cat("\nError! Re-Enter your choice\n")
     	     Poisson()
     	     
     	   }
     	 }
     	 
     	 Multinomial <<- function()
     	 {
     	   
     	   n <- as.numeric(readline(prompt = "\nEnter the number of trials\t"))
     	   k <- as.numeric(readline(prompt = "\nEnter the number of variables\t"))
     	   x <- array()
     	   Q <- array()
     	   
     	   sum_x <- 0
     	   sum_Q <- 0
     	   denom <- 1
     	   num <- 1
     	   
     	   for (i in 1:k)
     	   {
     	     
     	     x[i] <- as.numeric(readline(prompt = cat("\nEnter variable x",i,"\t")))
     	     sum_x <- sum_x+x[i]
     	     Q[i] <- as.numeric(readline(prompt = cat("\nEnter probability of variable x",i,"\t")))
     	     sum_Q <- sum_Q+Q[i]
     	     denom <- denom*Factorial(x[i])
     	     num <- num*(Q[i]^x[i])
     	     
     	   }
     	   
     	   if(sum_x==n && sum_Q==1)
     	   {
     	     
     	     f_x_n_Q <- Factorial(n)*num/denom
     	     cat("\nThe required probability is: ",f_x_n_Q," \n")
     	     
     	   }
     	   
     	   else
     	     cat("\nError! Check the values!\n")
     	 }
     	 
     	 Multivariate_Hypergeometric <<- function()
     	 {
     	   
     	   N <- as.numeric(readline(prompt = "\nEnter the total numer of elements\t"))
     	   n <- as.numeric(readline(prompt = cat("\nEnter the elements chosen out of ",N," elements\t")))
     	   k <- as.numeric(readline(prompt = "\nEnter the number of variables\t"))
     	   x <- array()
     	   M <- array()
     	   
     	   sum_x <- 0
     	   sum_M <- 0
     	   denom <- 1
     	   num <- 1
     	   
     	   for (i in 1:k)
     	   {
     	     
     	     M[i] <- as.numeric(readline(prompt = cat("\nEnter total number of elements of ",i," kind\t")))
     	     sum_M <- sum_M+M[i]
     	     x[i] <- as.numeric(readline(prompt = cat("\nEnter elements chosen of ",i," kind\t")))
     	     sum_x <- sum_x+x[i]
     	     num <- num*Combination(M[i],x[i])
     	     
     	   }
     	   
     	   if(sum_x==n && sum_M==N)
     	   {
     	     
     	     f_x_n_M <- num/Combination(N,n)
     	     cat("\nThe required probability is: ",f_x_n_M," \n")
     	     
     	   }
     	   
     	   else
     	     cat("\nError! Check the values!\n")
     	   
     	 }
 }     
 
 Continuous_Distribution <<- function()
 {
   
   Uniform <<- function()
   {
     
     a <- as.numeric(readline(prompt = "\nEnter the infimum of the interval\t"))
     b <- as.numeric(readline(prompt = "\nEnter the supremum of the interval\t"))
     x <- as.numeric(readline(prompt = "\nEnter a random variable\t"))
     
     if(x>=a || x<=b)
     {
       
       u_x_a_b <- 1/(b-a)
       cat("\nThe required probability is: ",u_x_a_b,"\n")
       
     }
     
     else
       cat("\nThe required probability is: ",0,"\n")
     
   }
   
   Normal <<- function()
   {
     
     i <- as.numeric(readline(prompt = "\nEnter 1, to find normal distribution\nEnter 2, to find standard normal distribution\t "))
     
     if(i==1)
     {
       
       Mean <- as.numeric(readline(prompt = "\nEnter the mean of the data\t"))
       S.D <- as.numeric(readline(prompt = "\nEnter the standard deviation of the data\t"))
       x <- as.numeric(readline(prompt = "\nEnter a random variable\t"))
       
       integrand <- function(x)
       {
         
         z <- ((x-Mean)/S.D)^2
         n_x_M_S.D <- exp(-z/2)/(S.D*sqrt(2*pi))
       
       }
       
       c.d.f <- integrate(integrand,lower = -Inf,upper = x)
       
       cat("\nThe required probability is ",n_x_M_S.D,"\n")
       cat("The required cumulative distribution is ",c.d.f$value,"\n")
       
     }
     
     else if(i==2)
     {
       
       # Mean = 0
       # Standard deviation = 1
       
       x <- as.numeric(readline(prompt = "\nEnter a random variable\t"))
       
       integrand <- function(x)
       {
         
       n_x_M_S.D <- exp(-(x^2)/2)/sqrt(2*pi)
       
       }
      
       c.d.f <- integrate(integrand,lower = -Inf,upper = x)
       
       cat("\nThe required probability is ",n_x_M_S.D,"\n")
       cat("The required cumulative distribution is ",c.d.f$value,"\n")
       
     }
     
     else
       
       cat("\nError! \n ")
       
   }
   
   Bivariate_Normal <<- function()
   {
     
     x <- as.numeric(readline(prompt = "\nEnter 1st random variable\t"))
     mean_x <- as.numeric(readline(prompt = "\nEnter the mean of 1st variable\t"))
     S.D_x <- as.numeric(readline(prompt = "\nEnter the standard deviation of 1st variable\t"))
     
     y <- as.numeric(readline(prompt = "\nEnter 2nd random variable\t"))
     mean_y <- as.numeric(readline(prompt = "\nEnter the mean of 2nd variable\t"))
     S.D_y <- as.numeric(readline(prompt = "\nEnter the standard deviation of 2nd variable\t"))
     
     corr_coef <- as.numeric(readline(prompt = "\nEnter the correlation coefficient of the two variables\t"))

     if(S.D_x > 0 && S.D_y > 0 && corr_coef > (-1) && corr_coef < 1)
     {
       
       z_x <- ((x-mean_x)/S.D_x)^2
       z_y <- ((y-mean_y)/S.D_y)^2
       z_xy <- 2*corr_coef*((x-mean_x)/S.D_x)*((y-mean_y)/S.D_y)
     
       f_x_y <- exp(-(z_x - z_xy + z_y)/(2*(1-corr_coef^2)))/(2*pi*S.D_x*S.D_y*sqrt(1-corr_coef^2))
       
       cat("\nThe required probability is ",f_x_y,"\n")
     
     }
     
     else
       cat("\nError!\n")
     
   }
   
   
   Gamma_func <<- function()
   {
     
     a <- as.numeric(readline(prompt = "\nEnter the parameter a:\t"))
     b <- as.numeric(readline(prompt = "\nEnter the parameter b:\t"))
     x <- as.numeric(readline(prompt = "\nEnter a random variable:\t"))
     
     if(x>0)
     {
       if(a>0 && b>0)
       {
         
         g_x_a_b <- ((x^(a-1))*exp(-x/b))/((b^a)*gamma(a))
         cat("\nThe required probability is: ",g_x_a_b,"\n")
         
       }
       
       else
        cat("\nError!!\n")
       
     }
     
     else
       cat("\nThe required probability is: ",0,"\n")
     
   }
   
   
   Exponential <<- function()
   {
     
     Q <- as.numeric(readline(prompt = "\nEnter the p b:\t"))
     x <- as.numeric(readline(prompt = "\nEnter a random variable:\t"))
     
     if(x>0)
     {
       if(Q>0)
       {
         
         g_x_Q <- exp(-x/Q)/Q
         cat("\nThe required probability is: ",g_x_Q,"\n")
         
       }
       
       else
         cat("\nError!!\n")
       
     }
     
     else
       cat("\nThe required probability is: ",0,"\n")
     
     
   }
   
 }
 
 Sample_Distribution_Test <<- function()
 {
   
   Chi_Square <<- function()
   {
  
     x <- as.numeric(readline(prompt = "\nEnter the parameter (alpha):\t"))
     n <- as.numeric(readline(prompt = "\nEnter the number of degree of freedom :\t"))
     
     chi_table <- read.table("Chi_Square_Table.csv",header=TRUE,sep=",")
     colnames(chi_table)=c(0.2,0.1,0.075,0.05,0.025,0.01,0.005,0.001,0.0005)
     
     n <- as.character(n)
     x <- as.character(x)
     
     cat("\nThe required value is :",chi_table[n,x],"\n")
     return(chi_table[n,x])
     
   }
   
   t_test <<- function(n1)
   {
     n <- (n1-1)
     i <- as.numeric(readline(prompt = "\nEnter\n 1. when alternative hypothesis x!=x0\n2.when alternative hypothesis x>x0\n3.when alternative hypothesis x<x0\t"))
     
     if(i==1)
     {
       t1 <- as.numeric(readline(prompt = "\nEnter the parameter (alpha):\t"))
       t<-t1/2
     }
     
     else
      t <- as.numeric(readline(prompt = "\nEnter the parameter (alpha):\t"))
     
     
     t_table <- read.csv("T_Table.csv",header=FALSE,sep = ",")
     colnames(t_table) <- c(0.40,0.25,0.10,0.05,0.04,0.025,0.02,0.01,0.005,0.0025,0.001,0.0005)
     rownames(t_table) <- c(1:40,60,80,100,120,140,160,180,200,250,'inf')
     
     
     
     t <- as.character(t)
     
     if(n>40 && n<60)
     {
       n <- 60
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>60 && n<80)
     {
       n <- 80
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>80 && n<100)
     {
       n <- 100
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>100 && n<120)
     {
       n <- 120
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>120 && n<140)
     {
       n <- 140
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>140 && n<160)
     {
       n <- 160
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>160 && n<180)
     {
       n <- 180
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>180 && n<200)
     {
       n <- 200
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>200 && n<250)
     {
       n <- 250
       n <- as.character(n)
       return(t_table[n,t])
     }
     
     else if(n>250)
     {
       n <-"inf"
       return(t_table[n,t])
     }
     
     else
     {
       n <- as.character(n)
       return(t_table[n,t])
     }
   }
   
   
   F_test <<- function(df1,df2)
   {
       f_table <- read.csv("F_Table.csv",header=TRUE,sep = ",")
       colnames(f_table) <- c(1:10,12,15,20,24,30,40,60,120,'inf')
       rownames(f_table) <- c(1:30,40,60,120,'inf')
       
       if(df2>30 && df2<40)
       {
         df2 <- 40
         df2 <- as.character(df2)
       
         if(df1==11)
         {
           df1 <- 12
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1==13 && df1==14)
         {
           df1 <- 15
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>15 && df1<20)
         {
           df1 <- 20
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>20 && df1<24)
         {
           df1 <- 24
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>24 && df1<30)
         {
           df1 <- 30
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>30 && df1<40)
         {
           df1 <- 40
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>40 && df1<60)
         {
           df1 <- 60
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>60 && df1<120)
         {
           df1 <- 120
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>120)
         {
           df1 <-"inf"
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else
         {
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
        
       }    
       
       else if (df2>40 && df2<60)
       {
         df2 <- 60
         df2 <- as.character(df2)
         
         if(df1==11)
         {
           df1 <- 12
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1==13 && df1==14)
         {
           df1 <- 15
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>15 && df1<20)
         {
           df1 <- 20
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>20 && df1<24)
         {
           df1 <- 24
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>24 && df1<30)
         {
           df1 <- 30
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>30 && df1<40)
         {
           df1 <- 40
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>40 && df1<60)
         {
           df1 <- 60
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>60 && df1<120)
         {
           df1 <- 120
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>120)
         {
           df1 <-"inf"
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else
         {
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
       } 
         
       else if (df2>60 && df2<120)
       {
         df2 <- 120
         df2 <- as.character(df2)
         
         if(df1==11)
         {
           df1 <- 12
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1==13 && df1==14)
         {
           df1 <- 15
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>15 && df1<20)
         {
           df1 <- 20
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>20 && df1<24)
         {
           df1 <- 24
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>24 && df1<30)
         {
           df1 <- 30
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>30 && df1<40)
         {
           df1 <- 40
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>40 && df1<60)
         {
           df1 <- 60
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>60 && df1<120)
         {
           df1 <- 120
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>120)
         {
           df1 <-"inf"
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else
         {
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
       } 
       
       else if (df2>120)
       {
         df2 <- "inf"
         
         if(df1==11)
         {
           df1 <- 12
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1==13 && df1==14)
         {
           df1 <- 15
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>15 && df1<20)
         {
           df1 <- 20
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>20 && df1<24)
         {
           df1 <- 24
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>24 && df1<30)
         {
           df1 <- 30
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>30 && df1<40)
         {
           df1 <- 40
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>40 && df1<60)
         {
           df1 <- 60
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>60 && df1<120)
         {
           df1 <- 120
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>120)
         {
           df1 <-"inf"
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else
         {
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
       } 
       
       else
       {
         df2 <- as.character(df2)
         
         if(df1==11)
         {
           df1 <- 12
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1==13 && df1==14)
         {
           df1 <- 15
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>15 && df1<20)
         {
           df1 <- 20
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>20 && df1<24)
         {
           df1 <- 24
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>24 && df1<30)
         {
           df1 <- 30
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>30 && df1<40)
         {
           df1 <- 40
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>40 && df1<60)
         {
           df1 <- 60
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>60 && df1<120)
         {
           df1 <- 120
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else if(df1>120)
         {
           df1 <-"inf"
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
         else
         {
           df1 <- as.character(df1)
           cat("\nThe required probability density is ",f_table[df2,df1],"\n")
           return(f_table[df2,df1])
         }
         
       } 
       
    
         
   }
   
   
   z_test <<- function()
   {
     
     z_neg <- c(0.0002, 0.0003, 0.0003, 0.0003, 0.0003, 0.0003, 0.0003, 0.0003, 0.0003, 0.0003, 0.0003, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0005, 0.0005, 0.0005, 0.0005, 0.0005, 0.0005, 0.0006, 0.0006, 0.0006, 0.0006, 0.0006, 0.0007, 0.0007, 0.0007, 0.0007, 0.0008, 0.0008, 0.0008, 0.0008, 0.0009, 0.0009, 0.0009, 0.0010, 0.0010, 0.0010, 0.0011, 0.0011, 0.0011, 0.0012, 0.0012, 0.0013, 0.0013, 0.0013, 0.0014, 0.0014, 0.0015, 0.0015, 0.0016, 0.0016, 0.0017, 0.0018, 0.0018, 0.0019, 0.0019, 0.0020, 0.0021, 0.0021,
                0.0022, 0.0023, 0.0023, 0.0024, 0.0025, 0.0026, 0.0026, 0.0027, 0.0028, 0.0029, 0.0030, 0.0031, 0.0032, 0.0033, 0.0034, 0.0035, 0.0036, 0.0037, 0.0038, 0.0039, 0.0040, 0.0041, 0.0043, 0.0044, 0.0045, 0.0047, 0.0048, 0.0049, 0.0051, 0.0052, 0.0054, 0.0055, 0.0057, 0.0059, 0.0060, 0.0062, 0.0064, 0.0066, 0.0068, 0.0069, 0.0071, 0.0073, 0.0075, 0.0078, 0.0080, 0.0082, 0.0084, 0.0087, 0.0089, 0.0091, 0.0094, 0.0096, 0.0099, 0.0102, 0.0104, 0.0107, 0.0110, 0.0113, 0.0116, 0.0119, 0.0122, 0.0125, 0.0129, 0.0132,
                0.0136, 0.0139, 0.0143, 0.0146, 0.0150, 0.0154, 0.0158, 0.0162, 0.0166, 0.0170, 0.0174, 0.0179, 0.0183, 0.0188, 0.0192, 0.0197, 0.0202, 0.0207, 0.0212, 0.0217, 0.0222, 0.0228, 0.0233, 0.0239, 0.0244, 0.0250, 0.0256, 0.0262, 0.0268, 0.0274, 0.0281, 0.0287, 0.0294, 0.0301, 0.0307, 0.0314, 0.0322, 0.0329, 0.0336, 0.0344, 0.0351, 0.0359, 0.0367, 0.0375, 0.0384, 0.0392, 0.0401, 0.0409, 0.0418, 0.0427, 0.0436, 0.0446, 0.0455, 0.0465, 0.0475, 0.0485, 0.0495, 0.0505, 0.0516, 0.0526, 0.0537, 0.0548, 0.0559, 0.0571,
                0.0582, 0.0594, 0.0606, 0.0618, 0.0630, 0.0643, 0.0655, 0.0668, 0.0681, 0.0694, 0.0708, 0.0721, 0.0735, 0.0749, 0.0764, 0.0778, 0.0793, 0.0808, 0.0823, 0.0838, 0.0853, 0.0869, 0.0885, 0.0901, 0.0918, 0.0934, 0.0951, 0.0968, 0.0985, 0.1003, 0.1020, 0.1038, 0.1056, 0.1075, 0.1093, 0.1112, 0.1131, 0.1151, 0.1170, 0.1190, 0.1210, 0.1230, 0.1251, 0.1271, 0.1292, 0.1314, 0.1335, 0.1357, 0.1379, 0.1401, 0.1423, 0.1446, 0.1469, 0.1492, 0.1515, 0.1539, 0.1562, 0.1587, 0.1611, 0.1635, 0.1660, 0.1685, 0.1711, 0.1736,
                0.1762, 0.1788, 0.1814, 0.1841, 0.1867, 0.1894, 0.1922, 0.1949, 0.1977, 0.2005, 0.2033, 0.2061, 0.2090, 0.2119, 0.2148, 0.2177, 0.2206, 0.2236, 0.2266, 0.2296, 0.2327, 0.2358, 0.2389, 0.2420, 0.2451, 0.2483, 0.2514, 0.2546, 0.2578, 0.2611, 0.2643, 0.2676, 0.2709, 0.2743, 0.2776, 0.2810, 0.2843, 0.2877, 0.2912, 0.2946, 0.2981, 0.3015, 0.3050, 0.3085, 0.3121, 0.3156, 0.3192, 0.3228, 0.3264, 0.3300, 0.3336, 0.3372, 0.3409, 0.3446, 0.3483, 0.3520, 0.3557, 0.3594, 0.3632, 0.3669, 0.3707, 0.3745, 0.3783, 0.3821,
                0.3859, 0.3897, 0.3936, 0.3974, 0.4013, 0.4052, 0.4090, 0.4129, 0.4168, 0.4207, 0.4247, 0.4286, 0.4325, 0.4364, 0.4404, 0.4443, 0.4483, 0.4522, 0.4562, 0.4602, 0.4641, 0.4681, 0.4721, 0.4761, 0.4801, 0.4840, 0.4880, 0.4920, 0.4960, 0.5000)
     z_pos <- c(0.5000, 0.5040, 0.5080, 0.5120, 0.5160, 0.5199, 0.5239, 0.5279, 0.5319, 0.5359, 0.5398, 0.5438, 0.5478, 0.5517, 0.5557, 0.5596, 0.5636, 0.5675, 0.5714, 0.5753, 0.5793, 0.5832, 0.5871, 0.5910, 0.5948, 0.5987, 0.6026, 0.6064, 0.6103, 0.6141, 0.6179, 0.6217, 0.6255, 0.6293,
                0.6331, 0.6368, 0.6406, 0.6443, 0.6480, 0.6517, 0.6554, 0.6591, 0.6628, 0.6664, 0.6700, 0.6736, 0.6772, 0.6808, 0.6844, 0.6879, 0.6915, 0.6950, 0.6985, 0.7019, 0.7054, 0.7088, 0.7123, 0.7157, 0.7190, 0.7224, 0.7257, 0.7291, 0.7324, 0.7357, 0.7389, 0.7422, 0.7454, 0.7486, 0.7517, 0.7549, 0.7580, 0.7611, 0.7642, 0.7673, 0.7704, 0.7734, 0.7764, 0.7794, 0.7823, 0.7852, 0.7881, 0.7910, 0.7939, 0.7967, 0.7995, 0.8023, 0.8051, 0.8078, 0.8106, 0.8133, 0.8159, 0.8186, 0.8212, 0.8238, 0.8264, 0.8289, 0.8315, 0.8340,
                0.8365, 0.8389, 0.8413, 0.8438, 0.8461, 0.8485, 0.8508, 0.8531, 0.8554, 0.8577, 0.8599, 0.8621, 0.8643, 0.8665, 0.8686, 0.8708, 0.8729, 0.8749, 0.8770, 0.8790, 0.8810, 0.8830, 0.8849, 0.8869, 0.8888, 0.8907, 0.8925, 0.8944, 0.8962, 0.8980, 0.8997, 0.9015, 0.9032, 0.9049, 0.9066, 0.9082, 0.9099, 0.9115, 0.9131, 0.9147, 0.9162, 0.9177, 0.9192, 0.9207, 0.9222, 0.9236, 0.9251, 0.9265, 0.9279, 0.9292, 0.9306, 0.9319, 0.9332, 0.9345, 0.9357, 0.9370, 0.9382, 0.9394, 0.9406, 0.9418, 0.9429, 0.9441, 0.9452, 0.9463,
                0.9474, 0.9484, 0.9495, 0.9505, 0.9515, 0.9525, 0.9535, 0.9545, 0.9554, 0.9564, 0.9573, 0.9582, 0.9591, 0.9599, 0.9608, 0.9616, 0.9625, 0.9633, 0.9641, 0.9649, 0.9656, 0.9664, 0.9671, 0.9678, 0.9686, 0.9693, 0.9699, 0.9706, 0.9713, 0.9719, 0.9726, 0.9732, 0.9738, 0.9744, 0.9750, 0.9756, 0.9761, 0.9767, 0.9772, 0.9778, 0.9783, 0.9788, 0.9793, 0.9798, 0.9803, 0.9808, 0.9812, 0.9817, 0.9821, 0.9826, 0.9830, 0.9834, 0.9838, 0.9842, 0.9846, 0.9850, 0.9854, 0.9857, 0.9861, 0.9864, 0.9868, 0.9871, 0.9875, 0.9878,
                0.9881, 0.9884, 0.9887, 0.9890, 0.9893, 0.9896, 0.9898, 0.9901, 0.9904, 0.9906, 0.9909, 0.9911, 0.9913, 0.9916, 0.9918, 0.9920, 0.9922, 0.9925, 0.9927, 0.9929, 0.9931, 0.9932, 0.9934, 0.9936, 0.9938, 0.9940, 0.9941, 0.9943, 0.9945, 0.9946, 0.9948, 0.9949, 0.9951, 0.9952, 0.9953, 0.9955, 0.9956, 0.9957, 0.9959, 0.9960, 0.9961, 0.9962, 0.9963, 0.9964, 0.9965, 0.9966, 0.9967, 0.9968, 0.9969, 0.9970, 0.9971, 0.9972, 0.9973, 0.9974, 0.9974, 0.9975, 0.9976, 0.9977, 0.9977, 0.9978, 0.9979, 0.9979, 0.9980, 0.9981,
                0.9981, 0.9982, 0.9982, 0.9983, 0.9984, 0.9984, 0.9985, 0.9985, 0.9986, 0.9986, 0.9987, 0.9987, 0.9987, 0.9988, 0.9988, 0.9989, 0.9989, 0.9989, 0.9990, 0.9990, 0.9990, 0.9991, 0.9991, 0.9991, 0.9992, 0.9992, 0.9992, 0.9992, 0.9993, 0.9993, 0.9993, 0.9993, 0.9994, 0.9994, 0.9994, 0.9994, 0.9994, 0.9995, 0.9995, 0.9995, 0.9995, 0.9995, 0.9995, 0.9996, 0.9996, 0.9996, 0.9996, 0.9996, 0.9996, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9998) 
     
     dim(z_neg) <- c(10,35)
     dim(z_pos) <- c(10,35)
     
     rownames(z_neg) <- c("0.09","0.08","0.07","0.06","0.05","0.04","0.03","0.02","0.01","0.00")
     rownames(z_pos) <- c("0.00","0.01","0.02","0.03","0.04","0.05","0.06","0.07","0.08","0.09")
     
     colnames(z_neg) <- c("-3.4","-3.3","-3.2","-3.1","-3.0","-2.9","-2.8","-2.7","-2.6","-2.5",
                          "-2.4","-2.3","-2.2","-2.1","-2.0","-1.9","-1.8","-1.7","-1.6","-1.5",
                          "-1.4","-1.3","-1.2","-1.1","-1.0","-0.9","-0.8","-0.7","-0.6","-0.5",
                          "-0.4","-0.3","-0.2","-0.1","-0.0")
     colnames(z_pos) <- c("0.0","0.1","0.2","0.3","0.4",
                          "0.5","0.6","0.7","0.8","0.9","1.0","1.1","1.2","1.3","1.4",
                          "1.5","1.6","1.7","1.8","1.9","2.0","2.1","2.2","2.3","2.4",
                          "2.5","2.6","2.7","2.8","2.9","3.0","3.1","3.2","3.3","3.4")
     
     
     alpha <- as.numeric(readline(prompt = "\nEnter the level of significance:\t"))
      
     
     z <-(trunc(alpha*10)/10)
     
     if(alpha<0.00)
     { 
	 if(z==-0.0)
	 {
	 	 z_0.0i <-(alpha-z)

		 if(z_0.0i==0.00)
		   return(z_neg["0.00","-0.0"])
		 else
		 {
			z_0.0i <- as.character(z_0.0i)
			return(z_neg[z_0.0i,"-0.0"])	
     }
	 }

	 else if(z==(-1.0))
	 {
	 	 z_0.0i <-(alpha-z)

		 if(z_0.0i==0.00)
		   return(z_neg["0.00","-1.0"])
		 else
		 {
			z_0.0i <- as.character(z_0.0i)
			return(z_neg[z_0.0i,"-1.0"])	
     }

	 }

	 else if(z==-2.0)
	 {
	 	 z_0.0i <-(alpha-z)

		 if(z_0.0i==0.00)
		   return(z_neg["0.00","-2.0"])
		 else
		 {
			z_0.0i <- as.character(z_0.0i)
			return(z_neg[z_0.0i,"-2.0"])	
     }

	 }

	 else if(z==-3.0)
	 {
		 z_0.0i <-(alpha-z)

		 if(z_0.0i==0.00)
		   return(z_neg["0.00","-3.0"])
		 else
		 {
			z_0.0i <- as.character(z_0.0i)
			return(z_neg[z_0.0i,"-3.0"])	
		 }

     		
	 }

	 else
	 {
       	z_0.0i <-(alpha-z)
	    	z <- as.character(z)
       	
		 if(z_0.0i==0.00)
		   return(z_neg["0.00",z])
	 	
		 else
		 {
			z_0.0i <- as.character(z_0.0i)
			return(z_neg[z_0.0i,z])	
     }

	 }
  }

  else
  {
	    if(z==0.0)
	    {
	 	    z_0.0i <-(alpha-z)

		    if(z_0.0i==0.00)
		      return(z_pos["0.00","0.0"])
	 	    
		    else
		    {
			    z_0.0i <- as.character(z_0.0i)
			    return(z_pos[z_0.0i,"0.0"])	
     	  }
	    }
    
    	 else if(z==1.0)
    	 {
    	 	 z_0.0i <-(alpha-z)
    
    		 if(z_0.0i==0.00)
    		   return(z_pos["0.00","1.0"])
    	 	 
    		 else
    		 {
    			z_0.0i <- as.character(z_0.0i)
    			return(z_pos[z_0.0i,"1.0"])	
         }
    
    	 }
    
    	 else if(z==2.0)
    	 {
    	 	 z_0.0i <-(alpha-z)
    
    		 if(z_0.0i==0.00)
    		   return(z_pos["0.00","2.0"])
    		 else
    		 {
    			z_0.0i <- as.character(z_0.0i)
    			return(z_pos[z_0.0i,"2.0"])	
      	 }
    
    	 }
    
    	 else if(z==3.0)
    	 {
    		 z_0.0i <-(alpha-z)
    
    		 if(z_0.0i==0.00)
    		   return(z_pos["0.00","3.0"])
    		 else
    		 {
    			z_0.0i <- as.character(z_0.0i)
    			return(z_pos[z_0.0i,"3.0"])	
         }
    
         		
    	 }
    
    	 else
    	 {
           	z_0.0i <-(alpha-z)
    	 	    z <- as.character(z)
           	
		     if(z_0.0i==0.00)
		       return(z_pos["0.00",z])
		     else
		     {
		      	z_0.0i <- as.character(z_0.0i)
			      return(z_pos[z_0.0i,z])	
     		 }

       }
     }
      
   }

 }
 
 Interval_Estimation <<- function()
 {
   
       mod<-function(a)
       {
         if(a<0)	#a is negative
         {
           return (-a)
         }
             else	#a is positive
             {
               return (a)
             }
        }
           
           
           
      Estimation_Mean <<-function()
      {
         z<-mod(qnorm(alpha/2))	#calculating mod
         error<-z*S_D_1/sqrt(number_1)
         lower_limit<-Mean_1-error
         upper_limit<-Mean_1+error
         return (c(lower_limit,upper_limit))	
      }
           
      Estimate_Difference_Mean <<- function()
      {
             z<-mod(qnorm(alpha/2)) #calculating mod
             product<-z*sqrt((S_D_1^2)/number_1+(S_D_2^2)/number_2)
             difference_mean<-Mean_1-Mean_2
             lower_limit<-difference_mean-product
             upper_limit<-difference_mean+product
             return(c(lower_limit,upper_limit))
      }
       
       Estimation_Proportions <<- function()
       {
         z<-mod(qnorm(alpha/2)) #calculating mod
         theta<-x/number_1
         product<-z*sqrt(theta*(1-theta)/number_1)
         lower_limit<-theta-product
         upper_limit<-theta+product
         return(c(lower_limit,upper_limit))
       }
       
       Estimation_Difference_Proportions <<-function()
       {
         z<-mod(qnorm(alpha/2))		#calculating mod
         theta1<-x1/number_1
         theta2<-x2/number_2
         difference<-theta1-theta2
         product<-z*sqrt(theta1*(1-theta1)/number_1+theta2*(1-theta2)/number_2)
         lower_limit<-difference-product
         upper_limit<-difference+product
         return(c(lower_limit,upper_limit))
       }
       
       Estimation_Variance <<- function()
       {
         Chi_sq_1<-qchisq(1-(alpha/2),number-1)
         
         Chi_sq_2<-qchisq((alpha/2),number-1)
         lower_limit<-(number-1)*(S_D^2)/Chi_sq_1
         upper_limit<-(number-1)*(S_D^2)/Chi_sq_2
         return(c(lower_limit,upper_limit))
       }
       
       Estimation_Ratio_Variance <<- function()
       {
         f_1<-qf(alpha/2,number_1-1,number_2-1)
         f_2<-qf(alpha/2,number_2-1,number_1-1)
         ratio_s_d<-(S_D_1^2)/(S_D_2^2)
         upper_limit<-(ratio_s_d/f_1)
         lower_limit<-(ratio_s_d*f_2)
         return(c(lower_limit,upper_limit))
       }
       
       
       
       
   
 }
 
 Non_Parametric <<- function()
 {
   
   mod<<-function(a)
{
	if(a<0)
	{
		return(-a)
	}
	else
	{
		return(a)
	}
}

Count<<-function()
{
	count_pos<-0
	for(i in 1:length(data1))
	{
		if(data1[i]>null)
		{
			count_pos<-count_pos+1
		}
	}
	return(count_pos)
}
Mod_Sort<<-function(coll,col_length)
{
	for(i in 1:col_length)
	{
		small<-Mod(coll[i])
		index<-i
		
		for(j in i:col_length)
		{
			if(Mod(coll[j])<small)
			{
				small<-Mod(coll[j])
				index<-j
			}
			else
			{
				
			}

		}
			
		
		if(i!=index)
		{
			temp<-coll[i]
			coll[i]<-coll[index]
			coll[index]<-temp
		}
	}
	return (coll)
}




Sign_Test<<-function(less)
{
	n<-length(data1)
	theta<-0.5
	x<-Count()
	cal_z<-(x-n*theta)/sqrt(n*theta*(1-theta))
	ob_z<-qnorm(alpha)
	if(less)
	{
		if(cal_z<ob_z)
		{
			cat('Null hypothesis is rejected')
		}
		else
		{
			cat('Null hypothesis is accepted')
		}
	}
	else if(less==0)
	{
		if(cal_z<ob_z)
		{
			cat('Null hypothesis is accepted')
		}
		else
		{
			cat('Null hypothesis is rejected')
		}
	}


}

Rank_matric<<-function(array,s)
{
	r_m<-matrix(0,nrow=s,ncol=2)
	count<-1
	for(i in 1:s)
	{	
		r_m[i,1]=array[i]
		r_m[i,2]<-i	
	}
	i<-1
	f<-1
	while(i<s)
	{
		count<-1
		if(array[i]==array[i+1])
		{
			f<-i
		}
		while((array[i]==array[i+1])&(i<s))
		{
			i<-i+1
			count<-count+1
		}
		if(count>1)
		{
			l<-i
			r<-count/2+0.5
			for(j in f:l)
			{
				r_m[j,2]<-f-1+r
			}
		}
		i<-i+1
	}
	return(r_m)
}
	


Wilcoxon_Signed_Rank_Test<<-function(condition)
{
	n<-length(data1)
	difference<-matrix(0,nrow=1,ncol=n)
	mod_difference<-matrix(0,nrow=1,ncol=n)
	count<-1
	for(i in 1:n)
	{
		if((data1[i]-null)!=0)
		{
			difference[count]<-data1[i]-null
			count<-count+1
		}
			
	}
	count<-count-1
	sorted_difference<-Mod_Sort(difference,count)
	pos_rank_sum<-0
	neg_rank_sum<-0
	for(i in 1:count)
	{
		if(sorted_difference[i]>0)
		{
			pos_rank_sum<-pos_rank_sum+i
		}
		else
		{ 
			if(sorted_difference[i]<0)
			{
			neg_rank_sum<-neg_rank_sum+i	
			}
		}
	
	}
	
	if(pos_rank_sum < neg_rank_sum)
	{
		cal_t<-pos_rank_sum
	}
	else if(pos_rank_sum>neg_rank_sum) 
	{  
		cat('67')	
		cal_t<-neg_rank_sum
	}

	switch(condition,
	
	"1"=
		{
			t<-qsignrank(alpha,n)-1
		},

	"2"=
		{
			t<-qsignrank(alpha,n)-1

		},	
	"3"=
		{
			t<-qsignrank(alpha/2,n)-1


			#t<-2alpha
			#t<-21
		}
	)


		if(t<cal_t)
			{
				cat('\nNULL HYPOTHESIS IS REJECTED')
			}
			else
			{
				cat('\nNULL HYPOTHESIS IS ACCEPTED')
			}
	cat('\n\n',cal_t,'\t',t)
		

}
Rank<<-function(array,el)
{
	s<-length(array)/2
	for(i in 1:s)
	{
		if(array[i,1]==el)
		{
			return(array[i,2])
		}
		else
		{

		}
	}
}
	

Mann_Whitney_Test<<-function(condition)
{
	n1<-length(data1)
	n2<-length(data2)
	rank_data1_sum<-0
	rank_data2_sum<-0
	data1_2<-matrix(0,nrow=1,ncol=n1+n2)
	for(i in 1:n1)
	{
		data1_2[i]<-data1[i]
	}
	i<-i+1
	for(j in 1:n2)
	{
		data1_2[i]<-data2[j]
		i<-i+1
	}
	i<-i-1
	sorted_data1_2<-Mod_Sort(data1_2,i)
	r_m<-Rank_matric(sorted_data1_2,n1+n2)
	if(condition==1)
	{
			rank_data1_sum<-0
			for(i in 1:n1)
			{
				rank_data1_sum<-rank_data1_sum+Rank(r_m,data1[i])
			}
			cal_u<-rank_data1_sum-n1*(n1+1)/2
			u<-qwilcox(alpha,n1,n2)-1
	}
		
	else if(condition==2)
	{
			rank_data2_sum<-0
			for(i in 1:n2)
			{
				rank_data2_sum<-rank_data2_sum+Rank(r_m,data2[i])
			}
			cal_u<-rank_data2_sum-n2*(n2+1)/2
				u<-qwilcox(alpha,n1,n2)-1
		#u2alpha
	}
	else 
	{
		u<-qwilcox(alpha/2,n1,n2)-1
		#u<-24	#ualpha
	}
	if(cal_u<=u)
	{
		cat('NULL HYPOTHESIS IS REJECTED')
	}
	else
	{
		cat('NULL HYPOTHESIS IS ACCEPTED')
	}
					
}
	

Kruskal_Wallis_Test<<-function()
{
	n1<-length(data1)
	n2<-length(data2)
	n3<-length(data3)
	s<-n1+n2+n3
	data1_2_3<-matrix(0,nrow=1,ncol=n1+n2+n3)
	for(i in 1:n1)
	{
		data1_2_3[i]<-data1[i]
	}
	i<-i+1
	for(j in 1:n2)
	{
		data1_2_3[i]<-data2[j]
		i<-i+1
	}
	for(k in 1:n3)
	{
		data1_2_3[i]<-data3[k]
		i<-i+1
	}

	i<-i-1
	sorted_data1_2_3<-Mod_Sort(data1_2_3,i)
	r_m<-Rank_matric(sorted_data1_2_3,s)
	rank_data1_sum<-0
	for(i in 1:n1)
			{
				rank_data1_sum<-rank_data1_sum+Rank(r_m,data1[i])

			}
	rank_data2_sum<-0
	for(i in 1:n2)
			{
				rank_data2_sum<-rank_data2_sum+Rank(r_m,data2[i])

			}


	rank_data3_sum<-0
	for(i in 1:n3)
			{
				rank_data3_sum<-rank_data3_sum+Rank(r_m,data3[i])

			}

	product<-(rank_data1_sum^2)/n1+(rank_data2_sum^2)/n2+(rank_data3_sum^2)/n3
	cal<-(product*12)/(s*(s+1))
	cal_h<-cal-3*(s+1)

	h=qchisq((1-alpha),2)
	 #chisqr0.005,2
	if(cal_h>=h)
	{
		cat('\nNULL HYPOTHESIS IS REJECTED')
	}
	
}
  
   
 }
 
 
 Visualization <<- function()
 {
   
   Histograms <<- function()
   {
     hist(coll,xlab = "Weight",col = "green",border = "red", xlim = c(0,20), ylim = c(0,70),
                breaks = 5)
     
   }
   
   Line_Graph <<- function()
   {
     
     plot(coll,type = "o",col = "red", xlab = "VALUE", ylab = "MPG", 
          main = "IRIS Sepal Length")
     lines(coll, type = "o", col = "blue")
     
   }
   
   Bar_Graph <<- function()
   {
     
     barplot(coll,names.arg = y,xlab = "Sepal Length",ylab = "",col = "blue",
             main = "Length chart",border = "red")
     
   }
     
   
   Pie_Chart <<- function()
   {
     
     x <- c(15, 35, 40, 10)
     pie(x,coll, main = "Length pie chart", col = rainbow(length(x)),radius = 1)
   }
   
   Scatter_plot <<- function()
   {
     
     plot(x = iris$Sepal.Length,y = iris$Petal.Length, xlab = "Sepal Length",ylab = "Petal Length",xlim = c(0,10),ylim = c(0,8),main = "Sepal Length vs Petal Length")
          
   }
   
   Box_plot <<- function()
   {  
     x1 <- iris
     boxplot(Sepal.Length ~ Petal.Length, data = x1, 
             xlab = "Sepal Length",
             ylab = "Petal Length", 
             main = "Iris Data",
             notch = FALSE, 
             varwidth = TRUE, 
             col = c("green","yellow","purple"),
             names = c("High","Medium","Low")
     )
     
     
   }
   
   q_q_plot <<- function()
   {
     
     qqplot(iris$Sepal.Length,iris$Petal.Length,main="Length EVALUATION",xlab = "Sepal",ylab ="Petal")
     
   }
   
   Stem_leaf_plot <<- function()
   {
     stem(iris$Sepal.Length, width = 100)
   }
   
   Pareto_Chart <<- function()
   {
     
     install.packages("qcc")
     pareto.chart(iris$Sepal.Length)
     
   }
   
 }
 
 
 
 Stat_Calc <<- function()
 {
   choice <- as.numeric(readline(prompt = c("\nEnter\n\n1.  Descriptive Analysis\n2.  Predictive Analysis\n3.  Probability Analysis\n4.  Discrete Distributions\n5.  Continuous Distributions\n6.  Sample Distribution Test\n7.  Interval Estimation\n8.  Non-Parametric Analysis\n9.  Visualizations\t ")))
   
     
   if(choice==1)
   {
     
	ch <- as.numeric(readline(prompt="\n1.Enter data from user\n2.Use 'iris' data"))
	if(ch==1)
		  {
			col_length<<-as.numeric(readline(prompt='Enter size of data: '))
			cat('\nEnter data')
			coll<<-array(0,dim=c(1,col_length))
			for(i in 1:col_length)
			{
				
				coll[i]<<-as.numeric(readline())
			}
		    
                 
	        }	  
	else
	{
		coll<<-iris$Sepal.Length			#taking the coloumn name of iris from user
    		 col_length<<-length(coll)		#calculating size of the data
     }
     Descriptive_Analysis()
     
     cat('\n1.MEAN')
     cat('\n2.MEDIAN')
     cat('\n3.MODE')
     cat('\n4.VARIANCE')
     cat('\n5.STANDARD DEVIATION')
     cat('\n6.MEAN ABSOLUTE DEVIATION')
     cat('\n7.RANGE')
     cat('\n8.QUARTILES')
     cat('\n9.INTER QUARTILE RANGE(IQR)')
     cat('\n10.MINIMUM')
     cat('\n11.MAXIMUM')
     cat('\n12.SKEWNESS')
     cat('\n13.KURTOSIS')
     cat('\n14.MOMENTS')
     cat('\n0.MAIN MENU')
     
     ch<-readline('\nEnter choice\t')
     switch(ch,
            
            "1"=
            {
	          cat("\nMean of data:",Mean(),"\n")
            },
            "2"=
            {
		    cat("\nMedian of data:",Median(),"\n")
              
            },
            "3"=
            {
		    cat("\nMode of data:",Mode(),"\n")
                
            },
            "4"=
            {
                  cat("\nVariance of data:",Variance(),"\n")
           	
            },
            "5"=
            {
		    cat("\nStandard Deviation of data:",Standard_Deviation(),"\n")
                 
            },
            "6"=
            {
              cat(Mean_Deviation())	
            },
            "7"=
            {
              cat(Range())
            },
            "8"=
            {
              n<-as.numeric(readline('\nEnter quartile number\t'))
              cat('\n',n,'th quartile is:',Quartile(n))	
            },
            "9"=
            {
              cat(IQR())
            },
            "10"=
            {
              cat(Minimum())	
            },
            "11"=
            {
              cat(Maximum())
            },
            "12"=
            {
		  s<-Skewness()
              cat(s)
		 if(s>0)
			{
				cat('\ndata is right skewed')
            	}
			else
			{
				cat('\nData is left skewed')
			}
		},
	
            "13"=
            {
              k<-Kurtosis()
		  cat(k)
		  if(k>0)
		  {
			cat('\nData is leptokurtic')
		  }
		  else if(k<0)
		  {
			cat('\nData is leptokurtic')
		  }
		  else
		  {
			cat('\nData is mesokurtic')
		  }
			

            },
            "14"=
            {
              n<<-as.numeric(readline('Enter number\t'))
              cat('\n',n,'th moment is: ',Moment(n))
            },
            "0"=
            {
              Stat_Calc()
            },
            {
              cat('WRONG CHOICE!!')
            }
     )
     
     
   }
   
   else if(choice==2)
   {
     
     Predictive_Analysis()
     
     cat('\n1.CORRELATION')
     cat('\n2.MULTIPLE LINEAR REGRESSION')
     cat('\n0.MAIN MENU')
     ch<-as.numeric(readline('\nEnter choice\t'))
     switch(ch,
            "1"=
            {
              x<<-iris$Sepal.Length
              y<<-iris$Petal.Length
              cat('\nCorrelation coefficient :',Correlation())
            },
            "2"=
            {
              x1<<-iris$Sepal.Length
              x2<<-iris$Petal.Length
              y<<-iris$Petal.Length
              X<-Multiple_Linear_Regression()
              cat('\nThe corresponding constants in order (b0,b1,b2) are: (',X[1][1],' , ',X[2][1],' , ',X[3][1],' ) ')	
            },
            "0"=
            {
              Stat_Calc()
            },
            {
              cat('Wrong choice!!')
            }
     )
   }
   
   else if(choice==3)
   {
     
     Probability_Analysis()
     
     i <- as.numeric(readline(prompt = "\nEnter\n\n1. Permutations\n2. Combinations\n3. Basic Probability\n4. Conditional Probability\n5. Bayes Theorem\n\nEnter 0 for Main Menu!\t"))
     
     if(i==1)
     {
       
       n <- as.numeric(readline(prompt = "\nEnter n:\t"))
       x <- as.numeric(readline(prompt = "\nEnter x:\t"))
       permutation <- Permutation(n,x)
       cat("\nnPx = ",permutation)
       
     }
     
     else if(i==2)
     {
       
       n <- as.numeric(readline(prompt = "\nEnter n:\t"))
       x <- as.numeric(readline(prompt = "\nEnter x:\t"))
       combination <- Combination(n,x)
       cat("\nnCx = ",combination)
       
     }
     
     else if(i==3)
     {
       
       probab <- Probability()
       cat("\nProbability of the event to occur is ",probab,"\n")
       
     }
     
     else if(i==4)
     {
       data1 <- as.vector(strsplit(readline(prompt= "\nEnter the data of set 1 (comma-separated list): \t"), ",")[[1]])
	 data2 <- as.vector(strsplit(readline(prompt= "\nEnter the data of set 2 (comma-separated list) :\t"), ",")[[1]])
      
       prob<-conditional_prob(data1,data2)


     }
     
     else if(i==5)
       Bayes_Theorem()
    
     else if(i==0)
       Stat_Calc()
     
     else
       cat("\nError!\n")
       
   }
   
   else if(choice==4)
   {
     
     Discrete_Distribution_Functions()
     
     i <- as.numeric(readline(prompt = "\nEnter\n\n1. Uniform\n2. Bernoulli\n3. Binomial\n4. Geometric\n5. Hyper-geometric\n6. Negative Binomial\n7. Poisson\n8. Multinomial\n9. Multivariate Hypergeometric\n\nEnter 0 for Main Menu!\t"))
     
     if(i==1)
       Uniform()
       
     else if(i==2)
       Bernoulli()
     
     else if(i==3)
       Binomial()
     
     else if(i==4)
       Geometric()
     
     else if(i==5)
       Hyper_Geometric()
     
     else if(i==6)
       Negative_Binomial()
     
     else if(i==7)
       Poisson()
     
     else if(i==8)
       Multinomial()
     
     else if(i==9)
       Multivariate_Hypergeometric()
     
     else if(i==0)
       Stat_Calc()
     
     else
       cat("\nError!\n")
       
   }
   
   else if(choice==5)
   {
     
     Continuous_Distribution()
     
     i <- as.numeric(readline(prompt = "\nEnter,\n\n1. Uniform\n2. Normal\n3. Bivariate Normal\n4. Gamma\n5. Exponential\n\nEnter 0 for Main Menu!\t"))
     
     if(i==1)
       Uniform()
     
     else if(i==2)
       Normal()
     
     else if(i==3)
       Bivariate_Normal()
     
     else if(i==4)
       Gamma_func()
     
     else if(i==5)
       Exponential()
     
     else if(i==0)
       Stat_Calc()
     
     else
       cat("\nError!\n")
     
   }
   
   else if(choice==6)
   {
     
     Sample_Distribution_Test()
     
     i <- as.numeric(readline(prompt = "\nEnter,\n\n1. Chi-Square\n2. Student t-test\n3. F-test\n4. Z-test\n\nEnter 0 for Main Menu!\t"))
     
     if(i==1)
     {
        chi_value <- Chi_Square()
        n <- as.numeric(readline(prompt = "\nEnter the size of the sample\t"))
        s.d_sq <- as.numeric(readline(prompt = "\nEnter the variance (null hypothesis)\t"))
        s.d_sq1<- as.numeric(readline(prompt = "\nEnter the sample variance (alternate hypothesis)\t"))
        
        chi<- (n-1)*s.d_sq1/s.d_sq
        chi <- Mod(chi)
        if(chi>=chi_value)
          cat("\n Null Hypothesis is rejected!\n")
        else
          cat("\nNull Hypothesis is accepted!\n")
        
        
     }
        
     
     else if(i==2)
      { 
       n <- as.numeric(readline(prompt = "\nEnter the size of the sample\t"))
       t_alpha <- t_test(n)
       
       mu <- as.numeric(readline(prompt = "\nEnter the mean (null hypothesis)\t"))
       m <- as.numeric(readline(prompt = "\nEnter the sample mean \t"))
      
       s.d <- as.numeric(readline(prompt = "\nEnter the standard deviation of sample\t"))
       
       t <- (m-mu)/(s.d/sqrt(n))
       
       if(Mod(t)>=t_alpha)
         cat("\n|",t,"| >= ",t_alpha,"\nSo, Null Hypothesis is rejected!\n")
       else
         cat("\n|",t,"| < ",t_alpha,"\nSo, Null Hypothesis is accepted!\n")
       
     }
        
       
     
     else if(i==3)
     {
       n1 <- as.numeric(readline(prompt = "\nEnter size of sample of 1st kind\t"))
       n2 <- as.numeric(readline(prompt = "\nEnter size of sample of 2nd kind\t"))
       
       df1 <- (n1-1)
       df2 <- (n2-1)
       f_value <- F_test(df1,df2)
       
       s.d_sq1 <- as.numeric(readline(prompt = "\nEnter the variance of sample of 1st kind\t"))
       s.d_sq2 <- as.numeric(readline(prompt = "\nEnter the variance of sample of 2nd kind\t"))
       
       f_test_val <- s.d_sq1/s.d_sq2
       
       if(f_test_val>=f_value)
         cat("\nNull Hypothesis must be rejected!\n")
       else
         cat("\nNull Hypothesis is accepted!\n")
     
     }
     
     else if(i==4)
     {
       z_alpha <- z_test()
       mu <- as.numeric(readline(prompt = "\nEnter the mean (null hypothesis)\t"))
       m <- as.numeric(readline(prompt = "\nEnter the sample mean \t"))
       n <- as.numeric(readline(prompt = "\nEnter the size of the sample\t"))
       s.d <- as.numeric(readline(prompt = "\nEnter the standard deviation\t"))
       
       z <- (m-mu)/(s.d/sqrt(n))
       
       if(Mod(z)>=z_alpha)
         cat("\n Null Hypothesis is rejected!\n")
       else
         cat("\nNull Hypothesis is accepted!\n")
       
     }
     
     
     else if(i==0)
       Stat_Calc()
     
     else
       cat("\nError!\n")
     
   }
   
   
   else if(choice==7)
   {
     
     Interval_Estimation()
     
     cat('\n1.ESTIMATION OF MEANS')
     cat('\n2.ESTIMATION OF DIFFERENCES IN MEANS')
     cat('\n3.ESTIMATION OF PROPORTIONS')
     cat('\n4.ESTIMATION OF DIFFERENCES IN PROPORTIONS')
     cat('\n5.ESTIMATION OF VARIANCES')
     cat('\n6.ESTIMATION OF RATIO OF TWO VARIANCES')
     ch<-readline('\nEnter choice: ')
     switch(ch,
            "1"=	{
              C_I<<-as.numeric(readline('Enter confidence interval: '))
              alpha<<-(1-(C_I/100))
              Mean_1<<-as.numeric(readline('Enter mean: '))
              number_1<<-as.numeric(readline('Eneter size of sample: '))
              S_D_1<<-as.numeric(readline('Enter standard deviation: '))
              X<-Estimation_Mean()
              cat('The mean lies in the interval [',X[1],',',X[2],']')
            },
            "2"=
            {
              C_I<<-as.numeric(readline('Enter confidence interval: '))
              cat('\nFirst Sample:\n')
              alpha<<-(1-(C_I/100))
              Mean_1<<-as.numeric(readline('Enter mean: '))
              number_1<<-as.numeric(readline('Eneter size of sample: '))
              S_D_1<<-as.numeric(readline('Enter standard deviation: '))
              cat('\nSecond sample\n')
              Mean_2<<-as.numeric(readline('Enter mean: '))
              number_2<<-as.numeric(readline('Eneter size of sample: '))
              S_D_2<<-as.numeric(readline('Enter standard deviation: '))
              X<-Estimate_Difference_Mean()
              cat('The difference between mean lies in the interval [',X[1],',',X[2],']')
              
            },
            "3"=
            {
              C_I<<-as.numeric(readline('Enter confidence interval: '))
              alpha<<-(1-(C_I/100))
              x<<-as.numeric(readline('\nEnter x: '))
              number_1<<-as.numeric(readline('Eneter size of sample: '))
              X<-Estimation_Proportions()
              cat('The mean lies in the interval [',X[1],',',X[2],']')
            },
            "4"=
            {
              C_I<<-as.numeric(readline('Enter confidence interval: '))
              alpha<<-(1-(C_I/100))
              cat('First sample:')
              x1<<-as.numeric(readline('\nEnter x: '))
              number_1<<-as.numeric(readline('Eneter size of sample: '))
              cat('\nSecond sample:')
              x2<<-as.numeric(readline('\nEnter x: '))
              number_2<<-as.numeric(readline('Eneter size of sample: '))
              X<-Estimation_Difference_Proportions()
              cat('The mean lies in the interval [',X[1],',',X[2],']')
            },
            "5"=
            {
              
              C_I<<-as.numeric(readline('Enter confidence interval: '))
              alpha<<-(1-(C_I/100))
              number<<-as.numeric(readline('Enter size of sample: '))
              S_D<<-as.numeric(readline('Enter standard deviation: '))
              X<-Estimation_Variance()
              cat('The variation lies in the interval [',X[1],',',X[2],']')
            },
            
            "6"=
            {
              
              C_I<<-as.numeric(readline('Enter confidence interval: '))
              alpha<<-(1-(C_I/100))
              cat('First sample:\n')
              S_D_1<<-as.numeric(readline('Enter standard deviation: '))
              number_1<<-as.numeric(readline('Eneter size of sample: '))
              cat('Second sample:\n')
              S_D_2<<-as.numeric(readline('Enter standard deviation: '))
              number_2<<-as.numeric(readline('Enter size of sample: '))
              X<-Estimation_Ratio_Variance()
              cat('The ratio of variation lies in the interval [',X[1],',',X[2],']')
              
            },
            
            {
              cat('WRONG CHOICE!!')
            }
            
     )
     
     
     
     
   }
   
   
   else if(choice==8)
   {
     Non_Parametric()
     
     	cat('\n1.SIGN TEST')
		cat('\n2.WILCOXON SIGNED RANK TEST')
		cat('\n3.MANN-WHITNEY TEST')
		cat('\n4.KRUSKAL-WALLIS TEST')
		ch<-readline('\nEnter choice')
		switch(ch,
		"1"={
			data1<<-c(17,15,20,29,19,18,22,25,27,9,24,20,17,6,24,14,15,23,24,26,19,23,28,19,16,22,24,17,20,13,19,10,23,18,31,13,20,17,24,14)
			alpha<<-as.numeric(readline('\nEnter level of significance: '))
			cat('\nFor null hypothesis: ')
			null<<-as.numeric(readline('\nEnter mean: '))
			cat('\nFor alternative hypothesis:')
			cat('\n1.Less than')
			cat('\n2.More than')
			cat('\n3.Not equal to')
			condition<-readline('\nSelect condition: ')
			switch(condition,
			"1"=
				{
					Sign_Test(1)
				},
				
			"2"=
				{
								
					Sign_Test(0)
				},
			
				{
					cat('WRONG CHOICE!!')
				}
			
		           )
			},
				
		"2"=				

			{
					data1<<-c(97.5,95.2,97.3,96,96.8,100.3,97.4,95.3,93.2,99.1,96.1,97.6,98.2,98.5,94.9)
					alpha<<-as.numeric(readline('\nEnter level of significance: '))
					cat('\nFor null hypothesis: ')
					null<<-as.numeric(readline('\nEnter mean: '))
					cat('\nFor alternative hypothesis:')
					cat('\n1.Less than')
					cat('\n2.More than')
					cat('\n3.Not equal to')
					condition<-readline('\nSelect condition: ')
					Wilcoxon_Signed_Rank_Test(condition)
			},

		"3"=
			{
				data1<<-c(14.9,11.3,13.2,16.6,17,14.1,15.4,13,16.9)
				data2<<-c(15.2,19.8,14.7,18.3,16.2,21.2,18.9,12.2,15.3,19.4)
				alpha<<-as.numeric(readline('\nEnter level of significance: '))
				cat('\nFor null hypothesis: ')
				cat('\nMean of first data is equal to mean of second data')
				cat('\nFor alternative hypothesis:')
				cat('\n1.Less than')
				cat('\n2.More than')
				cat('\n3.Not equal to')
				condition<-readline('\nSelect condition: ')
				Mann_Whitney_Test(condition)	
			},
		"4"=
			{
				data1<<-c(94,88,91,74,87,97)
				data2<<-c(85,82,79,84,61,72,80)
				data3<<-c(89,67,72,76,69)
				alpha<<-as.numeric(readline('\nEnter level of significance: '))
				cat('\nFor null hypothesis: ')
				cat('\nAll methods are equally effective\n')
				cat('\nFor alternative hypothesis:')
				cat('\nAll methods are equally effective')
				Kruskal_Wallis_Test()	
			},
						
			
			{
					cat('WRONG CHOICE!!')
			}
			 
		  )
		   		   
		
		
  
     
   }
   
   
   
   
   else if(choice==9)
   {
     
     Visualization()
     
     i <- as.numeric(readline(prompt = "\nEnter,\n\n1. Histograms\n2. Line Graph\n3. Bar Graph\n4. Pie Chart\n5. Scatter plot\n6. Box-plot\n7. q-q plot\n8. Stem-leaf plot\n9. Pareto Chart\n\n0. Main Menu"))     
     if(i==1)
       Histograms()
     
     else if(i==2)
       Line_Graph()
     
     else if(i==3)
       Bar_Graph()
     
     else if(i==4)
       Pie_Chart()
     
     else if(i==5)
       Scatter_plot()
     
     else if(i==6)
       Box_plot()
     
     else if(i==7)
       q_q_plot()
     
     else if(i==8)
       Stem_leaf_plot()
     
     else if(i==9)
       Pareto_Chart()
     
     else if(i==0)
       Stat_Calc()
     
     else
       cat("\nError!\n")

	
     
   }

	repeat
	{

		continue <- as.character(readline(prompt="Restart Again? (Y/N)\t"))

		if(continue=="Y"||continue=="y")
			Stat_Calc()
		else if(continue=="N"||continue=="n")
			break
		else
			cat("\nError! Re-Enter your Input\n")

		break
	}
     
     
 }
 Stat_Calc()