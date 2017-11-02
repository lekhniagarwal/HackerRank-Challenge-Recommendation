rec_itm_for_user = function(hacker) # fxn to find out 10  recommendations of challenge id to a particulr hacker
 {
   #extract all the challenges not attempted by hacker
   userRatings = ratings[hacker,] #ratings for a particular hacker
   non_attempted_challenges = list() # empty list for non attemped challenges
   attempted_challenges = list()  # empty list for attemped challenges
   for(i in 2:ncol(userRatings)){
     if(is.na(userRatings[,i]))   #If ratings are NA, then they were not attempted challenges, else attempted.
     {
       non_attempted_challenges = c(non_attempted_challenges,colnames(userRatings)[i]) # append all non_attempted challenges to list
     }
     else
     {
       attempted_challenges = c(attempted_challenges,colnames(userRatings)[i]) # append attempted challenges to list
     }
   }  
   non_attempted_challenges = unlist(non_attempted_challenges)   
   attempted_challenges = unlist(attempted_challenges)
   
   #create weighted similarity for all the attempted challenges by hacker
   non_attempted_pred_score = list() # create empty list for non_attempted_pred_score
   
   for(j in 1:length(non_attempted_challenges)){ # loop for total length of non_attempted_challenges
     temp_sum = 0 # set temp_sum to 0
	 # Get the item similarity row for every non attempted challenge.
     df = item_sim[which(rownames(item_sim)==non_attempted_challenges[j]),1:445]  # df is the similarity row for non_attempted challenge
     
	 
	 
	 for(i in 1:length(attempted_challenges)){ # loop for total length of attempted_challenges
       temp_sum = temp_sum+ df[which(colnames(df)== attempted_challenges[i])] # finding values of attempted challenges from similarity row of non_attempted challange 
        }
		# Multiply the ratings and similarity values to get the weighted matrix.
     weight_mat = df*ratings[hacker,2:ncol(ratings)]   # df order 1xn and ratings order 1xn
	    # Calculated the prediction score for all the non attempted challenges.
     non_attempted_pred_score = c(non_attempted_pred_score,rowSums(weight_mat,na.rm=T)/temp_sum)
     }
	 
	 
	 
	 
	 
   pred_rat_mat = as.data.frame(non_attempted_pred_score) # convert to dataframe 
   colnames(pred_rat_mat) = non_attempted_challenges # set colnames of pre_rat_mat as the non attempted challenges names
   print(hacker); # print hacker
   ans <- NULL;
   ans <- pred_rat_mat; # put pred_rat_mat to ans
   #Arrange the prediction score for every non attempted challenges in decreasing order and select the first 10 challenges.
   p <- data.frame(colnames(ans[,order(ans[1,], decreasing= T)][1:10])); # put challenge id names in p
   p <- t(p); # take transpose of p
   return (p); # return p
 }
 