
challenges$newChallengeId <- c(as.matrix(as.integer(factor(challenges$ChallengeId))))  # changed the hexadecimal Ids into unique integers

chall_data$newDomain <- (c(as.matrix(as.integer(factor(chall_data$domain)))))  # changed the hexadecimal domain into unique integers

 
ALL_uniqueIds <- data.frame(unique(chall_data$challenge_id))  #Getting unique Ids.
ratings <- t(ALL_uniqueIds) # transpose of all_uniqueids
colnames(ratings) <- ratings[1,] # set the columns names of ratings as first row of ratings values
 
set_submissions <- cbind(submissions$hacker_id, submissions$challenge_id) #extracting only hacker and challenge Ids.
colnames(set_submissions) <- c("hacker_id","challenge_id") # set columns names as mentioned
set_submissions <- as.data.frame(set_submissions) # convert to dataframe


set_submissions$'1' <- c(as.matrix(as.integer(factor(set_submissions$hacker_id))))   # allocating unique integer values to the hacker Ids. means adding new column as integer hacker ids
set_submissions$'2' <- c(as.matrix(as.integer(factor(set_submissions$challenge_id)))) # allocating unique integer values to the challenge Ids. means adding new column as integer challange ids

 item_sim = cosine(as.matrix(challenges$newChallengeId))  # doubts it results in a square matrix of size 2228.
 # Getting the similairity between all the challengeIds using cosine.

myMatrix <- data.frame(set_submissions$`1`) # put all integer hacker ids values in myMatrix
 
 # Coverting the given dataset into a data frame such that every row contains one hacker_id (it has 286064 rows ) with their respective column values
 # being 1 for the challenges the hacker took part in.
 # this for loop is creating a matrix which has hacker ids in 1 column (286064 rows) and challenge ids as other columns (only unique challenge ids as columns means 2156 columns of challenge ids)
for(i in 1:nrow(set_submissions)){
     x <- set_submissions[i,4] # set integer challenge id to x
     if(!is.element(as.character(x),colnames(myMatrix))) # comparing if  challenge id is present in  colnames of mymatrix
	 {
	 # in below lines a column is created corresponding to a challenge id if challenge id is not present as column name in my matrix
         columnMatrix <- NULL
         columnMatrix <- data.frame(matrix(ncol=1,nrow=nrow(set_submissions)))
         colnames(columnMatrix) <- as.character(x) # add a column name as challenge id name
         columnMatrix[i,] <- 1  #  put 1 in ith row of columnmatrix
         columnMatrix[-i,] <- NA # put NA in all places except ith row
         myMatrix <- cbind(myMatrix,columnMatrix) # appending a column in myMatrix
     }
     else{
         myMatrix[i,as.character(x)] <- 1 # if already challenge id is present as a column name then set 1 value
     }
 }
 
 colnames(myMatrix)[1] <- "Hacker_Id" # set column name of hacker ids to "hacker_Id"
 
 
  for(i in 1:ncol(myMatrix)){
     colnames(myMatrix)[i] <- set_submissions[which(colnames(myMatrix)[i]==set_submissions$`2`),2][1] # assigning column names of myMatrix to challenge ids  in hexdecimal  forms
     
 }
 
 colnames(myMatrix)[1] <- "Hacker_Id"
 
 
 # Getting the number of attempts each hacker(10000 rows) took for every challenge.
 hh <- ddply(myMatrix[,1:2],~Hacker_Id,plyr:::summarise,mySum=sum(`951`)) # doubt
 
 for(i in 2:ncol(sum_row)){  # this loop is adding all the values in a particular column and store it in a sum_row matrix which is of 1 x no of challenges dimension
     sum_row[1,i] <- sum(myMatrix[,i])
     
 }

 colnames(sum_row) <- colnames(myMatrix) # set colnames as mymatrix colnames
 sum_col <- t(sum_row) # take trnspose of sum_row
 colnames(sum_col) <- "sum" # set colname as sum
 sum_col <- data.frame(sum_col) # convert to dataframe
 sum_col$ID <- rownames(sum_col) # make a column id and put values in id from rownames of sum_col
 
 #Out of 2156 challenges listed in submissions.csv, getting the most popular 445 challenges  ,it can be done by finding mean of total submissions for challenge by (286064/2156 = 132.2)
 number <- NROW(sum_col[which(sum_col$sum > 132,arr.ind=T),"ID"]) # finding total no of rows in which a chaalenge has more than 132 submissions
 popular_challengeId <- data.frame(matrix(ncol=1,nrow=number)) # creating blank dataframe of size number x 1
 popular_challengeId[1:445,] <- sum_col[which(sum_col$sum > 132,arr.ind=T),"ID"] # fill 445 entries in popular _chaallenge by challenge ID's which has greater then 132 submissions
 

for(i in 1:nrow(popular_challengeId))
 { 
 x <- popular_challengeId[i,1];
 myText[i,2] <- print(paste0( "i <- i+1; print(i); newMatrix <- data.frame(as.matrix(myMatrix[which(myMatrix$`",x,"`!=0),c(@Hacker_Id@,@",x,"@)]));ls <- ddply(newMatrix,~Hacker_Id,plyr:::summarise,mySum=sum(X",x,"));ww <- merge(ww, ls, by = @Hacker_Id@, all.x = TRUE);"))
 }
 
 colnames(hh)[1:445] <- popular_challengeId[1:445,1] # doubt
 
 hh[hh==0] <- NA # put NA where hh == 0
 
 ratings <- hh # putt hh in ratings
 
 for(i in 1:ncol(item_sim)){ # change iten_sim to numeric values
     sim[,i] <- as.numeric(item_sim[,i])
     
 }
 
 item_sim <- sim # put sim back to item_sim
 
 # below 7 lines in doubt
 colnames(item_sim)[1] <- "Challenge_Id" # set first colname as challenge_id
 colnames(popular_challengeId)[1] <- "Challenge_Id" # set first colname as challenge_id
 for(i in 1:nrow(recommendations)){ # calling fxn rec_itm_for_user which will give 10 recommendation for a parricular hacker id
     
    recommendations[i,2:11] <- rec_itm_for_user(i)    #Calling the recommender function for every hacker_id
}
 new_item_sim <- merge(popular_challengeId, item_sim, by="Challenge_Id", all.x = TRUE ) # merge populalar chllenge id nd item_sim by challengeid
 
 myTranspose_sim <- data.frame(t(new_item_sim)) # take tranpose of new_item_sim
 myTranspose_sim$Challenge_Id <- rownames(myTranspose_sim) #  put rownames of mytranspose_sim to challenge_id column
 
 Final_sim <- merge(popular_challengeId, transpose_sim, by="Challenge_Id", all.x = TRUE) # merge populalar chllenge id nd transpose_sim by challengeid
 item_sim <- Final_sim # finally put final_sim back into item_sim
 
recommendations <- data.frame(matrix(ncol = 11, nrow = 10000)) # create dataframe of 11 cols and 10000 rows
colnames(recommendations) <- c("Hacker_Id",colnames(recommendations)[1:10]) # first column name is hacker id
recommendations$Hacker_Id <- ratings$Hacker_Id # put all hacker ids in hacker id columns from ratings



recommend_hex <- merge(recommendations, set_submissions[,c(1,3)], by = "Hacker_Id", all.x = TRUE)  
recommend_hex <- unique(recommend_hex) # take unique hacker id means remove duplication
rownames(recommend_hex) <- recommend_hex$Hacker_Id # make rownames as hacker_ids


recommend_hex_raw <- recommend_hex[1:2000,]
recommend_hex_raw[is.na(recommend_hex_raw)] <- 181 # doubt

for(i in 1:2000){ # for rows
     for(j in 1:10){ # for columns
         
         which(recommend_hex_raw[i,j]==challenge_code[,2])-
         {recommend_hex_raw[i,j] <- as.character(challenge_code[which(recommend_hex_raw[i,j]==challenge_code[,2]),"challenge_hex_id"])} # replace challenge id to challenge hex id
         
         
     }
 }
 new_recommend_hex_raw <- recommend_hex_raw[,c(11,1:10)] # put hacker ids before challenge ids

gg <- apply(new_recommend_hex_raw, 1, paste, collapse=",")    # Converting into required solution format as per the challenge.
gg <- data.frame(gg)
colnames(gg) <- " "

write.csv(gg, "recommendation.csv", row.names = FALSE)
