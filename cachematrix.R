## zbc's R assignment 2 complete codes

##part 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files

     ## 'pollutant' is a character vector of length 1 indicating
     ## the name of the pollutant for which we will calculate the
     ## mean; either "sulfate" or "nitrate".
   ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     ## Return the mean of the pollutant across all monitors list
     ## in the 'id' vector (ignoring NA values)
        
 dataPath<-paste(".",directory, sep="/") ## to get data path
       
 ## read all the visible files under the directory specified in ##the first parameter
	  dataFileNames<-list.files(path = dataPath, pattern = NULL, all.files = FALSE)
        fullDataFileNames<-paste(dataPath,dataFileNames, sep="/")
         print (length(fullDataFileNames))
	   selectedFileNames <- fullDataFileNames[id] ##one more NA element is appended?

     ##    print (length(selectedFileNames))
########
       if (pollutant=="sulfate"){
       print ("pollutant is sulfate. Its mean value is: ")
        allDataNoNA <-lapply(selectedFileNames,function(fileName){
		na.omit(read.csv(fileName, colClasses=c('NULL','numeric','NULL','numeric'), header=TRUE))})
       }
       else{
        print ("pollutant is nitrate. Its mean value is ")
        allDataNoNA <-lapply(selectedFileNames,function(fileName){
		na.omit(read.csv(fileName, colClasses=c('NULL','NULL','numeric','numeric'), header=TRUE))})
       }
        ##allDataNoNA is a list whose element is a list again       

        combinedData <- do.call(rbind,allDataNoNA) ## combine each file's data into one matrix
	  ##tail(combinedData, n=5) ##only work if typed in console 
        print( result<- round(mean(combinedData[,1]),3))
       ## print (result<-signif(mean(combinedData[,2]), 3)) ##average of id of non NA record
} 

##part 2
complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        dataPath<-paste(".",directory, sep="/") ## to get data path
       
        ## read all the visible files under the directory specified in the first parameter
	  dataFileNames<-list.files(path = dataPath, pattern = NULL, all.files = FALSE)
        fullDataFileNames<-paste(dataPath,dataFileNames, sep="/")
         #print (length(fullDataFileNames))
	   selectedFileNames <- fullDataFileNames[id] ##one more NA element is appended?
        ## selectedFileNames <- selectedFileNames[1:length(selectedFileNames)-1] ## adjust to avoid extra non-existed file
         #print (length(selectedFileNames))
########
                  dtNoNAList <-lapply(selectedFileNames,function(fileName){
		(na.omit(read.csv(fileName, colClasses=c('NULL','numeric','numeric','numeric'), header=TRUE)))})
      
        ##dtNoNAList is a list whose element is another list hold non NA data exclusive date      

            dataFrame <-lapply(dtNoNAList,function(a){
               if (nrow(a)!=0) 
                  rslt <-c(id=sum(a[3])/nrow(a),nobs=nrow(a))
               else rslt<-c(id=0, nobs=nrow(a))#should not hardcode it
		   #names(rslt)<-c("id","nobs")
             }) #why first element could not be a[3] 
      	#print ("First dataset")
      	#print (dtNoNAList[[1]])
 	
            resultFrame <- do.call(rbind,dataFrame)##convert to matrix
		            
            realFrame<-data.frame(resultFrame)
            for (i in 1:nrow(realFrame)){
              if (realFrame[i,2]==0) realFrame[i,1]<-i
            }
  		#print ("Result of data frame: ")
            #print (realFrame)
             realFrame
}

##part 3
corr <- function(directory, threshold=0){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## if there is no data for calculating correslation, it'll crash, ---need to handle further.         


         resultFrame1<-complete(directory, 1:332) ##call complete.R function to get totol No. or rows with non NA data for a minotor
         #use 1:332,it will read 333.csv as thelast file,occasionally the ending No, is big, following error occurs:       
         #Error in FUN(X[[1L]], ...) : 275 starts to have problem
         #only defined on a data frame with all numeric variables.
         # 275 starts to have problem,, or is it because open a lot fle twice, need to close it, then open again, etc?

	   resultCol2 <- resultFrame1[,2] ##get the no_of_non_NA data column, resultCol2 is a vector
         #resultCol2 <-matrix(unlist(resultCol2), nrow=1, byrow=TRUE)	#convert from list to matrix         

         #print (resultCol2 )

         aboveThresholdID <- resultFrame1[resultCol2>threshold,1] ##get id column data whose no_of_non_NA is bigger than threshold
         #print("ID above threshold:")
         #print( aboveThresholdID) #aboveThreadholdID is a vector
         if(length(aboveThresholdID)>0){
         #re-read with updated ID, can't reuse varable in function complete1(?)
         dataPath1<-paste(".",directory, sep="/") ## to get data path
       
        ## read all the visible files under the directory specified in the first parameter
	  dataFileNames1<-list.files(path = dataPath1, pattern = NULL, all.files = FALSE)
        fullDataFileNames1<-paste(dataPath1,dataFileNames1, sep="/")
         #print (length(fullDataFileNames1))
         #iD1<-matrix(unlist( aboveThresholdID), nrow=1) 
          iD1<-aboveThresholdID
        
	   selectedFileNames1 <- fullDataFileNames1[iD1] ##one more NA element is appended?
        ## selectedFileNames <- selectedFileNames[1:length(selectedFileNames1)-1] ## adjust to avoid extra non-existed file
        
        #print (length(selectedFileNames1))
        #print ("Selected File name above threshold:")
        #print (selectedFileNames1)
########
                  dtNoNAList1 <-lapply(selectedFileNames1,function(fileName){
	  (na.omit(read.csv(fileName, colClasses=c('NULL','numeric','numeric','numeric'), header=TRUE)))})
         #print(dtNoNAList1)

        
        statisticFrame <-lapply(dtNoNAList1,function(a){
           round(cor(a[1],a[2]),5) 
        }) #why first element could not be a[3]      

        ## Return a numeric vector of correlations
	  statisticData <- matrix(unlist(statisticFrame), ncol=1) #important
        #statisticData<-data.frame(statisticFrame)
        corV<-c(statisticData) #convert matrix to vector
        #print ("Correlation between nitrate and sulfate is:")
        #print (corV)
        corV
   }else(
    noData<-c()
  )   
}
