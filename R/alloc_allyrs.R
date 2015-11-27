#' An optimised demand allocation model 
#'
#' allocate students to schools based on capacity and travel time
#' simulate for all years
#' @param clustername, cohort, scenarios(1 to N)
#' @keywords asset planner
#' @export
#' @examples
#' alloc_yrs()


alloc_allyrs<-function(A,cohort,scenario1,scenario2,scenario3,scenario4,scenario5,scenario6,scenario7,scenario8,scenario9,scenario10){

	#specify files and working directory
	library(gdata)
	library(dplyr)
	library(reshape)
	# library(xlsx)
	studentseifa=read.xls("studentseifa.xlsx")
	row.names(studentseifa)<-as.vector(unlist(studentseifa[1])) #rename rows
	traveltime=read.xls("traveltime.xlsx")

	A=deparse(substitute(A))
	cohort=deparse(substitute(cohort))
	if(cohort!='p'&cohort!='s'){print("model is missing cohort argument. Please specify p or s (p=primary;s=secondary")}
	############SELECT PRIMARY OR SECONDARY STUDENTS FILE###############
	studentfilename=paste(paste("studentenroll",cohort,sep=""),".xlsx",sep="")
	studentenroll=read.xls(studentfilename)
	# results are:
	# studentenrollp=read.xls("studentenrollp.xlsx")
	#rename row name
	row.names(studentenroll)<-as.vector(unlist(studentenroll[1])) 

	############ SELECT STUDENTS THAT CLUSTER ONLY ###############
	clusterenroll=studentenroll[which(studentenroll["cluster"]==A),]#
	
	############ MATCH SEIFA by STUDENT ORIGINS ###############
	studentseifa=merge(studentenroll["MB"],studentseifa,by.x="MB",by.y="MB")
	clusterseifa=studentseifa[which(studentseifa["cluster"]==A),]
	
	############ SELECT SCHOOL FILE BY COHORT ###############
	schoolfilename=paste(paste("oschool",cohort,sep=""),".xlsx",sep="")
	school=read.xls(schoolfilename)
	row.names(school)<-as.vector(unlist(school[1]))#rename rows
	
	############ SELECT SCHOOLS IN THAT CLUSTER ONLY ###############
	oschool=school[which(school["cluster"]==A),]

	############ EXISTING SCHOOL CAPACITY ###############
	ocapacity=data.frame(oschool[1],oschool[2]) #secondary schools
	# opcapacity=data.frame(p[1],p[2]) #primary schools

	############ all tables first row is row name ###############
	############ DEFINE ROW NAMES ###############
	# catchment for each year (3 outputs)
	catchmentlist<-NULL

	# seifa for each year - then average (1 output)
	averageseifa<-NULL

	# travel time for each year - then average (1 output)
	# ## basically, average TT = catchment * clustertraveltime / total istudents allocated to school
	# ### catchment * clustertraveltime 
	# catchment[,sapply(catchment,is.numeric)]*clustertraveltime_m[,sapply(clustertraveltime_m,is.numeric)]
	# ### colSums(catchment * clustertraveltime)
	# colSums(catchment[,sapply(catchment,is.numeric)]*clustertraveltime_m[,sapply(clustertraveltime_m,is.numeric)])
	# ### total students allocated to school
	# colSums(catchment[,sapply(catchment,is.numeric)]))
	# #### calcaulate average
	averagett<-NULL

	# unallocated students for each year (3 outputs)
	unallocstudent<-NULL

	# unallocated capacity for each year (3 outputs)
	unalloccap<-NULL

	# compare against max for each year (3 outputs)
	# XXXXX

	############ CREATE A LIST OF INPUT SCENARIOS TO LOOP ###############
	arguments<-as.character(as.list(match.call()))
	inputfiles<-arguments[4:(length(arguments))]
	for(file in inputfiles){
		
		inputs=read.xls(paste(file,".xlsx",sep=""))
		row.names(inputs)<-as.vector(unlist(inputs[1])) #rename rows

		############ CONSTRUCT CLUSTER TRAVEL TIME MATRIX ############# (includes new schools)		
		clusterorigin=clusterenroll[1]
		clusterdestination=inputs["MB"]
		colnames(clusterorigin)="origin"
		colnames(clusterdestination)="destination"
		origin=merge(clusterorigin,traveltime,by.x="origin",by.y="origin")
		clustertraveltime_m=merge(clusterdestination,origin,by.x="destination",by.y="destination")
		clustertraveltime_m<- melt(clustertraveltime_m, id=c("origin","destination"))
		clustertraveltime_m<-cast(clustertraveltime_m,origin~destination,mean)


		############ CONSTRUCT CATCHMENT ############# 
		catchment_m=clustertraveltime_m
		catchment_m[which(sapply(catchment_m, is.numeric)=='TRUE')][catchment_m[which(sapply(catchment_m, is.numeric)=='TRUE')]>0]<-0


		############ INPUTS WITH CHANGE ############
		firstcol=which(sapply(inputs, is.numeric)=='TRUE')[1] 
		seconcol=which(sapply(inputs, is.numeric)=='FALSE')[2]#find the first column that is not numeric (aka MB)
		inputcapacity=inputs[firstcol:(seconcol-1)] 
		inputwithchange=inputcapacity

		inputwithchange=cbind('X'=rownames(inputwithchange),inputwithchange) #put names back to the first column
		colnames(inputwithchange)[1]=colnames(ocapacity[1]) #make sure the col names are same as ocapacity

		#find new MB and SEIFA
		#identify all schools (including new schools and their MB)
		############ OTHER INPUTS TBA ############
		inputMB=data.frame(inputs[1],inputs["MB"])
		inputSEIFA=data.frame(inputs[1],inputs["SEIFA"])		
		
		############ CHANGE YEARS ############
		changeyears=colnames(inputwithchange)

		############ FOR EACH YEAR THAT HAS BEEN UPDATED ############
		for(i in 2:length(changeyears)){
		# for(i in 2:length(changeyears)){

			#step1 - calculate school capacity of that year
			newcapacity=merge(ocapacity,data.frame(inputwithchange[1],inputwithchange[changeyears[i]]), all='T') # remember to retain row names
			newcapacity[is.na(newcapacity)]<-0 #omit NAs
			capacityofyear=data.frame(newcapacity[2]+newcapacity[3])
			newcapacityofyear=data.frame(newcapacity[1],capacityofyear)
			newcapacityofyear=merge(newcapacityofyear,inputMB,by.x=colnames(newcapacityofyear)[1],by.y="cluster.") #name column by MB
			# row.names(newcapacityofyear)<-as.vector(unlist(newcapacityofyear[ncol(newcapacityofyear)]))

			#step2 - find available students
			availstudents=data.frame(clusterenroll[1],clusterenroll[changeyears[i]])
			colnames(availstudents)[2]="students"

			#step3 - make duplicates of cluster and catchment
			catchment=catchment_m
			clustertraveltime=clustertraveltime_m
			#loop
			while (sum(newcapacityofyear[2])&&sum(availstudents[2])){#there is still capacity AND has available students
				
				#### FIND MB PAIRS WITH SHORTEST TRAVEL TIME ####
				clustertraveltime[,sapply(clustertraveltime, is.numeric)]#exclude non numeric
				coordinate = which(clustertraveltime == min(clustertraveltime[,sapply(clustertraveltime, is.numeric)]), arr.ind = TRUE) #find shortest travel time
				# print(coordinate[1,])
				originMBmintravel=as.character(clustertraveltime[coordinate[1,1],1]) #"MB2". if without "as.character, then [1] MB2"
				destMBmintravel=colnames(clustertraveltime)[coordinate[1,2]] #"MB2"	
				#### FIND NUMBER OF STUDENTS IN THIS MB PAIR ####
				numberofstudents=availstudents[which(availstudents["MB"]==originMBmintravel),2]

				#### CHECK IF THIS NUMBER OF STUDENTS CAN FIT IN THIS DESTINATION ####

				#### FIND SCHOOL(s) IN THIS MB PAIR ####
				schoolsinmbpair<-which(newcapacityofyear["MB"]==destMBmintravel)
				
				for (p in schoolsinmbpair){
					#### WHAT IS THE REMINING CAPACITY? ####
					destschoolcapacity=newcapacityofyear[p,2]
					
					#IF HAS SUFFICIENT CAPACITY
					if(numberofstudents<destschoolcapacity){
						#### ALLOCATE THESE STUDENTS TO SCHOOL ####
						catchment[coordinate[1,1],coordinate[1,2]]=numberofstudents
						#### MAKE CAPACITY IN THIS SCHOOL SMALLER ####
						newcapacityofyear[which(newcapacityofyear["MB"]==destMBmintravel),2]=destschoolcapacity-numberofstudents
						#### MAKE AVAIL STUDENTS = 0 #####
						availstudents[which(availstudents["MB"]==originMBmintravel),2]=0
						# availstudents=data.frame(availstudents,A,changeyears[i],file)
						clustertraveltime[coordinate[1,1],coordinate[1,2]]=99999
						# print(clustertraveltime)
					}
					else{ #numberofstudents>destschoolcapacity
						#### ALLOCATE PARTIAL STUDENTS TO SCHOOL ####
						catchment[coordinate[1,1],coordinate[1,2]]=destschoolcapacity
						#### MAKE CAPACITY IN THIS SCHOOL 0 ####
						#in this case, 0
						newcapacityofyear[which(newcapacityofyear["MB"]==destMBmintravel),2]=0
						#### MAKE AVAIL STUDENTS SMALLER #####
						availstudents[which(availstudents["MB"]==originMBmintravel),2]=numberofstudents-destschoolcapacity
						clustertraveltime[coordinate[1,1],coordinate[1,2]]=99999
						# print(clustertraveltime)
					}
				} #end for schools in mbpair

			}#end while
			#create [catchment'i']

			#output1 - average traveltime
			#tt calcs for 1 year
			averagettbyyear=colSums(catchment[,sapply(catchment,is.numeric)]*clustertraveltime_m[,sapply(clustertraveltime_m,is.numeric)])/colSums(catchment[,sapply(catchment,is.numeric)])
			averagettbyyear=data.frame(averagettbyyear,A,changeyears[i],file)
			averagett<-rbind(averagett,averagettbyyear)
			
			#output2 - average seifa
			#create a clusterseifa with same cols as catchment, then colsums(seifa*catchment)/colsums(Catchment)
			clusterseifa[2:length(catchment)]=clusterseifa[2]
			colnames(clusterseifa)=colnames(catchment)
			averageseifabyyear=colSums(clusterseifa[,sapply(clusterseifa,is.numeric)]*catchment[,sapply(catchment,is.numeric)])/colSums(catchment[,sapply(catchment,is.numeric)])
			averageseifabyyear=data.frame(averageseifabyyear,A,changeyears[i],file)
			averageseifa<-rbind(averageseifa,averageseifabyyear)

			#output3 - catchment
			catchment=data.frame(catchment,A,changeyears[i],file)
			catchmentlist<-rbind(catchmentlist,catchment)
			catchmentlist2<-melt(catchmentlist, id=c("changeyears.i.","file","A","origin"))


			#output4 - unallocated students
			availstudents=data.frame(availstudents,A,changeyears[i],file)
			unallocstudent<-rbind(unallocstudent,availstudents)

			#output5 - unallocated capacity
			newcapacityofyear=data.frame(newcapacityofyear,A,changeyears[i],file)
			unalloccap<-rbind(unalloccap,newcapacityofyear)
			
			
		} #end change year loop
	} #end inputfile loop
	
	# print(catchmentlist)
	# return(catchmentlist)
	# return(catchmentlist,unallocstudent)
	# print(unallocstudent)
	# print(unalloccap)
	# print(averagett)
	# print(averageseifa)
	write.table(catchmentlist2, "catchment.csv", row.names=FALSE, sep=",")
	write.table(unallocstudent, "unallocatedstudents.csv", row.names=FALSE, sep=",")
	write.table(unalloccap, "unallocatedcapacity.csv", row.names=FALSE, sep=",")
	write.table(averagett, "averagetraveltime.csv", col.names=TRUE, sep=",")
	write.table(averageseifa, "averageseifa.csv", col.names=TRUE, sep=",")
	print('process complete')
}


