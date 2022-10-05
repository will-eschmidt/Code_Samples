* I pasted the data from hd2010 into Stata from Excel 
*First, I clean the dataframe to reflect the goal dataset
gen year = 2010
*for consistency, I renmae unitID right away
rename unitid ID_IPEDS
drop if instcat ==1 
*that command dropped observations for universites that don't offer any bachelor;s degree

drop if stabbr != "TN"
*drops all states except Tennessee 

gen public = 0 
replace public = 1 of control = 1
*creates a dummy variable indicating if the univeristy is publicly owned or not 

gen degree_bach = 0 
replace degree_bach = 1 if instcat == 2
replace degree_bach = 1 if instcat == 3
*this creates a dummy variable for if the insitution grants bachelor's degrees

*Now, we have the variables we need from this dataset so we drop the rest
drop city stabbr zip iclevel control hloffer ugoffer groffer deggrant locale instcat instsize 

*rename frames for consistency as we will be switching between frames 
frame rename default hd2010 

*create frame for hd2011 and then switch to it. This saves space and RAM then trying to append the data the manual way.  
frame create hd2011
frame change hd2011

*paste hd2011 into my new, blank workspace - like before
*The process is much the same as it was for hd2010 

gen year = 2011
rename unitid ID_IPEDS
drop if instcat ==1 
drop if stabbr != "TN"
 
gen public = 0 
replace public = 1 of control = 1

gen degree_bach = 0 
replace degree_bach = 1 if instcat == 2
replace degree_bach = 1 if instcat == 3

*Now, we have the variables we need from this dataset so we drop the rest
drop city stabbr zip iclevel control hloffer ugoffer groffer deggrant locale instcat instsize 

*Now we do the same for hd2012 
frame create hd2012 
frame change hd2012
gen year = 2012
rename unitid ID_IPEDS
drop if instcat ==1 
drop if stabbr != "TN"
 
gen public = 0 
replace public = 1 of control = 1

gen degree_bach = 0 
replace degree_bach = 1 if instcat == 2
replace degree_bach = 1 if instcat == 3
drop city stabbr zip iclevel control hloffer ugoffer groffer deggrant locale instcat instsize 

*For 2013
frame create hd2013 
frame change hd2013
*After we past the hd2013 data into the data editor
gen year = 2013

rename unitid ID_IPEDS
drop if instcat ==1 
drop if stabbr != "TN"
 
gen public = 0 
replace public = 1 of control = 1

gen degree_bach = 0 
replace degree_bach = 1 if instcat == 2
replace degree_bach = 1 if instcat == 3
drop city stabbr zip iclevel control hloffer ugoffer groffer deggrant locale instcat instsize 

*For 2014:

frame create hd2014
frame change hd2014
*after we paste the data from hd2014 in this file, we clean the data
gen year = 2014
rename unitid ID_IPEDS
drop if instcat ==1 
drop if stabbr != "TN"
 
gen public = 0 
replace public = 1 of control = 1

gen degree_bach = 0 
replace degree_bach = 1 if instcat == 2
replace degree_bach = 1 if instcat == 3
drop city stabbr zip iclevel control hloffer ugoffer groffer deggrant locale instcat instsize 

*For 2015
frame create hd2015
frame change hd2015
*After we paste the data from hd2015 we clean the data
gen year = 2015
rename unitid ID_IPEDS
drop if instcat ==1 
drop if stabbr != "TN"
 
gen public = 0 
replace public = 1 of control = 1

gen degree_bach = 0 
replace degree_bach = 1 if instcat == 2
replace degree_bach = 1 if instcat == 3
drop city stabbr zip iclevel control hloffer ugoffer groffer deggrant locale instcat instsize 

*Finally we create one final frame for the financial aid data set
frame create sfa1015
frame change sfa1015
rename unitid ID_IPEDS
drop if stabbr != "TN"
*After pasting the data from sfa1015 excel file into the new, blank data editor, we can now begin to merge and link the data
*first we clean it just ever so slightly
rename unitid ID_IPEDS
drop if stabbr != "TN"
*then change back to hd2010 and begin linking
frame change hd2010 
frlink 1:1 ID_IPEDS, frame(sfa1015)
*This matches all observations from ID_Ipeds to it's matching ID_IPEDS classfier in the sfa1015 frames
frget scugffn2010 scugffn2011 scugffn2012 scugffn2013 scugffn2014 scugffn2015 fgrnt_a2010 fgrnt_a2011 fgrnt_a2012 fgrnt_a2013 fgrnt_a2014 fgrnt_a2015 sgrnt_a2010 sgrnt_a2011 sgrnt_a2012 sgrnt_a2012 sgrnt_a2013 sgrnt_a2014 sgrnt_a2015, from(sfa1015)

*This brings in all the years worth of college enrollment, federal grant money, and state/local grant money over the period of our panel

reshape long scugffn fgrnt_a sgrnt_a, i(ID_IPEDS) j(year)

*now we bring in all the other years of hd dataframe
frlink m:1 ID_IPEDS, frame(hd2011)
frlink m:1 ID_IPEDS, frame(hd2012)
frlink m:1 ID_IPEDS, frmae(hd2013)
frlink m:1 ID_IPEDS, frame(hd2014)
frlink m:1 ID_IPEDS, frmae(hd2015)

*Now we can eliminate all observations that weren't linked to an ID value in 2010 to create a truly balanced panel 

drop if hd2011 = .
drop if hd2012 = .
drop if hd2013 = .
drop if hd2014 = .
drop if hd2015 = .

*finally, we drop the sfa1015 link variable that was created
drop sfa1015

*Next we fix our variables as our grant variables only measure averages per student and not totals
gen total_federal = grant_federal * enroll_ftug
gen total_state = grant_state * enroll_ftug
 
*Now we rename our variables to better match the variables in the requested data set
rename scugffn enroll_ftug
rename total_federal grant_federal
rename total_state grant_state

*save the dataset! We're finished

perserve

*Create graph number 1 

bysort year public degree_bach: egen mean_grant_state = mean(grant_state)
graph twoway (line mean_grant_state year if degree_bach == 1 & public == 1) (line mean_grant_state year if degree_bach == 1 & public == 0) (line mean_grant_state year if degree_bach == 0 & public == 1) (line mean_grant_state year if degree_bach == 0 & public == 0)
*restore the data to original condition
restore
*Create graph number 2
perserve
bysort year public degree_bach: egen mean_enroll = mean(enroll_ftug)
graph twoway (line mean_enroll year if degree_bach == 1 & public == 1) (line mean_enroll year if degree_bach == 1 & public == 0) (line mean_enroll year if degree_bach == 0 & public == 1) (line mean_enroll year if degree_bach == 0 & public == 0)

restore

*run fixed effects regression by first setting a time variable 
xtset ID_IPEDS year
xtreg enroll_ftug public degree_bach grant_state grant_federal, fe

*you can try individal regression with a dummy for each insitution too that is equivilant to fixed effects as 
*xi: regress enroll_ftug grant_state grant_federal public degree_bach i.ID_IPEDS

*differences in differences by first creating a treatment variable
perserve  
gen promise = 0
replace promise = 1 if public == 1 & degree_bach == 0 & year == 2015
didregress (enroll_ftug) (promise), group(ID_IPEDS) time(year) wildbootstrap(rseed(111))
restore

*create the summary stats table in problem 1
perserve
collapse (sum) degree_bach public (mean) enroll_ftug grant_state grant_federal, by(year)
list, sep(6)
*then I copied and pasted it into google docs 
restore
save
*Done! End of .do file for the Blue Print labs coding assignment 




















