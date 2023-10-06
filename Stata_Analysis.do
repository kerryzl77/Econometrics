
cd "/Users/liuzikai/Desktop/EC3301 project files-20221109"
capture log close
log using "/Users/liuzikai/Desktop/EC3301 project files-20221109/200018630-Project.smcl",replace
use "/Users/liuzikai/Desktop/EC3301 project files-20221109/project crime.dta"


graph drop _all

describe

/*Exam the Dependent Variable and Decide on the Functional Form*/
label var crimerate "number of crimes committed per-person in each county"
sum crimerate
//
//     Variable |        Obs        Mean    Std. dev.       Min        Max
// -------------+---------------------------------------------------------
//    crimerate |         89    .0295536    .0169501   .0047703   .0874663

// generate crimerate2 = crimerate*100000
// number of crimes committed per-100000-person in each county
// sum crimerate2
//High standard deviation & skewedw right 

gen obs=_n 
graph twoway scatter crimerate obs, name(ScatterCrime) nodraw
hist crimerate, name(HistogramCrime) nodraw
graph combine ScatterCrime HistogramCrime, title("Crimerate Distribution")

// graph save "Graph" "/Users/liuzikai/Desktop/EC3301 project files-20221109/Crimerate_Level_Dist.gph"

generate lcrimerate = log(crimerate+1)
//Log form of number of crimes committed per-person in each county
hist lcrimerate
sktest lcrimerate
//Not pproximate Normal but better

/*Decide on the Independent Variables*/
label var prbarr "probability of arrest"
//*Expected negative correlation 
corr lcrimerate prbarr
//Correlation -0.4417 as expected
//Display negative relationship, NO OUTLIER 

label var prbconv  "probability of conviction if arrested"
//*Expected negative correlation 
corr lcrimerate prbconv
//Correlation -0.2779  as expected
//Display negative relationship, NO OUTLIER 

corr lcrimerate prbarr prbconv prbpris

scatter lcrimerate prbarr || lfit lcrimerate prbarr, name(prbarrfit) nodraw
scatter lcrimerate prbconv || lfit lcrimerate prbconv, name(prbconvfit) nodraw
graph combine prbarrfit prbconvfit, title("Prbarr and Prbconv Correlation")
// graph save "Graph" "/Users/liuzikai/Desktop/EC3301 project files-20221109/Crimerate_prbarr&prbconv.gph", replace

label var prbpris  "probability of prison sentence if convicted"
//Expected negative correlation 
corr lcrimerate prbpris
scatter lcrimerate prbpris || lfit lcrimerate prbpris, title("Prbpris Correlation") name(prbprisfit) 
//Correlation 0.1867 NOT as expected

label var polpc  "police per capita"
corr lcrimerate polpc
//*Positive relationship 0.1204
scatter lcrimerate polpc, title("Polpc Correlation")
gen polpcdum =0
replace polpcdum = 1 if polpc>0.03 
regress lcrimerate polpc polpcdum
// P polpcdum = .0561471 -- significant under 5%


label var density  "people per sq. mile"
corr lcrimerate density
scatter lcrimerate density || lfit lcrimerate density
//*Expected positive relationship and result in 0.7190


corr lcrimerate avgsen
//The correlation between crime & avg sentence days is -0.0593 so we DO NOT include
//REMOVE FROM MODEL

label var pctmin80 "percentage of population ethnic minority in 1980"
corr lcrimerate pctmin80
scatter lcrimerate pctmin80 || lfit lcrimerate pctmin80, name(pctmin80fit) nodraw
//* Expected Positive correlatuon --  corr crimerate pctmin80 = 0.134 as expected 
// scatter crimerate pctmin80 show not clear pattern of correlation
//REMOVE FROM MODEL

// corr lcrimerate pctymle
scatter lcrimerate pctymle || lfit lcrimerate pctymle, name(pctymlefit) nodraw
graph combine pctmin80fit pctymlefit ,title("Pctmin80 and Pctymle Correlation")

label var wtuc  "weekly wage in transport, utilities and communications"
label var wtrd  "weekly wage, wholesale, retail trade"
label var wfir  "weekly wage, financial, insurance, real estate"
label var wser  "weekly wage, service industry"
label var wmfg  "weekly wage, manufacturing"
label var wfed  "weekly wage, federal employees"
//* INCLUDE
label var wsta  "weekly wage, state employees"
label var wloc  "weekly wage, local govt employees"

corr crimerate wtuc wtrd wmfg wser wfir wfed wsta wloc
//             | crimer~e     wtuc     wtrd     wmfg     wser     wfir     wfed     wsta     wloc
// -------------+---------------------------------------------------------------------------------
//    crimerate |   1.0000
//         wtuc |   0.2500   1.0000
//         wtrd |   0.4287   0.2769   1.0000
//         wmfg |   0.3297   0.5127   0.3156   1.0000
//         wser |   0.3311   0.3498   0.5107   0.4102   1.0000
//         wfir |   0.3072   0.3176   0.6726   0.4727   0.4868   1.0000
//         wfed |   0.5027   0.3536   0.6389   0.4765   0.5018   0.6017   1.0000
//         wsta |   0.2428  -0.1715  -0.0304   0.0610   0.0653   0.1508   0.1891   1.0000
//         wloc |   0.3408   0.3186   0.5401   0.3954   0.4798   0.5873   0.5700   0.1000   1.0000

//TEST WEST
summarize crimerate if west == 0
summarize crimerate if west == 1
//The mean for west is  .0190199 and non-west for  .0328067
histogram crimerate if (west==0), name(Crime_east) title("Crime_east") nodraw
histogram crimerate if (west==1), name(Crime_west) title("Crime_west") nodraw
graph combine Crime_east Crime_west

//TEST CENTRAL
summarize crimerate if central == 0
summarize crimerate if central == 1
//The mean for central is  .0331671 and non-central for .0274243

//TEST Urban
summarize crimerate if urban == 0
summarize crimerate if urban == 1

describe

regress lcrimerate prbarr prbconv prbpris density pctmin80 pctymle wfed west central urban
estat vif 
estat imtest, white
estat hettest

//IMPROVED MODEL
regress lcrimerate prbarr prbconv density wfed pctymle west central 

estat vif 
//White's test for heteroskedasticity
estat imtest, white
//less general form of heteroskedaticity is Breusch- Pagan (1979) test
estat hettest

regress lcrimerate prbarr prbconv density wfed pctymle west central
predict res, r
histogram res, normal name("Log_Residual") title("Log Model Residual Distribution") nodraw
// sktest res

//Compare Linear / LOg residual
regress crimerate prbarr prbconv density wfed pctymle west central
predict res2, r
histogram res2, normal name("Linear_Residual") title("Linear Model Residual Distribution") nodraw
// sktest res2
graph combine Log_Residual Linear_Residual

//CrimerateLag and OVB test
regress lcrimerate prbarr prbconv density wfed pctymle west central crmrtelag


/*Test for interaction term*/
//Base case is central and urban
count if central == 1 & urban == 1 
count if west == 1 & urban == 1 
count if west == 1 & central == 1 

// generate noncentral_nonurban = (1-central) * (1-urban)
// generate noncentral_urban = (1-central) * urban
// generate central_nonurban = central * (1-urban)
//
// //Base case is west and urban
// generate east_nonurban = (1-west) * (1-urban)
// generate east_urban = (1-west) * urban
// generate west_nonurban = west * (1-urban)
//
// //Base case is west and central
// generate east_noncentral = (1-west) * (1-central)
// generate east_central = (1-west) * central
// generate west_noncentral = west * (1-central)
//
// regress crimerate prbarr prbconv density wfed pctymle noncentral_nonurban noncentral_urban central_nonurban
// regress crimerate prbarr prbconv density wfed pctymle east_nonurban east_urban west_nonurban
// regress crimerate prbarr prbconv density wfed pctymle east_noncentral east_central west_noncentral


log close 
exit, clear 
