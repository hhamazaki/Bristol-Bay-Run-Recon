//===================================================================
// Filename:	Syrah.tpl
//
// Description: Annual run reconstruction model for Bristol Bay Alaska. Data required include annual estimates of catch, escapement, age composition and
//                genetic composition of catch
//
// History:
//  2012/02/23	created by: curryc2 - Curry James Cunningham, School of Aquatic and Fishery Sciences, Univeristy of Washington
//  2012/03/01  Adapted from the original SYRAH Annual reconstruction model designed by Dr. Trevor Branch (SAFS, UW)
//  2012/03/16  Added additional POISSON likelihoods for total catch and total escapement
//  2012/03/26  Problem with escapement likelihoods is that negative values are being generated... MUST FIX
//
// NOTES:
//  1) Data file VERSION numbers: 1.0 = WestSide
//                                2.0 = EastSide
//
//  2) Distric ID's:  WestSide: 1 = Nushagak Distric
//					  EastSide: 1 = Naknek/Kvichak District
//                              2 = Egegik District
//                              3 = Ugashik District
//
//  3) Set Terminal directory ##### cd documents/"curry's syrah work"/"syrah annual"/syrah #####
//
//  4) Run the file ##### ./syrah -ind WestSide_2008.dat #####
//                  ##### ./syrah -ind datFiles/WestSide_2008.dat -nohess ##### -NEW
//
//  5) Copy outputFile to correct location 
//       ##### cp SYRAH_annual.out outputFiles/WestSide/WestSide__2008.out #####
//
//  6) Running BAT files:
//       a) Navigate to correct folder: ##### cd documents/"curry's syrah work"/"syrah annual"/syrah ##### 
//                REMEMBER TO RESET THE FILE NUMBER 1.0 TO 2.0 FOR EastSide
//       b) Make a executable of bash command file IN COMMAND LINE: ##### chmod u+x RunWestSidePRELIM.sh #####
//       c) Run the executable: ##### sh RunEastSide.sh #####
//                              ##### sh RunWestSidePRELIM.sh #####
//  
//  7) Remedies for the EastSide
//      a) Log-normal - divide all gen and AC sample sizes by 100 - NOT EFFECTIVE
//           produced worse fits all around. 
//
//  8) Iterative runs, with updated RunSize estimates allows for convergence to be achieved in all years
//       in the WestSide (poisson or log-normal) and all new years EastSide except 2009.   
//
//  9) NOTE: Running the .sh (batch) files is inefficient, this should be summarized in a R script using system()
//
// THINGS I LEARNED ABOUT ADMB:
//  1) "!!"Êonly necessary prior to if() statements or otherwise in DATA_SECTION and PARAMETER_SECTION
//  2) If you want to have a vector of phases (phz) for init_bounded_number_vector(1,..,lb,ub,phz),
//       phz must be of type " ivector phz(1,...); "
//  3) At the beginning of the REPORT_SECTION be sure to specifiy PRECISION with "report1.precision(10)"
//       This was an issue as we are dealing in thousands of fish and sometimes the remainder was removed... NOT COOL. 
//===================================================================


DATA_SECTION
  int debug;
  !!debug=0;  //0=no debugging output, 1=debug data section, 2=higher level calls, 3=within RunModel(),
              //4=within CalcAgeComp(), 5=within CalkeLikelihoods()
  int catEscError;
  !!catEscError=1;  //0=Poisson, 1=Log-normal specify, 2=Log-normal MLE est Sigma
  !!if(catEscError != 0 & catEscError != 1 & catEscError != 2) { cout<< "##### ERROR: LIKELIHOOD ERROR DIST FOR CAT. AND ESC. NOT AVAILABLE" <<endl; exit(1); }
  
  int contCatchType;
  !!contCatchType=0;  //0=exponentiated availability, 1=non-exponentiated avail.
  
  init_number version;  //Version number of data input file
  !!if(version != 1.0 & version != 2.0) { cout<< "##### ERROR: SYRAH VERSION 1.0 DOES NOT MATCH, DATA FILE VERSION IS: " << version <<endl; exit(1); }
  //DATA READ IN FROM THE DATA FILE
  //constants for array dimensions
  init_int year;  //Year of reconstruction
  !!cout<< "RECONSTRUCTING YEAR: " << year << endl;
  init_int NDISTRICTS;  //Number of districts
  !!if(debug == 1) { cout<< "NDISTRICTS: " << NDISTRICTS <<endl; }
  init_int NGROUPS;  //Number of groups = NGROUPS*NAGECOMPS
  !!if(debug == 1) { cout<< "NGROUPS: " << NGROUPS <<endl; }
  init_int NSTOCKS;  //Number of stocks (=rivers) 
  !!if(debug == 1) { cout<< "NSTOCKS: " << NSTOCKS <<endl; } 
  init_int NAGECOMPS;  //Number of age composition classes reported
  !!if(debug == 1) { cout<< "NAGECOMPS: " << NAGECOMPS <<endl; }
  init_vector AgeCompLabels(1,NAGECOMPS);  //Names of the different age comp groups
  init_vector GENdata(1,NDISTRICTS);  //Flag for genetic data: 1 = Data available, -1 = Data unavailable
  init_matrix GeneticFracData(1,NDISTRICTS,1,NSTOCKS+2);  //Genetic composition of catch data 
  init_matrix AgeCompCATCH(1,NDISTRICTS,1,NAGECOMPS+2);  //Age comp data for catches
  init_matrix AgeCompESC(1,NSTOCKS,1,NAGECOMPS+2);  //Age comp for escapments
  !!if(debug == 1) { cout<< "AgeCompESC" <<endl; cout<< AgeCompESC <<endl; }
  init_vector catchData(1,NDISTRICTS);  //Catch data for each district
  init_vector escData(1,NSTOCKS);  //Escapement data for each stock
  init_int NSELECTPAR;  //Number of selectivity parameters
  init_matrix TempSelectivity(1,NSELECTPAR,1,5);  //Selectivity to estimate for AGE GROUPS NOT GROUPS age groups are 1.2, 1.3, 2.2, 2.3 ect...  
  init_int NAVAILPAR;  //Number of availablility parameters = n.districts*n.stocks
  !!if(debug == 1) { cout<< "NAVAILPAR: " << NAVAILPAR <<endl; }
  init_matrix TempAvailability(1,NAVAILPAR,1,5);  //Availability to estimate for stocks in districts (i.e. Alagnak in Egegik District)
  init_matrix TempRunSize(1,NGROUPS,1,5);  //Input values for total run size estimates
  init_matrix groupCodes(1,NGROUPS,1,5);  //Reference codes for individual model groups
  !!if(debug == 1) { cout<< "groupCodes MATRIX" <<endl; cout<< groupCodes <<endl; }
  
  //Variance values for logNormal Likelihood
  init_number temp_sigmaCat;
  init_number temp_sigmaEsc;
  
  init_int testCode; //Checks to ensure data was read in correctly
  !!if(debug == 1) { cout<< "testCode Value: " << testCode << " SHOULD equal 12345" <<endl; }
  !!if(testCode != 12345) { cout<< "##### ERROR: testCode DOES NOT MATCH CORRECTLY, DATA NOT BEING READ IN CORRECTLY" <<endl; exit(1); }
  !!if(debug == 2) { cout<< "DATA_SECTION Complete" <<endl;}
  !!if(debug == 1) { exit(1); }
  //Parameter estimation control variables
  vector lbRun(1,NGROUPS);  //Lower Bound variables
  vector lbAvail(1,NAVAILPAR);
  vector lbSel(1,NSELECTPAR);
  vector ubRun(1,NGROUPS);  //Upper Bound variables
  vector ubAvail(1,NAVAILPAR);
  vector ubSel(1,NSELECTPAR);
  ivector phzRun(1,NGROUPS);  //Phase variables
  ivector phzAvail(1,NAVAILPAR);
  ivector phzSel(1,NSELECTPAR);
  vector startRun(1,NGROUPS);
  vector startAvail(1,NAVAILPAR);
  vector startSel(1,NSELECTPAR);
  

  
PARAMETER_SECTION
  objective_function_value negLogLike;
  //number test(1);
  //!!test=10;
  //Initialize 
 LOC_CALCS
  int i;  //Counter variable
  
  for(i=1;i<=NGROUPS;i++) {
    phzRun(i)=TempRunSize(i,2);
    startRun(i)=log(TempRunSize(i,3) + 1e-6);
    lbRun(i)=log(TempRunSize(i,4) + 1e-6);
    ubRun(i)=log(TempRunSize(i,5) + 1e-6);	
  }
  for(i=1;i<=NAVAILPAR;i++) {
    phzAvail(i)=TempAvailability(i,2);
    startAvail(i)=TempAvailability(i,3);
    lbAvail(i)=TempAvailability(i,4);
    ubAvail(i)=TempAvailability(i,5);
  }
  for(i=1;i<=NSELECTPAR;i++) {
    phzSel(i)=TempSelectivity(i,2);
    startSel(i)=TempSelectivity(i,3);
    lbSel(i)=TempSelectivity(i,4);
    ubSel(i)=TempSelectivity(i,5);
  }
  
 END_CALCS
  init_bounded_number_vector ln_RunSize(1,NGROUPS,lbRun,ubRun,phzRun);
  vector RunSize(1,NGROUPS);
  init_bounded_number_vector Availability(1,NAVAILPAR,lbAvail,ubAvail,phzAvail);
  init_bounded_number_vector Selectivity(1,NSELECTPAR,lbSel,ubSel,phzSel);
  
  //Parameters linked to groups
  vector SelectivityGroup(1,NGROUPS);
  matrix AvailabilityGroup(1,NGROUPS,1,NDISTRICTS);
  matrix catchByGroup(1,NGROUPS,1,NDISTRICTS);  //Used often
  vector escByGroup(1,NGROUPS);
  
  
  //vector catchableFishInDistrict(1,NDISTRICTS);
  //vector harvestRate(1,NDISTRICTS);
  
    //Variance values for logNormal Likelihood
  number sigmaCat;
  number sigmaEsc;
  
  //Negative Log Likelihood Parameters
  number NLLagecompCatch;
  number NLLagecompEsc;
  number NLLescape;
  number NLLcatch;
  number NLLgenetics;
  number NLLselOne;  //Likelihood Ensuring that availability parameters average to 1
  number NLLavailOne;  //Likelihood ensuring that availability parameters average to 1
  number NLLextra;  //Likelihood for posfun() and ensuring esc parameters > 0
  
  //Predicted numbers for catch and escapement
  vector catchTotal_pred(1,NDISTRICTS);
  vector escTotal_pred(1,NSTOCKS);
  matrix catchAgeComp_pred(1,NDISTRICTS,1,NAGECOMPS);
  matrix escAgeComp_pred(1,NSTOCKS,1,NAGECOMPS);
  vector catchByStock(1,NSTOCKS);
  matrix catchGenComp_pred(1,NDISTRICTS,1,NSTOCKS);
  
  //Fishing mortality rate to be calculated
  vector Fmort(1,NDISTRICTS);
  
  !!if(debug == 2) { cout<< "PARAMETER_SECTION Complete" <<endl;}
  
PRELIMINARY_CALCS_SECTION
  int d;  //Counter for districts
  int g;  //Counter for groups
  int s;  //Counter for stocks
  double tempSum;  //Temporary holder of sums;
  int i;  //General counter
  
  //Rationalize GeneticFracData - DATA STANDARDIZATION
  
  for(d=1;d<=NDISTRICTS;d++) {
    if(GENdata(d) == 1) {
      tempSum=0;
      for(s=1;s<=NSTOCKS;s++) {
        tempSum+=GeneticFracData(d,s+2);  
      }	
      for(s=1;s<=NSTOCKS;s++) {
        GeneticFracData(d,s+2)=GeneticFracData(d,s+2)/tempSum;
      }
    } 	
  }//next d
  
  //Fix starting values for predicted variables
  for(i=1;i<=NGROUPS;i++) {
    ln_RunSize(i)=startRun(i);
  }
  for(i=1;i<=NAVAILPAR;i++) {
    Availability(i)=startAvail(i);
  }
  for(i=1;i<=NSELECTPAR;i++) {
    Selectivity(i)=startSel(i);
  }  
  
  if(debug == 2) { cout<< "PRELIMINARY_CALCS_SECTION Complete" <<endl;}
  
PROCEDURE_SECTION
  //Run Model Functions
  if(debug == 2) { cout<< "Start of PROCEDURE_SECTION" <<endl;}
  InitializeVariables();
  if(debug == 2) { cout<< "After InitializeVariables in RunModel" <<endl; }
  RunModel();
  if(debug == 2) { cout<< "After RunModel in CalcAgeComp" <<endl; }
  CalcAgeComp();
  if(debug == 2) { cout << "After CalcAgeComp in CalcGeneticComp" <<endl; }
  CalcGeneticComp();
  if(debug == 2) { cout<< "After CalcGeneticComp in CalcLikelihoods" <<endl; }
  CalcLikelihoods();
  if(debug == 2) { cout<< "PROCEDURE_SECTION Complete" <<endl;}

FUNCTION InitializeVariables
  int d;  //Counter for districts
  int ac;  //Counter for age comp groups
  int s;  //Counter for stocks
  int g;  //Counter for groups
  
  //NEW 2021: Tranform Log RunSize
  RunSize=exp(ln_RunSize);

  //Objective Function Value
  negLogLike=0.0;
  //Negative Log Likelihood Parameters
  NLLagecompCatch=0.0;
  NLLagecompEsc=0.0;
  NLLescape=0.0;
  NLLcatch=0.0;
  NLLgenetics=0.0;
  NLLselOne=0.0;
  NLLavailOne=0.0;
  NLLextra=0.0;
  //Variance parameters
  sigmaCat=0.0;
  sigmaEsc=0.0;
  
  //Predicted numbers for catch and escapement
  for(d=1;d<=NDISTRICTS;d++) {
    Fmort(d)=0;
    catchTotal_pred(d)=0;
    for(ac=1;ac<=NAGECOMPS;ac++) {
      catchAgeComp_pred(d,ac)=0;
    }
  }
  for(s=1;s<=NSTOCKS;s++) {
    escTotal_pred(s)=0;
    catchByStock(s)=0;
    for(ac=1;ac<=NAGECOMPS;ac++) {
      escAgeComp_pred(s,ac)=0;
    }
  }
  //Predicted genetic composition of catch
  for(d=1;d<=NDISTRICTS;d++) {
    for(s=1;s<=NSTOCKS;s++) {
      catchGenComp_pred(d,s)=0;
    }// next s
  }//next d
  for(g=1;g<=NGROUPS;g++) {
    escByGroup(g)=0;
  }//next g
  
  //Parameters linked to groups - Maybe should be above
  for(g=1;g<=NGROUPS;g++) {
    //SelectivityGroup(g)=0;
    //tempSelPointer=groupCodes(g,4);
    SelectivityGroup(g)=Selectivity(groupCodes(g,4));
    for(d=1;d<=NDISTRICTS;d++) {
      //AvailabilityGroup(g,d)=0;
      //tempStockID=groupCodes(g,3);
      AvailabilityGroup(g,d)=Availability(NDISTRICTS*(groupCodes(g,3)-1)+d);
      catchByGroup(g,d)=0;
    }//next d
  }//next g
          
          
//============================================================================================
//Function calculates catch and escapement by first using 20 NR iterations to estimate
//  Fmort (fishing mortality rate) in each district
//============================================================================================

FUNCTION RunModel
  dvariable FishInArea;  //Total number of fish modeled
  dvariable catchableFishInArea;  //NEW 3.26.12
  dvariable tempHarvestRate;  //NEW 3.26.12
  dvariable catchModified;  //NEW 3.26.12
  //dvar_vector Ffinal(1,NDISTRICTS);  //Fishing mortality by district
  dvariable F0;
  int nNewton;  //Counter for Newton-Raphson iterations
  int g;  //Counter for group
  int d;  //Counter for districts
  int tempStockID;  //Temporary stockID
  int tempSelPointer;  //Temporary selectivity pointer
  dvariable numerator;
  dvariable denominator;
  dvariable tempSumCatch;  //Temporary sum for catch across districts for a particular group (used in ESC Calc)
  
  FishInArea=sum(RunSize);
  //if(debug == 3) { cout<< "FishInArea" << FishInArea <<endl; }
  //Newton-Raphson preliminary aproximation for district specific fishing mortality
  for(d=1;d<=NDISTRICTS;d++) {
    //Fmort(d)=0;
    if(FishInArea < 1e-10) {
      Fmort(d)=0;
      FishInArea=1e-06/(2.0-FishInArea/1e-06);  //Borrowed from Trevor's original SYRAH code
      cout<< "##### ERROR NO FISH IN MODEL" <<endl;
    }
    else {
      if(debug == 3) { cout<< "District: " << d <<" of " << NDISTRICTS <<endl; }
      if(debug == 3) { cout<< "FishInArea: " << FishInArea <<endl; }
      if(debug == 3) { cout<< "catchData: " << catchData(d) <<endl; }
      //NEW 3.26.12
      tempHarvestRate=catchData(d)/FishInArea;
      if(tempHarvestRate <= 0.95) {
        catchModified=tempHarvestRate;
        //cout<<"TRUE"<<endl;
        //cout<<"tempHarvestRate: "<<tempHarvestRate<<endl;
        
      }
      else {
        catchModified=1.0-0.0025/(tempHarvestRate-0.9);  //Corrects without using the posfun() - MUCH FASTER!
        //cout<<"FALSE"<<endl;
      }
      catchModified *= FishInArea;
      //cout<<"catchModified: "<<catchModified<<endl;
      //Calculate catchable fish in area
      catchableFishInArea=0;
      for(g=1;g<=NGROUPS;g++) {
        if(contCatchType == 0) {
          catchableFishInArea += AvailabilityGroup(g,d)*SelectivityGroup(g)*RunSize(g);
        }
        else {
          catchableFishInArea += AvailabilityGroup(g,d)*SelectivityGroup(g)*RunSize(g);
        }
      }
      F0=log(FishInArea/(FishInArea-catchModified))/(catchableFishInArea/FishInArea);  //First approximation of fishing mortality
      if(debug == 3) { cout<< "F0: " << F0 <<endl; }
      //F0=0.65;
      for(nNewton=1;nNewton<=20;nNewton++) {
        numerator=0;
        denominator=0;
        for(g=1;g<=NGROUPS;g++) {
          tempStockID=groupCodes(g,3);  //Set temporary stockID
          tempSelPointer=groupCodes(g,4);  //Set temporary selectivity pointer
          
          //AvailabilityGroup(g,d)=Availability(NDISTRICTS*(tempStockID-1)+d);
          //SelectivityGroup(g)=Selectivity(tempSelPointer);
          if(contCatchType == 0) {
            numerator += RunSize(g)*(1-mfexp(-1*AvailabilityGroup(g,d)*SelectivityGroup(g)*F0));
          }
          else {
            numerator += RunSize(g)*AvailabilityGroup(g,d)*(1-mfexp(-1*SelectivityGroup(g)*F0));
          }
          if(debug == 3) { cout<< "numerator: " << numerator <<endl; }
          if(contCatchType == 0) {
            denominator += RunSize(g)*AvailabilityGroup(g,d)*SelectivityGroup(g)*mfexp(-1*AvailabilityGroup(g,d)*SelectivityGroup(g)*F0);
          }
          else {
            denominator += RunSize(g)*AvailabilityGroup(g,d)*SelectivityGroup(g)*mfexp(-1*SelectivityGroup(g)*F0);
          }
          if(debug == 3) { cout<< "denominator: " << denominator <<endl; }
        }//next g
        F0+=(catchModified-numerator)/(2.0*denominator);
        if(debug == 3) { cout<< "nNewton: " << nNewton << " of 20" <<endl; cout<< "F0: " << F0 <<endl; }
        //if(d==2) { cout<<"##### d: "<<d<<endl;
        //cout<<"F0: "<<F0<<endl;}
      }//next nNewton
      Fmort(d)=F0;
    }//end if NEAR ZERO RUNSIZE
  }//next d
  if(debug == 3) { cout<< "After Newton-Raphson Calculation" <<endl; }

  //CALCULATE CATCH:
  for(g=1;g<=NGROUPS;g++) {
    tempStockID=groupCodes(g,3);  //Set temporary stockID
    tempSelPointer=groupCodes(g,4);  //Set temporary selectivity pointer
    if(debug == 3) { cout<< "g: " << g << " StockID: " << tempStockID << " SelPointer: " << tempSelPointer <<endl; }
    
    //SelectivityGroup(g)=Selectivity(tempSelPointer);  //Get selectivity for that group
    for(d=1;d<=NDISTRICTS;d++) {
      if(debug == 3) { cout<< "###d: " << d <<endl; }
      //AvailabilityGroup(g,d)=Availability(NDISTRICTS*(tempStockID-1)+d);
      
      //Calculate the Catch for a Group in a district
      if(contCatchType == 0) {
        catchByGroup(g,d)=RunSize(g)*(1-mfexp(-1*SelectivityGroup(g)*AvailabilityGroup(g,d)*Fmort(d)));
      }
      else {
        catchByGroup(g,d)=RunSize(g)*AvailabilityGroup(g,d)*(1-mfexp(-1*SelectivityGroup(g)*Fmort(d)));
      }
      catchTotal_pred(d)+=catchByGroup(g,d);
      catchByStock(tempStockID)+=catchByGroup(g,d);
    }//next d
  }//next g
  
  if(debug == 3) { cout<< "After Catch Calculation" <<endl; }
  
  //CALCULATE ESCAPEMENT: ------ ERROR is HERE
  for(g=1;g<=NGROUPS;g++) {
    tempStockID=groupCodes(g,3);
    tempSumCatch=0;
    for(d=1;d<=NDISTRICTS;d++) {
      tempSumCatch+=catchByGroup(g,d); //ERROR HERE NEED TO GO ACROSS DISTRICTS FOR A GROUP
    }
    escByGroup(g)=RunSize(g)-tempSumCatch;  //Updates predicted escapement by group
    escTotal_pred(tempStockID)+=escByGroup(g);  //Updates predicted escapement by stock
  }//next g

  //exit(1);
//============================================================================================
//Function calculates age composition proportion for CATCH and ESCAPEMENT
//============================================================================================
FUNCTION CalcAgeComp /////////////ERROR HERE LETS FIGURE THIS SHIT OUT!!!
  int g;  //Counter for groups
  int d;  //Counter for districts
  int ac;  //Counter for ageComp groups
  int s;  //Counter for stocks
  int tempAgeCompID;  //Temporary ageCompID
  int tempStockID;  //Temporary StockID
  //dvar_vector tempSumStock(1,NSTOCKS);  //Vector for temporary sums by stock
  dvar_matrix tempMat(1,NSTOCKS,1,NAGECOMPS);
  
  for(s=1;s<=NSTOCKS;s++) {
    for(ac=1;ac<=NAGECOMPS;ac++) {
      tempMat(s,ac)=0;
    }
  }
  
  //CATCH AGECOMP  
  for(d=1;d<=NDISTRICTS;d++) {
    for(g=1;g<=NGROUPS;g++) {
      tempAgeCompID=groupCodes(g,5);  //Set temporary ageCompID
      //cout<<tempAgeCompID<<endl;
      catchAgeComp_pred(d,tempAgeCompID)+=catchByGroup(g,d);
      //tempSum+=catchByGroup(g,d);
    }//next g
    //Divide all age comps in district by total catch in that district to get proportions
    for(ac=1;ac<=NAGECOMPS;ac++) {
      catchAgeComp_pred(d,ac)=(catchAgeComp_pred(d,ac)/catchTotal_pred(d));
    }//next ac
  }//next d
  
  //if(debug == 4) { cout<< "catchAgeComp_pred" <<endl; cout<< catchAgeComp_pred <<endl; } 
  
  if(debug == 4) { cout<< "escAgeComp_pred" <<endl; cout<< escAgeComp_pred <<endl; }
  if(debug == 4) { cout << "escByGroup" <<endl; cout<< escByGroup <<endl;}
  //exit(1);
  //ESCAPEMENT AGECOMP - problem is here
  for(g=1;g<=NGROUPS;g++)  {
    tempAgeCompID=groupCodes(g,5);  //Set temporary ageCompID
    tempStockID=groupCodes(g,3);  //Set temporary stockID
    escAgeComp_pred(tempStockID,tempAgeCompID)+=escByGroup(g);
    //tempSumStock(tempStockID)+=escByGroup(g);
    tempMat(tempStockID,tempAgeCompID)+=escByGroup(g);
  }//next g
  if(debug == 4) { cout<< "escAgeComp_pred" <<endl; cout<< escAgeComp_pred <<endl; } //OK to here
  
  
  
  for(s=1;s<=NSTOCKS;s++) {
    for(ac=1;ac<=NAGECOMPS;ac++) {
      escAgeComp_pred(s,ac)=(escAgeComp_pred(s,ac)/escTotal_pred(s));//ERROR HERE
      tempMat(s,ac)=(tempMat(s,ac)/escTotal_pred(s));
      //escAgeComp_pred(s,ac)=escAgeComp_pred(s,ac)/tempSumStock(s);
    }//next ac
  }//next s
  //if(debug == 4) { cout<< "tempSumStock  " << tempSumStock <<endl; }
  if(debug == 4) { cout<< "escTotal_pred  " << escTotal_pred <<endl; }
  if(debug == 4) { cout<< "escAgeComp_pred" <<endl; cout<< escAgeComp_pred <<endl; }
  if(debug == 4) { cout<< "########" <<endl; cout<< tempMat <<endl; }
  //exit(1);
//============================================================================================
//Function calculates genetic composition of catch
//============================================================================================  
FUNCTION CalcGeneticComp
  int g;  //Counter for group
  int d;  //Counter for district
  int s;  //Counter for stocks
  int tempStockID;  //Temporary stockID
  
  for(d=1;d<=NDISTRICTS;d++) {
    if(GENdata(d) == 1) {
      for(g=1;g<=NGROUPS;g++) {
        tempStockID=groupCodes(g,3);  //Set temporary stockID
        catchGenComp_pred(d,tempStockID)+=catchByGroup(g,d);
      }//next g
      for(s=1;s<=NSTOCKS;s++) {
        //Divide all stock catches in each district by total catch in that district
        catchGenComp_pred(d,s)=catchGenComp_pred(d,s)/catchTotal_pred(d);
      }//next s
    }//end if
  }//next d

//============================================================================================
//Function calculates likelihoods
//============================================================================================ 
FUNCTION CalcLikelihoods
  int s;  //Counter for stocks
  int d;  //Counter for districts
  int ac;  //Counter for age composition groups
  int g;  //Counter for groups
  dvariable tempPredProp;  //Temporary predicted proportion
  dvariable tempSS;  //Temporary variable for the sample size for age composition or genetic composition
  dvariable tempSum;  //Temporary sum for Multinomial proportions
  double pi; pi=3.14159265358979323844;  //Approximate value of pi
  //dvariable sigmaCat;  //CV for log-normal error distribuit
  //dvariable sigmaEsc;
  //sigmaCat=0.5; sigmaEsc=0.05; //0.1 WS; 0.05 ES
  //FOR IMPLEMENTATION OF POSFUN()
  dvariable pen;
  double min;
  min=1e-100;
  
  //Initialize
  sigmaCat = 0.0;
  sigmaEsc = 0.0;
  
  //CATCH
  if(catEscError == 1) {  
    sigmaCat = temp_sigmaCat;
  }else { //Log-normal MLE for sigma (2)
    for(d=1;d<=NDISTRICTS;d++) {
      sigmaCat += square(log(catchData(d)) - log(catchTotal_pred(d)))/NDISTRICTS;
    }//next d
    //cout<<"sigmaCat: " << sigmaCat <<endl;
    sigmaCat = sqrt(sigmaCat);
    //cout<<sigmaCat<<endl;
  }
  for(d=1;d<=NDISTRICTS;d++) {
    if(catEscError == 0) {
    //  Poisson distributed error
    //    1e-10 is a cludge factor to prevent log(0) errors
      NLLcatch += catchTotal_pred(d) - catchData(d)*log(catchTotal_pred(d)+(1e-10));
    }
    else {
      //  Log-normal distributed error
      //sigmaCat=0.01*catchData(d);
      NLLcatch += -1*log((1/(sigmaCat*sqrt(2*pi)))*mfexp(-1*square(log(catchData(d))-log(catchTotal_pred(d)))/(2*square(sigmaCat))));
    }
  }//next d
  
  //ESCAPEMENT
  if(catEscError == 1) {
    sigmaEsc = temp_sigmaEsc;
  }else {  //Log-normal MLE for sigma (2)
    for(s=1;s<=NSTOCKS;s++) {
      sigmaEsc += square(log(escData(s)) - log(escTotal_pred(s)))/NSTOCKS;
    }//next s
    //cout<<"sigmaEsc: " << sigmaEsc <<endl;
    sigmaEsc = sqrt(sigmaEsc);
    //cout<<sigmaEsc<<endl;
  }
  for(s=1;s<=NSTOCKS;s++) {
    //Posfun() penalty for escapement < 0
    pen=0;
    escTotal_pred(s)=posfun(escTotal_pred(s),min,pen);
    NLLextra += 1000*pen;
    if(catEscError == 0) {
    //  Poisson distributed error
    //    1e-10 is a cludge factor to prevent log(0) errors
    //    Might also look at Flynn method

      NLLescape += (escTotal_pred(s) - escData(s)*log(escTotal_pred(s)+(1e-10)));
    }
    else {
    //  Log-normal distributed error
      NLLescape += -1*log((1/(sigmaEsc*sqrt(2*pi)))*mfexp(-1*square(log(escData(s))-log(escTotal_pred(s)))/(2*square(sigmaEsc))));
      if(debug == 5) { 
        cout<< "Stock: " << s <<endl;
        cout<< "Obs: " << escData(s) << " ##### Pred: " << escTotal_pred(s) <<endl;
        cout<< "NLL: " << -1*log((1/(sigmaEsc*sqrt(2*pi)))*mfexp(-1*square(log(escData(s))-log(escTotal_pred(s)))/(2*square(sigmaEsc)))) <<endl;
      }
    }
  }//next s
  
  //Catch Age Comp
  //  Multinomial likelihood
  for(d=1;d<=NDISTRICTS;d++) {
    tempSS=AgeCompCATCH(d,2);  //Set sample size for this district
    tempSum=0;
    for(ac=1;ac<=NAGECOMPS;ac++) {
      tempPredProp=catchAgeComp_pred(d,ac);
      if(tempPredProp < 1e-06) {  //Very small or near zero predicted proportion
        tempPredProp=1e-06/(2-tempPredProp/1e-06);
      }
      tempSum += AgeCompCATCH(d,2+ac)*log(tempPredProp);  //Sum for multinomial
    }//next ac
    if(catEscError == 0) { NLLagecompCatch += -1*tempSS*tempSum; }
    else{ 
      if(tempSS == 0) { NLLagecompCatch += 0; }  //Year without age composition
      else { NLLagecompCatch += -100*tempSum; } //-100*tempSum
    }
  }//next d
  
  //cout<<"####"<<endl;cout<<escAgeComp_pred<<endl;
  //Esc Age Comp
  //  Multinomial likelihood
  for(s=1;s<=NSTOCKS;s++) {
    tempSS=AgeCompESC(s,2);  //Set sample size for this stock
    tempSum=0;
    for(ac=1;ac<=NAGECOMPS;ac++) {
      tempPredProp=escAgeComp_pred(s,ac);
      
      //cout<<tempPredProp<<endl;
      
      if(tempPredProp < 1e-06) {  //Very small or near zero predicted proportion - force it to be non-zero
        tempPredProp=1e-06/(2-tempPredProp/1e-06);
        //cout<<ac<<" DOH "<<tempPredProp <<endl;
      }
      //cout<<AgeCompESC(s,2+ac)<<" log "<<tempPredProp<<endl;
      tempSum += AgeCompESC(s,2+ac)*log(tempPredProp);  //Sum for multinomial
    }//next ac
    if(catEscError == 0) { NLLagecompEsc += -1*tempSS*tempSum; }
    else { 
      if(tempSS == 0) { NLLagecompEsc += 0; }  //Year without age composition
      else { NLLagecompEsc += -100*tempSum; }
    }
  }//next s
  
  //Genetic Composition
  //  Multinomial likelihood
  for(d=1;d<=NDISTRICTS;d++) {
    if(GENdata(d) == 1) {  //Genetic data available
      tempSS=GeneticFracData(d,2);  //Set sample size for this district
      tempSum=0;
      for(s=1;s<=NSTOCKS;s++) {
        tempPredProp=catchGenComp_pred(d,s);
        if(tempPredProp < 1e-06) {  //Very small or near zero predicted genetic proportion - force it to be non-zero
          tempPredProp=1e-06/(2-tempPredProp/1e-06);
        }
        tempSum += GeneticFracData(d,2+s)*log(catchGenComp_pred(d,s)+1e-10);  //Sum for multinomial
        //tempSum += GeneticFracData(d,2+s)*log(tempPredProp);  //Sum for multinomial
      }//next s
      if(catEscError == 0) { NLLgenetics += -1*tempSS*tempSum; }
      else { NLLgenetics += -100*tempSum; }
    }
    else {  //Genetic data unavailable
      NLLgenetics += 0;
    }
  }//next d
  
  
  //Selectivity Parameters Average to 1
  //  least squares
  NLLselOne += 1000*square((sum(Selectivity)/NSELECTPAR)-1);
  
  
  //Availability Parameters Average to 1
  //  least squares
  //NLLavailOne += 1000*square((sum(Availability)/NAVAILPAR)-1);
  
  for(d=1;d<=NDISTRICTS;d++) {
    tempSum=0;
    for(s=1;s<=NSTOCKS;s++) {
      tempSum+=Availability(NDISTRICTS*(s-1) + d);  
    }//next s
    NLLavailOne+=1000*square(tempSum/NSTOCKS-1.0);
  }//next d
  
  //NEW 3.27.12 - ADDITIONAL LIKELIHOOD
  for(g=1;g<=NGROUPS;g++) {
    pen=0;
    escByGroup(g)=posfun(escByGroup(g),min,pen);
    NLLextra += 1000*pen;
  }//next g
  //if(last_phase()) { cout << "NLLextra: " << NLLextra <<endl; }
  //==============================================================================================================
  //Total
  negLogLike = NLLcatch+NLLescape+NLLagecompCatch+NLLagecompEsc+NLLgenetics+NLLselOne+NLLavailOne+NLLextra;
  //negLogLike = NLLcatch+NLLescape+NLLagecompCatch+NLLagecompEsc+NLLgenetics+NLLextra;
  //==============================================================================================================

  
REPORT_SECTION
  if(debug == 2) { cout<< "Start of REPORT_SECTION" <<endl; }
  report1.precision(10);  //This is how we will set the precision for our reported values in the "report1" file
  int d;  //Counter for districts
  int s;  //Counter for stocks
  int ac;  //Counter for age composition groups
  int g;  //Counter for groups
  cout<<"MLEsigmaCat: "<<sigmaCat<<endl;
  cout<<"MLEsigmaEsc: "<<sigmaEsc<<endl;
  cout<<"MaxGrad: "<<objective_function_value::pobjfun->gmax<<endl;
  //Ensures .out file only written during last phase
  if(last_phase()) {
  report1 << "$ndistricts" <<endl;
  report1 << NDISTRICTS <<endl;
  report1 << "$ngroups" <<endl;
  report1 << NGROUPS <<endl;
  report1 << "$nstocks" <<endl;
  report1 << NSTOCKS <<endl;
  report1 << "$nagecomps" <<endl;
  report1 << NAGECOMPS <<endl;
  report1 << "$AgeCompLabels" <<endl;
  for(ac=1;ac<=NAGECOMPS;ac++) {
    report1 << " " << AgeCompLabels(ac);
  }//next ac
  report1 <<endl;
  report1 << "$navailpar" <<endl;
  report1 << NAVAILPAR <<endl;
  report1 << "$nselectpar" <<endl;
  report1 << NSELECTPAR <<endl;
  report1 << "$GENdata" <<endl;
  report1 << GENdata <<endl;
  report1 << "$maxGradient" <<endl;
  report1 << objective_function_value::pobjfun->gmax <<endl;
  report1 << "#LIKELIHOOD_VALUES_AND_PENALTIES" <<endl;
  report1 << "$totalNLL" <<endl;
  report1 << negLogLike <<endl;
  report1 << "$NLLcatch" <<endl;
  report1 << NLLcatch <<endl;
  report1 << "$NLLescape" <<endl;
  report1 << NLLescape <<endl;
  report1 << "$NLLagecompCatch" <<endl;
  report1 << NLLagecompCatch <<endl;
  report1 << "$NLLagecompEsc" <<endl;
  report1 << NLLagecompEsc <<endl;
  report1 << "$NLLgenetics" <<endl;
  report1 << NLLgenetics <<endl;
  report1 << "$NLLselOne" <<endl;
  report1 << NLLselOne <<endl;
  report1 << "$NLLavailOne" <<endl;
  report1 << NLLavailOne <<endl;
  report1 << "$NLLextra" <<endl;
  report1 << NLLextra <<endl;
  report1 << "$sigmaCat" <<endl;
  report1 << sigmaCat <<endl;
  report1 << "$sigmaEsc" <<endl;
  report1 << sigmaEsc <<endl;
  
  report1 << "#OBSERVED_AND_PREDICTED_VALUES" <<endl;
  report1 << "$predEsc" <<endl;  //ESCAPEMENT DATA
  for(s=1;s<=NSTOCKS;s++) {
    report1 << " " << escTotal_pred(s);
  }//next s
  report1 <<endl;
  report1 << "$obsEsc" <<endl;
  for(s=1;s<=NSTOCKS;s++) {
    report1 << " " << escData(s);
  }//next s
  report1 <<endl;
  report1 << "$predCatch" <<endl;  //CATCH DATA
  for(d=1;d<=NDISTRICTS;d++) {
    report1 << " " << catchTotal_pred(d);
  }//next d
  report1 <<endl;
  report1 << "$obsCatch" <<endl;
  for(d=1;d<=NDISTRICTS;d++) {
    report1 << " " << catchData(d);
  }//next d
  report1 <<endl;
  report1 << "$RunSize" <<endl;
  report1 << RunSize <<endl;
  //AGE COMPOSITION DATA
  //ESCAPEMENT
  report1 << "$SSageCompEsc" <<endl;  
  for(s=1;s<=NSTOCKS;s++) {
    report1 << " " << AgeCompESC(s,2);
  }//next s
  report1 <<endl;
  report1 << "$obsAgeCompEsc" <<endl;
  for(s=1;s<=NSTOCKS;s++) {
    for(ac=1;ac<=NAGECOMPS;ac++) {
      report1 << AgeCompESC(s,ac+2) << " ";
    }//next ac
    report1 <<endl;
  }//next s
  report1 << "$predAgeCompEsc" <<endl;
  for(s=1;s<=NSTOCKS;s++) {
    for(ac=1;ac<=NAGECOMPS;ac++) {
      report1 << escAgeComp_pred(s,ac) << " ";
    }//next ac
    report1 <<endl;
  }//next s
  //CATCH
  report1 << "$SSageCompCatch" <<endl;
  for(d=1;d<=NDISTRICTS;d++) {
    report1 << " " << AgeCompCATCH(d,2);
  }//next d
  report1 <<endl;
  report1 << "$obsAgeCompCatch" <<endl;
  for(d=1;d<=NDISTRICTS;d++) {
    for(ac=1;ac<=NAGECOMPS;ac++) {
      report1 << AgeCompCATCH(d,ac+2) << " ";
    }//next ac
    report1 <<endl;
  }//next d
  report1 << "$predAgeCompCatch" <<endl;
  for(d=1;d<=NDISTRICTS;d++) {
    for(ac=1;ac<=NAGECOMPS;ac++) {
      report1 << catchAgeComp_pred(d,ac) << " ";
    }//next ac
    report1 <<endl;
  }//next d
  //GENETIC FRACTION DATA
  report1 << "$genSS" <<endl;  //Genetic Sample Size
  for(d=1;d<=NDISTRICTS;d++) {
    report1 << " " << GeneticFracData(d,2);
  }// next d
  report1 <<endl;
  report1 << "$obsGenComp     #GeneticFracData" <<endl;
  for(d=1;d<=NDISTRICTS;d++) {
    for(s=1;s<=NSTOCKS;s++) {
      report1 << GeneticFracData(d,s+2) << " ";
    }//next s
    report1 <<endl;
  }//next d
  report1 << "$predGenComp     #catchGenComp_pred" <<endl;
  for(d=1;d<=NDISTRICTS;d++) {
    for(s=1;s<=NSTOCKS;s++) {
      report1 << catchGenComp_pred(d,s) << " ";
    }//next s
    report1 <<endl;
  }//next d
  report1 << "#PARAMETERS_OF_INTEREST" <<endl;
  report1 << "$catchByGroup" <<endl;
  for(g=1;g<=NGROUPS;g++) {
    for(d=1;d<=NDISTRICTS;d++) {
      report1 << catchByGroup(g,d) << " ";
    }//next d
    report1 <<endl;
  }//next g
  report1 << "$escByGroup" <<endl;
  report1 << escByGroup <<endl;
  report1 << "$catchByGroup" <<endl;
  for(d=1;d<=NDISTRICTS;d++) {
    for(g=1;g<=NGROUPS;g++) {
      report1 << catchByGroup(g,d) << " ";
    }
    report1 <<endl;
  }
  report1 << "$Fmort" <<endl;
  report1 << Fmort <<endl;
  report1 << "#ESTIMATED_PARAMETERS" <<endl;
  report1 << "$availability" <<endl;
  
  
  for(d=1;d<=NDISTRICTS;d++) {
    for(s=1;s<=NSTOCKS;s++) {
      report1 << Availability((s-1)*NDISTRICTS + d) << " ";
    }//next s
    report1 <<endl;  
  }//next d
  report1 << "$availabilityVector" <<endl;
  report1 << Availability <<endl;
  report1 << "$selectivity" <<endl;
  report1 << Selectivity <<endl;
  report1 << "$RunSize" <<endl;
  report1 << RunSize <<endl;

  }//end Last_Phase() 
  
  
  if(debug == 2) { cout<< "REPORT_SECTION Complete" <<endl; }

TOP_OF_MAIN_SECTION
  arrmblsize = 500000000;  //to ensure enough memory for large arrays - size of array you can create.
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(50000000); //related to minimization function
  gradient_structure::set_CMPDIF_BUFFER_SIZE(150000000);
  gradient_structure::set_MAX_NVAR_OFFSET(500); //
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(500); // max number of variables allowed in model
  
RUNTIME_SECTION
  convergence_criteria 1e-06, 1e-06, 1e-06, 1e-06, 1e-06
  maximum_function_evaluations 1e+06, 1e+06, 1e+06, 1e+06, 1e+06   //number of function calls in the first, second etc phases - make small numbers 

GLOBALS_SECTION
  #include <admodel.h>
  
  ofstream report1("SYRAH_annual.out");
  
