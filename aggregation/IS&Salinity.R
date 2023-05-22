
MR <- list("KH2PO4" = 136.086, "NaCl" = 58.44,  "Na2HPO4" = 141.96,"Na2HPO4·7H20" = 141.96+7*18.02, "Ca(NO3)2" = 164.1 ,
           "Ca(NO3)2·4H20" = 164.1+4*18.02 ,
           "KCl" = 74.55, "MgSO4" = 120.366, "MgSO4·7H2O" = 120.366+ 7*18.02,  'NaHCO3'=84.007 , "CaCl2" =110.98 ,
           "CaCl2·2H20" =110.98+2*18.02 , "Fe(NO3)3" = 241.86,
           "Fe(NO3)3·9H20" = 241.86+9*18.02,
           "NaH2PO4" = 119.98, "NaH2PO4·H20" = 119.98+18.02,  "MgCl2" = 95.211,  "MgCl2·67H20" = 95.211+6*18.02, "Na3C6H5O7" = 258.06,
           "Na2CO3" = 105.99, "NaNO3" = 85,
           "Na2MgEDTA" = 358.50 ,"FeCl3" = 162.2, "FeCl3·6H20" = 162.2+6*18.02,  "H3BO3" = 61.83, "MnCl2" = 125.84,
           "MnCl2·4H20" = 125.84+4*18.02, "ZnSO4" = 161.47 , "ZnSO4·7H20" = 161.47+7*18.02,
           "CuSO4" = 159.61, "CuSO4·5H20" = 159.61+5*18.02,"Co(NO3)2" = 182.94, "Co(NO3)2·6H20" = 182.94+6*18.02,
           "Na2MoO4" = 205.93, "Na2MoO4·2H20" = 205.93+2*18.02, "Na2SiO3" = 122.06, "Na2SiO3·9H20" = 122.06+9*18.02, "K2HPO4" = 174.2,
           "KNO3" = 101.10, "Na2EDTA" = 372.24, "(NH4)2SO4" = 132.14,  "NH4Cl" = 53.491,  "ZnCl2" = 136.29,
           "CoCl2·6H20" = 129.84+6*18.02, "CuCl2·2H20" = 134.45+2*18.02,
           "CaSO4·2H20" = 136.14+2*18.02)#g/mol

MR_no_water <- list("KH2PO4" = 136.086, "NaCl" = 58.44,  "Na2HPO4" = 141.96,"Na2HPO4·7H20" = 141.96, "Ca(NO3)2" = 164.1 ,
                    "Ca(NO3)2·4H20" = 164.1 ,
                    "KCl" = 74.55, "MgSO4" = 120.366, "MgSO4·7H2O" = 120.366,  'NaHCO3'=84.007 , "CaCl2" =110.98 ,
                    "CaCl2·2H20" =110.98 , "Fe(NO3)3" = 241.86,
                    "Fe(NO3)3·9H20" = 241.86,
                    "NaH2PO4" = 119.98, "NaH2PO4·H20" = 119.98,  "MgCl2" = 95.211, "MgCl2·67H20" = 95.211, "Na3C6H5O7" = 258.06, "Na2CO3" = 105.99, "NaNO3" = 85,
                    "Na2MgEDTA" = 358.50 ,"FeCl3" = 162.2, "FeCl3·6H20" = 162.2,  "H3BO3" = 61.83, "MnCl2" = 125.84,
                    "MnCl2·4H20" = 125.84, "ZnSO4" = 161.47 , "ZnSO4·7H20" = 161.47,
                    "CuSO4" = 159.61, "CuSO4·5H20" = 159.61,"Co(NO3)2" = 182.94, "Co(NO3)2·6H20" = 182.94,
                    "Na2MoO4" = 205.93, "Na2MoO4·2H20" = 205.93, "Na2SiO3" = 122.06, "Na2SiO3·9H20" = 122.06, "K2HPO4" = 174.2,
                    "KNO3" = 101.10, "Na2EDTA" = 372.24 - 2*18.02,  "(NH4)2SO4" = 132.14,  "NH4Cl" = 53.491,  "ZnCl2" = 136.29,
                    "CoCl2·6H20" = 129.84, "CuCl2·2H20" = 134.45, "CaSO4·2H20" = 136.14)#g/mol

valence <- list("KH2PO4" = c(1,-1), "NaCl" = c(1,-1), "Na2HPO4" = c(1,-2),"Na2HPO4·7H20" = c(1,-2), "Ca(NO3)2" = c(2,-1) ,
                "Ca(NO3)2·4H20" = c(2,-1) ,
                "KCl" = c(1,-1), "MgSO4" = c(2,-2), "MgSO4·7H2O" = c(2,-2), "NaHCO3"= c(1,-1) , "CaCl2" =c(2,-1) ,"CaCl2·2H20" = c(2,-1) , "Fe(NO3)3" = c(3,-1),
                "Fe(NO3)3·9H20" = c(3,-1),
                "NaH2PO4" = c(1,-1), "NaH2PO4·H20" =  c(1,-1), "MgCl2" = c(2,-1), "MgCl2·67H20" = c(2,-1),"Na3C6H5O7" = c(1, -3), "Na2CO3" = c(1,-2), 
                "NaNO3" = c(1,-1),"Na2MgEDTA" = c(1,-3), "FeCl3" = c(3,-1),  "FeCl3·6H20" = c(3,-1),  "H3BO3" = c(1,-3),
                "MnCl2" = c(2,-1), "MnCl2·4H20" = c(2,-1), "ZnSO4" = c(2,-2), "ZnSO4·7H20" = c(2,-2), "CuSO4" = c(2,-2) ,
                "CuSO4·5H20" =  c(2,-2), "Co(NO3)2" = c(2,-1), "Co(NO3)2·6H20" = c(2,-1), "Na2MoO4" = c(1,-2), 
                "Na2MoO4·2H20" = c(1,-2),
                "Na2SiO3" = c(1,-2), "Na2SiO3·9H20" = c(1,-2), "K2HPO4" = c(1,-2), "KNO3" = c(1,-1), "Na2EDTA" = c(1,-2), 
                "(NH4)2SO4" = c(1,-2), "NH4Cl" = c(1,-1), "ZnCl2" = c(2,-1), "CoCl2·6H20" = c(2,-1), "CuCl2·2H20" =  c(2,-1),
                "CaSO4·2H20" = c(2,-2))



molar_yield <- list("KH2PO4" = c(1,1), "NaCl" = c(1,1),  "Na2HPO4" = c(2,1),"Na2HPO4·7H20" = c(2,1), "Ca(NO3)2" = c(1,2) ,
                    "Ca(NO3)2·4H20" = c(1,2) ,
                    "KCl" = c(1,1), "MgSO4" = c(1,1), "MgSO4·7H2O" = c(1,1),'NaHCO3'=c(1,1) , "CaCl2" = c(1,2), "CaCl2·2H20"= c(1,2) ,
                    "Fe(NO3)3" = c(1,3),
                    "Fe(NO3)3·9H20" = c(1,3),
                    "NaH2PO4" = c(1,1), "NaH2PO4·H20" =  c(1,1),"MgCl2" = c(1,2), "MgCl2·67H20" = c(1,2),"Na3C6H5O7" = c(3,1),
                    "Na2CO3" =  c(2,1), "NaNO3" = c(1,1) ,"Na2MgEDTA" = c(3,1), "FeCl3" = c(1,3),  "FeCl3·6H20" = c(1,3), 
                    "H3BO3" = c(3,1),
                    "MnCl2" = c(1,2),"MnCl2·4H20" = c(1,2), "ZnSO4" = c(1,1),"ZnSO4·7H20" =  c(1,1), "CuSO4" = c(1,1) ,
                    "CuSO4·5H20" = c(1,1), "Co(NO3)2" = c(1,2), "Co(NO3)2·6H20" = c(1,2),
                    "Na2MoO4" = c(2,1), "Na2MoO4·2H20" = c(2,1), "Na2SiO3" = c(2,1), "Na2SiO3·9H20" = c(2,1), 
                    "K2HPO4" = c(2,1), "KNO3" = c(1,1),
                    "Na2EDTA" = c(2,1), "(NH4)2SO4" = c(2,1), "NH4Cl" = c(1,1), "ZnCl2" = c(1,2), "CoCl2·6H20" = c(1,2),
                    "CuCl2·2H20" =  c(1,2), "CaSO4·2H20" = c(1,1))

media <- list("PBS" = list("KH2PO4" = 144,  "NaCl" = 9000, "Na2HPO4·7H20" = 795),
              "RPMI1640" = list("Ca(NO3)2·4H20" = 100, "KCl" = 400 , "MgSO4" = 49, "NaCl"  = 6000,
                                "NaHCO3" = 2000, "Na2HPO4" = 800),
              
              "DMEM" = list("CaCl2" = 200, "Fe(NO3)3·9H20" = 0.1, "KCl" = 400, "MgSO4" = 97.67, 
                            "NaCl"  = 6400,"NaHCO3" = 3700, "NaH2PO4·H20" = 125),
              
              "L-15" = list("CaCl2" = 140, "MgCl2" = 93.7, "MgSO4" = 97.67,
                            "KCl" = 400, "KH2PO4" = 60, "NaCl" = 8000, "NaH2PO4" = 190),
              
              "EMEM" = list("CaCl2·2H20" = 200, "MgSO4" = 97.67, "KCl" = 400,
                            "NaCl" = 6800, "NaH2PO4" = 122),
              
              "M199" = list("CaCl2" = 200, "Fe(NO3)3·9H20" = 0.7, "MgSO4" = 97.67,
                            "KCl" = 400, "NaCl" = 6800, "NaH2PO4·H20" = 140),
              
              "Dryls_Buffer" = list("Na3C6H5O7" = 516.1, "NaH2PO4" = 239.96, 
                                    "Na2HPO4" = 141.96, "CaCl2" = 166.47),
              
              "E3" = list("NaCl" = 292.2, "KCl" = 12.67, "CaCl2" = 36.6,
                          "MgSO4" = 39.72),
              
              "BG-11" = list( "Na2CO3" = 25, "NaNO3" = 1500,"Na2MgEDTA" = 0.1,  "CaCl2" = 24.2, "FeCl3·6H20" = 6, "MgSO4·7H2O" = 75,
                              "K2HPO4" = 39 , "H3BO3" = 2.86,
                              "MnCl2·4H20" = 1.86, "ZnSO4·7H20" = 0.22 , "CuSO4·5H20" =  0.06,  "Co(NO3)2·6H20" =  0.05,
                              "Na2MoO4·2H20" = 0.39),
              
              "SM7" = list("NaHCO3" = 64.8, "NaNO3" = 0.274,  "KCl" = 5.8, "CaCl2·2H20" = 293.8, "MgSO4·7H2O" = 123.3,
                           "K2HPO4" = 0.184 , "KH2PO4" = 0.143,  "Na2SiO3·9H20" = 10  ),
              
              "ILM" = list("KNO3" = 350,"Ca(NO3)2·4H20" = 295, "KH2PO4" = 90, "K2HPO4" = 12.6 , "MgSO4·7H2O" = 100, "H3BO3" =  0.12,
                           "ZnSO4·7H20" = 0.18, "Na2MoO4·2H20" = 0.044, "MnCl2·4H20" = 0.18,
                           "FeCl3·6H20" = 0.76,   "Na2EDTA" = 1.5),
              
              "MMD" = list("K2HPO4" = 700, "KH2PO4" = 200, "(NH4)2SO4" = 660,
                           "Na3C6H5O7" = 500, "MgSO4·7H2O" = 100),
              
              "ISO_8692" = list("NaHCO3" = 50, "NH4Cl" = 15, "MgCl2·67H20" = 12,
                                "CaCl2" = 18, "MgSO4·7H2O" = 15, "K2HPO4" = 1.6,
                                "FeCl3·6H20" = 0.08, "Na2EDTA" = 0.1, "H3BO3" = 0.185,
                                "MnCl2·4H20" = 0.415, "ZnCl2" = 0.003, "CoCl2·6H20" = 0.0015, "Na2MoO4·2H20" = 0.007,
                                "CuCl2·2H20" = 0.0001),
              
              "EPA_MHW" = list("NaHCO3"=96, "CaSO4·2H20"=60, "MgSO4"=60, "KCl"=4)) #mg/L


IS <- list("PBS" = 0,  "RPMI1640" = 0, "DMEM" = 0, "L-15" = 0,  "EMEM" = 0,
           "M199" = 0, "Dryls_Buffer" = 0, "E3" = 0,  "BG-11" = 0, "SM7"  = 0,  "ILM" = 0, "MMD" = 0, "ISO_8692" = 0,
           "EPA_MHW"=0)

Salinity <- list("PBS" = 0,  "RPMI1640" = 0, "DMEM" = 0,"L-15" = 0,  "EMEM" = 0, 
                 "M199" = 0, "Dryls_Buffer" = 0, "E3" = 0, "BG-11" = 0, "SM7"  = 0,  "ILM" = 0, "MMD" = 0, "ISO_8692" = 0,
                 "EPA_MHW"=0)

Hardness <- list("PBS" = 0,  "RPMI1640" = 0, "DMEM" = 0,"L-15" = 0,  "EMEM" = 0, 
                 "M199" = 0, "Dryls_Buffer" = 0, "E3" = 0, "BG-11" = 0, "SM7"  = 0,  "ILM" = 0, "MMD" = 0, "ISO_8692" = 0,
                 "EPA_MHW"=0)

Ion_con <- list("PBS" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "RPMI1640" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "DMEM" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0),
                "L-15" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0),
                "EMEM" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "M199" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0),
                "Dryls_Buffer" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "E3" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "BG-11" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "SM7"  = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "ILM" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "MMD" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0), 
                "ISO_8692" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0),
                "EPA_MHW" = list("+1" = 0, "+2" = 0, "+3"= 0,"-1" = 0, "-2" = 0, "-3" = 0))


ion_charge<- list("K" = 1,  "Na" = 1, "NH4"  = 1 , "Mg" = 2, "Ca" = 2, "Zn" = 2, "Cu" = 2, "Co" = 2,"Al" = 3,  
                  "Fe" = 3, "Cl" = -1, "NO3" = -1, "HCO3" = -1,"H2PO4" = -1, "SO4" = -2,   "CO3" = -2,
                  "HPO4" = -2, "MoO4" = -2, "SiO3" = -2, "BO3" =  -3,
                  "C6H5O7" = -3, "PO4" = -3,"EDTA" = -3)

ion_MR <- list("Mg" = 24.31, "Ca" = 40.08, "Zn" = 65.38, "Cu" = 63.55, "Na" = 22.99, "Al" = 26.98, "K" = 39.10, 
               "Fe" = 55.85, "Cl" = 35.45, "NO3" = 62.01, "SO4" = 96.06,  "PO4" =94.97, "CO3" = 100.09,
               "HCO3" = 61.02, "H2PO4" = 96.99,"HPO4" = 95.99, "BO3" =  58.81, "MoO4" = 159.95, "SiO3" = 76.08, "NH4"  = 18.04 ,
               "Co" = 28.01, "C6H5O7" = 498.46, "EDTA" = 292.24 )


for(medium in names(media)){
  for(compound in names(media[[medium]])){
    moles <- media[[medium]][[compound]][1]/(MR[[compound]]*1000)
    #Ionic Strength
    IS[[medium]] <- IS[[medium]] + ((molar_yield[[compound]][1]* valence[[compound]][1]^2 + 
                                       molar_yield[[compound]][2]* valence[[compound]][2]^2)*moles)/2
    #Salinity
    Salinity[[medium]] <- Salinity[[medium]] +  MR_no_water[[compound]]*moles
    for(ion in names(ion_charge)){
      #check if the ion is in the salt
      if(grepl(ion,compound, fixed = TRUE)){
        if(ion_charge[[ion]]==1){
          Ion_con[[medium]][["+1"]] <-  Ion_con[[medium]][["+1"]]  + molar_yield[[compound]][1]*moles
        }else if(ion_charge[[ion]]==2){
          Ion_con[[medium]][["+2"]] <- Ion_con[[medium]][["+2"]]+ molar_yield[[compound]][1]*moles
          if(ion ==  "Mg"){
            Hardness[[medium]] <- Hardness[[medium]] + 4.118*molar_yield[[compound]][1]*moles*ion_MR$Mg*1000
          }else if(ion ==  "Ca"){
            Hardness[[medium]] <- Hardness[[medium]] + 2.497**molar_yield[[compound]][1]*moles*ion_MR$Ca*1000
          }
        }else if(ion_charge[[ion]]==3){
          Ion_con[[medium]][["+3"]] <- Ion_con[[medium]][["+3"]] + molar_yield[[compound]][1]*moles
        }else if(ion_charge[[ion]]==-1){
          Ion_con[[medium]][["-1"]] <- Ion_con[[medium]][["-1"]] + molar_yield[[compound]][2]*moles
        }else if(ion_charge[[ion]]==-2){
          Ion_con[[medium]][["-2"]] <- Ion_con[[medium]][["-2"]] + molar_yield[[compound]][2]*moles
        }else if(ion_charge[[ion]]==-3){
          Ion_con[[medium]][["-3"]] <-  Ion_con[[medium]][["-3"]] + molar_yield[[compound]][2]*moles
        }
      }
    }
    
  }
}

##################################################################################
##################################################################################
##################################################################################
##################################################################################

# Function for estimating alkalinity, hardness and ion molarity
#if molar is TRUE, provide the molarity of each ion in mol/L, else provide ion mass in mg/L
ion_calc <- function(Mg = 0, Ca = 0, Zn = 0, Cu = 0, Na = 0, Al = 0, K = 0, Sr = 0, Br = 0,
                 Fe = 0, Cl = 0, NO3 = 0, SO4 = 0,  PO4 =0, CO3 = 0, HCO3 = 0, H2PO4 = 0, HPO4 = 0, BO3 =  0, 
                 MoO4 = 0, SiO3 = 0, NH4  = 0, Ti=0, Mn=0, B=0, P=0, Ba=0, Fluorine=0,
                 Co = 0,  C6H5O7 = 0, EDTA = 0, Si=0, molar = FALSE){


   ion_charge<- list("K" = 1,  "Na" = 1, "NH4"  = 1 , 
                     "Mg" = 2, "Ca" = 2, "Zn" = 2, "Cu" = 2, "Co" = 2, "Sr" = 2, "Mn" = 2, "Ba"=2,
                     "Al" = 3, "Fe" = 3, "B" = 3, "Ti" = 4, "Si" = 4,
                     "Cl" = -1, "NO3" = -1, "HCO3" = -1,"H2PO4" = -1, "Br"=-1, "Fluorine"=-1,
                     "SO4" = -2, "CO3" = -2, "HPO4" = -2, "MoO4" = -2, "SiO3" = -2, 
                     "BO3" = -3, "P"=-3, "C6H5O7" = -3, "PO4" = -3,"EDTA" = -3)
  
   ion_MR <- list("Mg" = 24.31, "Ca" = 40.08, "Zn" = 65.38, "Cu" = 63.55, "Na" = 22.99, "Al" = 26.98, "K" = 39.10, 
                 "Fe" = 55.85, "Cl" = 35.45, "NO3" = 62.01, "SO4" = 96.06,  "PO4" =94.97, "CO3" = 100.09,
                 "HCO3" = 61.02, "H2PO4" = 96.99,"HPO4" = 95.99, "BO3" =  58.81, "MoO4" = 159.95, "SiO3" = 76.08, "NH4"  = 18.04 ,
                 "Co" = 28.01, "C6H5O7" = 498.46, "EDTA" = 292.24, "Sr" = 87.62, "Br" = 79.904, "Ti"=47.867, 
                 "Mn"=54,938044, "B"=10.811, "P"=30.97, "Ba"=137.327, "Fluorine"=18.998, "Si"=28,0855)
  
  if(molar == FALSE){
    
    hardness <- 4.118*Mg + 2.497*Ca  #in mg/L
    for (compound in names(ion_charge)){
      #convert atoms/molecules of arguments from mass concentration (mg/L) to molar concentration (mol/L)
      eval(parse(text=paste(compound,"=",compound, "/(ion_MR[[compound]]*1000)",sep="")))
    }
    ion_valence_concentation <- list("+1" = K+Na+NH4, "+2" = Mg+Ca+Zn+Cu+Co+Sr+Mn+Ba , "+3"= Al+Fe+B, "+4"=Ti+Si,
                            "-1" = Cl+NO3+HCO3+H2PO4+Br+Fluorine,
                            "-2" = SO4+CO3+HPO4+MoO4+SiO3, "-3" = BO3+C6H5O7+PO4+EDTA ) #in mol/L
  }else{
    ion_valence_concentation <- list("+1" = K+Na+NH4, "+2" = Mg+Ca+Zn+Cu+Co+Sr+Mn+Ba , "+3"= Al+Fe+B, "+4"=Ti+Si,
                            "-1" = Cl+NO3+HCO3+H2PO4+Br+Fluorine,
                            "-2" = SO4+CO3+HPO4+MoO4+SiO3, "-3" = BO3+C6H5O7+PO4+EDTA+P  )
    for (compound in names(ion_charge)){
      #convert atoms/molecules of arguments from mass concentration (mg/L) to molar concentration (mol/L)
      eval(parse(text=paste(compound,"=",compound,"*(ion_MR[[compound]]*1000)",sep="")))
    }
    hardness <- 4.118*Mg + 2.497*Ca 
  }
  return(list("ion_valence_concentation" = ion_valence_concentation, "hardness" = hardness))
}



#Tso et al.2010

Tso_2008_lake <- ion_calc(Cl = 22.59, NO3 = 1.17, SO4 = 16.62, Ca = 47.3, Mg = 2.92,   molar = FALSE)
Tso_2008_ww <- ion_calc(Cl = 241.1, NO3 = 11.8, SO4 = 682.9, Ca = 13.9, Mg = 3.17,   molar = FALSE)



# Brunelli et al., 2013
#--------------------------

AFW <- ion_calc(Ca=0.1, K=0.1, Mg=0.1, Na=54.72, Sr=0.1, Cl=0.1, BO3=0.1, Br=0.1,
                SiO3=0.1, SO4=0.1, HCO3=145.22,
                molar = FALSE)

AEW <- ion_calc(Ca=5.61, K=5.86, Mg=18.47, Na=156.10, Sr=0.1, Cl=274.41, BO3=0.59, Br=0.8,
                SiO3=0.1, SO4=46.11, HCO3=1.83,
                molar = FALSE)

ASW1 <- ion_calc(Ca=274.53, K=344.06, Mg=1167.13, Na=10204.80, 
                 Sr=0.1, Cl=17650.28, BO3=21.76, Br=0.1,
                 SiO3=0.1, SO4=2986.54, HCO3=145.20,
                 molar = FALSE)

ASW2 <- ion_calc(Ca=403.99, K=402.32, Mg=1296.43, Na=10922.32, 
                 Sr=6.13, Cl=19203.47, BO3=28.81, Br=67.12,
                 SiO3=10.65, SO4=3227.65, HCO3=145.20,
                 molar = FALSE)

LW <- ion_calc(Ca=413.12, K=413.7, Mg=1341.07, Na=11287.76, 
               Sr=0.1, Cl=19255.87, BO3=54.83, Br=141.92,
               SiO3=0.1, SO4=2933.41, HCO3=0.1,
               molar = FALSE)

SW <- ion_calc(Ca=427.04, K=466.97, Mg=1405.15, Na=11647.63, 
               Sr=0.1, Cl=21052.65, BO3=71.89, Br=153.15,
               SiO3=0.1, SO4=2987.76, HCO3=0.1,
               molar = FALSE)

# Ji et al., 2010
#-----------------
BEGM <- ion_calc(Cl=3985, SO4=15.1, PO4=203, Na=3041, Mg= 13.8, Al=0.03,
                 K=51, Ca=5, Ti=0.01, Mn=0.001, Fe=0.09, Cu=0.003, Zn=0.22,
                 B=0, P=66,
                 molar=FALSE)

DMEM <- ion_calc(Cl=4070, SO4=99.9, PO4=80, Na=3483, Mg= 19.2, Al=0.04,
                 K=191, Ca=70.3, Ti=0.01, Mn=0.005, Fe=0.19, Cu=0.017, Zn=0.25,
                 B=0.02, P=26,
                 molar=FALSE)

LB <- ion_calc(Cl=4708, SO4=41, PO4=235, Na=2635, Mg= 3.7, Al=0.09,
               K=419, Ca=11.1, Ti=0.04, Mn=0.017, Fe=0.39, Cu=0.012, Zn=1.15,
               B=0.06, P=77,
               molar=FALSE)

TSB <- ion_calc(Cl=5070, SO4=87, PO4=1078, Na=2464, Mg= 13.1, Al=0.24,
               K=1237, Ca=10, Ti=0.13, Mn=0.025, Fe=0.58, Cu=0.018, Zn=0.82,
               B=0.30, P=352,
               molar=FALSE)

SD <- ion_calc(Cl=121, SO4=0, PO4=528, Na=40, Mg= 94.1, Al=0.06,
                K=241, Ca=35.6, Ti=0.03, Mn=0.199, Fe=0.03, Cu=0.014, Zn=0.16,
                B=0.08, P=172,
                molar=FALSE)

YPD <- ion_calc(Cl=52, SO4=0, PO4=781, Na=207, Mg= 6.1, Al=0.15,
               K=511, Ca=10.8, Ti=0.02, Mn=0.028, Fe=0.67, Cu=0.039, Zn=1.30,
               B=0.06, P=255,
               molar=FALSE)

# Sillanpaa et al., (2011)
#-----------------

# Brackish
Langskar <- ion_calc(Al=0*1000, Ba=0*1000 , Cl=0, Fluorine=0*1000, Fe=0*1000, K=0,
                     Mg=0, Mn=0*1000, Na=0, NH4=2*1000,
                     NO3=2*1000, PO4=3*1000, P=25*1000, SiO3=0.49, Sr=0,
                     molar = FALSE)
# Brackish
Tvarmine <- ion_calc(Al=0*1000, Ba=0*1000 , Cl=0, Fluorine=0*1000, Fe=0*1000, K=0,
                     Mg=0, Mn=0*1000, Na=0, NH4=0*1000,
                     NO3=2*1000, PO4=3*1000, P=25*1000, SiO3=0.52, Sr=0,
                     molar = FALSE)

# freshwater
Simijarvi <- ion_calc(Al=73*1000, Ba=6.3*1000, Ca=1.7, Cl=1.8, Fluorine=41*1000, Fe=24*1000, K=0.3,
                      Mg=0.7, Mn=7.5*1000, Na=1.5, NH4=2*1000,
                      NO3=154*1000, PO4=2*1000, P=3*1000, SiO3=2.5, SO4=5.4, Sr=15,
                      molar = FALSE)

# freshwater 
Iso_Lehmalampi <- ion_calc(Al=160*1000, Ba=4.7*1000, Ca=0.7, Cl=1.5, Fluorine=47*1000, Fe=400*1000, K=0.4,
                           Mg=0.3, Mn=15*1000, Na=1.1, NH4=48*1000,
                           NO3=41*1000, PO4=2*1000, P=8*1000, SiO3=1.6, SO4=2.7, Sr=5.3,
                           molar = FALSE)

# Checkli et al., 2014
#-------------------------
LW <- ion_calc(Cl = 34.55, NO3 = 15.46, SO4=12.97, PO4 = 0.39,
               Na = 41.01, K = 5.89, Ca = 16.51, Mg = 9.60,
               molar=FALSE)

Sewage_effluent <- ion_calc(Cl = 237.22, NO3 = 9.37, SO4=53.04, PO4 = 3.73,
                            Na = 163.31, K = 12.19, Ca = 28.89, Mg = 25,
                            molar=FALSE)

Seawater <- ion_calc(Cl = 21442, NO3 = 0.06, SO4=3055, PO4 = 0.02,
                     Na = 10756, K = 358, Ca = 415, Mg = 1338,
                     molar=FALSE)

wastewater <- ion_calc(Cl = 237.22, NO3 = 9.37, SO4=53.04, PO4 = 3.73,
                      Na = 163.31, K = 12.19, Ca = 28.89, Mg = 25,
                      molar=FALSE)

Groundwater <- ion_calc(Cl = 267.4, NO3 = 35.14, SO4=39.31, PO4 = 1.43,
                        Na = 122.02, K = 7.06, Ca = 59.23, Mg = 52.11,
                        molar=FALSE)

# Checkli et al., 2014
#-------------------------
R28 <-  ion_calc(Cl = 1.21/1000, NO3 = 0.01/1000, SO4=0.24/1000, PO4 = 0.07/1000,
                 Na = 2.1/1000, K = 0.11/1000, Ca = 0/1000, Mg = 0.3/1000,
                 molar=TRUE)

R67 <-  ion_calc(Cl = 5.15/1000, NO3 = 0.05/1000, SO4=0.27/1000, PO4 = 0/1000,
                 Na = 5.17/1000, K = 0.12/1000, Ca = 0.16/1000, Mg = 0.3/1000,
                 molar=TRUE)

R187 <-  ion_calc(Cl = 14.8/1000, NO3 = 0.07/1000, SO4=0.82/1000, PO4 = 0.08/1000,
                 Na = 15.1/1000, K = 0.41/1000, Ca = 0.36/1000, Mg = 0.56/1000,
                 molar=TRUE)

R342 <-  ion_calc(Cl = 278.9/1000, NO3 = 2.16/1000, SO4=14.7/1000, PO4 = 0/1000,
                  Na = 495.9/1000, K = 1.59/1000, Ca = 4.02/1000, Mg = 8.61/1000,
                  molar=TRUE)

# Farner et al., 2019 (EPA MHW)
EPA_MHW <- ion_calc(Na = 0.001143, HCO3 = 0.001143, Ca = 0.0003484725, SO4 = 0.0003484725+0.000499,  
                Mg = 0.000499, K = 0.000054, Cl = 0.000054,
                molar = TRUE)

# Du et al.2015
Du_river <- ion_calc(Fluorine = 0.31, Cl = 8.89, NO3 = 4.60, SO4 = 17.28,
                     K = 2.59, Na = 13.85, Ca = 29.2, Mg = 7.33, Al = 1.22,
                     Fe = 1.47, Ti = 0.04,
                     molar=FALSE)

# Ren et al., (2017)
Ren_wastewater <- ion_calc(Cl = 30.08, NO3=17.53, Na=20.53, Ca = 26.07, Mg=5.19, 
                           molar = FALSE)

# Ottofueling et al., (2011)
Otto_ground_water <- ion_calc(HCO3 = 290, Na = 12, K=3, Ca=93, Mg=20, Mn=0,
                              Cl=18, NO3 = 23, SO4 = 29, Si = 4, Al = 0, Fe=0,
                              molar = FALSE)

Otto_lake_water <- ion_calc(HCO3 = 145, Na = 0, K=0, Ca=46, Mg=7, Mn=0,
                            Cl=1, NO3 = 4, SO4 = 2, Si = 1, Al = 0, Fe=0,
                            molar = FALSE)

Otto_drinking_water <- ion_calc(HCO3 = 161, Na = 0, K=0, Ca=51, Mg=10, Mn=0,
                                Cl=2, NO3 = 5, SO4 = 16, Si = 1, Al = 0, Fe=0,
                                molar = FALSE)

Otto_freshwater <- ion_calc(HCO3 = 10, Na = 1, K=1, Ca=4, Mg=0, Mn=0,
                                Cl=7, NO3 = 3, SO4 = 12, Si = 5, Al = 0, Fe=0,
                                molar = FALSE)

Otto_seawater <- ion_calc(HCO3 = 1490, Na = 8500, K=340, Ca=430, Mg=1300, Mn=0,
                            Cl=22500, NO3 = 0, SO4 = 2900, Si = 0, Al = 0, Fe=0,
                            molar = FALSE)

Otto_wastewater_inflow <- ion_calc(HCO3 = 170, Na = 37, K=18, Ca=48, Mg=13, Mn=0,
                                   Cl=61, NO3 = 0, SO4 = 45, Si = 2, Al = 0, Fe=0,
                                   molar = FALSE)

Otto_wastewater_outflow <- ion_calc(HCO3 = 120, Na = 36, K=9, Ca=44, Mg=12, Mn=0,
                                   Cl=56, NO3 = 7, SO4 = 58, Si = 2, Al = 0, Fe=0,
                                   molar = FALSE)

Otto_EPA_vs <- ion_calc(HCO3 = 8, Na = 4, K=0, Ca=2, Mg=2, Mn=0,
                        Cl=0, NO3 = 0, SO4 = 8, Si = 0, Al = 0, Fe=0,
                        molar = FALSE)


Otto_EPA_mh <- ion_calc(HCO3 = 64, Na = 32, K=2, Ca=17, Mg=14, Mn=0,
                        Cl=2, NO3 = 0, SO4 = 41, Si = 0, Al = 0, Fe=0,
                        molar = FALSE)

Otto_EPA_vh <- ion_calc(HCO3 = 255, Na = 130, K=8, Ca=69, Mg=56, Mn=0,
                        Cl=8, NO3 = 0, SO4 = 298, Si = 0, Al = 0, Fe=0,
                        molar = FALSE)
