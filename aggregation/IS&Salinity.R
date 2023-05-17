MR <- list("KH2PO4" = 136.086, "NaCl" = 58.44,  "Na2HPO4" = 141.96,"Na2HPO4·7H20" = 141.96+7*18.02, "Ca(NO3)2" = 164.1 ,
           "Ca(NO3)2·4H20" = 164.1+4*18.02 ,
           "KCl" = 74.55, "MgSO4" = 120.366, "MgSO4·7H2O" = 120.366+ 7*18.02,  'NaHCO3'=84.007 , "CaCl2" =110.98 ,
           "CaCl2·2H20" =110.98+2*18.02 , "Fe(NO3)3" = 241.86,
           "Fe(NO3)3·9H20" = 241.86+9*18.02,
           "NaH2PO4" = 119.98, "NaH2PO4·H20" = 119.98+18.02,  "MgCl2" = 95.211,  "MgCl2·67H20" = 95.211+6*18.02, "Na3C6H5O7" = 258.06, "Na2CO3" = 105.99, "NaNO3" = 85,
           "Na2MgEDTA" = 358.50 ,"FeCl3" = 162.2, "FeCl3·6H20" = 162.2+6*18.02,  "H3BO3" = 61.83, "MnCl2" = 125.84,
           "MnCl2·4H20" = 125.84+4*18.02, "ZnSO4" = 161.47 , "ZnSO4·7H20" = 161.47+7*18.02,
           "CuSO4" = 159.61, "CuSO4·5H20" = 159.61+5*18.02,"Co(NO3)2" = 182.94, "Co(NO3)2·6H20" = 182.94+6*18.02,
           "Na2MoO4" = 205.93, "Na2MoO4·2H20" = 205.93+2*18.02, "Na2SiO3" = 122.06, "Na2SiO3·9H20" = 122.06+9*18.02, "K2HPO4" = 174.2,
           "KNO3" = 101.10, "Na2EDTA" = 372.24, "(NH4)2SO4" = 132.14,  "NH4Cl" = 53.491,  "ZnCl2" = 136.29,
           "CoCl2·6H20" = 129.84+6*18.02, "CuCl2·2H20" = 134.45+2*18.02 )#g/mol

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
                    "KNO3" = 101.10, "Na2EDTA" = 372.24 - 2*18.02, "(NH4)2SO4" = 132.14,  "NH4Cl" = 53.491,  "ZnCl2" = 136.29,
                    "CoCl2·6H20" = 129.84, "CuCl2·2H20" = 134.45)#g/mol

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
                "(NH4)2SO4" = c(1,-2), "NH4Cl" = c(1,-1), "ZnCl2" = c(2,-1), "CoCl2·6H20" = c(2,-1), "CuCl2·2H20" =  c(2,-1))



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
                    "Na2EDTA" = c(2,1),  "(NH4)2SO4" = c(2,1), "NH4Cl" = c(1,1), "ZnCl2" = c(1,2), "CoCl2·6H20" = c(1,2),
                    "CuCl2·2H20" =  c(1,2))

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
                                "CuCl2·2H20" = 0.0001)) #mg/L


IS <- list("PBS" = 0,  "RPMI1640" = 0, "DMEM" = 0, "L-15" = 0,  "EMEM" = 0,
           "M199" = 0, "Dryls_Buffer" = 0, "E3" = 0,  "BG-11" = 0, "SM7"  = 0,  "ILM" = 0, "MMD" = 0, "ISO_8692" = 0 )

Salinity <- list("PBS" = 0,  "RPMI1640" = 0, "DMEM" = 0,"L-15" = 0,  "EMEM" = 0, 
                 "M199" = 0, "Dryls_Buffer" = 0, "E3" = 0, "BG-11" = 0, "SM7"  = 0,  "ILM" = 0, "MMD" = 0, "ISO_8692" = 0)

for(medium in names(media)){
  for(compound in names(media[[medium]])){
    #Ionic Strength
    IS[[medium]] <- IS[[medium]] + ((molar_yield[[compound]][1]* valence[[compound]][1]^2 + 
                                       molar_yield[[compound]][2]* valence[[compound]][2]^2)*
                                      media[[medium]][[compound]][1]/(MR[[compound]]*1000))/2
    #Salinity
    Salinity[[medium]] <- Salinity[[medium]] +  MR_no_water[[compound]]*media[[medium]][[compound]][1]/(MR[[compound]]*1000)
  }
}


