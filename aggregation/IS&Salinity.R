MR <- list("KH2PO4" = 136.086, "NaCl" = 58.44, "Na2HPO4" = 141.96, "Ca(NO3)2" = 164.1 ,
           "KCl" = 74.55, "MgSO4" = 120.366, 'NaHCO3'=84.007 , "CaCl2" =110.98 , "Fe(NO3)3" = 241.86,
           "NaH2PO4" = 119.98,  "MgCl" = 95.211, "Na3C6H5O7" = 258.06)#g/mol

valence <- list("KH2PO4" = c(1,-1), "NaCl" = c(1,-1), "Na2HPO4" = c(1,-2), "Ca(NO3)2" = c(2,-1) ,
                "KCl" = c(1,-1), "MgSO4" = c(2,-2), "NaHCO3"=c(1,-1) , "CaCl2" =c(2,-1) , "Fe(NO3)3" = c(3,-3),
                "NaH2PO4" =c(1,-1), "MgCl" = c(1,-1), "Na3C6H5O7" = c(1, -3))



molar_yield <- list("KH2PO4" = c(1,1), "NaCl" = c(1,1), "Na2HPO4" = c(2,1), "Ca(NO3)2" = c(1,2) ,
                    "KCl" = c(1,1), "MgSO4" = c(1,1), 'NaHCO3'=c(1,1) , "CaCl2" =c(1,2) , "Fe(NO3)3" = c(1,3),
                    "NaH2PO4" = c(1,1), "MgCl" = c(1,1), "Na3C6H5O7" = c(3,1))

media <- list("PBS" = list("KH2PO4" = 144,  "NaCl" = 9000, "Na2HPO4" = 795),
              "RPMI1640" = list("Ca(NO3)2" = 100, "KCl" = 400 , "MgSO4" = 49, "NaCl"  = 6000,
                                "NaHCO3" = 2000, "Na2HPO4" = 800),
              
              "DMEM" = list("CaCl2" = 200, "Fe(NO3)3" = 0.1, "KCl" = 400, "MgSO4" = 97.67, 
                            "NaCl"  = 6400,"NaHCO3" = 3700, "NaH2PO4" = 125),
              
              "L-15" = list("CaCl2" = 140, "MgCl" = 93.7, "MgSO4" = 97.67,
                            "KCl" = 400, "KH2PO4" = 60, "NaCl" = 8000, "NaH2PO4" = 190),
              
              "EMEM" = list("CaCl2" = 200, "MgSO4" = 97.67, "KCl" = 400,
                            "NaCl" = 6800, "NaH2PO4" = 122),
              
              "M199" = list("CaCl2" = 200, "Fe(NO3)3" = 0.7, "MgSO4" = 97.67,
                            "KCl" = 400, "NaCl" = 6800, "NaH2PO4" = 140),
              
              "Dryls_Buffer" = list("Na3C6H5O7" = 516.1, "NaH2PO4" = 239.96, 
                                    "Na2HPO4" = 141.96, "CaCl2" = 166.47),
              "E3" = list("NaCl" = 292.2, "KCl" = 12.67, "CaCl2" = 36.6,
                          "MgSO4" = 39.72)) #mg/L

IS <- list("PBS" = 0,  "RPMI1640" = 0, "DMEM" = 0, "L-15" = 0,  "EMEM" = 0,
           "M199" = 0, "Dryls_Buffer" = 0, "E3" = 0)
Salinity <- list("PBS" = 0,  "RPMI1640" = 0, "DMEM" = 0,"L-15" = 0,  "EMEM" = 0, 
                 "M199" = 0, "Dryls_Buffer" = 0, "E3" = 0)

for(medium in names(media)){
  for(compound in names(media[[medium]])){
    #Ionic Strength
    IS[[medium]] <- IS[[medium]] + ((molar_yield[[compound]][1]* valence[[compound]][1]^2 + 
                                       molar_yield[[compound]][2]* valence[[compound]][2]^2)*
                                      media[[medium]][[compound]][1]/(MR[[compound]]*1000))/2
    #Salinity
    Salinity[[medium]] <- Salinity[[medium]] + media[[medium]][[compound]][1]/1000
  }
}


