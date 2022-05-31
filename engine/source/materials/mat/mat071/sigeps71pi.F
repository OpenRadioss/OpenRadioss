Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  SIGEPS71PI                    source/materials/mat/mat071/sigeps71pi.F
Chd|-- called by -----------
Chd|        MULAWP                        source/elements/beam/mulawp.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS71PI(
     1               NEL     ,NUPARAM ,UPARAM  ,IPM     ,MAT     ,
     2               OFF     ,DEPSXX  ,DEPSXY  ,DEPSXZ  ,        
     3               SIGOXX  ,SIGOXY  ,SIGOXZ  ,EPSXX   ,EPSXY  ,    
     4               EPSXZ   ,SIGNXX  ,SIGNXY  ,SIGNXZ  ,ETSE    ,
     5               NUVAR   ,UVAR    ,IFUNC   ,NPF     ,
     6               TF      ,NFUNC   ,JTHE    ,TEMPEL  ,FM     ,
     7               TREPS)      
c  Law for SMA (Shape memory alloy - NiTinol)
c  based on Auricchio 1997
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,
     .   NFUNC,IFUNC(NFUNC),NPF(*),JTHE
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: MAT
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: EPSXX,EPSXY,EPSXZ,
     .   DEPSXX,DEPSXY,DEPSXZ,SIGOXX,SIGOXY,SIGOXZ,TEMPEL
      my_real ,DIMENSION(*) ,INTENT(IN) :: UPARAM
      my_real
     .   TF(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: SIGNXX,SIGNXY,SIGNXZ,ETSE
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: OFF
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT):: UVAR
      my_real
     .   FM(NEL),TREPS(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IADBUF,KK
      INTEGER ,DIMENSION(NEL) ::EFLAG
      my_real :: FASS,FSAS,FASF,FSAF,RSAS,RFAS,FS0,RSSA,RFSA,DFM, FSS,
     .           SQDT,AA,BB,CC,FCT,FCTP,BETA,DGT,GS,DET,SHFACT,DFMSA,DFMAS 
      my_real, DIMENSION(NEL) ::E,K,EMART,G,G2, ALPHA,EPSL,GM,KM,
     .           YLD_ASS,YLD_ASF,YLD_SAS,TINI,
     .           YLD_SAF,CAS,CSA,TSAS,TFAS, TSSA,TFSA, 
     .          FS,TEMP,ET,GT
C-----------------------------------------------
c    UVAR  2= FS0 
c    UVAR  3= FS0 
c    UVAR 11=  TEMP
c=======================================================================
      SHFACT = FIVE_OVER_6       

      DO I=1,NEL                                 
        IADBUF       = IPM(7,MAT(I))-1               
        E(I)         = UPARAM(IADBUF+1)                        
        G(I)         = UPARAM(IADBUF+3)              
        K(I)         = UPARAM(IADBUF+4)                  
        YLD_ASS(I)   = UPARAM(IADBUF+6)              
        YLD_ASF(I)   = UPARAM(IADBUF+7)              
        YLD_SAS(I)   = UPARAM(IADBUF+8)              
        YLD_SAF(I)   = UPARAM(IADBUF+9)              
        ALPHA (I)    = UPARAM(IADBUF+10)             
        EPSL(I)      = UPARAM(IADBUF+11)             
        EMART(I)     = UPARAM(IADBUF+14)             
        EFLAG(I)     = INT(UPARAM(IADBUF+15))        
        GM(I)        = UPARAM(IADBUF+16)             
        KM(I)        = UPARAM(IADBUF+17)             
        G2(I)        = TWO*G(I)                      
        CAS(I)       = UPARAM(IADBUF+18)             
        CSA(I)       = UPARAM(IADBUF+19)             
        TSAS(I)      = UPARAM(IADBUF+20)             
        TFAS(I)      = UPARAM(IADBUF+21)             
        TSSA(I)      = UPARAM(IADBUF+22)             
        TFSA(I)      = UPARAM(IADBUF+23)  
        TINI(I)      = UPARAM(IADBUF+25)           
      ENDDO                           
      !  Compute temperature - adiabatic conditions
      IF(JTHE < 0 ) THEN
         DO I=1,NEL     
           TEMP(I) = TEMPEL(I)
         ENDDO
      ELSE         
         DO I=1,NEL
           TEMP(I) = TINI (I)
c     .             + EINT(I)/ RHO0(I)/CP/MAX(EM15,VOLUME(I))
         ENDDO
      ENDIF
c==============================================================
      DO I = 1,NEL
        !compute limits for start and end of transformation
        RSAS = YLD_ASS(I) -CAS(I)*TSAS(I) !start from aust to martensite
        RFAS = YLD_ASF(I) -CAS(I)*TFAS(I) !end from aust to martensite
        RSSA = YLD_SAS(I) -CSA(I)*TSSA(I) !start from martensite to austenite
        RFSA = YLD_SAF(I) -CSA(I)*TFSA(I) !end from martensite to austenite
   
        IF (EFLAG(I) > ZERO)THEN ! young modulus dependent on martensite fraction
          GT(I) = G(I) + FM(I) * (GM(I)    - G(I)) !G_n
          ET(I) = E(I) + FM(I) * (EMART(I) - E(I)) !E_n
          GS = SHFACT*GT(I)      
          !trial stress (sigma_tr_n+1)
          SIGNXX(I) =   ET(I)*(EPSXX(I) - EPSL(I)*FM(I)* SIGN(ONE,EPSXX(I)) )
          SIGNXY(I) =   GS   * EPSXY(I)
          SIGNXZ(I) =   GS   * EPSXZ(I)
          ETSE(I)   =   ONE                        

          BETA      =   ET(I)*EPSL(I)* SIGN(ONE,EPSXX(I))-(EMART(I)- E(I))*EPSXX(I) 
     .                                        + (EMART(I) - E(I)) *EPSL(I)* SIGN(ONE,EPSXX(I)) *FM(I)  
          DFMSA = ZERO
          DFMAS = ZERO 
          !---------------
          !Check Austenite -----> martensite  
          !---------------
	     FS(I)     =   ABS (SIGNXX(I) ) -CAS(I)*TEMP(I)! trial
          FASS = FS(I) -  RSAS
          FASF = FS(I) -  RFAS
          FS0  = UVAR(I,2)
          IF((FS(I) - FS0) > ZERO .AND. FASS > ZERO.AND. FASF < ZERO .AND. FM(I) < ONE )THEN
            AA = (ONE - FM(I))*(EMART(I) - E(I))*EPSL(I)
            BB = -(FS0 - RFAS - (ONE - FM(I))*BETA* SIGN(ONE,EPSXX(I)))
            CC = -(ONE - FM(I)) * (FS(I) - FS0)
            DFMAS =  MIN(ONE , - (ONE - FM(I))*(FS(I) - FS0 ) / (FS0-RFAS - (ONE - FM(I))*E(I)*EPSL(I)  )  ) 
            DO KK = 1,3
             FCT   = DFMAS*DFMAS *AA+ DFMAS*   BB  + CC
             FCTP  = TWO*DFMAS *AA+ BB
             DFMAS = DFMAS - FCT / FCTP     
            ENDDO
            DFMAS = MIN(ONE,DFMAS  )  
          ENDIF
          !---------------
          !Check marteniste -----> austenite
          !---------------
	     FS(I)     =   ABS (SIGNXX(I) ) -CSA(I)*TEMP(I)! trial
          FSAS = FS(I) - RSSA
          FSAF = FS(I) - RFSA
          FS0  = UVAR(I,3)
          IF((FS(I) - FS0) < ZERO .AND. FSAS < ZERO.AND. FSAF > ZERO .AND. FM(I) >ZERO)THEN
            AA =  FM(I)*(EMART(I) - E(I))*EPSL(I)
            BB =  FS0-RFSA   +  FM(I)*BETA* SIGN(ONE,EPSXX(I))
            CC =  -FM(I) * (FS(I)-FS0 )
            DFMSA =  MAX(-ONE , FM(I) * ( FS(I) - FS0 ) / (FS0 - RFSA + FM (I)* E(I)*EPSL(I) ))
            DO KK = 1,3
             FCT  = DFMSA*DFMSA *AA+ DFMSA*   BB  +CC
             FCTP = TWO*DFMSA *AA+ BB
             DFMSA = DFMSA - FCT / FCTP
            ENDDO
            DFMSA =  MAX(-ONE , DFMSA )
          ENDIF
         !--------------------------
         !new martensite fraction increment        
         !--------------------------
          DFM =  DFMAS + DFMSA 
          IF(DFM < ZERO .AND. FM(I) == ZERO) DFM = ZERO
         ! --------------------------
         ! UPDATE
         ! --------------------------
          DGT = DFM * (GM (I)   - G(I))
          DET = DFM * (EMART(I) - E(I))
          SIGNXX(I) = SIGNXX(I) + DET * ( EPSXX(I) - EPSL(I) * (FM(I) + DFM)* SIGN(ONE,EPSXX(I))   ) 
     .                                           - ET(I) * EPSL(I) * DFM* SIGN(ONE,EPSXX(I))

          FS(I) =  ABS (SIGNXX(I) )   
          FM(I) = FM(I) + DFM
          FM(I) = MAX(ZERO,FM(I))  
          FM(I) = MIN(ONE  ,FM(I))  
          TREPS(I) =  FM(I) * SIGN(ONE,EPSXX(I)) * EPSL(I)  !transformation strain for output  
          UVAR(I,2) = FS(I) - CAS(I)*TEMP(I)
          UVAR(I,3) = FS(I) - CSA(I)*TEMP(I) 
          UVAR(I,11) = TEMP(I)

c==============================================================
        ELSE ! constant young modulus    
          GS   = SHFACT*G(I)                             
          !trial stress(sigma_tr_n+1)
          SIGNXX(I) =   E(I)*(EPSXX(I) - EPSL(I)*FM(I)* SIGN(ONE,EPSXX(I)))
          SIGNXY(I) =   GS  * EPSXY(I)
          SIGNXZ(I) =   GS  * EPSXZ(I)

          ETSE(I)   =   ONE                        
	     FS(I)     =   ABS (SIGNXX(I) ) - CAS(I)*TEMP(I)! trial
          !---------------
          !Check Austenite -----> martensite  
          !---------------
          FASS = FS(I) -  RSAS
          FASF = FS(I) -  RFAS
          FS0 = UVAR(I,2)
          DFMSA = ZERO
          DFMAS = ZERO 
          IF((FS(I) - FS0) > ZERO .AND. FASS > ZERO.AND. FASF < ZERO .AND. FM(I) < ONE )THEN
            ! transformation to martensite 
            ! compute martensite phase fraction increment
             DFMAS =  MIN(ONE , - (FS(I) - FS0 )*(ONE - FM(I)) / (FS0-RFAS - (ONE - FM(I))*E(I)*EPSL(I)  )  ) 
          ENDIF
         !---------------
         !Check marteniste -----> austenite  
         !---------------
	    FS(I)     =   ABS (SIGNXX(I) ) - CSA(I)*TEMP(I)! trial
         FSAS = FS(I) - RSSA  
         FSAF = FS(I) - RFSA  
         FS0 = UVAR(I,3)
         IF((FS(I) - FS0) < ZERO .AND. FSAS < ZERO.AND. FSAF > ZERO  )THEN 
            ! transformation to austenite 
            ! compute martensite phase fraction increment
           DFMSA =  MAX(-ONE , FM(I) * ( FS(I) - FS0 ) / (FS0 - RFSA + FM(I) * E(I)*EPSL(I) ))
         ENDIF
         !--------------------------
         !new martensite fraction increment        
         !--------------------------
         DFM =  DFMAS + DFMSA 
         IF(DFM < ZERO .AND. FM(I) == ZERO) DFM = ZERO
         !--------------------------
         !UPDATE
         !--------------------------
         SIGNXX(I) = SIGNXX(I) - E(I) * DFM * EPSL(I) *  SIGN(ONE,EPSXX(I))
         FS(I) =  ABS (SIGNXX(I) )   
c
         FM(I) = FM(I) + DFM
         FM(I) = MAX(ZERO,FM(I))  
         FM(I) = MIN(ONE  ,FM(I))  

         UVAR(I,2) = FS(I) - CAS(I)*TEMP(I)
         UVAR(I,3) = FS(I) - CSA(I)*TEMP(I) 
         TREPS(I) = FM(I) * EPSL(I) *  SIGN(ONE,EPSXX(I)) !transformation strain for output

         UVAR(I,11) = TEMP(I)
        ENDIF ! EFLAG        
      ENDDO  
      RETURN
      END
