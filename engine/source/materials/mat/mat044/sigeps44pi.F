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
Chd|  SIGEPS44PI                    source/materials/mat/mat044/sigeps44pi.F
Chd|-- called by -----------
Chd|        MULAWP                        source/elements/beam/mulawp.F 
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS44PI(
     1               NEL     ,NUPARAM ,UPARAM  ,IPM     ,MAT     ,
     2               OFF     ,PLA     ,DEPSXX  ,DEPSXY  ,DEPSXZ  ,        
     3               SIGOXX  ,SIGOXY  ,SIGOXZ  ,EPST    ,EPSP    ,      
     4               SIGNXX  ,SIGNXY  ,SIGNXZ  ,ETSE    ,NUVAR   ,
     5               UVAR    ,IFUNC   ,NVARTMP ,VARTMP  ,NPF     ,
     6               TF      ,NFUNC   )      
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
     .   NFUNC,IFUNC(NFUNC),NPF(*),NVARTMP
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: MAT
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: EPST,
     .   DEPSXX,DEPSXY,DEPSXZ,SIGOXX,SIGOXY,SIGOXZ
      my_real ,DIMENSION(*) ,INTENT(IN) :: UPARAM
      my_real
     .   TF(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: SIGNXX,SIGNXY,SIGNXZ,ETSE
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: PLA,OFF,VARTMP(NEL,NVARTMP),EPSP
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT):: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IADBUF
      my_real :: SVM,SHFACT,GS,EPIF,DMG,R,FRATE,ALPHA,EPSDOT
      INTEGER, DIMENSION(NEL) :: ICC,ISRATE,VFLAG,IAD,IPOS,ILEN
      my_real, DIMENSION(NEL) :: E,NU,G,G3,YLD,YLDMAX,EPMAX,EPDR,
     .   EPSR1,EPSR2,CA,CB,CN,CP,ASRATE,YSCALE,DFDPLA,DPLA
c=======================================================================
      SHFACT = CSIX
      EPIF   = ZERO
c
      DO I=1,NEL
        IADBUF   = IPM(7,MAT(I))-1
        E(I)     = UPARAM(IADBUF+1)
        NU(I)    = UPARAM(IADBUF+2)
        CA(I)    = UPARAM(IADBUF+3)
        YLDMAX(I)= UPARAM(IADBUF+4)
        EPMAX(I) = UPARAM(IADBUF+5)
        EPSR1(I) = UPARAM(IADBUF+6)
        EPSR2(I) = UPARAM(IADBUF+7)
        CB(I)    = UPARAM(IADBUF+8)
        CN(I)    = UPARAM(IADBUF+9)
        ICC(I)   = NINT(UPARAM(IADBUF+10))
        EPDR(I)  = UPARAM(IADBUF+11)
        EPIF     = MAX(EPIF,EPDR(I))              
        CP(I)    = UPARAM(IADBUF+12)
        G(I)     = UPARAM(IADBUF+16)
        G3(I)    = UPARAM(IADBUF+18)
        ISRATE(I)= NINT(UPARAM(IADBUF+13))
        ASRATE(I)= UPARAM(IADBUF+14)
        VFLAG(I) = NINT(UPARAM(IADBUF+23))
        YSCALE(I)= UPARAM(IADBUF+24)
        IF (VFLAG(I) == 1) THEN
          EPSP(I) = UVAR(I,1)
        ENDIF
        DPLA(I) = ZERO
      ENDDO
c        
c---    Contraintes elastiques
c
      DO I = 1,NEL
        GS = SHFACT*G(I)                         
        SIGNXX(I) = SIGOXX(I) + E(I)*DEPSXX(I)
        SIGNXY(I) = SIGOXY(I) + GS*DEPSXY(I)
        SIGNXZ(I) = SIGOXZ(I) + GS*DEPSXZ(I)
        ETSE(I)   = UN                        
      ENDDO  
c        
c---    Yield stress
c
      IF (NFUNC > 0) THEN
        IPOS(1:NEL) = NINT(VARTMP(1:NEL,1))
        IAD (1:NEL) = NPF(IFUNC(1)) / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        CALL VINTER(TF,IAD,IPOS,ILEN,NEL,PLA,DFDPLA,YLD) 
        VARTMP(1:NEL,1) = IPOS(1:NEL)
      ENDIF
c
c
c
      DO I = 1,NEL
        IF (NFUNC > 0) THEN 
          YLD(I) = YSCALE(I)*YLD(I)
        ELSE
          YLD(I) = CA(I)
        ENDIF
      ENDDO
c
c
c
      DO I = 1,NEL
        IF (PLA(I) > ZERO) THEN
          IF (NFUNC > 0) THEN 
            YLD(I) = YSCALE(I)*YLD(I)
          ELSE
            YLD(I) = CA(I) + CB(I)*EXP(CN(I)*LOG(PLA(I)))
          ENDIF
        ENDIF                                          
      ENDDO  
c
c     Strain rate effect
c
      IF (EPIF > ZERO) THEN
        DO I = 1,NEL
          IF (EPDR(I) > ZERO) THEN
            FRATE = UN + (EPSP(I)*EPDR(I))**CP(I)
            IF (ICC(I)== 1) YLDMAX(I) = YLDMAX(I) * FRATE
            IF ((NFUNC > 0) .AND. (CA(I) /= ZERO)) THEN 
              IF (PLA(I)>ZERO) THEN 
                YLD(I) = YLD(I) + (CA(I) + CB(I)*EXP(CN(I)*LOG(PLA(I))))*(FRATE-UN)
              ELSE
                YLD(I) = YLD(I) + CA(I)*(FRATE-UN)
              ENDIF
            ELSE
              YLD(I) = YLD(I) * FRATE
            ENDIF  
          ENDIF
        ENDDO
      ENDIF
c-------------------
c     PROJECTION   -   radial return
c-------------------
      DO I = 1,NEL
        YLD(I) = MIN(YLD(I),YLDMAX(I))                  
        SVM    = SIGNXX(I)**2 + TROIS*(SIGNXY(I)**2 + SIGNXZ(I)**2)
        IF (SVM > YLD(I)**2) THEN
          SVM = SQRT(SVM)                                        
          R   = MIN( UN, YLD(I) / SVM)                  
          SIGNXX(I) = SIGNXX(I)*R                                  
          SIGNXY(I) = SIGNXY(I)*R                                  
          SIGNXZ(I) = SIGNXZ(I)*R   
          DPLA(I) = OFF(I)*SVM*(UN - R) / G3(I)                       
          PLA(I) = PLA(I) + OFF(I)*SVM*(UN - R) / G3(I)
        ENDIF                                   
      ENDDO     
c--------------------------------
c     DUCTILE RUPTURE
c--------------------------------
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < UN)   OFF(I) = OFF(I)*QCINQ
      ENDDO
c--------------------------------
c     AXIAL TENSION OR PLASTIC STRAIN FAILURE 
c--------------------------------
      DO I = 1,NEL
        IF (OFF(I) == UN) THEN
          DMG = UN
          IF (EPST(I) > EPSR1(I)) THEN
            DMG = (EPSR2(I) - EPST(I)) / (EPSR2(I) - EPSR1(I))
            DMG = MAX(DMG, ZERO)
            SIGNXX(I) = SIGNXX(I)*DMG                               
            SIGNXY(I) = SIGNXY(I)*DMG                               
            SIGNXZ(I) = SIGNXZ(I)*DMG                               
          ENDIF
c         test strain failure
          IF (DMG == ZERO .or. PLA(I) >= EPMAX(I)) THEN
            OFF(I)   = QCINQ
            IDEL7NOK = 1
          ENDIF
          IF (VFLAG(I) == 1) THEN 
            ALPHA   = MIN(UN, ASRATE(I)*DT1)
            EPSDOT  = DPLA(I)/MAX(EM20,DT1) 
            EPSP(I) = ALPHA*EPSDOT + (UN - ALPHA)*EPSP(I)  
            UVAR(I,1) = EPSP(I)
          ENDIF      
c
        ENDIF
      ENDDO                                          
c-----------
      RETURN
      END
