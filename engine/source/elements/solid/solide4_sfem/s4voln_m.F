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
Chd|  S4VOLN_M                      source/elements/solid/solide4_sfem/s4voln_m.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/s8edefo3.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE S4VOLN_M(VARNOD,NC1,NC2,NC3,NC4,MAT,OFFG,RHO,RHO0,
     .                    FXX, FXY, FXZ, FYX, FYY, FYZ, FZX, FZY, FZZ,
     .                    VOL0,VOLN,VOL0DP,VOLDP,AMU0,DXX,DYY,DZZ,MATPARAM,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*),MAT(*),NEL
C     REAL ou REAL*8
      my_real
     .   OFFG(*),VOL0(*),AMU0(*),
     .   DXX(*),DYY(*),DZZ(*),VARNOD(*),VOLN(*),
     .   FXX(*), FXY(*), FXZ(*),
     .   FYX(*), FYY(*), FYZ(*),
     .   FZX(*), FZY(*), FZZ(*),
     .   RHO(*),RHO0
      TYPE(MATPARAM_STRUCT_), DIMENSION(NUMMAT) :: MATPARAM
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K,MX
C     REAL ou REAL*8

      my_real
     .   AMU(MVSIZ), SUM,DTR,DTREP_R,DIVDE(MVSIZ),JAC_M(MVSIZ),
     .   JAC(MVSIZ),FAC,BASE,JFAC,DVDP
      DOUBLE PRECISION 
     .   VOL0DP(*),VOLDP(*)

C----------------------------
      MX = MAT(1)
      IF(ISMSTR==1.OR.ISMSTR==11)THEN
        IF (TT==ZERO) THEN
          DO I=LFT,LLT
           IF(OFFG(I)==ZERO) CYCLE
           AMU0(I) = RHO(I)/RHO0-UN
          ENDDO
        ELSE
          DO I=LFT,LLT
           IF(OFFG(I)==ZERO) CYCLE
           SUM=VARNOD(NC1(I))+VARNOD(NC2(I))+VARNOD(NC3(I))+VARNOD(NC4(I))
           AMU(I) = QUATRE/SUM -UN
           DIVDE(I) = AMU0(I)-AMU(I)
           DTR=DIVDE(I)/DT1             
           DTREP_R = TIERS*(DTR-DXX(I)-DYY(I)-DZZ(I))
           DXX(I) = DXX(I) + DTREP_R
           DYY(I) = DYY(I) + DTREP_R
           DZZ(I) = DZZ(I) + DTREP_R 
           AMU0(I)= RHO(I)/RHO0-UN-DIVDE(I)
          ENDDO
        END IF
      ELSE
c-------------------------------------------------------------------------
        DO I=LFT,LLT
         IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))>UN) CYCLE
         SUM=VARNOD(NC1(I))+VARNOD(NC2(I))+VARNOD(NC3(I))+VARNOD(NC4(I))
         VOLN(I)=QUART*SUM*VOL0(I)             
        ENDDO
        IF (MATPARAM(MX)%STRAIN_FORMULATION==1) THEN 
C------compute AMU(t+dt) for large strain   
          IF(IRESP==1)THEN
            AMU(LFT:LLT) = VOL0DP(LFT:LLT)/VOLDP(LFT:LLT) - UN
          ELSE
            AMU(LFT:LLT) = VOL0(LFT:LLT)/VOLN(LFT:LLT) - UN
          END IF
          IF (TT==ZERO) THEN
            AMU0(LFT:LLT) = AMU(LFT:LLT)
          ELSE          
            DO I = LFT,LLT
              IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))>UN) CYCLE
              DTR   = (DXX(I) + DYY(I) + DZZ(I))*DT1
              DTREP_R = TIERS*((AMU(I)-AMU0(I))+DTR)/DT1
              DXX(I) = DXX(I) - DTREP_R
              DYY(I) = DYY(I) - DTREP_R
              DZZ(I) = DZZ(I) - DTREP_R  
              AMU0(I) = AMU(I)
            ENDDO
          END IF            
        ENDIF
        IF(ISMSTR>=10)THEN
           DO I=LFT,LLT
            IF(OFFG(I)==ZERO) CYCLE
            JAC_M(I)=VOLN(I)/VOL0(I)             
           ENDDO
        ENDIF
        IF((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3) THEN
          IF (TT==ZERO) THEN
            DO I=LFT,LLT
             IF(OFFG(I)==ZERO) CYCLE
             AMU0(I) = RHO(I)/RHO0-UN
            ENDDO
          ELSE
            DO I=LFT,LLT
             IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))<=UN) CYCLE
             SUM=VARNOD(NC1(I))+VARNOD(NC2(I))+VARNOD(NC3(I))+VARNOD(NC4(I))
             AMU(I) = QUATRE/SUM -UN
             DIVDE(I) = AMU0(I)-AMU(I)
             DTR=DIVDE(I)/DT1             
             DTREP_R = TIERS*(DTR-DXX(I)-DYY(I)-DZZ(I))
             DXX(I) = DXX(I) + DTREP_R
             DYY(I) = DYY(I) + DTREP_R
             DZZ(I) = DZZ(I) + DTREP_R  
             DVDP = DIVDE(I)*(VOL0(I)/VOLN(I))
             AMU0(I)= RHO(I)/RHO0-UN-DVDP
            ENDDO
C for totale strain modif in the next         
            IF(IRESP==1.AND.ISMSTR==12)THEN
              DO I=LFT,LLT
               IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))<=UN) CYCLE
               DVDP = DIVDE(I)*(VOL0(I)/VOLN(I))
               AMU0(I) = VOL0DP(I)/VOLDP(I)-UN-DVDP
              ENDDO
            END IF
          END IF !(TT==ZERO) THEN
         ENDIF
      ENDIF
C--------total strain modif      
       IF (ISMSTR==11) THEN
C---- modify Dii w/ rho/rho_0       
        DO I=LFT,LLT
          DTREP_R = -TIERS*(AMU0(I)+FXX(I)+FYY(I)+FZZ(I))
          FXX(I) = FXX(I) + DTREP_R
          FYY(I) = FYY(I) + DTREP_R
          FZZ(I) = FZZ(I) + DTREP_R 
        ENDDO
       ELSEIF(ISMSTR>=10) THEN
        DO I=LFT,LLT
          IF(ABS(OFFG(I))<=UN) CYCLE
          DTREP_R = -TIERS*(AMU0(I)+FXX(I)+FYY(I)+FZZ(I))
          FXX(I) = FXX(I) + DTREP_R
          FYY(I) = FYY(I) + DTREP_R
          FZZ(I) = FZZ(I) + DTREP_R 
        ENDDO
        CALL JACOB_J33(JAC,
     .                FXX, FXY, FXZ ,
     .                FYX, FYY, FYZ ,
     .                FZX, FZY, FZZ )
C
        FAC=TIERS
        DO I=LFT,LLT
         IF(ABS(OFFG(I))>UN) CYCLE
         BASE = JAC_M(I)/MAX(EM20,JAC(I))
         JFAC =EXP(FAC*LOG(MAX(EM20,BASE)))
C         
         FXX(I) = JFAC*FXX(I)+JFAC-UN
         FYY(I) = JFAC*FYY(I)+JFAC-UN
         FZZ(I) = JFAC*FZZ(I)+JFAC-UN
         FXY(I) = JFAC*FXY(I)
         FYX(I) = JFAC*FYX(I)
         FZX(I) = JFAC*FZX(I)
         FXZ(I) = JFAC*FXZ(I)
         FYZ(I) = JFAC*FYZ(I)
         FZY(I) = JFAC*FZY(I)
        ENDDO
       END IF
      RETURN
      END
