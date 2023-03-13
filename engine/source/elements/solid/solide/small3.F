Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  SMALLA3                       source/elements/solid/solide/small3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMALLA3(SAV, OFFG ,OFF  ,WXX,WYY,
     .                   WZZ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   OFFG(*),OFF(*),WXX(*),WYY(*),WZZ(*)
      DOUBLE PRECISION
     .  SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION X , Y, Z   ,DV   
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
      IF(ISMSTR==11)THEN
C----SAV : (ismstr=10,11) is differently saved to Ismstr=1      
       DO I=LFT,LLT
C-----------------------------------------------
         IF(OFFG(I)==ZERO) CYCLE
         X=SAV(I,1)
         Y=SAV(I,8)
         Z=SAV(I,15)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,2)
         Y=SAV(I,9)
         Z=SAV(I,16)
         SAV(I,2) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,9) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,16) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,3)
         Y=SAV(I,10)
         Z=SAV(I,17)
         SAV(I,3) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,10) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,17) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,11)
         Z=SAV(I,18)
         SAV(I,4) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,5)
         Y=SAV(I,12)
         Z=SAV(I,19)
         SAV(I,5) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,12) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,19) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,6)
         Y=SAV(I,13)
         Z=SAV(I,20)
         SAV(I,6) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,13) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,20) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,14)
         Z=SAV(I,21)
         SAV(I,7) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21) = Z - X*WYY(I) + Y*WXX(I) 
       ENDDO

      ELSEIF(ISMSTR<=4.AND.JLAG>0)THEN
       DO I=LFT,LLT
       IF(OFFG(I)>ONE)THEN
C-----------------------------------------------
         X=SAV(I,1)
         Y=SAV(I,2)
         Z=SAV(I,3)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,2) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,3) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,5)
         Z=SAV(I,6)
         SAV(I,4) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,5) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,6) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,8)
         Z=SAV(I,9)
         SAV(I,7) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,9) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,10)
         Y=SAV(I,11)
         Z=SAV(I,12)
         SAV(I,10) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,12) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,13)
         Y=SAV(I,14)
         Z=SAV(I,15)
         SAV(I,13) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,16)
         Y=SAV(I,17)
         Z=SAV(I,18)
         SAV(I,16) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,17) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,19)
         Y=SAV(I,20)
         Z=SAV(I,21)
         SAV(I,19) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,20) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21) = Z - X*WYY(I) + Y*WXX(I) 

        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMALLB3                       source/elements/solid/solide/small3.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMALLB3(OFFG ,OFF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real OFFG(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF(ISMSTR==1.OR.ISMSTR==3)THEN
        DO I=LFT,LLT
          IF(OFFG(I)>ZERO)OFFG(I)=TWO 
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        IF(OFF(I)<ONE)THEN
          IF(OFF(I)==ZERO)THEN
            OFFG(I)=ZERO
          ELSEIF(OFFG(I)>ONE)THEN
            OFFG(I) = ONE+OFF(I)
          ELSE
            OFFG(I) = OFF(I)
          END IF
        END IF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SMALLC3                       source/elements/solid/solide/small3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMALLC3(SAV, OFFG ,OFF  ,WXX,WYY,
     .                   WZZ,DXX,DYY,DZZ,RHO,RHOO,
     .                   DVOL,VOL,SDV  ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*),OFF(*),WXX(*),WYY(*),WZZ(*),
     .   DXX(*),DYY(*),DZZ(*),RHO(*),RHOO(*),DVOL(*),VOL(*),SDV(*)
      DOUBLE PRECISION
     .  SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION X , Y, Z    ,DV   ,DV1
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
      IF (ISMDISP > 0 .OR. ISMSTR == 11) THEN
       DO I=LFT,LLT
         DV=(DXX(I)+DYY(I)+DZZ(I))*DT1
         DV1=DV+SDV(I)
C-----------------------------
         RHO(I) = RHOO(I) - RHO(I)*DV1
         RHO(I) = MAX(RHO(I),EM30)
         DVOL(I)=VOL(I)*DV1
       ENDDO
      ELSEIF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
C----- to be done outside the integrating point cycle      
      ENDIF
      IF (ISMDISP>0) THEN
      ELSEIF(ISMSTR<=4.AND.JLAG>0)THEN
C      ISMSTR=3 <==> ISMSTR=1
       DO I=LFT,LLT
        IF(OFFG(I)>ONE)THEN
         DV=(DXX(I)+DYY(I)+DZZ(I))*DT1+SDV(I)
         RHO(I) = RHOO(I) - RHO(I)*DV
         RHO(I) = MAX(RHO(I),EM30)
         DVOL(I)=VOL(I)*DV
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMALLG3                       source/elements/solid/solide/small3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMALLG3(SAV, OFFG ,WXX,WYY,WZZ,
     .     R11, R12, R13, R21, R22, R23, R31, R32, R33,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   R11(*),R12(*),R13(*),
     .   R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),
     .   OFFG(*),WXX(*),WYY(*),WZZ(*)
      DOUBLE PRECISION
     .  SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,j
C     REAL
      DOUBLE PRECISION
     .   X , Y, Z   ,DV   ,WXXG,WYYG,WZZG
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
C------------Wxx,y,z from local system to global 
      IF (JCVT > 0 ) THEN
       DO I=LFT,LLT
         WXXG=R11(I)*WXX(I)+R12(I)*WYY(I)+R13(I)*WZZ(I)
         WYYG=R21(I)*WXX(I)+R22(I)*WYY(I)+R23(I)*WZZ(I)
         WZZG=R31(I)*WXX(I)+R32(I)*WYY(I)+R33(I)*WZZ(I)
         WXX(I) = WXXG
         WYY(I) = WYYG
         WZZ(I) = WZZG
       END DO
      END IF 
      IF (ISMSTR==11) THEN
C      IF(ISMSTR==11.AND.NCYCLE>1)THEN
C----SAV : (ismstr=10,11) is differently saved to Ismstr=1      
       DO I=LFT,LLT
C-----------------------------------------------
         IF(OFFG(I)==ZERO) CYCLE
         X=SAV(I,1)
         Y=SAV(I,8)
         Z=SAV(I,15)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,2)
         Y=SAV(I,9)
         Z=SAV(I,16)
         SAV(I,2) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,9) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,16)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,3)
         Y=SAV(I,10)
         Z=SAV(I,17)
         SAV(I,3)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,10) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,17) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,11)
         Z=SAV(I,18)
         SAV(I,4)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,5)
         Y=SAV(I,12)
         Z=SAV(I,19)
         SAV(I,5)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,12) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,19) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,6)
         Y=SAV(I,13)
         Z=SAV(I,20)
         SAV(I,6)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,13) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,20) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,14)
         Z=SAV(I,21)
         SAV(I,7)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21) = Z - X*WYY(I) + Y*WXX(I) 
       ENDDO
      ELSEIF(ISMSTR==1.OR.
     .  	((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3))THEN
       DO I=LFT,LLT
       IF(OFFG(I)>ONE)THEN
C-----------------------------------------------
         X=SAV(I,1)
         Y=SAV(I,2)
         Z=SAV(I,3)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,2) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,3) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,5)
         Z=SAV(I,6)
         SAV(I,4) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,5) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,6) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,8)
         Z=SAV(I,9)
         SAV(I,7) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,9) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,10)
         Y=SAV(I,11)
         Z=SAV(I,12)
         SAV(I,10) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,12) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,13)
         Y=SAV(I,14)
         Z=SAV(I,15)
         SAV(I,13) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,16)
         Y=SAV(I,17)
         Z=SAV(I,18)
         SAV(I,16) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,17) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,19)
         Y=SAV(I,20)
         Z=SAV(I,21)
         SAV(I,19) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,20) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21) = Z - X*WYY(I) + Y*WXX(I) 
C
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SRMALLA11                     source/elements/solid/solide/small3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SRMALLA11(SAV, OFFG ,WXX,WYY,WZZ,
     .     R11, R12, R13, R21, R22, R23, R31, R32, R33,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   OFFG(*),WXX(*),WYY(*),WZZ(*),
     .   R11(*),R12(*),R13(*),
     .   R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*)
      DOUBLE PRECISION
     .  SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,j
C     REAL
      DOUBLE PRECISION
     .   X , Y, Z    ,WXXG,WYYG,WZZG
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES(REF)POUR SMALL STRAIN
C-----------------------------
      IF(ISMSTR==11)THEN
C------------Wxx,y,z from local system to global 
       DO I=LFT,LLT
         WXXG=R11(I)*WXX(I)+R12(I)*WYY(I)+R13(I)*WZZ(I)
         WYYG=R21(I)*WXX(I)+R22(I)*WYY(I)+R23(I)*WZZ(I)
         WZZG=R31(I)*WXX(I)+R32(I)*WYY(I)+R33(I)*WZZ(I)
         WXX(I) = WXXG
         WYY(I) = WYYG
         WZZ(I) = WZZG
       END DO
       DO I=LFT,LLT
C-----------------------------------------------
         IF(OFFG(I)==ZERO) CYCLE
         X=SAV(I,1)
         Y=SAV(I,8)
         Z=SAV(I,15)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,2)
         Y=SAV(I,9)
         Z=SAV(I,16)
         SAV(I,2) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,9) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,16)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,3)
         Y=SAV(I,10)
         Z=SAV(I,17)
         SAV(I,3) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,10)= Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,17)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,11)
         Z=SAV(I,18)
         SAV(I,4) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11)= Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,5)
         Y=SAV(I,12)
         Z=SAV(I,19)
         SAV(I,5) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,12)= Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,19)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,6)
         Y=SAV(I,13)
         Z=SAV(I,20)
         SAV(I,6) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,13)= Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,20)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,14)
         Z=SAV(I,21)
         SAV(I,7) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14)= Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21)= Z - X*WYY(I) + Y*WXX(I) 
       ENDDO
	  ELSEIF(ISMSTR==12)THEN
C------------Wxx,y,z from local system to global 
       DO I=LFT,LLT
         IF(OFFG(I)<=ONE) CYCLE
         WXXG=R11(I)*WXX(I)+R12(I)*WYY(I)+R13(I)*WZZ(I)
         WYYG=R21(I)*WXX(I)+R22(I)*WYY(I)+R23(I)*WZZ(I)
         WZZG=R31(I)*WXX(I)+R32(I)*WYY(I)+R33(I)*WZZ(I)
         WXX(I) = WXXG
         WYY(I) = WYYG
         WZZ(I) = WZZG
       END DO
       DO I=LFT,LLT
C-----------------------------------------------
         IF(OFFG(I)<=ONE) CYCLE
         X=SAV(I,1)
         Y=SAV(I,2)
         Z=SAV(I,3)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,2) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,3) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,5)
         Z=SAV(I,6)
         SAV(I,4) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,5) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,6) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,8)
         Z=SAV(I,9)
         SAV(I,7) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,9) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,10)
         Y=SAV(I,11)
         Z=SAV(I,12)
         SAV(I,10) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,12) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,13)
         Y=SAV(I,14)
         Z=SAV(I,15)
         SAV(I,13) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,16)
         Y=SAV(I,17)
         Z=SAV(I,18)
         SAV(I,16) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,17) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,19)
         Y=SAV(I,20)
         Z=SAV(I,21)
         SAV(I,19) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,20) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21) = Z - X*WYY(I) + Y*WXX(I) 
       ENDDO
      END IF !(ISMSTR==11)THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  SMALLGEO3                     source/elements/solid/solide/small3.F
Chd|-- called by -----------
Chd|        S10MALLGEO3                   source/elements/solid/solide10/s10mall3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMALLGEO3(NGL, OFFG ,VOLG ,DELTAX, VOLG0 ,ITET )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITET,NGL(*)
      my_real
     .   OFFG(*),VOLG(*),DELTAX(*), VOLG0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ICOUNT,LIST(MVSIZ)
      my_real
     .   TET_COLLA(MVSIZ),COEF,DEF_V,AL
C---------------------------------------------
C----- MIN_ASPECT, MIN_DEFV will be defined in include file--
      IF(MIN_ASPECT==ZERO.AND.MIN_DEFV==ZERO) RETURN
      IF(ISMSTR/=2.AND.ISMSTR/=12) RETURN
      COEF = ONE
      IF (ITET >0) COEF = ONE/1.24/SQR3
C
      IF (MIN_DEFV >ZERO) THEN
       DO I=LFT,LLT
        IF (OFFG(I)>ONE.OR.OFFG(I)==ZERO) CYCLE
        DEF_V = VOLG(I)/VOLG0(I)
        IF (DEF_V < MIN_DEFV) THEN
         OFFG(I) = TWO
#include "lockon.inc"
         CALL ANCMSG(MSGID=267,ANMODE=ANINFO,
     .                    I1=NGL(I),
     .                    R1=DEF_V )
#include "lockoff.inc"
        END IF	
       END DO
      END IF !(MIN_DEFV)>ZERO) THEN
C      
      IF (MIN_ASPECT>ZERO) THEN
       ICOUNT = 0
       DO I=LFT,LLT
        IF (OFFG(I)>ONE.OR.OFFG(I)==ZERO) CYCLE
c       AL = VOLG(I)**(1.0D0/3.0D0)!  EXP(THIRD*LOG(VOLG(I)))
c       TET_COLLA(I)= COEF* (DELTAX(I)/AL)**(3.0D0/2.0D0)!COEF*EXP(THREE_HALF*LOG(DELTAX(I)/AL))
        TET_COLLA(I) = COEF * SQRT(DELTAX(I)**3 / VOLG(I))
        IF (TET_COLLA(I) < MIN_ASPECT) THEN
         OFFG(I) = TWO
         ICOUNT=ICOUNT+1
         LIST(ICOUNT)=I
        END IF	
       END DO
       IF (ICOUNT > 0) THEN
#include "lockon.inc"
          WRITE(IOUT,1000)
          WRITE(ISTDO,1000)
        DO J=1,ICOUNT
          I=LIST(J)
          WRITE(IOUT,'(6X,I10,1PG20.13)')NGL(I),TET_COLLA(I)
          WRITE(ISTDO,'(6X,I10,1PG20.13)')NGL(I),TET_COLLA(I)
        ENDDO
         WRITE(IOUT,*)
         WRITE(ISTDO,*)
#include "lockoff.inc"
       END IF !(ICOUNT > 0) THEN
      END IF !(MIN_ASPECT)>ZERO) THEN
C
      RETURN
 1000 FORMAT(/
     .    'THE FOLLOWING ELEMENTS WILL BE SWITCHED TO SMALL STRAIN DUE TO BAD ASPECT RATIO' /)
      END
