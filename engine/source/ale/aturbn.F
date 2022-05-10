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
Chd|  ATURBN                        source/ale/aturbn.F           
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ATURBN(PM,OFF  ,RHO  ,RK   ,RE   ,GEO  , 
     2               EINC ,DVOL ,VNEW ,PNEW ,TMU  ,VIS  ,
     3               VD2  ,MAT  ,NGEO )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NGEO(*)
      my_real
     .   PM(NPROPM,*), OFF(*), RHO(*), RK(*), RE(*),GEO(NPROPG,*)
      my_real
     .   EINC(*), DVOL(*), VNEW(*), PNEW(*), TMU(*), VIS(*),
     .   VD2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   EI, 
     .   XFAC, ARK, C1, C2, C3, SGSL, CMU, SE, FAC, RESGS     
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C
      IF(JPOR.NE.2)THEN
      
       DO I=LFT,LLT
         XFAC=TMU(I)/VIS(I)
         PNEW(I)=DEUX*RK(I)/(VNEW(I)*TROIS)
         EI  =XFAC*EINC(I)-DVOL(I)*PNEW(I)
         EI  =MAX(ZERO,EI)
         RK(I)  =RK(I)+(EI-RE(I)*DT1)*OFF(I)
         ARK= MAX(RK(I),EM15)
         C1 =PM(82,MAT(I))/ARK
         C2 =PM(83,MAT(I))/ARK
         C3 =PM(84,MAT(I))
         RE(I)=RE(I)*(UN +(C1*EI-C2*RE(I)*DT1+C3*DVOL(I)/VNEW(I))*OFF(I))
         RK(I)= MAX(EM15,RK(I)/VNEW(I))
         RE(I)= MAX(EM15,RE(I)/VNEW(I))
       ENDDO
       IF(JCLOSE.EQ.0)THEN
        DO I=LFT,LLT
          SGSL=PM(46,MAT(I))
          CMU=PM(81,MAT(I))
          C1 =PM(82,MAT(I))
          C2 =PM(83,MAT(I))
          SE =PM(86,MAT(I))
          FAC=SQRT(CMU/(SE*(C2-C1)))/SGSL
          RESGS=RHO(I)*FAC*(RK(I)/RHO(I))**TDEMI
          RE(I)= MAX(RE(I),RESGS)
        ENDDO
       ELSE
        DO I=LFT,LLT
          SGSL=MAX(2.*GEO(130,NGEO(I)),PM(46,MAT(I)))
          CMU=PM(81,MAT(I))
          C1 =PM(82,MAT(I))
          C2 =PM(83,MAT(I))
          SE =PM(86,MAT(I))
          FAC=SQRT(CMU/(SE*(C2-C1)))/MAX(SGSL,EM15)
          RESGS=RHO(I)*FAC*(RK(I)/RHO(I))**TDEMI
          RE(I)= MAX(RE(I),RESGS)
        ENDDO
       ENDIF
      ELSE
      !MILIEU POREUX OPTION TURBULENCE IMPOSEE
       DO I=LFT,LLT
         ! W VITESSE DE GRILLE EST SUPOSEE FAIBLE DEVANT V (QUASI EULERIEN)
         RK(I)=RHO(I)*GEO(22,NGEO(I))**2*VD2(I)
         RE(I)=(RK(I)**3/RHO(I))**UNDEMI/GEO(23,NGEO(I))
       ENDDO
       
      ENDIF! (JPOR.NE.2)
C
      RETURN
      END
