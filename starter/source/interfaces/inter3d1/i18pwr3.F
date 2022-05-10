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
Chd|  I18PWR3                       source/interfaces/inter3d1/i18pwr3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I18PWR3(ITAB,INACTI,CAND_E,CAND_N,STFN,
     1                  STF ,X     ,NSV   ,IWPENE,CAND_P,
     2                  CAND_EN,CAND_NN,TAG,NOINT,GAPV  ,
     3                  IX1,IX2,IX3,IX4,NSVG,
     4                  PENE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),CAND_E(*),CAND_N(*),CAND_EN(*),CAND_NN(*)
      INTEGER NSV(*),TAG(*),IWPENE,INACTI,NOINT
      my_real
     .   STF(*),STFN(*),X(3,*),CAND_P(*),GAPV(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: PENE
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real:: PENEOLD
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        DO I=LFT,LLT
          IF(IPRI.GE.1.AND.PENE(I).GT.ZERO)THEN
            WRITE(IOUT,FMT=FMW_5I)
     1       ITAB(NSVG(I)),
     2       ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))
          ELSEIF(IPRI.GE.6)THEN
            WRITE(IOUT,FMT=FMW_5I)
     1       ITAB(NSVG(I)),
     2       ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))
          ENDIF
          IF(PENE(I).GT.ZERO)THEN
            TAG(NSVG(I))=TAG(NSVG(I))+1
            CAND_P(IWPENE+1)  = ZERO
            CAND_NN(IWPENE+1) = CAND_N(I)
            CAND_EN(IWPENE+1) = CAND_E(I)
            IWPENE=IWPENE+1
          ENDIF
        ENDDO
C-----------------------------------------------
      RETURN
      END
