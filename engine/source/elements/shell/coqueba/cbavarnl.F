Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CBAVARNL                      source/elements/shell/coqueba/cbavarnl.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE CBAVARNL(JFT      ,JLT      ,NG       ,IXC      ,NLOC_DMG ,
     .                    VARNL    ,NDDL     ,NC1      ,NC2      ,NC3      ,
     .                    NC4      ,NEL      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NG,IXC(NIXC,*),NDDL,NEL
      INTEGER, INTENT(INOUT) :: 
     .   NC1(*), NC2(*), NC3(*), NC4(*)
      MY_REAL, INTENT(INOUT) ::
     .   VARNL(NEL,NDDL)
      TYPE(NLOCAL_STR_), TARGET :: NLOC_DMG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  K, 
     .        INOD(4), IPOS(4),L_NLOC
      MY_REAL 
     .   VPG(2,4),PG1,PG,KSI,ETA,SF1,SF2,SF3,SF4
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
      MY_REAL, 
     .   DIMENSION(:), POINTER :: DNL 
C--------------------------
C     INITIALISATION
C-------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C---------------------------------------------------------------------------------------------------------------
C --- Non-local variable VARNL in Gauss point NG
C    
C --- Position of the Gauss point
       KSI = VPG(1,NG)
       ETA = VPG(2,NG)
C --- Shape function value
       SF1 = (1-KSI)*(1-ETA)
       SF2 = (1+KSI)*(1-ETA)
       SF3 = (1+KSI)*(1+ETA)
       SF4 = (1-KSI)*(1+ETA)
C --- Length of the non-local vector       
       L_NLOC = NLOC_DMG%L_NLOC
       DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
C --- Loop over 
       DO I=JFT,JLT       
         NC1(I)  = IXC(2,I)
         NC2(I)  = IXC(3,I)
         NC3(I)  = IXC(4,I)
         NC4(I)  = IXC(5,I)
       ENDDO
       DO K = 1,NDDL
#include "vectorize.inc"
         DO I=JFT,JLT
           INOD(1) = NLOC_DMG%IDXI(NC1(I))
           INOD(2) = NLOC_DMG%IDXI(NC2(I))
           INOD(3) = NLOC_DMG%IDXI(NC3(I))
           INOD(4) = NLOC_DMG%IDXI(NC4(I)) 
           IPOS(1) = NLOC_DMG%POSI(INOD(1)) 
           IPOS(2) = NLOC_DMG%POSI(INOD(2)) 
           IPOS(3) = NLOC_DMG%POSI(INOD(3)) 
           IPOS(4) = NLOC_DMG%POSI(INOD(4)) 
           VARNL(I,K) = ZERO
           VARNL(I,K) = FOURTH*(SF1*DNL(IPOS(1)+K-1) + SF2*DNL(IPOS(2)+K-1) 
     .                       + SF3*DNL(IPOS(3)+K-1) + SF4*DNL(IPOS(4)+K-1))
         ENDDO
       ENDDO
C
      END
