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
Chd|  CARM24                        source/materials/mat/mat024/carm24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CARM24(NEL  ,YMS  ,Y0S  ,ETS  ,EPXA ,SIGA ,
     .                  DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real, INTENT(IN) :: YMS,Y0S,ETS
      my_real, DIMENSION(NEL,3) ,INTENT(INOUT) :: SIGA,EPXA
      my_real, DIMENSION(NEL), INTENT(IN) :: DEPS1,DEPS2,DEPS3,
     .  DEPS4,DEPS5,DEPS6
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real HS
      my_real, DIMENSION(NEL) :: S01,S02,S03,S1,S2,S3,
     .   DE1,DE2,DE3,SCLE1,SCLE2,SCLE3,SCAL1,SCAL2,SCAL3
c=======================================================================
         HS = YMS*ETS / MAX(YMS-ETS,EM20)
c
         DO I = 1,NEL
            S1(I) = SIGA(I,1)+YMS*DEPS1(I)
            S2(I) = SIGA(I,2)+YMS*DEPS2(I)
            S3(I) = SIGA(I,3)+YMS*DEPS3(I)
            S01(I) = Y0S+HS*ABS(EPXA(I,1))
            S02(I) = Y0S+HS*ABS(EPXA(I,2))
            S03(I) = Y0S+HS*ABS(EPXA(I,3))
            SCLE1(I) = UNDEMI+SIGN(UNDEMI,ABS(S1(I))-S01(I) )
            SCLE2(I) = UNDEMI+SIGN(UNDEMI,ABS(S2(I))-S02(I) )
            SCLE3(I) = UNDEMI+SIGN(UNDEMI,ABS(S3(I))-S03(I) )
         ENDDO
         DO I = 1,NEL
            S01(I) = SIGN(S01(I),S1(I))
            S02(I) = SIGN(S02(I),S2(I))
            S03(I) = SIGN(S03(I),S3(I))
            SCAL1(I) = ABS(S1(I)-S01(I))/MAX(ABS(YMS*DEPS1(I)),EM20)
            SCAL2(I) = ABS(S2(I)-S02(I))/MAX(ABS(YMS*DEPS2(I)),EM20)
            SCAL3(I) = ABS(S3(I)-S03(I))/MAX(ABS(YMS*DEPS3(I)),EM20)
            DE1(I) = SCLE1(I)*SCAL1(I)*(UN-ETS/(YMS+EM10))*DEPS1(I)
            DE2(I) = SCLE2(I)*SCAL2(I)*(UN-ETS/(YMS+EM10))*DEPS2(I)
            DE3(I) = SCLE3(I)*SCAL3(I)*(UN-ETS/(YMS+EM10))*DEPS3(I)
            EPXA(I,1) = EPXA(I,1)+DE1(I)
            EPXA(I,2) = EPXA(I,2)+DE2(I)
            EPXA(I,3) = EPXA(I,3)+DE3(I)
         ENDDO
         DO I = 1,NEL
            S01(I) = S01(I)+HS*DE1(I)
            S02(I) = S02(I)+HS*DE2(I)
            S03(I) = S03(I)+HS*DE3(I)
            SIGA(I,1) = (UN-SCLE1(I))*S1(I)+SCLE1(I)*S01(I)
            SIGA(I,2) = (UN-SCLE2(I))*S2(I)+SCLE2(I)*S02(I)
            SIGA(I,3) = (UN-SCLE3(I))*S3(I)+SCLE3(I)*S03(I) 
         ENDDO
c--------------
         RETURN
         END
