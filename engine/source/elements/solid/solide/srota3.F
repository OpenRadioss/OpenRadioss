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
C ds008  18/2/00  +2
Chd|====================================================================
Chd|  SROTA3                        source/elements/solid/solide/srota3.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTA3(SIG ,S1  ,S2  ,S3  ,S4  ,
     .                  S5  ,S6  ,WXX ,WYY ,WZZ ,
     .                  NEL )
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
      INTEGER NEL
      my_real
     .   SIG(NEL,6),WXX(*), WYY(*), WZZ(*),
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ), S4(MVSIZ), S5(MVSIZ), S6(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .   Q1, Q2, Q3
C=======================================================================
      S1(1:NEL)=SIG(1:NEL,1)
      S2(1:NEL)=SIG(1:NEL,2)
      S3(1:NEL)=SIG(1:NEL,3)
      S4(1:NEL)=SIG(1:NEL,4)
      S5(1:NEL)=SIG(1:NEL,5)
      S6(1:NEL)=SIG(1:NEL,6)

C
      IF (MTN==6.OR.MTN==17.OR.MTN==46.OR.MTN==47
     .   .OR.ISMSTR==10.OR.ISMSTR==12) RETURN
C
      DO I=1,NEL
        Q1 = TWO*S4(I)*WZZ(I)
        Q2 = TWO*S6(I)*WYY(I)
        Q3 = TWO*S5(I)*WXX(I)
!
        SIG(I,1)=S1(I)-Q1+Q2
        SIG(I,2)=S2(I)+Q1-Q3
        SIG(I,3)=S3(I)-Q2+Q3
        SIG(I,4)=S4(I)+WZZ(I)*(S1(I)-S2(I))+WYY(I)*S5(I)-WXX(I)*S6(I)
        SIG(I,5)=S5(I)+WXX(I)*(S2(I)-S3(I))+WZZ(I)*S6(I)-WYY(I)*S4(I)
        SIG(I,6)=S6(I)+WYY(I)*(S3(I)-S1(I))+WXX(I)*S4(I)-WZZ(I)*S5(I)
      ENDDO
C-----------    
      RETURN
      END
