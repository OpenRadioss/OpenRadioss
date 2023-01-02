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
Chd|  RDTIME33                      source/elements/joint/rdtime33.F
Chd|-- called by -----------
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RDTIME33(JFT   ,JLT    ,DT2T  ,NELTST,ITYPTST,
     2                    IXR   ,EINT   ,STI   ,STIR  ,OFF   ,
     3                    XKM   ,XKR   ,XCM    ,XCR   ,UMAS  ,
     4                    UINER ,FX     ,FY    ,FZ    ,
     5                    XMOM  ,YMOM  ,ZMOM   ,ROT1  ,ROT2  ,
     6                    MSRT  ,DMELRT,NUVAR  ,UVAR  ,JNTYP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr02_c.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IXR(NIXR,*), NELTST, ITYPTST,
     .   NUVAR,JNTYP
      my_real DT2T, STI(3,*), STIR(3,*), OFF(*), EINT(*) ,  
     .   XKM(*),XKR(*),XCM(*),XCR(*),UMAS(*),UINER(*),
     .   FX(*), FY(*), FZ(*), XMOM(*), YMOM(*),ZMOM(*),
     .   ROT1(*), ROT2(*), MSRT(*), DMELRT(*),
     .   UVAR(NUVAR,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real DX(3),THETA(3),E0(MVSIZ),TH1,TH2,TH3,
     .   DT(JLT-JFT+1), DTC(1), DTINV,  A, MASS2, IN2, DTA, DTB, MX2,
     .   MS, IN	 
C=======================================================================
C     nodal time step
C--------------------------------------------
      IF((IDTMINS/=2).AND.(JNTYP==33)) NODADT = 1
C
      DO I=JFT,JLT
        STI(1,I) = XKM(I) 
        STIR(1,I) = XKR(I)
        STI(2,I) = STI(1,I) 
        STIR(2,I) = STIR(1,I)
	
C-------- Masse harmonique --------------
        MS = (UVAR(34,I)*UVAR(35,I))/MAX(EM20,UVAR(34,I)+UVAR(35,I))
        IN = (UVAR(36,I)*UVAR(37,I))/MAX(EM20,UVAR(36,I)+UVAR(37,I))
				
C-------- Node 1--------------
	IF (MS>EM15)
     .    STI(1,I) = ((XCM(I)+SQRT(XCM(I)**2+XKM(I)*MS))**2)/MS		
	IF (IN>EM15)
     .    STIR(1,I) = ((XCR(I)+SQRT(XCR(I)**2+XKR(I)*IN))**2)/IN
     
C-------- Node 2--------------					
        STI(2,I) = STI(1,I) 
        STIR(2,I) = STIR(1,I)
	 
      ENDDO
C
      IF(IDTMINS==2.AND.JSMS/=0)THEN
C
        DTA=DTMINS/DTFACS
        DTB=DTA*DTA
        DO I=JFT,JLT
         IF(OFF(I)<=ZERO) CYCLE
          XKM(I)   = MAX(EM15,XKM(I))
          DMELRT(I)=MAX(DMELRT(I),
     .      XCM(I)*DTA+HALF*XKM(I)*DTB-HALF*MSRT(I))
C
C         MX2 = 2*(Mn+2*DeltaM)
          MX2 =MSRT(I)+TWO*DMELRT(I)
          DT(I)=DTFACS*
     .      MX2 /MAX(EM15,SQRT(XCM(I)*XCM(I)+MX2*XKM(I))+XCM(I))
          DT(I)=DTMINS
        ENDDO
C
        DO I=JFT,JLT
         IF(OFF(I)<=ZERO) CYCLE
         IF(DT(I)<DT2T) THEN
          DT2T=DT(I)
          NELTST =IXR(NIXR,I)
          ITYPTST=6
         ENDIF
        ENDDO
C
      ENDIF
C-----------------------------------------------
      RETURN
      END
