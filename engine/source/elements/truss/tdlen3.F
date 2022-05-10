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
Chd|  TDLEN3                        source/elements/truss/tdlen3.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TDLEN3(JFT  ,JLT   ,PM     ,OFFG,OFF   ,
     .                  DT2T ,NELTST,ITYPTST,MSTR,DMELTR,
     .                  DTEL ,NEL   ,G_DT   ,MAT ,NGL   ,
     .                  AL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "couple_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),NGL(MVSIZ)
      INTEGER, INTENT(IN)    :: NEL
      my_real, INTENT(INOUT) :: DTEL(NEL)
      INTEGER, INTENT(IN)    :: G_DT
      INTEGER JFT,JLT,NELTST,ITYPTST
      my_real DT2T,
     .   PM(NPROPM,*), OFF(*),OFFG(MVSIZ), MSTR(*), DMELTR(*),
     .   AL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SSP(MVSIZ), DT(MVSIZ)
C-----------------------------------------------      
      DO I=JFT,JLT
        SSP(I)=PM(27,MAT(I))
      ENDDO

      IF(IDTMINS/=2.OR.JSMS==0)THEN

       DO I=JFT,JLT
         DT(I)=DTFAC1(4)*AL(I)/SSP(I)
       ENDDO
       
       IF(NODADT.NE.0.AND.IDTMIN(4).EQ.0)RETURN

       DO I=JFT,JLT
         IF(OFF(I).LT.UN.OR.OFFG(I).LT.ZERO) CYCLE
         IF(IDTMIN(4).EQ.1.AND.DT(I).LT.DTMIN1(4))THEN
           TSTOP = TT
#include "lockon.inc"
           WRITE(IOUT,*) ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR TRUSS ELEMENT'
           WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR TRUSS ELEMENT'
#include "lockoff.inc"
         ELSEIF(IDTMIN(4).EQ.5.AND.DT(I).LT.DTMIN1(4))THEN
           MSTOP = 2
#include "lockon.inc"
           WRITE(IOUT,*) ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR TRUSS ELEMENT'
           WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR TRUSS ELEMENT'
#include "lockoff.inc"
         ELSEIF(IDTMIN(4).EQ.2.AND.DT(I).LT.DTMIN1(4))THEN
           OFF(I) = ZERO
#include "lockon.inc"
           WRITE(IOUT,*) '-- DELETE  OF TRUSS ELEMENT NUMBER',NGL(I)
#include "lockoff.inc"
           IDEL7NOK = 1
         ENDIF
         IF(NODADT.NE.0.OR.DT(I).GE.DT2T) CYCLE
         DT2T=DT(I)
         NELTST =NGL(I)
         ITYPTST=4
       ENDDO!next I
C----
      ELSE!(IDTMINS==2.AND.JSMS/=0)
       !IDTMINS=2 & JSMS=1 <=> AMS & elementary time step
       DO I=JFT,JLT
         DT(I)=AL(I)/SSP(I)
       END DO
       DO I=JFT,JLT
         IF(OFF(I).LT.UN.OR.OFFG(I).LT.ZERO) CYCLE
          DMELTR(I) = MAX(DMELTR(I) , MSTR(I)*((DTMINS/(DTFACS*DT(I)))**2 - UN))
          DT(I)     = DTFACS*SQRT(UN+DMELTR(I)/MSTR(I))*DT(I)
       END DO
       DO I=JFT,JLT
         IF(OFF(I).LT.UN.OR.OFFG(I).LT.ZERO) CYCLE
          IF(DT(I).LT.DT2T)THEN
            DT2T    = DT(I)
            NELTST  = NGL(I)
            ITYPTST = 4
          END  IF
        END DO         
      END IF!(IDTMINS/=2.OR.JSMS==0)
C-----------------------------------------------           
      IF(G_DT.NE.ZERO)THEN
        DO I=LFT,LLT
          DTEL(I) = DT(I)
        ENDDO
      ENDIF
C-----------------------------------------------      
      
      RETURN
      END
