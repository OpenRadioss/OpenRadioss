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
Chd|  R26DEF3                       source/elements/spring/r26def3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        R26SIG                        source/elements/spring/r26sig.F
Chd|====================================================================
      SUBROUTINE R26DEF3(F       ,E       ,DL      ,AL0     ,DV0     ,
     2                   FEP     ,DPL2    ,IPOS    ,GEO     ,IGEO    ,
     3                   NPF     ,TF      ,V       ,OFF     ,ANIM    ,
     4                   FR_WAVE ,AL0_ERR ,X1DP    ,X2DP    ,NGL     ,
     5                   MGN     ,EX      ,EY      ,EZ      ,XK      ,
     6                   XM      ,XC      ,AK      )
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
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "scr14_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*),IGEO(NPROPGI,*),NGL(*),MGN(*)
C     REAL
      my_real
     .   GEO(NPROPG,*), F(*), AL0(*), E(*), DL(*), TF(*), OFF(*),
     .   DV0(*), DPL2(*), FEP(*),ANIM(*),IPOS(*),FR_WAVE(*),V(3,*),
     .   AL0_ERR(*),EX(MVSIZ), EY(MVSIZ), EZ(MVSIZ),XK(MVSIZ),
     .   XM(MVSIZ),XC(MVSIZ),AK(MVSIZ)
      DOUBLE PRECISION X1DP(3,*),X2DP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, ILENG,NINDX, PID
      INTEGER NC1(MVSIZ), NC2(MVSIZ),INDX(MVSIZ)
C     REAL
      my_real
     .   DLOLD(MVSIZ),DMN(MVSIZ),DMX(MVSIZ),XL0(MVSIZ),
     .   DV(MVSIZ),DMAX(MVSIZ),ALPHA(MVSIZ)
      my_real
     .    DT11, BID, SUM
      DOUBLE PRECISION EXDP(MVSIZ),EYDP(MVSIZ),EZDP(MVSIZ),ALDP(MVSIZ),
     .                 AL0DP(MVSIZ)
C-----------------------------------------------
      DT11 = DT1
      IF (DT11 == ZERO) DT11 = EP30
c      
      DO I=LFT,LLT
        PID = MGN(I) 
        XM(I)   =GEO(1,PID)
        XK(I)   =GEO(2,PID)
        XC(I)   =ZERO
        DMAX(I) =GEO(3,PID)
        ALPHA(I)=GEO(4,PID)
        DMN(I)  =GEO(15,PID)
        DMX(I)  =GEO(16,PID)
        AK(I) = ONE
      ENDDO
C
      DO I=LFT,LLT
        EXDP(I)=X2DP(1,I)-X1DP(1,I)
        EYDP(I)=X2DP(2,I)-X1DP(2,I)
        EZDP(I)=X2DP(3,I)-X1DP(3,I)
        DLOLD(I)=DL(I)
        ALDP(I)=SQRT(EXDP(I)*EXDP(I)+EYDP(I)*EYDP(I)+EZDP(I)*EZDP(I))
      ENDDO
C
      IF (TT == ZERO) THEN
        DO I=LFT,LLT
          AL0(I)=ALDP(I)                   ! cast double vers My_real
          AL0_ERR(I)=ALDP(I)-AL0(I)        ! difference entre double et My_real
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        AL0DP(I) = AL0(I)                  ! cast My_real en double
        AL0DP(I) = AL0DP(I) + AL0_ERR(I)   ! AL_DP doit etre recalcule ainsi afin de garantir la coherence absolue entre AL0_DP et AL_DP   
      ENDDO
C
      DO I=LFT,LLT
        SUM  = MAX(ALDP(I),EM15)
        EXDP(I)= EXDP(I)/SUM
        EYDP(I)= EYDP(I)/SUM
        EZDP(I)= EZDP(I)/SUM
        EX(I)=EXDP(I)
        EY(I)=EYDP(I)
        EZ(I)=EZDP(I)
      ENDDO
C
      DO I=LFT,LLT
        DL(I) = ALDP(I) - AL0DP(I)
      ENDDO
C
      DO I=LFT,LLT
        ILENG = NINT(GEO(93,MGN(I)))
        IF (ILENG /= 0) THEN
          XL0(I)= AL0DP(I)
        ELSE
          XL0(I)=ONE
        ENDIF
      ENDDO
C
C-----
      CALL R26SIG(F       ,XK       ,DL        ,DLOLD    ,DMAX     ,
     .            E       ,OFF      ,XL0       ,TF       ,NPF      , 
     .            ANIM    ,ANIM_FE(11),FR_WAVE ,DMN      ,DMX      , 
     .            IGEO     ,GEO       ,MGN     ,DV0      ,ALPHA    )                    
C-----
      NINDX = 0
      DO I=LFT,LLT
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (DL(I) > DMX(I) .OR. DL(I) < DMN(I)) THEN
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
          ENDIF
        ENDIF
      ENDDO
      DO J=1,NINDX
        I = INDX(J)
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(I)
        WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
      ENDDO
      DO I=LFT,LLT
        XM(I)=XM(I)*XL0(I)
        XK(I)=XK(I)/XL0(I)
        XC(I)=XC(I)/XL0(I)
      ENDDO
C-----------
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C-----------
      RETURN
      END SUBROUTINE R26DEF3
