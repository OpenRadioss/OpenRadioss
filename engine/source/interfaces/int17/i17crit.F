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
Cas|         I17MAIN_PENA            /int17/i17main_pena.F
Chd|====================================================================
Chd|  I17CRIT                       source/interfaces/int17/i17crit.F
Chd|-- called by -----------
Chd|        I17MAIN                       source/interfaces/int17/i17main.F
Chd|        I17MAIN_CRIT_TRI              source/interfaces/int17/i17main_pena.F
Chd|-- calls ---------------
Chd|        I16BOX                        source/interfaces/int16/i16crit.F
Chd|        I20BOX                        source/interfaces/int16/i16crit.F
Chd|====================================================================
       SUBROUTINE I17CRIT(
     1       X      ,V      ,A       ,NELEM ,EMINX  ,
     2       NME    ,ITASK  ,XSAV    ,IXS   ,IXS16  ,
     3       IXS20  ,XMSR_G ,SIZE_T_G)
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
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK,NME,
     .        NELEM(*),IXS(NIXS,*),IXS16(8,*),IXS20(12,*)
      my_real
     .   X(3,*),V(3,*),A(3,*),XSAV(3,*),EMINX(6,*),
     .   XMSR_G(6),SIZE_T_G
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMEF,NSNL,NMEL,I, J, II, K,I16,I20,LFT16,LLT16,
     .        LFT20,LLT20,INDEX16(MVSIZ),INDEX20(MVSIZ)
      my_real
     .   XMSR(6), SIZE_T
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C------------------------------------
      NMEF = 1 + ITASK*NME / NTHREAD
      NMEL = (ITASK+1)*NME / NTHREAD
C--------------------------------------------------------------
C     0- CALCUL DU CRITERE POUR SAVOIR SI ON DOIT TRIER OU NON
C--------------------------------------------------------------
      XMSR(1) = -EP30
      XMSR(2) = -EP30
      XMSR(3) = -EP30
      XMSR(4) =  EP30
      XMSR(5) =  EP30
      XMSR(6) =  EP30    
      SIZE_T  =  ZERO
C------------------------------------
C     CALCUL DES BORNES DES ELEMENTS
C------------------------------------
      DO I=NMEF,NMEL
        EMINX(1,I) = EP30
        EMINX(2,I) = EP30
        EMINX(3,I) = EP30
        EMINX(4,I) = -EP30
        EMINX(5,I) = -EP30
        EMINX(6,I) = -EP30      
      ENDDO
C
      LFT16=1
      LLT16=0
      LFT20=1
      LLT20=0
      DO I=NMEF,NMEL
          I20=NELEM(I)-NUMELS8-NUMELS10
          I16=I20-NUMELS20
          IF(I16>=1.AND.I16<=NUMELS16)THEN
            LLT16=LLT16+1
            INDEX16(LLT16)=I
            IF(LLT16==MVSIZ-1)THEN
              CALL I16BOX(
     1            LFT16,LLT16  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS16,SIZE_T,
     3            XMSR ,INDEX16,XSAV  )
              LLT16=0
            ENDIF
          ELSEIF(I20>=1.AND.I20<=NUMELS20)THEN
            LLT20=LLT20+1
            INDEX20(LLT20)=I
            IF(LLT20==MVSIZ-1)THEN
              CALL I20BOX(
     1            LFT20,LLT20  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS20,SIZE_T,
     3            XMSR ,INDEX20,XSAV  )
              LLT20=0
            ENDIF
          ENDIF
      END DO
      IF(LLT16>0)CALL I16BOX(
     1            LFT16,LLT16  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS16,SIZE_T,
     3            XMSR ,INDEX16,XSAV  )
      IF(LLT20>0)CALL I20BOX(
     1            LFT20,LLT20  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS20,SIZE_T,
     3            XMSR ,INDEX20,XSAV  )
C
#include "lockon.inc"
      XMSR_G(1)=MAX(XMSR_G(1),XMSR(1))
      XMSR_G(2)=MAX(XMSR_G(2),XMSR(2))
      XMSR_G(3)=MAX(XMSR_G(3),XMSR(3))
      XMSR_G(4)=MIN(XMSR_G(4),XMSR(4))
      XMSR_G(5)=MIN(XMSR_G(5),XMSR(5))
      XMSR_G(6)=MIN(XMSR_G(6),XMSR(6))
      SIZE_T_G = SIZE_T_G + SIZE_T
#include "lockoff.inc"
C
      RETURN
      END
