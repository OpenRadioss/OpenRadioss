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
Chd|  CDK6UPDT3                     source/elements/sh3n/coquedk6/cdk6updt3.F
Chd|-- called by -----------
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDK6UPDT3(JFT  ,JLT ,F ,M , NVC  ,
     2                   OFFG ,OFF ,STI ,STIR,STIFN,
     3                   STIFR,IXTG,IXTG1, F11  ,
     4                   F12    ,F13  ,F21 ,F22 ,F23  ,
     5                   F31    ,F32  ,F33 ,F14 ,F15  ,
     7                   F16    ,F24  ,F25 ,F26 ,F34 ,
     8                   F35    ,F36  ,NVS ,IVS )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NVC,IXTG(NIXTG,*),IXTG1(4,*),NVS,IVS(*)
      my_real  
     .   OFFG(*), OFF(*), STI(*), STIR(*), 
     .   F(3,*), M(3,*), STIFN(*), STIFR(*)
      my_real 
     .   F11(*), F12(*), F13(*),
     .   F21(*), F22(*), F23(*), F31(*), F32(*), F33(*),
     .   F14(*), F15(*), F16(*),F24(*), F25(*), F26(*),
     .   F34(*), F35(*), F36(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVC1, NVC2, NVC3, I, J, EP
      INTEGER NC1, NC2, NC3,NC,NCJ
C-----------------------------------------------
C
      DO 20 I=JFT,JLT
        IF(OFF(I)<1.)OFFG(I) = OFF(I)
 20   CONTINUE
C
C      NVC1= NVC/8
C      NVC2=(NVC-NVC1*8)/4
C      NVC3=(NVC-NVC1*8-NVC2*4)/2
C
       DO I=JFT,JLT
        NC = IXTG(2,I)
        F(1,NC)=F(1,NC)-F11(I)
        F(2,NC)=F(2,NC)-F21(I)
        F(3,NC)=F(3,NC)-F31(I)
        STIFN(NC)=STIFN(NC)+STI(I)
       ENDDO
       DO I=JFT,JLT
        NC = IXTG(3,I)
        F(1,NC)=F(1,NC)-F12(I)
        F(2,NC)=F(2,NC)-F22(I)
        F(3,NC)=F(3,NC)-F32(I)
        STIFN(NC)=STIFN(NC)+STI(I)
       ENDDO
       DO I=JFT,JLT
        NC = IXTG(4,I)
        F(1,NC)=F(1,NC)-F13(I)
        F(2,NC)=F(2,NC)-F23(I)
        F(3,NC)=F(3,NC)-F33(I)
        STIFN(NC)=STIFN(NC)+STI(I)
       ENDDO
C---------avec voisins----------------------
       DO EP=JFT,NVS
        I =IVS(EP)
        NCJ = IXTG1(1,I)
        F(1,NCJ)=F(1,NCJ)-F14(I)
        F(2,NCJ)=F(2,NCJ)-F24(I)
        F(3,NCJ)=F(3,NCJ)-F34(I)
       ENDDO
       DO EP=JFT,NVS
        I =IVS(EP)
        NCJ = IXTG1(2,I)
        F(1,NCJ)=F(1,NCJ)-F15(I)
        F(2,NCJ)=F(2,NCJ)-F25(I)
        F(3,NCJ)=F(3,NCJ)-F35(I)
       ENDDO
       DO EP=JFT,NVS
        I =IVS(EP)
        NCJ = IXTG1(3,I)
        F(1,NCJ)=F(1,NCJ)-F16(I)
        F(2,NCJ)=F(2,NCJ)-F26(I)
        F(3,NCJ)=F(3,NCJ)-F36(I)
       ENDDO
C---------sans voisins----------------------
       DO EP=NVS+1,JLT
        I =IVS(EP)
        NCJ = IXTG1(1,I)
        IF (NCJ > 0) THEN
         F(1,NCJ)=F(1,NCJ)-F14(I)
         F(2,NCJ)=F(2,NCJ)-F24(I)
         F(3,NCJ)=F(3,NCJ)-F34(I)
        ENDIF 
       ENDDO
       DO EP=NVS+1,JLT
        I =IVS(EP)
        NCJ = IXTG1(2,I)
        IF (NCJ > 0) THEN
         F(1,NCJ)=F(1,NCJ)-F15(I)
         F(2,NCJ)=F(2,NCJ)-F25(I)
         F(3,NCJ)=F(3,NCJ)-F35(I)
        ENDIF 
       ENDDO
       DO EP=NVS+1,JLT
        I =IVS(EP)
        NCJ = IXTG1(3,I)
        IF (NCJ > 0) THEN
         F(1,NCJ)=F(1,NCJ)-F16(I)
         F(2,NCJ)=F(2,NCJ)-F26(I)
         F(3,NCJ)=F(3,NCJ)-F36(I)
        ENDIF 
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CDK6UPDT3P                    source/elements/sh3n/coquedk6/cdk6updt3.F
Chd|-- called by -----------
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDK6UPDT3P(JFT,JLT ,OFFG ,OFF,STI,
     2                   STIR,FSKY,FSKYV,IADTG,IADTG1,
     4             F11    ,F12  ,F13  ,F21 ,F22 ,
     5             F23    ,F31  ,F32  ,F33 ,F14 ,
     7             F15    ,F16  ,F24  ,F25 ,F26 ,
     8             F34    ,F35  ,F36  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IADTG(3,*),IADTG1(3,*)
      my_real
     .   OFFG(*), OFF(*), STI(*), STIR(*), FSKYV(LSKY,8),
     .   FSKY(8,LSKY)
      my_real 
     .   F11(*), F12(*), F13(*),
     .   F21(*), F22(*), F23(*), F31(*), F32(*), F33(*),
     .   F14(*), F15(*), F16(*), F24(*), F25(*), F26(*),
     .   F34(*), F35(*), F36(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K
C-----------------------------------------------
      DO 20 I=JFT,JLT
        IF(OFF(I)<1.)OFFG(I) = OFF(I)
 20   CONTINUE
C
      IF (IVECTOR==1) THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          K = IADTG(1,I)
          FSKYV(K,1)=-F11(I)
          FSKYV(K,2)=-F21(I)
          FSKYV(K,3)=-F31(I)
          FSKYV(K,7)=STI(I)
          K = IADTG(2,I)
          FSKYV(K,1)=-F12(I)
          FSKYV(K,2)=-F22(I)
          FSKYV(K,3)=-F32(I)
          FSKYV(K,7)=STI(I)
          K = IADTG(3,I)
          FSKYV(K,1)=-F13(I)
          FSKYV(K,2)=-F23(I)
          FSKYV(K,3)=-F33(I)
          FSKYV(K,7)=STI(I)
        ENDDO
        DO I=JFT,JLT
          K = IADTG1(1,I)
          IF (K>0) THEN
           FSKYV(K,1)=-F14(I)
           FSKYV(K,2)=-F24(I)
           FSKYV(K,3)=-F34(I)
          ENDIF 
          K = IADTG1(2,I)
          IF (K>0) THEN
           FSKYV(K,1)=-F15(I)
           FSKYV(K,2)=-F25(I)
           FSKYV(K,3)=-F35(I)
          ENDIF 
          K = IADTG1(3,I)
          IF (K>0) THEN
           FSKYV(K,1)=-F16(I)
           FSKYV(K,2)=-F26(I)
           FSKYV(K,3)=-F36(I)
          ENDIF 
        ENDDO
      ELSE
        DO I=JFT,JLT
          K = IADTG(1,I)
          FSKY(1,K)=-F11(I)
          FSKY(2,K)=-F21(I)
          FSKY(3,K)=-F31(I)
          FSKY(7,K)=STI(I)
          K = IADTG(2,I)
          FSKY(1,K)=-F12(I)
          FSKY(2,K)=-F22(I)
          FSKY(3,K)=-F32(I)
          FSKY(7,K)=STI(I)
          K = IADTG(3,I)
          FSKY(1,K)=-F13(I)
          FSKY(2,K)=-F23(I)
          FSKY(3,K)=-F33(I)
          FSKY(7,K)=STI(I)
        ENDDO
        DO I=JFT,JLT
          K = IADTG1(1,I)
          IF (K>0) THEN
           FSKY(1,K)=-F14(I)
           FSKY(2,K)=-F24(I)
           FSKY(3,K)=-F34(I)
          ENDIF 
          K = IADTG1(2,I)
          IF (K>0) THEN
           FSKY(1,K)=-F15(I)
           FSKY(2,K)=-F25(I)
           FSKY(3,K)=-F35(I)
          ENDIF 
          K = IADTG1(3,I)
          IF (K>0) THEN
           FSKY(1,K)=-F16(I)
           FSKY(2,K)=-F26(I)
           FSKY(3,K)=-F36(I)
          ENDIF 
        ENDDO
      ENDIF
C
      RETURN
      END
