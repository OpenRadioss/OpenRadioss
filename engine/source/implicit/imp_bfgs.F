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
Chd|  BFGS_INI                      source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_INI(NDDL,MAX_BFGS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NDDL,MAX_BFGS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IER1,IER2,IER3,LV
C------------------------------------------
C
      IF (INSOLV==5) THEN
       IF(ALLOCATED(BFGS_V)) DEALLOCATE(BFGS_V)
       ALLOCATE(BFGS_V(NDDL,1),STAT=IER2)
      ELSE
       IF(L_BFGS==0) THEN
        LV = MAX_BFGS
       ELSE
        LV = L_BFGS
       ENDIF
C
       IF(ALLOCATED(BFGS_V)) DEALLOCATE(BFGS_V)
       ALLOCATE(BFGS_V(NDDL,LV),STAT=IER2)
C
       IF(ALLOCATED(BFGS_W)) DEALLOCATE(BFGS_W)
       ALLOCATE(BFGS_W(NDDL,LV),STAT=IER3)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_0                        source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_0
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C------------------------------------------
       N_BFGS = 0
       S_LIN = ONE
C
       IACTB = 0
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_LS                       source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_LS(LS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   LS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C------------------------------------------
       S_LIN = LS
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_1                        source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        BFGS_RHD                      source/implicit/imp_bfgs.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_1(NDDL,W_DDL,F,A2,IT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NDDL,W_DDL(*),IT
      my_real
     .   F(*) ,A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
      my_real
     .   A1 ,B1
C------------------------------------------
       IF (IT==0.OR.(IACTB==0.AND.IT<2)) RETURN
       N = N_BFGS + 1
C--------V->dr----------------------------------
       A2=ZERO
       DO I=1,NDDL
        BFGS_V(I,N) = F(I) - BFGS_V(I,N)
       ENDDO
       CALL PRODUT_W(NDDL,BFGS_W(1,N),BFGS_V(1,N),W_DDL,A1)
       IF (ABS(A1)>EM10) THEN
         N_BFGS = N
         CALL PRODUT_W(NDDL,BFGS_W(1,N),F,W_DDL,A2)
         A2 = A2*S_LIN
C--------W->b1*du----------------------------------
         B1 = ONE/A1
         DO I=1,NDDL
          BFGS_W(I,N) = B1*BFGS_W(I,N)
         ENDDO
       ELSE
       ENDIF
       DO I=N_BFGS,1,-1
        CALL BFGS_RHD(NDDL,W_DDL,BFGS_W(1,I),BFGS_V(1,I),F)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_RHD                      source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        BFGS_1                        source/implicit/imp_bfgs.F    
Chd|        BFGS_1P                       source/implicit/imp_bfgs.F    
Chd|        BFGS_2                        source/implicit/imp_bfgs.F    
Chd|        BFGS_2P                       source/implicit/imp_bfgs.F    
Chd|-- calls ---------------
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE BFGS_RHD(NDDL,W_DDL,BW,BV,B)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NDDL,W_DDL(*)
      my_real
     .   BW(*),BV(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  A1
C------------------------------------------
      CALL PRODUT_W(NDDL,BW,B,W_DDL,A1)
C
      DO I=1,NDDL
       B(I) = B(I) - A1*BV(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_2                        source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        BFGS_RHD                      source/implicit/imp_bfgs.F    
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_2(NDDL,W_DDL,U,F,A2,IT,MAX_BFGS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NDDL,W_DDL(*),IT,MAX_BFGS
      my_real
     .   F(*)  ,U(*),A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
C------------------------------------------
       IF (IACTB==0.AND.IT==0) RETURN
       IACTB = 1
       IF (IT==0.AND.L_BFGS==0) N_BFGS = 0
       IF(N_BFGS>0) THEN
        DO I=1,N_BFGS
         CALL BFGS_RHD(NDDL,W_DDL,BFGS_V(1,I),BFGS_W(1,I),U)
        ENDDO
        IF (A2/=ZERO) THEN
         DO I=1,NDDL
          U(I) = U(I) - A2*BFGS_W(I,N_BFGS)
         ENDDO
        ENDIF
       ENDIF
       IF (L_BFGS>0) THEN
        IF (N_BFGS<L_BFGS) THEN
         N = N_BFGS + 1
         DO I=1,NDDL
          BFGS_W(I,N) = U(I)
          BFGS_V(I,N) = F(I)
         ENDDO
        ELSEIF (N_BFGS==L_BFGS) THEN
         N_BFGS=L_BFGS-1
         DO N=1,N_BFGS
          DO I=1,NDDL
           BFGS_W(I,N) = BFGS_W(I,N+1)
           BFGS_V(I,N) = BFGS_V(I,N+1)
          ENDDO
         ENDDO
         DO I=1,NDDL
          BFGS_W(I,N_BFGS+1) = U(I)
          BFGS_V(I,N_BFGS+1) = F(I)
         ENDDO
        ENDIF
       ELSE
       IF (N_BFGS<MAX_BFGS) THEN
        N = N_BFGS + 1
        DO I=1,NDDL
         BFGS_W(I,N) = U(I)
         BFGS_V(I,N) = F(I)
        ENDDO
       ENDIF
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_1P                       source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        BFGS_RHD                      source/implicit/imp_bfgs.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_1P(NDDL,W_DDL,F,A2,IT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NDDL,W_DDL(*),IT
      my_real
     .   F(*) ,A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
      my_real
     .   A1 ,B1
C------------------------------------------
       IF (IT==0.OR.(IACTB==0.AND.IT<2)) RETURN
       N = N_BFGS + 1
C--------V->dr----------------------------------
       DO I=1,NDDL
        BFGS_V(I,N) = F(I) - BFGS_V(I,N)
       ENDDO
       CALL PRODUT_W(NDDL,BFGS_W(1,N),BFGS_V(1,N),W_DDL,A1)
       CALL PRODUT_W(NDDL,BFGS_W(1,N),F,W_DDL,A2)
       A1 = S_LIN*A1
       IF (ABS(A2)>EM10) THEN
        B1=-A1/A2
        IF (ABS(A1)>EM10.AND.B1>ZERO) THEN
         N_BFGS = N
C--------W->b1*du----------------------------------
         B1 = SQRT(B1)
         DO I=1,NDDL
          BFGS_W(I,N) = BFGS_W(I,N)/A1
          BFGS_V(I,N) = BFGS_V(I,N)-B1*F(I)
         ENDDO
        ENDIF
       ENDIF
       DO I=N_BFGS,1,-1
        CALL BFGS_RHD(NDDL,W_DDL,BFGS_W(1,I),BFGS_V(1,I),F)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_2P                       source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        BFGS_RHD                      source/implicit/imp_bfgs.F    
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_2P(NDDL,W_DDL,U,F,A2,IT,MAX_BFGS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NDDL,W_DDL(*),IT,MAX_BFGS
      my_real
     .   F(*)  ,U(*),A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
C------------------------------------------
       IF (IACTB==0.AND.IT==0) RETURN
       IACTB = 1
       IF (IT==0.AND.L_BFGS==0) N_BFGS = 0
C
       IF(N_BFGS>0) THEN
        DO I=1,N_BFGS
         CALL BFGS_RHD(NDDL,W_DDL,BFGS_V(1,I),BFGS_W(1,I),U)
        ENDDO
       ENDIF
C
       IF (L_BFGS>0) THEN
        IF (N_BFGS<L_BFGS) THEN
         N = N_BFGS + 1
         DO I=1,NDDL
          BFGS_W(I,N) = U(I)
          BFGS_V(I,N) = F(I)
         ENDDO
        ELSEIF (N_BFGS==L_BFGS) THEN
         N_BFGS=L_BFGS-1
         DO N=1,N_BFGS
          DO I=1,NDDL
           BFGS_W(I,N) = BFGS_W(I,N+1)
           BFGS_V(I,N) = BFGS_V(I,N+1)
          ENDDO
         ENDDO
         DO I=1,NDDL
          BFGS_W(I,N_BFGS+1) = U(I)
          BFGS_V(I,N_BFGS+1) = F(I)
         ENDDO
        ENDIF
       ELSE
        IF (N_BFGS<MAX_BFGS) THEN
         N = N_BFGS + 1
         DO I=1,NDDL
          BFGS_W(I,N) = U(I)
          BFGS_V(I,N) = F(I)
         ENDDO
        ENDIF
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  NSLOAN_0                      source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE NSLOAN_0(NDDL0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL0
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       N_BFGS = NDDL0
       S_LIN = ONE
C------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  NSLOAN_5                      source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        D_TO_U                        source/implicit/produt_v.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE NSLOAN_5(NDDL  ,IDDL  ,NDOF  ,IKC   ,W_DDL  ,
     .                    DD    ,DDR   ,U     ,F     ,ICONV  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NDDL,W_DDL(*),IDDL(*)  ,NDOF(*)  ,IKC(*),ICONV
      my_real
     .   DD(*) ,DDR(*),U(*),F(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NDDL0
      my_real
     .   UOLD(NDDL),RE,REP
C------------------------------------------
       IF (ICONV==0) THEN
        NDDL0 = N_BFGS
        CALL D_TO_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .             DD    ,DDR   ,UOLD  )
        CALL PRODUT_W(NDDL,UOLD,UOLD,W_DDL,RE)
        CALL PRODUT_W(NDDL,UOLD,U   ,W_DDL,REP)
        S_LIN = S_LIN + REP/MAX(EM20,RE)
        S_LIN = MAX(S_LIN,EM10)
        DO I=1,NDDL
         U(I) = U(I) + UOLD(I)
         F(I) = BFGS_V(I,1)
        ENDDO
       ELSE
        DO I=1,NDDL
         BFGS_V(I,1) =  F(I)
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_SLIN                      source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE GET_SLIN(FR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL0
      my_real
     .   FR
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       FR = S_LIN
C------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_H1                       source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        BFGS_RHDH                     source/implicit/imp_bfgs.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_H1(F_DDL,L_DDL,W_DDL,F,A2,IT,ITASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER F_DDL,L_DDL,W_DDL(*),IT,ITASK
      my_real
     .   F(*) ,A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
      my_real
     .   A1 ,B1
C------------------------------------------
       IF (IT==0.OR.(IACTB==0.AND.IT<2)) RETURN
       N = N_BFGS + 1
C--------V->dr----------------------------------
       A2=ZERO
       DO I=F_DDL,L_DDL
        BFGS_V(I,N) = F(I) - BFGS_V(I,N)
       ENDDO
       CALL PRODUT_H(F_DDL,L_DDL,BFGS_W(1,N),BFGS_V(1,N),W_DDL,A1,
     .               ITASK)
       IF (ITASK==0) THEN
        IF (ABS(A1)>EM10) N_BFGS = N
       END IF
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ABS(A1)>EM10) THEN
         CALL PRODUT_H(F_DDL,L_DDL,BFGS_W(1,N),F,W_DDL,A2,ITASK)
         IF (ITASK==0) A2 = A2*S_LIN
C--------W->b1*du----------------------------------
         B1 = ONE/A1
         DO I=F_DDL,L_DDL
          BFGS_W(I,N) = B1*BFGS_W(I,N)
         ENDDO
       ENDIF
C 
       DO I=N_BFGS,1,-1
        CALL BFGS_RHDH(F_DDL,L_DDL,W_DDL,BFGS_W(1,I),BFGS_V(1,I),F,
     .                 ITASK)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_RHDH                     source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        BFGS_H1                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H1P                      source/implicit/imp_bfgs.F    
Chd|        BFGS_H2                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H2P                      source/implicit/imp_bfgs.F    
Chd|-- calls ---------------
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE BFGS_RHDH(F_DDL,L_DDL,W_DDL,BW,BV,B,ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER F_DDL,L_DDL,ITASK,W_DDL(*)
      my_real
     .   BW(*),BV(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  A1
C------------------------------------------
      CALL PRODUT_H(F_DDL,L_DDL,BW,B,W_DDL,A1,ITASK)
      DO I=F_DDL,L_DDL
       B(I) = B(I) - A1*BV(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_H2                       source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        BFGS_RHDH                     source/implicit/imp_bfgs.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_H2(F_DDL,L_DDL,W_DDL,U,F,A2,IT,MAX_BFGS,ITASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER F_DDL,L_DDL,W_DDL(*),IT,MAX_BFGS,ITASK
      my_real
     .   F(*)  ,U(*),A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
C------------------------------------------
       IF (IACTB==0.AND.IT==0) RETURN
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK==0) THEN
        IACTB = 1
        IF (IT==0.AND.L_BFGS==0) N_BFGS = 0
       END IF !(ITASK==0) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
       IF(N_BFGS>0) THEN
        DO I=1,N_BFGS
         CALL BFGS_RHDH(F_DDL,L_DDL,W_DDL,BFGS_V(1,I),BFGS_W(1,I),U,
     .                  ITASK)
        ENDDO
        IF (A2/=ZERO) THEN
         DO I=F_DDL,L_DDL
          U(I) = U(I) - A2*BFGS_W(I,N_BFGS)
         ENDDO
        ENDIF
       ENDIF
C
       IF (L_BFGS>0) THEN
        IF (N_BFGS<L_BFGS) THEN
         N = N_BFGS + 1
         DO I=F_DDL,L_DDL
          BFGS_W(I,N) = U(I)
          BFGS_V(I,N) = F(I)
         ENDDO
        ELSEIF (N_BFGS==L_BFGS) THEN
         DO N=1,N_BFGS-1
          DO I=F_DDL,L_DDL
           BFGS_W(I,N) = BFGS_W(I,N+1)
           BFGS_V(I,N) = BFGS_V(I,N+1)
          ENDDO
         ENDDO
         DO I=F_DDL,L_DDL
          BFGS_W(I,N_BFGS) = U(I)
          BFGS_V(I,N_BFGS) = F(I)
         ENDDO
        ENDIF
       ELSE
C
        IF (N_BFGS<MAX_BFGS) THEN
         N = N_BFGS + 1
         DO I=F_DDL,L_DDL
          BFGS_W(I,N) = U(I)
          BFGS_V(I,N) = F(I)
          ENDDO
        ELSEIF (N_BFGS==MAX_BFGS) THEN
         DO N=1,N_BFGS-1
          DO I=F_DDL,L_DDL
           BFGS_W(I,N) = BFGS_W(I,N+1)
           BFGS_V(I,N) = BFGS_V(I,N+1)
          ENDDO
         ENDDO
         DO I=F_DDL,L_DDL
          BFGS_W(I,N_BFGS) = U(I)
          BFGS_V(I,N_BFGS) = F(I)
         ENDDO
        END IF !IF (N_BFGS<MAX_BFGS)
C
       END IF !IF (L_BFGS>0)
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK==0) THEN
        IF (L_BFGS>0) THEN
         IF (N_BFGS==L_BFGS) N_BFGS=L_BFGS-1
        ELSEIF (N_BFGS==MAX_BFGS) THEN
         N_BFGS=MAX_BFGS-1
        END IF
       END IF !(ITASK==0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_H1P                      source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        BFGS_RHDH                     source/implicit/imp_bfgs.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_H1P(F_DDL,L_DDL,W_DDL,F,A2,IT,ITASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER F_DDL,L_DDL,ITASK,W_DDL(*),IT
      my_real
     .   F(*) ,A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
      my_real
     .   A0,A1 ,B1
C------------------------------------------
       IF (IT==0.OR.(IACTB==0.AND.IT<2)) RETURN
       N = N_BFGS + 1
C--------V->dr----------------------------------
       DO I=F_DDL,L_DDL
        BFGS_V(I,N) = F(I) - BFGS_V(I,N)
       ENDDO
       CALL PRODUT_H(F_DDL,L_DDL,BFGS_W(1,N),BFGS_V(1,N),W_DDL,A0,
     .               ITASK)
       CALL PRODUT_H(F_DDL,L_DDL,BFGS_W(1,N),F,W_DDL,A2,ITASK)
C       
       A1 = S_LIN*A0
       IF (ITASK==0) THEN
        IF (ABS(A2)>EM10) THEN
         B1=-A1/A2
         IF (ABS(A1)>EM10.AND.B1>ZERO) N_BFGS = N
        END IF
       END IF
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ABS(A2)>EM10) THEN
        B1=-A1/A2
        IF (ABS(A1)>EM10.AND.B1>ZERO) THEN
C--------W->b1*du----------------------------------
         B1 = SQRT(B1)
         DO I=F_DDL,L_DDL
          BFGS_W(I,N) = BFGS_W(I,N)/A1
          BFGS_V(I,N) = BFGS_V(I,N)-B1*F(I)
         ENDDO
        ENDIF
       ENDIF
C----------------------
      CALL MY_BARRIER
C---------------------
       DO I=N_BFGS,1,-1
        CALL BFGS_RHDH(F_DDL,L_DDL,W_DDL,BFGS_W(1,I),BFGS_V(1,I),F,
     .                 ITASK)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BFGS_H2P                      source/implicit/imp_bfgs.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        BFGS_RHDH                     source/implicit/imp_bfgs.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE BFGS_H2P(F_DDL,L_DDL,W_DDL,U,F,A2,IT,MAX_BFGS,ITASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_BFGS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER F_DDL,L_DDL,ITASK,W_DDL(*),IT,MAX_BFGS
      my_real
     .   F(*)  ,U(*),A2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N
C------------------------------------------
       IF (IACTB==0.AND.IT==0) RETURN
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK==0) THEN
        IACTB = 1
        IF (IT==0.AND.L_BFGS==0) N_BFGS = 0
       END IF !(ITASK==0) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
       IF(N_BFGS>0) THEN
        DO I=1,N_BFGS
         CALL BFGS_RHDH(F_DDL,L_DDL,W_DDL,BFGS_V(1,I),BFGS_W(1,I),U,
     .                  ITASK)
        ENDDO
       ENDIF
C
       IF (L_BFGS>0) THEN
        IF (N_BFGS<L_BFGS) THEN
         N = N_BFGS + 1
         DO I=F_DDL,L_DDL
          BFGS_W(I,N) = U(I)
          BFGS_V(I,N) = F(I)
         ENDDO
        ELSEIF (N_BFGS==L_BFGS) THEN
         DO N=1,N_BFGS-1
          DO I=F_DDL,L_DDL
           BFGS_W(I,N) = BFGS_W(I,N+1)
           BFGS_V(I,N) = BFGS_V(I,N+1)
          ENDDO
         ENDDO
         DO I=F_DDL,L_DDL
          BFGS_W(I,N_BFGS) = U(I)
          BFGS_V(I,N_BFGS) = F(I)
         ENDDO
        ENDIF
       ELSE
        IF (N_BFGS<MAX_BFGS) THEN
         N = N_BFGS + 1
         DO I=F_DDL,L_DDL
          BFGS_W(I,N) = U(I)
          BFGS_V(I,N) = F(I)
         ENDDO
        ELSEIF (N_BFGS==MAX_BFGS) THEN
         DO N=1,N_BFGS-1
          DO I=F_DDL,L_DDL
           BFGS_W(I,N) = BFGS_W(I,N+1)
           BFGS_V(I,N) = BFGS_V(I,N+1)
          ENDDO
         ENDDO
         DO I=F_DDL,L_DDL
          BFGS_W(I,N_BFGS) = U(I)
          BFGS_V(I,N_BFGS) = F(I)
         ENDDO
        END IF !IF (N_BFGS<MAX_BFGS)
       END IF !IF (L_BFGS>0)
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK==0) THEN
        IF (L_BFGS>0) THEN
         IF (N_BFGS==L_BFGS) N_BFGS=L_BFGS-1
        ELSEIF (N_BFGS==MAX_BFGS) THEN
         N_BFGS=MAX_BFGS-1
        END IF
       END IF !(ITASK==0) THEN
C
      RETURN
      END

