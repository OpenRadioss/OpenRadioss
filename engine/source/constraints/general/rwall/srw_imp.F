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
Chd|  FV_RWL                        source/constraints/general/rwall/srw_imp.F
Chd|-- called by -----------
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- calls ---------------
Chd|        L_DIR0                        source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FV_RWL(IDDL   ,IKC   ,NDOF  ,UD    ,V    ,A    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_RWL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*),IKC(*),NDOF(*)
C     REAL
      my_real
     .  UD(3,*), V(3,*), A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,K1,ND,ID,NL,IERR1,IERR2
C     REAL
C-----------------------------------------------
      N_RWL = 0
      DO I = 1,NUMNOD
       IF (NDOF(I)>0) THEN
        ND = IDDL(I)+1
        IF (IKC(ND)==10) N_RWL=N_RWL+1
       ENDIF
      ENDDO
      IF (N_RWL==0) RETURN
C--------allocation------ 
      IF(ALLOCATED(IN_RWL)) DEALLOCATE(IN_RWL)
      ALLOCATE(IN_RWL(N_RWL),STAT=IERR1)
      IF(ALLOCATED(NOR_RWL)) DEALLOCATE(NOR_RWL)
      ALLOCATE(NOR_RWL(3,N_RWL),STAT=IERR2)
      NL = 0
      DO I = 1,NUMNOD
       IF (NDOF(I)>0) THEN
        ND = IDDL(I)
        IF (IKC(ND+1)==10) THEN
         NL=NL+1
         IN_RWL(NL)=I
         NOR_RWL(1,NL) = A(1,I)
         NOR_RWL(2,NL) = A(2,I)
         NOR_RWL(3,NL) = A(3,I)
         CALL L_DIR0(NOR_RWL(1,NL),J)
         UD(J,I) = V(1,I)*DT2/NOR_RWL(J,NL)
         IKC(ND+1)=0
         IKC(ND+J)=10
        ENDIF
       ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_RWL0                       source/constraints/general/rwall/srw_imp.F
Chd|-- called by -----------
Chd|        FV_RW0                        source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        FV_UPDK                       source/constraints/general/impvel/fv_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FV_RWL0(IDDL   ,IFIX  ,NDOF  ,IADK  ,JDIK ,
     1                   DIAG_K ,LT_K  ,UD    ,B     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_RWL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IDDL(*),IFIX(*),IADK(*),JDIK(*),NDOF(*)
C     REAL
      my_real
     .  UD(3,*), DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,IR
C     REAL
      my_real
     .   EJ(3)
C----------------sliding RW ->FXVEL local---------------
      IR =0
      DO I = 1,N_RWL
        N=IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J)
        CALL FV_UPDK(N     ,IDDL  ,EJ    ,J     ,IR    ,
     1               IADK  ,JDIK  ,DIAG_K,LT_K  ,B     ,UD     )
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RWL_IMPD                      source/constraints/general/rwall/srw_imp.F
Chd|-- called by -----------
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|-- calls ---------------
Chd|        BC_UPDD                       source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE RWL_IMPD(UD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_RWL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  UD(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J
C     REAL
      my_real
     .   EJ(3)
C---------------------------------------------------------------
      DO I = 1,N_RWL
        N=IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J)
        CALL BC_UPDD(N     ,EJ   ,J     ,UD     )
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_RWLR0                      source/constraints/general/rwall/srw_imp.F
Chd|-- called by -----------
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        BC_UPDB                       source/constraints/general/bcs/bc_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FV_RWLR0(IDDL   ,B     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_RWL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IDDL(*)
C     REAL
      my_real
     .  B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,IR,ID
C     REAL
      my_real
     .   EJ(3)
C----------------sliding RW ->FXVEL local- RHS seulement--------------
      IR =0
      DO I = 1,N_RWL
        N=IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J)
        ID = IDDL(N)
        CALL BC_UPDB(ID    ,EJ    ,J     ,IR    ,B     )
      ENDDO
C
      RETURN
      END
