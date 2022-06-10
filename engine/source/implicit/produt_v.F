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
C---------------------r={x}^t{y}---
Chd|====================================================================
Chd|  PRODUT_V                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_QRF                       source/implicit/imp_pc_inv.F  
Chd|        MAV_QT                        source/implicit/imp_pc_inv.F  
Chd|        NORMVECT                      com/eig/eigvect.F             
Chd|        NORMVECTP                     com/eig/eigvect.F             
Chd|-- calls ---------------
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE PRODUT_V( NDDL  ,X   ,Y  ,R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  
C     REAL
      my_real
     .  X(*), Y(*)  ,R 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      R = ZERO
      DO I=1,NDDL
       R = R + X(I)*Y(I)
      ENDDO
      IF (IMACH.EQ.3.AND.NSPMD.GT.1) CALL SPMD_SUM_S(R)
C--------------------------------------------
      RETURN
      END
C---------------------r={x}^t{y}---
Chd|====================================================================
Chd|  PRODUT_V_LOC                  source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        MAV_MN                        source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRODUT_V_LOC( NDDL  ,X   ,Y  ,R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  
C     REAL
      my_real
     .  X(*), Y(*)  ,R 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      R = ZERO
      DO I=1,NDDL
       R = R + X(I)*Y(I)
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  PRODUT_W                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        BFGS_1                        source/implicit/imp_bfgs.F    
Chd|        BFGS_1P                       source/implicit/imp_bfgs.F    
Chd|        BFGS_RHD                      source/implicit/imp_bfgs.F    
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|        NSLOAN_5                      source/implicit/imp_bfgs.F    
Chd|        PRODUT_U                      source/implicit/produt_v.F    
Chd|        PRODUT_U2                     source/implicit/produt_v.F    
Chd|        PRODUT_VM                     source/implicit/produt_v.F    
Chd|        RER02                         source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE PRODUT_W( NDDL  ,X   ,Y  ,W , R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,W(*) 
C     REAL
      my_real
     .  X(*), Y(*)  ,R 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      R = ZERO
      IF (IMACH.EQ.3.AND.NSPMD.GT.1) THEN
       DO I=1,NDDL
        IF (W(I).NE.0) R = R + X(I)*Y(I)
       ENDDO
       CALL SPMD_SUM_S(R)
      ELSE
       DO I=1,NDDL
        R = R + X(I)*Y(I)
       ENDDO
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  D_TO_U                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        NSLOAN_5                      source/implicit/imp_bfgs.F    
Chd|        PRODUT_VM                     source/implicit/produt_v.F    
Chd|        PRODUT_VMH                    source/implicit/produt_v.F    
Chd|        PRODUT_VMHP                   source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|====================================================================
      SUBROUTINE D_TO_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  D     ,DR    ,U     )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*)  
C     REAL
      my_real
     .  D(*),DR(*), U(*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  X(NDDL0)
C-----------------------------
       CALL IMP_SETB(D   ,DR   ,IDDL   ,NDOF  ,X    )
       CALL CONDENS_B(NDDL0  ,IKC  ,X  )
       CALL CP_REAL(NDDL,X,U)
C--------------------------------------------
      RETURN
      END
C---------------------r={x}^t{y}--x comes from u(3,*)-
Chd|====================================================================
Chd|  PRODUT_VM                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        D_TO_U                        source/implicit/produt_v.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE PRODUT_VM(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     DD    ,DDR   ,Y     ,R     ,W_IMP )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*) ,W_IMP(*) 
C     REAL
      my_real
     .  DD(*),DDR(*), Y(*)  ,R 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  X(NDDL)
C-----------------------------
       CALL D_TO_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .             DD    ,DDR   ,X     )
       CALL PRODUT_W(NDDL,X,Y,W_IMP,R)
C--------------------------------------------
      RETURN
      END
C---------------------norm2={x}^t{x}--x comes from u(3,*)-
Chd|====================================================================
Chd|  PRODUT_U                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        AL_CONSTRAINT1                source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINT2                source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE PRODUT_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    DD    ,DDR   ,NORM2 ,W_IMP )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_IMP(*)  
C     REAL
      my_real
     .  DD(*),DDR(*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  X(NDDL0)
C-----------------------------
       CALL IMP_SETB(DD  ,DDR   ,IDDL   ,NDOF  ,X    )
       CALL CONDENS_B(NDDL0  ,IKC  ,X  )
       CALL PRODUT_W(NDDL,X,X,W_IMP,NORM2)
C--------------------------------------------
      RETURN
      END
C---------------------norm2={x}^t{y}--x,y come from D1(3,*),D2--
Chd|====================================================================
Chd|  PRODUT_U2                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        AL_CONSTRAINT1                source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINT2                source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE PRODUT_U2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     D1    ,D1R   ,D2    ,D2R   ,NORM2 ,
     .                     W_IMP )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_IMP(*)  
C     REAL
      my_real
     .  D1(*),D1R(*), D2(*),D2R(*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  X(NDDL0),Y(NDDL0)
C-----------------------------
       CALL IMP_SETB(D1  ,D1R   ,IDDL   ,NDOF  ,X    )
       CALL IMP_SETB(D2  ,D2R   ,IDDL   ,NDOF  ,Y    )
       CALL CONDENS_B(NDDL0  ,IKC  ,X  )
       CALL CONDENS_B(NDDL0  ,IKC  ,Y  )
       CALL PRODUT_W(NDDL,X,Y,W_IMP,NORM2)
C--------------------------------------------
      RETURN
      END
C---------------------r={x}^t{y}--(only with free dof)-
Chd|====================================================================
Chd|  ZERO_UD                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ZERO_UD( NUM,IDDL,NDOF,IKC ,D ,DR ,IR)
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
      INTEGER  NUM,IDDL(*),IKC(*) ,NDOF(*) ,IR
C     REAL
      my_real
     .  D(3,*), DR(3,*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID
C-----------------------------
      DO I=1,NUM
       DO J=1,3
        ID = IDDL(I)+J
        IF (IKC(ID).EQ.2) D(J,I)=ZERO
       ENDDO
      ENDDO
      IF (IR.NE.0) THEN
       DO I=1,NUM
        IF (NDOF(I).GT.3) THEN
         DO J=1,3
          ID = IDDL(I)+J+3
          IF (IKC(ID).EQ.2) DR(J,I)=ZERO
         ENDDO
        ENDIF 
       ENDDO
      ENDIF 
C--------------------------------------------
      RETURN
      END
C-------------produit {w}=[K]{v} using only trangle-upper----
Chd|====================================================================
Chd|  MAV_LT                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        SMS_PCG1                      source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAV_LT(
     1                    NDDL  ,NNZ   ,IADL  ,JDIL  ,DIAG_K ,   
     2                    LT_K  ,V     ,W     )
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
      INTEGER  NDDL  ,NNZ   ,IADL(*)  ,JDIL(*)
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL
       W(I)=DIAG_K(I)*V(I)
      ENDDO
C      
      DO I=1,NDDL
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        L_K = LT_K(J)
        W(I) = W(I) + L_K*V(K)
        W(K) = W(K) + L_K*V(I)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
C----version //-------------
C      DO I=1,NDDL
C       W(I)=DIAG_K(I)*V(I)
C      ENDDO
C      DO I=1,NDDL-1
C       DO J =IADL(I),IADL(I+1)-1
C        K =JDIL(J)
C        W(K) = W(K) + LT_K(J)*V(I)
C       ENDDO
C      ENDDO
C
C      DO I=1,NDDL
C       DO J =IADL(I),IADL(I+1)-1
C        K =JDIL(J)
C        W(I) = W(I) + LT_K(J)*V(K)
C       ENDDO
C      ENDDO
C-------------produit {w}=[K]{v} non-sym----
Chd|====================================================================
Chd|  MAV_LT1                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        PREC_SOLV                     source/implicit/prec_solv.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAV_LT1(
     1                    NDDL  ,NNZ   ,IADL  ,JDIL  ,DIAG_K ,   
     2                    LT_K  ,V     ,W     )
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
      INTEGER  NDDL  ,NNZ   ,IADL(*)  ,JDIL(*)
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL
       W(I)=DIAG_K(I)*V(I)
      ENDDO
C      
      DO I=1,NDDL
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        L_K = LT_K(J)
        W(I) = W(I) + L_K*V(K)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
C-------------produit {w}=[LT_K]{v}+[LT_I]{v} using only trangle-upper----
Chd|====================================================================
Chd|  MAV_LT2                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INT_MATV                      source/implicit/imp_int_k.F   
Chd|        MATV_KIF                      source/implicit/imp_solv.F    
Chd|        MV_MATV                       source/airbag/monv_imp0.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MAV_LT2(
     1                    NDDL  ,NDDLI ,IADL  ,JDIL  ,DIAG_K,   
     2                    LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3                    V     ,W     ,MONVOL,VOLMON,X     ,
     4                    IGRSURF,NMONV ,IMONV,NDOF  ,
     5                    IPARI ,INTBUF_TAB    ,A    ,AR    ,
     6                    D     ,IBFV   ,SKEW  ,XFRAME,VE   ,
     7                    MS    ,NUM_IMP,NS_IMP,NE_IMP,INDEX2,
     8                    XI_C  ,IUPD   ,IRBE3 ,LRBE3 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NDDLI,IUPD,
     .         IADL(*),JDIL(*),IADI(*),JDII(*),ITOK(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),
     .        IPARI(*)  ,NDOF(*),IBFV(*),
     .         NUM_IMP(*),NS_IMP(*) ,NE_IMP(*),INDEX2(*),
     .         IRBE3(*) ,LRBE3(*)
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,LT_I(*)  ,V(*) 
      my_real
     .   X(3,*),A(3,*),AR(3,*), VOLMON(*)  ,D(3,*),
     .   SKEW(*)  ,XFRAME(*),VE(3,*),MS(*),XI_C(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,KK,IBID
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL
       W(I)=DIAG_K(I)*V(I)
      ENDDO
C
       DO I=1,NDDL
        DO J =IADL(I),IADL(I+1)-1
         K =JDIL(J)
         L_K = LT_K(J)
         W(I) = W(I) + L_K*V(K)
         W(K) = W(K) + L_K*V(I)
        ENDDO
       ENDDO
C  ------[K]{V}  
      IF (NDDLI.GT.0.AND.INTP_C.LT.0 ) THEN
        IF (ILINTF.GT.0) THEN
         CALL INT_MATV(IPARI ,INTBUF_TAB     ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,XI_C   ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,IUPD   ,
     4                IRBE3  ,LRBE3  ,IBID   ,IBID   )
        ELSE
         CALL INT_MATV(IPARI  ,INTBUF_TAB  ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,X      ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,IUPD   ,
     4                IRBE3  ,LRBE3  ,IBID   ,IBID   )
        ENDIF
      ELSE
C  ------LT_I  
       DO I=1,NDDLI
        II = ITOK(I)
        DO J =IADI(I),IADI(I+1)-1
         K =JDII(J)
         KK = ITOK(K)
         L_K = LT_I(J) 
         W(II) = W(II) + L_K*V(KK)
         W(KK) = W(KK) + L_K*V(II)
        ENDDO
       ENDDO
      ENDIF
       CALL MATV_KIF(V,W)
       IF (NMONV.GT.0) THEN
        CALL MV_MATV(MONVOL ,VOLMON  ,X      ,IGRSURF,
     1               IBID   ,NMONV   ,IMONV  ,V      ,W      ,
     2               NDOF   ,IPARI  ,INTBUF_TAB  ,A      ,
     3               AR     ,D      ,IBFV    ,SKEW   ,XFRAME ,
     4               IRBE3  ,LRBE3  ,IBID    ,IBID   )
       ENDIF 
C--------------------------------------------
      RETURN
      END
C------spmd----produit {w}=[LT_K]{v}+[LT_I]{v} using only trangle-upper----
Chd|====================================================================
Chd|  MAV_LTP                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|-- calls ---------------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|        MATV_KIF                      source/implicit/imp_solv.F    
Chd|        MV_MATV                       source/airbag/monv_imp0.F     
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MAV_LTP(
     1                    NDDL  ,NDDLI ,IADL  ,JDIL  ,DIAG_K,   
     2                    LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3                    V     ,W     ,A     ,AR    ,VE    ,
     5                    MS    ,X     ,D     ,DR    ,NDOF  ,
     6                    IPARI ,INTBUF_TAB ,NUM_IMP,NS_IMP,
     7                    NE_IMP,NSREM ,NSL   ,IBFV  ,SKEW  ,
     8                    XFRAME,MONVOL,VOLMON,IGRSURF,
     9                    FR_MV ,NMONV ,IMONV ,INDEX2 ,XI_C  ,
     A                    IUPD  ,IRBE3 ,LRBE3 ,IRBE2 ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include       "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NDDLI,NDOF(*),IUPD,
     .         IADL(*),JDIL(*),IADI(*),JDII(*),ITOK(*),
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,
     .         NE_IMP(*),NSREM ,NSL,IBFV(*),INDEX2(*),
     .         IRBE3(*)  ,LRBE3(*),IRBE2(*)  ,LRBE2(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,LT_I(*)  ,V(*) ,
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),X(3,*),
     .  MS(*),VOLMON(*),SKEW(*),XFRAME(*),XI_C(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,KK,IBID
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL
       W(I)=DIAG_K(I)*V(I)
      ENDDO
C      
       DO I=1,NDDL
#include      "vectorize.inc"
        DO J =IADL(I),IADL(I+1)-1
         K =JDIL(J)
         L_K = LT_K(J)
         W(I) = W(I) + L_K*V(K)
         W(K) = W(K) + L_K*V(I)
        ENDDO
       ENDDO
C  ------[K]{V}  
      IF ((NDDLI+NSREM+NSL).GT.0.AND.INTP_C.LT.0 ) THEN
        IF (ILINTF.GT.0) THEN
         CALL INT_MATVP(IPARI  ,INTBUF_TAB  ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,XI_C   ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,DR     ,
     4                NSREM  ,NSL    ,IUPD   ,IRBE3  ,LRBE3  ,
     5                IRBE2  ,LRBE2  )
        ELSE
         CALL INT_MATVP(IPARI  ,INTBUF_TAB  ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,X      ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,DR     ,
     4                NSREM  ,NSL    ,IUPD   ,IRBE3  ,LRBE3  ,
     5                IRBE2  ,LRBE2  )
        ENDIF
      ELSE
C  ------LT_I    
       DO I=1,NDDLI
        II = ITOK(I)
#include      "vectorize.inc"
        DO J =IADI(I),IADI(I+1)-1
         K =JDII(J)
         KK = ITOK(K)
         L_K = LT_I(J) 
         W(II) = W(II) + L_K*V(KK)
         W(KK) = W(KK) + L_K*V(II)
        ENDDO
       ENDDO
      ENDIF
      CALL MATV_KIF(V,W)
       IF (NMONV.GT.0) THEN
        CALL MV_MATV(MONVOL ,VOLMON  ,X      ,IGRSURF,
     1               FR_MV  ,NMONV  ,IMONV   ,V      ,W      ,
     2               NDOF   ,IPARI  ,INTBUF_TAB  ,A      ,
     3               AR     ,D      ,IBFV    ,SKEW   ,XFRAME ,
     4               IRBE3  ,LRBE3  ,IRBE2   ,LRBE2  )
       ENDIF
C    
      IF (IMACH.EQ.3.AND.NSPMD.GT.1) THEN
       IF ((NSREM+NSL).GT.0.AND.INTP_C.GE.0) 
     .   CALL FR_MATV(  A     ,VE    ,D      ,MS     ,X      ,
     1                  DR    ,AR    ,IPARI  ,INTBUF_TAB     ,
     2                  NDOF  ,NUM_IMP,NS_IMP,NE_IMP,V       ,
     3                  NSREM ,NSL    ,IBFV  ,SKEW  ,XFRAME  ,
     4                  W     ,IRBE3  ,LRBE3 ,IRBE2  ,LRBE2  )
       CALL SPMD_SUMF_V(W )
      ENDIF
C--------------------------------------------
      RETURN
      END
C-------------produit {w}=[K]{v} with {v} non zero indices----
Chd|====================================================================
Chd|  MAV_ZI                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAV_ZI(II,NDDL  ,NNZ   ,IADL  ,JDIL  ,DIAG_K ,   
     1                  LT_K  ,NNZZ ,IADM   ,JDIM  , LT_M  ,W )
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
      INTEGER  NDDL  ,NNZ   ,IADL(*)  ,JDIL(*),
     1          II,NNZZ ,IADM(*)  ,JDIM(*)   
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,LT_M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- LT_M(NNZM) IADM(NNZZ) position in LT_M,JDIM(NNZZ) line in LT_M
C-------implicitement v(nnzz+1)=1,jdim(nnzz+1)=ii----
      INTEGER I,J,K,IZ,IM,JJ
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL
       W(I)=ZERO
      ENDDO
      DO I = 1,II-1
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        IF (K.EQ.II) THEN
         W(I) = LT_K(J)
        ENDIF 
       ENDDO
      ENDDO
      W(II)=DIAG_K(II)
      DO J =IADL(II),IADL(II+1)-1
       K =JDIL(J)
       W(K) = LT_K(J)
      ENDDO
C-------Kij Vj (j>i)---
      DO IZ=1,NNZZ
       I =JDIM(IZ)
       IM=IADM(IZ)
       W(I)=W(I)+DIAG_K(I)*LT_M(IM)
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        L_K = LT_K(J)*LT_M(IM)
        W(K) = W(K) + L_K
       ENDDO
      ENDDO
C-------Kij Vj (j<i)---
      DO I = 1,II-1
       DO 100 IZ=1,NNZZ
        JJ =JDIM(IZ)
        DO J =IADL(I),IADL(I+1)-1
         K =JDIL(J)
         IF (K.GT.JJ) THEN
          GOTO 100
         ELSEIF (K.EQ.JJ) THEN
          IM=IADM(IZ)
          W(I)=W(I)+LT_K(J)*LT_M(IM)
         ENDIF
        ENDDO
 100   CONTINUE
      ENDDO
C
C--------------------------------------------
      RETURN
      END
C-------------produit {w}=[K]{v} with {v}=LT_M(*,II) en format C.C.S.----
Chd|====================================================================
Chd|  MAV_Z                         source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAV_Z(II,NDDL  ,NNZ   ,IADL  ,JDIL  ,DIAG_K ,   
     1                  LT_K   ,NNZM ,IADM   ,JDIM  , LT_M  ,W )
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
      INTEGER  NDDL  ,NNZ   ,IADL(*)  ,JDIL(*),
     1          II,NNZM ,IADM(*)  ,JDIM(*)   
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,LT_M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-------implicitement LT_M(II,II)=1----
      INTEGER I,J,K,IM,IZ
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL
       W(I)=ZERO
      ENDDO
      DO I = 1,II-1
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        IF (K.EQ.II) THEN
         W(I) = LT_K(J)
        ENDIF 
       ENDDO
      ENDDO
      W(II)=DIAG_K(II)
      DO J =IADL(II),IADL(II+1)-1
       K =JDIL(J)
       W(K) = LT_K(J)
      ENDDO
C-------Kij Vj (j>i)---
      DO IM=IADM(II),IADM(II+1)-1
       I =JDIM(IM)
       W(I)=W(I)+DIAG_K(I)*LT_M(IM)
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        L_K = LT_K(J)*LT_M(IM)
        W(K) = W(K) + L_K
       ENDDO
      ENDDO
C-------Kij Vj (j<i)---
      DO I = 1,II-1
       DO 100 IM=IADM(II),IADM(II+1)-1
        IZ =JDIM(IM)
        DO J =IADL(I),IADL(I+1)-1
         K =JDIL(J)
         IF (K.GT.IZ) THEN
          GOTO 100
         ELSEIF (K.EQ.IZ) THEN
          W(I)=W(I)+LT_K(J)*LT_M(IM)
         ENDIF
        ENDDO
 100   CONTINUE
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  BUF_DIM                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_CPRE                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BUF_DIM( L1,L2,L3,L4)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  L1,L2,L3 ,L4
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----longueur de ELBUF,BUFMAT--------
      L1=SELBUF
      L2=SBUFMAT
      L3=SFSAV
      L4=SVOLMON
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CP_REAL                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        CLCEIG                        source/constraints/general/bcs/bc_imp0.F
Chd|        CP_IFRONT                     source/implicit/produt_v.F    
Chd|        CP_IMPBUF                     source/implicit/produt_v.F    
Chd|        DIS_CP                        source/implicit/imp_solv.F    
Chd|        DYNA_CPK0                     source/implicit/imp_dyna.F    
Chd|        DYNA_CPR0                     source/implicit/imp_dyna.F    
Chd|        D_TO_U                        source/implicit/produt_v.F    
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        IMPRREST                      source/output/restart/rdresb.F
Chd|        IMP_CPRE                      source/implicit/imp_solv.F    
Chd|        IMP_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKS                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        IMP_RESTARCP                  source/implicit/imp_sol_init.F
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        IND_LT2LN                     source/implicit/imp_fsa_inv.F 
Chd|        L2G_KLOC                      source/implicit/ind_glob_k.F  
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|        SAVE_KIF                      source/implicit/imp_solv.F    
Chd|        TRA_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_ASPC                      source/constraints/general/bcs/bc_imp0.F
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CP_REAL( N  ,X   ,XC)
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
      INTEGER  N  
C     REAL
      my_real
     .  X(*), XC(*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      DO I=1,N
       XC(I) = X(I)
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CP_INT                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        CP_IADD                       source/mpi/implicit/imp_fri.F 
Chd|        CP_IFRONT                     source/implicit/produt_v.F    
Chd|        CP_INTTD                      source/implicit/imp_int_k.F   
Chd|        CP_SLNR                       source/mpi/implicit/imp_fri.F 
Chd|        DIM_KTOT                      source/implicit/ind_glob_k.F  
Chd|        DIM_SPA2                      source/implicit/ind_glob_k.F  
Chd|        DIM_SPAN                      source/implicit/ind_glob_k.F  
Chd|        DOUB_NRS                      source/mpi/implicit/imp_fri.F 
Chd|        DYNA_CPK0                     source/implicit/imp_dyna.F    
Chd|        IMP_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKS                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        IND_LT2LN                     source/implicit/imp_fsa_inv.F 
Chd|        IND_SPA2                      source/implicit/ind_glob_k.F  
Chd|        IND_SPAN                      source/implicit/ind_glob_k.F  
Chd|        L2G_KLOC                      source/implicit/ind_glob_k.F  
Chd|        RMDIM_IMP                     source/model/remesh/rm_imp0.F 
Chd|        SAVE_KIF                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CP_INT( N  ,X   ,XC)
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
      INTEGER  N ,X(*), XC(*) 
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      DO I=1,N
       XC(I) = X(I)
      ENDDO
C--------------------------------------------
      RETURN
      END
C---------------------r={x}^t{y}---
Chd|====================================================================
Chd|  PRODUT_V0                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        SMS_PCG1                      source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRODUT_V0( NDDL  ,X   ,Y  ,R)
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
      INTEGER  NDDL  
C     REAL
      my_real
     .  X(*), Y(*)  ,R 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      R = ZERO
      DO I=1,NDDL
       R = R + X(I)*Y(I)
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  BUF_DIM1                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BUF_DIM1( L1,LT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  L1,LT
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  L2,L3,L4,L5,L6
C----longueur de ELBUF,BUFMAT--------
      L1=SELBUF
      L2=SBUFMAT
      L3=SFSAV
      L4=SVOLMON
      L5=NPSAV*NPART
      LT = L2+L3+L4+L5
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CP_IMPBUF                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        COPY_ELBUF                    source/elements/elbuf/copy_elbuf.F
Chd|        COPY_INTBUF_TAB               ../common_source/interf/copy_intbuf_tab.F
Chd|        CP_IFRONT                     source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        IMP_I7CP                      share/modules/imp_intm.F      
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CP_IMPBUF(
     .           IFLAG   ,ELBUF   ,ELBUF_C ,BUFMAT   ,BUFMAT_C ,
     .           FSAV    ,VOLMON  ,PARTSAV ,INTBUF_TAB,
     .           INTBUF_TAB_C ,IPARI   ,ISLEN7   ,IRLEN7   ,
     .           ISLEN11 ,IRLEN11 ,ISLEN17 ,IRLEN17  ,IRLEN7T  ,
     .           ISLEN7T ,IRLEN20 ,ISLEN20 ,IRLEN20T ,ISLEN20T,
     .           IRLEN20E,ISLEN20E,NEWFRONT,ELBUF_TAB,ELBUF_IMP,
     .           IPARG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE IMP_INTBUF
      USE IMP_I7CP
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IFLAG, IPARI(NPARI,*),ISLEN7 ,IRLEN7   ,
     .  ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .  ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .  IRLEN20E,ISLEN20E,NEWFRONT(*),IPARG(NPARG,NGROUP)
C     REAL
      my_real
     .  ELBUF(*)   ,ELBUF_C(*) ,BUFMAT(*)  ,BUFMAT_C(*)  ,
     .  FSAV(*)  ,VOLMON(*)  ,PARTSAV(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB, ELBUF_IMP 

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*), INTBUF_TAB_C(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  LI1,LI2,LI3,LI4,LI5,LI6,LL,N,IAD,JD(50),JFI,
     .         ITY,IGSTI,NREBOU
C--------------Iflag= 1->copy; 2 ->restore---------------
      LI1=SELBUF
      LI2=SBUFMAT
      LI3=SFSAV
      LI4=SVOLMON
      LI5=NPSAV*NPART
c      LI6=SBUFIN
C------------BUF->BUF_C--------------------------------
      IF (IFLAG.EQ.1) THEN
            CALL COPY_ELBUF(ELBUF_TAB,ELBUF_IMP,IPARG,NGROUP)
            CALL CP_REAL(LI1,ELBUF,ELBUF_C)
            CALL CP_REAL(LI2,BUFMAT,BUFMAT_C)
            LL=LI2+1
              CALL CP_REAL(LI3,FSAV,BUFMAT_C(LL))
            LL=LL+LI3
              CALL CP_REAL(LI4,VOLMON,BUFMAT_C(LL))
            LL=LL+LI4
            CALL CP_REAL(LI5,PARTSAV,BUFMAT_C(LL))
           IF (NINTER.NE.0.AND.ILINE.NE.1) THEN
C------for int24 + Istif=6
            DO N = 1,NINTER
             ITY   =IPARI(7,N)
             IF (ITY==0) CYCLE
             IGSTI =IPARI(34,N)
             IF (ITY == 24.AND.IGSTI==6) THEN
              IF (IPARI(53,N)<0) IPARI(53,N)= IABS(IPARI(53,N))
             END IF
             !integral copy of interface buffer structure
             !INTBUF_TAB -> INTBUF_TAB_C
             CALL COPY_INTBUF_TAB(INTBUF_TAB(N), INTBUF_TAB_C(N) )
            END DO !N = 1,NINTER

            CALL CP_IFRONT(IFLAG ,IPARI ,ISLEN7 ,IRLEN7   ,
     .                     ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                     ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .                     IRLEN20E,ISLEN20E,NEWFRONT)
          END IF 
C------------BUF_C->BUF--------------------------------
      ELSEIF (IFLAG.EQ.2) THEN
            CALL COPY_ELBUF(ELBUF_IMP,ELBUF_TAB,IPARG,NGROUP)
            CALL CP_REAL(LI1,ELBUF_C,ELBUF)
            CALL CP_REAL(LI2,BUFMAT_C,BUFMAT)
            LL=LI2+1
            CALL CP_REAL(LI3,BUFMAT_C(LL),FSAV)
            LL=LL+LI3
            CALL CP_REAL(LI4,BUFMAT_C(LL),VOLMON)
            LL=LL+LI4
            CALL CP_REAL(LI5,BUFMAT_C(LL),PARTSAV)
          IF (NINTER.NE.0.AND.ILINE.NE.1) THEN
C------for int24 + Istif=6 
            DO N = 1,NINTER
             ITY   =IPARI(7,N)
             IF (ITY==0) CYCLE
             IGSTI =IPARI(34,N)
             NREBOU=IPARI(53,N)
             IF (ITY==24.AND.IGSTI==6.AND.NREBOU<0) THEN
C---------divergence supprime the treatement , line-serach doesn't change stif            
              IF (IMCONV<-1) THEN
               IPARI(53,N) = -NREBOU
              ELSEIF (IMCONV>=0) THEN
               LL   =2*IPARI(5,N)
               CALL CP_REAL(LL,INTBUF_TAB(N)%STIF_OLD,INTBUF_TAB_CP(N)%STIF_OLD)
              END IF
             END IF
             !integral copy of interface buffer structure
             !INTBUF_TAB_C -> INTBUF_TAB
             CALL COPY_INTBUF_TAB(INTBUF_TAB_C(N), INTBUF_TAB(N) )
            END DO !N = 1,NINTER

C------for int24 + Istif=6 remote part is inside CP_IFRONT
            CALL CP_IFRONT(IFLAG ,IPARI ,ISLEN7 ,IRLEN7   ,
     .                     ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                     ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .                     IRLEN20E,ISLEN20E,NEWFRONT)
c           CALL CP_INTBUF(BUFIN_C,INBUF_C ,BUFIN,INBUF ,IPARI )
          END IF 
      ENDIF
C--------------------------------------------
      RETURN
      END
C-------------produit {w}=[K]{v} using only trangle-upper----
Chd|====================================================================
Chd|  MAV_LT_H                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        MAV_LTH0                      source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE MAV_LT_H(NDDL   ,
     1                    F_DDL  ,L_DDL   ,IADL  ,JDIL   ,DIAG_K ,   
     2                    LT_K   ,V       ,W     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  F_DDL  ,L_DDL   ,IADL(*)  ,JDIL(*),NDDL
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N
      my_real
     .   L_K,W_TMP(NDDL)
C----------------------------
      DO I=1,NDDL
       W_TMP(I)=ZERO
      ENDDO
      DO I=F_DDL,L_DDL
       W(I)=DIAG_K(I)*V(I)
      ENDDO
C      
      DO I=F_DDL,L_DDL
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        L_K = LT_K(J)
        W(I) = W(I) + L_K*V(K)
        W_TMP(K) = W_TMP(K) + L_K*V(I)
       ENDDO
      ENDDO
C----------------------
      CALL MY_BARRIER
C---------------------
#include "lockon.inc"
      DO I=1,NDDL
        W(I) = W(I) + W_TMP(I)
      ENDDO
#include "lockoff.inc"
C--------------------------------------------
      RETURN
      END
C------Hybrid----produit {w}=[LT_K]{v}+[LT_I]{v} using only trangle-upper----
Chd|====================================================================
Chd|  MAV_LTH0                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|        MATV_KIF                      source/implicit/imp_solv.F    
Chd|        MAV_LT_H                      source/implicit/produt_v.F    
Chd|        MV_MATV                       source/airbag/monv_imp0.F     
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MAV_LTH0(
     1                    NDDL  ,NDDLI ,IADL  ,JDIL  ,DIAG_K,   
     2                    LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3                    V     ,W     ,A     ,AR    ,VE    ,
     5                    MS    ,X     ,D     ,DR    ,NDOF  ,
     6                    IPARI ,INTBUF_TAB   ,NUM_IMP,NS_IMP,
     7                    NE_IMP,NSREM ,NSL   ,IBFV  ,SKEW  ,
     8                    XFRAME,MONVOL,VOLMON,IGRSURF ,
     9                    FR_MV ,NMONV ,IMONV ,INDEX2 ,XI_C  ,
     A                    IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 ,
     B                    F_DDL ,L_DDL ,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include       "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NDDLI,NDOF(*),IUPD,
     .         IADL(*),JDIL(*),IADI(*),JDII(*),ITOK(*),
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,
     .         NE_IMP(*),NSREM ,NSL,IBFV(*),INDEX2(*),
     .         IRBE3(*),LRBE3(*),F_DDL  ,L_DDL   ,ITASK,
     .         IRBE2(*),LRBE2(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,LT_I(*)  ,V(*) ,
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),X(3,*),
     .  MS(*),VOLMON(*),SKEW(*),XFRAME(*),XI_C(*)     

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,KK,F_DDLI,L_DDLI
      my_real
     .   L_K,WORK_II(NDDLI)
C-----------------------------
            CALL MAV_LT_H(NDDL   ,
     1                    F_DDL  ,L_DDL   ,IADL  ,JDIL   ,DIAG_K ,   
     2                    LT_K   ,V       ,W     )
C----------------------
      CALL MY_BARRIER
C---------------------
C  ------[K]{V}  
      IF ((NDDLI+NSREM+NSL).GT.0.AND.INTP_C.LT.0 ) THEN
C--------spmd only for the moment-------
       IF (ITASK.EQ.0) THEN
        IF (ILINTF.GT.0) THEN
         CALL INT_MATVP(IPARI  ,INTBUF_TAB  ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,XI_C   ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,DR     ,
     4                NSREM  ,NSL    ,IUPD   ,IRBE3  ,LRBE3  ,
     5                IRBE2  ,LRBE2  )
        ELSE
         CALL INT_MATVP(IPARI  ,INTBUF_TAB  ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,X      ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,DR     ,
     4                NSREM  ,NSL    ,IUPD   ,IRBE3  ,LRBE3  ,
     5                IRBE2  ,LRBE2  )
        ENDIF
       END IF !(ITASK.EQ.0) THEN
      ELSEIF(NDDLI.GT.0) THEN
C  ------LT_I    //
       F_DDLI=1+ITASK*NDDLI/NTHREAD
       L_DDLI=(ITASK+1)*NDDLI/NTHREAD
C
       DO I=1,NDDLI
         WORK_II(I) = ZERO
       ENDDO
C
       DO I=F_DDLI,L_DDLI
        II = ITOK(I)
        DO J =IADI(I),IADI(I+1)-1
         K =JDII(J)
         KK = ITOK(K)
         L_K = LT_I(J) 
         WORK_II(I) = WORK_II(I) + L_K*V(KK)
         WORK_II(K) = WORK_II(K) + L_K*V(II)
        ENDDO
       ENDDO
C
#include "lockon.inc"
       DO I=1,NDDLI
        II = ITOK(I)
        W(II) = W(II) + WORK_II(I)
       ENDDO
#include "lockoff.inc"
C
      END IF !((NDDLI+NSREM+NSL).GT.0.AND.INTP_C.LT.0 ) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
C
      IF (ITASK.EQ.0) THEN
      CALL MATV_KIF(V,W)
       IF (NMONV.GT.0) THEN
        CALL MV_MATV(MONVOL ,VOLMON  ,X      ,IGRSURF,
     1               FR_MV  ,NMONV  ,IMONV   ,V      ,W      ,
     2               NDOF   ,IPARI  ,INTBUF_TAB      ,A      ,
     3               AR     ,D      ,IBFV    ,SKEW   ,XFRAME ,
     4               IRBE3  ,LRBE3  ,IRBE2  ,LRBE2  )
       ENDIF 
C    
      IF (NSPMD.GT.1) THEN
       IF ((NSREM+NSL).GT.0.AND.INTP_C.GE.0) 
     .   CALL FR_MATV(  A     ,VE    ,D      ,MS     ,X      ,
     1                  DR    ,AR    ,IPARI  ,INTBUF_TAB     ,
     2                  NDOF  ,NUM_IMP,NS_IMP,NE_IMP,V       ,
     3                  NSREM ,NSL    ,IBFV  ,SKEW  ,XFRAME  ,
     4                  W     ,IRBE3  ,LRBE3 ,IRBE2  ,LRBE2  )
       CALL SPMD_SUMF_V(W )
      ENDIF
      END IF !(ITASK.EQ.0) THEN
C--------------------------------------------
      RETURN
      END
C------Hybrid----produit {w}=[LT_K]{v}+[LT_I]{v} using only trangle-upper----
Chd|====================================================================
Chd|  MAV_LTH                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|        MATV_KIF                      source/implicit/imp_solv.F    
Chd|        MAV_LIUH                      source/implicit/produt_v.F    
Chd|        MAV_LU_H                      source/implicit/produt_v.F    
Chd|        MV_MATV                       source/airbag/monv_imp0.F     
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MAV_LTH(
     1                    NDDL  ,NDDLI ,IADL  ,JDIL  ,DIAG_K,   
     2                    LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3                    V     ,W     ,A     ,AR    ,VE    ,
     5                    MS    ,X     ,D     ,DR    ,NDOF  ,
     6                    IPARI ,INTBUF_TAB   ,NUM_IMP,NS_IMP,
     7                    NE_IMP,NSREM ,NSL   ,IBFV  ,SKEW  ,
     8                    XFRAME,MONVOL,VOLMON,IGRSURF,
     9                    FR_MV ,NMONV ,IMONV ,INDEX2 ,XI_C  ,
     A                    IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 ,
     B                    F_DDL ,L_DDL ,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NDDLI,NDOF(*),IUPD,
     .         IADL(*),JDIL(*),IADI(*),JDII(*),ITOK(*),
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,
     .         NE_IMP(*),NSREM ,NSL,IBFV(*),INDEX2(*),
     .         IRBE3(*),LRBE3(*),F_DDL  ,L_DDL   ,ITASK,
     .         IRBE2(*),LRBE2(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,LT_I(*)  ,V(*) ,
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),X(3,*),
     .  MS(*),VOLMON(*),SKEW(*),XFRAME(*),XI_C(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,KK,F_DDLI,L_DDLI
      my_real
     .   L_K,WORK_II(NDDLI)
C-----------PCG_GP-----------
          CALL MAV_LU_H(NDDL   ,
     1                    F_DDL  ,L_DDL   ,IADL  ,JDIL   ,DIAG_K ,   
     2                    LT_K   ,V       ,W     )
C----------------------
      CALL MY_BARRIER
C---------------------
      IF ((NDDLI+NSREM+NSL).GT.0.AND.INTP_C.LT.0 ) THEN
C--------spmd only for the moment-------
       IF (ITASK.EQ.0) THEN
        IF (ILINTF.GT.0) THEN
         CALL INT_MATVP(IPARI,INTBUF_TAB     ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,XI_C   ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,DR     ,
     4                NSREM  ,NSL    ,IUPD   ,IRBE3  ,LRBE3  ,
     5                IRBE2  ,LRBE2  )
        ELSE
         CALL INT_MATVP(IPARI,INTBUF_TAB     ,NDOF   ,NUM_IMP,
     1                NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                VE     ,X      ,MS     ,D      ,IBFV   ,
     3                SKEW   ,XFRAME ,V      ,W      ,DR     ,
     4                NSREM  ,NSL    ,IUPD   ,IRBE3  ,LRBE3  ,
     5                IRBE2  ,LRBE2  )
        ENDIF
       END IF !(ITASK.EQ.0) THEN
      ELSEIF(NDDLI.GT.0) THEN
       F_DDLI=1+ITASK*NDDLI/NTHREAD
       L_DDLI=(ITASK+1)*NDDLI/NTHREAD
C
       IF (ISOLV > 7) THEN
         CALL MAV_LIUH(F_DDLI ,L_DDLI  ,IADI  ,JDII  ,ITOK  ,
     1                 LT_I   ,WORK_II ,V     ,W     ,ITASK )
C         CALL MAV_LUI_H(F_DDL ,L_DDL  ,V   ,W     )
       ELSE
C  ------LT_I    //
       DO I=1,NDDLI
         WORK_II(I) = ZERO
       ENDDO
C
       DO I=F_DDLI,L_DDLI
        II = ITOK(I)
        DO J =IADI(I),IADI(I+1)-1
         K =JDII(J)
         KK = ITOK(K)
         L_K = LT_I(J) 
         WORK_II(I) = WORK_II(I) + L_K*V(KK)
         WORK_II(K) = WORK_II(K) + L_K*V(II)
        ENDDO
       ENDDO
C
#include "lockon.inc"
       DO I=1,NDDLI
        II = ITOK(I)
        W(II) = W(II) + WORK_II(I)
       ENDDO
#include "lockoff.inc"
       END IF !(ISOLV > 7) THEN
C
      END IF !((NDDLI+NSREM+NSL).GT.0.AND.INTP_C.LT.0 ) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
C
      IF (ITASK.EQ.0) THEN
       CALL MATV_KIF(V,W)
       IF (NMONV.GT.0) THEN
        CALL MV_MATV(MONVOL ,VOLMON  ,X      ,IGRSURF,
     1               FR_MV  ,NMONV  ,IMONV   ,V      ,W      ,
     2               NDOF   ,IPARI  ,INTBUF_TAB,A      ,
     3               AR     ,D      ,IBFV    ,SKEW   ,XFRAME ,
     4               IRBE3  ,LRBE3  ,IRBE2  ,LRBE2  )
       ENDIF 
C    
       IF (NSPMD.GT.1) THEN
        IF ((NSREM+NSL).GT.0.AND.INTP_C.GE.0) 
     .   CALL FR_MATV(  A     ,VE    ,D      ,MS     ,X      ,
     1                  DR    ,AR    ,IPARI ,INTBUF_TAB      ,
     2                  NDOF  ,NUM_IMP,NS_IMP,NE_IMP,V       ,
     3                  NSREM ,NSL    ,IBFV  ,SKEW  ,XFRAME  ,
     4                  W     ,IRBE3  ,LRBE3 ,IRBE2  ,LRBE2  )
        IF(IMONM > 0) CALL STARTIME(66,1)
        CALL SPMD_SUMF_V(W )
        IF(IMONM > 0) CALL STOPTIME(66,1)
       ENDIF
      END IF !(ITASK.EQ.0) THEN
C--------------------------------------------
      RETURN
      END
C-----------Hybrid {x}t{y}-.{Weight}--
Chd|====================================================================
Chd|  PRODUT_H                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        BFGS_H1                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H1P                      source/implicit/imp_bfgs.F    
Chd|        BFGS_RHDH                     source/implicit/imp_bfgs.F    
Chd|        IMP_INISI                     source/implicit/imp_pcg.F     
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        IMP_UPDV2                     source/implicit/imp_pcg.F     
Chd|        MAM_NM                        source/implicit/produt_v.F    
Chd|        MAV_NM                        source/implicit/produt_v.F    
Chd|        MORTHO_GS                     source/implicit/produt_v.F    
Chd|        PRODUT_UH                     source/implicit/produt_v.F    
Chd|        PRODUT_UH2                    source/implicit/produt_v.F    
Chd|        PRODUT_VMH                    source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRODUT_H(F_DDL  ,L_DDL ,X   ,Y  ,W , R ,ITASK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "impl2_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  F_DDL  ,L_DDL ,W(*) ,ITASK
C     REAL
      my_real
     .  X(*), Y(*)  ,R
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,N,J,NE
      my_real
     .  RTMP(MVSIZ),RL
C-----------------------------
      IF (ITASK.EQ.0) R_N2 = ZERO
C----------------------
      CALL MY_BARRIER
C---------------------
      IF (NSPMD == 1) THEN
       RL = ZERO
       DO N=F_DDL,L_DDL,NVSIZ
        NE =MIN(L_DDL-N+1,NVSIZ)
C
        DO I=1,NE
         J=N+I-1
         RTMP(I) = X(J)*Y(J)
        ENDDO
        DO I=1,NE
         RL = RL + RTMP(I)
        ENDDO
       END DO 
#include "lockon.inc"
        R_N2 = R_N2 + RL
#include "lockoff.inc"
C------------
      ELSE 
C------------NSPMD>1--------
        RL = ZERO
       DO N=F_DDL,L_DDL,MVSIZ
        NE =MIN(L_DDL-N+1,MVSIZ)
C
        DO I=1,NE
         J=N+I-1
         RTMP(I) = X(J)*Y(J)*W(J)
        ENDDO
        DO I=1,NE
         RL = RL + RTMP(I)
        ENDDO
       END DO 
#include "lockon.inc"
         R_N2 = R_N2 + RL
#include "lockoff.inc"
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK.EQ.0) THEN
         IF(IMONM > 0) CALL STARTIME(67,1)
         CALL SPMD_SUM_S(R_N2)
         IF(IMONM > 0) CALL STARTIME(67,1)
       END IF
C
      END IF !(NSPMD == 1) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
      R = R_N2
C----------------------
      CALL MY_BARRIER
C---------------------
      RETURN
      END
C---------------------r={x}^t{y}--x comes from u(3,*)-
Chd|====================================================================
Chd|  PRODUT_VMH                    source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        D_TO_U                        source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PRODUT_VMH(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                      DD    ,DDR   ,Y     ,R     ,W_IMP ,
     .                      F_DDL ,L_DDL ,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*) ,W_IMP(*) ,
     .         F_DDL ,L_DDL ,ITASK 
C     REAL
      my_real
     .  DD(*),DDR(*), Y(*)  ,R 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
       IF (ITASK == 0 ) THEN
        ALLOCATE(TMP_W1(NDDL))
        CALL D_TO_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .              DD    ,DDR   ,TMP_W1 )
       END IF
C----------------------
      CALL MY_BARRIER
C---------------------
       CALL PRODUT_H(F_DDL,L_DDL,TMP_W1,Y,W_IMP,R,ITASK)
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK == 0 ) DEALLOCATE(TMP_W1)
C--------------------------------------------
      RETURN
      END
C-------------norm2={x}^t{x}--x comes from u(3,*)- Hybrid---
Chd|====================================================================
Chd|  PRODUT_UH                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        AL_CONSTRAINTH1               source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINTH2               source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PRODUT_UH(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     DD    ,DDR   ,NORM2 ,W_IMP ,F_DDL ,
     .                     L_DDL ,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_IMP(*) ,
     .         F_DDL ,L_DDL ,ITASK  
C     REAL
      my_real
     .  DD(*),DDR(*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
       IF (ITASK == 0 ) THEN
        ALLOCATE(TMP_W1(NDDL0))
        CALL IMP_SETB(DD  ,DDR   ,IDDL   ,NDOF  ,TMP_W1)
        CALL CONDENS_B(NDDL0  ,IKC  ,TMP_W1)
       END IF
C----------------------
      CALL MY_BARRIER
C---------------------
      CALL PRODUT_H(F_DDL,L_DDL,TMP_W1,TMP_W1,W_IMP,NORM2,ITASK)
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK == 0 ) DEALLOCATE(TMP_W1)
C--------------------------------------------
      RETURN
      END
C---------------------norm2={x}^t{y}--x,y come from D1(3,*),D2-- Hybrid
Chd|====================================================================
Chd|  PRODUT_UH2                    source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        AL_CONSTRAINTH1               source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINTH2               source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PRODUT_UH2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     D1    ,D1R   ,D2    ,D2R   ,NORM2 ,
     .                     W_IMP ,F_DDL ,L_DDL ,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_IMP(*),  
     .         F_DDL ,L_DDL ,ITASK  
C     REAL
      my_real
     .  D1(*),D1R(*), D2(*),D2R(*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
       IF (ITASK == 0 ) THEN
        ALLOCATE(TMP_W1(NDDL0),TMP_W2(NDDL0))
        CALL IMP_SETB(D1  ,D1R   ,IDDL   ,NDOF  ,TMP_W1)
        CALL IMP_SETB(D2  ,D2R   ,IDDL   ,NDOF  ,TMP_W2)
        CALL CONDENS_B(NDDL0  ,IKC  ,TMP_W1)
        CALL CONDENS_B(NDDL0  ,IKC  ,TMP_W2)
       END IF
C----------------------
      CALL MY_BARRIER
C----------------------
      CALL PRODUT_H(F_DDL,L_DDL,TMP_W1,TMP_W2,W_IMP,NORM2,ITASK)
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK == 0 ) DEALLOCATE(TMP_W1,TMP_W2)
C--------------------------------------------
      RETURN
      END
C-------------produit {w}=[K]{v} using [K] complete----
Chd|====================================================================
Chd|  MAV_LU_H                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        MAV_LTGH                      source/implicit/produt_v.F    
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE MAV_LU_H(NDDL   ,
     1                    F_DDL  ,L_DDL   ,IADL  ,JDIL   ,DIAG_K ,   
     2                    LT_K   ,V       ,W     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
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
      INTEGER  F_DDL  ,L_DDL   ,IADL(*)  ,JDIL(*),NDDL
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N
      my_real
     .   L_K
C----------------------------
      DO I=F_DDL,L_DDL
       W(I)=DIAG_K(I)*V(I)
      ENDDO
C      
      DO I=F_DDL,L_DDL
       DO J =IADL(I),IADL(I+1)-1
        K =JDIL(J)
        L_K = LT_K(J)
        W(I) = W(I) + L_K*V(K)
       ENDDO
      ENDDO
C      
      DO I=F_DDL,L_DDL
       DO J =IADK0(I),IADK0(I+1)-1
        K =JDIK0(J)
        L_K = LT_K0(J)
        W(I) = W(I) + L_K*V(K)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CP_IFRONT                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        CP_IMPBUF                     source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        IMP_I7CP                      share/modules/imp_intm.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE CP_IFRONT(IFLAG ,IPARI ,ISLEN7 ,IRLEN7   ,
     .                     ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                     ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .                     IRLEN20E,ISLEN20E,NEWFRONT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE IMP_I7CP
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "impl1_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IFLAG ,IPARI(NPARI,*),ISLEN7 ,IRLEN7   ,
     .                     ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                     ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .                     IRLEN20E,ISLEN20E,NEWFRONT(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N, LENS,LENR,INACTI,NSN,NMN,IERR,IID,RID,I,P,
     .         IGAP,ITYP,LENS0,LENR0,INTTH,J,JFI,JD(50),ITY,IGSTI,
     .         NREBOU
C--------------------------------------------
      IF(IFLAG.EQ.1) THEN
         IF(ALLOCATED(IPARICP)) DEALLOCATE(IPARICP)
         IF(ALLOCATED(IAD_STIFOLD)) DEALLOCATE(IAD_STIFOLD)
         ALLOCATE(IPARICP(NPARI,NINTER),IAD_STIFOLD(NINTER),STAT=IERR)
          DO I =1, NINTER
           ITY   =IPARI(7,I)
           IGSTI =IPARI(34,I)
	       IAD_STIFOLD(I)=1
C           IF (ITY == 24.AND.IGSTI==6) THEN
C	        IAD_STIFOLD(I)=JD(32)
C           END IF
           DO J =1, NPARI
            IPARICP(J,I) = IPARI(J,I)
           END DO 
          END DO 
      ELSE	
          DO I =1, NINTER
           ITY   =IPARI(7,I)
           IGSTI =IPARI(34,I)
           IF (ITY == 24.AND.IGSTI==6) THEN
            IPARICP(26,I) = IPARI(26,I)
            IPARICP(27,I) = IPARI(27,I)
            IPARICP(53,I) = IPARI(53,I)
           END IF
          DO J =1, NPARI
            IPARI(J,I) = IPARICP(J,I)  
          END DO 
          END DO 
      ENDIF !IF(IFLAG.EQ.1) THEN
C      
      IF (NSPMD.LE.1) RETURN
C      
      IF(IFLAG.EQ.1) THEN
       LII7CP=0
       LRI7CP=0
        IF(NINTER.NE.0) THEN
C dim compute---
          DO I =1, NINTER
            ITYP = IPARI(7,I)
            IGAP = IPARI(21,I)
            INACTI = IPARI(22,I)
            INTTH = IPARI(47,I)
            IF(ITYP.EQ.7.OR.ITYP.EQ.10.OR.ITYP.EQ.11.OR.ITYP.EQ.24)THEN
              LENS = 0
              LENR = 0
              DO P = 1, NSPMD
                LENS = LENS + NSNSI(I)%P(P)
                LENR = LENR + NSNFI(I)%P(P)
              END DO
              LII7CP=LII7CP+2*NSPMD+2
              LII7CP=LII7CP+LENS
C
              IF (LENR.GT.0) THEN
                LII7CP=LII7CP+LENR
                IF(ITYP.EQ.7.OR.ITYP.EQ.10.OR.ITYP.EQ.24) THEN
C-------------------ITAFI,KINFI
                  LII7CP=LII7CP+2*LENR
                  IF (INTTH > 0 ) LII7CP=LII7CP+LENR
C-------------------MSFI,STIFI
                  LRI7CP=LRI7CP+2*LENR
                  IF(IGAP.NE.0) LRI7CP=LRI7CP+LENR
C-------------------XFI,VFI
                  LRI7CP=LRI7CP+6*LENR
                 IF(IPARIT.EQ.0) THEN
C-------------------AFI,STNFI,VSCFI
                    LRI7CP=LRI7CP+4*LENR*NTHREAD
                  IF(KDTINT.NE.0)LRI7CP=LRI7CP+LENR*NTHREAD
                  IF(INTTH > 0 )LRI7CP=LRI7CP+2*LENR+2*LENR*NTHREAD
                 ELSE
C-----------------PARITON not yet with implicit----	       
	             ENDIF
C-----  IRTLM_FI,TIME_SFI,SECND_FRFI,PENE_OLDFI,STIF_OLDFI
                 IF(ITYP==24) THEN
                  LII7CP=LII7CP+2*LENR
                  LRI7CP=LRI7CP+LENR
                  LRI7CP=LRI7CP+6*LENR
                  LRI7CP=LRI7CP+5*LENR
                  LRI7CP=LRI7CP+2*LENR
                 END IF
                ELSEIF(ITYP.EQ.11) THEN
C-------------------ITAFI
                  LII7CP=LII7CP+2*LENR
C-------------------MASFI,STIFI
                  LRI7CP=LRI7CP+3*LENR
                  IF(IGAP.NE.0) LRI7CP=LRI7CP+LENR
                  IF (INTTH > 0 ) LII7CP=LII7CP+LENR
C-------------------XFI,VFI
                  LRI7CP=LRI7CP+12*LENR
                  IF(INACTI.EQ.5.OR.INACTI.EQ.6)LRI7CP=LRI7CP+2*LENR
                 IF(IPARIT.EQ.0) THEN
C-------------------AFI,STNFI,VSCFI,AREASFI,TEMPFI
                  LRI7CP=LRI7CP+8*LENR*NTHREAD
                  IF(KDTINT.NE.0)LRI7CP=LRI7CP+2*LENR*NTHREAD
                  IF(INTTH > 0 )LRI7CP=LRI7CP+3*LENR   
                 ELSE
C-----------------PARITON not yet with implicit----	       
	         ENDIF
                ELSEIF(ITYP.EQ.17)THEN
                END IF
              END IF
            ENDIF
         ENDDO
        END IF
        IF((LII7CP+LRI7CP).EQ.0) RETURN
        IF(NINTER.NE.0) THEN
         IF(ALLOCATED(II7CP)) DEALLOCATE(II7CP)
         IF(ALLOCATED(RI7CP)) DEALLOCATE(RI7CP)
         IF(ALLOCATED(NEWFRCP)) DEALLOCATE(NEWFRCP)
         ALLOCATE(II7CP(LII7CP),NEWFRCP(SNEWFRONT),STAT=IERR)
         IF(LRI7CP.GT.0)ALLOCATE(RI7CP(LRI7CP),STAT=IERR)
C ----copy-to II7CP,RI7CP ---------------
          IID=1
	      RID=1
          DO I =1, NINTER
            ITYP = IPARI(7,I)
            IGAP = IPARI(21,I)
            INACTI = IPARI(22,I)
            INTTH = IPARI(47,I)
           IF(ITYP.EQ.7.OR.ITYP.EQ.10.OR.ITYP.EQ.11.OR.ITYP.EQ.24)THEN
	        II7CP(IID) = IPARI(24,I)
	        II7CP(IID+1) = IPARI(57,I)
	        IID=IID+2
	        CALL CP_INT(NSPMD,NSNSI(I)%P(1),II7CP(IID))
	        IID=IID+NSPMD
	        CALL CP_INT(NSPMD,NSNFI(I)%P(1),II7CP(IID))
	        IID=IID+NSPMD
              LENS = 0
              LENR = 0
              DO P = 1, NSPMD
                LENS = LENS + NSNSI(I)%P(P)
                LENR = LENR + NSNFI(I)%P(P)
              END DO
               IF (LENS.GT.0)
     .       	   CALL CP_INT(LENS,NSVSI(I)%P(1),II7CP(IID))
	        IID=IID+LENS
C
            IF (LENR.GT.0) THEN
	         CALL CP_INT(LENR,NSVFI(I)%P(1),II7CP(IID))
	         IID=IID+LENR
             IF(ITYP.EQ.7.OR.ITYP.EQ.10.OR.ITYP.EQ.24) THEN
C-------------------ITAFI,KINFI
	          CALL CP_INT(LENR,ITAFI(I)%P(1),II7CP(IID))
	          IID=IID+LENR
	          CALL CP_INT(LENR,KINFI(I)%P(1),II7CP(IID))
	          IID=IID+LENR
              IF (INTTH > 0 ) THEN
	           CALL CP_INT(LENR,MATSFI(I)%P(1),II7CP(IID))
	           IID=IID+LENR
              END IF
C-------------------MSFI,STIFI
	          CALL CP_REAL(LENR,MSFI(I)%P(1),RI7CP(RID))
	           RID=RID+LENR
	          CALL CP_REAL(LENR,STIFI(I)%P(1),RI7CP(RID))
	           RID=RID+LENR
              IF(IGAP.NE.0) THEN
	           CALL CP_REAL(LENR,GAPFI(I)%P(1),RI7CP(RID))
	           RID=RID+LENR
              ENDIF
C-------------------XFI,VFI
	          CALL CP_REAL(3*LENR,XFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+3*LENR
	          CALL CP_REAL(3*LENR,VFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+3*LENR
              IF(IPARIT.EQ.0) THEN
C-------------------AFI,STNFI,VSCFI
                    CALL CP_REAL(3*LENR*NTHREAD,AFI(I)%P(1,1),RI7CP(RID))
	            RID=RID+3*LENR*NTHREAD
	            CALL CP_REAL(LENR*NTHREAD,STNFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR*NTHREAD
               IF(KDTINT.NE.0) THEN
	            CALL CP_REAL(LENR*NTHREAD,VSCFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR*NTHREAD
               ENDIF
               IF(INTTH.NE.0) THEN
	            CALL CP_REAL(LENR*NTHREAD,FTHEFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR*NTHREAD
	            CALL CP_REAL(LENR*NTHREAD,CONDNFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR*NTHREAD
	            CALL CP_REAL(LENR,TEMPFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR
	            CALL CP_REAL(LENR,AREASFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR
               ENDIF
              ELSE
C-----------------PARITON not yet with implicit----	       
	          ENDIF
              IF(ITYP==24) THEN
	           CALL CP_INT(2*LENR,IRTLM_FI(I)%P(1,1),II7CP(IID))
	           IID=IID+2*LENR
	           CALL CP_REAL(LENR,TIME_SFI(I)%P(1),RI7CP(RID))
	           RID=RID+LENR
	           CALL CP_REAL(6*LENR,SECND_FRFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+6*LENR
	           CALL CP_REAL(5*LENR,PENE_OLDFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+5*LENR
	           CALL CP_REAL(2*LENR,STIF_OLDFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+2*LENR
              END IF
             ELSEIF(ITYP.EQ.11) THEN
C-------------------ITAFI
	          CALL CP_INT(2*LENR,ITAFI(I)%P(1),II7CP(IID))
	          IID=IID+2*LENR
              IF (INTTH > 0 ) THEN
	           CALL CP_INT(LENR,MATSFI(I)%P(1),II7CP(IID))
	           IID=IID+LENR
              END IF
C-------------------MSFI,STIFI
	          CALL CP_REAL(2*LENR,MSFI(I)%P(1),RI7CP(RID))
	           RID=RID+2*LENR
	          CALL CP_REAL(LENR,STIFI(I)%P(1),RI7CP(RID))
	           RID=RID+LENR
              IF(IGAP.NE.0) THEN
	           CALL CP_REAL(LENR,GAPFI(I)%P(1),RI7CP(RID))
	           RID=RID+LENR
              ENDIF
C-------------------XFI,VFI
	          CALL CP_REAL(6*LENR,XFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+6*LENR
	          CALL CP_REAL(6*LENR,VFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+6*LENR
              IF(INACTI.EQ.5.OR.INACTI.EQ.6) THEN
	           CALL CP_REAL(2*LENR,PENFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+2*LENR
              ENDIF
              IF(IPARIT.EQ.0) THEN
C-------------------AFI,STNFI,VSCFI
                   CALL CP_REAL(6*LENR*NTHREAD,AFI(I)%P(1,1),RI7CP(RID))
	           RID=RID+6*LENR*NTHREAD
	           CALL CP_REAL(2*LENR*NTHREAD,STNFI(I)%P(1),RI7CP(RID))
	           RID=RID+2*LENR*NTHREAD
               IF(KDTINT.NE.0) THEN
	            CALL CP_REAL(2*LENR*NTHREAD,VSCFI(I)%P(1),RI7CP(RID))
	            RID=RID+2*LENR*NTHREAD
               ENDIF
               IF(INTTH.NE.0) THEN
	            CALL CP_REAL(LENR,FTHEFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR
	            CALL CP_REAL(LENR,TEMPFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR
	            CALL CP_REAL(LENR,AREASFI(I)%P(1),RI7CP(RID))
	            RID=RID+LENR
               ENDIF
              ELSE
C-----------------PARITON not yet with implicit----	       
	          ENDIF
             ELSEIF(ITYP.EQ.17)THEN
             END IF
            END IF !IF (LENR.GT.0)
          ENDIF !IF(ITYP.EQ.7.OR.ITYP.EQ.10.OR.IT
         ENDDO
         LENSCP(1)=ISLEN7
         LENSCP(2)=IRLEN7
         LENSCP(3)=ISLEN11
         LENSCP(4)=IRLEN11
         LENSCP(5)=ISLEN17
         LENSCP(6)=IRLEN17
         LENSCP(7)=IRLEN7T 
         LENSCP(8)=ISLEN7T
         LENSCP(9)=IRLEN20
         LENSCP(10)=ISLEN20
         LENSCP(11)=IRLEN20T
         LENSCP(12)=ISLEN20T
         LENSCP(13)=IRLEN20E
         LENSCP(14)=ISLEN20E
         CALL CP_INT(SNEWFRONT,NEWFRONT,NEWFRCP)
         IF (IID.GT.(LII7CP+1).OR.RID.GT.(LRI7CP+1)) then
	       CALL ANCMSG(MSGID=82,ANMODE=ANINFO,
     .            I1=IID,I2=LII7CP,I3=RID,I4=LRI7CP)
           CALL ARRET(2)
         ENDIF
        END IF 
      ELSE	
        IF((LII7CP+LRI7CP).EQ.0) RETURN
        IF(NINTER.NE.0) THEN
C ----copy- from II7CP,RI7CP -------------
          IID=1
          RID=1
          DO I =1, NINTER
            ITYP = IPARI(7,I)
            IGAP = IPARI(21,I)
            INACTI = IPARI(22,I)
            INTTH = IPARI(47,I)
            NREBOU=IPARI(53,I)
            IF(ITYP.EQ.7.OR.ITYP.EQ.10.OR.ITYP.EQ.11.OR.ITYP.EQ.24)THEN
	         IPARI(24,I) = II7CP(IID) 
	         IPARI(57,I) = II7CP(IID+1) 
	         IID=IID+2
              LENR0 = 0
              DO P = 1, NSPMD
                LENR0 = LENR0 + NSNFI(I)%P(P)
              END DO
	         CALL CP_INT(NSPMD,II7CP(IID),NSNSI(I)%P(1))
	         IID=IID+NSPMD
	         CALL CP_INT(NSPMD,II7CP(IID),NSNFI(I)%P(1))
	         IID=IID+NSPMD
              LENS = 0
              LENR = 0
              DO P = 1, NSPMD
                LENS = LENS + NSNSI(I)%P(P)
                LENR = LENR + NSNFI(I)%P(P)
              END DO
	         IF (LENS.GT.0) THEN
               IF(ASSOCIATED(NSVSI(I)%P)) DEALLOCATE(NSVSI(I)%P)
               ALLOCATE(NSVSI(I)%P(LENS),STAT=IERR)
	          CALL CP_INT(LENS,II7CP(IID),NSVSI(I)%P(1))
	          IID=IID+LENS
	         ENDIF	      
C-----------------ALLOCATION cp from II7CP,RI7CP----------
              IF (LENR.GT.0) THEN
               IF(ASSOCIATED(NSVFI(I)%P)) DEALLOCATE(NSVFI(I)%P)
               ALLOCATE(NSVFI(I)%P(LENR),STAT=IERR)
	           CALL CP_INT(LENR,II7CP(IID),NSVFI(I)%P(1))
	           IID=IID+LENR
               IF(ITYP.EQ.7.OR.ITYP.EQ.10.OR.ITYP.EQ.24) THEN
                  IF(ASSOCIATED(ITAFI(I)%P)) DEALLOCATE(ITAFI(I)%P)
                  ALLOCATE(ITAFI(I)%P(LENR),STAT=IERR)
	              CALL CP_INT(LENR,II7CP(IID),ITAFI(I)%P(1))
	              IID=IID+LENR
                  IF(ASSOCIATED(KINFI(I)%P)) DEALLOCATE(KINFI(I)%P)
                  ALLOCATE(KINFI(I)%P(LENR),STAT=IERR)
	              CALL CP_INT(LENR,II7CP(IID),KINFI(I)%P(1))
	              IID=IID+LENR
                  IF(INTTH > 0 ) THEN
                   IF(ASSOCIATED(MATSFI(I)%P)) DEALLOCATE(MATSFI(I)%P)
                   ALLOCATE(MATSFI(I)%P(LENR),STAT=IERR)
	               CALL CP_INT(LENR,II7CP(IID),MATSFI(I)%P(1))
	               IID=IID+LENR
                  ENDIF
                  IF(ASSOCIATED(MSFI(I)%P)) DEALLOCATE(MSFI(I)%P)
                  ALLOCATE(MSFI(I)%P(LENR),STAT=IERR)
	              CALL CP_REAL(LENR,RI7CP(RID),MSFI(I)%P(1))
	              RID=RID+LENR
                  IF(ASSOCIATED(STIFI(I)%P)) DEALLOCATE(STIFI(I)%P)
                  ALLOCATE(STIFI(I)%P(LENR),STAT=IERR)
	              CALL CP_REAL(LENR,RI7CP(RID),STIFI(I)%P(1))
	              RID=RID+LENR
                  IF(IGAP.NE.0) THEN
                   IF(ASSOCIATED(GAPFI(I)%P)) DEALLOCATE(GAPFI(I)%P)
                   ALLOCATE(GAPFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR,RI7CP(RID),GAPFI(I)%P(1))
	               RID=RID+LENR
                  ENDIF
                  IF(ASSOCIATED(XFI(I)%P)) DEALLOCATE(XFI(I)%P)
                  ALLOCATE(XFI(I)%P(3,LENR),STAT=IERR)
	              CALL CP_REAL(3*LENR,RI7CP(RID),XFI(I)%P(1,1))
	              RID=RID+3*LENR
                  IF(ASSOCIATED(VFI(I)%P)) DEALLOCATE(VFI(I)%P)
                  ALLOCATE(VFI(I)%P(3,LENR),STAT=IERR)
	              CALL CP_REAL(3*LENR,RI7CP(RID),VFI(I)%P(1,1))
	              RID=RID+3*LENR
                  IF(IPARIT.EQ.0) THEN
                     IF(ASSOCIATED(AFI(I)%P)) DEALLOCATE(AFI(I)%P)
                     ALLOCATE(AFI(I)%P(3,LENR*NTHREAD),STAT=IERR)
	             CALL CP_REAL(3*LENR*NTHREAD,RI7CP(RID),AFI(I)%P(1,1))
	             RID=RID+3*LENR*NTHREAD
                     IF(ASSOCIATED(STNFI(I)%P)) DEALLOCATE(STNFI(I)%P)
                     ALLOCATE(STNFI(I)%P(LENR*NTHREAD),STAT=IERR)
	             CALL CP_REAL(LENR*NTHREAD,RI7CP(RID),STNFI(I)%P(1))
	             RID=RID+LENR*NTHREAD
                     NLSKYFI(I)=LENR
                  IF(KDTINT.NE.0)THEN
                   IF(ASSOCIATED(VSCFI(I)%P)) DEALLOCATE(VSCFI(I)%P)
                   ALLOCATE(VSCFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR*NTHREAD,RI7CP(RID),VSCFI(I)%P(1))
	               RID=RID+LENR*NTHREAD
                  ENDIF
                  IF(INTTH > 0 )THEN    
C
                   IF(ASSOCIATED(FTHEFI(I)%P)) DEALLOCATE(FTHEFI(I)%P)
                   ALLOCATE(FTHEFI(I)%P(LENR*NTHREAD),STAT=IERR)
	               CALL CP_REAL(LENR*NTHREAD,RI7CP(RID),FTHEFI(I)%P(1))
	               RID=RID+LENR*NTHREAD
C
                   IF(ASSOCIATED(CONDNFI(I)%P)) DEALLOCATE(CONDNFI(I)%P)
                   ALLOCATE(CONDNFI(I)%P(LENR*NTHREAD),STAT=IERR)
	               CALL CP_REAL(LENR*NTHREAD,RI7CP(RID),CONDNFI(I)%P(1))
	               RID=RID+LENR*NTHREAD
C
                   IF(ASSOCIATED(TEMPFI(I)%P)) DEALLOCATE(TEMPFI(I)%P)
                   ALLOCATE(TEMPFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR,RI7CP(RID),TEMPFI(I)%P(1))
	               RID=RID+LENR
C
                   IF(ASSOCIATED(AREASFI(I)%P))DEALLOCATE(AREASFI(I)%P)
                   ALLOCATE(AREASFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR,RI7CP(RID),AREASFI(I)%P(1))
	               RID=RID+LENR
                  ENDIF
                ELSE
C-----------------PARITON not yet with implicit----	       
	            ENDIF
                IF(ITYP==24) THEN
                 IF(ASSOCIATED(IRTLM_FI(I)%P))DEALLOCATE(IRTLM_FI(I)%P)
                 ALLOCATE(IRTLM_FI(I)%P(2,LENR),STAT=IERR)
	             CALL CP_INT(2*LENR,II7CP(IID),IRTLM_FI(I)%P(1,1))
	             IID=IID+2*LENR
                 IF(ASSOCIATED(TIME_SFI(I)%P))DEALLOCATE(TIME_SFI(I)%P)
                 ALLOCATE(TIME_SFI(I)%P(LENR),STAT=IERR)
	             CALL CP_REAL(LENR,RI7CP(RID),TIME_SFI(I)%P(1))
	             RID=RID+LENR
                 IF(ASSOCIATED(SECND_FRFI(I)%P))
     +                    DEALLOCATE(SECND_FRFI(I)%P)
                 ALLOCATE(SECND_FRFI(I)%P(6,LENR),STAT=IERR)
	             CALL CP_REAL(6*LENR,RI7CP(RID),SECND_FRFI(I)%P(1,1))
	             RID=RID+6*LENR
                 IF(ASSOCIATED(PENE_OLDFI(I)%P))
     +                    DEALLOCATE(PENE_OLDFI(I)%P)
                 ALLOCATE(PENE_OLDFI(I)%P(5,LENR),STAT=IERR)
	             CALL CP_REAL(5*LENR,RI7CP(RID),PENE_OLDFI(I)%P(1,1))
	             RID=RID+5*LENR
                 IF (NREBOU <0.AND.IMCONV>=0) THEN
	              CALL CP_REAL(2*LENR,STIF_OLDFI(I)%P(1,1),RI7CP(RID))
                 ELSE
                  IF(ASSOCIATED(STIF_OLDFI(I)%P))
     +                    DEALLOCATE(STIF_OLDFI(I)%P)
                  ALLOCATE(STIF_OLDFI(I)%P(2,LENR),STAT=IERR)
	              CALL CP_REAL(2*LENR,RI7CP(RID),STIF_OLDFI(I)%P(1,1))
                 END IF !(NREBOU <0.AND.IMCONV>=0) THEN
	             RID=RID+2*LENR
                END IF
               ELSEIF(ITYP.EQ.11) THEN
C
                IF(ASSOCIATED(ITAFI(I)%P)) DEALLOCATE(ITAFI(I)%P)
                ALLOCATE(ITAFI(I)%P(2*LENR),STAT=IERR)
	            CALL CP_INT(2*LENR,II7CP(IID),ITAFI(I)%P(1))
	            IID=IID+2*LENR
                  IF(INTTH > 0 ) THEN
                   IF(ASSOCIATED(MATSFI(I)%P)) DEALLOCATE(MATSFI(I)%P)
                   ALLOCATE(MATSFI(I)%P(LENR),STAT=IERR)
	               CALL CP_INT(LENR,II7CP(IID),MATSFI(I)%P(1))
	               IID=IID+LENR
                  ENDIF
                  IF(ASSOCIATED(MSFI(I)%P)) DEALLOCATE(MSFI(I)%P)
                  ALLOCATE(MSFI(I)%P(2*LENR),STAT=IERR)
	            CALL CP_REAL(2*LENR,RI7CP(RID),MSFI(I)%P(1))
	            RID=RID+2*LENR
                  IF(ASSOCIATED(STIFI(I)%P)) DEALLOCATE(STIFI(I)%P)
                  ALLOCATE(STIFI(I)%P(LENR),STAT=IERR)
	            CALL CP_REAL(LENR,RI7CP(RID),STIFI(I)%P(1))
	            RID=RID+LENR
                 IF(IGAP.NE.0) THEN
                   IF(ASSOCIATED(GAPFI(I)%P)) DEALLOCATE(GAPFI(I)%P)
                   ALLOCATE(GAPFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR,RI7CP(RID),GAPFI(I)%P(1))
	               RID=RID+LENR
                 ENDIF
                  IF(ASSOCIATED(XFI(I)%P)) DEALLOCATE(XFI(I)%P)
                  ALLOCATE(XFI(I)%P(3,2*LENR),STAT=IERR)
	             CALL CP_REAL(6*LENR,RI7CP(RID),XFI(I)%P(1,1))
	             RID=RID+6*LENR
                  IF(ASSOCIATED(VFI(I)%P)) DEALLOCATE(VFI(I)%P)
                  ALLOCATE(VFI(I)%P(3,2*LENR),STAT=IERR)
	             CALL CP_REAL(6*LENR,RI7CP(RID),VFI(I)%P(1,1))
	             RID=RID+6*LENR
                 IF(INACTI.EQ.5.OR.INACTI.EQ.6) THEN
                  IF(ASSOCIATED(PENFI(I)%P)) DEALLOCATE(PENFI(I)%P)
                  ALLOCATE(PENFI(I)%P(2,LENR),STAT=IERR)
	              CALL CP_REAL(2*LENR,RI7CP(RID),PENFI(I)%P(1,1))
	              RID=RID+2*LENR
                 END IF
                 IF(IPARIT.EQ.0) THEN

                  IF(ASSOCIATED(AFI(I)%P)) DEALLOCATE(AFI(I)%P)
                  ALLOCATE(AFI(I)%P(3,2*LENR*NTHREAD),STAT=IERR)
	              CALL CP_REAL(6*LENR*NTHREAD,RI7CP(RID),AFI(I)%P(1,1))
	              RID=RID+6*LENR*NTHREAD

                  IF(ASSOCIATED(STNFI(I)%P)) DEALLOCATE(STNFI(I)%P)
                  ALLOCATE(STNFI(I)%P(2*LENR*NTHREAD),STAT=IERR)
	              CALL CP_REAL(2*LENR*NTHREAD,RI7CP(RID),STNFI(I)%P(1))
	              RID=RID+2*LENR

                  IF(KDTINT.NE.0)THEN
                   IF(ASSOCIATED(VSCFI(I)%P)) DEALLOCATE(VSCFI(I)%P)
                   ALLOCATE(VSCFI(I)%P(2*LENR),STAT=IERR)
	               CALL CP_REAL(2*LENR*NTHREAD,RI7CP(RID),VSCFI(I)%P(1))
	               RID=RID+2*LENR*NTHREAD
                  ENDIF
                  IF(INTTH > 0 )THEN    
                   IF(ASSOCIATED(FTHEFI(I)%P)) DEALLOCATE(FTHEFI(I)%P)
                   ALLOCATE(FTHEFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR,RI7CP(RID),FTHEFI(I)%P(1))
	               RID=RID+LENR
                   IF(ASSOCIATED(TEMPFI(I)%P)) DEALLOCATE(TEMPFI(I)%P)
                   ALLOCATE(TEMPFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR,RI7CP(RID),TEMPFI(I)%P(1))
	               RID=RID+LENR
                   IF(ASSOCIATED(AREASFI(I)%P))DEALLOCATE(AREASFI(I)%P)
                   ALLOCATE(AREASFI(I)%P(LENR),STAT=IERR)
	               CALL CP_REAL(LENR,RI7CP(RID),AREASFI(I)%P(1))
	               RID=RID+LENR
                  ENDIF
                     NLSKYFI(I)=LENR*2
                 ELSE
C-----------------PARITON not yet with implicit----	       
	             ENDIF
               ELSEIF(ITYP.EQ.17)THEN
	          ENDIF !IF(ITYP.EQ.7.OR.ITYP.EQ.10)	      
	         ENDIF !IF (LENR.GT0)	      
            ENDIF
         ENDDO
         ISLEN7 = LENSCP(1)
         IRLEN7 = LENSCP(2)
         ISLEN11= LENSCP(3)
         IRLEN11= LENSCP(4)
         ISLEN17= LENSCP(5)
         IRLEN17= LENSCP(6)
         IRLEN7T= LENSCP(7) 
         ISLEN7T= LENSCP(8)
         IRLEN20= LENSCP(9)
         ISLEN20= LENSCP(10)
         IRLEN20T=LENSCP(11)
         ISLEN20T=LENSCP(12)
         IRLEN20E=LENSCP(13)
         ISLEN20E=LENSCP(14)
         CALL CP_INT(SNEWFRONT,NEWFRCP,NEWFRONT)
         IF (IID.GT.(LII7CP+1).OR.RID.GT.(LRI7CP+1)) then
	      CALL ANCMSG(MSGID=82,ANMODE=ANINFO,
     .            I1=IID,I2=LII7CP,I3=RID,I4=LRI7CP)
          CALL ARRET(2)
         ENDIF
        END IF !IF(NINTER.NE.0) THEN
      ENDIF !IF(IFLAG.EQ.1) THEN
C-----------------------------------------------------------------------
      RETURN
      END
C---------norm2={x}^t{x}--x comes from D(3,*)- including kinematic nodes
Chd|====================================================================
Chd|  PRODUT_U0                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE PRODUT_U0(DD    ,DDR   ,NORM2 ,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  WEIGHT(*)  
C     REAL
      my_real
     .  DD(3,*),DDR(3,*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      NORM2=ZERO
      DO I=1,NUMNOD
       IF (WEIGHT(I).EQ.1) THEN
        NORM2 = NORM2 + DD(1,I)*DD(1,I)
        NORM2 = NORM2 + DD(2,I)*DD(2,I)
        NORM2 = NORM2 + DD(3,I)*DD(3,I)
       END IF 
      END DO
      IF (IRODDL.GT.0) THEN
       DO I=1,NUMNOD
        IF (WEIGHT(I).EQ.1) THEN
         NORM2 = NORM2 + DDR(1,I)*DDR(1,I)
         NORM2 = NORM2 + DDR(2,I)*DDR(2,I)
         NORM2 = NORM2 + DDR(3,I)*DDR(3,I)
        END IF 
       END DO
      END IF 
C       
      IF (NSPMD.GT.1) THEN
        CALL SPMD_SUM_S(NORM2)
      END IF 
C--------------------------------------------
      RETURN
      END
C     
C------Hybrid----produit {W}=[DIAG_K]{v}+[LT_K]{v}+[LT_K0]{v}+[KI0]{v}----
Chd|====================================================================
Chd|  MAV_LTGH                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MAV_LUI_H                     source/implicit/produt_v.F    
Chd|        MAV_LU_H                      source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE MAV_LTGH(
     1                    NDDL  ,IADL  ,JDIL  ,DIAG_K,LT_K  ,   
     2                    V     ,W     ,F_DDL ,L_DDL ,ITASK ,
     3                    NDDLI )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADL(*),JDIL(*),F_DDL  ,L_DDL   ,ITASK,
     .         NDDLI
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------
            CALL MAV_LU_H(NDDL   ,
     1                    F_DDL  ,L_DDL   ,IADL  ,JDIL   ,DIAG_K ,   
     2                    LT_K   ,V       ,W     )
      IF (NDDLI>0) CALL MAV_LUI_H(F_DDL ,L_DDL  ,V   ,W     )
C----------------------
      CALL MY_BARRIER
C---------------------
      IF (ITASK.EQ.0.AND.NSPMD.GT.1) CALL SPMD_SUMF_V(W )
C--------------------------------------------
      RETURN
      END
C-------------produit {w}=[K]{v} using [K] completebut w/o DIAG----
Chd|====================================================================
Chd|  MAV_LUI_H                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        MAV_LTGH                      source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE MAV_LUI_H(F_DDL  ,L_DDL   ,V      ,W     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
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
      INTEGER  F_DDL  ,L_DDL   
C     REAL
      my_real
     .  W(*), V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   L_K
C      
      DO I=F_DDL,L_DDL
       DO J =IADI0(I),IADI0(I+1)-1
        K =JDII0(J)
        L_K = LT_I0(J)
        W(I) = W(I) + L_K*V(K)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CP_DM                         source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_RESTARCP                  source/implicit/imp_sol_init.F
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CP_DM(NUMGEO,GEO,IGEO,DMCP,IFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMGEO,IGEO(NPROPGI,*),IFLAG  
C     REAL
      my_real
     .  GEO(NPROPG,*),DMCP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGTYP
C-----remove membraine material damping w/ implicit static-----
C-----IFLAG=1 > store dm to DMCP and put dm=zero ; IFLAG=2 reput dm
      IF (IFLAG == 1) THEN
         DO I=1,NUMGEO
          IGTYP = IGEO(11,I)  
          IF(IGTYP==1.OR.(IGTYP>=9 .AND. IGTYP<=11).OR.IGTYP==16) THEN  
            DMCP(I) = GEO(16,I)                       
            GEO(16,I) = EM30                        
          ENDIF                                                
	     END DO
      ELSE
         DO I=1,NUMGEO
          IGTYP = IGEO(11,I)  
          IF(IGTYP==1.OR.(IGTYP>=9 .AND. IGTYP<=11).OR.IGTYP==16) THEN  
            GEO(16,I) = DMCP(I)                        
          ENDIF                                                
	     END DO
      END IF
      RETURN
      END
Chd|====================================================================
Chd|  VSCAL_H                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        MORTHO_GS                     source/implicit/produt_v.F    
Chd|        SMS_MORTHO_GS                 source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VSCAL_H(F_DDL  ,L_DDL ,V   ,S    ,ITASK  )
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
      INTEGER  F_DDL,L_DDL ,ITASK
C     REAL
      my_real
     .  S, V(*)
C-----------------------------------------------
c PURPOSE:    V(*)<-S*V(*)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   F_DDL,L_DDL,ITASK  - dim. of V(F_DDL:L_DDL), of Itask (Thread id)
c  IO  V(*)               - V(*) scaled for output
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
       DO I= F_DDL,L_DDL
        V(I) = S*V(I)
       END DO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  VAXPY_H                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        MORTHO_GS                     source/implicit/produt_v.F    
Chd|        SMS_MORTHO_GS                 source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VAXPY_H(F_DDL  ,L_DDL ,A   ,B    ,S    ,ITASK  )
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
      INTEGER  F_DDL,L_DDL ,ITASK
C     REAL
      my_real
     .  S, A(*),B(*)
C-----------------------------------------------
c PURPOSE:    B(*)<- B(*)+S*A(*)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   F_DDL,L_DDL,ITASK  - dim. of V(F_DDL:L_DDL), of Itask (Thread id)
c  I   A(*)               - input vector
c  IO  B(*)               - axpy for output
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
       DO I= F_DDL,L_DDL
        B(I) = B(I) + S*A(I)
       END DO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MORTHO_GS                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_INISI                     source/implicit/imp_pcg.F     
Chd|        IMP_UPDST                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        VAXPY_H                       source/implicit/produt_v.F    
Chd|        VSCAL_H                       source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE MORTHO_GS(F_DDL  ,L_DDL  ,NDDL   ,MD_F   ,MD_L   ,
     .                     A      ,WDDL   ,ITASK  )
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
      INTEGER  NDDL,MD_F,MD_L,F_DDL,L_DDL    ,WDDL(*), ITASK
C     REAL
      my_real
     .  A(NDDL,*)
C-----------------------------------------------
c FUNCTION: stabilized Gram-Schmidt orthonormalization (from MD_F to MD_L)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   MD_F to MD_L       - vectors to be orthonormalized dim. of A(*,MD) should be MD_L
c  I   F_DDL,L_DDL,ITASK  - dim. of A(F_DDL:L_DDL,MD), of Itask (Thread id)
c  I   WDDL(*)            - itag for each id(F_DDL,L_DDL) with subdomains
c  IO  A(NDDL,MD)         - A(NDDL,MD) orthonormalized for output
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .  SII,SIJ,S,SJJ
C-----------------------------
       DO J= MD_F ,MD_L 
        DO I=1,J-1
          CALL PRODUT_H(F_DDL ,L_DDL ,A(1,I) ,A(1,J) ,WDDL, SIJ ,ITASK)
          S = -SIJ
          CALL VAXPY_H(F_DDL ,L_DDL ,A(1,I) ,A(1,J) ,S  ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
        END DO
         CALL PRODUT_H(F_DDL ,L_DDL ,A(1,J) ,A(1,J) ,WDDL, SJJ ,ITASK)
         S= ONE/MAX(EM20,SQRT(SJJ))
         CALL VSCAL_H(F_DDL ,L_DDL ,A(1,J) ,S  ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
       END DO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MAV_NM                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_INIX                      source/implicit/imp_pcg.F     
Chd|        IMP_PRO_P                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE MAV_NM(F_ND ,L_ND ,ND   ,MD  ,A  ,B  ,C  ,WDDL,ITASK )
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
      INTEGER  F_ND ,L_ND ,ND  ,MD   ,ITASK,WDDL(*)
C     REAL
      my_real
     .  A(ND,*), B(*), C(*)  
C-----------------------------------------------
c FUNCTION: product {C}=[A]^t{B}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   ND,MD             - Matrix dimension 2D
c  I   WDDL(*)           - itag for each id of subdomains
c  I   F_ND,L_ND,ITASK   - id in each ITASK:thread id (//)
c  I   A(ND,MN),B(ND)    - right-hand vector
c  O   C(NM)             - left-hand vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C-----------------------------
      DO I=1,MD
       CALL PRODUT_H( F_ND ,L_ND ,A(1,I)  ,B  ,WDDL ,C(I),ITASK)
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MAV_MN                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_INIX                      source/implicit/imp_pcg.F     
Chd|        IMP_PRO_P                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        PRODUT_V_LOC                  source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE MAV_MN(ND   ,MD     ,A     ,B     ,C      ,ITASK )
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
      INTEGER  ND  ,MD   ,ITASK
C     REAL
      my_real
     .  A(ND,*), B(*), C(*)  
C-----------------------------------------------
c FUNCTION: product {C}=[A]{B}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   ND,MN             - Matrix dimension 2D
c  I   ITASK             - thread id (//)
c  I   B(NM)             - right-hand vector
c  O   C(ND)             - left-hand vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  W(MD) 
C-----------------------------
      IF (ITASK /= 0) RETURN
C------------may add dynamic smp on ND after--      
      DO I=1,ND
       DO J= 1,MD
        W(J)= A(I,J)
       END DO
       CALL PRODUT_V_LOC( MD  ,W  ,B  ,C(I))
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MAM_NM                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_INIST                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE MAM_NM(F_ND ,L_ND ,ND, MD   ,A   ,B   ,C  ,WDDL,ITASK)
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
      INTEGER  F_ND ,L_ND ,ND  ,MD   ,ITASK,WDDL(*)
C     REAL
      my_real
     .  A(ND,*), B(ND,*), C(MD,*)  
C-----------------------------------------------
c FUNCTION: product {C}=[A]^t[B]
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   ND,MD             - Matrix dimension 2D
c  I   WDDL(*)           - itag for each id of subdomains
c  I   F_ND,L_ND,ITASK   - id in each ITASK:thread id (//)
c  I   B(ND,MD)          - right-hand Matrix
c  O   C(NM,MD)             - left-hand vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C-----------------------------
      DO I=1,MD
       DO J=1,MD
        CALL PRODUT_H( F_ND ,L_ND ,A(1,I)  ,B(1,J) ,WDDL,C(I,J),ITASK)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MAV_MM                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_INIST                     source/implicit/imp_pcg.F     
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAV_MM(ND   ,MD     ,A     ,B     ,ITASK )
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
      INTEGER  ND  ,MD   ,ITASK
C     REAL
      my_real
     .  A(ND,*), B(MD,*)  
C-----------------------------------------------
c FUNCTION: product [A]<-[A][B]
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   ND,MN             - Matrix dimension 2D
c  I   ITASK             - thread id (//)
c  IO  A(ND,MD)          - Matrix A
c  O   B(MD,MD)          - Matrix B
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  C(ND,MD)
C-----------------------------
      IF (ITASK /= 0) RETURN
C------------may add dynamic smp on ND after--      
      DO I=1,ND
      DO J=1,MD
       C(I,J)=ZERO
       DO K=1,MD
        C(I,J) = C(I,J)+A(I,K)*B(K,J)
       END DO
      ENDDO
      ENDDO
C      
      DO I=1,ND
      DO J=1,MD
       A(I,J) = C(I,J)
      ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MMAV_LTH                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_INISI                     source/implicit/imp_pcg.F     
Chd|        IMP_INIST                     source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        IMP_UPDV2                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MMV_LH                        source/implicit/produt_v.F    
Chd|        MMV_LTH                       source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MMAV_LTH(
     1                    NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2                    LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3                    V     ,W     ,A     ,AR    ,VE    ,
     5                    MS    ,X     ,D     ,DR    ,NDOF  ,
     6                    IPARI ,INTBUF_TAB   ,NUM_IMP,NS_IMP,
     7                    NE_IMP,NSREM ,NSL   ,IBFV  ,SKEW  ,
     8                    XFRAME,MONVOL,VOLMON,IGRSURF ,
     9                    FR_MV ,NMONV ,IMONV ,IND_IMP ,XI_C  ,
     A                    IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 ,
     B                    IADM  ,JDIM  ,DIAG_M,LT_M   ,F_DDL  ,
     C                    L_DDL ,ITASK ,V_W   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NDDLI,NDOF(*),IUPD,
     .         IADK(*),JDIK(*),IADI(*),JDII(*),ITOK(*),
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,
     .         NE_IMP(*),NSREM ,NSL,IBFV(*),IND_IMP(*),
     .         IRBE3(*),LRBE3(*),F_DDL  ,L_DDL   ,ITASK,
     .         IRBE2(*),LRBE2(*),IADM(*) ,JDIM(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*), W(*), LT_K(*)  ,LT_I(*)  ,V(*) ,
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),X(3,*),
     .  MS(*),VOLMON(*),SKEW(*),XFRAME(*),XI_C(*) ,
     .  DIAG_M(*),LT_M(*),V_W(*)  

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C------Hybrid----produit {w}=[K']{v}=Lm[K]Lm^t{v} (Lm=L_M*sqrt(Diag_M))--DIAG_M->sqrt()--
C--------------if M=Lm*D*Lm^t, should be -> [K']=sqrt(D)*Lm^t[K]Lm*sqrt(D)
      INTEGER I,J,K
      my_real
     .  ZW1(NDDL) 
C--------------------------------------------
C------------------{zw1}=Lm^t{v}--------------------------
        CALL MMV_LH(
     1              NDDL  ,IADM0 ,JDIM0 ,DIAG_M ,LT_M0 ,   
     2              V     ,V_W   ,F_DDL ,L_DDL  ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
       DO I=1 ,NDDL
        ZW1(I) = V_W(I)
       ENDDO
C----------------------
      CALL MY_BARRIER
C-----------------{zw2}=[K]{zw1}----
       CALL MAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            ZW1   ,V_W   ,A     ,AR    ,
     5            VE    ,MS    ,X     ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,F_DDL ,L_DDL ,ITASK )
C----------------------
      CALL MY_BARRIER
C-----------------{w}=[Lm]{zw2}----
        CALL MMV_LTH(
     1               NDDL  ,IADM  ,JDIM  ,DIAG_M ,LT_M  ,   
     2               V_W   ,W     ,F_DDL ,L_DDL  ,ITASK )
C      
      RETURN
      END
Chd|====================================================================
Chd|  MMV_LTH                       source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE MMV_LTH(
     1                    NDDL  ,IADM  ,JDIM  ,DIAG_M ,LT_M  ,   
     2                    V     ,Z     ,F_DDL ,L_DDL  ,ITASK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "impl1_c.inc"
#include       "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADM(*)  ,JDIM(*),F_DDL ,L_DDL,ITASK
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C------hybrid version-solves  {z}=[D]^-1/2[Z]^t{v}-----
      INTEGER I,J,K
C-----------------------------
      DO I=F_DDL ,L_DDL
        Z(I) = V(I)
      ENDDO
C      
      IF (IPREC.EQ.2) THEN
       DO I=F_DDL ,L_DDL
        Z(I) = V(I)*DIAG_M(I)
       ENDDO
      ELSEIF (IPREC.EQ.5) THEN
C--------{z}=[Z]^t{v}-------------
       DO I=F_DDL ,L_DDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(I) = Z(I)+LT_M(J)*V(K)
        ENDDO
       ENDDO
C--------{z}=[D]^-1/2{z}-------------
       DO I=F_DDL ,L_DDL
        Z(I) = Z(I)*DIAG_M(I)
       ENDDO
      END IF !(IPREC.EQ.2) THEN
C      
      IF (IPREC.GT.1) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK.EQ.0.AND.NSPMD.GT.1) CALL SPMD_SUMF_V(Z)
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MMV_LH                        source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE MMV_LH(
     1                    NDDL  ,IADM  ,JDIM  ,DIAG_M ,LT_M  ,   
     2                    V     ,Z     ,F_DDL ,L_DDL  ,ITASK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "impl1_c.inc"
#include       "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADM(*)  ,JDIM(*),F_DDL ,L_DDL,ITASK
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-------hybrid version-solves  {z}=[Z][D]^-1/2{v}----[Z] en colonne-
      INTEGER I,J,K
      my_real
     .  TMP(NDDL) 
C-----------------------------
      DO I=F_DDL ,L_DDL
        Z(I) = V(I)
      ENDDO
C      
      IF (IPREC.EQ.2) THEN
       DO I=F_DDL ,L_DDL
        Z(I) = V(I)*DIAG_M(I)
       ENDDO
      ELSEIF (IPREC.EQ.5) THEN
C--------{z}=[D]^-1/2{v}-------------
       DO I=F_DDL ,L_DDL
        Z(I) = V(I)*DIAG_M(I)
       ENDDO
C----------------------
      CALL MY_BARRIER
C---------------------
       DO I=1 ,NDDL
        TMP(I) = Z(I)
       ENDDO
C----------------------
      CALL MY_BARRIER
C---------------------
C --------{z}=[Z]{z}-------     
       DO I=F_DDL ,L_DDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(I) = Z(I)+LT_M(J)*TMP(K)
        ENDDO
       ENDDO
      END IF !(IPREC.EQ.2) THEN
C      
      IF (IPREC.GT.1) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK.EQ.0.AND.NSPMD.GT.1) CALL SPMD_SUMF_V(Z)
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MAV_LIUH                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE MAV_LIUH(F_DDL  ,L_DDL   ,IADI  ,JDII   ,ITOK  ,   
     2                    LT_I   ,WORK_II ,V     ,W      ,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
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
      INTEGER  F_DDL  ,L_DDL   ,IADI(*)  ,JDII(*),ITOK(*),ITASK
C     REAL
      my_real
     .  W(*), LT_I(*)  ,V(*) ,WORK_II(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,KK
      my_real
     .   L_K
C----------------------------
       DO I=F_DDL,L_DDL
         WORK_II(I) = ZERO
       ENDDO
C
      DO I=F_DDL,L_DDL
       DO J =IADI(I),IADI(I+1)-1
        K =JDII(J)
        KK = ITOK(K)
        L_K = LT_I(J)
        WORK_II(I) = WORK_II(I) + L_K*V(KK)
       ENDDO
      ENDDO
C      
      DO I=F_DDL,L_DDL
       DO J =IADI0(I),IADI0(I+1)-1
        K =JDII0(J)
        KK = ITOK(K)
        L_K = LT_I0(J)
        WORK_II(I) = WORK_II(I) + L_K*V(KK)
       ENDDO
      ENDDO
C---------------------
      DO I=F_DDL,L_DDL
        II = ITOK(I)
        W(II) = W(II) + WORK_II(I)
      ENDDO
C--------------------------------------------
      RETURN
      END
C-----------Hybrid {x}t{y}-.{Weight}--HP: Hyprid SMP // inside
Chd|====================================================================
Chd|  PRODUT_HP                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|        PRODUT_UHP                    source/implicit/produt_v.F    
Chd|        PRODUT_UHP2                   source/implicit/produt_v.F    
Chd|        PRODUT_VMHP                   source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRODUT_HP(NDDL ,X   ,Y  ,W , R )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "impl2_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,W(*) 
C     REAL
      my_real
     .  X(*), Y(*)  ,R
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  F_DDL  ,L_DDL ,ITSK
      INTEGER I ,N,J
      my_real
     .  RL
C-----------------------------
      R = ZERO
C---------------------
      IF (NSPMD > 1) THEN
!$OMP PARALLEL PRIVATE(ITSK,F_DDL ,L_DDL,RL,J)
       CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL  )
       RL = ZERO
       DO J=F_DDL,L_DDL
         RL = RL + X(J)*Y(J)*W(J)
       END DO 
#include "lockon.inc"
        R = R + RL
#include "lockoff.inc"
!$OMP END PARALLEL 
         IF(IMONM > 0) CALL STARTIME(67,1)
         CALL SPMD_SUM_S(R)
         IF(IMONM > 0) CALL STARTIME(67,1)
C------mono domain
      ELSE
!$OMP PARALLEL PRIVATE(ITSK,F_DDL ,L_DDL,RL,J )
       CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL  )
       RL = ZERO
       DO J=F_DDL,L_DDL
         RL = RL + X(J)*Y(J)
       END DO 
#include "lockon.inc"
        R = R + RL
#include "lockoff.inc"
!$OMP END PARALLEL 
      END IF !(NSPMD > 1) THEN
C---------------------
      RETURN
      END
C---------------------r={x}^t{y}--x comes from u(1-3,*)-
Chd|====================================================================
Chd|  PRODUT_VMHP                   source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        D_TO_U                        source/implicit/produt_v.F    
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE PRODUT_VMHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                       DD    ,DDR   ,Y     ,R     ,W_IMP )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*) ,W_IMP(*) 
      my_real
     .  DD(*),DDR(*), Y(*)  ,R 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .  TMP_W1(NDDL) 
C-------------to // D_TO_U----------------
        CALL D_TO_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .              DD    ,DDR   ,TMP_W1 )
C---------------------
        CALL PRODUT_HP(NDDL,TMP_W1,Y,W_IMP,R)
C--------------------------------------------
      RETURN
      END
C-------------norm2={x}^t{x}--x comes from u(3,*)- Hybrid---
Chd|====================================================================
Chd|  PRODUT_UHP                    source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        AL_CONSTRAINT1_HP             source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINT2_HP             source/implicit/nl_solv.F     
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                      DD    ,DDR   ,NORM2 ,W_IMP )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_IMP(*) ,
     .         F_DDL ,L_DDL ,ITASK  
C     REAL
      my_real
     .  DD(*),DDR(*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  TMP_W1(NDDL0)
C----------to // IMP_SETB,CONDENS_B-------------------
       CALL IMP_SETB(DD  ,DDR   ,IDDL   ,NDOF  ,TMP_W1)
       CALL CONDENS_B(NDDL0  ,IKC  ,TMP_W1)
       CALL PRODUT_HP(NDDL,TMP_W1,TMP_W1,W_IMP,NORM2)
      RETURN
      END
C---------------------norm2={x}^t{y}--x,y come from D1(3,*),D2-- Hybrid
Chd|====================================================================
Chd|  PRODUT_UHP2                   source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        AL_CONSTRAINT1_HP             source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINT2_HP             source/implicit/nl_solv.F     
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE PRODUT_UHP2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                       D1    ,D1R   ,D2    ,D2R   ,NORM2 ,
     .                       W_IMP )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_IMP(*)  
C     REAL
      my_real
     .  D1(*),D1R(*), D2(*),D2R(*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  TMP_W1(NDDL0),TMP_W2(NDDL0)
C-----------------------------
        CALL IMP_SETB(D1  ,D1R   ,IDDL   ,NDOF  ,TMP_W1)
        CALL IMP_SETB(D2  ,D2R   ,IDDL   ,NDOF  ,TMP_W2)
        CALL CONDENS_B(NDDL0  ,IKC  ,TMP_W1)
        CALL CONDENS_B(NDDL0  ,IKC  ,TMP_W2)
C----------------------
      CALL PRODUT_HP(NDDL,TMP_W1,TMP_W2,W_IMP,NORM2)
C--------------------------------------------
      RETURN
      END
C---------norm2={x}^t{x}--x comes from D(3,*)- including kinematic nodes
Chd|====================================================================
Chd|  PRODUT_UHP0                   source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE PRODUT_UHP0(DD    ,DDR   ,NORM2 ,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  WEIGHT(*)  
C     REAL
      my_real
     .  DD(3,*),DDR(3,*), NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITSK,NODFT ,NODLT,I
C-----------------------------
      NORM2=ZERO
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,I)
      CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT ,NUMNOD )
      DO I=NODFT ,NODLT
       IF (WEIGHT(I).EQ.1) THEN
#include "lockon.inc"
        NORM2 = NORM2 + DD(1,I)*DD(1,I)
        NORM2 = NORM2 + DD(2,I)*DD(2,I)
        NORM2 = NORM2 + DD(3,I)*DD(3,I)
#include "lockoff.inc"
       END IF 
      END DO
      IF (IRODDL.GT.0) THEN
       DO I=NODFT ,NODLT
        IF (WEIGHT(I).EQ.1) THEN
#include "lockon.inc"
         NORM2 = NORM2 + DDR(1,I)*DDR(1,I)
         NORM2 = NORM2 + DDR(2,I)*DDR(2,I)
         NORM2 = NORM2 + DDR(3,I)*DDR(3,I)
#include "lockoff.inc"
        END IF 
       END DO
      END IF 
!$OMP END PARALLEL 
C       
      IF (NSPMD.GT.1) THEN
        CALL SPMD_SUM_S(NORM2)
      END IF 
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  VSCAL_HP                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE VSCAL_HP(N , V   ,S    )
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
      INTEGER  N 
C     REAL
      my_real
     .  S, V(*)
C-----------------------------------------------
c PURPOSE:    V(*) <- S*V(*)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   N                 - dim. of V()
c  IO  V(*)               - V(*) scaled for output
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITSK,NFT,NLT
C-----------------------------
!$OMP PARALLEL PRIVATE(ITSK,NFT,NLT,I)
      CALL IMP_SMPINI(ITSK   ,NFT  ,NLT  ,N )
      DO I = NFT  ,NLT
       V(I) = S*V(I)
      END DO !I=NFT  ,NLT
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  VSCALY_HP                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE VSCALY_HP(N , V   ,Y   ,S    )
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
      INTEGER  N 
C     REAL
      my_real
     .  S, V(*),Y(*)
C-----------------------------------------------
c PURPOSE:    V(*) <- S*Y(*)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   N                 - dim. of V()
c  I  Y(*)               - Y(*) 
c  O  V(*)               - V(*) scaled for output
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITSK,NFT,NLT
C-----------------------------
!$OMP PARALLEL PRIVATE(ITSK,NFT,NLT,I)
      CALL IMP_SMPINI(ITSK   ,NFT  ,NLT  ,N )
      DO I = NFT  ,NLT
       V(I) = S*Y(I)
      END DO !I=NFT  ,NLT
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  VAXPY_HP                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE VAXPY_HP(N , V   ,Y   ,S    )
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
      INTEGER  N 
C     REAL
      my_real
     .  S, V(*),Y(*)
C-----------------------------------------------
c PURPOSE:    V(*) <- V(*)+S*Y(*)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   N                 - dim. of V()
c  I  Y(*)               - Y(*) 
c  O  V(*)               - V(*) scaled for output
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITSK,NFT,NLT
C-----------------------------
!$OMP PARALLEL PRIVATE(ITSK,NFT,NLT,I)
      CALL IMP_SMPINI(ITSK   ,NFT  ,NLT  ,N )
      DO I = NFT  ,NLT
       V(I) = V(I) + S*Y(I)
      END DO !I=NFT  ,NLT
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CP_REAL_HP                    source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_INTFR                     source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE CP_REAL_HP( N  ,X   ,XC)
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
      INTEGER  N  
C     REAL
      my_real
     .  X(*), XC(*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITSK,NFT,NLT
C-----------------------------
!$OMP PARALLEL PRIVATE(ITSK,NFT,NLT,I)
      CALL IMP_SMPINI(ITSK   ,NFT  ,NLT  ,N )
      DO I = NFT  ,NLT
       XC(I) = X(I)
      ENDDO
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CP_INT_HP                     source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE CP_INT_HP( N  ,X   ,XC)
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
      INTEGER  N ,X(*), XC(*) 
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITSK,NFT,NLT
C-----------------------------
!$OMP PARALLEL PRIVATE(ITSK,NFT,NLT,I)
      CALL IMP_SMPINI(ITSK   ,NFT  ,NLT  ,N )
      DO I = NFT  ,NLT
       XC(I) = X(I)
      ENDDO
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  ZEROR_HP                      source/implicit/produt_v.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE ZEROR_HP( X   ,N)
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
      INTEGER  N 
C     REAL
      my_real
     .      X(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITSK,NFT,NLT,N3
C-----------------------------
      N3=3*N
!$OMP PARALLEL PRIVATE(ITSK,NFT,NLT,I)
      CALL IMP_SMPINI(ITSK   ,NFT  ,NLT  ,N3 )
      DO I = NFT  ,NLT
       X(I) = ZERO
      ENDDO
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
