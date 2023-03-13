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
#ifdef MUMPS5
C---------------------solves [L][D][L]^t{z}={v}--DIAG_M(deja inverse)-----
C---------------------or  {z}=[Z][D]^-1[Z]^t{v}-----for inverse option IPREC>=6
Chd|====================================================================
Chd|  PREC_SOLV                     source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|        MAV_LT1                       source/implicit/produt_v.F    
Chd|        PREC0_SOLV                    source/implicit/prec_solv.F   
Chd|        PRECIC_SOLV                   source/implicit/prec_solv.F   
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE PREC_SOLV(IPREC,
     1                    IADK  ,JDIK  ,DIAG_K,LT_K  ,ITASK  ,
     2                    GRAPHE,ITAB  ,INSOLV,IT    ,FAC_K  ,
     3                    IPIV_K,NK    ,IDSC  ,ISOLV ,IPRINT ,
     4                    NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     5                    LT_M  ,V     ,Z     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*) ,JDIM(*),IPREC,ITASK,IPRINT
      INTEGER  IADK(*),JDIK(*),
     .         ISOLV ,ITAB(*), INSOLV,IT, IPIV_K(*), NK, IDSC
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
      my_real
     .  DIAG_K(*),LT_K(*),FAC_K(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c     iprec=1 => [I] 
c     iprec=2 => jacobien NNZ=0
c     iprec=3 => I.C.(0) :[LT_M]-->strict upper triangle [L]^t en c.r.s.
c     iprec=4 => I.C.(0)_Stab :item
c     iprec=5 => fsai .r same indice than [K]
c     iprec=12 => I.C.(J) :[LT_M]-->strict upper triangle [L]^t en c.r.s.
c     iprec=13 => ORTH    :[LT_M]-->strict upper triangle [L]^t en c.r.s.
c     iprec=14 => inv ORTH.C:[LT_M]-->strict upper triangle [Z] en c.c.s.
c     iprec=15 => inv ORTH.R:[LT_M]-->strict upper triangle [Z] en c.r.s.
c     iprec=16,19=>inv Approx.R:[LT_M]-->strict upper triangle [Z] en c.r.s.
c     iprec=20,23=>f.inv Approx.C:[LT_M]-->lower triangle [L] en c.r.s.
C-----------------------------
      INTEGER I,J,K,NI0,IBID,NNZK
      my_real
     .        RBID
C-----------------------------
      IF (IPREC==1) THEN
       IF (ISOLV>2) THEN
        NI0= 0
        NNZK = IADK(NDDL+1)-IADK(1)
#ifdef MUMPS5
        CALL LIN_SOLV2(
     1                    NDDL  ,NNZK  ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NI0   ,IBID  ,IBID  ,IBID   ,
     3                    RBID  ,Z     ,V     ,ITASK ,IPRINT ,
     4                    ISOLV ,IBID  ,GRAPHE,ITAB  ,INSOLV ,
     5                    IT    ,FAC_K ,IPIV_K,NK    ,RBID   ,
     6                    IDSC  )
#else
      WRITE(6,*) "Fatal error: MUMPS required"
      CALL FLUSH(6)
      CALL ARRET(5)
#endif
       ELSE
        DO I=1,NDDL
         Z(I)=V(I)
        ENDDO
       ENDIF
      ELSEIF (IPREC==5) THEN
       CALL PRECIC_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                  LT_M  ,V     ,Z     ) 
      ELSEIF (IPREC==14) THEN
       CALL PRECIC_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                  LT_M  ,V     ,Z     ) 
      ELSEIF (IPREC==15) THEN
       CALL PRECIC_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                  LT_M  ,V     ,Z     )
      ELSEIF (IPREC>=16.AND.IPREC<=19) THEN
       CALL MAV_LT1( NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     2               LT_M  ,V     ,Z     )
      ELSEIF (IPREC>=20.AND.IPREC<=23) THEN
       CALL PRECIC_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                  LT_M  ,V     ,Z     ) 
      ELSE 
       CALL PREC0_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                 LT_M  ,V     ,Z     )
      ENDIF
C--------------------------------------------
      RETURN
      END
C-----------spmd----------solves [L][D][L]^t{z}={v}--DIAG_M(deja inverse)-----
Chd|====================================================================
Chd|  PREC_SOLVP                    source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|        IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|-- calls ---------------
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|        PREC0_SOLV                    source/implicit/prec_solv.F   
Chd|        PRECIC_SOLV                   source/implicit/prec_solv.F   
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE PREC_SOLVP(IPREC, ITASK   ,
     1                      GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
     2                      IADK  ,JDIK    ,ITAB   ,IPRINT,INSOLV , 
     3                      IT    ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
     4                      CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
     5                      INLOC  ,NDOF   , NDDL  ,NNZ   ,IADM     ,
     6                      JDIM   ,DIAG_M , LT_M  ,V     ,Z     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*)  ,JDIM(*),IPREC, ITASK
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), IADK(*), JDIK(*), 
     .        ITAB(*), IPRINT, 
     .        INSOLV, IT, IPIV_K(*), NK, CDDLP(*), ISOLV, IDSC,
     .        IDDL(*), IKC(*), INLOC(*), NDOF(*)
      my_real DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
      my_real DIAG_K(*), LT_K(*),FAC_K(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c     iprec=1 => [I] 
c     iprec=2 => jacobien NNZ=0
c     iprec=5 => fsai .r same indice than [K]
C-----------------------------
      INTEGER I,J,K,IBID,NI0
      my_real
     .        RBID
C-----------------------------
      IF (IPREC==1) THEN
       IF (ISOLV>2) THEN
        NI0= 0
#ifdef MUMPS5
          CALL LIN_SOLVP2(GRAPHE, V     , NDDL , IAD_ELEM , FR_ELEM,
     1                    DIAG_K, LT_K  , IADK , JDIK     , Z      ,
     2                    ITAB  , IPRINT, NI0  , IBID     , IBID   ,
     3                    RBID  , RBID  , IBID , INSOLV   , IT     ,
     4                    FAC_K , IPIV_K, NK   , MUMPS_PAR, CDDLP  ,
     5                    ISOLV , IDSC  , IDDL , IKC      , INLOC  ,
     6                    NDOF  , ITASK )
#else 
      WRITE(6,*) "Fatal error: MUMPS required"
      CALL FLUSH(6)

#endif
       ELSE
        DO I=1,NDDL
         Z(I)=V(I)
        ENDDO
       ENDIF 
      ELSEIF (IPREC==5) THEN
       CALL PRECIC_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                  LT_M  ,V     ,Z     ) 
       CALL SPMD_SUMF_V(Z)
      ELSE 
       CALL PREC0_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                 LT_M  ,V     ,Z     )
       CALL SPMD_SUMF_V(Z)
      ENDIF
C
C--------------------------------------------
      RETURN
      END
C---------------------solves [L][D][L]^t{z}={v}--DIAG_M(deja inverse)-----
Chd|====================================================================
Chd|  PREC0_SOLV                    source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|        PREC_SOLV                     source/implicit/prec_solv.F   
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        PREC_SOLVP                    source/implicit/prec_solv.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PREC0_SOLV(
     1                    NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     2                    LT_M  ,V     ,Z     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*)  ,JDIM(*)
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C------------[LT_M]-->strict upper triangle--- 
      INTEGER I,J,K
C-----------------------------
      DO I=1,NDDL
       Z(I)=V(I)
      ENDDO
      IF (NNZ>0) THEN
C --------Forword---[LT_M]^t[D]{z}={v}----
       DO I=1,NDDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(K) = Z(K)-LT_M(J)*Z(I)
        ENDDO
        Z(I) = Z(I)*DIAG_M(I)
       ENDDO
C --------Backword----[LT_M]{z}={v}---     
       DO I=NDDL-1,1,-1
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(I) = Z(I)-LT_M(J)*Z(K)
        ENDDO
       ENDDO
      ELSE
       DO I=1,NDDL
        Z(I) = Z(I)*DIAG_M(I)
       ENDDO
      ENDIF
C--------------------------------------------
      RETURN
      END
C-------------solves  {z}=[Z][D]^-1[Z]^t{v}-----
Chd|====================================================================
Chd|  PRECIR_SOLV                   source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRECIR_SOLV(
     1                    NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     2                    LT_M  ,V     ,Z     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*)  ,JDIM(*),IPREC
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  TMP(NDDL) 
C--[LT_M]-->[Z]^t strict lower triangle c.c.s.(= transpose of strict upper tria c.r.s.)--- 
C--------- tmp est utilisee pour la raison //-------- 
       DO I=1,NDDL
        Z(I) = V(I)
       ENDDO
C--------{z}=[Z]^t{v}-------------
       DO J=1,NDDL
        DO I =IADM(J),IADM(J+1)-1
         K = JDIM(I)
         Z(K) = Z(K)+LT_M(I)*V(J)
        ENDDO
       ENDDO
C--------{z}=[D]^-1{v}-------------
       DO I=1,NDDL
        Z(I) = Z(I)*DIAG_M(I)
        TMP(I) = Z(I)
       ENDDO
C --------[Z]{z}-------     
       DO I=1,NDDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(I) = Z(I)+LT_M(J)*TMP(K)
        ENDDO
       ENDDO
C--------------------------------------------
      RETURN
      END
C-------------solves  {z}=[Z][D]^-1[Z]^t{v}----[Z] en colonne-
Chd|====================================================================
Chd|  PRECIC_SOLV                   source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|        PREC_SOLV                     source/implicit/prec_solv.F   
Chd|        PREC_SOLVP                    source/implicit/prec_solv.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRECIC_SOLV(
     1                    NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     2                    LT_M  ,V     ,Z     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*)  ,JDIM(*),IPREC
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  TMP(NDDL) 
C--[LT_M]-->[Z]^t strict lower triangle c.r.s.(= transpose of strict upper tria c.c.s.)--- 
C--------- tmp est utilisee pour la raison //-------- 
C-----------------------------
       DO I=1,NDDL
        Z(I) = V(I)
       ENDDO
C--------{z}=[Z]^t{v}-------------
       DO I=2,NDDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(I) = Z(I)+LT_M(J)*V(K)
        ENDDO
       ENDDO
C--------{z}=[D]^-1{z}-------------
       DO I=1,NDDL
        Z(I) = Z(I)*DIAG_M(I)
        TMP(I) = Z(I)
       ENDDO
C --------{z}=[Z]{z}-------     
       DO J = 2,NDDL
        DO I =IADM(J),IADM(J+1)-1
         K = JDIM(I)
         Z(K) = Z(K)+LT_M(I)*TMP(J)
        ENDDO
       ENDDO
C--------------------------------------------
      RETURN
      END
C------------hibrid version-solves  {z}=[Z][D]^-1[Z]^t{v}----[Z] en colonne-
Chd|====================================================================
Chd|  PREC5H_SOLV                   source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE PREC5H_SOLV(
     1                    NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     2                    LT_M  ,V     ,Z     ,F_DDL ,L_DDL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*)  ,JDIM(*),F_DDL ,L_DDL
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  TMP(NDDL) 
C--[LT_M]-->[Z]^t strict lower triangle c.r.s.(= transpose of strict upper tria c.c.s.)--- 
C--------- tmp est utilisee pour la raison //-------- 
C-----------------------------
       DO I=F_DDL ,L_DDL
        Z(I) = V(I)
       ENDDO
C-------------------
       DO I= 1 ,NDDL
        TMP(I) = ZERO
       ENDDO
C--------{z}=[Z]^t{v}-------------
       DO I=F_DDL ,L_DDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(I) = Z(I)+LT_M(J)*V(K)
        ENDDO
       ENDDO
C--------{z}=[D]^-1{z}-------------
       DO I=F_DDL ,L_DDL
        Z(I) = Z(I)*DIAG_M(I)
       ENDDO
C --------{z}=[Z]{z}-------     
       DO I = F_DDL ,L_DDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         TMP(K) = TMP(K)+LT_M(J)*Z(I)
        ENDDO
       ENDDO
C----------------------
      CALL MY_BARRIER
C---------------------
#include "lockon.inc"
       DO I= 1 ,NDDL
        Z(I) = Z(I) + TMP(I)
       ENDDO
#include "lockoff.inc"
C--------------------------------------------
      RETURN
      END
C------------hibrid version-solves  {z}=[Z][D]^-1[Z]^t{v}----[Z] en colonne-
Chd|====================================================================
Chd|  PREC5HC_SOLV                  source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|        PREC_SOLVGH                   source/implicit/prec_solv.F   
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PREC5HC_SOLV(
     1                    NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     2                    LT_M  ,V     ,Z     ,F_DDL ,L_DDL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*)  ,JDIM(*),F_DDL ,L_DDL
C     REAL
      my_real
     .  DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  TMP(NDDL) 
C--[LT_M]-->[Z]^t ,  [LT_M0] ->[Z]----------
C-----------------------------
       DO I=F_DDL ,L_DDL
        Z(I) = V(I)
       ENDDO
C--------{z}=[Z]^t{v}-------------
       DO I=F_DDL ,L_DDL
        DO J =IADM(I),IADM(I+1)-1
         K = JDIM(J)
         Z(I) = Z(I)+LT_M(J)*V(K)
        ENDDO
       ENDDO
C--------{z}=[D]^-1{z}-------------
       DO I=F_DDL ,L_DDL
        Z(I) = Z(I)*DIAG_M(I)
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
        DO J =IADM0(I),IADM0(I+1)-1
         K = JDIM0(J)
         Z(I) = Z(I)+LT_M0(J)*TMP(K)
        ENDDO
       ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  PREC2H_SOLV                   source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|        PREC_SOLVGH                   source/implicit/prec_solv.F   
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PREC2H_SOLV(
     1                      F_DDL ,L_DDL ,DIAG_M ,V     ,Z    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  F_DDL ,L_DDL  
C     REAL
      my_real
     .  DIAG_M(*), Z(*) ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
       DO I=F_DDL ,L_DDL
        Z(I) = V(I)*DIAG_M(I)
       ENDDO
C--------------------------------------------
      RETURN
      END
C-----------spmd----------solves [L][D][L]^t{z}={v}--DIAG_M(deja inverse)-----
Chd|====================================================================
Chd|  PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PREC0_SOLV                    source/implicit/prec_solv.F   
Chd|        PREC2H_SOLV                   source/implicit/prec_solv.F   
Chd|        PREC5HC_SOLV                  source/implicit/prec_solv.F   
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE PREC_SOLVH(IPREC, ITASK   ,
     1                      GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
     2                      IADK  ,JDIK    ,ITAB   ,IPRINT,INSOLV , 
     3                      IT    ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
     4                      CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
     5                      INLOC  ,NDOF   , NDDL  ,NNZ   ,IADM     ,
     6                      JDIM   ,DIAG_M , LT_M  ,V     ,Z        ,
     7                      F_DDL  ,L_DDL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "timeri_c.inc"
#include "dmumps_struc.h"
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ   ,IADM(*)  ,JDIM(*),IPREC, ITASK
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), IADK(*), JDIK(*), 
     .        ITAB(*), IPRINT, 
     .        INSOLV, IT, IPIV_K(*), NK, CDDLP(*), ISOLV, IDSC,
     .        IDDL(*), IKC(*), INLOC(*), NDOF(*),F_DDL  ,L_DDL
      my_real DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
      my_real DIAG_K(*), LT_K(*),FAC_K(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c     iprec=1 => [I] 
c     iprec=2 => jacobien NNZ=0
c     iprec=5 => fsai .r same indice than [K]
C-----------------------------
      INTEGER I,J,K,IBID,NI0,NNZK
      my_real RBID
C-----------------------------
      IF (IPREC==1) THEN
       IF (ISOLV>2.AND.ISOLV<7) THEN
#ifdef MUMPS5
        IF (ITASK==0) THEN
         IF (NSPMD>1) THEN

          NI0= 0
          CALL LIN_SOLVP2(GRAPHE, V     , NDDL , IAD_ELEM , FR_ELEM,
     1                    DIAG_K, LT_K  , IADK , JDIK     , Z      ,
     2                    ITAB  , IPRINT, NI0  , IBID     , IBID   ,
     3                    RBID  , RBID  , IBID , INSOLV   , IT     ,
     4                    FAC_K , IPIV_K, NK   , MUMPS_PAR, CDDLP  ,
     5                    ISOLV , IDSC  , IDDL , IKC      , INLOC  ,
     6                    NDOF  , ITASK )
         ELSE
          NI0= 0
          NNZK = IADK(NDDL+1)-IADK(1)
          CALL LIN_SOLV2(
     1                    NDDL  ,NNZK  ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NI0   ,IBID  ,IBID  ,IBID   ,
     3                    RBID  ,Z     ,V     ,ITASK ,IPRINT ,
     4                    ISOLV ,IBID  ,GRAPHE,ITAB  ,INSOLV ,
     5                    IT    ,FAC_K ,IPIV_K,NK    ,RBID   ,
     6                    IDSC  )
         END IF !(NSPMD>1) THEN
        END IF 
#else
      WRITE(6,*) "Fatal error: MUMPS required"
      CALL FLUSH(6)
#endif

C----------------------
      CALL MY_BARRIER
C---------------------
       ELSE
        DO I = F_DDL  ,L_DDL
         Z(I)=V(I)
        ENDDO
       ENDIF 
      ELSEIF (IPREC==2) THEN
       CALL PREC2H_SOLV(
     1                  F_DDL ,L_DDL ,DIAG_M ,V     ,Z     )
      ELSEIF (IPREC==5) THEN
       CALL PREC5HC_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                   LT_M  ,V     ,Z     ,F_DDL ,L_DDL ) 
      ELSE 
       IF (ITASK==0) THEN
        CALL PREC0_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                 LT_M  ,V     ,Z     )
       ENDIF 
      ENDIF
C
      IF (IPREC>1) THEN
C----------------------
      CALL MY_BARRIER
       IF (ITASK==0.AND.NSPMD>1) THEN
         IF(IMONM > 0) CALL STARTIME(66,1)
         CALL SPMD_SUMF_V(Z)
         IF(IMONM > 0) CALL STOPTIME(66,1)
       END IF
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  PREC_SOLVGH                   source/implicit/prec_solv.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PREC2H_SOLV                   source/implicit/prec_solv.F   
Chd|        PREC5HC_SOLV                  source/implicit/prec_solv.F   
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE PREC_SOLVGH(IPREC, ITASK   ,NDDL  ,IADM  ,JDIM   ,
     6                      DIAG_M , LT_M  ,V     ,Z      ,F_DDL  ,
     7                      L_DDL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADM(*)  ,JDIM(*),IPREC, ITASK,
     .         F_DDL  ,L_DDL
      my_real DIAG_M(*), Z(*), LT_M(*)  ,V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c     iprec=1 => [I] 
c     iprec=2 => jacobien NNZ=0
c     iprec=5 => fsai .r same indice than [K]
C-----------------------------
      INTEGER I,J,K,IBID,NI0,NNZ
      my_real RBID
C-----------------------------
      IF (IPREC==1) THEN
        DO I = F_DDL  ,L_DDL
         Z(I)=V(I)
        ENDDO
      ELSEIF (IPREC==2) THEN
       CALL PREC2H_SOLV(
     1                  F_DDL ,L_DDL ,DIAG_M ,V     ,Z     )
      ELSEIF (IPREC==5) THEN
       NNZ=IADM(NDDL+1)-IADM(1)
       CALL PREC5HC_SOLV(NDDL  ,NNZ   ,IADM  ,JDIM  ,DIAG_M ,   
     1                   LT_M  ,V     ,Z     ,F_DDL ,L_DDL ) 
      ENDIF
C
      IF (IPREC>1) THEN
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK==0.AND.NSPMD>1) CALL SPMD_SUMF_V(Z)
      ENDIF
C--------------------------------------------
      RETURN
      END
#endif
