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
C   ----  nouvelle routine---------
Chd|====================================================================
Chd|  UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        BC_IMP0                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMP1                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPA                       source/constraints/general/bcs/bc_imp0.F
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        CONDENS_K                     source/implicit/upd_glob_k.F  
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_ALLO                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMP0                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMPL                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RW0                        source/constraints/general/impvel/fv_imp0.F
Chd|        I2_IMP0                       source/interfaces/interf/i2_imp0.F
Chd|        INI_DOFSPC                    source/implicit/upd_glob_k.F  
Chd|        RBE2_IMP0                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMP0                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMP0                      source/constraints/general/rbody/rby_imp0.F
Chd|        RM_IMP0                       source/model/remesh/rm_imp0.F 
Chd|        UPD_ASPC                      source/constraints/general/bcs/bc_imp0.F
Chd|        IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPD_GLOB_K(ICODT ,ICODR ,ISKEW ,IBFV    ,NPC    ,
     1                      TF    ,VEL   ,XFRAME ,
     2                      RBY   ,X     ,SKEW   ,LPBY   ,NPBY   ,
     3                      ITAB  ,WEIGHT,MS     ,IN     ,NRBYAC ,
     4                      IRBYAC,NSC   ,ISIJ   ,NMC    ,IMIJ   ,
     5                      NSS   ,ISS   ,NINT2  ,IINT2  ,NSC2   ,
     6                      ISIJ2 ,NSS2  ,ISS2   ,IPARI  ,INTBUF_TAB,
     7                      NDDL  ,NNZ    ,IADK   ,JDIK   ,
     8                      DIAG_K,LT_K  ,NDOF   ,IDDL   ,IKC    ,
     9                      UD    ,B     ,NKUD   ,IKUD   ,BKUD   ,
     A                      NMC2  ,IMIJ2 ,NT_RW  ,RD     ,LJ     ,
     B                      IRBE3 ,LRBE3 ,FRBE3  ,ISS3   ,IRBE2  ,
     C                      LRBE2 ,ISB2  ,NSRB2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FVBCL
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IBFV(NIFV,*),
     .        ICODT(*),ICODR(*),ISKEW(*),NSC(*),ISIJ(*),
     .        NMC,IMIJ(*),NSS(*),ISS(*),NINT2 ,IINT2(*),NT_RW,
     .        NSC2(*),ISIJ2(*),NSS2(*),ISS2(*),NKUD(*) ,IKUD(*),
     .        NMC2,IMIJ2(*),LJ(*)
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ITAB(*),
     .        IPARI(NPARI,*), NRBYAC,IRBYAC(*),
     .        NDDL,NNZ,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*),
     .        IRBE3(*) ,LRBE3(*),ISS3(*),IRBE2(*),LRBE2(*),
     .        ISB2(*),NSRB2(*)
      my_real
     .   RBY(NRBY,*) ,X(3,*) ,SKEW(*),IN(*),MS(*)
      my_real
     .  TF(*),VEL(LFXVELR,*),UD(3,*),DIAG_K(*),LT_K(*),
     .  B(*) ,BKUD(*),RD(3,*),XFRAME(NXFRAME,*),FRBE3(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DOFSPC
C----IKC(NDDL) : 1->fix_global,2->Ud_global,3,4->fv_rw,5->Int2_0,6->Int2_1,7->RB-
C----a faire (8->fix_local,9->Ud_local)-----10,11->sliding rw---12->remesh,13->RBE3,14->AUTO-SPC
C----15->AUTO-SPC-in local sys,16->RBE2,
C----No ddl -> IDDL(NDDL)+NDOF(NUMNOD)-MIN(IKC,1)---
      IF(NINT2>0)THEN
       CALL I2_IMP0(NINT2 ,IINT2 ,
     1              IPARI,INTBUF_TAB,X,MS   ,IN   ,
     1              NMC2  ,IMIJ2 ,ITAB   ,
     2              NSC2   ,ISIJ2 ,NSS2  ,ISS2,
     3              WEIGHT,IKC ,NDOF ,NDDL,IDDL    ,
     4              IADK  ,JDIK   ,DIAG_K ,LT_K  ,B      )
      ENDIF
c
      IF(NRBE2>0)THEN
       CALL RBE2_IMP0(
     1                IRBE2  ,LRBE2  ,X      ,NSRB2  ,ISB2   ,
     2                IKC    ,NDOF   ,IDDL   ,IADK   ,JDIK   ,
     3                DIAG_K ,LT_K   ,B      ,WEIGHT ,ITAB   ,
     4                SKEW   )
      ENDIF
      IF(NRBE3>0)THEN
       CALL RBE3_IMP0(
     1              IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW    ,
     2              ISS3   ,IKC   ,NDOF   ,IDDL  ,IADK    ,
     3              JDIK   ,DIAG_K,LT_K   ,B     ,WEIGHT  ,
     4              ITAB  )
      ENDIF
      IF(NRBYAC>0)THEN
       CALL RBY_IMP0(X     ,RBY   ,LPBY  ,NPBY  ,SKEW  ,
     1               NRBYAC,IRBYAC,NSC   ,ISIJ  ,NMC   ,
     2               IMIJ  ,NSS   ,ISS   ,ISKEW ,ITAB  ,
     3               WEIGHT,MS    ,IN    ,
     4               NDDL  ,IADK  ,JDIK  ,DIAG_K,
     5               LT_K  ,NDOF  ,IDDL  ,IKC   ,B     )
      ENDIF
      IF(NADMESH/=0)THEN
            CALL RM_IMP0(
     4                   NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     5                   NDOF  ,IDDL  ,IKC   ,B      ,ITAB  )
      ENDIF
C -----enforce b.c. FV va tenir compte b.c.---------------------
      CALL BC_IMP0(ICODT ,ICODR,ISKEW,IKC,NDOF,IDDL)
C-----case coupling FV+BCS using SKEW ----
      NFVBCL=0
      IF(NFXVEL > 0) THEN
       CALL DIM_FVBCL(IBFV   ,LJ    ,ISKEW  ,ICODT ,ICODR  ,
     1                NDDL   ,IDDL  ,IKC    ,IADK  ,JDIK   ,
     2                SKEW   ,NFVBCL,NKUD_L )
c       NFVBCL=0
       IF(NFVBCL > 0 ) THEN
        CALL FVBC_ALLO()
        CALL FVBC_IMPL(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1                 IKC    ,NDOF  ,IADK   ,JDIK ,DIAG_K ,
     2                 LT_K   ,UD    ,RD     ,B    ,NDDL   ,
     3                 ICODT  ,ICODR ,ICT_1  ,ICR_1,NKUD_1,
     4                 IKUD_1 ,BKUD_1 )
       END IF!(NFVBCL > 0) THEN
      ENDIF
C
      IF(NFVBCL > 0) THEN
      CALL BC_IMP1(ICT_1 ,ICR_1 ,ISKEW ,SKEW  ,IKC   ,
     1             NDOF  ,IDDL  ,IADK  ,JDIK  ,DIAG_K,
     2             LT_K  )
      ELSE
      CALL BC_IMP1(ICODT ,ICODR ,ISKEW ,SKEW  ,IKC   ,
     1             NDOF  ,IDDL  ,IADK  ,JDIK  ,DIAG_K,
     2             LT_K  )
      END IF
      IF(NFXVEL>0) THEN
       CALL FV_IMPL(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1              IKC    ,NDOF  ,IADK   ,JDIK ,DIAG_K ,
     2              LT_K   ,UD    ,RD     ,B    )
       CALL FV_IMP0(IDDL   ,IKC   ,NDOF  ,IADK  ,JDIK   ,
     1              DIAG_K ,LT_K  ,UD    ,NKUD  ,IKUD   ,
     2              BKUD   ,NDDL  ,RD    )
      ENDIF
C
      IF(NT_RW>0) THEN
       CALL FV_RW0(IDDL   ,IKC   ,NDOF  ,IADK  ,JDIK ,
     1             DIAG_K ,LT_K  ,UD    ,B     )
      ENDIF
      IF(IAUTSPC>0) THEN
       ALLOCATE(DOFSPC(NUMNOD))
       CALL INI_DOFSPC(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,NDOF      ,IRBE3     ,
     3    LRBE3     ,IRBE2     ,LRBE2     ,X         ,DOFSPC    )
       DO I=1,NDDL
        IF (IKC(I)==14.OR.IKC(I)==15) IKC(I)=0
       END DO
       CALL UPD_ASPC(NDDL  ,DOFSPC,IDDL  ,IKC    ,ITAB  ,
     .               IADK  ,JDIK  ,DIAG_K,LT_K   )
       CALL BC_IMPA(IADK  ,JDIK   ,DIAG_K,LT_K  ,NDOF  ,
     1              IDDL  ,IKC    )
       DEALLOCATE(DOFSPC)
      ENDIF
C -----update---condense-IKC>=1------------------------------------
      CALL CONDENS_B(NDDL  ,IKC   ,DIAG_K)
      CALL CONDENS_K(NDDL  ,NNZ   ,IADK   ,JDIK   ,LT_K  ,
     1               IKC   )
C
      RETURN
      END
C   ----  nouvelle routine---------
Chd|====================================================================
Chd|  CONDENS_K                     source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        CONDENS_K0                    source/implicit/upd_glob_k.F  
Chd|====================================================================
      SUBROUTINE CONDENS_K(
     1                      NDDL  ,NNZ   ,IADK   ,JDIK   ,
     2                      LT_K  ,IKC   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,NNZ,IADK(*),JDIK(*),IKC(*)
      my_real
     .  LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NL,ND,IFIX(NDDL),NFT,JLT,NZF,NZL
C -----update---condense-IKC>=1------------------------------------
      ND = NDDL
      NDDL=0
      NNZ=0
      NL = 0
C
      DO NFT = 0 , ND-1 , NNSIZ
       JLT = MIN( NNSIZ, ND - NFT )
        NZF=IADK(NFT+1)-1
        NZL=IADK(JLT+NFT+1)-IADK(NFT+1)
        CALL CONDENS_K0(
     1                      NDDL  ,NNZ   ,IADK   ,JDIK   ,NL    ,
     2                      LT_K  ,IKC   ,IFIX   ,NFT    ,JLT   ,
     3                      NZF   ,NZL   )
      ENDDO
      IADK(NDDL+1)=NNZ+1
C
      DO J = 1,NNZ
        K= JDIK(J)
        JDIK(J) = JDIK(J) -IFIX(K)
      ENDDO
C
      RETURN
      END
C   ----  nouvelle routine---------
Chd|====================================================================
Chd|  CONDENS_K0                    source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        CONDENS_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CONDENS_K0(
     1                      NDDL  ,NNZ   ,IADK   ,JDIK   ,NL    ,
     2                      LT_K  ,IKC   ,IFIX   ,NDF    ,NDL   ,
     3                      NZF   ,NZL    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,NNZ,IADK(*),JDIK(*),IKC(*),IFIX(*),NL,
     .        NDF,NDL,NZF,NZL
      my_real
     .  LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADK1(NDL+1),JDIK1(NZL)
      INTEGER I,J,K,I1,J1,ND,NI,JI
      my_real
     .  LT_K1(NZL)
C -----update---condense-IKC>=1------------------------------------
        DO NI = 1,NDL+1
         I=NI+NDF
         IADK1(NI)=IADK(I)
        ENDDO
        DO NI = 1,NZL
         I=NI+NZF
         JDIK1(NI)=JDIK(I)
         LT_K1(NI)=LT_K(I)
        ENDDO
C
       DO NI = 1,NDL
        I=NI+NDF
        IFIX(I) = NL
        IF (IKC(I)<1) THEN
         NDDL=NDDL+1
         IADK(NDDL)=NNZ+1
         DO J = IADK1(NI),IADK1(NI+1)-1
          JI=J-NZF
          K= JDIK1(JI)
          IF (IKC(K)<1) THEN
           NNZ=NNZ+1
           JDIK(NNZ)=K
           LT_K(NNZ)=LT_K1(JI)
          ENDIF
         ENDDO
        ELSE
         NL=NL+1
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CONDENS_IND                   source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CONDENS_IND(NDDL  ,NNZ   ,IADK  ,JDIK   ,IKC   )
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
      INTEGER NDDL,NNZ,IADK(*),JDIK(*),IKC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADK1(NDDL+1),JDIK1(NNZ),IFIX(NDDL)
      INTEGER I,J,K,I1,J1,ND,NI,JI,NL
C -----update---condense-IKC>=1------------------------------------
       IF (NDDL==0) RETURN
        DO I = 1,NDDL+1
         IADK1(I)=IADK(I)
        ENDDO
        DO NI = 1,NNZ
         JDIK1(NI)=JDIK(NI)
        ENDDO
C
       ND = 0
       NNZ = 0
       NL = 0
       DO I = 1,NDDL
        IFIX(I) = NL
        IF (IKC(I)<1) THEN
         ND=ND+1
         IADK(ND)=NNZ+1
         DO J = IADK1(I),IADK1(I+1)-1
          K= JDIK1(J)
          IF (IKC(K)<1) THEN
           NNZ=NNZ+1
           JDIK(NNZ)=K
          ENDIF
         ENDDO
        ELSE
         NL=NL+1
        ENDIF
       ENDDO
       IADK(ND+1)=NNZ+1
       NDDL=ND
C
      DO J = 1,NNZ
        K= JDIK(J)
        JDIK(J) = JDIK(J) -IFIX(K)
      ENDDO
C
      RETURN
      END
C   ----  nouvelle routine---NDDL n'est pas modifie------
Chd|====================================================================
Chd|  CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        D_TO_U                        source/implicit/produt_v.F    
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        GET_FEXT                      source/implicit/imp_solv.F    
Chd|        IMP_FHHT1                     source/implicit/imp_dyna.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        PRODUT_U                      source/implicit/produt_v.F    
Chd|        PRODUT_U2                     source/implicit/produt_v.F    
Chd|        PRODUT_UH                     source/implicit/produt_v.F    
Chd|        PRODUT_UH2                    source/implicit/produt_v.F    
Chd|        PRODUT_UHP                    source/implicit/produt_v.F    
Chd|        PRODUT_UHP2                   source/implicit/produt_v.F    
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CONDENS_B(NDDL  ,IKC   ,B   )
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
      INTEGER NDDL,IKC(*)
      my_real
     .  B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND
      my_real
     .  B1(NDDL)
C -----update---condense-IKC>=1------------------------------------
      DO I = 1,NDDL
       B1(I)=B(I)
      ENDDO
C
      ND=0
C
      DO I = 1,NDDL
       IF (IKC(I)<1) THEN
        ND=ND+1
        B(ND)=B1(I)
       ENDIF
      ENDDO
C
      RETURN
      END
C   ----  nouvelle routine---------
Chd|====================================================================
Chd|  UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        BC_IMP0                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMP1                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPA                       source/constraints/general/bcs/bc_imp0.F
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        CONDENS_K                     source/implicit/upd_glob_k.F  
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        FV_IMPI                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMPL                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RW0                        source/constraints/general/impvel/fv_imp0.F
Chd|        I2_IMPI                       source/interfaces/interf/i2_imp0.F
Chd|        RBE2_IMPI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMPI                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMPI                      source/constraints/general/rbody/rby_imp0.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPD_INT_K(ICODT  ,ICODR ,ISKEW ,IBFV    ,NPC    ,
     1                      TF    ,VEL   ,XFRAME ,
     2                      RBY   ,X     ,SKEW   ,LPBY   ,NPBY  ,
     3                      ITAB  ,WEIGHT,MS     ,IN     ,NRBYAC,
     4                      IRBYAC,NSS   ,ISS    ,IPARI  ,INTBUF_TAB,
     5                      NINT2 ,IINT2  ,IAINT2 ,NSS2  ,
     5                      ISS2  ,NDDLI ,NNZI   ,IADI  ,JDII   ,
     6                      DIAG_I ,LT_I  ,IDDLI ,NDDL  ,IADK   ,
     7                      JDIK   ,IKC   ,DIAG_K,LT_K  ,IDDL   ,
     8                      NDOFI  ,ITOK  ,UD    ,LB    ,LUJ    ,
     9                      NT_RW  ,IRBE3 ,LRBE3 ,FRBE3 ,NSS3   ,
     A                      ISS3   ,IRBE2 ,LRBE2 ,NSB2  ,ISB2   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IBFV(NIFV,*),LUJ(*),
     .        ICODT(*),ICODR(*),ISKEW(*),ITOK(*),NDDL,NT_RW
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ITAB(*),
     .        IPARI(NPARI,*), NRBYAC,IRBYAC(*),
     .        IDDL(*),IKC(*),NSS(*),ISS(*),NSS2(*),ISS2(*),
     .        IADK(*),JDIK(*),NDDLI,NNZI,IADI(*),JDII(*),
     .        IDDLI(*),NDOFI(*),NINT2 ,IINT2(*),IAINT2(*)
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),NSS3(*),ISS3(*),
     .        IRBE2(*),LRBE2(*),NSB2(*),ISB2(*)
      my_real
     .   RBY(NRBY,*) ,X(3,*) ,SKEW(*),IN(*),MS(*)
      my_real
     .  TF(*), VEL(LFXVELR,*),DIAG_K(*),LT_K(*),
     .  DIAG_I(*),LT_I(*),LB(*),UD(3,*),XFRAME(NXFRAME,*),FRBE3(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LUJI(NFXVEL),IBID
      INTEGER I,J,N, IAD,NTY,NDDL0,IIC(NDDLI),ND,
     .        ITOK1(NDDLI),IDEN(NDDL),NNZK0,L1,L2,K,IKP,NN
C     REAL
      my_real
     .   LBI(NDDLI),RBID,LB0(NDDLI)
C--------diak=0->IIC=10--------------------------------------
C
      DO I=1,NDDLI
       IAD=ITOK(I)
       IIC(I)=0
       IF (IKC(IAD)==2.OR.IKC(IAD)==9) IIC(I)=IKC(IAD)
       IF (IKC(IAD)==3.OR.IKC(IAD)==4.OR.
     .       IKC(IAD)==10.OR.IKC(IAD)==11) IIC(I)=IKC(IAD)
       IF (IKC(IAD)==14.OR.IKC(IAD)==15) IIC(I)=IKC(IAD)
       IF (IKC(IAD)<1) THEN
        LBI(I)=LB(IAD)
       ELSE
        LBI(I)=ZERO
       ENDIF
       ITOK1(I)=IAD
      ENDDO
      IF(NINT2>0.OR.NRBYAC>0.OR.NRBE3>0)
     .    CALL CP_REAL(NDDLI,LBI,LB0)
      NDDL0=NDDLI
      NNZK0=NNZI
C -----update---due to kinematic conditions-----------------------------------
C----IKC(NDDL) : 1->fix_global,2->Ud_global,3->fix_local,4->Ud_local,5->Int2_0,6->Int2_1,7->RB------
C-----l'interface 2 ------
      IF(NINT2>0)THEN
       CALL I2_IMPI(NINT2  ,IINT2 ,IPARI ,INTBUF_TAB ,
     1              X      ,MS    ,IN    ,NSS2  ,ISS2  ,
     2              WEIGHT ,IIC   ,NDOFI ,NDDLI ,IDDLI ,
     3              IADI   ,JDII  ,DIAG_I,LT_I  ,IAINT2,
     4              LB0    ,ITAB  )
      ENDIF
      IF(NRBE2>0)THEN
       CALL RBE2_IMPI(
     1                IRBE2  ,LRBE2 ,X     ,SKEW   ,
     2                NSB2   ,ISB2  ,IIC   ,NDOFI  ,IDDLI  ,
     3                IADI   ,JDII  ,DIAG_I ,LT_I  ,LB0    ,
     4                WEIGHT ,ITAB  )
      ENDIF
C-----rbe3 ------
      IF(NRBE3>0)THEN
       CALL RBE3_IMPI(
     1              IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW    ,
     2              NSS3   ,ISS3  ,IIC   ,NDOFI  ,IDDLI   ,
     3              IADI   ,JDII  ,DIAG_I,LT_I   ,LB0     ,
     4              WEIGHT ,ITAB  )
      ENDIF
C-----RIGID BODIES-------
      IF(NRBYAC>0)THEN
       CALL RBY_IMPI(X     ,RBY   ,LPBY  ,NPBY  ,SKEW  ,
     1               NRBYAC,IRBYAC,NSS   ,ISS   ,ISKEW ,
     2               ITAB  ,WEIGHT,MS    ,IN    ,
     3               NDDLI ,IADI ,JDII   ,DIAG_I,
     4               LT_I  ,NDOFI ,IDDLI ,IIC   ,LB0   )
      ENDIF
C------------
      CALL BC_IMP0(ICODT ,ICODR,ISKEW,IIC,NDOFI,IDDLI)
      CALL BC_IMP1(ICODT ,ICODR ,ISKEW ,SKEW  ,IIC   ,
     1             NDOFI ,IDDLI ,IADI  ,JDII  ,DIAG_I,
     2             LT_I  )
      IF(IAUTSPC>0) THEN
       CALL BC_IMPA(IADI  ,JDII   ,DIAG_I,LT_I  ,NDOFI ,
     1              IDDLI ,IIC    )
      ENDIF
      IF(NFXVEL>0) THEN
        DO I=1,NFXVEL
         N=IABS(IBFV(1,I))
         IF (NDOFI(N)>0.AND.LUJ(I)<=3) THEN
          LUJI(I) = LUJ(I)
         ELSE
          LUJI(I) = 0
         ENDIF
        ENDDO
        CALL FV_IMPL(IBFV   ,SKEW  ,XFRAME ,LUJI ,IDDLI  ,
     1               IIC    ,NDOFI ,IADI   ,JDII ,DIAG_I ,
     2               LT_I   ,UD    ,RBID   ,LBI  )
        CALL FV_IMPI(IDDLI  ,IIC   ,NDOFI ,IADI  ,JDII   ,
     1               DIAG_I ,LT_I  ,UD    ,LBI   ,NDDLI  )
      ENDIF
      IF(NT_RW>0) THEN
       CALL FV_RW0(IDDLI  ,IIC   ,NDOFI ,IADI  ,JDII ,
     1             DIAG_I ,LT_I  ,UD    ,LBI   )
      ENDIF
C -----diak=0------------------------------------
      IF (INTP_C>=0) THEN
      DO I=1,NDDLI
      IF (IIC(I)<1.AND.DIAG_I(I)<=EM20) IIC(I)=10
      ENDDO
      ENDIF
C -----update---LB------------------------------------
      DO I=1,NDDLI
       IAD=ITOK(I)
       IF (IIC(I)<1) LB(IAD)=LBI(I)
      ENDDO
C -----update---condense-IKC>=1------------------------------------
      CALL CONDENS_B(NDDLI ,IIC    ,DIAG_I)
      CALL CONDENS_K(NDDLI ,NNZI   ,IADI   ,JDII   ,LT_I  ,IIC   )
C -----update---ITOK------------------------------------
      ND=0
      DO I = 1,NDDL
       IF (IKC(I)<1) THEN
        ND=ND+1
        IDEN(I)=ND
       ELSE
        IDEN(I)=-IKC(I)
       ENDIF
      ENDDO
C
      ND=0
      DO I = 1,NDDL0
       IF (IIC(I)<1) THEN
        ND=ND+1
        L1=ITOK1(I)
        L2=IDEN(L1)
        IF (L2<=0)WRITE(*,*)'---ERROR ITOK:---',I,L1,L2
        ITOK(ND)=L2
       ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        FV_IMPRL                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RWLR0                      source/constraints/general/rwall/srw_imp.F
Chd|        I2_IMPR1                      source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPR2                      source/interfaces/interf/i2_imp1.F
Chd|        IMP_DYCRB                     source/implicit/imp_dyna.F    
Chd|        IMP_DYNAR                     source/implicit/imp_dyna.F    
Chd|        IMP_FHHT                      source/implicit/imp_dyna.F    
Chd|        IMP_FHHT1                     source/implicit/imp_dyna.F    
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        IMP_SETBA                     source/implicit/imp_setb.F    
Chd|        IMP_SETBP                     source/implicit/imp_setb.F    
Chd|        RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMPR1                     source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPR2                     source/constraints/general/rbody/rby_imp0.F
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPD_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1                   RBY   ,X     ,SKEW   ,LPBY   ,NPBY   ,
     2                   NRBYAC,IRBYAC,NINT2  ,IINT2  ,IPARI  ,
     3                   INTBUF_TAB   ,NDOF   ,IDDL   ,IKC    ,
     4                   NDDL0 ,B     ,IUPD   ,INLOC  ,LJ     ,
     5                   A     ,AR    ,AC     ,ACR    ,NT_RW  ,
     6                   IRFLAG,W_DDL ,NDDL   ,R02    ,IDYNA  ,
     7                   V     ,VR    ,MS     ,IN     ,IRBE3  ,
     8                   LRBE3 ,FRBE3 ,WEIGHT ,IRBE2  ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),ICODT(*),ICODR(*),ISKEW(*),
     .        NINT2 ,IINT2(*),LJ(*),NDDL0,IUPD,
     .        INLOC(*),NT_RW,IRFLAG,W_DDL(*) ,NDDL,IDYNA
      INTEGER LPBY(*),NPBY(NNPBY,*),NDOF(*),IDDL(*),IKC(*),
     .        IPARI(NPARI,*), NRBYAC,IRBYAC(*)
      INTEGER WEIGHT(*),IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
      my_real
     .   RBY(NRBY,*) ,X(3,*) ,SKEW(*),R02
      my_real
     .  B(*) ,XFRAME(NXFRAME,*),A(3,*),AR(3,*),AC(3,*),ACR(3,*),
     .  V(3,*),VR(3,*),MS(*),IN(*),FRBE3(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,JI,JB,K1,IFLAG
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: MA,INA
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: B0
C-------int2,RBE3,rby speciale (Fext seulement)----------
        IF (IUPD==0) THEN
         DO I=1,NINT2
          N=IINT2(I)
          CALL I2_IMPR1(IPARI(1,N),INTBUF_TAB(N) ,
     .                  X  ,NDOF ,IDDL    ,B  )
         ENDDO
         IF (NRBE2>0) THEN
          CALL RBE2_IMPR1(
     1                    IRBE2  ,LRBE2 ,X     ,SKEW   ,NDOF   ,
     2                    IDDL   ,B     ,WEIGHT)
         ENDIF
         IF (NRBE3>0) THEN
          CALL RBE3_IMPR1(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,B      ,WEIGHT)
         ENDIF
         DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMPR1(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                   NDOF  ,IDDL   ,B    )
         ENDDO
        ENDIF
C-------int2,rby speciale (elems deleted)----------
         DO I=1,NINT2
          N=IINT2(I)
          CALL I2_IMPR2(IPARI(1,N),INTBUF_TAB(N) ,AC    ,ACR  ,
     .                  X  ,NDOF ,IDDL    ,B  )
         ENDDO
         IF (NRBE3>0) THEN
          CALL RBE3_IMPR2(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,B      ,WEIGHT,AC     ,
     3                    ACR    )
         ENDIF
         DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMPR2(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                   NDOF  ,IDDL   ,B    ,AC    ,ACR  )
         ENDDO
        IF (IDYNA>0) THEN
          ALLOCATE(MA(3,NUMNOD))
          IF (IRODDL/=0) ALLOCATE(INA(3,NUMNOD))
          CALL IMP_DYNAR(MA   ,INA   ,MS   ,IN     ,A    ,AR    ,
     .                   V    ,VR    )
         DO I=1,NRBYAC
          N=IRBYAC(I)
          CALL IMP_DYCRB(INA   ,IN     ,VR     ,NPBY(1,N),RBY(1,N),
     .                   WEIGHT,ICODR  ,ISKEW  ,SKEW   )
         ENDDO
        ENDIF
        IF (IRFLAG>0) THEN
         CALL EXT_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1                X     ,SKEW  ,NDOF  ,IDDL    ,IKC    ,
     2                NDDL0 ,B     ,INLOC  ,LJ     ,AC     ,
     3                ACR   ,NT_RW ,W_DDL  ,NDDL   ,R02    )
         ALLOCATE(B0(NDDL0))
         CALL IMP_SETB(A    ,AR    ,IDDL  ,NDOF    ,B0     )
         CALL EXT_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1                X     ,SKEW  ,NDOF  ,IDDL    ,IKC    ,
     2                NDDL0 ,B0    ,INLOC  ,LJ     ,A      ,
     3                AR    ,NT_RW ,W_DDL  ,NDDL   ,R02    )
         IF (IDYNA>0) THEN
         CALL IMP_SETB(MA   ,INA   ,IDDL  ,NDOF    ,B0     )
         CALL EXT_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1                X     ,SKEW  ,NDOF  ,IDDL    ,IKC    ,
     2                NDDL0 ,B0    ,INLOC  ,LJ     ,MA     ,
     3                INA   ,NT_RW ,W_DDL  ,NDDL   ,R02    )
         END IF !(IDYNA>0) THEN
         DEALLOCATE(B0)
        ENDIF
C -----add Fint---------------------
        IFLAG = 1
        CALL IMP_SETBA(A     ,AR      ,IDDL   ,NDOF  ,B     ,
     1                 IFLAG )
        DO I = 1,NUMNOD
         A(1,I) = A(1,I) + AC(1,I)
         A(2,I) = A(2,I) + AC(2,I)
         A(3,I) = A(3,I) + AC(3,I)
        ENDDO
        IF (IRODDL/=0) THEN
         DO I = 1,NUMNOD
          AR(1,I) = AR(1,I) + ACR(1,I)
          AR(2,I) = AR(2,I) + ACR(2,I)
          AR(3,I) = AR(3,I) + ACR(3,I)
         ENDDO
        ENDIF
C+++
        IF (IDYNA>0) THEN
C-------------
         CALL IMP_FHHT(NDDL0  ,B    )
C -----add Ma----M,IN sont deja condenses-----------------
         CALL IMP_SETBA(MA    ,INA     ,IDDL   ,NDOF  ,B     ,
     1                  IFLAG )
         DO I = 1,NUMNOD
          A(1,I) = A(1,I) + MA(1,I)
          A(2,I) = A(2,I) + MA(2,I)
          A(3,I) = A(3,I) + MA(3,I)
         ENDDO
         IF (IRODDL/=0) THEN
          DO I = 1,NUMNOD
           AR(1,I) = AR(1,I) + INA(1,I)
           AR(2,I) = AR(2,I) + INA(2,I)
           AR(3,I) = AR(3,I) + INA(3,I)
          ENDDO
         ENDIF
          DEALLOCATE(MA)
          IF (IRODDL/=0) DEALLOCATE(INA)
        ENDIF
C ---
C
C
       CALL BC_IMPR1(ICODT ,ICODR ,ISKEW ,SKEW  ,NDOF  ,
     1               IDDL  ,B     )
C
       IF(NFXVEL>0) THEN
         CALL FV_IMPRL(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1                 NDOF   ,B     )
       ENDIF
C
       IF(NT_RW>0) CALL FV_RWLR0(IDDL   ,B     )
C
       IF (IMACH==3.AND.NSPMD>1) THEN
C ----------B->dB---------------------
        IFLAG = 0
        CALL IMP_SETBA(A     ,AR      ,IDDL   ,NDOF  ,B     ,
     1                 IFLAG )
        CALL CONDENS_B(NDDL0  ,IKC  ,B    )
        CALL SPMD_SUMF_V(B )
        CALL IMP_SETBP(A     ,AR     ,IDDL   ,NDOF   ,IKC   ,
     .                 INLOC ,B      )
       ELSE
        CALL CONDENS_B(NDDL0  ,IKC  ,B    )
       ENDIF
C-------------
       IF (IDYNA>0) CALL IMP_FHHT1(NDDL0  ,NDDL    ,B    ,IKC )
C
      RETURN
      END
Chd|====================================================================
Chd|  EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        FV_IMPRL                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RWLR0                      source/constraints/general/rwall/srw_imp.F
Chd|        IMP_SETBA                     source/implicit/imp_setb.F    
Chd|        IMP_SETBP                     source/implicit/imp_setb.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE EXT_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1                   X     ,SKEW  ,NDOF  ,IDDL    ,IKC    ,
     4                   NDDL0 ,B     ,INLOC  ,LJ     ,AC     ,
     5                   ACR   ,NT_RW ,W_DDL  ,NDDL   ,R02    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),ICODT(*),ICODR(*),ISKEW(*),
     .        LJ(*),NDDL0,INLOC(*),NT_RW,W_DDL(*),NDDL
      INTEGER NDOF(*),IDDL(*),IKC(*)
      my_real
     .   X(3,*) ,SKEW(*),R02
      my_real
     .  B(*) ,XFRAME(NXFRAME,*),AC(3,*),ACR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,JI,JB,K1,IFLAG,ND
      my_real
     .  LB(NDDL0) ,REXT
C------------------------------------------
        CALL CP_REAL(NDDL0,B,LB)
        CALL BC_IMPR1(ICODT ,ICODR ,ISKEW ,SKEW  ,NDOF  ,
     1                IDDL  ,LB    )
        IF(NFXVEL>0) THEN
         CALL FV_IMPRL(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1                 NDOF   ,LB    )
        ENDIF
        IF(NT_RW>0) CALL FV_RWLR0(IDDL   ,LB    )
C
       IF (IMACH==3.AND.NSPMD>1) THEN
C ----------LB->dB---------------------
        IFLAG = 0
        CALL IMP_SETBA(AC     ,ACR      ,IDDL   ,NDOF  ,LB    ,
     1                 IFLAG  )
        CALL CONDENS_B(NDDL0  ,IKC  ,LB    )
        CALL SPMD_SUMF_V(LB )
        CALL IMP_SETBP(AC    ,ACR    ,IDDL   ,NDOF   ,IKC   ,
     .                 INLOC ,LB     )
       ELSE
        CALL CONDENS_B(NDDL0  ,IKC  ,LB    )
       ENDIF
C
      CALL PRODUT_W(NDDL, LB, LB,W_DDL,REXT )
      R02=MAX(R02,REXT )
C
      RETURN
      END
Chd|====================================================================
Chd|  RER02                         source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SETBP                     source/implicit/imp_setb.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|        RER_INT_V                     source/implicit/upd_glob_k.F  
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RER02(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                   ITAB  ,WEIGHT,MS    ,IN    ,
     2                   IBFV  ,VEL   ,ICODT,ICODR ,
     3                   NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                   INTBUF_TAB    ,NDOF  ,D     ,DR    ,
     5                   X     ,XFRAME,LJ    ,IXR   ,IXC   ,
     6                   IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                   FRBE3 ,IADK   ,JDIK  ,DIAG_K,LT_K ,
     8                   IDDL  ,IKC    ,INLOC ,NUM_IMP,NS_IMP,
     9                   NE_IMP,INDEX2,NDDL   ,W_DDL  ,A   ,
     A                   AR    ,R02   ,IRBE2  ,LRBE2  ,X_C )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*)
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ITAB(*),
     .        IPARI(NPARI,*), ISKEW(*),
     .        NRBYAC,IRBYAC(*),NINT2 ,IINT2(*),IXR(*)
      INTEGER NDOF(*),ICODT(*) ,ICODR(*),IXC(*),IXTG(*),
     .        SH4TREE(*), SH3TREE(*),IRBE3(*),LRBE3(*),
     .        IRBE2(*),LRBE2(*)
      INTEGER NDDL,IADK(*),JDIK(*),IDDL(*),IKC(*) ,INLOC(*),
     .        NUM_IMP(*),NS_IMP(*),NE_IMP(*),INDEX2(*),W_DDL(*)
      my_real
     .   RBY(NRBY,*) ,SKEW(*),IN(*),MS(*),
     .   VEL(LFXVELR,*), XFRAME(NXFRAME,*),FRBE3(*),X_C(*)
      my_real
     .  X(3,*),D(3,*),DR(3,*),DIAG_K(*),LT_K(*),A(3,*),AR(3,*),R02

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,ND,NNZ
      my_real
     .  U(NDDL),F(NDDL),R2
C-------D=0 for free dof------------
      DO N =1,NUMNOD
         A(1,N)= D(1,N)
         A(2,N)= D(2,N)
         A(3,N)= D(3,N)
       ENDDO
       IF (IRODDL/=0) THEN
        DO N =1,NUMNOD
         AR(1,N)= DR(1,N)
         AR(2,N)= DR(2,N)
         AR(3,N)= DR(3,N)
        ENDDO
       ENDIF
       DO N =1,NUMNOD
        I=INLOC(N)
        DO J=1,MIN(3,NDOF(I))
         ND = IDDL(I)+J
         IF (IKC(ND)==0) A(J,I) =ZERO
        ENDDO
        DO J=4,NDOF(I)
         ND = IDDL(I)+J
         IF (IKC(ND)==0) AR(J-3,I) =ZERO
        ENDDO
       ENDDO
C
       IF(IMP_LR > 0)THEN
         CALL RECUKIN(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                ITAB  ,WEIGHT,MS    ,IN    ,
     2                IBFV  ,VEL   ,ICODT,ICODR ,
     3                NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                INTBUF_TAB,NDOF  ,A     ,AR    ,
     5                X_C   ,XFRAME,LJ    ,IXR   ,IXC   ,
     6                IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                FRBE3 ,IRBE2 ,LRBE2 )
       ELSE
         CALL RECUKIN(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                ITAB  ,WEIGHT,MS    ,IN    ,
     2                IBFV  ,VEL   ,ICODT,ICODR ,
     3                NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                INTBUF_TAB ,NDOF  ,A     ,AR    ,
     5                X     ,XFRAME,LJ    ,IXR   ,IXC   ,
     6                IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                FRBE3 ,IRBE2 ,LRBE2 )
       END IF !(IMP_LR > 0)THEN
C
        CALL RER_INT_V(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    X         ,A         )
        DO I =1,NDDL
         U(I) =ZERO
        ENDDO
        CALL IMP_SETBP(A     ,AR     ,IDDL   ,NDOF   ,IKC   ,
     .                 INLOC ,U      )
        DO I =1,NDDL
         F(I)=DIAG_K(I)*U(I)
        ENDDO
        IF (NSPMD > 1)CALL SPMD_SUMF_V(F )
c        NNZ = IADK(NDDL+1)-IADK(1)
c        CALL MAV_LT(
c     1               NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,
c     2               LT_K  ,U     ,F     )
        CALL PRODUT_W( NDDL  ,F   ,F  ,W_DDL  , R2)
        IF (R2>R02.AND.IMCONV>=0.AND.
     .         (LPRINT/=0.OR.NPRINT/=0)) THEN
         IF(IMACH/=3.OR.ISPMD==0) THEN
            WRITE(IOUT,1006)SQRT(R2)
           IF(NPRINT<0) THEN
            WRITE(ISTDO,1006)SQRT(R2)
           ENDIF
         ENDIF
        ENDIF
        R02= MAX(R2,R02)
C
 1006 FORMAT(3X,'DUE TO THE CONTACT,',
     .       ' REFERENCE RESIDUAL(F) NORM HAS BEEN CHANGED TO:',E11.4)
      RETURN
      END
Chd|====================================================================
Chd|  RER_INT_V                     source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        RER02                         source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        UD_INT11                      source/implicit/upd_glob_k.F  
Chd|        UD_INT5                       source/implicit/upd_glob_k.F  
Chd|        UD_INT7                       source/implicit/upd_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RER_INT_V(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    X         ,UD             )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NUM_IMP(*),
     .        NS_IMP(*),NE_IMP(*)
C     REAL
      my_real
     .  X(3,*),UD(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,NTY,NSN
      INTEGER I,J,K,L,NDOFI,N,IAD,
     .        IDREC,INSV,INS11,NRTS
C-----------------------------------------------
      IAD=1
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
C
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
          CALL UD_INT5(
     1       IPARI(1,NIN),INTBUF_TAB(NIN) ,X       ,
     2       NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD)  ,UD    )
         IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
C
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
C
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
       ELSEIF(NTY==6)THEN
C------------verify effect, if R02 is too high-----
       ELSEIF(NTY==7.OR.NTY==10.OR.NTY==24)THEN
C
        CALL UD_INT7(
     1    NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),INTBUF_TAB(NIN)%IRECTM,
     .    INTBUF_TAB(NIN)%NSV,
     2    NSN         ,X          ,UD         )
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==11)THEN
C
        NRTS   =IPARI(3,NIN)
        CALL UD_INT11(
     1    NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),INTBUF_TAB(NIN)%IRECTS,
     2    INTBUF_TAB(NIN)%IRECTM,NSN        ,X          ,UD        )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ----
Chd|====================================================================
Chd|  UD_INT7                       source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        RER_INT_V                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        INIHI7                        source/implicit/upd_glob_k.F  
Chd|====================================================================
      SUBROUTINE UD_INT7(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT    ,NSV       ,
     2    NSN       ,X         ,UD          )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NSN
C     REAL
      my_real
     .  X(3,*),UD(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,M(4)
      my_real
     .  H(4),US(3)
C-------Missed SPMD----------------------------------------
       DO I = 1, JLT
C--------secnd node-----add case Ud_s >0
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        NE=NE_IMP(I)
        DO J = 1, 4
         M(J)= IRECT(J,NE)
        ENDDO
        CALL INIHI7(N1,M,X ,H)
        IF ((ABS(UD(1,N1))+ABS(UD(2,N1))+ABS(UD(3,N1)))>ZERO) THEN
         DO J = 1, 4
          US(1) = H(J)*UD(1,N1)
          US(2) = H(J)*UD(2,N1)
          US(3) = H(J)*UD(3,N1)
         IF (ABS(US(1))>ABS(UD(1,M(J)))) UD(1,M(J))= US(1)
         IF (ABS(US(2))>ABS(UD(2,M(J)))) UD(2,M(J))= US(2)
         IF (ABS(US(3))>ABS(UD(3,M(J)))) UD(3,M(J))= US(3)
         ENDDO
        ELSE
         US(1) = ZERO
         US(2) = ZERO
         US(3) = ZERO
         DO J = 1, 4
         US(1) = US(1)+ H(J)*UD(1,M(J))
         US(2) = US(2)+ H(J)*UD(2,M(J))
         US(3) = US(3)+ H(J)*UD(3,M(J))
         ENDDO
         IF (ABS(US(1))>ABS(UD(1,N1))) UD(1,N1)= US(1)
         IF (ABS(US(2))>ABS(UD(2,N1))) UD(2,N1)= US(2)
         IF (ABS(US(3))>ABS(UD(3,N1))) UD(3,N1)= US(3)
        END IF 
       ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  UD_INT11                      source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        RER_INT_V                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        INIHI11                       source/implicit/upd_glob_k.F  
Chd|====================================================================
      SUBROUTINE UD_INT11(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTS   ,IRECTM    ,
     2    NSN       ,X         ,UD        )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTS(2,*),IRECTM(2,*),NSN
C     REAL
      my_real
     .  X(3,*),UD(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,M1,M2,NS(2),NM(2)
      my_real
     .  HS(2) ,HM(2),US(3,2),VM
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        NS(1) = IRECTS(1,IG)
        NS(2) = IRECTS(2,IG)
        NE=NE_IMP(I)
        NM(1) = IRECTM(1,NE)
        NM(2) = IRECTM(2,NE)
        CALL INIHI11(NS, NM, X  ,HS ,HM)
        DO J = 1, 3
         VM = HM(1)*UD(J,NM(1))+HM(2)*UD(J,NM(2))
         US(J,1)=HS(1)*VM
         US(J,2)=HS(2)*VM
         IF (ABS(US(J,1))>ABS(UD(J,NS(1)))) UD(J,NS(1))= US(J,1)
         IF (ABS(US(J,2))>ABS(UD(J,NS(2)))) UD(J,NS(2))= US(J,2)
        ENDDO
       ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INIHI7                        source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        UD_INT7                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIHI7(NS,IRECT,X    ,H)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS,IRECT(4)
C     REAL
      my_real
     .   X(3,*)    ,H(4)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER N1,N2,N3,N4
      my_real
     .   X0, Y0, Z0, XL1, XL2, XL3, XL4, YY1, YY2, YY3, YY4,
     .   ZZ1, ZZ2, ZZ3, ZZ4, XI1, XI2, XI3, XI4, YI1, YI2, YI3, YI4,
     .   ZI1, ZI2, ZI3, ZI4, XN1, YN1, ZN1, XN2, YN2, ZN2, XN3, YN3,
     .   ZN3, XN4, YN4, ZN4, AN, AREA, A12, A23, A34, A41, B12, B23,
     .   B34, B41, AB1, AB2, TP, TM, SP, SM, X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,XI,YI,ZI,NX,NY,NZ,ALP,SSC,TTC
C-----------------------------------------------
      ALP = TWOEM2
      H(1) = ZERO
      H(2) = ZERO
      H(3) = ZERO
      H(4) = ZERO
      N1 = IRECT(1)
      N2 = IRECT(2)
      N3 = IRECT(3)
      N4 = IRECT(4)
      X1=X(1,N1)
      X2=X(1,N2)
      X3=X(1,N3)
      X4=X(1,N4)
      Y1=X(2,N1)
      Y2=X(2,N2)
      Y3=X(2,N3)
      Y4=X(2,N4)
      Z1=X(3,N1)
      Z2=X(3,N2)
      Z3=X(3,N3)
      Z4=X(3,N4)
      XI=X(1,NS)
      YI=X(2,NS)
      ZI=X(3,NS)
C
      X0 = FOURTH*(X1+X2+X3+X4)
      Y0 = FOURTH*(Y1+Y2+Y3+Y4)
      Z0 = FOURTH*(Z1+Z2+Z3+Z4)
C
      XL1 = X1-X0
      XL2 = X2-X0
      XL3 = X3-X0
      XL4 = X4-X0
      YY1 = Y1-Y0
      YY2 = Y2-Y0
      YY3 = Y3-Y0
      YY4 = Y4-Y0
      ZZ1 = Z1-Z0
      ZZ2 = Z2-Z0
      ZZ3 = Z3-Z0
      ZZ4 = Z4-Z0
C
      XI1 = X1-XI
      XI2 = X2-XI
      XI3 = X3-XI
      XI4 = X4-XI
      YI1 = Y1-YI
      YI2 = Y2-YI
      YI3 = Y3-YI
      YI4 = Y4-YI
      ZI1 = Z1-ZI
      ZI2 = Z2-ZI
      ZI3 = Z3-ZI
      ZI4 = Z4-ZI
C
      XN1 = YY1*ZZ2 - YY2*ZZ1
      YN1 = ZZ1*XL2 - ZZ2*XL1
      ZN1 = XL1*YY2 - XL2*YY1
      NX=XN1
      NY=YN1
      NZ=ZN1
C
      XN2 = YY2*ZZ3 - YY3*ZZ2
      YN2 = ZZ2*XL3 - ZZ3*XL2
      ZN2 = XL2*YY3 - XL3*YY2
      NX=NX+XN2
      NY=NY+YN2
      NZ=NZ+ZN2
C
      XN3 = YY3*ZZ4 - YY4*ZZ3
      YN3 = ZZ3*XL4 - ZZ4*XL3
      ZN3 = XL3*YY4 - XL4*YY3
      NX=NX+XN3
      NY=NY+YN3
      NZ=NZ+ZN3
C
      XN4 = YY4*ZZ1 - YY1*ZZ4
      YN4 = ZZ4*XL1 - ZZ1*XL4
      ZN4 = XL4*YY1 - XL1*YY4
      NX=NX+XN3
      NY=NY+YN3
      NZ=NZ+ZN3
C
      AN= MAX(EM20,SQRT(NX*NX+NY*NY+NZ*NZ))
      IF(AN<=EM19) RETURN
      NX=NX/AN
      NY=NY/AN
      NZ=NZ/AN
      AREA=HALF*AN
C
      A12=(NX*XN1+NY*YN1+NZ*ZN1)
      A23=(NX*XN2+NY*YN2+NZ*ZN2)
      A34=(NX*XN3+NY*YN3+NZ*ZN3)
      A41=(NX*XN4+NY*YN4+NZ*ZN4)
C
      XN1 = YI1*ZI2 - YI2*ZI1
      YN1 = ZI1*XI2 - ZI2*XI1
      ZN1 = XI1*YI2 - XI2*YI1
      B12=(NX*XN1+NY*YN1+NZ*ZN1)
C
      XN2 = YI2*ZI3 - YI3*ZI2
      YN2 = ZI2*XI3 - ZI3*XI2
      ZN2 = XI2*YI3 - XI3*YI2
      B23=(NX*XN2+NY*YN2+NZ*ZN2)
C
      XN3 = YI3*ZI4 - YI4*ZI3
      YN3 = ZI3*XI4 - ZI4*XI3
      ZN3 = XI3*YI4 - XI4*YI3
      B34=(NX*XN3+NY*YN3+NZ*ZN3)
C
      XN4 = YI4*ZI1 - YI1*ZI4
      YN4 = ZI4*XI1 - ZI1*XI4
      ZN4 = XI4*YI1 - XI1*YI4
      B41=(NX*XN4+NY*YN4+NZ*ZN4)
C
      AB1=A23*B41
      AB2=B23*A41
C
      IF(ABS(AB1+AB2)/AREA>EM10)THEN
       SSC=(AB1-AB2)/(AB1+AB2)
      ELSE
       SSC=ZERO
      ENDIF
      IF(ABS(A34/AREA)>EM10)THEN
       AB1=B12*A34
       AB2=B34*A12
       TTC=(AB1-AB2)/(AB1+AB2)
      ELSE
       TTC=(B12-A12)/A12
       IF(B23<=ZERO.AND.B41<=ZERO)THEN
         IF(-B23/A12<=ALP.AND.-B41/A12<=ALP)SSC=ZERO
       ELSEIF(B23<=ZERO)THEN
         IF(-B23/A12<=ALP)SSC=ONE
       ELSEIF(B41==ZERO)THEN
         IF(B41<ZERO.AND.-B41/A12<=ALP)SSC=-ONE
       ENDIF
      ENDIF
C
      IF(ABS(SSC)>ONE+ALP.OR.ABS(TTC)>ONE+ALP) RETURN
      IF(ABS(SSC)>ONE)SSC=SSC/ABS(SSC)
      IF(ABS(TTC)>ONE)TTC=TTC/ABS(TTC)
C
      TP=FOURTH*(ONE+TTC)
      TM=FOURTH*(ONE-TTC)
C
      SP=ONE+SSC
      SM=ONE-SSC
      H(1)=TM*SM
      H(2)=TM*SP
      H(3)=TP*SP
      H(4)=TP*SM
C
      RETURN
      END
Chd|====================================================================
Chd|  INIHI11                       source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        UD_INT11                      source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIHI11(NS, NM, X  ,HS ,HM)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS(2),NM(2)
C     REAL
      my_real
     .   X(3,*)    ,HS(2),HM(2)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET
C-----------------------------------------------
      HS(1) = ZERO
      HS(2) = ZERO
      HM(1) = ZERO
      HM(2) = ZERO
       XS12 = X(1,NS(2))-X(1,NS(1))
       YS12 = X(2,NS(2))-X(2,NS(1))
       ZS12 = X(3,NS(2))-X(3,NS(1))
       XS2  = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = X(1,NM(2))-X(1,NM(1))
       YM12 = X(2,NM(2))-X(2,NM(1))
       ZM12 = X(3,NM(2))-X(3,NM(1))
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = X(1,NM(2))-X(1,NS(2))
       YS2M2 = X(2,NM(2))-X(2,NS(2))
       ZS2M2 = X(3,NM(2))-X(3,NS(2))
C
       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       HS(1) = (XB*XSM-XA*XM2) / DET
       HM(1) = (XA*XSM-XB*XS2) / DET
C
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       IF(HM(1)<ZERO)THEN
         HM(1) = ZERO
         HS(1) = -XA / XS2
       ELSEIF(HM(1)>ONE)THEN
         HM(1) = ONE
         HS(1) = -(XA + XSM) / XS2
       ENDIF
C
       IF(HS(1)<ZERO)THEN
         HS(1) = ZERO
         HM(1) = -XB / XM2
       ELSEIF(HS(1)>ONE)THEN
         HS(1) = ONE
         HM(1) = -(XB + XSM) / XM2
       ENDIF
C
       HM(1) = MIN(ONE,HM(1))
       HM(1) = MAX(ZERO,HM(1))
C
       HS(2) = ONE -HS(1)
       HM(2) = ONE -HM(1)
C
      RETURN
      END
C   --------- ----
Chd|====================================================================
Chd|  UD_INT5                       source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        RER_INT_V                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        I3CST3                        source/interfaces/inter3d/i3cst3.F
Chd|        I3DIS3                        source/interfaces/inter3d/i3dis3.F
Chd|        I3GAP3                        source/interfaces/inter3d/i3gap3.F
Chd|        I5CORK3                       source/interfaces/inter3d/i5cork3.F
Chd|        UD_INTG5                      source/implicit/upd_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UD_INT5(
     1                   IPARI ,INTBUF_TAB,X    ,NUM_IMP,
     2                   CAND_N,CAND_E ,UD   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr07_c.inc"
#include      "scr05_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*)
C     REAL
      my_real
     .  X(3,*),UD(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGAP, INACTI, IFQ, MFROT, IGSTI,INTY
      INTEGER JX1(MVSIZ), JX2(MVSIZ), JX3(MVSIZ), JX4(MVSIZ),
     .        NSVG(MVSIZ), I3N ,IGIMP
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,DIST(MVSIZ)
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ) :: XFACE,N1,N2,N3
      my_real, DIMENSION(MVSIZ) :: SSC,TTC,AREA,THK,ALP
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0,ANS
      my_real, DIMENSION(MVSIZ) :: XX1,XX2,XX3,XX4
      my_real, DIMENSION(MVSIZ) :: YY1,YY2,YY3,YY4
      my_real, DIMENSION(MVSIZ) :: ZZ1,ZZ2,ZZ3,ZZ4
      my_real, DIMENSION(MVSIZ) :: XI1,XI2,XI3,XI4
      my_real, DIMENSION(MVSIZ) :: YI1,YI2,YI3,YI4
      my_real, DIMENSION(MVSIZ) :: ZI1,ZI2,ZI3,ZI4
      my_real, DIMENSION(MVSIZ) :: XN1,XN2,XN3,XN4
      my_real, DIMENSION(MVSIZ) :: YN1,YN2,YN3,YN4
      my_real, DIMENSION(MVSIZ) :: ZN1,ZN2,ZN3,ZN4
      my_real, DIMENSION(MVSIZ) :: XP,YP,ZP
      my_real, DIMENSION(MVSIZ) :: H1,H2,H3,H4
C-----------------------------------------------
      INTY   = IPARI(7)
C-------NSN defined in scr04_c.inc--
       GAP  =INTBUF_TAB%VARIABLES(2)
        DO NFT = 0 , NUM_IMP - 1 , NVSIZ
          LFT=1
          LLT = MIN( NVSIZ, NUM_IMP - NFT )
          CALL I5CORK3(X ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,
     .                INTBUF_TAB%NSV,
     .                INTBUF_TAB%IRTLM,CAND_N(NFT+1),CAND_E(NFT+1),
     .                NSVG  ,JX1   ,JX2     ,JX3   ,JX4   ,
     2                X1    ,X2    ,X3      ,
     3                X4    ,Y1    ,Y2    ,Y3    ,Y4      ,
     4                Z1    ,Z2    ,Z3    ,Z4    ,XFACE   ,
     5                XI    ,YI    ,ZI    ,IX1   ,IX2     ,
     6                IX3   ,IX4)
          CALL I3CST3(  X1    ,X2      ,X3    ,X4    ,XI ,
     1                  Y1    ,Y2      ,Y3    ,Y4    ,YI ,
     2                  Z1    ,Z2      ,Z3    ,Z4    ,ZI ,
     3                  XFACE ,N1      ,N2    ,N3    ,SSC,
     4                  TTC   ,X0      ,Y0    ,Z0    ,XX1,
     5                  XX2   ,XX3     ,XX4   ,YY1   ,YY2,
     6                  YY3   ,YY4     ,ZZ1   ,ZZ2   ,ZZ3,
     7                  ZZ4   ,XI1     ,XI2   ,XI3   ,XI4,
     8                  YI1   ,YI2     ,YI3   ,YI4   ,ZI1,
     9                  ZI2   ,ZI3     ,ZI4   ,XN1   ,XN2,
     1                  XN3   ,XN4     ,YN1   ,YN2   ,YN3,
     2                  YN4   ,ZN1     ,ZN2   ,ZN3   ,ZN4,
     3                  AREA)
          CALL I3GAP3(GAP,AREA,THK,ALP)
          CALL I3DIS3(IGIMP,INTY,DIST,X1    ,X2 ,
     1              X3   ,X4  ,XI  ,Y1    ,Y2 ,
     2              Y3   ,Y4  ,YI  ,Z1    ,Z2 ,
     3              Z3   ,Z4  ,ZI  ,XFACE ,N1 ,
     4              N2   ,N3  ,SSC ,TTC   ,ALP,
     5              ANS  ,XP  ,YP  ,ZP    ,H1 ,
     6              H2   ,H3  ,H4)
          CALL UD_INTG5(LFT    ,LLT    ,NSVG   ,JX1    ,JX2    ,
     .                  JX3    ,JX4    ,UD     ,H1     ,H2     ,
     2                  H3     ,H4     ,XFACE)
        END DO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ----
Chd|====================================================================
Chd|  UD_INTG5                      source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        UD_INT5                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UD_INTG5(LFT    ,LLT    ,NSVG   ,JX1    ,JX2    ,
     .                    JX3    ,JX4    ,UD     ,H1     ,H2     ,
     2                    H3     ,H4     ,XFACE)
C----6---------------------------------------------------------------7---------8
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
      INTEGER LFT    ,LLT    ,NSVG(*) ,JX1(*), JX2(*),
     .         JX3(*), JX4(*)
C     REAL
      my_real
     .   UD(3,*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4,XFACE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, L, NN,M(4),J,NS
      my_real
     .   H(4),US(3)
C-----------------------------------------------
       DO I = LFT    ,LLT
C--------secnd node-----
        NS = NSVG(I)
        M(1)= JX1(I)
        M(2)= JX2(I)
        M(3)= JX3(I)
        M(4)= JX4(I)
        H(1)= H1(I)*XFACE(I)
        H(2)= H2(I)*XFACE(I)
        H(3)= H3(I)*XFACE(I)
        H(4)= H4(I)*XFACE(I)
        US(1) = ZERO
        US(2) = ZERO
        US(3) = ZERO
        DO J = 1, 4
         US(1) = US(1)+ H(J)*UD(1,M(J))
         US(2) = US(2)+ H(J)*UD(2,M(J))
         US(3) = US(3)+ H(J)*UD(3,M(J))
        ENDDO
        IF (ABS(US(1))>ABS(UD(1,NS))) UD(1,NS)= US(1)
        IF (ABS(US(2))>ABS(UD(2,NS))) UD(2,NS)= US(2)
        IF (ABS(US(3))>ABS(UD(3,NS))) UD(3,NS)= US(3)
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_DOFSPC                    source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        COLINEAR3                     source/implicit/upd_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INI_DOFSPC(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,NDOF      ,IRBE3     ,
     3    LRBE3     ,IRBE2     ,LRBE2     ,X         ,DOFSPC    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),NRBYAC,IRBYAC(*),LPBY(*),DOFSPC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),NDOF(*),
     .        IRBE3(NRBE3L,*) ,LRBE3(*),IRBE2(NRBE2L,*),LRBE2(*)
C     REAL
      my_real
     .   X(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NMN,JI,K10,K11,K12,K13,K14,J,K
      INTEGER I,N,M,L,NS,ID,NM,NSN,IAD,ICOL,N1,N2,N3
C-----------------------------------------------
       DO I=1,NUMNOD
        DOFSPC(I)=NDOF(I)
       END DO
c      RETURN
C
      DO I=1,NRBYAC
       N=IRBYAC(I)
       K=IRBYAC(I+NRBYKIN)
       M=NPBY(1,N)
       IF (M<=0) CYCLE
       DOFSPC(M)=0
       NSN  =NPBY(2,N)
       ICOL = 1
       DO N =1,NSN
        NS=LPBY(K+N)
        IF (NDOF(NS)>3) THEN
         ICOL = 0
         CYCLE
        ENDIF
       END DO
       IF (ICOL==1) THEN
        IF (NSN<=2) THEN
         DOFSPC(M)=NDOF(M)
        ELSE
         CALL COLINEAR3(LPBY(K+1),LPBY(K+2),LPBY(K+3),X,ICOL)
         IF (ICOL==1) THEN
          N1=LPBY(K+2)
          N2=LPBY(K+3)
           DO N =4,NSN
            N3=LPBY(K+N)
            CALL COLINEAR3(N1,N2,N3,X,ICOL)
            IF (ICOL==0) CYCLE
            N1=N2
            N2=N3
           END DO
           IF (ICOL==1) DOFSPC(M)=NDOF(M)
         END IF
        END IF
       END IF !(ICOL==1) THEN
      ENDDO
C------interface 2-----------
      DO I=1,NINT2
       N=IINT2(I)
       NSN = IPARI(5,N)
       NMN = IPARI(6,N)
       JI=IPARI(1,N)
       K10=JI-1
       K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
       K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
       K13=K12+NSN
C------MSR(NMN)-----
       K14=K13+NMN
C------IRTL(NSN)--main el number---
c       DO J=1,NSN
c        NS=INBUF(K12+J)
c        IF (NDOF(NS)>0) THEN
c         L=INBUF(K14+J)
c         ID=K11+4*(L-1)
c         DO M=1,4
c          NM=INBUF(ID+M)
c          DOFSPC(NM)=0
c         ENDDO
c        ENDIF
c       ENDDO
       DO J=1,NMN
        NM=INTBUF_TAB(N)%MSR(J)
        DOFSPC(NM)=0
       ENDDO
      ENDDO
C
      DO I=1,NRBE3
       IAD=IRBE3(1,I)
       NS =IRBE3(3,I)
       IF (NS==0) CYCLE
       NMN=IRBE3(5,I)
       DO J=1,NMN
         NM=LRBE3(IAD+J)
         DOFSPC(NM)=0
       ENDDO
      ENDDO
C
      DO I=1,NRBE2
       J=IRBE2(3,I)
       IAD=IRBE2(1,I)
       NSN = IRBE2(5,I)
       IF (NSN==0) CYCLE
       ICOL = 1
       DOFSPC(J)=0
       DO N =1,NSN
        NS = LRBE2(IAD+N)
        IF (NDOF(NS)>3) THEN
         ICOL = 0
         CYCLE
        ENDIF
       END DO
       IF (ICOL==1) THEN
        IF (NSN==1) THEN
         DOFSPC(J)=NDOF(J)
        ELSEIF (NSN==2) THEN
         CALL COLINEAR3(J,LRBE2(IAD+1),LRBE2(IAD+2),X,ICOL)
         IF (ICOL==1) DOFSPC(J)=NDOF(J)
        ELSE
         CALL COLINEAR3(J,LRBE2(IAD+1),LRBE2(IAD+2),X,ICOL)
         IF (ICOL==1) THEN
          N1=LRBE2(IAD+1)
          N2=LRBE2(IAD+2)
           DO N =3,NSN
            N3=LRBE2(IAD+N)
            CALL COLINEAR3(N1,N2,N3,X,ICOL)
            IF (ICOL==0) CYCLE
            N1=N2
            N2=N3
           END DO
           IF (ICOL==1) DOFSPC(J)=NDOF(J)
         END IF
        END IF
       END IF !(ICOL==1) THEN
      END DO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  COLINEAR3                     source/implicit/upd_glob_k.F  
Chd|-- called by -----------
Chd|        INI_DOFSPC                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COLINEAR3(N1,N2,N3,X,ICOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,N3,ICOL
      my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J  ,K
      MY_REAL
     .    X21,Y21,Z21,X31,Y31,Z31,PR
C
       ICOL=0
       X21=X(1,N2)-X(1,N1)
       Y21=X(2,N2)-X(2,N1)
       Z21=X(3,N2)-X(3,N1)
       X31=X(1,N3)-X(1,N1)
       Y31=X(2,N3)-X(2,N1)
       Z31=X(3,N3)-X(3,N1)
C       -----------------
       PR = Y21 * Z31 - Z21 * Y31
       IF (ABS(PR)<=EM10) THEN
        PR = Z21 * X31 - X21 * Z31
        IF (ABS(PR)<=EM10) THEN
         PR = X21 * Y31 - Y21 * X31
         IF (ABS(PR)<=EM10) ICOL=1
        END IF
       END IF
C
      RETURN
      END
