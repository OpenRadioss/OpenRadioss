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
Chd|  CHKSLV3                       source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHKSLV3(
     .  NSN     ,NSV,STFN,ITAG,ITASK,
     .  NEWFRONT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*), ITAG(*), ITASK, NEWFRONT
C     REAL
      my_real
     .        STFN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NSNF, NSNL
C     REAL
C-----------------------------------------------
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      DO I = NSNF, NSNL
C si tag nul sur noeuds secnds alors stifn = 0.
        IF (ITAG(NSV(I)) == 0.AND.STFN(I) > ZERO) THEN
C         STFN(I) = ZERO => Prise en compte cycle suivant apres comm SPMD (cf i7for3)
          STFN(I) = -STFN(I)
          NEWFRONT = -1
        ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHKSLV3_T24                   source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHKSLV3_T24(
     .  NSN     ,NSV,   STFN,    ITAG, ITASK,
     .  IS2SE   ,IRTSE, NEWFRONT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*), ITAG(*), ITASK, NEWFRONT
      INTEGER IS2SE(2,*),IRTSE(5,*)
C     REAL
      my_real
     .        STFN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NSNF, NSNL,ND,SE
C     REAL
      INTEGER IK1(4),IK2(4),IE1,IE2,IED,NS1,NS2
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
C-----------------------------------------------
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      DO I = NSNF, NSNL
C si tag nul sur noeuds secnds alors stifn = 0.
        ND = NSV(I)
        IF (ND > NUMNOD)THEN
          SE=IS2SE(1,ND-NUMNOD)
          IED=IRTSE(5,SE)
          NS1= IRTSE(IK1(IED),SE)
          NS2= IRTSE(IK2(IED),SE)
          IF(ITAG(NS1)==0 .AND.ITAG(NS2)==0 .AND. STFN(I) > ZERO) THEN
            STFN(I) = -STFN(I)
            NEWFRONT = -1
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CHKSLV3B                      source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHKSLV3B(NSN,NSV,STFN,ITAG,ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*), ITAG(*), ITASK
C     REAL
      my_real
     .        STFN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NSNF, NSNL
C     REAL
C-----------------------------------------------
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      DO I = NSNF, NSNL
C si tag nul sur noeuds secnds alors stifn = 0. des le cycle courant
        IF (ITAG(NSV(I)) == 0) THEN
          STFN(I) = ZERO
        END IF
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHKSLV3C                      source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHKSLV3C(
     .  NSN     ,NSV,STFA,ITAG,ITASK,
     .  NEWFRONT,NLG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*), ITAG(*), NLG(*), ITASK, NEWFRONT
C     REAL
      my_real
     .        STFA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NSNF, NSNL
C     REAL
C-----------------------------------------------
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      DO I = NSNF, NSNL
C si tag nul sur noeuds secnds alors stifn = 0.
        IF (ITAG(NLG(NSV(I))) == 0.AND.STFA(NSV(I)) > ZERO) THEN
C         STFA(NSV(I)) = ZERO => Prise en compte cycle suivant apres comm SPMD (cf i7for3)
          STFA(NSV(I)) = -STFA(NSV(I))
          NEWFRONT = -1
        ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHKIPARI                      source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHKIPARI(IPARI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, ITY
CC-----------------------------------------------
C
C ipari(16) : flag parallelisation interfaces sauf type 2
C ipari(16) : nombre de noeuds secnds locaux int. type 2
C ipari(17) : flag delete facettes/noeuds int. type7, type2
C ipari(17) = 0 => ras
C ipari(17) = 1 => delete facettes+noeuds methode 1 
C ipari(17) = 2 => delete facettes+noeuds methode 2 (sauf type 2)
      IDEL7NG = 0
      IDEL7NOK = 0
      DO NG=1,NINTER
        ITY = IPARI(7,NG)
        IF(ITY/=2) IPARI(16,NG)=-1
        IF(ITY== 2.OR.ITY== 3.OR.ITY== 5.OR.
     +     ITY== 7.OR.ITY==10.OR.ITY==11.OR.
     +     ITY==20.OR.ITY==21.OR.ITY==22.OR.
     +     ITY==23.OR.ITY==24.OR.ITY==25)
     +    IDEL7NG = MAX(IDEL7NG,IPARI(17,NG))
      ENDDO
      IF (IDEL7NG>=1) IDEL7NOK = 1
C    
      RETURN
      END
C
Chd|====================================================================
Chd|  CHKINIT                       source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHKINIT(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10    ,IXS20    ,
     4   IXS16      ,IXTG1      ,GEO        ,ADDCNEL  ,CNEL     ,
     5   ADSKY      ,IPARG      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*), 
     .        IXS10(6,*),IXS20(12,*),IXS16(8,*),IXTG1(4,*),
     .        ADDCNEL(0:*), CNEL(0:*), ADSKY(0:*), IPARG(NPARG,*)
      my_real 
     .        GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, N, ITY, NEL, LLT, LFT, NFT, IE, ISOLNOD, ICNOD,
     .        OFQ, OFC, OFT, OFP, OFR, OFTG, OFUR, NG
CC-----------------------------------------------
C
C Pre construction de ADDCNEL
C    
      DO I=0,NUMNOD+1
        ADDCNEL(I) = 0
      END DO
C
      DO  K=2,9
        DO  I=1,NUMELS
          N = IXS(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=1,6
        DO I=1,NUMELS10
          N = IXS10(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=1,12
        DO I=1,NUMELS20
          N = IXS20(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=1,8
        DO I=1,NUMELS16
          N = IXS16(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=2,5
        DO I=1,NUMELQ
          N = IXQ(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=2,5
        DO I=1,NUMELC
          N = IXC(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=2,3
        DO I=1,NUMELT
          N = IXT(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=2,3
        DO I=1,NUMELP
          N = IXP(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=2,3
        DO I=1,NUMELR
          N = IXR(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C traitement a part du 3eme noeud optionnel sauf type 12
      DO I=1,NUMELR
        N = IXR(4,I) + 1
        IF(NINT(GEO(12,IXR(1,I))) == 12)  ADDCNEL(N) = ADDCNEL(N) + 1
      END DO
C
      DO K=2,4
        DO I=1,NUMELTG
          N = IXTG(K,I) + 1
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      DO K=1,3
        DO I=1,NUMELTG6
          N = IXTG1(K,I) + 1
          IF (N > 1) ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO
C
      ADDCNEL(1) = 1
      DO I=2,NUMNOD+1
        ADDCNEL(I) = ADDCNEL(I) + ADDCNEL(I-1)
      END DO
C
C Construction de la matrice CNEL
C
C CNEL est contruite de maniere analogue a ce qui est fait dans chkstfn3n (traitement idel)
C la numerotation dans CNEL est globale de 1 a NUMELS+NUMELQ+...+NUMELR
C
      ADSKY(0) = 0
      DO I = 1, NUMNOD
        ADSKY(I) = ADDCNEL(I)
      ENDDO
C
      OFQ=NUMELS
      OFC=OFQ+NUMELQ
      OFT=OFC+NUMELC
      OFP=OFT+NUMELT
      OFR=OFP+NUMELP
      OFTG=OFR+NUMELR
      OFUR=OFTG+NUMELTG
C
      DO NG = 1,NGROUP
          ITY     = IPARG(5,NG)
          NEL     = IPARG(2,NG)
          NFT     = IPARG(3,NG)
          ICNOD   = IPARG(11,NG)
          ISOLNOD = IPARG(28,NG)
          LFT     = 1
          LLT     = NEL
          IF(ITY == 1) THEN
C#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I
              DO K=2,9
                N = IXS(K,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDDO
            ENDDO
C
            IF(ISOLNOD == 10) THEN   
C#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                DO K=1,6
                  N = IXS10(K,NFT+I-NUMELS8)
                  CNEL(ADSKY(N)) = IE
                  ADSKY(N) = ADSKY(N)+1
                ENDDO
              ENDDO
            ELSEIF(ISOLNOD == 20) THEN            
C#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                DO K=1,12
                  N = IXS20(K,NFT+I-NUMELS8-NUMELS10)
                  CNEL(ADSKY(N)) = IE
                  ADSKY(N) = ADSKY(N)+1
                ENDDO
              ENDDO
            ELSEIF(ISOLNOD == 16) THEN       
C#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                DO K=1,8
                  N = IXS16(K,NFT+I-NUMELS8-NUMELS10-NUMELS20)
                  CNEL(ADSKY(N)) = IE
                  ADSKY(N) = ADSKY(N)+1
                ENDDO
              ENDDO
            ENDIF
C
          ELSEIF(ITY == 2) THEN
C#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFQ
              DO K=2,5
                N = IXQ(K,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDDO
            ENDDO
C    
          ELSEIF(ITY == 3)THEN
C#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFC
              DO K=2,5
                N = IXC(K,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDDO
            ENDDO
C
          ELSEIF(ITY == 4)THEN
C#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFT
              DO K=2,3
                N = IXT(K,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDDO
            ENDDO
C
          ELSEIF(ITY == 5)THEN
C#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFP
              DO K=2,3
                N = IXP(K,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDDO
            ENDDO
C
          ELSEIF(ITY == 6)THEN
C#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFR
              DO K=2,3
                N = IXR(K,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDDO
              IF(NINT(GEO(12,IXR(1,NFT+I))) == 12) THEN
                N = IXR(4,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDIF
            ENDDO
C
          ELSEIF(ITY == 7)THEN
C#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFTG
              DO K=2,4
                N = IXTG(K,NFT+I)
                CNEL(ADSKY(N)) = IE
                ADSKY(N) = ADSKY(N)+1
              ENDDO
            ENDDO
            IF(ICNOD == 6) THEN   
C#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                DO K=1,3
                  N = MAX(0,IXTG1(K,NFT+I-NUMELTG+NUMELTG6))
                  CNEL(ADSKY(N)) = IE
                  ADSKY(N) = ADSKY(N)+1
                ENDDO
              ENDDO
            END IF
C
          ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        EXCH_TAGEL_C                  source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_IBUF_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_SHMBUF_C                  source/coupling/rad2rad/rad2rad_c.c
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        R2R_EXCH_ITAG                 source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_TAGEL                     source/coupling/rad2rad/r2r_exchange.F
Chd|        SEND_IBUF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_SHMBUF_C                 source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_EXCH_IDEL                source/mpi/interfaces/spmd_exch_idel.F
Chd|        SPMD_EXCH_R2R_ITAG            source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_R2R_TAGEL                source/mpi/r2r/spmd_r2r.F     
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
      SUBROUTINE TAGOFF3N(
     1    GEO     ,IXS   ,IXS10   ,IXS20   ,IXS16  ,IXQ    ,
     2    IXC     ,IXT    ,IXP    ,IXR     ,IXTG   ,
     3    ITAG    ,NODFT  ,NODLT  ,IPARG   ,EV     ,ITASK  ,
     4    ITAGL   ,IXTG1  ,IAD_ELEM,FR_ELEM,ITAB   ,ITABM1 ,
     5    ADDCNEL ,CNEL   ,KXSP   ,ELBUF_TAB,TAGEL ,IEXLNK ,
     6    IGRNOD  ,DD_R2R,DD_R2R_ELEM,SDD_R2R_ELEM,IDEL7NOK_SAV,
     7    IDEL7NOK_R2R,TAGTRIMC,TAGTRIMTG,S_ELEM_STATE,ELEM_STATE,
     8    SHOOT_STRUCT,GLOBAL_NB_ELEM_OFF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE RAD2R_MOD     
      USE REMESH_MOD
      USE GROUPDEF_MOD
      USE SHOOTING_NODE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "rad2r_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   LINDIDEL, LBUFIDEL,
     .   IXS(NIXS,*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .   IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXTG1(4,*),ITAG(*),
     .   IPARG(NPARG,*), NODFT,NODLT,ITASK, ITAGL (*),
     .   IAD_ELEM(2,*),FR_ELEM(*),ITAB(*),ITABM1(*),
     .   ADDCNEL(0:*),CNEL(0:*),KXSP(NISP,*),
     .   TAGEL(*),
     .   IEXLNK(NR2R,NR2RLNK),DD_R2R(NSPMD+1,*), 
     .   TAGTRIMC(*),TAGTRIMTG(*),
     .   DD_R2R_ELEM(*),SDD_R2R_ELEM,IDEL7NOK_SAV,IDEL7NOK_R2R
       my_real
     .   GEO(NPROPG,*), EV(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
!
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      INTEGER, INTENT(in) :: S_ELEM_STATE   ! size of ELEM_STATE
      LOGICAL, DIMENSION(S_ELEM_STATE), INTENT(inout) :: ELEM_STATE ! boolean : true if element is ON, false if element is OFF
      INTEGER, DIMENSION(NTHREAD), INTENT(inout) :: GLOBAL_NB_ELEM_OFF
      TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NG, K, ITY, MLW, NEL, NFT, ISOLNOD, LFT, LLT,
     .        KAD, NPT, IHBE, JD(50), KD(50), JFI, KFI, NRTM, NRTS,
     .        NTY, NSN, ISTRA, N, IDEL, NMN,ILEV,
     .        N1, N2, N3, N4, SIZE, LENR, IDB, IDBS, INC, IDELKEEP,
     .        IDEB, OFQ, OFC, OFT, OFP, OFR, OFTG, OFUR, ICNOD, IE,
     .        NLINSA, NLINMA, NSNE, NMNE, IEXPAN, IRSIZE,
     .        IRECV(NSPMD),SIZ,J,R2R_NUMEL,TAGEL_R2R_ISPMD(NSPMD+1),
     .        IPARTR2R,NTAGEL_R2R_RECV,NTAGEL_R2R_SEND,NTAGEL_R2R_SENDG,
     .        TAGEL_SIZE,LEVEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGEL_R2R_RECV,TAGEL_R2R_SENDG
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_ELEM_INDEX
      INTEGER :: SHIFT
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2      
C-----------------------------------------------
      ! allocation of local list of deactivated element
      ALLOCATE( LOCAL_ELEM_INDEX(S_ELEM_STATE) )
      GLOBAL_NB_ELEM_OFF(ITASK+1) = 0
      LFT = NODFT
      LLT = NODLT
      NTAGEL_R2R_SEND = 0
      NTAGEL_R2R_RECV = 0
      TAGEL_SIZE = 0
C
      DO I = LFT, LLT
        ITAG(I) = 0
      ENDDO
C
#include "vectorize.inc"
      DO I = LFT, LLT
        ITAG(NUMNOD+I) = 0
      ENDDO
C
      CALL MY_BARRIER()
C
      OFQ=NUMELS
      OFC=OFQ+NUMELQ
      OFT=OFC+NUMELC
      OFP=OFT+NUMELT
      OFR=OFP+NUMELP
      OFTG=OFR+NUMELR
      OFUR=OFTG+NUMELTG
C      
!$OMP DO

       DO NG = 1,NGROUP
          GBUF => ELBUF_TAB(NG)%GBUF
          ITY   =IPARG(5,NG)
          MLW     = IPARG(1,NG)
          NEL     = IPARG(2,NG)
          NFT     = IPARG(3,NG)
          KAD     = IPARG(4,NG)
          NPT     = IPARG(6,NG)
          ICNOD   = IPARG(11,NG)
          ISTRA   = IPARG(44,NG)
          IHBE    = IPARG(23,NG)
          ISOLNOD = IPARG(28,NG)
          IEXPAN  = IPARG(49,NG)
          IPARTR2R = IPARG(77,NG)
          IF (IHBE == 101) THEN
            IHBE=1
          ELSEIF(IHBE == 102) THEN
            IHBE=0
          ELSEIF(IHBE == 112) THEN
            IHBE=0
          ENDIF
          LFT   = 1
          LLT   = NEL
          IF(ITY == 1) THEN
           IF (MLW/=0) THEN
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I
              IF (ABS(GBUF%OFF(I)) == ONE .OR.
     .            ABS(GBUF%OFF(I)) == TWO) THEN
                TAGEL(IE)=1
#include "lockon.inc"
                DO K=2,9
                  N = IXS(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
#include "lockoff.inc"
              ELSE
                IF ((R2R_SIU == 1).AND.(TAGEL(IE) > -1)) THEN
                  CALL R2R_TAGEL(NTAGEL_R2R_SEND,IXS(11,NFT+I),ITAB(IXS(2,NFT+I)),ITY,
     .                           OFUR,TAGEL_SIZE) 
                ENDIF
                TAGEL(IE)=-1
                IF(ELEM_STATE(IE)) THEN
                    GLOBAL_NB_ELEM_OFF(ITASK+1) = GLOBAL_NB_ELEM_OFF(ITASK+1) + 1
                    LOCAL_ELEM_INDEX(GLOBAL_NB_ELEM_OFF(ITASK+1)) = IE
                ENDIF
                ELEM_STATE(IE) = .FALSE.
#include "lockon.inc"
                DO K=2,9
                  N = IXS(K,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDDO
#include "lockoff.inc"
              ENDIF
            ENDDO
           ELSEIF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN         
C Void elements never break and doesn't have ELBUF
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I
              TAGEL(IE)=1
              DO K=2,9
                N = IXS(K,NFT+I)
                ITAG(N) = 1
              ENDDO
            ENDDO
           ENDIF
C
            IF(ISOLNOD == 10) THEN   
             IF(MLW/=0)THEN
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                IF(ABS(GBUF%OFF(I)) == ONE.OR.
     .             ABS(GBUF%OFF(I)) == TWO) THEN
                  DO K=1,6
                    N = IXS10(K,NFT+I-NUMELS8)
                    ITAG(N) = 1
                  ENDDO
                ELSE
                  DO K=1,6
                    N = IXS10(K,NFT+I-NUMELS8)
                    ITAG(NUMNOD+N) = 1
                  ENDDO
                ENDIF
              ENDDO
             ELSEIF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN
C Void elements never break and doesn't have ELBUF
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                  DO K=1,6
                    N = IXS10(K,NFT+I-NUMELS8)
                    ITAG(N) = 1
                  ENDDO
                ENDDO
             ENDIF
            ELSEIF(ISOLNOD == 20) THEN            
             IF(MLW/=0)THEN
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                IF(ABS(GBUF%OFF(I)) == ONE.OR.
     .             ABS(GBUF%OFF(I)) == TWO) THEN
                  DO K=1,12
                    N = IXS20(K,NFT+I-NUMELS8-NUMELS10)
                    ITAG(N) = 1
                  ENDDO
                ELSE
                  DO K=1,12
                    N = IXS20(K,NFT+I-NUMELS8-NUMELS10)
                    ITAG(NUMNOD+N) = 1
                  ENDDO
                ENDIF
              ENDDO
             ELSEIF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN 
C Void elements never break and doesn't have ELBUF
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                DO K=1,12
                  N = IXS20(K,NFT+I-NUMELS8-NUMELS10)
                  ITAG(N) = 1
                ENDDO
              ENDDO
             ENDIF
            ELSEIF(ISOLNOD == 16) THEN       
             IF(MLW/=0)THEN
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                IF(ABS(GBUF%OFF(I)) == ONE.OR.
     .             ABS(GBUF%OFF(I)) == TWO) THEN
                  DO K=1,8
                    N = IXS16(K,NFT+I-NUMELS8-NUMELS10-NUMELS20)
                    ITAG(N) = 1
                  ENDDO
                ELSE
                  DO K=1,8
                    N = IXS16(K,NFT+I-NUMELS8-NUMELS10-NUMELS20)
                    ITAG(NUMNOD+N) = 1
                  ENDDO
                ENDIF
              ENDDO
             ELSEIF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN 
C Void elements never break and doesn't have ELBUF
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I
                DO K=1,8
                  N = IXS16(K,NFT+I-NUMELS8-NUMELS10-NUMELS20)
                  ITAG(N) = 1
                ENDDO
              ENDDO
             ENDIF
            ENDIF
C
          ELSEIF(ITY == 2) THEN
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFQ
              IF(ABS(GBUF%OFF(I))>=ONE) THEN
               TAGEL(IE)=1
                DO K=2,5
                  N = IXQ(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
              ELSE
                IF ((R2R_SIU == 1).AND.(TAGEL(IE) > -1)) THEN
                  CALL R2R_TAGEL(NTAGEL_R2R_SEND,IXQ(7,NFT+I),ITAB(IXQ(2,NFT+I)),ITY,
     .                           OFUR,TAGEL_SIZE)
                ENDIF
                TAGEL(IE)=-1
                IF(ELEM_STATE(IE)) THEN
                    GLOBAL_NB_ELEM_OFF(ITASK+1) = GLOBAL_NB_ELEM_OFF(ITASK+1) + 1
                    LOCAL_ELEM_INDEX(GLOBAL_NB_ELEM_OFF(ITASK+1)) = IE
                ENDIF
                ELEM_STATE(IE) = .FALSE.
                DO K=2,5
                  N = IXQ(K,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDDO
              ENDIF
            ENDDO
C    
          ELSEIF(ITY == 3)THEN
           IF(MLW/=0)THEN
#include "vectorize.inc"
            DO I = LFT,LLT 
              IE = NFT+I+OFC
              IF(NADMESH/=0) THEN
                IF(ABS(GBUF%OFF(I))>=ONE.AND.TAGTRIMC(IE)==0)THEN  
                  TAGEL(IE)=1
                  DO K=2,5
                     N = IXC(K,NFT+I)
                     ITAG(N) = 1   
                 ENDDO   
                ENDIF               
              ELSEIF(ABS(GBUF%OFF(I))>=ONE) THEN
                TAGEL(IE)=1
                DO K=2,5
                  N = IXC(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
              ELSE
                IF ((R2R_SIU == 1).AND.(TAGEL(IE) > -1)) THEN
                  CALL R2R_TAGEL(NTAGEL_R2R_SEND,IXC(7,NFT+I),ITAB(IXC(2,NFT+I)),ITY,
     .                           OFUR,TAGEL_SIZE)                                     
                ENDIF 
                TAGEL(IE)=-1
                IF(ELEM_STATE(IE)) THEN
                    GLOBAL_NB_ELEM_OFF(ITASK+1) = GLOBAL_NB_ELEM_OFF(ITASK+1) + 1
                    LOCAL_ELEM_INDEX(GLOBAL_NB_ELEM_OFF(ITASK+1)) = IE
                ENDIF
                ELEM_STATE(IE) = .FALSE.
                DO K=2,5
                  N = IXC(K,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDDO
              ENDIF
            ENDDO
           ELSEIF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN
C Void elements never break and doesn't have ELBUF
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFC
              TAGEL(IE)=1
               DO K=2,5
                  N = IXC(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
            ENDDO
           ENDIF
C
          ELSEIF(ITY == 4)THEN
            IF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFT
              IF (ABS(GBUF%OFF(I)) >= ONE) THEN
                TAGEL(IE)=1
                DO K=2,3
                  N = IXT(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
              ELSE
                IF ((R2R_SIU == 1).AND.(TAGEL(IE) > -1)) THEN
                  CALL R2R_TAGEL(NTAGEL_R2R_SEND,IXT(5,NFT+I),ITAB(IXT(2,NFT+I)),ITY,
     .                           OFUR,TAGEL_SIZE)                                      
                ENDIF
                TAGEL(IE)=-1
                IF(ELEM_STATE(IE)) THEN
                    GLOBAL_NB_ELEM_OFF(ITASK+1) = GLOBAL_NB_ELEM_OFF(ITASK+1) + 1
                    LOCAL_ELEM_INDEX(GLOBAL_NB_ELEM_OFF(ITASK+1)) = IE
                ENDIF
                ELEM_STATE(IE) = .FALSE.
                DO K=2,3
                  N = IXT(K,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDDO
              ENDIF
            ENDDO
            ENDIF
C
          ELSEIF(ITY == 5)THEN
            IF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFP
              IF (ABS(GBUF%OFF(I)) >= ONE) THEN
                TAGEL(IE)=1
                DO K=2,3
                  N = IXP(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
              ELSE
                IF ((R2R_SIU == 1).AND.(TAGEL(IE) > -1)) THEN
                  CALL R2R_TAGEL(NTAGEL_R2R_SEND,IXP(6,NFT+I),ITAB(IXP(2,NFT+I)),ITY,
     .                           OFUR,TAGEL_SIZE)                                      
                ENDIF
                TAGEL(IE)=-1
                IF(ELEM_STATE(IE)) THEN
                    GLOBAL_NB_ELEM_OFF(ITASK+1) = GLOBAL_NB_ELEM_OFF(ITASK+1) + 1
                    LOCAL_ELEM_INDEX(GLOBAL_NB_ELEM_OFF(ITASK+1)) = IE
                ENDIF
                ELEM_STATE(IE) = .FALSE.
                DO K=2,3
                  N = IXP(K,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDDO
              ENDIF
            ENDDO
            ENDIF
C
          ELSEIF(ITY == 6)THEN
            IF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFR
              IF (ABS(GBUF%OFF(I)) >= ONE) THEN
                TAGEL(IE)=1
                DO K=2,3
                  N = IXR(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
                IF(NINT(GEO(12,IXR(1,NFT+I))) == 12) THEN
                  N = IXR(4,NFT+I)
                  ITAG(N) = 1
                ENDIF
              ELSE
                IF ((R2R_SIU == 1).AND.(TAGEL(IE) > -1)) THEN
                 CALL R2R_TAGEL(NTAGEL_R2R_SEND,IXR(NIXR,NFT+I),ITAB(IXR(2,NFT+I)),ITY,
     .                          OFUR,TAGEL_SIZE)                                       
                ENDIF
                TAGEL(IE)=-1
                IF(ELEM_STATE(IE)) THEN
                    GLOBAL_NB_ELEM_OFF(ITASK+1) = GLOBAL_NB_ELEM_OFF(ITASK+1) + 1
                    LOCAL_ELEM_INDEX(GLOBAL_NB_ELEM_OFF(ITASK+1)) = IE
                ENDIF
                ELEM_STATE(IE) = .FALSE.
                DO K=2,3
                  N = IXR(K,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDDO
                IF(NINT(GEO(12,IXR(1,NFT+I))) == 12) THEN
                  N = IXR(4,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDIF
               ENDIF
            ENDDO
            ENDIF
C
          ELSEIF(ITY == 7)THEN
           IF(MLW/=0)THEN
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFTG

              IF(NADMESH/=0) THEN
                IF(ABS(GBUF%OFF(I))>=ONE.AND.TAGTRIMTG(IE)==0)THEN            
                  TAGEL(IE)=1
                  DO K=2,4
                     N = IXTG(K,NFT+I)
                     ITAG(N) = 1   
                  ENDDO
                ENDIF                  
              ELSEIF(ABS(GBUF%OFF(I))>=ONE) THEN
                TAGEL(IE)=1
                DO K=2,4
                  N = IXTG(K,NFT+I)
                  ITAG(N) = 1
                ENDDO
              ELSE
                IF ((R2R_SIU == 1).AND.(TAGEL(IE) > -1)) THEN
                CALL R2R_TAGEL(NTAGEL_R2R_SEND,IXTG(6,NFT+I),ITAB(IXTG(2,NFT+I)),ITY,
     .                         OFUR,TAGEL_SIZE)                                      
                ENDIF
                TAGEL(IE)=-1
                IF(ELEM_STATE(IE)) THEN
                    GLOBAL_NB_ELEM_OFF(ITASK+1) = GLOBAL_NB_ELEM_OFF(ITASK+1) + 1
                    LOCAL_ELEM_INDEX(GLOBAL_NB_ELEM_OFF(ITASK+1)) = IE
                ENDIF
                ELEM_STATE(IE) = .FALSE.
                DO K=2,4
                  N = IXTG(K,NFT+I)
                  ITAG(NUMNOD+N) = 1
                ENDDO
              ENDIF
            ENDDO
           ELSEIF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN
C Void elements never break and doesn't have ELBUF
#include "vectorize.inc"
            DO I = LFT,LLT
              IE = NFT+I+OFTG
              TAGEL(IE)=1
              DO K=2,4
                N = IXTG(K,NFT+I)
                ITAG(N) = 1
              ENDDO
            ENDDO
           ENDIF
C
            IF(ICNOD == 6) THEN   
             IF(MLW/=0)THEN
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I+OFTG
                IF(ABS(GBUF%OFF(I))>=ONE) THEN
                  DO K=1,3
                    N = IXTG1(K,NFT+I-NUMELTG+NUMELTG6)
                    ITAG(N) = 1
                  ENDDO
                ELSE
                  DO K=1,3
                    N = IXTG1(K,NFT+I-NUMELTG+NUMELTG6)
                    ITAG(NUMNOD+N) = 1
                  ENDDO
                ENDIF
              ENDDO
             ELSEIF ((R2R_SIU == 0).OR.(IPARTR2R > 0)) THEN
C Void elements never break and doesn't have ELBUF
#include "vectorize.inc"
              DO I = LFT,LLT
                IE = NFT+I+OFTG
                DO K=1,3
                  N = IXTG1(K,NFT+I-NUMELTG+NUMELTG6)
                  ITAG(N) = 1
                ENDDO
              ENDDO
             ENDIF
            END IF
C
 	    ELSEIF(ITY == 51) THEN
#include "vectorize.inc"
 	      DO I = LFT,LLT
 	        IF(ABS(GBUF%OFF(I))>=ONE) THEN
 		         N = KXSP(3,NFT+I)
 		         ITAG(N) = 1
 	        ELSE
 		         N = KXSP(3,NFT+I)
 		         ITAG(NUMNOD+N) = 1
 	        ENDIF
 	      END DO
            ENDIF
        ENDDO

!$OMP END DO

        IF(ITASK==0) THEN            
            IF(ALLOCATED( SHOOT_STRUCT%GLOBAL_ELEM_INDEX ) ) DEALLOCATE( SHOOT_STRUCT%GLOBAL_ELEM_INDEX )
            ! compute the total number of new deactivated element
            SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX = 0
            DO I=1,NTHREAD
                SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX = SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX + GLOBAL_NB_ELEM_OFF(I)
            ENDDO
            ! allocate the array "list of new deactivated element"
            ALLOCATE( SHOOT_STRUCT%GLOBAL_ELEM_INDEX(SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX) ) 
        ENDIF
        CALL MY_BARRIER( )

        ! omp reduction of "list of new deactivated elements"
        SHIFT = 0
        DO I=1,ITASK
                SHIFT = SHIFT + GLOBAL_NB_ELEM_OFF(I)
        ENDDO
        SHOOT_STRUCT%GLOBAL_ELEM_INDEX(1+SHIFT:GLOBAL_NB_ELEM_OFF(ITASK+1)+SHIFT) =
     .            LOCAL_ELEM_INDEX(1:GLOBAL_NB_ELEM_OFF(ITASK+1))


C      CALL MY_BARRIER() => remplace par barriere implicite sur do //
C
C SPMD SPECIFIQUE : ECHANGE ITAG NOEUDS FRONTIERES
C
      IF (NSPMD > 1) THEN

C Partie non parallele
!$OMP SINGLE

        SIZE = 2
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_IDEL(ITAG,IAD_ELEM,FR_ELEM,SIZE,LENR)

C Fin Partie non parallele
!$OMP END SINGLE

      ENDIF

C--------------------------------
C R2R part : only for IDEL INTER
C-------------------------------

C Partie non parallele
!$OMP SINGLE
      IF (R2R_SIU == 1.AND.IDEL7NOK==1) THEN
C-----------------------------------------------------------------
C       Envoi de ITAG et TAGEL pour multidomaines
C-----------------------------------------------------------------
        IF (IDEL7NOK_SAV > 0) THEN
C       communication partie nodale ITAGq
          CALL SEND_SHMBUF_C(IDEL7NOK,1)
          CALL R2R_EXCH_ITAG(IEXLNK,IGRNOD,ITAG,0)
C       communication partie elementaire TAGEL
          NTAGEL_R2R_SENDG = NTAGEL_R2R_SEND
          CALL SPMD_ALLGLOB_ISUM9(NTAGEL_R2R_SENDG,1)
          IF (NTAGEL_R2R_SENDG > 0) THEN
            IF (NSPMD > 1) THEN
              TAGEL_R2R_ISPMD(:)=0
              TAGEL_R2R_ISPMD(ISPMD+1) = 3*NTAGEL_R2R_SEND 
              CALL SPMD_ALLGLOB_ISUM9(TAGEL_R2R_ISPMD,NSPMD)
              ALLOCATE(TAGEL_R2R_SENDG(3*NTAGEL_R2R_SENDG))
              CALL SPMD_R2R_TAGEL(TAGEL_R2R_SENDG,TAGEL_R2R_SEND,TAGEL_R2R_ISPMD)
              CALL SPMD_IBCAST(TAGEL_R2R_SENDG,TAGEL_R2R_SENDG,3*NTAGEL_R2R_SENDG,1,0,2)
              CALL EXCH_TAGEL_C(NTAGEL_R2R_SENDG,TAGEL_R2R_SENDG,0)
              DEALLOCATE(TAGEL_R2R_SENDG)
            ELSE
              CALL EXCH_TAGEL_C(NTAGEL_R2R_SENDG,TAGEL_R2R_SEND,0)
            ENDIF
          ENDIF
        ENDIF
C-----------------------------------------------------------------
C       SYNCRO
C-----------------------------------------------------------------
        IF (NCYCLE == 0) THEN
           LENR = 2209
           CALL SEND_IBUF_C(LENR,1)
           CALL GET_IBUF_C(LENR,1)
        ENDIF
C-----------------------------------------------------------------
C       Assemblage ITAG pour multidomaines
C-----------------------------------------------------------------
        CALL R2R_EXCH_ITAG(IEXLNK,IGRNOD,ITAG,1)
        IF (SDD_R2R_ELEM > 0) THEN
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXCH_R2R_ITAG(ITAG,IAD_ELEM,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
        ENDIF
C-----------------------------------------------------------------
C       Reception TAGEL pour multidomaines
C-----------------------------------------------------------------
        IF (IDEL7NOK_R2R > 0) THEN
          CALL GET_SHMBUF_C(NTAGEL_R2R_RECV,4)
          NTAGEL_R2R_RECV = NTAGEL_R2R_RECV / 3     
          IF (NTAGEL_R2R_RECV > 0) THEN
            ALLOCATE(TAGEL_R2R_RECV(3*NTAGEL_R2R_RECV))
            CALL EXCH_TAGEL_C(NTAGEL_R2R_RECV,TAGEL_R2R_RECV,1)
          ENDIF
          DO I=1,NTAGEL_R2R_RECV
            N1 = SYSFUS2(TAGEL_R2R_RECV((I-1)*3+2),ITABM1,NUMNOD)          
            IF (N1 > 0) THEN
              ITY = TAGEL_R2R_RECV((I-1)*3+3)
              DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1            
                IE = CNEL(J)
C
                IF ((ITY == 1).AND.(IE<=OFQ)) THEN
                  R2R_NUMEL = IXS(11,IE)
                ELSEIF ((ITY == 2).AND.((IE > OFQ).AND.(IE<=OFC))) THEN                
                  R2R_NUMEL = IXQ(7,IE-OFQ)            
                ELSEIF ((ITY == 3).AND.((IE > OFC).AND.(IE<=OFT))) THEN
                  R2R_NUMEL = IXC(7,IE-OFC)
                ELSEIF ((ITY == 4).AND.((IE > OFT).AND.(IE<=OFP))) THEN               
                  R2R_NUMEL = IXT(5,IE-OFT)
                ELSEIF ((ITY == 5).AND.((IE > OFP).AND.(IE<=OFR))) THEN
                  R2R_NUMEL = IXP(6,IE-OFP)              
                ELSEIF ((ITY == 6).AND.((IE > OFR).AND.(IE<=OFTG))) THEN
                  R2R_NUMEL = IXR(NIXR,IE-OFR)              
                ELSEIF ((ITY == 7).AND.((IE > OFTG).AND.(IE<=OFUR))) THEN
                  R2R_NUMEL = IXTG(6,IE-OFTG)
                ENDIF
C
                IF (R2R_NUMEL == TAGEL_R2R_RECV((I-1)*3+1)) THEN
                  TAGEL(IE) = -1
                  EXIT
                ENDIF
              ENDDO
            ENDIF   
          ENDDO
C       RAZ de IDEL7NOK_R2R et NTAGEL_R2R_RECV
          IDEL7NOK_R2R = 0
          NTAGEL_R2R_RECV = 0
          CALL SEND_SHMBUF_C(IDEL7NOK_R2R,2)
          CALL SEND_SHMBUF_C(NTAGEL_R2R_RECV,4)
          IF(ALLOCATED(TAGEL_R2R_RECV)) DEALLOCATE(TAGEL_R2R_RECV)
        ENDIF
C--------------------------------------------------------
      ENDIF
C Fin Partie non parallele
!$OMP END SINGLE
      ! deallocation of local list of deactivated element
      DEALLOCATE( LOCAL_ELEM_INDEX )
C
      RETURN
      END


Chd|====================================================================
Chd|  CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CHK11MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|        CHK11MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|        CHK20EMSR3N                   source/interfaces/interf/chkstfn3.F
Chd|        CHK20EMSR3NB                  source/interfaces/interf/chkstfn3.F
Chd|        CHK20MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|        CHK20MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|        CHK23MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|        CHK23MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|        CHK2MSR3N                     source/interfaces/interf/chkstfn3.F
Chd|        CHK2MSR3NB                    source/interfaces/interf/chkstfn3.F
Chd|        CHK2MSR3NP                    source/interfaces/interf/chkstfn3.F
Chd|        CHKMSR3N                      source/interfaces/interf/chkstfn3.F
Chd|        CHKMSR3NB                     source/interfaces/interf/chkstfn3.F
Chd|        CHKSLV3                       source/interfaces/interf/chkstfn3.F
Chd|        CHKSLV3B                      source/interfaces/interf/chkstfn3.F
Chd|        CHKSLV3C                      source/interfaces/interf/chkstfn3.F
Chd|        CHKSLV3_T24                   source/interfaces/interf/chkstfn3.F
Chd|        SETMSR2                       source/interfaces/interf/chkstfn3.F
Chd|        SETMSR3                       source/interfaces/interf/chkstfn3.F
Chd|        SPMD_EXCHMSR_IDEL             source/mpi/interfaces/spmd_exchmsr_idel.F
Chd|        SPMD_INIT_IDEL                source/mpi/interfaces/spmd_init_idel.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CHKSTFN3N(
     1    IPARI   ,GEO     ,IXS    ,IXQ       ,IXC      ,IXT     ,
     2    IXP     ,IXR     ,IXTG   ,ITAG      ,IPARG    ,ITASK   ,
     3    NEWFRONT,ITAGL   ,MS     ,IN        ,ADM      ,ITAB    ,
     4    ITABM1  ,ADDCNEL ,CNEL   ,IND       ,NINDEX1  ,NINDEX2 ,
     5    NINDEX3 ,NINDEX4 ,TAGEL   ,INT24USE,IBUFSEGLO,INDSEGLO ,
     6    IBUFS ,INTBUF_TAB,IAD_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE INTBUFDEF_MOD    
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   IPARI(NPARI,*), LINDIDEL, LBUFIDEL,
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),ITAG(*),
     .   IPARG(NPARG,*), ITASK, NEWFRONT(*),ITAGL (*),
     .   ITAB(*),ITABM1(*),ADDCNEL(0:*),CNEL(0:*),
     .   NINDEX1(*), NINDEX2(*),NINDEX3(*), NINDEX4(*),
     .   IND(*), TAGEL(*),INT24USE,IBUFSEGLO(*),INDSEGLO(*),
     .   IBUFS(*)
       my_real
     .   GEO(NPROPG,*), MS(*),IN(*), ADM(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NG, K, ITY, MLW, NEL, NFT, ISOLNOD,
     .        KAD, NPT, IHBE, JD(50), KD(50), JFI, KFI, NRTM, NRTS,
     .        NTY, NSN, ISTRA, N, IDEL, NMN,ILEV,
     .        N1, N2, N3, N4, SIZE, LENR, IDB, IDBS, INC, IDELKEEP,
     .        IDEB, OFQ, OFC, OFT, OFP, OFR, OFTG, OFUR, ICNOD, IE,
     .        NLINSA, NLINMA, NSNE, NMNE, IEXPAN, IRSIZE,
     .        IRECV(NSPMD),SIZ,J,R2R_NUMEL,TAGEL_R2R_ISPMD(NSPMD+1),
     .        IPARTR2R,NTAGEL_R2R_RECV,NTAGEL_R2R_SEND,NTAGEL_R2R_SENDG,
     .        TAGEL_SIZE,LEVEL
      INTEGER, DIMENSION(:),ALLOCATABLE ::IBUFSEGLO_SAV,INDSEGLO_sav
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2      
C-----------------------------------------------
C
      OFQ=NUMELS
      OFC=OFQ+NUMELQ
      OFT=OFC+NUMELC
      OFP=OFT+NUMELT
      OFR=OFP+NUMELP
      OFTG=OFR+NUMELR
      OFUR=OFTG+NUMELTG
C

      IDB = 1
      IDBS = 1
      DO NG=1,NINTER
       NTY   =IPARI(7,NG)
       IDEL=IPARI(17,NG)
       IDELKEEP=IPARI(61,NG)
       IF(INT24USE==1.OR.NINTER25/=0)THEN
!$OMP SINGLE
         INDSEGLO(NG+1)=INDSEGLO(NG)
!$OMP END SINGLE
      ENDIF

       IF((NTY==7.OR.NTY==10.OR.NTY==22.OR.NTY==24.OR.NTY==25).AND.
     .     IDEL>=1) THEN
         NSN = IPARI(5,NG)
         IF(IDELKEEP /= 1) THEN
           IF(NTY==24)THEN
C T24 E2E requires specific treatments for check 
C E2E have fictive nodes with NSV  > NUMNOD
             CALL CHKSLV3_T24(
     .           NSN         ,INTBUF_TAB(NG)%NSV,INTBUF_TAB(NG)%STFNS,ITAG,ITASK,
     .           INTBUF_TAB(NG)%IS2SE,INTBUF_TAB(NG)%IRTSE,NEWFRONT(NG))
           ENDIF
         ENDIF
         NMN   =IPARI(6,NG)
         NRTM  =IPARI(4,NG)
         INC=4
         IF(IDEL == 1) THEN
!$OMP SINGLE
           NINDEX1(NG) = 0
!$OMP END SINGLE
           CALL CHKMSR3NB(
     1   NMN          ,INTBUF_TAB(NG)%MSR ,ITAG          ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2   NRTM         ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3   IXTG         ,IXQ          ,IPARG         ,ITAGL        ,
     3   NTY          ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5   OFC          ,OFT          ,OFTG          ,OFUR ,NINDEX1(NG)  ,
     6   IBUFS(IDBS+4),IND(IDB)     ,TAGEL         ,NG   ,INTBUF_TAB(NG)%MSEGLO,
     7   INTBUF_TAB(NG)%MVOISIN,INDSEGLO     ,IBUFSEGLO)
         ELSEIF(IDEL == 2)THEN
!$OMP SINGLE
            NINDEX1(NG) = 0
!$OMP END SINGLE
           CALL CHKMSR3N( 
     1   NMN          ,INTBUF_TAB(NG)%MSR ,ITAG           ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2   NRTM         ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3   IXTG         ,IXQ          ,IPARG         ,ITAGL        ,
     4   NTY          ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5   OFC          ,OFT          ,OFTG          ,OFUR ,NINDEX1(NG)  ,
     6   IBUFS(IDBS+4),IND(IDB)     ,TAGEL         ,NG,
     7   INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO     ,IBUFSEGLO)
         END IF
!$OMP SINGLE
         NINDEX2(NG)=0
!$OMP END SINGLE

         IF(NINDEX1(NG)+NINDEX2(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=NTY
           IBUFS(IDBS+2)=NINDEX1(NG)
           IBUFS(IDBS+3)=NINDEX2(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX1(NG)+NINDEX2(NG)) + 4
           IDB  = IDB  + NINDEX1(NG) + NINDEX2(NG)
         END IF
       ELSEIF(NTY == 23.AND.IDEL>=1) THEN
         NSN = IPARI(5,NG)
	 IF(IDELKEEP /= 1) CALL CHKSLV3(
     .     NSN         ,INTBUF_TAB(NG)%NSV,INTBUF_TAB(NG)%STFNS,ITAG,ITASK,
     .     NEWFRONT(NG))
         NMN   =IPARI(6,NG)
         NRTM  =IPARI(4,NG)
         INC=4
         IF(IDEL == 1) THEN
           CALL CHK23MSR3NB(
     1   NMN          ,INTBUF_TAB(NG)%MSR ,ITAG    ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2   NRTM         ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3   IXTG         ,IXQ          ,IPARG         ,ITAGL        ,
     3   NTY          ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5   OFC          ,OFT          ,OFTG          ,OFUR ,NINDEX1(NG)  ,
     6   IBUFS(IDBS+4),IND(IDB)     ,TAGEL         )
         ELSEIF(IDEL == 2)THEN
	   CALL CHK23MSR3N(
     1   NMN	      ,INTBUF_TAB(NG)%MSR,ITAG	   ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2   NRTM	      ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC	       ,
     3   IXTG	      ,IXQ	    ,IPARG	   ,ITAGL	 ,
     4   NTY	      ,ITAB	    ,ITABM1	   ,CNEL ,ADDCNEL      ,
     5   OFC	      ,OFT	    ,OFTG	   ,OFUR ,NINDEX1(NG)  ,
     6   IBUFS(IDBS+4),IND(IDB)     ,TAGEL	   )
         END IF
         NINDEX2(NG)=0
         IF(NINDEX1(NG)+NINDEX2(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=NTY
           IBUFS(IDBS+2)=NINDEX1(NG)
           IBUFS(IDBS+3)=NINDEX2(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX1(NG)+NINDEX2(NG)) + 4
           IDB  = IDB  + NINDEX1(NG) + NINDEX2(NG)
         END IF
       ELSEIF((NTY == 11).AND.IDEL>=1) THEN
         NMN  =IPARI(6,NG)
         NSN  =IPARI(5,NG)
         NRTM =IPARI(4,NG)
         NRTS =IPARI(3,NG)
         INC=2
         IF(IDEL == 1) THEN
Cote main
           CALL CHK11MSR3NB(
     1     NMN    ,INTBUF_TAB(NG)%MSR ,ITAG          ,ITASK  ,INTBUF_TAB(NG)%IRECTM ,
     2     NRTM   ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS    ,IXC           ,
     3     IXTG   ,IXQ          ,IPARG         ,ITAGL         ,
     4     NTY    ,ITAB         ,ITABM1        ,NEWFRONT(NG),IXT      ,
     5     IXP    ,IXR          ,GEO           ,1      ,CNEL          ,
     6     ADDCNEL,OFC          ,OFT           ,OFTG   ,OFUR          ,
     7     OFR    ,OFP          ,NINDEX1(NG)   ,IBUFS(IDBS+4),IND(IDB),
     8     TAGEL  )
Cote secnd
           CALL CHK11MSR3NB(
     1     NSN    ,INTBUF_TAB(NG)%NSV ,ITAG          ,ITASK  ,INTBUF_TAB(NG)%IRECTS,
     2     NRTS   ,INTBUF_TAB(NG)%STFS,ITAG(NUMNOD+1),IXS    ,IXC          ,
     3     IXTG   ,IXQ          ,IPARG         ,ITAGL        ,
     4     NTY    ,ITAB         ,ITABM1        ,NEWFRONT(NG),IXT     ,
     5     IXP    ,IXR          ,GEO           ,2      ,CNEL         ,
     6     ADDCNEL,OFC          ,OFT           ,OFTG   ,OFUR         ,
     7     OFR    ,OFP          ,NINDEX2(NG)   ,
     +     IBUFS(IDBS+4+NINDEX1(NG)*INC), IND(IDB+NINDEX1(NG))       ,
     8     TAGEL  )
         ELSEIF(IDEL == 2)THEN
Cote main
           CALL CHK11MSR3N(
     1     NMN    ,INTBUF_TAB(NG)%MSR ,ITAG          ,ITASK  ,INTBUF_TAB(NG)%IRECTM ,
     2     NRTM   ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS    ,IXC           ,
     3     IXTG   ,IXQ          ,IPARG         ,ITAGL         ,
     4     NTY    ,NEWFRONT(NG) ,IXT           ,IXP    ,IXR           ,
     5     GEO    ,1            ,ITAB          ,ITABM1 ,CNEL          ,
     6     ADDCNEL,OFC          ,OFT           ,OFTG   ,OFUR          ,
     7     OFR    ,OFP          ,NINDEX1(NG)   ,IBUFS(IDBS+4),IND(IDB),
     8     TAGEL  )
Cote secnd
           CALL CHK11MSR3N(
     1     NSN    ,INTBUF_TAB(NG)%NSV ,ITAG          ,ITASK  ,INTBUF_TAB(NG)%IRECTS,
     2     NRTS   ,INTBUF_TAB(NG)%STFS,ITAG(NUMNOD+1),IXS    ,IXC          ,
     3     IXTG   ,IXQ          ,IPARG         ,ITAGL        ,
     4     NTY    ,NEWFRONT(NG) ,IXT           ,IXP    ,IXR          ,
     5     GEO    ,2            ,ITAB          ,ITABM1 ,CNEL         ,
     6     ADDCNEL,OFC          ,OFT           ,OFTG   ,OFUR         ,
     7     OFR    ,OFP          ,NINDEX2(NG)   ,
     +     IBUFS(IDBS+4+NINDEX1(NG)*INC), IND(IDB+NINDEX1(NG))       ,
     8     TAGEL  )
         END IF
         IF(NINDEX1(NG)+NINDEX2(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=NTY
           IBUFS(IDBS+2)=NINDEX1(NG)
           IBUFS(IDBS+3)=NINDEX2(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX1(NG)+NINDEX2(NG)) + 4
           IDB  = IDB  + NINDEX1(NG) + NINDEX2(NG)
         END IF
C------
       ELSEIF(NTY == 21.AND.IDEL>=1) THEN
         NSN = IPARI(5,NG)
         IF(IDELKEEP /= 1)
     .     CALL CHKSLV3B(NSN,INTBUF_TAB(NG)%NSV,INTBUF_TAB(NG)%STFNS,ITAG,ITASK)
C------
       ELSEIF(NTY == 20.AND.IDEL>=1) THEN
         NSN = IPARI(5,NG)
	 IF(IDELKEEP /= 1) CALL CHKSLV3C(
     .     NSN         ,INTBUF_TAB(NG)%NSV,INTBUF_TAB(NG)%STFA,ITAG,ITASK,
     .     NEWFRONT(NG),INTBUF_TAB(NG)%NLG)
         NMN   =IPARI(6,NG)
         NRTM  =IPARI(4,NG)
         INC=4
	 IF(IDEL == 1) THEN
	   CALL CHK20MSR3NB(
     1   NMN	      ,INTBUF_TAB(NG)%MSR,ITAG	   ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2   NRTM	      ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC	       ,
     3   IXTG	      ,IXQ	    ,IPARG	   ,ITAGL	 ,
     3   NTY	      ,ITAB	    ,ITABM1	   ,CNEL ,ADDCNEL      ,
     5   OFC	      ,OFT	    ,OFTG	   ,OFUR ,NINDEX1(NG)  ,
     6   IBUFS(IDBS+4),IND(IDB)     ,INTBUF_TAB(NG)%NLG ,TAGEL)
	 ELSEIF(IDEL == 2)THEN
	   CALL CHK20MSR3N(
     1   NMN	      ,INTBUF_TAB(NG)%MSR,ITAG	   ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2   NRTM	      ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC	       ,
     3   IXTG	      ,IXQ	    ,IPARG	   ,ITAGL	 ,
     4   NTY	      ,ITAB	    ,ITABM1	   ,CNEL ,ADDCNEL      ,
     5   OFC	      ,OFT	    ,OFTG	   ,OFUR ,NINDEX1(NG)  ,
     6   IBUFS(IDBS+4),IND(IDB)     ,INTBUF_TAB(NG)%NLG ,TAGEL)
         END IF
         NINDEX2(NG)=0
         IF(NINDEX1(NG)+NINDEX2(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=NTY
           IBUFS(IDBS+2)=NINDEX1(NG)
           IBUFS(IDBS+3)=NINDEX2(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX1(NG)+NINDEX2(NG)) + 4
           IDB  = IDB  + NINDEX1(NG) + NINDEX2(NG)
         END IF
C
C Rajout type20 partie edge
C
         NLINSA =IPARI(53,NG)
         NLINMA =IPARI(54,NG)
         NSNE   =IPARI(55,NG)
         NMNE   =IPARI(56,NG)
         INC=2
         IF(IDEL == 1) THEN
Cote main
	   CALL CHK20EMSR3NB(
     1     NMNE   ,INTBUF_TAB(NG)%MSRL,ITAG	       ,ITASK  ,INTBUF_TAB(NG)%IXLINM ,
     2     NLINMA ,INTBUF_TAB(NG)%STF,ITAG(NUMNOD+1),IXS    ,IXC	      ,
     3     IXTG   ,IXQ  	,IPARG         ,ITAGL	      ,
     4     NTY    ,ITAB 	,ITABM1        ,NEWFRONT(NG),IXT      ,
     5     IXP    ,IXR  	,GEO	       ,1      ,CNEL	      ,
     6     ADDCNEL,OFC  	,OFT	       ,OFTG   ,OFUR	      ,
     7     OFR    ,OFP  	,NINDEX3(NG)   ,IBUFS(IDBS+4),IND(IDB),
     8     INTBUF_TAB(NG)%NLG	,TAGEL)
Cote secnd
	   CALL CHK20EMSR3NB(
     1     NSNE   ,INTBUF_TAB(NG)%NSVL,ITAG	       ,ITASK  ,INTBUF_TAB(NG)%IXLINS,
     2     NLINSA ,INTBUF_TAB(NG)%STFS,ITAG(NUMNOD+1),IXS    ,IXC	     ,
     3     IXTG   ,IXQ  	,IPARG         ,ITAGL	     ,
     4     NTY    ,ITAB 	,ITABM1        ,NEWFRONT(NG),IXT     ,
     5     IXP    ,IXR  	,GEO	       ,2      ,CNEL	     ,
     6     ADDCNEL,OFC  	,OFT	       ,OFTG   ,OFUR	     ,
     7     OFR    ,OFP  	,NINDEX4(NG)   ,
     +     IBUFS(IDBS+4+NINDEX3(NG)*INC), IND(IDB+NINDEX3(NG))       ,
     8     INTBUF_TAB(NG)%NLG	,TAGEL)
         ELSEIF(IDEL == 2)THEN
Cote main
	   CALL CHK20EMSR3N(
     1     NMNE   ,INTBUF_TAB(NG)%MSRL,ITAG	       ,ITASK  ,INTBUF_TAB(NG)%IXLINM ,
     2     NLINMA ,INTBUF_TAB(NG)%STF,ITAG(NUMNOD+1),IXS    ,IXC	      ,
     3     IXTG   ,IXQ  	,IPARG         ,ITAGL	      ,
     4     NTY    ,NEWFRONT(NG) ,IXT	       ,IXP    ,IXR	      ,
     5     GEO    ,1		,ITAB	       ,ITABM1 ,CNEL	      ,
     6     ADDCNEL,OFC  	,OFT	       ,OFTG   ,OFUR	      ,
     7     OFR    ,OFP  	,NINDEX3(NG)   ,IBUFS(IDBS+4),IND(IDB),
     8     INTBUF_TAB(NG)%NLG	,TAGEL)
Cote secnd
	   CALL CHK20EMSR3N(
     1     NSNE   ,INTBUF_TAB(NG)%NSVL,ITAG	       ,ITASK  ,INTBUF_TAB(NG)%IXLINS,
     2     NLINSA ,INTBUF_TAB(NG)%STFS,ITAG(NUMNOD+1),IXS    ,IXC	     ,
     3     IXTG   ,IXQ  	,IPARG         ,ITAGL	     ,
     4     NTY    ,NEWFRONT(NG) ,IXT	       ,IXP    ,IXR	     ,
     5     GEO    ,2		,ITAB	       ,ITABM1 ,CNEL	     ,
     6     ADDCNEL,OFC  	,OFT	       ,OFTG   ,OFUR	     ,
     7     OFR    ,OFP  	,NINDEX4(NG)   ,
     +     IBUFS(IDBS+4+NINDEX3(NG)*INC), IND(IDB+NINDEX3(NG))       ,
     8     INTBUF_TAB(NG)%NLG	,TAGEL)
         END IF
C
         IF(NINDEX3(NG)+NINDEX4(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=-NTY             ! -20 pour reperage partie edge
           IBUFS(IDBS+2)=NINDEX3(NG)
           IBUFS(IDBS+3)=NINDEX4(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX3(NG)+NINDEX4(NG)) + 4
           IDB  = IDB  + NINDEX3(NG) + NINDEX4(NG)
         END IF
C------
       ELSEIF(NTY == 3.AND.IDEL>=1) THEN
         IF(ISPMD == 0) THEN
           NSN = IPARI(5,NG)
           IF(IDELKEEP /= 1) 
     .       CALL CHKSLV3B(NSN,INTBUF_TAB(NG)%NSV,INTBUF_TAB(NG)%STFNS,ITAG,ITASK)
           NMN   =IPARI(6,NG)
           IF(IDELKEEP /= 1)
     .       CALL CHKSLV3B(NMN,INTBUF_TAB(NG)%MSR,INTBUF_TAB(NG)%STFNM,ITAG,ITASK)
           NRTS  =IPARI(3,NG)
           NRTM  =IPARI(4,NG)
         ELSE   !  interface traitee par p0 uniquement
           NSN  = 0
           NMN  = 0
           NRTS = 0
           NRTM = 0
         END IF
         INC=4
         IF(IDEL == 1) THEN
C cote secnd
           CALL CHKMSR3NB(
     1       NSN      ,INTBUF_TAB(NG)%NSV,ITAG          ,ITASK,INTBUF_TAB(NG)%IRECTS,
     2       NRTS     ,INTBUF_TAB(NG)%STFS,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3       IXTG     ,IXQ          ,IPARG         ,ITAGL        ,
     3       NTY      ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5       OFC      ,OFT          ,OFTG          ,OFUR ,NINDEX1(NG)  ,
     6       IBUFS(IDBS+4),IND(IDB) ,TAGEL         ,NG   ,INTBUF_TAB(NG)%IRTLOS,
     7       INTBUF_TAB(NG)%ILOCM,INDSEGLO     ,IBUFSEGLO)
C cote main
           CALL CHKMSR3NB(
     1       NMN      ,INTBUF_TAB(NG)%MSR,ITAG          ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2       NRTM     ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3       IXTG     ,IXQ          ,IPARG         ,ITAGL        ,
     3       NTY      ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5       OFC      ,OFT          ,OFTG          ,OFUR ,NINDEX2(NG)  ,
     6       IBUFS(IDBS+4+NINDEX1(NG)*INC),IND(IDB+NINDEX1(NG)),TAGEL  ,
     7       NG   ,INTBUF_TAB(NG)%IRTLOS,INTBUF_TAB(NG)%ILOCM,INDSEGLO,IBUFSEGLO )
         ELSEIF(IDEL == 2)THEN
C cote secnd
           CALL CHKMSR3N(
     1       NSN      ,INTBUF_TAB(NG)%NSV,ITAG          ,ITASK,INTBUF_TAB(NG)%IRECTS,
     2       NRTS     ,INTBUF_TAB(NG)%STFS,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3       IXTG     ,IXQ          ,IPARG         ,ITAGL        ,
     4       NTY      ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5       OFC      ,OFT          ,OFTG          ,OFUR ,NINDEX1(NG)  ,
     6       IBUFS(IDBS+4),IND(IDB) ,TAGEL         ,NG,
     7   INTBUF_TAB(NG)%IRTLOS,INTBUF_TAB(NG)%ILOCM,INDSEGLO     ,IBUFSEGLO)
C cote main
           CALL CHKMSR3N(
     1       NMN      ,INTBUF_TAB(NG)%MSR,ITAG         ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2       NRTM     ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3       IXTG     ,IXQ          ,IPARG         ,ITAGL        ,
     4       NTY      ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5       OFC      ,OFT          ,OFTG          ,OFUR ,NINDEX2(NG)  ,
     6       IBUFS(IDBS+4+NINDEX1(NG)*INC),IND(IDB+NINDEX1(NG)),TAGEL  ,NG,
     7   INTBUF_TAB(NG)%IRTLOS,INTBUF_TAB(NG)%ILOCM,INDSEGLO     ,IBUFSEGLO)
         END IF
         IF(NINDEX1(NG)+NINDEX2(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=NTY
           IBUFS(IDBS+2)=NINDEX1(NG)
           IBUFS(IDBS+3)=NINDEX2(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX1(NG)+NINDEX2(NG)) + 4
           IDB  = IDB  + NINDEX1(NG) + NINDEX2(NG)
         END IF
       ELSEIF(NTY == 5.AND.IDEL>=1) THEN
         IF(ISPMD == 0) THEN
           NSN = IPARI(5,NG)
           IF(IDELKEEP /= 1) 
     .       CALL CHKSLV3B(NSN,INTBUF_TAB(NG)%NSV,INTBUF_TAB(NG)%STFNS,ITAG,ITASK)
           NMN   =IPARI(6,NG)
           NRTM  =IPARI(4,NG)
         ELSE
           NMN = 0
           NRTM = 0
         END IF
         INC=4
         IF(IDEL == 1) THEN
	   CALL CHKMSR3NB(
     1       NMN      ,INTBUF_TAB(NG)%MSR,ITAG	   ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2       NRTM     ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC	       ,
     3       IXTG     ,IXQ	    ,IPARG	   ,ITAGL	 ,
     3       NTY      ,ITAB	    ,ITABM1	   ,CNEL ,ADDCNEL      ,
     5       OFC      ,OFT	    ,OFTG	   ,OFUR ,NINDEX1(NG)  ,
     6       IBUFS(IDBS+4),IND(IDB) ,TAGEL	   ,NG   ,INTBUF_TAB(NG)%IRTLOS,
     7       INTBUF_TAB(NG)%ILOCM,INDSEGLO     ,IBUFSEGLO )
         ELSEIF(IDEL == 2)THEN
           CALL CHKMSR3N(
     1       NMN      ,INTBUF_TAB(NG)%MSR,ITAG          ,ITASK,INTBUF_TAB(NG)%IRECTM,
     2       NRTM     ,INTBUF_TAB(NG)%STFM,ITAG(NUMNOD+1),IXS  ,IXC          ,
     3       IXTG     ,IXQ          ,IPARG         ,ITAGL        ,
     4       NTY      ,ITAB         ,ITABM1        ,CNEL ,ADDCNEL      ,
     5       OFC      ,OFT          ,OFTG          ,OFUR ,NINDEX1(NG)  ,
     6       IBUFS(IDBS+4),IND(IDB) ,TAGEL         ,NG,
     7       INTBUF_TAB(NG)%IRTLOS,INTBUF_TAB(NG)%ILOCM,INDSEGLO     ,IBUFSEGLO)
         END IF
         NINDEX2(NG)=0
         IF(NINDEX1(NG)+NINDEX2(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=NTY
           IBUFS(IDBS+2)=NINDEX1(NG)
           IBUFS(IDBS+3)=NINDEX2(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX1(NG)+NINDEX2(NG)) + 4
           IDB  = IDB  + NINDEX1(NG) + NINDEX2(NG)
         END IF
       ELSEIF (NTY == 2 .AND. IDEL > 0) THEN
         NSN  = IPARI(5,NG)
         ILEV = IPARI(20,NG)
C
         INC=4
         IF (IDEL == 2) THEN 
           CALL CHK2MSR3N(
     1	    NSN  	,INTBUF_TAB(NG)%NSV ,ITAG,ITASK,INTBUF_TAB(NG)%IRECTM,
     2	    INTBUF_TAB(NG)%IRTLM,ITAG(NUMNOD+1),IXS	,IXC  ,IXTG	    ,
     3	    IXQ  	,IPARG       ,ITAGL,MS	    ,
     4	    IN		,INTBUF_TAB(NG)%SMAS ,INTBUF_TAB(NG)%SINER,ADM,CNEL      ,
     5	    ADDCNEL	,OFC	       ,OFT	,OFTG ,OFUR	    ,
     6     TAGEL   ,ILEV     )
         ELSEIF (IDEL == 1) THEN 
           CALL CHK2MSR3NB(
     1	    NSN  	,INTBUF_TAB(NG)%NSV ,ITAG	,ITASK,INTBUF_TAB(NG)%IRECTM,
     2	    INTBUF_TAB(NG)%IRTLM,ITAG(NUMNOD+1),IXS	,IXC  ,IXTG	    ,
     3	    IXQ  	,IPARG       ,ITAGL,MS	    ,
     4	    IN		,INTBUF_TAB(NG)%SMAS ,INTBUF_TAB(NG)%SINER,ADM,CNEL      ,
     5	    ADDCNEL	,OFC	       ,OFT	,OFTG ,OFUR	    ,
     6     NINDEX1(NG)  ,IBUFS(IDBS+4) ,IND(IDB),TAGEL,ITAB         ,
     7     ILEV      )

         ENDIF
C
         IF (NSPMD > 1 .AND. IDEL == 2) THEN
           N1 = NUMNOD+1
           CALL CHK2MSR3NP(
     1	   NSN  	,INTBUF_TAB(NG)%NSV,ITAG   ,ITASK ,INTBUF_TAB(NG)%IRECTM,
     2	   INTBUF_TAB(NG)%IRTLM,ITAG(N1)     ,IXS    ,IXC   ,IXTG	   ,
     3	   IXQ  	,IPARG        ,ITAGL ,MS	   ,
     4	   IN,INTBUF_TAB(NG)%SMAS,INTBUF_TAB(NG)%SINER     ,ADM   ,ITAB	   ,
     5	   ITABM1	,CNEL	      ,ADDCNEL,OFC   ,OFT	   ,
     6	   OFTG 	,OFUR,NINDEX1(NG),IBUFS(IDBS+4),IND(IDB)   ,
     7     IDEL)
         ELSEIF (IDEL == 2) THEN
           NINDEX1(NG)=0
         ENDIF
         NINDEX2(NG)=0
         IF(NINDEX1(NG)+NINDEX2(NG) > 0)THEN
C Partie non parallele
!$OMP SINGLE
           IBUFS(IDBS)=IDEL
           IBUFS(IDBS+1)=NTY
           IBUFS(IDBS+2)=NINDEX1(NG)
           IBUFS(IDBS+3)=NINDEX2(NG)
C Fin Partie non parallele
!$OMP END SINGLE
           IDBS = IDBS + INC*(NINDEX1(NG)+NINDEX2(NG)) + 4
           IDB  = IDB  + NINDEX1(NG) + NINDEX2(NG)
         END IF
C------
       ENDIF
      ENDDO
C
      IF(NSPMD > 1) THEN

C
C Traitement supplementaires en SPMD avec un seul point de communication
C

C Partie non parallele

!$OMP SINGLE

       CALL SPMD_INIT_IDEL(IDBS-1, IRSIZE, IRECV,IAD_ELEM)
       CALL SPMD_EXCHMSR_IDEL(
     1         IBUFS  ,IDBS-1 ,IXS    ,IXC    ,IXTG   , 
     2         IXQ    ,IPARG  ,ITAGL  ,ITABM1 ,
     3         IRSIZE ,IRECV  ,CNEL   ,ADDCNEL,OFC    ,
     4         OFT    ,OFTG   ,OFUR   ,OFR    ,OFP    ,
     5         IDB-1  ,IXP    ,IXR    ,IXT    ,GEO    ,
     6         TAGEL  ,IAD_ELEM)

C
C Finalisation de la partie MAJ STIF
C
       IF(INT24USE==1.OR.NINTER25/=0)THEN
         ALLOCATE(INDSEGLO_SAV(NINTER+1))
         SIZ=INDSEGLO(NINTER+1)-INDSEGLO(1)
         ALLOCATE(IBUFSEGLO_SAV(SIZ))

         INDSEGLO_SAV(1:NINTER+1)=INDSEGLO(1:NINTER+1)
         IBUFSEGLO_SAV(1:SIZ)=IBUFSEGLO(1:SIZ)

         INDSEGLO(1:NINTER+1)=0
         INDSEGLO(1)=1
         IBUFSEGLO(1:SIZ)=0
       ENDIF

       IDB=1
       DO NG=1,NINTER
        NTY   =IPARI(7,NG)
        NRTM  =IPARI(4,NG)
        IDEL=IPARI(17,NG)
        IF(INT24USE==1.OR.NINTER25/=0)THEN
            INDSEGLO(NG+1)=INDSEGLO(NG)
        ENDIF
        IF((NTY==7.OR.NTY==10.OR.NTY==5.OR.NTY==20.OR.NTY==22
     +    .OR.NTY==23.OR.NTY==24.OR.NTY==25).AND.IDEL>=1) THEN

         IF(INT24USE==1.OR.NINTER25/=0)THEN
            SIZ=INDSEGLO_SAV(NG+1)-INDSEGLO_SAV(NG)
            DO I=1,SIZ
              IBUFSEGLO(INDSEGLO(NG+1))=IBUFSEGLO_SAV(INDSEGLO_SAV(NG)+I-1)
              INDSEGLO(NG+1)=INDSEGLO(NG+1)+1
            ENDDO
         ENDIF
         CALL SETMSR3(
     1          INTBUF_TAB(NG)%STFM,NINDEX1(NG),IBUFS(IDB),IND(IDB),NTY,
     2          IDEL         ,0, NEWFRONT(NG),NG,NRTM,
     7          INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO,IBUFSEGLO)
         IDB = IDB+NINDEX1(NG)
        ELSEIF((NTY == 11).AND.IDEL>=1) THEN
Cote main
         CALL SETMSR3(
     1          INTBUF_TAB(NG)%STFM,NINDEX1(NG),IBUFS(IDB),IND(IDB),NTY,
     2          IDEL         ,1, NEWFRONT(NG),NG,NRTM,
     7          INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO,IBUFSEGLO)
         IDB = IDB+NINDEX1(NG)
Cote secnd
         CALL SETMSR3(
     1          INTBUF_TAB(NG)%STFS,NINDEX2(NG),IBUFS(IDB),IND(IDB),NTY,
     2          IDEL         ,2, NEWFRONT(NG),NG,NRTM,
     7          INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO,IBUFSEGLO)
         IDB = IDB+NINDEX2(NG)
        ELSEIF(NTY == 3.AND.IDEL>=1) THEN
C cote secnd
         CALL SETMSR3(
     1          INTBUF_TAB(NG)%STFS,NINDEX1(NG),IBUFS(IDB),IND(IDB),NTY,
     2          IDEL         ,0, NEWFRONT(NG),NG,NRTM,
     7          INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO,IBUFSEGLO)
         IDB = IDB+NINDEX1(NG)
C cote main
         CALL SETMSR3(
     1          INTBUF_TAB(NG)%STFM,NINDEX2(NG),IBUFS(IDB),IND(IDB),NTY,
     2          IDEL         ,0, NEWFRONT(NG),NG,NRTM,
     7          INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO,IBUFSEGLO)
         IDB = IDB+NINDEX2(NG)
        ELSEIF(NTY == 2.AND.IDEL/=0)THEN
         CALL SETMSR2(
     1          NINDEX1(NG)  ,IBUFS(IDB),IND(IDB)     ,INTBUF_TAB(NG)%NSV,MS,
     2         INTBUF_TAB(NG)%SMAS,IN   ,INTBUF_TAB(NG)%SINER,IDEL)
         IDB = IDB+NINDEX1(NG)
C
        ENDIF
C
C Rajout type20 partie edge
C
        IF(NTY == 20.AND.IDEL>=1)THEN
Cote main
         CALL SETMSR3(
     1          INTBUF_TAB(NG)%STF,NINDEX3(NG),IBUFS(IDB),IND(IDB),-NTY,    ! -NTY => type20 edge
     2          IDEL         ,1, NEWFRONT(NG),NG,NRTM,
     7          INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO,IBUFSEGLO)
         IDB = IDB+NINDEX3(NG)
Cote secnd
         CALL SETMSR3(
     1          INTBUF_TAB(NG)%STFS,NINDEX4(NG),IBUFS(IDB),IND(IDB),-NTY,    ! -NTY => type20 edge
     2          IDEL         ,2, NEWFRONT(NG),NG,NRTM,
     7          INTBUF_TAB(NG)%MSEGLO,INTBUF_TAB(NG)%MVOISIN,INDSEGLO ,IBUFSEGLO)
         IDB = IDB+NINDEX4(NG)
        END IF    ! fin type20 edge
       ENDDO


      IF(INT24USE > 0.OR.NINTER25/=0)THEN
        DEALLOCATE(INDSEGLO_SAV)
        DEALLOCATE(IBUFSEGLO_SAV)
      ENDIF

C Fin Partie non parallele
!$OMP END SINGLE

      END IF  ! specifique NSPMD > 1    
C
C barrier et remise a 0 de idel7nok effectuees dans resol
C
      RETURN
      END
 
Chd|====================================================================
Chd|  CHKMSR3N                      source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        I24_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|        I25_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHKMSR3N(
     1    NMN  ,MSR   ,ITAG  ,ITASK,IRECT   ,
     2    NRTM ,STF   ,ITAG2 ,IXS  ,IXC     ,
     3    IXTG ,IXQ   ,IPARG ,ITAGL   ,
     4    NTY  ,ITAB  ,ITABM1,CNEL ,ADDCNEL ,
     5    OFC  ,OFT   ,OFTG  ,OFUR ,NINDG   ,
     6    BUFS ,NINDEX,TAGEL ,NG,
     7    MSEGLO,MVOISIN,
     8    INDSEGLO ,IBUFSEGLO)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, MSR(*), ITAG(*), ITASK, IRECT(4,*), NRTM,
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*),ITAB(*),ITABM1(*),
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, NINDG,
     .        NINDEX(*), BUFS(*), TAGEL(*) ,NG,MSEGLO(*),MVOISIN(*),
     .        INDSEGLO(*) ,IBUFSEGLO(*)
C     REAL
      my_real
     .        STF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, N, NIND2, NINDL(NRTM*2),MA_SURF,IND_SEGLO(NRTM*2),NIND_SEGLO
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
C
      IF(NTY/=3.AND.NTY/=5) THEN
C mise a - uniquement pour optimiser les interfaces type 7, 10
        DO I = NMNF, NMNL
C si tag nul sur noeuds main alors msr(i) = -msr(i)
          IF (ITAG(ABS(MSR(I))) == 0) THEN
            MSR(I) = -ABS(MSR(I))
          ENDIF
        ENDDO
      END IF

!$OMP SINGLE
      ICOMP = 0
!$OMP END SINGLE
C
      CALL MY_BARRIER()
      IF(NTY==7.OR.NTY==10.OR.NTY==22.OR.NTY==24.OR.NTY==25) RETURN
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      NIND_SEGLO = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1 = IRECT(1,I)
          N2 = IRECT(2,I)
          N3 = IRECT(3,I)
          N4 = IRECT(4,I)
          IF(N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            STF(I) = ZERO
            IF(NTY==24.OR.NTY==25)THEN
              NIND_SEGLO = NIND_SEGLO + 1
              IND_SEGLO(NIND_SEGLO)=I
            ENDIF
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      DO N = 1, NIND
       I = NINDL(N)
       N1 = IRECT(1,I)
       N2 = IRECT(2,I)
       N3 = IRECT(3,I)
       N4 = IRECT(4,I)
       IF(N4 == 0) N4 = N3
       DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
         II = CNEL(J)
         IF(TAGEL(II)<0) THEN    !    elt detruit trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFC) THEN ! solide detruit
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell detruit
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           END IF
           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
             STF(I) = ZERO
             MA_SURF=I


             IF(NTY==24.OR.NTY==25)THEN
               NIND_SEGLO = NIND_SEGLO + 1
               IND_SEGLO(NIND_SEGLO)=I
              ENDIF

             GOTO 400
           END IF
         END IF
       END DO
C
C   on a rien trouve, il faut voir sur les autres procs en SPMD (cas elt double ou facette avec nds frontiere sur 2 cpus)
       IF(NSPMD > 1) THEN
#include "lockon.inc"
         ICOMP = ICOMP + 1
         NIND2 = ICOMP
#include "lockoff.inc"
     	 NINDEX(NIND2) = I
     	 BUFS(4*(NIND2-1)+1) = ITAB(N1)
     	 BUFS(4*(NIND2-1)+2) = ITAB(N2)
     	 BUFS(4*(NIND2-1)+3) = ITAB(N3)
     	 BUFS(4*(NIND2-1)+4) = ITAB(N4)
       END IF
 400   CONTINUE
      END DO
      IF(NTY==24)THEN
         CALL I24_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NG,NRTM,MSEGLO,MVOISIN,1)
         IF(NSPMD > 1)THEN
#include "lockon.inc"
             DO I=1,NIND_SEGLO
                IBUFSEGLO(INDSEGLO(NG+1))=MSEGLO(IND_SEGLO(I))
                INDSEGLO(NG+1)=INDSEGLO(NG+1)+1
             ENDDO
#include "lockoff.inc"
         ENDIF
      ELSEIF(NTY==25)THEN
         CALL I25_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NG,NRTM,MSEGLO,MVOISIN,1)
         IF(NSPMD > 1)THEN
#include "lockon.inc"
             DO I=1,NIND_SEGLO
                IBUFSEGLO(INDSEGLO(NG+1))=MSEGLO(IND_SEGLO(I))
                INDSEGLO(NG+1)=INDSEGLO(NG+1)+1
             ENDDO
#include "lockoff.inc"
         ENDIF
      ENDIF
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHKMSR3NB                     source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        I24_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|        I25_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHKMSR3NB(
     1    NMN  ,MSR   ,ITAG  ,ITASK,IRECT   ,
     2    NRTM ,STF   ,ITAG2 ,IXS  ,IXC     ,
     3    IXTG ,IXQ   ,IPARG ,ITAGL   ,
     4    NTY  ,ITAB  ,ITABM1,CNEL ,ADDCNEL ,
     5    OFC  ,OFT   ,OFTG  ,OFUR ,NINDG   ,
     6    BUFS ,NINDEX,TAGEL ,NG,MSEGLO,
     7    MVOISIN,INDSEGLO,IBUFSEGLO)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, NRTM, MSR(*), ITAG(*), ITASK, IRECT(4,*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITAB(*),
     .        ITABM1(*), CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR,
     .        NINDG, NINDEX(*), BUFS(*), TAGEL(*),
     .        NG,MSEGLO(*),MVOISIN(*),IBUFSEGLO(*),INDSEGLO(*)
C     REAL
      my_real
     .        STF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, NIND2, N, NINDL(NRTM),IND_SEGLO(NRTM*2),NIND_SEGLO
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
      IF(NTY/=3.AND.NTY/=5) THEN
C mise a - uniquement pour optimiser les interfaces type 7, 10
        DO I = NMNF, NMNL
C si tag nul sur noeuds main alors msr(i) = -msr(i)
          IF (ITAG(ABS(MSR(I))) == 0) THEN
            MSR(I) = -ABS(MSR(I))
          END IF
        ENDDO
      END IF
C
      CALL MY_BARRIER()     
      IF(NTY==7.OR.NTY==10.OR.NTY==22.OR.NTY==24.OR.NTY==25) RETURN
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      NIND_SEGLO = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1 = IRECT(1,I)
          N2 = IRECT(2,I)
          N3 = IRECT(3,I)
          N4 = IRECT(4,I)
          IF(N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            STF(I) = ZERO
            IF(NTY==24.OR.NTY==25)THEN
              NIND_SEGLO = NIND_SEGLO + 1
              IND_SEGLO(NIND_SEGLO)=I
            ENDIF
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      DO N = 1, NIND
        I = NINDL(N)
        N1 = IRECT(1,I)
        N2 = IRECT(2,I)
        N3 = IRECT(3,I)
        N4 = IRECT(4,I)
        IF(N4 == 0) N4 = N3
        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
          II = CNEL(J)
          IF(TAGEL(II) > 0) THEN    !    elt actif trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFC) THEN ! solide actif
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell actif
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN ! triangle actif
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           END IF
           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
             GOTO 400
           END IF
          END IF
        END DO
C si aucun element actif : stif a 0 en smp ou mono
        IF(NSPMD == 1) THEN
          STF(I) = ZERO
          IF(NTY==24.OR.NTY==25)THEN
             NIND_SEGLO = NIND_SEGLO + 1
             IND_SEGLO(NIND_SEGLO)=I
          ENDIF
C si aucun element actif :comm en spmd
        ELSE
#include "lockon.inc"
          ICOMP = ICOMP + 1
          NIND2 = ICOMP
#include "lockoff.inc"
          NINDEX(NIND2) = I
          BUFS(4*(NIND2-1)+1) = ITAB(N1)
          BUFS(4*(NIND2-1)+2) = ITAB(N2)
          BUFS(4*(NIND2-1)+3) = ITAB(N3)
          BUFS(4*(NIND2-1)+4) = ITAB(N4)
        END IF
 400    CONTINUE
      END DO
C
      IF(NTY==24)THEN
         CALL I24_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NG,NRTM,MSEGLO,MVOISIN,1)
         IF(NSPMD > 1)THEN
#include "lockon.inc"
             DO I=1,NIND_SEGLO
                IBUFSEGLO(INDSEGLO(NG+1))=MSEGLO(IND_SEGLO(I))
                INDSEGLO(NG+1)=INDSEGLO(NG+1)+1
             ENDDO
#include "lockoff.inc"
         ENDIF
      ELSEIF(NTY==25)THEN
         CALL I25_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NG,NRTM,MSEGLO,MVOISIN,1)
         IF(NSPMD > 1)THEN
#include "lockon.inc"
             DO I=1,NIND_SEGLO
                IBUFSEGLO(INDSEGLO(NG+1))=MSEGLO(IND_SEGLO(I))
                INDSEGLO(NG+1)=INDSEGLO(NG+1)+1
             ENDDO
#include "lockoff.inc"
         ENDIF
      ENDIF
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK20MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK20MSR3N(
     1    NMN  ,MSR   ,ITAG  ,ITASK,IRECT   ,
     2    NRTM ,STF   ,ITAG2 ,IXS  ,IXC     ,
     3    IXTG ,IXQ   ,IPARG ,ITAGL   ,
     4    NTY  ,ITAB  ,ITABM1,CNEL ,ADDCNEL ,
     5    OFC  ,OFT   ,OFTG  ,OFUR ,NINDG   ,
     6    BUFS ,NINDEX,NLG   ,TAGEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, MSR(*), ITAG(*), ITASK, IRECT(4,*), NRTM,
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*),ITAB(*),ITABM1(*),
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, NINDG,
     .        NINDEX(*), BUFS(*), NLG(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, N, NIND2, N1L, N2L, N3L, N4L,
     .        NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
      IF(NTY/=3.AND.NTY/=5) THEN
C mise a - uniquement pour optimiser les interfaces type 7, 10, 20
        DO I = NMNF, NMNL
C si tag nul sur noeuds main alors msr(i) = -msr(i)
        IF (ITAG(ABS(NLG(ABS(MSR(I))))) == 0) THEN
            MSR(I) = -ABS(MSR(I))
          ENDIF
        ENDDO
      END IF
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1L = IRECT(1,I)
          N2L = IRECT(2,I)
          N3L = IRECT(3,I)
          N4L = IRECT(4,I)
          N1  = NLG(N1L)
          N2  = NLG(N2L)
          N3  = NLG(N3L)
          N4  = NLG(N4L)
          IF(N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            STF(I) = ZERO
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      NIND2 = 0
      DO N = 1, NIND
       I = NINDL(N)
       N1L = IRECT(1,I)
       N2L = IRECT(2,I)
       N3L = IRECT(3,I)
       N4L = IRECT(4,I)
       N1  = NLG(N1L)
       N2  = NLG(N2L)
       N3  = NLG(N3L)
       N4  = NLG(N4L)
       IF(N4 == 0) N4 = N3
       DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
         II = CNEL(J)
         IF(TAGEL(II)<0) THEN    !    elt detruit trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFC) THEN ! solide detruit
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell detruit
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           END IF
           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
             STF(I) = ZERO
             GOTO 400
           END IF
         END IF
       END DO
C
C   on a rien trouver, il faut voir sur les autres procs en SPMD (cas elt double ou facette avec nds frontiere sur 2 cpus)
       IF(NSPMD > 1) THEN
#include "lockon.inc"
         ICOMP = ICOMP + 1
         NIND2 = ICOMP
#include "lockoff.inc"
     	 NINDEX(NIND2) = I
     	 BUFS(4*(NIND2-1)+1) = ITAB(N1)
     	 BUFS(4*(NIND2-1)+2) = ITAB(N2)
     	 BUFS(4*(NIND2-1)+3) = ITAB(N3)
     	 BUFS(4*(NIND2-1)+4) = ITAB(N4)
       END IF
 400   CONTINUE
      END DO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK20MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK20MSR3NB(
     1    NMN  ,MSR   ,ITAG  ,ITASK,IRECT   ,
     2    NRTM ,STF   ,ITAG2 ,IXS  ,IXC     ,
     3    IXTG ,IXQ   ,IPARG ,ITAGL   ,
     4    NTY  ,ITAB  ,ITABM1,CNEL ,ADDCNEL ,
     5    OFC  ,OFT   ,OFTG  ,OFUR ,NINDG   ,
     6    BUFS ,NINDEX,NLG   ,TAGEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, NRTM, MSR(*), ITAG(*), ITASK, IRECT(4,*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITAB(*),
     .        ITABM1(*), CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR,
     .        NINDG, NINDEX(*), BUFS(*), NLG(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, NIND2, N, N1L, N2L, N3L, N4L,
     .        NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
      IF(NTY/=3.AND.NTY/=5) THEN
C mise a - uniquement pour optimiser les interfaces type 7, 10, 20
        DO I = NMNF, NMNL
C si tag nul sur noeuds main alors msr(i) = -msr(i)
        IF (ITAG(ABS(NLG(ABS(MSR(I))))) == 0) THEN
            MSR(I) = -ABS(MSR(I))
          END IF
        ENDDO
      END IF
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1L = IRECT(1,I)
          N2L = IRECT(2,I)
          N3L = IRECT(3,I)
          N4L = IRECT(4,I)
          N1  = NLG(N1L)
          N2  = NLG(N2L)
          N3  = NLG(N3L)
          N4  = NLG(N4L)
          IF(N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            STF(I) = ZERO
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      NIND2 = 0
      DO N = 1, NIND
        I = NINDL(N)
        N1L = IRECT(1,I)
        N2L = IRECT(2,I)
        N3L = IRECT(3,I)
        N4L = IRECT(4,I)
        N1  = NLG(N1L)
        N2  = NLG(N2L)
        N3  = NLG(N3L)
        N4  = NLG(N4L)
        IF(N4 == 0) N4 = N3
        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
          II = CNEL(J)
          IF(TAGEL(II) > 0) THEN    !    elt actif trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFC) THEN ! solide actif
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell actif
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN ! triangle actif
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           END IF
           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
             GOTO 400
           END IF
          END IF
        END DO
C si aucun element actif : stif a 0 en smp ou mono
        IF(NSPMD == 1) THEN
          STF(I) = ZERO
C si aucun element actif :comm en spmd
        ELSE
#include "lockon.inc"
          ICOMP = ICOMP + 1
          NIND2 = ICOMP
#include "lockoff.inc"
          NINDEX(NIND2) = I
          BUFS(4*(NIND2-1)+1) = ITAB(N1)
          BUFS(4*(NIND2-1)+2) = ITAB(N2)
          BUFS(4*(NIND2-1)+3) = ITAB(N3)
          BUFS(4*(NIND2-1)+4) = ITAB(N4)
        END IF
 400    CONTINUE
      END DO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END

Chd|====================================================================
Chd|  CHK11MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK11MSR3N(
     1    NMN    ,MSR     ,ITAG  ,ITASK  ,IRECT  ,
     2    NRTM   ,STF     ,ITAG2 ,IXS    ,IXC	 ,
     3    IXTG   ,IXQ     ,IPARG ,ITAGL  ,
     4    NTY    ,NEWFRONT,IXT   ,IXP    ,IXR    ,
     5    GEO    ,IFL     ,ITAB  ,ITABM1 ,CNEL   ,
     6    ADDCNEL,OFC     ,OFT   ,OFTG   ,OFUR   ,
     7    OFR    ,OFP     ,NINDG ,BUFS   ,NINDEX ,
     8    TAGEL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, MSR(*), ITAG(*), ITASK, IRECT(2,*), NRTM,
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITABM1(*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),ITAB(*),
     .        IFL,NEWFRONT,
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, OFR, OFP,
     .        NINDG, NINDEX(*), BUFS(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*), GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, N, NIND2, NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
      DO I = NMNF, NMNL           
C si tag nul sur noeuds main alors msr(i) = -msr(i)
        IF (ITAG(ABS(MSR(I))) == 0) THEN
          MSR(I) = -ABS(MSR(I))
        ENDIF
      ENDDO
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
C
      NINDG = ICOMP
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK11MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK11MSR3NB(
     1    NMN    ,MSR   ,ITAG  ,ITASK   ,IRECT ,
     2    NRTM   ,STF   ,ITAG2 ,IXS     ,IXC   ,
     3    IXTG   ,IXQ   ,IPARG ,ITAGL ,
     4    NTY    ,ITAB  ,ITABM1,NEWFRONT,IXT   ,
     5    IXP    ,IXR   ,GEO   ,IFL     ,CNEL  ,
     6    ADDCNEL,OFC   ,OFT   ,OFTG    ,OFUR  ,
     7    OFR    ,OFP   ,NINDG ,BUFS    ,NINDEX,
     8    TAGEL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, NRTM, IFL, NEWFRONT,
     .        MSR(*), ITAG(*), ITASK, IRECT(2,*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITAB(*),
     .        ITABM1(*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, OFR, OFP,
     .        NINDG, NINDEX(*), BUFS(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*), GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, NIND2, N, NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
C mise a - uniquement pour optimiser les interfaces type 7, 10
      DO I = NMNF, NMNL
C si tag nul sur noeuds main alors msr(i) = -msr(i)
        IF (ITAG(ABS(MSR(I))) == 0) THEN
          MSR(I) = -ABS(MSR(I))
        END IF
      ENDDO
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
C
      NIND2 = 0
C
      NINDG = 0
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK20EMSR3N                   source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK20EMSR3N(
     1    NMN    ,MSR     ,ITAG  ,ITASK  ,IRECT  ,
     2    NRTM   ,STF     ,ITAG2 ,IXS    ,IXC	 ,
     3    IXTG   ,IXQ     ,IPARG ,ITAGL  ,
     4    NTY    ,NEWFRONT,IXT   ,IXP    ,IXR    ,
     5    GEO    ,IFL     ,ITAB  ,ITABM1 ,CNEL   ,
     6    ADDCNEL,OFC     ,OFT   ,OFTG   ,OFUR   ,
     7    OFR    ,OFP     ,NINDG ,BUFS   ,NINDEX ,
     8    NLG    ,TAGEL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, MSR(*), ITAG(*), ITASK, IRECT(2,*), NRTM,
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITABM1(*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),ITAB(*),
     .        IFL,NEWFRONT,
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, OFR, OFP,
     .        NINDG, NINDEX(*), BUFS(*), NLG(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*), GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N1L, N2L, 
     .        NN, II, IX, K, NIND, N, NIND2, NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
      DO I = NMNF, NMNL           
C si tag nul sur noeuds main alors msr(i) = -msr(i)
        IF (ITAG(ABS(NLG(ABS(MSR(I))))) == 0) THEN
          MSR(I) = -ABS(MSR(I))
        ENDIF
      ENDDO
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1L = IRECT(1,I)
          N2L = IRECT(2,I)
          N1  = NLG(N1L)
          N2  = NLG(N2L)
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0) THEN
C suivant facette main ou second
            IF(IFL == 1) THEN
              STF(I) = ZERO
            ELSE
              STF(I) =-ABS(STF(I))
              NEWFRONT = -1
            END IF 
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      NIND2 = 0
      DO N = 1, NIND
        I = NINDL(N)
          N1L = IRECT(1,I)
          N2L = IRECT(2,I)
          N1  = NLG(N1L)
          N2  = NLG(N2L)
C
        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
         II = CNEL(J)
         IF(TAGEL(II)<0) THEN    !    elt detruit trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           IF(II<=OFC) THEN ! solide detruit
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell detruit
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN ! triangle detruit
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFT.AND.II<=OFP)THEN ! truss detruit
             II = II - OFT
             DO K=2,3
               IX = IXT(K,II)
               ITAGL(IX) = 1
             ENDDO
           ELSEIF(II > OFP.AND.II<=OFR)THEN ! poutre detruit
             II = II - OFP
             DO K=2,3
               IX = IXP(K,II)
               ITAGL(IX) = 1
             ENDDO
           ELSEIF(II > OFR.AND.II<=OFTG)THEN ! ressort detruit
             II = II - OFR
             DO K=2,3
               IX = IXR(K,II)
               ITAGL(IX) = 1
             ENDDO
             IF(NINT(GEO(12,IXR(1,II))) == 12) THEN ! ressort detruit
               IX = IXR(4,II)
               ITAGL(IX) = 1
             ENDIF
           END IF
           IF(ITAGL(N1)+ITAGL(N2) == 2)THEN
C suivant facette main ou second
            IF(IFL == 1) THEN
              STF(I) = ZERO
            ELSE
              STF(I) =-ABS(STF(I))
              NEWFRONT = -1
            END IF 
            GOTO 400
           END IF
         END IF
        END DO
C
C   on a rien trouver, il faut voir sur les autres procs en SPMD
        IF(NSPMD > 1) THEN
#include "lockon.inc"
          ICOMP = ICOMP + 1
          NIND2 = ICOMP
#include "lockoff.inc"
          NINDEX(NIND2) = I
          BUFS(2*(NIND2-1)+1) = ITAB(N1)
          BUFS(2*(NIND2-1)+2) = ITAB(N2)
        END IF
 400    CONTINUE
      END DO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
c      NINDG = NIND2      

C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK20EMSR3NB                  source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK20EMSR3NB(
     1    NMN    ,MSR   ,ITAG  ,ITASK   ,IRECT ,
     2    NRTM   ,STF   ,ITAG2 ,IXS     ,IXC   ,
     3    IXTG   ,IXQ   ,IPARG ,ITAGL ,
     4    NTY    ,ITAB  ,ITABM1,NEWFRONT,IXT   ,
     5    IXP    ,IXR   ,GEO   ,IFL     ,CNEL  ,
     6    ADDCNEL,OFC   ,OFT   ,OFTG    ,OFUR  ,
     7    OFR    ,OFP   ,NINDG ,BUFS    ,NINDEX,
     8    NLG    ,TAGEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, NRTM, IFL, NEWFRONT,
     .        MSR(*), ITAG(*), ITASK, IRECT(2,*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITAB(*),
     .        ITABM1(*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, OFR, OFP,
     .        NINDG, NINDEX(*), BUFS(*), NLG(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*), GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N1L, N2L, 
     .        NN, II, IX, K, NIND, NIND2, N, NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
C mise a - uniquement pour optimiser les interfaces type 7, 10
      DO I = NMNF, NMNL
C si tag nul sur noeuds main alors msr(i) = -msr(i)
        IF (ITAG(ABS(NLG(ABS(MSR(I))))) == 0) THEN
          MSR(I) = -ABS(MSR(I))
        END IF
      ENDDO
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1L = IRECT(1,I)
          N2L = IRECT(2,I)
          N1  = NLG(N1L)
          N2  = NLG(N2L)
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0) THEN
C suivant facette main ou second
            IF(IFL == 1) THEN
              STF(I) = ZERO
            ELSE
              STF(I) =-ABS(STF(I))
              NEWFRONT = -1
            END IF 
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      NIND2 = 0
      DO N = 1, NIND
        I = NINDL(N)
        N1L = IRECT(1,I)
        N2L = IRECT(2,I)
        N1  = NLG(N1L)
        N2  = NLG(N2L)
C
        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
         II = CNEL(J)
         IF(TAGEL(II) > 0) THEN    !    elt actif trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           IF(II<=OFC) THEN ! solide actif
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell actif
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN ! triangle actif
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFT.AND.II<=OFP)THEN ! truss actif
             II = II - OFT
             DO K=2,3
               IX = IXT(K,II)
               ITAGL(IX) = 1
             ENDDO
           ELSEIF(II > OFP.AND.II<=OFR)THEN ! poutre actif
             II = II - OFP
             DO K=2,3
               IX = IXP(K,II)
               ITAGL(IX) = 1
             ENDDO
           ELSEIF(II > OFR.AND.II<=OFTG)THEN ! ressort actif
             II = II - OFR
             DO K=2,3
               IX = IXR(K,II)
               ITAGL(IX) = 1
             ENDDO
             IF(NINT(GEO(12,IXR(1,II))) == 12) THEN ! ressort actif
               IX = IXR(4,II)
               ITAGL(IX) = 1
             ENDIF
           END IF
           IF(ITAGL(N1)+ITAGL(N2) == 2)THEN
             GOTO 400
           ENDIF
         ENDIF
        ENDDO
C
C si aucun element actif : stif a 0
        IF(NSPMD == 1) THEN
C suivant facette main ou second
          IF(IFL == 1) THEN
            STF(I) = ZERO
          ELSE
            STF(I) =-ABS(STF(I))
            NEWFRONT = -1
          END IF 
        ELSE
#include "lockon.inc"
          ICOMP = ICOMP + 1
          NIND2 = ICOMP
#include "lockoff.inc"
          NINDEX(NIND2) = I
          BUFS(2*(NIND2-1)+1) = ITAB(N1)
          BUFS(2*(NIND2-1)+2) = ITAB(N2)
        END IF
C
 400    CONTINUE
      END DO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK2MSR3N                     source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK2MSR3N(
     1    NSN    ,NSV  ,ITAG ,ITASK,IRECT,
     2    IRTL   ,ITAG2,IXS  ,IXC  ,IXTG ,
     3    IXQ    ,IPARG,ITAGL,MS   ,
     4    IN     ,SMAS ,SINER,ADM  ,CNEL ,
     5    ADDCNEL,OFC  ,OFT  ,OFTG ,OFUR ,
     6    TAGEL  ,ILEV )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*), ITAG(*), ITASK, IRECT(4,*), IRTL(*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), TAGEL(*),
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR,ILEV
C     REAL
      my_real
     .        MS(*),IN(*),SMAS(*),SINER(*),ADM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NSNF, NSNL, IS, L, N1, N2, N3, N4, 
     .        II, IX, K, NIND, N, NINDEX(NSN)
C     REAL
C-----------------------------------------------
C
      CALL MY_BARRIER()
C
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      NIND = 0
      DO I = NSNF, NSNL
        IS=NSV(I)
        IF (IS > 0) THEN
          L  =IRTL(I)
          N1 = IRECT(1,L)
          N2 = IRECT(2,L)
          N3 = IRECT(3,L)
          N4 = IRECT(4,L)
          IF (N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            NSV(I) = -NSV(I)
            IF (ILEV /= 25 .and. ILEV /= 26) THEN
              MS(IS) = SMAS(I)
              IN(IS) = SINER(I)
            ENDIF
C         attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDEX(NIND) = I
          END IF
        END IF
      END DO
C
      DO N = 1, NIND
        I = NINDEX(N)
        IS= NSV(I)
        L  =IRTL(I)
        N1 = IRECT(1,L)
        N2 = IRECT(2,L)
        N3 = IRECT(3,L)
        N4 = IRECT(4,L)
        IF(N4 == 0) N4 = N3
C
        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
          II = CNEL(J)
          IF(TAGEL(II)<0) THEN    !    elt detruit trouve
            ITAGL(N1) = 0
            ITAGL(N2) = 0
            ITAGL(N3) = 0
            ITAGL(N4) = 0
            IF(II<=OFC) THEN ! solide detruit
              DO K = 2, 9
                IX = IXS(K,II)
                ITAGL(IX) = 1
              END DO
            ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell detruit
              II = II - OFC
              DO K=2,5
                IX = IXC(K,II)
                ITAGL(IX)=1
              END DO
            ELSEIF(II > OFTG.AND.II<=OFUR)THEN
              II = II - OFTG
              DO K=2,4
                IX = IXTG(K,II)
                ITAGL(IX) = 1
              END DO
            END IF
            IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
              NSV(I) = -NSV(I)
              IF (ILEV /= 25 .and. ILEV /= 26) THEN
                MS(IS)  = SMAS(I)
                IN(IS)  = SINER(I)
              ENDIF
              GOTO 400
            END IF
          END IF
        END DO
 400    CONTINUE
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK2MSR3NB                    source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK2MSR3NB(
     1    NSN    ,NSV  ,ITAG ,ITASK,IRECT,
     2    IRTL   ,ITAG2,IXS  ,IXC  ,IXTG ,
     3    IXQ    ,IPARG,ITAGL,MS   ,
     4    IN     ,SMAS ,SINER,ADM  ,CNEL ,
     5    ADDCNEL,OFC  ,OFT  ,OFTG ,OFUR ,
     6    NINDG  ,BUFS ,NINDEX,TAGEL,ITAB  ,
     7    ILEV   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*), ITAG(*), ITASK, IRECT(4,*), IRTL(*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), TAGEL(*),
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, ILEV,
     .        NINDG, NINDEX(*), BUFS(*),ITAB(*)
C     REAL
      my_real
     .        MS(*),IN(*),SMAS(*),SINER(*),ADM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NSNF, NSNL, IS, L, N1, N2, N3, N4, 
     .        II, IX, K, NIND, N, NINDEX0(NSN),NIND2
C     REAL
C-----------------------------------------------
C
      ICOMP = 0
      CALL MY_BARRIER()
C
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      NIND = 0
      DO I = NSNF, NSNL
        IS=NSV(I)
        IF(IS > 0) THEN
          L  =IRTL(I)
          N1 = IRECT(1,L)
          N2 = IRECT(2,L)
          N3 = IRECT(3,L)
          N4 = IRECT(4,L)
          IF(N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            NSV(I) = -NSV(I)
            IF (ILEV /= 25 .and. ILEV /= 26) THEN
              MS(IS) = SMAS(I)
              IN(IS) = SINER(I)
            ENDIF
C         attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +       ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDEX0(NIND) = I
          END IF
        END IF
      END DO
C
      DO N = 1, NIND
        I = NINDEX0(N)
        IS = NSV(I)
        L = IRTL(I)
        N1 = IRECT(1,L)
        N2 = IRECT(2,L)
        N3 = IRECT(3,L)
        N4 = IRECT(4,L)
        IF(N4 == 0) N4 = N3
        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
          II = CNEL(J)
          IF(TAGEL(II) > 0) THEN    !    elt actif trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFC) THEN ! solide actif
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell actif
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN ! triangle actif
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           END IF
           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
             GOTO 400
           END IF
          END IF
        END DO
C si aucun element actif : stif a 0 en smp ou mono
        IF(NSPMD == 1) THEN
           NSV(I) = -NSV(I)
           IF (ILEV /= 25 .and. ILEV /= 26) THEN
             MS(IS) = SMAS(I)
             IN(IS) = SINER(I)
           ENDIF
C si aucun element actif :comm en spmd
        ELSE
#include "lockon.inc"
          ICOMP = ICOMP + 1
          NIND2 = ICOMP
#include "lockoff.inc"
          NINDEX(NIND2) = I
          BUFS(4*(NIND2-1)+1) = ITAB(N1)
          BUFS(4*(NIND2-1)+2) = ITAB(N2)
          BUFS(4*(NIND2-1)+3) = ITAB(N3)
          BUFS(4*(NIND2-1)+4) = ITAB(N4)
        END IF
 400    CONTINUE
      END DO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK2MSR3NP                    source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK2MSR3NP(
     1    NSN   ,NSV  ,ITAG   ,ITASK,IRECT ,
     2    IRTL  ,ITAG2,IXS    ,IXC  ,IXTG  ,
     3    IXQ   ,IPARG,ITAGL,MS    ,
     4    IN    ,SMAS ,SINER  ,ADM  ,ITAB  ,
     5    ITABM1,CNEL ,ADDCNEL,OFC  ,OFT   ,
     6    OFTG  ,OFUR ,NINDG  ,BUFS ,INDEX ,
     7    IDEL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NSV(*), ITAG(*), ITASK, IRECT(4,*), IRTL(*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITAB(*),
     .        ITABM1(*), CNEL(0:*), ADDCNEL(0:*),
     .        OFC, OFT, OFTG, OFUR, NINDG,
     .        INDEX(*), BUFS(*),IDEL
C     REAL
      my_real
     .        MS(*),IN(*),SMAS(*),SINER(*),ADM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NSNF, NSNL, IS, L, N1, N2, N3, N4, 
     .        NN, II, NINDEX, J
C     REAL
C-----------------------------------------------
      ICOMP = 0
      CALL MY_BARRIER()
C
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      DO I = NSNF, NSNL
        IS=NSV(I)
        IF(IS > 0) THEN
C
C       la facette detruite est eventuellement sur un autre processeur
C
          L  =IRTL(I)
          N1 = IRECT(1,L)
          N2 = IRECT(2,L)
          N3 = IRECT(3,L)
          N4 = IRECT(4,L)
          IF(N4 == 0) N4 = N3
C         attention >= 1 car cumul noeud frontiere des tags a 1
          IF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1 .AND. IDEL== 2) THEN
#include "lockon.inc"
            ICOMP = ICOMP + 1
            NINDEX = ICOMP
#include "lockoff.inc"
            INDEX(NINDEX) = I
            BUFS(4*(NINDEX-1)+1) = ITAB(N1)
            BUFS(4*(NINDEX-1)+2) = ITAB(N2)
            BUFS(4*(NINDEX-1)+3) = ITAB(N3)
            BUFS(4*(NINDEX-1)+4) = ITAB(N4)
C         attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1 .AND. IDEL== 1) THEN
#include "lockon.inc"
            ICOMP = ICOMP + 1
            NINDEX = ICOMP
#include "lockoff.inc"
            INDEX(NINDEX) = I
            BUFS(4*(NINDEX-1)+1) = ITAB(N1)
            BUFS(4*(NINDEX-1)+2) = ITAB(N2)
            BUFS(4*(NINDEX-1)+3) = ITAB(N3)
            BUFS(4*(NINDEX-1)+4) = ITAB(N4)
          ENDIF
        ENDIF
      ENDDO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK23MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK23MSR3N(
     1    NMN  ,MSR   ,ITAG  ,ITASK,IRECT   ,
     2    NRTM ,STF   ,ITAG2 ,IXS  ,IXC     ,
     3    IXTG ,IXQ   ,IPARG ,ITAGL   ,
     4    NTY  ,ITAB  ,ITABM1,CNEL ,ADDCNEL ,
     5    OFC  ,OFT   ,OFTG  ,OFUR ,NINDG   ,
     6    BUFS ,NINDEX,TAGEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, MSR(*), ITAG(*), ITASK, IRECT(4,*), NRTM,
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*),ITAB(*),ITABM1(*),
     .        CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR, NINDG,
     .        NINDEX(*), BUFS(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, N, NIND2, NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
c arebrancher       IF(NTY/=3.AND.NTY/=5) THEN
c arebrancherC mise a - uniquement pour optimiser les interfaces type 7, 10
c arebrancher        DO I = NMNF, NMNL
c arebrancherC si tag nul sur noeuds main alors msr(i) = -msr(i)
c arebrancher          IF (ITAG(ABS(MSR(I))) == 0) THEN
c arebrancher            MSR(I) = -ABS(MSR(I))
c arebrancher          ENDIF
c arebrancher        ENDDO
c arebrancher      END IF
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1 = MSR(IRECT(1,I))
          N2 = MSR(IRECT(2,I))
          N3 = MSR(IRECT(3,I))
          N4 = MSR(IRECT(4,I))
          IF(N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            STF(I) = ZERO
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      DO N = 1, NIND
       I = NINDL(N)
       N1 = MSR(IRECT(1,I))
       N2 = MSR(IRECT(2,I))
       N3 = MSR(IRECT(3,I))
       N4 = MSR(IRECT(4,I))
       IF(N4 == 0) N4 = N3
       DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
         II = CNEL(J)
         IF(TAGEL(II)<0) THEN    !    elt detruit trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFC) THEN ! solide detruit
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell detruit
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           END IF
           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
             STF(I) = ZERO
             GOTO 400
           END IF
         END IF
       END DO
C
C   on a rien trouver, il faut voir sur les autres procs en SPMD (cas elt double ou facette avec nds frontiere sur 2 cpus)
       IF(NSPMD > 1) THEN
#include "lockon.inc"
         ICOMP = ICOMP + 1
         NIND2 = ICOMP
#include "lockoff.inc"
     	 NINDEX(NIND2) = I
     	 BUFS(4*(NIND2-1)+1) = ITAB(N1)
     	 BUFS(4*(NIND2-1)+2) = ITAB(N2)
     	 BUFS(4*(NIND2-1)+3) = ITAB(N3)
     	 BUFS(4*(NIND2-1)+4) = ITAB(N4)
       END IF
 400   CONTINUE
      END DO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHK23MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE CHK23MSR3NB(
     1    NMN  ,MSR   ,ITAG  ,ITASK,IRECT   ,
     2    NRTM ,STF   ,ITAG2 ,IXS  ,IXC     ,
     3    IXTG ,IXQ   ,IPARG ,ITAGL   ,
     4    NTY  ,ITAB  ,ITABM1,CNEL ,ADDCNEL ,
     5    OFC  ,OFT   ,OFTG  ,OFUR ,NINDG   ,
     6    BUFS ,NINDEX,TAGEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
      COMMON /IDELG/ICOMP
      INTEGER ICOMP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, NRTM, MSR(*), ITAG(*), ITASK, IRECT(4,*),
     .        ITAG2(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .        IXTG(NIXTG,*), IPARG(NPARG,*), ITAGL(*), ITAB(*),
     .        ITABM1(*), CNEL(0:*), ADDCNEL(0:*), OFC, OFT, OFTG, OFUR,
     .        NINDG, NINDEX(*), BUFS(*), TAGEL(*)
C     REAL
      my_real
     .        STF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND, NIND2, N, NINDL(NRTM)
C     REAL
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      ICOMP = 0
C
c arebrancher      IF(NTY/=3.AND.NTY/=5) THEN
c arebrancherC mise a - uniquement pour optimiser les interfaces type 7, 10
c arebrancher        DO I = NMNF, NMNL
c arebrancherC si tag nul sur noeuds main alors msr(i) = -msr(i)
c arebrancher          IF (ITAG(ABS(MSR(I))) == 0) THEN
c arebrancher            MSR(I) = -ABS(MSR(I))
c arebrancher          END IF
c arebrancher        ENDDO
c arebrancher      END IF
C
      CALL MY_BARRIER()
C
      NRTF = 1 + ITASK*NRTM / NTHREAD
      NRTL = (ITASK+1)*NRTM / NTHREAD
C
      NIND = 0
      DO I = NRTF, NRTL
        IF(STF(I)/=ZERO) THEN
          N1 = MSR(IRECT(1,I))
          N2 = MSR(IRECT(2,I))
          N3 = MSR(IRECT(3,I))
          N4 = MSR(IRECT(4,I))
          IF(N4 == 0) N4 = N3
          IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +       ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN
            STF(I) = ZERO
C attention >= 1 car cumul noeud frontiere des tags a 1
          ELSEIF(ITAG2(N1)>=1.AND.ITAG2(N2)>=1.AND.
     +           ITAG2(N3)>=1.AND.ITAG2(N4)>=1) THEN
            NIND = NIND + 1
            NINDL(NIND) = I
          END IF
        END IF
      END DO
C
      DO N = 1, NIND
        I = NINDL(N)
        N1 = MSR(IRECT(1,I))
        N2 = MSR(IRECT(2,I))
        N3 = MSR(IRECT(3,I))
        N4 = MSR(IRECT(4,I))
        IF(N4 == 0) N4 = N3
        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
          II = CNEL(J)
          IF(TAGEL(II) > 0) THEN    !    elt actif trouve
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFC) THEN ! solide actif
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell actif
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN ! triangle actif
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           END IF
           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN
             GOTO 400
           END IF
          END IF
        END DO
C si aucun element actif : stif a 0 en smp ou mono
        IF(NSPMD == 1) THEN
          STF(I) = ZERO
C si aucun element actif :comm en spmd
        ELSE
#include "lockon.inc"
          ICOMP = ICOMP + 1
          NIND2 = ICOMP
#include "lockoff.inc"
          NINDEX(NIND2) = I
          BUFS(4*(NIND2-1)+1) = ITAB(N1)
          BUFS(4*(NIND2-1)+2) = ITAB(N2)
          BUFS(4*(NIND2-1)+3) = ITAB(N3)
          BUFS(4*(NIND2-1)+4) = ITAB(N4)
        END IF
 400    CONTINUE
      END DO
C
      CALL MY_BARRIER()
C
      NINDG = ICOMP
C
      CALL MY_BARRIER()
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SETMSR3                       source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        I24_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|        I25_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|====================================================================
      SUBROUTINE SETMSR3(STF  , NINDG, BUFS    , NINDEX, NTY,
     1                   IDEL , IFL  , NEWFRONT,NG,NRTM,
     2                   MSEGLO,MVOISIN,INDSEGLO,IBUFSEGLO)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDG, NTY, IDEL, IFL, NEWFRONT, NINDEX(*), BUFS(*),NRTM,
     *        NG,MSEGLO(*),MVOISIN(*),IBUFSEGLO(*),INDSEGLO(*)
C     REAL
      my_real
     .        STF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NN,IND_SEGLO(NRTM*2),NIND_SEGLO
C-----------------------------------------------
      IF(NTY==7.OR.NTY==10.OR.NTY==22.OR.NTY==23.OR.
     .NTY==5.OR.NTY==20.OR.NTY==3.OR.NTY==24.OR.NTY==25)THEN
        IF(IDEL==2)THEN
          NIND_SEGLO = 0
#include "vectorize.inc"
          DO J = 1, NINDG
            NN = BUFS(J)
            IF(NN > 0) THEN
              I  = NINDEX(J)
C suivant facette main ou second
              STF(I) = ZERO
              IF(NTY==24.OR.NTY==25)THEN
                NIND_SEGLO = NIND_SEGLO + 1
                IND_SEGLO(NIND_SEGLO)=I
              ENDIF
            END IF
          END DO
        ELSEIF(IDEL == 1)THEN
          NIND_SEGLO = 0
#include "vectorize.inc"
          DO J = 1, NINDG
            NN = BUFS(J)
            IF(NN == 0) THEN
              I  = NINDEX(J)
              STF(I) = ZERO
              IF(NTY==24.OR.NTY==25)THEN
                NIND_SEGLO = NIND_SEGLO + 1
                IND_SEGLO(NIND_SEGLO)=I
              ENDIF
            END IF
          END DO
        END IF
      ELSEIF(NTY == 11.OR.NTY == -20) THEN
        IF(IDEL == 2)THEN
#include "vectorize.inc"
          DO J = 1, NINDG
            NN = BUFS(J)
            IF(NN > 0) THEN
              I  = NINDEX(J)
C suivant facette main ou second
              IF(IFL == 1) THEN
                STF(I) = ZERO
              ELSE
                STF(I) =-ABS(STF(I))
                NEWFRONT = -1
              END IF 
            END IF
          END DO
        ELSEIF(IDEL == 1)THEN
#include "vectorize.inc"
          DO J = 1, NINDG
            NN = BUFS(J)
            IF(NN == 0) THEN
              I  = NINDEX(J)
C suivant facette main ou second
              IF(IFL == 1) THEN
                STF(I) = ZERO
              ELSE
                STF(I) =-ABS(STF(I))
                NEWFRONT = -1
              END IF 
            END IF
          END DO
        END IF
      END IF
C
      IF(NTY==24)THEN
         CALL I24_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NG,NRTM,MSEGLO,MVOISIN,1)
         IF(NSPMD > 1)THEN
#include "lockon.inc"
             DO I=1,NIND_SEGLO
                IBUFSEGLO(INDSEGLO(NG+1))=MSEGLO(IND_SEGLO(I))
                INDSEGLO(NG+1)=INDSEGLO(NG+1)+1
             ENDDO
#include "lockoff.inc"
         ENDIF
      ELSEIF(NTY==25)THEN
         CALL I25_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NG,NRTM,MSEGLO,MVOISIN,1)
         IF(NSPMD > 1)THEN
#include "lockon.inc"
             DO I=1,NIND_SEGLO
                IBUFSEGLO(INDSEGLO(NG+1))=MSEGLO(IND_SEGLO(I))
                INDSEGLO(NG+1)=INDSEGLO(NG+1)+1
             ENDDO
#include "lockoff.inc"
         ENDIF
       ENDIF

C
      RETURN
      END
C
Chd|====================================================================
Chd|  SETMSR2                       source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SETMSR2(NINDG, BUFS, NINDEX, NSV, MS,
     2                   SMAS , IN  , SINER ,IDEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDG, NINDEX(*), BUFS(*), NSV(*), IDEL
      my_real
     .        MS(*), SMAS(*), IN(*), SINER(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IS, NN
C-----------------------------------------------
      IF(IDEL == 2)THEN
#include "vectorize.inc"
        DO J = 1, NINDG
          NN = BUFS(J)
          IF(NN == 1) THEN
            I  = NINDEX(J)
            IS = NSV(I)
            IF(IS > 0)THEN
              NSV(I)  = -NSV(I)
              MS(IS)  = SMAS(I)
              IN(IS)  = SINER(I)
            ENDIF
          ENDIF
        ENDDO
      ELSEIF(IDEL == 1)THEN
#include "vectorize.inc"
        DO J = 1, NINDG
          NN = BUFS(J)
          IF(NN == 0) THEN
            I  = NINDEX(J)
            IS = NSV(I)
            IF(IS > 0)THEN
              NSV(I)  = -NSV(I)
              MS(IS)  = SMAS(I)
              IN(IS)  = SINER(I)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END


Chd|====================================================================
Chd|  I24_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKMSR3N                      source/interfaces/interf/chkstfn3.F
Chd|        CHKMSR3NB                     source/interfaces/interf/chkstfn3.F
Chd|        SETMSR3                       source/interfaces/interf/chkstfn3.F
Chd|        SPMD_EXCH_IDEL_SEGLO          source/mpi/interfaces/spmd_exch_idel_seglo.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NIN, NRTM,MSEGLO,MVOISIN,FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         INTEGER MA_SURF,MSEGLO(*),MVOISIN(4,*),NRTM,IND_SEGLO(NRTM),FLAG,
     *   NIND_SEGLO,I
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
         INTEGER NIN,K,GLOB_ID
C-----------------------------------------------
         DO I=1,NIND_SEGLO
           MA_SURF=IND_SEGLO(I)
           GLOB_ID = MA_SURF
           IF (FLAG==1)GLOB_ID = MSEGLO(MA_SURF)
           DO K=1,NRTM
             IF (MVOISIN(1,K)==GLOB_ID) MVOISIN(1,K)=0
             IF (MVOISIN(2,K)==GLOB_ID) MVOISIN(2,K)=0
             IF (MVOISIN(3,K)==GLOB_ID) MVOISIN(3,K)=0
             IF (MVOISIN(4,K)==GLOB_ID) MVOISIN(4,K)=0
             IF(MSEGLO(K)==GLOB_ID)THEN
               MVOISIN(1,K)=0
               MVOISIN(2,K)=0
               MVOISIN(3,K)=0
               MVOISIN(4,K)=0
             ENDIF
           ENDDO
         ENDDO
      END
Chd|====================================================================
Chd|  I25_REMOVE_GLOBAL_SEGMENT     source/interfaces/interf/chkstfn3.F
Chd|-- called by -----------
Chd|        CHKMSR3N                      source/interfaces/interf/chkstfn3.F
Chd|        CHKMSR3NB                     source/interfaces/interf/chkstfn3.F
Chd|        SETMSR3                       source/interfaces/interf/chkstfn3.F
Chd|        SPMD_EXCH_IDEL_SEGLO          source/mpi/interfaces/spmd_exch_idel_seglo.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25_REMOVE_GLOBAL_SEGMENT(IND_SEGLO,NIND_SEGLO,NIN, NRTM,MSEGLO,MVOISIN,FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         INTEGER MA_SURF,MSEGLO(*),MVOISIN(4,*),NRTM,IND_SEGLO(NRTM),FLAG,
     *   NIND_SEGLO,I
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
         INTEGER NIN,K,GLOB_ID
C-----------------------------------------------
         DO I=1,NIND_SEGLO
           IF(FLAG==1)THEN
             MA_SURF = IND_SEGLO(I)
             GLOB_ID = MSEGLO(MA_SURF)
             DO K=1,NRTM
               IF (MVOISIN(1,K)==MA_SURF) MVOISIN(1,K)=0
               IF (MVOISIN(2,K)==MA_SURF) MVOISIN(2,K)=0
               IF (MVOISIN(3,K)==MA_SURF) MVOISIN(3,K)=0
               IF (MVOISIN(4,K)==MA_SURF) MVOISIN(4,K)=0
c              IF(MSEGLO(K)==GLOB_ID)THEN
c                MVOISIN(1,K)=0
c                MVOISIN(2,K)=0
c                MVOISIN(3,K)=0
c                MVOISIN(4,K)=0
c              ENDIF
             ENDDO
           ELSE
             MA_SURF = IND_SEGLO(I)
             GLOB_ID = MA_SURF
             DO K=1,NRTM
               IF (MVOISIN(1,K) < 0)THEN
                 IF(MVOISIN(1,K)==-GLOB_ID) MVOISIN(1,K)=0
               ENDIF
               IF (MVOISIN(2,K) < 0)THEN
                 IF(MVOISIN(2,K)==-GLOB_ID) MVOISIN(2,K)=0
               ENDIF
               IF (MVOISIN(3,K) < 0)THEN
                 IF(MVOISIN(3,K)==-GLOB_ID) MVOISIN(3,K)=0
               ENDIF
               IF (MVOISIN(4,K) < 0)THEN
                 IF(MVOISIN(4,K)==-GLOB_ID) MVOISIN(4,K)=0
               ENDIF
             ENDDO
           END IF
         ENDDO
      END
