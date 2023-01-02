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
Chd|  LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        INICRKFILL                    source/elements/xfem/inicrkfill.F
Chd|-- calls ---------------
Chd|        ACTIV_XFEM                    source/elements/xfem/lslocal.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C3COORI                       source/elements/sh3n/coque3n/c3coori.F
Chd|        CCOORI                        source/elements/shell/coque/ccoori.F
Chd|        EDGETIP3N                     source/elements/xfem/lslocal.F
Chd|        EDGETIP4N                     source/elements/xfem/lslocal.F
Chd|        ELCUT3N                       source/elements/xfem/lslocal.F
Chd|        ELCUT4N                       source/elements/xfem/lslocal.F
Chd|        PREINICRK3N                   source/elements/xfem/preinicrk3N.F
Chd|        PREINICRK4N                   source/elements/xfem/preinicrk4N.F
Chd|        XYZLOC3N                      source/elements/xfem/lslocal.F
Chd|        XYZLOC4N                      source/elements/xfem/lslocal.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE LSLOCAL(ELBUF_TAB,XFEM_TAB,
     .                   IPARG   ,IXC     ,IXTG    ,XREFC   ,XREFTG  ,
     .                   X       ,ICRK    ,INOD_CRK,NXSEG   ,NODLS   ,
     .                   RATIOLS ,NTAG    ,IELCRKC ,IELCRKTG,IEDGESH4,
     .                   IEDGESH3,NODEDGE ,TAGSKYC ,TAGSKYTG,KNOD2ELC,
     .                   TAGEDGE ,CRKLVSET,CRKSHELL,CRKEDGE ,XFEM_PHANTOM,
     .                   ITAB    ,ID      ,TITR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .  ICRK,INOD_CRK(*),NXSEG,NODLS(2,*),IELCRKC(*),IELCRKTG(*),
     .  NTAG(*),IEDGESH4(4,*),IEDGESH3(3,*),NODEDGE(2,*),
     .  TAGSKYC(4,*),TAGSKYTG(3,*),KNOD2ELC(*),TAGEDGE(*),ITAB(*),ID
      my_real
     . X(3,*),XREFC(4,3,*),XREFTG(3,3,*),RATIOLS(*)
C
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX)     :: CRKLVSET
      TYPE (XFEM_SHELL_)  , DIMENSION(NLEVMAX)     :: CRKSHELL
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX)    :: CRKEDGE       
      TYPE (XFEM_PHANTOM_), DIMENSION(NXLAYMAX)    :: XFEM_PHANTOM  
      CHARACTER TITR*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER XNOD(2,2),TAGXNOD(NXSEG+1),
     .  NGL(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ)
      INTEGER I,K,IED,NG,NEL,LS,FAC,IHBE,ISH3N,IXFEM,ITG,NELCUT,ILAY,NXLAY
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                             X1L,Y1L,X2L,Y2L,X3L,Y3L,X4L,Y4L
      my_real BETA0(2)
      my_real RATIO
C
      INTEGER,DIMENSION(:)  ,ALLOCATABLE :: ELCUT
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: EDGEC,EDGETG
      my_real,DIMENSION(:,:),ALLOCATABLE :: BETA
C=======================================================================
      ITG = 1+NUMELC
      ALLOCATE (ELCUT(NUMELC+NUMELTG))  
      ALLOCATE (BETA(2,NUMELC+NUMELTG)) 
      ALLOCATE (EDGEC(4,NUMELC))        
      ALLOCATE (EDGETG(3,NUMELTG))      
      ELCUT  = 0                         
      BETA   = 0                          
      EDGEC  = 0                         
      EDGETG = 0                        
C
      TAGXNOD = 0
c-----------------------
      DO LS=1,NXSEG
C
c           Xnod2
c    (4)- - -x- - -(3)
c     |      |      |
c     |      |      |
c     |   (NXSEG)   |
c     |      |      |
c     |      |      |
c     |      |      |
c    (1)- - -x- - -(2)
c           Xnod1
c
        NELCUT = 0
        ELCUT = 0
C  - first intersection node - (Xnod1)
        XNOD(1,1) = NODLS(1,LS)  ! node (1)
        XNOD(1,2) = NODLS(2,LS)  ! node (2)
C  - second intersection node - (Xnod2)
        XNOD(2,1) = NODLS(1,LS+1)  ! node (3)
        XNOD(2,2) = NODLS(2,LS+1)  ! node (4)
C
        BETA0(1) = RATIOLS(LS)
        BETA0(2) = RATIOLS(LS+1)
C
        DO I=1,2
          RATIO = BETA0(I)
          IF(RATIO == ZERO)THEN
            BETA0(I) = EM05
          ELSEIF(RATIO == ONE)THEN
            BETA0(I) = ONE-EM05
          ENDIF
        ENDDO
C-----------------------------------------------
C       TAG  CUT  ELEMENTS :
C-----------------------------------------------
        DO 200 NG=1,NGROUP
          IXFEM = IPARG(54,NG)
          IF (IXFEM == 0) CYCLE
c
          NXLAY  = ELBUF_TAB(NG)%NLAY
          NEL    = IPARG(2,NG)
          NFT    = IPARG(3,NG)
          ITY    = IPARG(5,NG)
          IHBE   = IPARG(23,NG)
          LFT = 1
          LLT = MIN(NVSIZ,NEL)
c
          IF (ITY == 7) IHBE = 0
c
c-------------------------
          IF (ITY==3) THEN
C           SHELL - 4N -
c
            DO I=LFT,LLT
              FAC = 0
C
C      - First check - (coincident nodes not accepted)
C
              IF(XNOD(1,1) == XNOD(1,2)) FAC = 1
C
              IF(FAC == 1)THEN
                CALL ANCMSG(MSGID=1618,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            I2=1,
     .            C1=TITR )
              ENDIF
C
              IF(XNOD(2,1) == XNOD(2,2)) FAC = 2
C
              IF(FAC == 2)THEN
                CALL ANCMSG(MSGID=1618,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            I2=2,
     .            C1=TITR )
              ENDIF
C---
              FAC = 0
c             find edge corresponding to Xnod1  =>  EDGEC(IED) = 1  
              IED = 1
              CALL ELCUT4N(I, IXC(1,NFT+1), XNOD, EDGEC(1,NFT+1),FAC,IED)
              IF (FAC == 1) TAGXNOD(LS) = 1
C
C add warning message if FAC /= 1  ! XNOD1 must fit an element edge's
C
c             find edge corresponding to Xnod2  =>  EDGEC(IED) = 2      
              IED = 2
              CALL ELCUT4N(I, IXC(1,NFT+1), XNOD, EDGEC(1,NFT+1),FAC,IED)
              IF (FAC == 2) TAGXNOD(LS+1) = 1
C
C add warning message if FAC /= 2
C
              IF(FAC == 2)THEN
                ELCUT(I+NFT) = 1
                NELCUT = 1
                NUMELCRK = NUMELCRK + 1
              ENDIF
            ENDDO
C---
            IF(NELCUT == 0)GOTO 200
C---
            CALL CCOORI(X,XREFC(1,1,NFT+1),IXC(1,NFT+1),
     .           X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,       
     .           Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,       
     .           IX1 ,IX2 ,IX3 ,IX4 ,NGL )            
C           local coordinates
            CALL XYZLOC4N(X1L,Y1L,X2L,Y2L,X3L,Y3L,X4L,Y4L,
     .                    X1 ,X2 ,X3 ,X4 ,Y1 ,Y2 ,
     .                    Y3 ,Y4 ,Z1 ,Z2 ,Z3 ,Z4 )
C 
c---
            CALL PREINICRK4N(ELBUF_TAB(NG),XFEM_TAB(NG,1:NXEL)  ,
     .           X1L     ,Y1L    ,X2L     ,Y2L     ,X3L     ,        
     .           Y3L     , X4L   ,Y4L     ,LFT     ,LLT     ,        
     .           NFT     ,NXLAY  ,IELCRKC ,EDGEC   ,BETA0   ,        
     .           IEDGESH4,ELCUT  ,XNOD    ,IXC     ,NODEDGE ,        
     .           TAGSKYC ,KNOD2ELC,TAGEDGE,CRKLVSET,CRKSHELL,
     .           CRKEDGE ,XFEM_PHANTOM)                 
c
c-------------------------
          ELSE IF (ITY==7) THEN
c           SHELL 3N
c-------------------------
            ISH3N = IPARG(23,NG)
c
            DO I=LFT,LLT
              FAC = 0
C
c             - First check - (coincident nodes not accepted)
C
              IF(XNOD(1,1) == XNOD(1,2)) FAC = 1
C
              IF(FAC == 1)THEN
                CALL ANCMSG(MSGID=1618,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            I2=1,
     .            C1=TITR )
              ENDIF
C
              IF(XNOD(2,1) == XNOD(2,2)) FAC = 2
C
              IF(FAC == 2)THEN
                CALL ANCMSG(MSGID=1618,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            I2=2,
     .            C1=TITR )
              ENDIF
C---
              FAC = 0
              CALL ELCUT3N(I,IXTG(1,NFT+1),XNOD,EDGETG(1,NFT+1),FAC,1)
              IF (FAC == 1) TAGXNOD(LS) = 1
C
c             add warning message if FAC /= 1  ! XNOD1 must fit an element edge's
C
              CALL ELCUT3N(I,IXTG(1,NFT+1),XNOD,EDGETG(1,NFT+1),FAC,2)
              IF (FAC == 2) TAGXNOD(LS+1) = 1
C
c             add warning message if FAC /= 2
C
              IF (FAC == 2) THEN
                ELCUT(I+NFT+NUMELC) = 1
                NELCUT = 1
                NUMELCRK = NUMELCRK + 1
              ENDIF
            ENDDO    !  I=LFT,LLT
c----------------------------------
            IF (NELCUT == 0) GOTO 200
C---
            CALL C3COORI(X,XREFTG(1,1,NFT+1),IXTG(1,NFT+1),NGL,
     .                   X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                   Z1  ,Z2  ,Z3  ,IX1 ,IX2 ,IX3 )
C           local coordinates
            CALL XYZLOC3N(X1L ,Y1L ,X2L ,Y2L ,X3L ,Y3L ,
     .                    X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                    Z1  ,Z2  ,Z3  )
c---
            CALL PREINICRK3N(ELBUF_TAB(NG),XFEM_TAB(NG,1:NXEL)   ,
     .              X1L     ,Y1L     ,X2L    ,Y2L     ,X3L       ,
     .              Y3L     ,LFT     ,LLT    ,NFT     ,NXLAY     ,
     .              IELCRKTG,EDGETG  ,BETA0  ,IEDGESH3,ELCUT(ITG),
     .              XNOD    ,IXTG    ,NODEDGE,TAGSKYTG,KNOD2ELC  ,
     .              TAGEDGE ,CRKLVSET,CRKSHELL,CRKEDGE,XFEM_PHANTOM)
          ENDIF
C---
          IF (NELCUT == 1) EXIT ! cracked element already tag
C---
 200  CONTINUE
c------------------------------------------------------------------------
C       to update warning message
        IF(NELCUT == 0)THEN
          IF(TAGXNOD(LS) == 0)THEN
            CALL ANCMSG(MSGID=1617,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            I2=ITAB(XNOD(1,1)),
     .            I3=ITAB(XNOD(1,2)),
     .            C1=TITR )

          ELSEIF(TAGXNOD(LS+1) == 0)THEN
            CALL ANCMSG(MSGID=1617,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            I2=ITAB(XNOD(2,1)),
     .            I3=ITAB(XNOD(2,2)),
     .            C1=TITR )

          ENDIF
        ENDIF
c-----
      ENDDO   ! LS=1,NXSEG
c------------------------------------------------------------------------
c     Tag tip edges and activate xfem groups
c------------------------------------------------------------------------
      DO NG=1,NGROUP                                                
        IXFEM = IPARG(54,NG)                                        
        IF (IXFEM == 0) CYCLE                                       
c
        NXLAY = ELBUF_TAB(NG)%NLAY                                  
        NEL   = IPARG(2,NG)                                         
        NFT   = IPARG(3,NG)                                         
        ITY   = IPARG(5,NG)                                         
        LFT   = 1                                                   
        LLT   = MIN(NVSIZ,NEL)                                      
C---
        IF (ITY == 3) THEN                                          
          CALL EDGETIP4N(LFT   ,LLT    ,NFT    ,IELCRKC ,IEDGESH4,  
     .                   NXLAY ,EDGEC  ,TAGEDGE,CRKLVSET,CRKEDGE)           
C
C         activation of new group if initial cracks                 
          CALL ACTIV_XFEM(IPARG ,NFT    ,LFT    ,LLT ,NXLAY,        
     .                    NG    ,IELCRKC,ITY    ,CRKEDGE)                   
        ELSEIF (ITY == 7) THEN                                      
          CALL EDGETIP3N(LFT   ,LLT    ,NFT    ,IELCRKTG,IEDGESH3,  
     .                   NXLAY ,EDGETG ,TAGEDGE,CRKLVSET,CRKEDGE)           
C
C         activation of new group if initial cracks                 
          CALL ACTIV_XFEM(IPARG ,NFT     ,LFT    ,LLT ,NXLAY,       
     .                    NG    ,IELCRKTG,ITY    ,CRKEDGE)                  
        ENDIF                                                       
      ENDDO                                                         
c     Tag ICUTEDGE=2 for tip edges
      DO ILAY=1,NXLAY
        DO I=1,NUMEDGES
          IF (CRKEDGE(ILAY)%EDGETIP(1,I) == 1 .or. 
     .        CRKEDGE(ILAY)%EDGETIP(2,I) == 1) THEN 
             DO K=1,NXEL                             
               CRKLVSET(NXEL*(ILAY-1)+K)%ICUTEDGE(I) = 2     
            ENDDO
          ENDIF
        ENDDO                                   
      ENDDO                                                         
c------------------------------------------------------------------------
      IF(ALLOCATED(ELCUT)) DEALLOCATE(ELCUT)
      IF(ALLOCATED(BETA))  DEALLOCATE(BETA)
      IF(ALLOCATED(EDGEC)) DEALLOCATE(EDGEC)
      IF(ALLOCATED(EDGETG))DEALLOCATE(EDGETG)
C-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  XYZLOC4N                      source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XYZLOC4N(X1L,Y1L,X2L,Y2L,X3L,Y3L,X4L,Y4L,
     .                    X1,X2,X3,X4,Y1,Y2,
     .                    Y3,Y4,Z1,Z2,Z3,Z4)
C-----------------------------------------------
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: X1L,Y1L,X2L,Y2L,X3L,Y3L,X4L,Y4L
      my_real, DIMENSION(MVSIZ), INTENT(IN)  :: X1,X2,X3,X4,Y1,Y2,
     .   Y3,Y4,Z1,Z2,Z3,Z4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real S1,S2,SUMA
      my_real, DIMENSION(MVSIZ) :: X21,Y21,Z21,X31,Y31,Z31,X41,Y41,Z41,
     .   X42,Y42,Z42,E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,SUM
C=======================================================================
      DO I=LFT,LLT
        X21(I) = X2(I)-X1(I)
        Y21(I) = Y2(I)-Y1(I)
        Z21(I) = Z2(I)-Z1(I)
        X31(I) = X3(I)-X1(I)
        Y31(I) = Y3(I)-Y1(I)
        Z31(I) = Z3(I)-Z1(I)
        X41(I) = X4(I)-X1(I)
        Y41(I) = Y4(I)-Y1(I)
        Z41(I) = Z4(I)-Z1(I)
        X42(I) = X4(I)-X2(I)
        Y42(I) = Y4(I)-Y2(I)
        Z42(I) = Z4(I)-Z2(I)
      ENDDO
c
C---    repere orthogonalise vers 5.0 
      DO I=LFT,LLT
        E1X(I) = X2(I)+X3(I)-X1(I)-X4(I)
        E1Y(I) = Y2(I)+Y3(I)-Y1(I)-Y4(I)
        E1Z(I) = Z2(I)+Z3(I)-Z1(I)-Z4(I)
C
        E2X(I) = X3(I)+X4(I)-X1(I)-X2(I)
        E2Y(I) = Y3(I)+Y4(I)-Y1(I)-Y2(I)
        E2Z(I) = Z3(I)+Z4(I)-Z1(I)-Z2(I)
C
        E3X(I) = E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
        E3Y(I) = E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
        E3Z(I) = E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
      ENDDO
C---
      DO I=LFT,LLT                         
        SUMA   = E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)  
        SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
        E3X(I) = E3X(I)*SUMA                              
        E3Y(I) = E3Y(I)*SUMA                              
        E3Z(I) = E3Z(I)*SUMA                              
C
        S1     = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I) 
        S2     = E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I) 
        SUMA   = SQRT(S1/S2)                
        E1X(I) = E1X(I) + (E2Y(I)*E3Z(I)-E2Z(I)*E3Y(I))*SUMA
        E1Y(I) = E1Y(I) + (E2Z(I)*E3X(I)-E2X(I)*E3Z(I))*SUMA
        E1Z(I) = E1Z(I) + (E2X(I)*E3Y(I)-E2Y(I)*E3X(I))*SUMA
C
        SUMA   = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)  
        SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
        E1X(I) = E1X(I)*SUMA                              
        E1Y(I) = E1Y(I)*SUMA                              
        E1Z(I) = E1Z(I)*SUMA                              
C
        E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
        E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
        E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
      ENDDO
C
      DO I=LFT,LLT
        X1L(I) = ZERO
        Y1L(I) = ZERO
        X2L(I) = E1X(I)*X21(I)+E1Y(I)*Y21(I)+E1Z(I)*Z21(I)
        Y2L(I) = E2X(I)*X21(I)+E2Y(I)*Y21(I)+E2Z(I)*Z21(I)
        X3L(I) = E1X(I)*X31(I)+E1Y(I)*Y31(I)+E1Z(I)*Z31(I)
        Y3L(I) = E2X(I)*X31(I)+E2Y(I)*Y31(I)+E2Z(I)*Z31(I)
        X4L(I) = E1X(I)*X41(I)+E1Y(I)*Y41(I)+E1Z(I)*Z41(I)
        Y4L(I) = E2X(I)*X41(I)+E2Y(I)*Y41(I)+E2Z(I)*Z41(I)
      ENDDO
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  XYZLOC3N                      source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/shell/coque/clskew.F
Chd|====================================================================
      SUBROUTINE XYZLOC3N(X1L,Y1L,X2L,Y2L,X3L,Y3L,
     .                    X1G,X2G,X3G,Y1G,Y2G,Y3G,
     .                    Z1G,Z2G,Z3G)
C-----------------------------------------------
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: X1L,Y1L,X2L,Y2L,X3L,Y3L
      my_real, DIMENSION(MVSIZ), INTENT(IN)  :: X1G,X2G,X3G,Y1G,Y2G,Y3G,
     .                                          Z1G,Z2G,Z3G
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,I1
      my_real, DIMENSION(MVSIZ) ::  SUM, RX, RY, RZ, SX, SY, SZ,
     .     E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C=======================================================================
      DO I=LFT,LLT
        RX(I) = X2G(I) - X1G(I)
        RY(I) = Y2G(I) - Y1G(I)
        RZ(I) = Z2G(I) - Z1G(I)
        SX(I) = X3G(I) - X1G(I)
        SY(I) = Y3G(I) - Y1G(I)
        SZ(I) = Z3G(I) - Z1G(I)
      ENDDO
C----------------------------
      I1 = 0
      CALL CLSKEW3(LFT,LLT,I1,
     .     RX, RY, RZ, SX, SY, SZ, 
     .     E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,SUM)
c      DO I=JFT,JLT
c        AREA(I) = HALF*SUM(I)
c      ENDDO
C----------------------------
C
      DO I=LFT,LLT
        X1L(I) = ZERO
        Y1L(I) = ZERO
        X2L(I) = E1X(I)*RX(I) + E1Y(I)*RY(I) + E1Z(I)*RZ(I)
        Y2L(I) = E2X(I)*RX(I) + E2Y(I)*RY(I) + E2Z(I)*RZ(I)
        Y3L(I) = E2X(I)*SX(I) + E2Y(I)*SY(I) + E2Z(I)*SZ(I)
        X3L(I) = E1X(I)*SX(I) + E1Y(I)*SY(I) + E1Z(I)*SZ(I)
      ENDDO
C---------------------------------------------------------
      RETURN
      END
c
Chd|====================================================================
Chd|  ACTIV_XFEM                    source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE ACTIV_XFEM(IPARG ,NFT    ,LFT    ,LLT ,NXLAY,
     .                      NG    ,IEL_CRK,ITY    ,CRKEDGE)
C-----------------------------------------------
      USE XFEM2DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,LFT,LLT,NXLAY,NG,ITY
      INTEGER IPARG(NPARG,*),IEL_CRK(*)
      TYPE (XFEM_EDGE_) , DIMENSION(*)  :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ELCRK,ILAYCUT,ILAY,IGON,ISHEON
C=======================================================================
C     TEST FOR NEW GROUP ACTIVATION
C----------------------------------------
      IGON = 0
      DO I=LFT,LLT
        ISHEON = 0
        ELCRK = IEL_CRK(I+NFT)
c        IF (ITY == 7) ELCRK = ELCRK + ECRKXFEC
        IF (ELCRK > 0) THEN
          DO ILAY = 1,NXLAY
            ILAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
            ISHEON  = MAX(ISHEON, ILAYCUT)
          ENDDO
          IF (ISHEON > 0) THEN
            IGON = 1
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IPARG(70,NG) = IGON
C-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  ELCUT4N                       source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ELCUT4N(IEL   ,IXC ,XNOD, EDGEC, FAC , IED)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c K s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEL,IXC(NIXC,*),XNOD(2,2),EDGEC(4,*),FAC,IED
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N1,N2,d1(4),d2(4)
      DATA d1/2,3,4,5/
      DATA d2/3,4,5,2/
C=======================================================================
      DO K=1,4    ! edges
        N1 = IXC(d1(K),IEL)  ! 1st Node 
        N2 = IXC(d2(K),IEL)  ! 2nd Node 
        IF ((N1 == XNOD(IED,1) .AND. N2 == XNOD(IED,2)) .OR.
     .      (N2 == XNOD(IED,1) .AND. N1 == XNOD(IED,2))) THEN
          FAC = FAC + 1
          EDGEC(K,IEL) = FAC
          EXIT
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  ELCUT3N                       source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ELCUT3N(IEL   ,IXTG ,XNOD, EDGETG, FAC , IED)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c K s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEL,IXTG(NIXTG,*),XNOD(2,2),EDGETG(3,*),FAC,IED
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N1,N2,d1(3),d2(3)
      DATA d1/2,3,4/
      DATA d2/3,4,2/
C=======================================================================
      DO K=1,3
        N1 = IXTG(d1(K),IEL)
        N2 = IXTG(d2(K),IEL)
        IF ((N1 == XNOD(IED,1) .AND. N2 == XNOD(IED,2)) .OR.
     .      (N2 == XNOD(IED,1) .AND. N1 == XNOD(IED,2))) THEN
          FAC = FAC + 1
          EDGETG(K,IEL) = FAC
          EXIT
        ENDIF
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  EDGETIP4N                     source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE EDGETIP4N(LFT   ,LLT    ,NFT    ,IELCRKC ,IEDGESH4,
     .                     NXLAY  ,EDGEC  ,TAGEDGE,CRKLVSET,CRKEDGE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c K s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NFT,IELCRKC(*),EDGEC(4,*),
     .        IEDGESH4(4,*),NXLAY,TAGEDGE(*)
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX) :: CRKLVSET
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX):: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ELCRK,IED,IEDGE,IXEL,ILEV,ILAY,ELCUT
C=======================================================================
C     set tip edges:
C--------------------
      DO ILAY=1,NXLAY
        DO I=LFT,LLT
          ELCRK = IELCRKC(I+NFT)
          ELCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (ELCUT /= 0) THEN
            DO K=1,4
              IED = EDGEC(K,I+NFT)
              IEDGE = IEDGESH4(K,ELCRK)
              IF (IED > 0)THEN
                IF (TAGEDGE(IEDGE) == 1) THEN
                  DO IXEL=1,NXEL
                    ILEV = NXEL*(ILAY-1) + IXEL
                    CRKLVSET(ILEV)%ICUTEDGE(IEDGE) = 2
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  EDGETIP3N                     source/elements/xfem/lslocal.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE EDGETIP3N(LFT   ,LLT    ,NFT    ,IELCRKTG,IEDGESH3,
     .                     NXLAY  ,EDGETG ,TAGEDGE,CRKLVSET,CRKEDGE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c K s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NFT,IELCRKTG(*),EDGETG(3,*),
     .        IEDGESH3(3,*),NXLAY,TAGEDGE(*)
      TYPE (XFEM_LVSET_) , DIMENSION(NLEVMAX) :: CRKLVSET
      TYPE (XFEM_EDGE_)  , DIMENSION(NXLAYMAX):: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ELCRK,IED,IEDGE,IXEL,ILEV,ILAY,ELCUT
C=======================================================================
C     set tip edges:
C--------------------
      DO ILAY=1,NXLAY
        DO I=LFT,LLT
          ELCRK = IELCRKTG(I+NFT) - ECRKXFEC
          ELCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (ELCUT /= 0) THEN
            DO K=1,3
              IED   = EDGETG(K,I+NFT)
              IEDGE = IEDGESH3(K,ELCRK)
              IF (IED > 0) THEN
                IF (TAGEDGE(IEDGE) == 1) THEN
                  DO IXEL=1,NXEL
                    ILEV = NXEL*(ILAY-1) + IXEL
                    CRKLVSET(ILEV)%ICUTEDGE(IEDGE) = 2
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C-----------
      RETURN
      END
