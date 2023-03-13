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
Chd|  UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRK_TAGXP3                    source/elements/xfem/upxfem_tagxp.F
Chd|        CRK_TAGXP4                    source/elements/xfem/upxfem_tagxp.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        UPENRIC3_N3                   source/elements/xfem/upenric3_nx.F
Chd|        UPENRIC3_N4                   source/elements/xfem/upenric3_nx.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPXFEM_TAGXP(XFEM_TAB,
     .                        IPARG    ,IXC     ,NGROUC  ,IGROUC  ,IXTG    ,
     .                        IADC_CRK ,IEL_CRK ,INOD_CRK,ELCUTC  ,NODEDGE ,
     .                        ENRTAG   ,CRKEDGE ,XEDGE4N ,XEDGE3N ,ITAB    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
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
#include      "com_xfem1.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),NGROUC,IGROUC(*),IEL_CRK(*),
     .   ELCUTC(2,*),IADC_CRK(*),INOD_CRK(*),IXTG(NIXTG,*),ITAB(*),
     .   NODEDGE(2,*),ENRTAG(NUMNOD,*),XEDGE4N(4,*),XEDGE3N(3,*)
C
      TYPE(ELBUF_STRUCT_), TARGET ,DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,NG,JFT,JLT,NEL,NXLAY,NF1,IXFEM,OFF,ITG1,ITG2,ACTIFXFEM
C=======================================================================
      ITG1 = 1+NUMELC
      ITG2 = 1+4*ECRKXFEC
C-----------------------------------------------
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO IG = 1, NGROUC
          NG = IGROUC(IG)
          OFF   = IPARG(8,NG)
          IXFEM = IPARG(54,NG) * IPARG(70,NG)  ! IXFEM * ACTIVEXFEM
          IF (OFF == 0 .and. IXFEM > 0) THEN
            IF (IDDW > 0) CALL STARTIMEG(NG)                                    
C---
            ITY   = IPARG(5,NG)                                                 
            NEL   = IPARG(2,NG)                                                 
            NFT   = IPARG(3,NG)                                                 
            NXLAY = IPARG(59,NG)                                                
            LFT   = 1                                                           
            LLT   = MIN(NVSIZ,NEL)                                              
            JFT = LFT                                                           
            JLT = LLT                                                           
C---
            IF (ITY == 3) THEN                                                
              CALL CRK_TAGXP4(IPARG  ,IXC     ,NFT    ,JFT     ,JLT   ,       
     .                        ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,ENRTAG,         
     .                        NXLAY  ,CRKEDGE ,XEDGE4N,ITAB    )                        
            ELSEIF (ITY == 7) THEN                                            
              CALL CRK_TAGXP3(                                                
     .          IPARG         ,IXTG          ,NFT          ,JFT     ,JLT   ,  
     .          ELCUTC(1,ITG1),IADC_CRK(ITG2),IEL_CRK(ITG1),INOD_CRK,ENRTAG,  
     .          NXLAY         ,CRKEDGE       ,XEDGE3N      ,ITAB    )                  
            ENDIF                                                             
C---
            IF (IDDW > 0) CALL STOPTIMEG(NG)                                  
          ENDIF
        ENDDO
!$OMP END DO
C-----------------------------------------------
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO IG = 1, NGROUC
          NG = IGROUC(IG)
          OFF   = IPARG(8,NG)
          IXFEM = IPARG(54,NG) * IPARG(70,NG)  ! IXFEM * ACTIVEXFEM
          IF (OFF == 0 .and. IXFEM > 0) THEN
          IF (IDDW > 0) CALL STARTIMEG(NG)
C---
            ITY   = IPARG(5,NG)
            NEL   = IPARG(2,NG)
            NFT   = IPARG(3,NG)
            LFT   = 1 
            LLT   = MIN(NVSIZ,NEL)
            JFT=LFT
            JLT=LLT
C---
            IF (ITY == 3) THEN
              CALL UPENRIC3_N4(XFEM_TAB(NG,1:NXEL),
     .                     IPARG    ,IXC     ,NFT    ,JFT     ,JLT  ,
     .                     ELCUTC   ,IADC_CRK,IEL_CRK,INOD_CRK,IXFEM,
     .                     CRKEDGE  ,XEDGE4N )
            ELSEIF (ITY == 7) THEN
              CALL UPENRIC3_N3(XFEM_TAB(NG,1:NXEL),
     .            IPARG         ,IXTG          ,NFT          ,JFT     ,JLT  ,
     .            ELCUTC(1,ITG1),IADC_CRK(ITG2),IEL_CRK(ITG1),INOD_CRK,IXFEM,
     .            CRKEDGE       ,XEDGE3N       )
            ENDIF
C---
            IF (IDDW > 0) CALL STOPTIMEG(NG)
          ENDIF
        ENDDO
!$OMP END DO
c-------------
      RETURN
      END
c
Chd|====================================================================
Chd|  CRK_TAGXP4                    source/elements/xfem/upxfem_tagxp.F
Chd|-- called by -----------
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_TAGXP4(IPARG  ,IXC     ,NFT    ,JFT     ,JLT   ,
     .                       ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,ENRTAG,
     .                       NXLAY  ,CRKEDGE ,XEDGE4N,ITAB    )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),ELCUTC(2,*),INOD_CRK(*),
     .  IADC_CRK(4,*),IEL_CRK(*),ENRTAG(NUMNOD,*),XEDGE4N(4,*),ITAB(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,IAD,NELCRK,ELCRK,ILEV,ILAY,IXEL,ELCUT,LAYCUT,
     .   IECUT,ENR0,ENR,IBOUNDEDGE,IED,EDGE,COUNT,ITIP,ITRI,NUMXEL,NSX,
     .   ISEND_NSX,ISEND_IAD
      INTEGER JCT(MVSIZ),NS(4),IADC(4)
C=======================================================================
      NELCRK = 0
      DO I=JFT,JLT
        JCT(I) = 0
        IF (ELCUTC(1,I+NFT) /= 0) THEN
          NELCRK = NELCRK + 1
          JCT(NELCRK) = I
        ENDIF
      ENDDO
      IF (NELCRK == 0) RETURN
c--------------------
      DO ILAY=1,NXLAY
        DO IR=1,NELCRK
          I = JCT(IR)
          ELCRK  = IEL_CRK(I+NFT)                       
          ELCUT  = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
          LAYCUT = ABS(CRKEDGE(ILAY)%LAYCUT(ELCRK))        
          IF (ELCUT /= 0 .and. LAYCUT == 1) THEN  !  new advancing crack
            ITRI    = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)    
            NS(1)   = IXC(2,I+NFT)       
            NS(2)   = IXC(3,I+NFT)       
            NS(3)   = IXC(4,I+NFT)       
            NS(4)   = IXC(5,I+NFT)       
            IADC(1) = IADC_CRK(1,ELCRK)  
            IADC(2) = IADC_CRK(2,ELCRK)  
            IADC(3) = IADC_CRK(3,ELCRK)  
            IADC(4) = IADC_CRK(4,ELCRK)
            ISEND_NSX = 0
            ISEND_IAD = 0
c           Tag phantom nodes with new positive enrichment to copy the velocities
            IF (ITRI /= 0) THEN
              IXEL = 3 
              ILEV = NXEL*(ILAY-1) + IXEL 
              ! sender = third phantom
              DO K=1,4                                                            
                IAD  = IADC(K)                                                     
                ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)                         
                ENR  = CRKLVSET(ILEV)%ENR0(1,IAD)                         
                EDGE = XEDGE4N(K,ELCRK) ! global egdge N                  
                IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)                
                NSX = INOD_CRK(NS(K))                                     
                IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN                   
                  XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,ENR) = IAD                                           
                  XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,ENR) = ILEV              
                  XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2   ! counter
                  ISEND_NSX = NSX   
                  ISEND_IAD = IAD   
                ENDIF                                                                
              ENDDO         
            ENDIF
c
            NUMXEL = 2    ! receiver : first or second phantom
            DO IXEL=1,NUMXEL
              ILEV = NXEL*(ILAY-1) + IXEL
              DO K=1,4                                                          
                IAD  = IADC(K)                                                   
                ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)                         
                ENR  = ABS(CRKLVSET(ILEV)%ENR0(1,IAD))                    
                EDGE = XEDGE4N(K,ELCRK) ! global egdge N                  
                IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)                
                NSX = INOD_CRK(NS(K)) 
                IF (ISEND_NSX /= NSX  .and. ISEND_IAD /= IAD) THEN                                  
                  IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN                   
                    XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,ENR) = IAD                                  
                    XFEM_PHANTOM(ILAY)%TAGXP(5,NSX,ENR) = ILEV                    
                    XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2      ! counter        
                  ENDIF                                                             
                ENDIF                                                               
              ENDDO                                                               
            ENDDO  !  DO IXEL=1,NUMXEL
C--------------------------------------------------------------------------
          ENDIF    !  IF (ELCUT /= 0 .and. LAYCUT == 1)
        ENDDO      !  DO IR=1,NELCRK
      ENDDO        !  DO ILAY=1,NXLAY
c-----------------------------------------------
      RETURN
      END
c
Chd|====================================================================
Chd|  CRK_TAGXP3                    source/elements/xfem/upxfem_tagxp.F
Chd|-- called by -----------
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_TAGXP3(IPARG  ,IXTG     ,NFT      ,JFT     ,JLT   ,
     .                      ELCUTC ,IAD_CRKTG,IEL_CRKTG,INOD_CRK,ENRTAG,
     .                      NXLAY  ,CRKEDGE  ,XEDGE3N  ,ITAB    )
C-----------------------------------------------
      USE CRACKXFEM_MOD
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
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IPARG(NPARG,*),IXTG(NIXTG,*),ELCUTC(2,*),INOD_CRK(*),
     .  IAD_CRKTG(3,*),XEDGE3N(3,*),IEL_CRKTG(*),ENRTAG(NUMNOD,*),ITAB(NUMNOD)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,ELCRK,ELCRKTG,ILEV,ILAY,IXEL,ELCUT,IECUT,NELCRK,NSX,
     .  IED,EDGE,FAC,IBOUNDEDGE,LAYCUT,ENR0,ENR,ITRI,ITIP,IAD,COUNT
      INTEGER JCT(MVSIZ),IADC(3),NS(3)
C=======================================================================
      NELCRK = 0
      DO I=JFT,JLT
        JCT(I) = 0
        IF (ELCUTC(1,I+NFT) /= 0) THEN
          NELCRK = NELCRK + 1
          JCT(NELCRK) = I
        ENDIF
      ENDDO
      IF (NELCRK == 0) RETURN
C
      DO ILAY=1,NXLAY
        DO IXEL=1,NXEL
          ILEV = NXEL*(ILAY-1) + IXEL
          DO IR=1,NELCRK
            I = JCT(IR)
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK  = ELCRKTG + ECRKXFEC
            ELCUT  = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
            IF (ELCUT /= 0) THEN
              NS(1) = IXTG(2,I+NFT)
              NS(2) = IXTG(3,I+NFT)
              NS(3) = IXTG(4,I+NFT)
              IADC(1) = IAD_CRKTG(1,ELCRKTG)
              IADC(2) = IAD_CRKTG(2,ELCRKTG)
              IADC(3) = IAD_CRKTG(3,ELCRKTG)
              ITRI    = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)
              LAYCUT  = CRKEDGE(ILAY)%LAYCUT(ELCRK)
c---------------------------------------
              IF (ABS(LAYCUT) == 1) THEN
c               new advancing crack
c               Tag phantom nodes with positive enrichment to copy the velocities
c---------------------------------------
                IF (IXEL == 3) THEN         ! sender
                  DO K=1,3                                                       
                    IAD  = IADC(K)                                                 
                    ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)
                    ENR  = CRKLVSET(ILEV)%ENR0(1,IAD)
                    EDGE = XEDGE3N(K,ELCRKTG) ! global egdge N       
                    IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                    NSX = INOD_CRK(NS(K))
                    IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN
                      XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,ENR) = IAD                                       
                      XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,ENR) = ILEV 
                      XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2   ! counter
                    ENDIF                                                             
                  ENDDO                                                               
                ELSE                        ! receiver                                             
                  DO K=1,3                                                        
                    IAD  = IADC(K)                                                 
                    ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)
                    ENR  = ABS(CRKLVSET(ILEV)%ENR0(1,IAD))
                    EDGE = XEDGE3N(K,ELCRKTG) ! global egdge N       
                    IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                    NSX = INOD_CRK(NS(K))
                    IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN
                      XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,ENR) = IAD                                  
                      XFEM_PHANTOM(ILAY)%TAGXP(5,NSX,ENR) = ILEV
                      XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2      ! counter
                    ENDIF                                                             
                  ENDDO
                ENDIF                                                             
              ENDIF   ! LAYCUT = 1                                                            
C--------------------------------------------------------------------------
            ENDIF  ! IF(ELCUT /= 0)THEN
          ENDDO  !  DO IR=1,NELCRK
        ENDDO  !  DO IXEL=1,NXEL
      ENDDO  !  DO ILAY=1,NXLAY
C-----------------------------------------------
      RETURN
      END
