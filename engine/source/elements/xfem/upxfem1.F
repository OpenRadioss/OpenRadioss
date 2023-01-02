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
Chd|  UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        UPENRIC1_N3                   source/elements/xfem/upxfem1.F
Chd|        UPENRIC1_N4                   source/elements/xfem/upxfem1.F
Chd|        UPENRIC2_N3                   source/elements/xfem/upxfem1.F
Chd|        UPENRIC2_N4                   source/elements/xfem/upxfem1.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPXFEM1(XFEM_TAB,
     .                   IPARG    ,IXC     ,NGROUC  ,IGROUC  ,IXTG    ,
     .                   IADC_CRK ,IEL_CRK ,INOD_CRK,ELCUTC  ,NODEDGE ,
     .                   ENRTAG   ,CRKEDGE ,XEDGE4N ,XEDGE3N )
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
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),NGROUC,IGROUC(*),IEL_CRK(*),
     .   ELCUTC(2,*),IADC_CRK(*),INOD_CRK(*),IXTG(NIXTG,*),
     .   NODEDGE(2,*),ENRTAG(NUMNOD,*),XEDGE4N(4,*),XEDGE3N(3,*)
C
      TYPE (ELBUF_STRUCT_), TARGET ,DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG,NG,JFT,JLT,NEL,NF1,ITG1,ITG2,NXLAY,OFF,IXFEM
C=======================================================================
C     check cracks connections
c---------------------------
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
c---------------------------
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
          JFT=LFT
          JLT=LLT
c
C---      connection des fissures sur une arrete (enrichissements) : iecut=3
          IF (ITY == 3) THEN
            CALL UPENRIC1_N4(IPARG  ,IXC     ,NFT     ,JFT     ,JLT    ,
     .                       ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK ,NXLAY  ,
     .                       NODEDGE,ENRTAG  ,CRKEDGE ,XEDGE4N )
          ELSEIF (ITY == 7) THEN
            ITG1 = 1+NUMELC
            ITG2 = 1+4*ECRKXFEC
            CALL UPENRIC1_N3(
     .         IPARG  ,IXTG    ,NFT    ,JFT     ,JLT   ,
     .         ELCUTC(1,ITG1),IADC_CRK(ITG2),IEL_CRK(ITG1),INOD_CRK,NXLAY,
     .         NODEDGE       ,ENRTAG,CRKEDGE ,XEDGE3N )
          ENDIF
C---
          IF (IDDW > 0) CALL STOPTIMEG(NG)
        ENDIF
      ENDDO
!$OMP END DO
C-----------------------------------------------
C
C Boucle parallele dynamique SMP
C
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
          JFT=LFT
          JLT=LLT
C---
          IF (ITY == 3) THEN
            CALL UPENRIC2_N4(IPARG  ,IXC     ,NFT    ,JFT     ,JLT   ,
     .                    ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,ENRTAG,
     .                    NXLAY  ,CRKEDGE ,XEDGE4N )
          ELSEIF (ITY == 7) THEN
            ITG1 = 1+NUMELC
            ITG2 = 1+4*ECRKXFEC
            CALL UPENRIC2_N3(
     .        IPARG         ,IXTG          ,NFT          ,JFT     ,JLT   ,
     .        ELCUTC(1,ITG1),IADC_CRK(ITG2),IEL_CRK(ITG1),INOD_CRK,ENRTAG,
     .        NXLAY         ,CRKEDGE       ,XEDGE3N      )
          ENDIF
C---
          IF (IDDW > 0) CALL STOPTIMEG(NG)
        ENDIF
      ENDDO
!$OMP END DO
c-------------
      RETURN
      END
Chd|====================================================================
Chd|  UPENRIC1_N4                   source/elements/xfem/upxfem1.F
Chd|-- called by -----------
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPENRIC1_N4(IPARG  ,IXC     ,NFT     ,JFT     ,JLT   ,
     .                       ELCUTC ,IADC_CRK,IEL_CRK ,INOD_CRK,NXLAY ,
     .                       NODEDGE,ENRTAG  ,CRKEDGE ,XEDGE4N )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),ELCUTC(2,*),IADC_CRK(4,*),
     .  IEL_CRK(*),ENRTAG(NUMNOD,*),INOD_CRK(*),NODEDGE(2,*),XEDGE4N(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,K1,K2,IR,II,ELCRK,ILEV,LAYCUT,IECUT,ILAY,IXEL,NELCRK,
     .   IADC1,IADC2,IADC3,IADC4,IE10,IE20,IE1,IE2,NOD1,NOD2,IED,EDGE,
     .   EN1,EN2,EN3,EN4
      INTEGER JCT(MVSIZ),ENR0(4),D(4),NS(4)
      DATA D/2,3,4,1/
      TYPE (XFEM_EDGE_) , DIMENSION(*) :: CRKEDGE
C=======================================================================
c     tag all standard cracked elements (all layers included)
      NELCRK = 0
      DO I=JFT,JLT
        JCT(I) = 0
        IF (ELCUTC(1,I+NFT) /= 0) THEN
          NELCRK = NELCRK + 1
          JCT(NELCRK) = I
        ENDIF
      ENDDO
      IF (NELCRK == 0) RETURN
C---
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)    
        DO IR=1,NELCRK
          I = JCT(IR)
          ELCRK  = IEL_CRK(I+NFT)
          LAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (LAYCUT /= 0) THEN
            IADC1 = IADC_CRK(1,ELCRK)
            IADC2 = IADC_CRK(2,ELCRK)
            IADC3 = IADC_CRK(3,ELCRK)
            IADC4 = IADC_CRK(4,ELCRK)
            NS(1) = IXC(2,I+NFT)
            NS(2) = IXC(3,I+NFT)
            NS(3) = IXC(4,I+NFT)
            NS(4) = IXC(5,I+NFT)
C
            DO IXEL=1,NXEL
              ILEV = II+IXEL  
              ENR0(1) = 0
              ENR0(2) = 0
              ENR0(3) = 0
              ENR0(4) = 0
              EN1 = CRKLVSET(ILEV)%ENR0(1,IADC1)
              EN2 = CRKLVSET(ILEV)%ENR0(1,IADC2)
              EN3 = CRKLVSET(ILEV)%ENR0(1,IADC3)
              EN4 = CRKLVSET(ILEV)%ENR0(1,IADC4)
              IF (EN1 /= 0) ENR0(1) = EN1
              IF (EN2 /= 0) ENR0(2) = EN2
              IF (EN3 /= 0) ENR0(3) = EN3
              IF (EN4 /= 0) ENR0(4) = EN4
C
              DO K=1,4
                EDGE  = XEDGE4N(K,ELCRK)
                IECUT = CRKEDGE(ILAY)%ICUTEDGE(EDGE)
                IE1   = 0
                IE2   = 0
                IED   = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                IF (IECUT == 3 .and. IED > 0) THEN ! connection edge  (crklayer_adv,_ini)
                  NOD1 = NODEDGE(1,EDGE)
                  NOD2 = NODEDGE(2,EDGE)
                  IE10 = CRKEDGE(ILAY)%EDGEENR(1,EDGE)
                  IE20 = CRKEDGE(ILAY)%EDGEENR(2,EDGE)
                  IF (NOD1 == IXC(K+1,I+NFT) .and.
     .                NOD2 == IXC(d(K)+1,I+NFT)) THEN
                    K1 = K
                    K2 = d(K)
                    IE1 = ENR0(K)
                    IE2 = ENR0(d(K))
                  ELSE IF (NOD2 == IXC(K+1,I+NFT) .and.
     .                     NOD1 == IXC(d(K)+1,I+NFT)) THEN
                    K1 = d(K)
                    K2 = K
                    IE1 = ENR0(d(K))
                    IE2 = ENR0(K)
                  ENDIF
c
c                 set ENRTAG for nodal enrichment update
c
                  IF (IE1 /= 0) ENRTAG(NS(K1),ABS(IE1))
     .                        = MAX(ENRTAG(NS(K1),ABS(IE1)),IE10)
                  IF (IE2 /= 0) ENRTAG(NS(K2),ABS(IE2))
     .                        = MAX(ENRTAG(NS(K2),ABS(IE2)),IE20)
     
c                  if (IE1 /= 0) then
c                    write(*,'(A,3I5)') 'UPX1: NS,IE,ENRTAG=',NS(K1),IE1,ENRTAG(NS(K1),ABS(IE1))
c                  endif
c                  if (IE2 /= 0) then
c                    write(*,'(A,3I5)') 'UPX1: NS,IE,ENRTAG=',NS(K2),IE1,ENRTAG(NS(K2),ABS(IE2))
c                  endif
C
                ENDIF ! IF (IECUT == 3)
              ENDDO ! DO K=1,4
            ENDDO   ! IXEL=1,NXEL
          ENDIF   ! IF (LAYCUT /= 0)
        ENDDO     ! DO IR=1,NELCRK
      ENDDO       ! DO ILAY=1,NXLAY
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  UPENRIC2_N4                   source/elements/xfem/upxfem1.F
Chd|-- called by -----------
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPENRIC2_N4(IPARG  ,IXC     ,NFT    ,JFT     ,JLT   ,
     .                       ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,ENRTAG,
     .                       NXLAY  ,CRKEDGE ,XEDGE4N)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),ELCUTC(2,*),INOD_CRK(*),
     .  IADC_CRK(4,*),IEL_CRK(*),ENRTAG(NUMNOD,*),XEDGE4N(4,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,IAD,NELCRK,ELCRK,ILEV,ILAY,IXEL,ELCUT,LAYCUT,
     .   IECUT,ENR,IBOUNDEDGE,IED,EDGE,FAC,COUNT,ITIP,ITRI,NUMXEL,NSX
      INTEGER JCT(MVSIZ),NTAG(4),D(4),NS(4),IADC(4)
      DATA D/2,3,4,1/
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
          ELCRK = IEL_CRK(I+NFT)                       
          ELCUT  = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)     
          IF (ELCUT /= 0) THEN
            ITRI    = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)    
            LAYCUT  = CRKEDGE(ILAY)%LAYCUT(ELCRK)         
            NS(1)   = IXC(2,I+NFT)       
            NS(2)   = IXC(3,I+NFT)       
            NS(3)   = IXC(4,I+NFT)       
            NS(4)   = IXC(5,I+NFT)       
            IADC(1) = IADC_CRK(1,ELCRK)  
            IADC(2) = IADC_CRK(2,ELCRK)  
            IADC(3) = IADC_CRK(3,ELCRK)  
            IADC(4) = IADC_CRK(4,ELCRK)
            IF (ITRI == 0) THEN
              NUMXEL = 2
            ELSE
              NUMXEL = NXEL
            ENDIF 
c
            DO IXEL=1,NUMXEL
              ILEV = NXEL*(ILAY-1) + IXEL
              FAC      = 0
              NTAG(1:4)= 0
c---------------------------------------
              IF (ABS(LAYCUT) == 1) THEN
c               new advancing (or init) crack => LAYCUT = +/- 1
c---------------------------------------
                DO K=1,4
                  IED   = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)   ! = 1,2 => sequence des arretes coupes
                  EDGE  = XEDGE4N(K,ELCRK) ! global egdge N 
                  IECUT = CRKEDGE(ILAY)%ICUTEDGE(EDGE)
c                 Tip edges => NTAG = 2               
                  IF (IED > 0 .and. IECUT == 2) THEN    ! (cas IECUT=3 traite avant)
                    NTAG(K)    = 2
                    NTAG(d(K)) = 2
                    FAC = FAC + 1
                  ENDIF
                ENDDO
c               crack touches boundary or goes through a hole due to erased element
c               touching boundary    => (IBOUNDEDGE == 1)
c               going through a hole => (IBOUNDEDGE == 2)
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
c                 Tag boundary edges : NTAG=1               
                  IF ( IED > 0 .and. IBOUNDEDGE > 0) THEN
                    NTAG(K)    = 1
                    NTAG(d(K)) = 1
                    FAC = FAC + 1
                  ENDIF
                ENDDO
c
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  ITIP = CRKEDGE(ILAY)%EDGETIP(2,EDGE)
                  IF (IED > 0 .and. ITIP == 1) THEN
                    IF (ITRI /= 0 .and. IXEL == 3) THEN  
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                      FAC = FAC + 1
                    ENDIF                           
                  ENDIF                           
                ENDDO
c               NTAG = 1 for all except internal tip nodes
                IF (FAC > 0) THEN
                  DO K=1,4
                    IF (NTAG(K) /= 2) NTAG(K) = 1
                  ENDDO
                ENDIF
c---------------------------------------
              ELSE IF (LAYCUT == 2) THEN
c               old crack, element already cut previously
c---------------------------------------
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                  IF (IED > 0 .and. IBOUNDEDGE == 2) THEN
                    NTAG(K)    = 1
                    NTAG(d(K)) = 1
                    FAC = FAC + 1
                  ENDIF
                END DO
c
c               check if no tip edge in phantom element (count=0 => no tip)
c
                COUNT = 0
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  ITIP = CRKEDGE(ILAY)%EDGETIP(2,EDGE)
                  IF (IED > 0 .and. ITIP == 1) COUNT = COUNT + 1
                END DO
c                
                IF (COUNT == 0) THEN  ! already cracked phantom, no tip edge => no negative enrichments
                  DO K=1,4
                    IAD  = IADC(K)
                    IF (ITRI < 0 .and. IXEL == 2 .and. CRKLVSET(ILEV)%ENR0(1,IAD) < 0) THEN
c                     enrichment of IXEL = 2 < 0 => copy from IXEL = 3 and tag edge
                      CRKLVSET(ILEV)%ENR0(1,IAD) = CRKLVSET(ILEV+1)%ENR0(1,IAD)
                      FAC = FAC + 1
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                    ELSEIF (ITRI > 0 .and. IXEL == 1 .and. CRKLVSET(ILEV)%ENR0(1,IAD) < 0) THEN
c                     enrichment of IXEL = 1 < 0 => copy from IXEL = 3 and tag edge
                      CRKLVSET(ILEV)%ENR0(1,IAD) = CRKLVSET(ILEV+2)%ENR0(1,IAD)
                      FAC = FAC + 1
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                    ENDIF
                  END DO
                ENDIF  !  COUNT == 0
c                
              ENDIF    !  LAYCUT
C--------------------------------------------------------------------------
c             set ENRTAG for nodal enrichment update (ENR > 0 only for boundary nodes)
C--------------------------------------------------------------------------
              IF (FAC > 0) THEN
                DO K=1,4
                  ENR = ABS(CRKLVSET(ILEV)%ENR0(1,IADC(K)))
                  IF (ENR > 0 .and. NTAG(K) == 1) THEN
                    IF (ENRTAG(NS(K),ENR) == 0) ENRTAG(NS(K),ENR) = ENR
                  ENDIF
                ENDDO
              ENDIF
C--------------------------------------------------------------------------
            ENDDO  !  DO IXEL=1,NXEL
          ENDIF    !  IF(ELCUT /= 0)THEN
        ENDDO      !  DO IR=1,NELCRK
      ENDDO        !  DO ILAY=1,NXLAY
C-----------------------------------------------
      RETURN
      END
c
Chd|====================================================================
Chd|  UPENRIC1_N3                   source/elements/xfem/upxfem1.F
Chd|-- called by -----------
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPENRIC1_N3(IPARG  ,IXTG     ,NFT       ,JFT     ,JLT   ,
     .                       ELCUTC ,IAD_CRKTG,IEL_CRKTG ,INOD_CRK,NXLAY ,
     .                       NODEDGE,ENRTAG   ,CRKEDGE   ,XEDGE3N )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXTG(NIXTG,*),NFT,JFT,JLT,NXLAY,
     .        ELCUTC(2,*),IAD_CRKTG(3,*),XEDGE3N(3,*),IEL_CRKTG(*),
     .        INOD_CRK(*),NODEDGE(2,*),ENRTAG(NUMNOD,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,II,ELCRK,ILEV,LAYCUT,IECUT,ILAY,IXEL,
     .   IR,NELCRK,EDGE,IADC1,IADC2,IADC3,NOD1,NOD2,ELCRKTG,IED,
     .   IE10,IE20,IE1,IE2,K1,K2,EN1,EN2,EN3
      INTEGER JCT(MVSIZ),ENR0(3),NS(3),D(3)
      DATA d/2,3,1/
C=======================================================================
      IR = 0
      DO I=JFT,JLT
        JCT(I) = 0
        IF (ELCUTC(1,I+NFT) /= 0) THEN
          IR = IR + 1
          JCT(IR) = I
        ENDIF
      ENDDO
C---
      NELCRK = IR
      IF (NELCRK == 0) RETURN
C---
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)    
        DO IR=1,NELCRK
          I = JCT(IR)
          ELCRKTG = IEL_CRKTG(I+NFT)
          ELCRK   = ELCRKTG + ECRKXFEC
          LAYCUT  = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (LAYCUT /= 0) THEN
            IADC1 = IAD_CRKTG(1,ELCRKTG)
            IADC2 = IAD_CRKTG(2,ELCRKTG)
            IADC3 = IAD_CRKTG(3,ELCRKTG)
            NS(1) = IXTG(2,I+NFT)
            NS(2) = IXTG(3,I+NFT)
            NS(3) = IXTG(4,I+NFT)
c
            DO IXEL=1,NXEL
              ILEV = II+IXEL  
              ENR0(1) = 0
              ENR0(2) = 0
              ENR0(3) = 0
              EN1 = CRKLVSET(ILEV)%ENR0(1,IADC1)
              EN2 = CRKLVSET(ILEV)%ENR0(1,IADC2)
              EN3 = CRKLVSET(ILEV)%ENR0(1,IADC3)
              IF (EN1 /= 0) ENR0(1) = EN1
              IF (EN2 /= 0) ENR0(2) = EN2
              IF (EN3 /= 0) ENR0(3) = EN3
c
              DO K=1,3
                EDGE  = XEDGE3N(K,ELCRKTG)
                IED   = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
                IECUT = CRKEDGE(ILAY)%ICUTEDGE(EDGE)
                IE1 = 0
                IE2 = 0
                IF (IECUT == 3 .AND. IED > 0) THEN ! connection edge
                  NOD1 = NODEDGE(1,EDGE)
                  NOD2 = NODEDGE(2,EDGE)
                  IE10 = CRKEDGE(ILAY)%EDGEENR(1,EDGE)
                  IE20 = CRKEDGE(ILAY)%EDGEENR(2,EDGE)
                  IF (NOD1 == IXTG(K+1,I+NFT) .and.
     .                NOD2 == IXTG(d(K)+1,I+NFT)) THEN
                    K1  = K
                    K2  = d(K)
                    IE1 = ENR0(K)
                    IE2 = ENR0(d(K))
                  ELSE IF (NOD2 == IXTG(K+1,I+NFT) .and.
     .                     NOD1 == IXTG(d(K)+1,I+NFT)) THEN
                    K1  = d(K)
                    K2  = K
                    IE1 = ENR0(d(K))
                    IE2 = ENR0(K)
                  ENDIF
c
c                 set ENRTAG for nodal enrichment update
                  IF(IE1 /= 0) ENRTAG(NS(K1),ABS(IE1))
     .                               = MAX(ENRTAG(NS(K1),ABS(IE1)),IE10)
                  IF(IE2 /= 0) ENRTAG(NS(K2),ABS(IE2))
     .                               = MAX(ENRTAG(NS(K2),ABS(IE2)),IE20)
c
                ENDIF ! IF(IECUT == 3)THEN
              ENDDO ! DO K=1,3
            ENDDO   ! IXEL=1,NXEL
          ENDIF ! IF(LAYCUT /= 0)THEN
        ENDDO ! DO IR=1,NELCRK
      ENDDO ! DO ILAY=1,NXLAY
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  UPENRIC2_N3                   source/elements/xfem/upxfem1.F
Chd|-- called by -----------
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPENRIC2_N3(IPARG  ,IXTG     ,NFT      ,JFT     ,JLT   ,
     .                       ELCUTC ,IAD_CRKTG,IEL_CRKTG,INOD_CRK,ENRTAG,
     .                       NXLAY  ,CRKEDGE  ,XEDGE3N  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IPARG(NPARG,*),IXTG(NIXTG,*),ELCUTC(2,*),INOD_CRK(*),
     .  IAD_CRKTG(3,*),XEDGE3N(3,*),IEL_CRKTG(*),ENRTAG(NUMNOD,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,ELCRK,ELCRKTG,ILEV,ILAY,IXEL,ELCUT,IECUT,NELCRK,NSX,
     .  IED,EDGE,FAC,IBOUNDEDGE,LAYCUT,ENR,ITRI,ITIP,IAD,COUNT
      INTEGER JCT(MVSIZ),IADC(3),ENR0(3),IENR(3),NTAG(3),d(3),NS(3)
      DATA D/2,3,1/
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
C
              FAC      = 0
              NTAG(1:3)= 0
              ITRI     = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)
              LAYCUT   = CRKEDGE(ILAY)%LAYCUT(ELCRK)
c---------------------------------------
              IF (ABS(LAYCUT) == 1) THEN
c               new advancing crack
c---------------------------------------
                DO K=1,3
                  IED  = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)   ! = 1,2 => sequence des arretes coupes
                  EDGE = XEDGE3N(K,ELCRKTG)
                  IECUT = CRKEDGE(ILAY)%ICUTEDGE(EDGE)
c                 Tip edges => NTAG = 2               
                  IF (IED > 0 .and. IECUT == 2) THEN    ! (cas IECUT=3 traite avant)
                    NTAG(K)    = 2
                    NTAG(d(K)) = 2
                    FAC = FAC + 1
                  ENDIF
                ENDDO
c               crack touches boundary or goes through a hole due to erased element
c               touching boundary    => (IBOUNDEDGE == 1)
c               going through a hole => (IBOUNDEDGE == 2)
                DO K=1,3
                  EDGE = XEDGE3N(K,ELCRKTG)
                  IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
c                 Tag boundary edges : NTAG=1               
                  IED = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
                  IF (IBOUNDEDGE > 0 .AND. IED > 0) THEN
                    NTAG(K)    = 1
                    NTAG(d(K)) = 1
                    FAC = FAC + 1
                  ENDIF
                ENDDO
                DO K=1,3
                  IED  = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
                  EDGE = XEDGE3N(K,ELCRKTG)
                  ITIP = CRKEDGE(ILAY)%EDGETIP(2,EDGE)
                  IF (IED > 0 .and. ITIP == 1) THEN
                    IF (ITRI /= 0 .and. IXEL == 3) THEN  
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                      FAC = FAC + 1
                    ENDIF                           
                  ENDIF                           
                ENDDO
c
c               NTAG = 1 for all except internal tip nodes
                IF (FAC > 0) THEN
                  DO K=1,3
                    IF (NTAG(K) /= 2) NTAG(K) = 1
                  END DO
                ENDIF
c---------------------------------------
              ELSE IF (LAYCUT == 2) THEN
c               old crack, element already cut previously
c---------------------------------------
                DO K=1,3
                  IED  = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
                  EDGE = XEDGE3N(K,ELCRKTG)
                  IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                  IF (IED > 0 .and. IBOUNDEDGE == 2) THEN
                    NTAG(K)    = 1
                    NTAG(d(K)) = 1
                    FAC = FAC + 1
                  ENDIF
                ENDDO
c
c               check if no tip edge in phantom element (count=0 => no tip)
c
                COUNT = 0
                DO K=1,3
                  IED  = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
                  EDGE = XEDGE3N(K,ELCRKTG)
                  ITIP = CRKEDGE(ILAY)%EDGETIP(2,EDGE)
                  IF (IED > 0 .and. ITIP == 1) COUNT = COUNT + 1
                END DO
c                
                IF (COUNT == 0) THEN  ! already cracked phantom, no tip edge => no negative enrichments
                  DO K=1,3
                    IED  = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
                    EDGE = XEDGE3N(K,ELCRKTG)
                    IAD  = IADC(K)
                    IF (ITRI < 0 .and. IXEL == 2 .and. CRKLVSET(ILEV)%ENR0(1,IAD) < 0) THEN
c                     enrichment of IXEL = 2 < 0 => copy from IXEL = 3 and tag edge
                      CRKLVSET(ILEV)%ENR0(1,IAD) = CRKLVSET(ILEV+1)%ENR0(1,IAD)
                      FAC = FAC + 1
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                    ELSEIF (ITRI > 0.and. IXEL == 1 .and. CRKLVSET(ILEV)%ENR0(1,IAD) < 0) THEN
c                     enrichment of IXEL = 1 < 0 => copy from IXEL = 3 and tag edge
                      CRKLVSET(ILEV)%ENR0(1,IAD) = CRKLVSET(ILEV+2)%ENR0(1,IAD)
                      FAC = FAC + 1
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                    ENDIF
                  END DO
                ENDIF  !  COUNT == 0
c
              ENDIF  !  LAYCUT
C--------------------------------------------------------------------------
c             set ENRTAG for nodal enrichment update (ENR > 0 only for boundary nodes)
C--------------------------------------------------------------------------
              IF (FAC > 0) THEN
                DO K=1,3
                  ENR = ABS(CRKLVSET(ILEV)%ENR0(1,IADC(K)))
                  IF (ENR > 0 .AND. NTAG(K) == 1) THEN
                    IF (ENRTAG(NS(K),ENR) == 0) ENRTAG(NS(K),ENR) = ENR
                  ENDIF
                ENDDO
              ENDIF
C--------------------------------------------------------------------------
            ENDIF  ! IF(ELCUT /= 0)THEN
          ENDDO  !  DO IR=1,NELCRK
        ENDDO  !  DO IXEL=1,NXEL
      ENDDO  !  DO ILAY=1,NXLAY
C-----------------------------------------------
      RETURN
      END
