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
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
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
