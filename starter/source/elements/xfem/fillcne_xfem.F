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
Chd|  FILLCNE_XFEM                  source/elements/xfem/fillcne_xfem.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE FILLCNE_XFEM(LCNE_CRKXFEM,IPARG,
     .   IEL_CRKXFEM   ,INOD_CRKXFEM   ,IXC    ,IXTG , CEP,
     .   ADDCNE_CRKXFEM,CNE_XFE,CEL_XFE,CEP_XFE,CRKNODIAD )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCNE_CRKXFEM
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),ADDCNE_CRKXFEM(0:NCRKXFE+1),
     .  CNE_XFE(LCNE_CRKXFEM),IEL_CRKXFEM(NUMELC+NUMELTG),INOD_CRKXFEM(*),
     .  CEP(*),CEL_XFE(ECRKXFE),CEP_XFE(ECRKXFE),CRKNODIAD(LCNE_CRKXFEM),
     .  IPARG(NPARG,NGROUP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,NG,NP,NEL,NFT,ITY,ITYO,II,III,NIN,P,PROC,INDX,OFFC,OFFTG
      INTEGER ADSKY(0:NCRKXFE+1)
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: KNOD2ELC
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: TAGSKYC,TAGSKYTG
      INTEGER, DIMENSION(70000)    :: WORK
      INTEGER, DIMENSION(NUMELC)   :: ITRIC
      INTEGER, DIMENSION(NUMELTG)  :: ITRITG
      INTEGER, DIMENSION(NUMELC*2) :: INDXC
      INTEGER, DIMENSION(NUMELTG*2):: INDXTG
C=======================================================================
C   CALCUL DE CNE ADDCNE_CRKXFEM CEL for XFEM part
C-----------------------------------------------
      ALLOCATE(KNOD2ELC(NUMNOD+1))
      KNOD2ELC = 0
      ALLOCATE(TAGSKYC(4,NUMELC))
      TAGSKYC = 0
      ALLOCATE(TAGSKYTG(3,NUMELTG))
      TAGSKYTG = 0
C
      DO I = 0, NCRKXFE + 1           ! NCRKXFE = Nb of nodes xfem
        ADSKY(I) = ADDCNE_CRKXFEM(I)    
      ENDDO
C
      OFFC  = NUMELS + NUMELQ
      OFFTG = OFFC  + NUMELT + NUMELP + NUMELR + NUMELC
c
c---------------------------
c     Connectivities
c---------------------------
      DO I = 1, NUMELC
        ITRIC(I) = IXC(7,I)    !  ID elements std dans l'ordre d'input
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRIC,INDXC,NUMELC,1)
c
      DO I = 1, NUMELTG
        ITRITG(I) = IXTG(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRITG,INDXTG,NUMELTG,1)
c---------------------------
      DO J=1,NUMELC
        I = INDXC(J)
        DO K=1,4
          N = IXC(K+1,I)
          KNOD2ELC(N)  = KNOD2ELC(N) + 1
          TAGSKYC(K,I) = KNOD2ELC(N)      ! Nb d'elements std connectes a un noeud
        END DO
      END DO
c---
      DO J=1,NUMELTG
        I = INDXTG(J)
        DO K=1,3
          N = IXTG(K+1,I)
          KNOD2ELC(N)   = KNOD2ELC(N) + 1
          TAGSKYTG(K,I) = KNOD2ELC(N)
        END DO
      END DO
c---------------------------
c     SHELL -4N- Connectivities
c---------------------------
      INDX = 0
      DO J=1,NUMELC
        I = INDXC(J)
        IF (IEL_CRKXFEM(I) > 0) THEN
          INDX = INDX + 1
          DO K=1,4
            N  = IXC(K+1,I)          ! Num noeud std
            NP = INOD_CRKXFEM(N)     ! Num noeud phantome
            CNE_XFE(ADSKY(NP)) = I  
            CRKNODIAD(ADSKY(NP)) = TAGSKYC(K,I)
            ADSKY(NP) = ADSKY(NP) + 1
          ENDDO
        ENDIF 
      ENDDO
c---------------------------
c     SHELL -3N- Connectivities
c---------------------------
      DO J=1,NUMELTG
        I = INDXTG(J)
        IF (IEL_CRKXFEM(I+NUMELC) > 0) THEN
          INDX = INDX + 1
          DO K=1,3
            N = IXTG(K+1,I)
            NP = INOD_CRKXFEM(N)
            CNE_XFE(ADSKY(NP)) = I + NUMELC
            CRKNODIAD(ADSKY(NP)) = TAGSKYTG(K,I)
            ADSKY(NP) = ADSKY(NP) + 1
          ENDDO
        ENDIF 
      ENDDO
C-----------------------------------------------
c     Remplissage de CEL_XFE, CEP_XFE :  Element Xfem Global/Local
C-----------------------------------------------
c     SHELL -4N-
c
      DO PROC = 1, NSPMD
        NIN = 0
        DO NG = 1, NGROUP
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          ITY = IPARG(5,NG)
          P   = IPARG(32,NG)+1
          IF (ITY == 3) THEN
            IF (P == PROC) THEN
              DO I = 1, NEL
                N = IEL_CRKXFEM(I+NFT)
                IF (N > 0) THEN
                  NIN = NIN + 1
                  CEL_XFE(N) = NIN
                  CEP_XFE(N) = P-1
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO
c
c     SHELL -3N-
c
      DO PROC = 1, NSPMD
        NIN = 0
        DO NG = 1, NGROUP
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          ITY = IPARG(5,NG)
          P   = IPARG(32,NG)+1
          IF (ITY == 7) THEN
            IF (P == PROC) THEN
              II = NUMELC + NFT
              DO I = 1, NEL
                N = IEL_CRKXFEM(II + I)
                IF (N > 0) THEN
                  NIN = NIN + 1
                  CEL_XFE(N) = NIN
                  CEP_XFE(N) = P-1
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO
c-----------
      DEALLOCATE(TAGSKYC,TAGSKYTG,KNOD2ELC)
c-----------
      RETURN
      END
