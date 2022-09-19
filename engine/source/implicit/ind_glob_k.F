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
Chd|  DIM_ELEMS1                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_I                     source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_ELEMS1(
     1    IGEO      ,ELBUF     ,IPARG     ,IXS       ,IXQ       ,
     2    IXC       ,IXT       ,IXP       ,IXR       ,IXTG      ,
     3    IXTG1     ,IXS10     ,IXS20     ,IXS16     ,IXUR      ,
     4    NDOF      ,NROW      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IGEO(NPROPGI,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),NDOF(*),NROW(*)
C     REAL
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(20),NG,ITY,NEL,NFT,IAD,ISNOD,ICNOD,
     .        I,J,K,N,M,EP,L,IGTYP,NNOD,K1,IHBE,EP1,IAD0,NPT,IDRIL
      my_real
     .   IOF
C--------NROW(NUMNOD) : number of connnected nodes (sym)
C----6---------------------------------------------------------------7---------8
      DO 100 NG=1,NGROUP
       IF (IPARG(8,NG)/=1) THEN
        ITY=IPARG(5,NG)
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        NPT=IPARG(6,NG)
        ICNOD=IPARG(11,NG)
        ISNOD=IPARG(28,NG)
        IDRIL=IPARG(41,NG)
        IAD0 = IAD-1
C----------no ndof defined for void, rigid mat add dof to pass U_D later-       
       IF (IPARG(1,NG) == 0 .OR. IPARG(1,NG) == 13) THEN
C----------------deformable       
       ELSE
C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
       IF (ITY==1) THEN
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         NNOD=8
         EP=I+NFT
         IF (ISNOD==4) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
         ELSEIF (ISNOD==6) THEN
          NNOD=6
          NC(1)=IXS(2,EP)
          NC(2)=IXS(3,EP)
          NC(3)=IXS(4,EP)
          NC(4)=IXS(6,EP)
          NC(5)=IXS(7,EP)
          NC(6)=IXS(8,EP)
         ELSEIF (ISNOD==10) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
          EP1=EP-NUMELS8
          DO J=1,6
           IF (IXS10(J,EP1)>0) THEN
            NNOD = NNOD + 1
            NC(NNOD) = IXS10(J,EP1)
           ENDIF
          ENDDO
         ELSEIF (ISNOD==8) THEN
          NNOD=8
         DO J=1,NNOD
          NC(J)=IXS(J+1,EP)
         ENDDO
         ELSEIF (ISNOD==20) THEN
          NNOD=20
         DO J=1,8
          NC(J)=IXS(J+1,EP)
         ENDDO
         EP1=EP-(NUMELS8+NUMELS10)
         DO J=9,20
          NC(J)=IXS20(J-8,EP1)
         ENDDO

         ELSE
          NNOD=0
         ENDIF
         CALL REORDER_I(NNOD,NC)
         DO J=1,NNOD
          N=NC(J)
          NDOF(N)=MAX(3,NDOF(N))
          DO L=J+1,NNOD
           IF (N/=NC(L)) NROW(N)=NROW(N)+1
          ENDDO
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     2. ELEMENTS 2D
C-----------------------
       ELSEIF(ITY==2)THEN
        DO I=1,NEL
          IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
C
         NNOD=4
         EP=I+NFT
C        IF (ISNOD==4) THEN
C         NNOD=4
          DO J=1,NNOD
           NC(J)=IXQ(J+1,EP)
          ENDDO
C        ELSE
C         NNOD=0
C        ENDIF
C
         CALL REORDER_I(NNOD,NC)
         DO J=1,NNOD
          N=NC(J)
          NDOF(N)=MAX(3,NDOF(N)) !3
          DO L=J+1,NNOD
           IF (N/=NC(L)) NROW(N)=NROW(N)+1
          ENDDO
         ENDDO
C
        ENDIF
        ENDDO
C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
       ELSEIF(ITY==3)THEN
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         NNOD=4
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXC(J+1,EP)
         ENDDO
         CALL REORDER_I(NNOD,NC)
         DO J=1,NNOD
          N=NC(J)
          IF (NPT==1.AND.IDRIL==0) THEN
           NDOF(N)=MAX(3,NDOF(N))
          ELSE
           NDOF(N)=6
          END IF
          DO L=J+1,NNOD
           IF (N/=NC(L)) NROW(N)=NROW(N)+1
          ENDDO
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     4. ELEMENTS TIGES
C-----------------------
       ELSEIF(ITY==4)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXT(2,EP)
         NC(2)=IXT(3,EP)
         CALL REORDER_I(NNOD,NC)
         DO J=1,NNOD
          N=NC(J)
          NDOF(N)=MAX(3,NDOF(N))
          DO L=1,NNOD
           IF (N/=NC(L)) NROW(N)=NROW(N)+1
          ENDDO
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
       ELSEIF(ITY==5)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXP(2,EP)
         NC(2)=IXP(3,EP)
         CALL REORDER_I(NNOD,NC)
         DO J=1,NNOD
          N=NC(J)
          NDOF(N)=6
          DO L=J+1,NNOD
           IF (N/=NC(L)) NROW(N)=NROW(N)+1
          ENDDO
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
       ELSEIF(ITY==6)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXR(2,EP)
         NC(2)=IXR(3,EP)
         IGTYP = IGEO(11,IXR(1,EP))
         IF (IGTYP==12) THEN
          NNOD=3
          NC(3)=IXR(4,EP)
         ENDIF
         CALL REORDER_I(NNOD,NC)
         DO J=1,NNOD
          N=NC(J)
          DO L=J+1,NNOD
           IF (N/=NC(L)) NROW(N)=NROW(N)+1
          ENDDO
         ENDDO
         IF (IGTYP==8.OR.IGTYP==13) THEN
          DO J=1,NNOD
           NDOF(NC(J))=6
          ENDDO
         ELSEIF (IGTYP==4.OR.IGTYP==12.OR.IGTYP==32) THEN
          DO J=1,NNOD
           NDOF(NC(J))=MAX(3,NDOF(NC(J)))
          ENDDO
         ENDIF
        ENDIF
        ENDDO
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
       ELSEIF(ITY==7.AND.ICNOD/=6)THEN
        NNOD=3
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXTG(J+1,EP)
         ENDDO
         CALL REORDER_I(NNOD,NC)
         DO J=1,NNOD
          N=NC(J)
          IF (NPT==1.AND.IDRIL==0) THEN
           NDOF(N)=MAX(3,NDOF(N))
          ELSE
           NDOF(N)=6
          END IF
          DO L=J+1,NNOD
           IF (N/=NC(L)) NROW(N)=NROW(N)+1
          ENDDO
         ENDDO
        ENDIF
        ENDDO
       ENDIF
C
       END IF !(IPARG(1,NG) == 0 .OR. IPARG(1,NG) == 13) THEN
       ENDIF
  100 CONTINUE
      RETURN
      END
C------version \\-----
Chd|====================================================================
Chd|  DIM_ELEMS3                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_ELEMAX                    source/implicit/ind_glob_k.F  
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_ELEMS3(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW      ,
     4    INLOC     ,NKMAX     ,ICOK      ,IGEO      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NKMAX,IGEO(NPROPGI,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),NROW(*),ICOK(NKMAX,*),INLOC(*)
C     REAL
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(20),NG,ITY,NEL,NFT,IAD,ISNOD,ICNOD,
     .        I,J,K,N,M,EP,L,IGTYP,NNOD,K1,IHBE,NK,EP1,IAD0
      my_real
     .  IOF
C--------NROW(NUMNOD) : number of connnected nodes (non sym)
C----6---------------------------------------------------------------7---------8
      DO 100 NG=1,NGROUP
       IF (IPARG(8,NG)/=1) THEN
        ITY=IPARG(5,NG)
        NEL=IPARG(2,NG)
C----------void, rigid mat        
        IF (IPARG(1,NG) == 0 .OR. IPARG(1,NG) == 13) CYCLE
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ICNOD=IPARG(11,NG)
        ISNOD=IPARG(28,NG)
        IAD0 = IAD-1
C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
       IF (ITY==1) THEN
        NNOD=8
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         IF (ISNOD==4) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
         ELSEIF (ISNOD==6) THEN
          NNOD=6
          NC(1)=IXS(2,EP)
          NC(2)=IXS(3,EP)
          NC(3)=IXS(4,EP)
          NC(4)=IXS(6,EP)
          NC(5)=IXS(7,EP)
          NC(6)=IXS(8,EP)
         ELSEIF (ISNOD==10) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
          EP1=EP-NUMELS8
          DO J=1,6
           IF (IXS10(J,EP1)>0) THEN
            NNOD = NNOD + 1
            NC(NNOD) = IXS10(J,EP1)
           ENDIF
          ENDDO
         ELSEIF (ISNOD==8) THEN
          NNOD=8
         DO J=1,NNOD
          NC(J)=IXS(J+1,EP)
         ENDDO

Czg add solid element 20
         ELSEIF (ISNOD==20) THEN
          NNOD=20
         DO J=1,8
          NC(J)=IXS(J+1,EP)
         ENDDO
         EP1=EP-(NUMELS8+NUMELS10)
         DO J=9,20
          NC(J)=IXS20(J-8,EP1)
         ENDDO

         ELSE
          NNOD=0
         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     2. ELEMENTS 2D
C-----------------------
       ELSEIF(ITY==2)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
C
          EP=I+NFT
C         IF (ISNOD==4) THEN
C           NNOD=4
            DO J=1,NNOD
              NC(J)=IXQ(J+1,EP)
            ENDDO
C         ELSE
C           NNOD=0
C         ENDIF
C
          DO J=1,NNOD
           N=NC(J)
           NK=INLOC(N)
           IF (NK>0) THEN
            DO L=1,NNOD
             IF (N/=NC(L)) THEN
              CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
             ENDIF
            ENDDO
           ENDIF
          ENDDO
C
        ENDIF
        ENDDO
C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
       ELSEIF(ITY==3)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXC(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     4. ELEMENTS TIGES
C-----------------------
       ELSEIF(ITY==4)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXT(2,EP)
         NC(2)=IXT(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
       ELSEIF(ITY==5)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXP(2,EP)
         NC(2)=IXP(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
       ELSEIF(ITY==6)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXR(2,EP)
         NC(2)=IXR(3,EP)
         IGTYP = IGEO(11,IXR(1,EP))
         IF (IGTYP==12) THEN
          NNOD=3
          NC(3)=IXR(4,EP)
         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
       ELSEIF(ITY==7.AND.ICNOD/=6)THEN
        NNOD=3
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXTG(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
       ENDIF
C
       ENDIF
  100 CONTINUE
      RETURN
      END
Chd|====================================================================
Chd|  DIM_ELEMS2                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_ELEMS2(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW      ,
     4    INLOC     ,NNMAX     ,ICOK      ,NKMAX     ,ICOKM     ,
     5    INK       ,IGEO      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NNMAX,NKMAX,IGEO(NPROPGI,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),
     .    NROW(*),ICOK(NNMAX,*),ICOKM(NKMAX,*),INLOC(*),INK
C     REAL
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(20),NG,ITY,NEL,NFT,IAD,ISNOD,ICNOD,
     .        I,J,K,N,M,EP,L,IGTYP,NNOD,K1,IHBE,NK,EP1,IAD0
      my_real
     .   IOF
C----6---------------------------------------------------------------7---------8
      DO 100 NG=1,NGROUP
       IF (IPARG(8,NG)/=1) THEN
        ITY=IPARG(5,NG)
        NEL=IPARG(2,NG)
C----------void, rigid mat        
        IF (IPARG(1,NG) == 0 .OR. IPARG(1,NG) == 13) CYCLE
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ICNOD=IPARG(11,NG)
        ISNOD=IPARG(28,NG)
        IAD0 = IAD-1
C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
       IF (ITY==1) THEN
        NNOD=8
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         IF (ISNOD==4) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
         ELSEIF (ISNOD==6) THEN
          NNOD=6
          NC(1)=IXS(2,EP)
          NC(2)=IXS(3,EP)
          NC(3)=IXS(4,EP)
          NC(4)=IXS(6,EP)
          NC(5)=IXS(7,EP)
          NC(6)=IXS(8,EP)
         ELSEIF (ISNOD==10) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
          EP1=EP-NUMELS8
          DO J=1,6
           IF (IXS10(J,EP1)>0) THEN
            NNOD = NNOD + 1
            NC(NNOD) = IXS10(J,EP1)
           ENDIF
          ENDDO
         ELSEIF (ISNOD==8) THEN
          NNOD=8
         DO J=1,NNOD
          NC(J)=IXS(J+1,EP)
         ENDDO

Czg add solid element 20
         ELSEIF (ISNOD==20) THEN
          NNOD=20
         DO J=1,8
          NC(J)=IXS(J+1,EP)
         ENDDO
         EP1=EP-(NUMELS8+NUMELS10)
         DO J=9,20
          NC(J)=IXS20(J-8,EP1)
         ENDDO

         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     2. ELEMENTS 2D
C-----------------------
       ELSEIF(ITY==2)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
C
         EP=I+NFT
C        IF (ISNOD==4) THEN
C         NNOD=4
          DO J=1,NNOD
           NC(J)=IXQ(J+1,EP)
          ENDDO
C        ELSE
C         NNOD=0
C        ENDIF
C
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
C
        ENDIF
        ENDDO
C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
       ELSEIF(ITY==3)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXC(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     4. ELEMENTS TIGES
C-----------------------
       ELSEIF(ITY==4)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXT(2,EP)
         NC(2)=IXT(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
       ELSEIF(ITY==5)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXP(2,EP)
         NC(2)=IXP(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
       ELSEIF(ITY==6)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXR(2,EP)
         NC(2)=IXR(3,EP)
         IGTYP = IGEO(11,IXR(1,EP))
         IF (IGTYP==12) THEN
          NNOD=3
          NC(3)=IXR(4,EP)
         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
       ELSEIF(ITY==7.AND.ICNOD/=6)THEN
        NNOD=3
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXTG(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
       ENDIF
C
       ENDIF
  100 CONTINUE
      RETURN
      END
C------version \\-----
Chd|====================================================================
Chd|  DIM_ELEMS4                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_ELEMS4(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW      ,
     4    INLOC     ,NNMAX     ,ICOK      ,NKMAX     ,ICOKM     ,
     5    INK       ,IGEO      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NNMAX,NKMAX,IGEO(NPROPGI,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),
     .    NROW(*),ICOK(NNMAX,*),ICOKM(NKMAX,*),INLOC(*),INK
C     REAL
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(20),NG,ITY,NEL,NFT,IAD,ISNOD,ICNOD,
     .        I,J,K,N,M,EP,L,IGTYP,NNOD,K1,IHBE,NK,NK1,EP1,IAD0
      my_real
     .   IOF
C--------NROW(NUMNOD) : number of connnected nodes (non sym)
C----6---------------------------------------------------------------7---------8
      DO 100 NG=1,NGROUP
       IF (IPARG(8,NG)/=1) THEN
        ITY=IPARG(5,NG)
        NEL=IPARG(2,NG)
C----------void, rigid mat        
        IF (IPARG(1,NG) == 0 .OR. IPARG(1,NG) == 13) CYCLE
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ICNOD=IPARG(11,NG)
        ISNOD=IPARG(28,NG)
        IAD0 = IAD-1
C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
       IF (ITY==1) THEN
        NNOD=8
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         IF (ISNOD==4) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
         ELSEIF (ISNOD==6) THEN
          NNOD=6
          NC(1)=IXS(2,EP)
          NC(2)=IXS(3,EP)
          NC(3)=IXS(4,EP)
          NC(4)=IXS(6,EP)
          NC(5)=IXS(7,EP)
          NC(6)=IXS(8,EP)
         ELSEIF (ISNOD==10) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
          EP1=EP-NUMELS8
          DO J=1,6
           IF (IXS10(J,EP1)>0) THEN
            NNOD = NNOD + 1
            NC(NNOD) = IXS10(J,EP1)
           ENDIF
          ENDDO
         ELSEIF (ISNOD==8) THEN
          NNOD=8
         DO J=1,NNOD
          NC(J)=IXS(J+1,EP)
         ENDDO

Czg add solid element 20
         ELSEIF (ISNOD==20) THEN
          NNOD=20
         DO J=1,8
          NC(J)=IXS(J+1,EP)
         ENDDO
         EP1=EP-(NUMELS8+NUMELS10)
         DO J=9,20
          NC(J)=IXS20(J-8,EP1)
         ENDDO

         ELSE
          NNOD=0
         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
            NK1=NK-INK
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK1),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     2. ELEMENTS 2D
C-----------------------
       ELSEIF(ITY==2)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
C
          EP=I+NFT
C          IF (ISNOD==4) THEN
C           NNOD=4
            DO J=1,NNOD
              NC(J)=IXQ(J+1,EP)
            ENDDO
C         ELSE
C           NNOD=0
C         ENDIF
C
          DO J=1,NNOD
            N=NC(J)
            NK=INLOC(N)
            IF (NK>INK) THEN
              NK1=NK-INK
              DO L=1,NNOD
              IF (N/=NC(L)) THEN
                CALL REORDER_A(NROW(NK),ICOKM(1,NK1),NC(L))
              ENDIF
              ENDDO
            ELSEIF (NK>0) THEN
              DO L=1,NNOD
              IF (N/=NC(L)) THEN
                CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
              ENDIF
              ENDDO
            ENDIF
          ENDDO
C
        ENDIF
        ENDDO
C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
       ELSEIF(ITY==3)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXC(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
            NK1=NK-INK
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK1),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     4. ELEMENTS TIGES
C-----------------------
       ELSEIF(ITY==4)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXT(2,EP)
         NC(2)=IXT(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
            NK1=NK-INK
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK1),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
       ELSEIF(ITY==5)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXP(2,EP)
         NC(2)=IXP(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
            NK1=NK-INK
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK1),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
       ELSEIF(ITY==6)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXR(2,EP)
         NC(2)=IXR(3,EP)
         IGTYP = IGEO(11,IXR(1,EP))
         IF (IGTYP==12) THEN
          NNOD=3
          NC(3)=IXR(4,EP)
         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
            NK1=NK-INK
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK1),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
       ELSEIF(ITY==7.AND.ICNOD/=6)THEN
        NNOD=3
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXTG(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>INK) THEN
            NK1=NK-INK
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOKM(1,NK1),NC(L))
            ENDIF
           ENDDO
          ELSEIF (NK>0) THEN
           DO L=1,NNOD
            IF (N/=NC(L)) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),NC(L))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
       ENDIF
C
       ENDIF
  100 CONTINUE
      RETURN
      END
C------version \\-----
Chd|====================================================================
Chd|  DIM_ELEMSP                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        IND_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_ELEMSP(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW      ,
     4    INLOC     ,NKMAX     ,ICOK      ,IGEO      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NKMAX,IGEO(NPROPGI,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),NROW(*),ICOK(NKMAX,*),INLOC(*)
C     REAL
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(20),NG,ITY,NEL,NFT,IAD,ISNOD,ICNOD,
     .        I,J,K,N,M,EP,L,IGTYP,NNOD,K1,IHBE,NK,NJ,EP1,IAD0
      my_real
     .  IOF
C--------NROW(NUMNOD) : number of connnected nodes (non sym)
C----6---------------------------------------------------------------7---------8
      DO 100 NG=1,NGROUP
       IF (IPARG(8,NG)/=1) THEN
        ITY=IPARG(5,NG)
        NEL=IPARG(2,NG)
C----------void, rigid mat        
        IF (IPARG(1,NG) == 0 .OR. IPARG(1,NG) == 13) CYCLE
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ICNOD=IPARG(11,NG)
        ISNOD=IPARG(28,NG)
        IAD0 = IAD-1
C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
       IF (ITY==1) THEN
        NNOD=8
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         IF (ISNOD==4) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
         ELSEIF (ISNOD==6) THEN
          NNOD=6
          NC(1)=IXS(2,EP)
          NC(2)=IXS(3,EP)
          NC(3)=IXS(4,EP)
          NC(4)=IXS(6,EP)
          NC(5)=IXS(7,EP)
          NC(6)=IXS(8,EP)
         ELSEIF (ISNOD==10) THEN
          NNOD=4
          NC(1)=IXS(2,EP)
          NC(2)=IXS(4,EP)
          NC(3)=IXS(7,EP)
          NC(4)=IXS(6,EP)
          EP1=EP-NUMELS8
          DO J=1,6
           IF (IXS10(J,EP1)>0) THEN
            NNOD = NNOD + 1
            NC(NNOD) = IXS10(J,EP1)
           ENDIF
          ENDDO
         ELSEIF (ISNOD==8) THEN
          NNOD=8
         DO J=1,NNOD
          NC(J)=IXS(J+1,EP)
         ENDDO

Czg add solid element 20
         ELSEIF (ISNOD==20) THEN
          NNOD=20
         DO J=1,8
          NC(J)=IXS(J+1,EP)
         ENDDO
         EP1=EP-(NUMELS8+NUMELS10)
         DO J=9,20
          NC(J)=IXS20(J-8,EP1)
         ENDDO

         ELSE
          NNOD=0
         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            NJ=NC(L)
            IF (N/=NJ.AND.INLOC(NJ)>0) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),INLOC(NJ))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     2. ELEMENTS 2D
C-----------------------
       ELSEIF(ITY==2)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
C
         EP=I+NFT
C        IF (ISNOD==4) THEN
C         NNOD=4
          DO J=1,NNOD
           NC(J)=IXQ(J+1,EP)
          ENDDO
C        ELSE
C         NNOD=0
C        ENDIF
C
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            NJ=NC(L)
            IF (N/=NJ.AND.INLOC(NJ)>0) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),INLOC(NJ))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
C
        ENDIF
        ENDDO
C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
       ELSEIF(ITY==3)THEN
        NNOD=4
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXC(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            NJ=NC(L)
            IF (N/=NJ.AND.INLOC(NJ)>0) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),INLOC(NJ))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     4. ELEMENTS TIGES
C-----------------------
       ELSEIF(ITY==4)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXT(2,EP)
         NC(2)=IXT(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            NJ=NC(L)
            IF (N/=NJ.AND.INLOC(NJ)>0) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),INLOC(NJ))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
       ELSEIF(ITY==5)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXP(2,EP)
         NC(2)=IXP(3,EP)
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            NJ=NC(L)
            IF (N/=NJ.AND.INLOC(NJ)>0) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),INLOC(NJ))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
       ELSEIF(ITY==6)THEN
        NNOD=2
        DO I=1,NEL
        IOF=ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         NC(1)=IXR(2,EP)
         NC(2)=IXR(3,EP)
         IGTYP = IGEO(11,IXR(1,EP))
         IF (IGTYP==12) THEN
          NNOD=3
          NC(3)=IXR(4,EP)
         ENDIF
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            NJ=NC(L)
            IF (N/=NJ.AND.INLOC(NJ)>0) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),INLOC(NJ))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
       ELSEIF(ITY==7.AND.ICNOD/=6)THEN
        NNOD=3
        DO I=1,NEL
        IOF = ELBUF_TAB(NG)%GBUF%OFF(I)
        IF(IOF>ZERO)THEN
         EP=I+NFT
         DO J=1,NNOD
          NC(J)=IXTG(J+1,EP)
         ENDDO
         DO J=1,NNOD
          N=NC(J)
          NK=INLOC(N)
          IF (NK>0) THEN
           DO L=1,NNOD
            NJ=NC(L)
            IF (N/=NJ.AND.INLOC(NJ)>0) THEN
             CALL REORDER_A(NROW(NK),ICOK(1,NK),INLOC(NJ))
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        ENDDO
       ENDIF
C
       ENDIF
  100 CONTINUE
      RETURN
      END
Chd|====================================================================
Chd|  DIM_ELEMAX                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        DIM_ELEMS3                    source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_ELEMAX(
     1    IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     2    IXR       ,IXTG      ,IXTG1     ,IXS10     ,IXS20     ,
     3    IXS16     ,IXUR      ,IPARG     ,ELBUF     ,NDOF      ,
     4    NROW      ,INLOC     ,NNMAX     ,L_MAX     ,C_MAX     ,
     5    IGEO      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
      INTEGER IPARG(NPARG,*),IGEO(NPROPGI,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),NDOF(*),NROW(*),INLOC(*),
     .   NNMAX,L_MAX,C_MAX
C     REAL
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICOL(L_MAX,C_MAX),I,J,K,N,NFT,JLT,NK
C-----------------------------------------------
      NNMAX=0
      DO N =1,NUMNOD
       NROW(N)=0
       INLOC(N)=0
      ENDDO
      DO NFT = 0 , NUMNOD-1 ,C_MAX
       JLT = MIN( C_MAX, NUMNOD - NFT )
       DO NK=1,JLT
        N=NK+NFT
        INLOC(N)=NK
       ENDDO
       CALL DIM_ELEMS3(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW(NFT+1) ,
     4    INLOC     ,L_MAX     ,ICOL      ,IGEO      ,ELBUF_TAB )
       DO NK=1,JLT
        N=NK+NFT
        INLOC(N)=0
        NNMAX=MAX(NNMAX,NROW(N))
       ENDDO
      ENDDO	
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KINE_P                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINE_P(
     1    IGEO      ,NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     2    SENSOR    ,IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,
     3    NDOF      ,NSI2      ,NSRB      ,NKINE     ,
     7    INLOC     ,IRBE3     ,IRBE2     ,LRBE2     ,NKINM     ,
     8    INTBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
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
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),NSI2,NSRB,
     .        NDOF(*),NKINE,INLOC(*),IGEO(*),IRBE3(NRBE3L,*),
     .        IRBE2(NRBE2L,*),LRBE2(*),NKINM
C     REAL
      my_real
     .   SENSOR(NSENSR,*)

      TYPE (INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN,I,J,K,N,M,NS
C-----------------------------------------------
      NKINE=0
C----- main of rigid body first------
      DO I=1,NRBYAC
       N=IRBYAC(I)
       M=NPBY(1,N)
       IF (INLOC(M)==0) THEN
        NKINE=NKINE+1
        INLOC(M)=-NKINE
       ENDIF
      ENDDO
C----- rbe2 main------
      DO I=1,NRBE2
       K = IRBE2(1,I)
       M = IRBE2(3,I)
       IF (INLOC(M)==0) THEN
         NKINE=NKINE+1
         INLOC(M)=NKINE
       ENDIF
      ENDDO
C-----will be stored in ICOKM(NKMAX,*)	  
      NKINM=NKINE
C-----pour IND_GLOB_K, passer NKINM par include ou module, modifier dans IND_KINE_	  
C      K=0
C------interface 2-------------- 
      DO I=1,NINT2
       N=IINT2(I)
       NSN = IPARI(5,N)
       DO J=1,NSN
        NS=INTBUF_TAB(N)%NSV(J)
        IF (INLOC(NS)==0.AND.NDOF(NS)>0) THEN
         NKINE=NKINE+1
         INLOC(NS)=NKINE
        ENDIF
       ENDDO
      ENDDO
C----- rbe3 ------
      DO I=1,NRBE3
       NS=IRBE3(3,I)
       IF (NS==0) CYCLE
       IF (INLOC(NS)==0.AND.NDOF(NS)>0) THEN
         NKINE=NKINE+1
         INLOC(NS)=NKINE
       ENDIF
      ENDDO
C----- rbe2 ------
      DO I=1,NRBE2
       K = IRBE2(1,I)
       M = IRBE2(3,I)
       NSN = IRBE2(5,I)
       DO J=1,NSN
        NS=LRBE2(K+J)
        IF (INLOC(NS)==0.AND.NDOF(NS)>0) THEN
         NKINE=NKINE+1
         INLOC(NS)=NKINE
        ENDIF
       ENDDO
      ENDDO
C----- rigid body ------
      DO I=1,NRBYAC
       N=IRBYAC(I)
       K=IRBYAC(I+NRBYKIN)
       NSN  =NPBY(2,N)
       DO J=1,NSN
        NS=LPBY(K+J)
        IF (INLOC(NS)==0.AND.NDOF(NS)>0) THEN
         NKINE=NKINE+1
         INLOC(NS)=NKINE
        ENDIF
       ENDDO
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_NDOF_I                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        I2_PREM                       source/interfaces/interf/i2_prem.F
Chd|        RBYAC_IMP                     source/constraints/general/rbody/rbyac_imp.F
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_NDOF_I(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,SENSOR    ,
     2    IRBYAC    ,NDOF      ,NSRB      ,IPARI     ,
     3    NINT2     ,IINT2     ,NSI2      ,NPRW      ,IRBE3     ,
     4    IRBE2     ,NSRB2     ,FR_ELEM   ,IAD_ELEM  ,INTBUF_TAB)
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
#include      "com09_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),NSI2
      INTEGER NSRB,NDOF(*),NPRW(*),IRBE3(NRBE3L,*),IRBE2(NRBE2L,*),
     .        NSRB2,FR_ELEM(*),IAD_ELEM(2,*)
C     REAL
      my_real
     .   SENSOR(NSENSR,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NMN,JI,K10,K11,K12,K13,K14,J,K,NDOFI,NSN
      INTEGER I,N,M,IC,ICT,ICR,INS(NRBE3),NP,NIN,NTY,
     .        KD(50),KFI,JIN
C-----------------------------------------------
      NSI2=0
      NINT2=0
      DO K=0,NHIN2
       CALL I2_PREM(IPARI,K,NINT2,IINT2,NSI2)
      ENDDO
C------rigid body main------------- 
      CALL RBYAC_IMP(NPBY,ITAB,SENSOR,NRBYAC,IRBYAC,NSRB)
      DO I=1,NRBYAC
       N=IRBYAC(I)
       M=NPBY(1,N)
       NDOF(M)=6
      ENDDO
C      
      NDOFI=3
      IF (IRODDL>0) NDOFI=6
      DO I=1,NRWALL
       N = I + 2*NRWALL
       M = NPRW(N)
       IF (M>0) NDOF(M)=NDOFI
      ENDDO
C--------temporaly to avoid issue w/ contact +spmd
      IF (NSPMD > 1 .AND.NINTER > 0) THEN
       DO I=1,NRBE3
        N=IRBE3(3,I)
        INS(I)=0
         DO NIN=1,NINTER
           NSN   =IPARI(5,NIN)
           NTY   =IPARI(7,NIN)
           IF (ISPMD/=0.AND.(NTY<7.OR.NTY==8
     .                     .OR.NTY==14.OR.NTY==15)) CYCLE
              IF(NTY==5.OR.NTY==7.OR.NTY==10.OR.NTY==11
     .           .OR.NTY==24) THEN
C
                IF (INTAB(NSN,INTBUF_TAB(NIN)%NSV(1),N)) INS(I)=1
              ENDIF
          ENDDO
       ENDDO !
       DO I=1,NRBE3
        N  = IRBE3(3,I)
        IF (N==0.OR.INS(I)==0) CYCLE
        IC = IRBE3(4,I)
        ICT=IC/512
        ICR=(IC-512*ICT)/64
        IF (ICR>0) THEN
         NDOF(N) = 6
        ELSE
         NDOF(N) = 3
        ENDIF
       ENDDO
      END IF !(NSPMD > 1 .AND.NINTER > 0) THEN
C-----if m is secnd of rb
      NSRB2=0
      DO N=1,NRBE2
       M=IRBE2(3,N)
       NSN =IRBE2(5,N)
C--------case NSN=1 is treated in DIM_NDOF_II       
       IF(NDOF(M)==0.AND.NSN >1) NDOF(M)=NDOFI
       NSRB2= NSRB2+NSN
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_NDOF_II                   source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_NDOF_II(
     1    NINT2     ,IINT2     ,IPARI     ,NDOF      ,
     2    NRBE3     ,IRBE3     ,LRBE3     ,NRBE2     ,IRBE2     ,
     3    LRBE2     ,INTBUF_TAB)
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
#include      "com09_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),NDOF(*),
     .        NRBE3 ,IRBE3(NRBE3L,*),LRBE3(*),NRBE2,IRBE2(NRBE2L,*),
     .        LRBE2(*)
C     REAL

      TYPE (INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NMN,J,K
      INTEGER I,N,M,L,NS,ID,NM,NSN,IAD,ILEV,NDOFM,IROT,IC
C-----------------------------------------------
C------interface 2---au cas surface main est deactive----------- 
      DO I=1,NINT2
       N=IINT2(I)
       NSN = IPARI(5,N)
       NMN = IPARI(6,N)
       ILEV = IPARI(20,N)
C------IRTL(NSN)--main el number---
       NDOFM=3
       IF (ILEV == 0) NDOFM=6
       DO J=1,NSN
        NS=INTBUF_TAB(N)%NSV(J)
        IF (NDOF(NS)>0) THEN
         L=INTBUF_TAB(N)%IRTLM(J)
         ID=4*(L-1)
         DO M=1,4
          NM=INTBUF_TAB(N)%IRECTM(ID+M)
          IF (NDOF(NM)<=0) NDOF(NM)=MIN(NDOF(NM),-NDOFM)
         ENDDO
        ENDIF
       ENDDO
      ENDDO
C
      DO I=1,NINT2
       N=IINT2(I)
       NSN = IPARI(5,N)
       NMN = IPARI(6,N)
C------IRTL(NSN)--main el number---
       DO J=1,NSN
        NS=INTBUF_TAB(N)%NSV(J)
        IF (NDOF(NS)>0) THEN
         L=INTBUF_TAB(N)%IRTLM(J)
         ID=4*(L-1)
         DO M=1,4
          NM=INTBUF_TAB(N)%IRECTM(ID+M)
          IF (NDOF(NM)<0) NDOF(NM)=-NDOF(NM)
         ENDDO
        ENDIF
       ENDDO
      ENDDO
C      
      DO I=1,NRBE3
       IAD=IRBE3(1,I)
       NS =IRBE3(3,I)
       IROT =IRBE3(6,I)
       IF (NS==0.OR.NDOF(NS)==0) CYCLE
       NDOFM=3
       IF (IROT > 0) NDOFM=6
       NMN=IRBE3(5,I)
       DO J=1,NMN
         NM=LRBE3(IAD+J)
         IF (NDOF(NM)<=0) NDOF(NM)=MIN(NDOF(NM),-NDOFM)
       ENDDO
      ENDDO
C      
      DO I=1,NRBE3
       IAD=IRBE3(1,I)
       NS =IRBE3(3,I)
       IF (NS==0.OR.NDOF(NS)==0) CYCLE
       NMN=IRBE3(5,I)
       DO J=1,NMN
         NM=LRBE3(IAD+J)
         IF (NDOF(NM)<=0) NDOF(NM)=-NDOF(NM)
       ENDDO
      ENDDO
C      
      DO I=1,NRBE2
       M=IRBE2(3,I)
       NSN =IRBE2(5,I)
C--------case NSN=1 -------------       
       IF(NSN==1) THEN
        IAD=IRBE2(1,I)
        NS=LRBE2(IAD+1)
        IC = IRBE2(4,I)/512
        IF (NDOF(NS)<=3) IRBE2(4,I)=IC*512
        NDOF(M)=MAX(NDOF(M),NDOF(NS))
       END IF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_NDOF_D                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        SPMD_I2D                      source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE DIM_NDOF_D(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NDOF      ,
     2    IAD_RBY   ,FR_RBY    )
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),LPBY(*),NRBYAC,IRBYAC(*)
      INTEGER NDOF(*),IAD_RBY(*),FR_RBY(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,M,NSN,NS,IE,NN
C-----------------------------------------------
      IF (NRBYAC==0) RETURN
      DO I=1,NRBYAC
       N=IRBYAC(I)
       K=IRBYAC(I+NRBYKIN)
       M=NPBY(1,N)
       NSN  =NPBY(2,N)
       IE = 0
       DO J=1,NSN
        NS=LPBY(K+J)
        IF (NDOF(NS)>0) IE = 1
       ENDDO
       IF (IE==0) THEN
        NDOF(M) = 0
       ENDIF
      ENDDO
      IF (IMACH==3.AND.NSPMD>1) THEN
       NN=IAD_RBY(NSPMD+1)-IAD_RBY(1)
       IF (NN>0) CALL SPMD_I2D(NDOF,FR_RBY,IAD_RBY,NN)
      ENDIF
C-------actualise NRBYAC,IRBYAC-------
      IE = 0
      DO I=1,NRBYAC
       N=IRBYAC(I)
       M=NPBY(1,N)
       IF (NDOF(M)>0) THEN
        IE = IE + 1
        IRBYAC(IE) = IRBYAC(I)
        IRBYAC(IE+NRBYKIN) = IRBYAC(I+NRBYKIN)
       ENDIF
      ENDDO
      NRBYAC = IE
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KINE_S                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINE_S(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOF      ,
     3    NNMAX     ,NROW      ,NROWI     ,NKINE     ,INLOC     ,
     4    ICOK      ,IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     )
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
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),
     .        NDOF(*),NKINE,INLOC(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        IRBE2(NRBE2L,*),LRBE2(*)
      INTEGER NROW(*),NROWI(*),ICOK(NNMAX,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NKS,NKM
      INTEGER NSN,NMN,I,J,K,N,M,EP,L,NI,NJ,K12,ID,NS,NK,NM,NNOD,N1,L1,NL1,NL,J1,NM1,
     .        JI1,L10,L11,L12,L13,L14,NNOD1,I1,NSN1,M1,NKE,IAD,IC
C     NROW(NUMNOD) :nombre of connected nodes (non sym)
C     NROWI(NKINE) :INDICE LOCALE <=NNMAX (NROW peut eventuellement depasse NNMAX)
C     INLOC(NUMNOD) :indice locale des "kinematic nodes"
C-----------------------------------------------
C------interface 2-------------- 
      DO I=1,NINT2
       N=IINT2(I)
       NSN = IPARI(5,N)
       NMN = IPARI(6,N)
       DO 20 J=1,NSN
        NS=INTBUF_TAB(N)%NSV(J)
        IF (NDOF(NS)>0) THEN
         L=INTBUF_TAB(N)%IRTLM(J)
         ID=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(ID+3)==INTBUF_TAB(N)%IRECTM(ID+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         DO M=1,NNOD
          NM=INTBUF_TAB(N)%IRECTM(ID+M)
          IF (NDOF(NM)>0)NROW(NM)=NROW(NM)+NNOD-1
         ENDDO
         NKS=INLOC(NS)
         DO NK=1,NROWI(NKS)
          NJ=ICOK(NK,NKS)
          IF (.NOT.INTAB(NSN,INTBUF_TAB(N)%NSV(1) ,NJ)) THEN
           IF (INLOC(NJ)==0.AND.NDOF(NJ)>0) THEN
            NKINE=NKINE+1
            INLOC(NJ)=NKINE
           ENDIF
           DO M=1,NNOD
            NM=INTBUF_TAB(N)%IRECTM(ID+M)
            NROW(NM)=NROW(NM)+1
            NROW(NJ)=NROW(NJ)+1
           ENDDO
          ENDIF
         ENDDO
C-----with Kij block-(i,j have the same M)-----
         DO N1=J+1,NSN
          NJ=INTBUF_TAB(N)%NSV(N1)
          L1=INTBUF_TAB(N)%IRTLM(N1)
          IF (NDOF(NJ)>0.AND.L/=L1) THEN
           IF (INTAB(NROWI(NKS),ICOK(1,NKS),NJ)) THEN
            NL1=4*(L1-1)
            DO M=1,NNOD
             NM=INTBUF_TAB(N)%IRECTM(ID+M)
             DO J1=1,4
              NM1=INTBUF_TAB(N)%IRECTM(NL1+J1)
              IF (NM/=NM1) THEN
               NROW(NM)=NROW(NM)+1
               NROW(NM1)=NROW(NM1)+1
              ENDIF
             ENDDO
            ENDDO
           ENDIF
          ENDIF
         ENDDO
        ENDIF
 20    CONTINUE
       DO J=1,NMN
        NM=INTBUF_TAB(N)%MSR(J)
        IF (INLOC(NM)==0.AND.NDOF(NM)>0) THEN
         NKINE=NKINE+1
         INLOC(NM)=NKINE
        ENDIF
       ENDDO
      ENDDO
C+++couplage entre int2----
      IF (NINT2>1) THEN
       DO J=1,NINT2
        N=IINT2(J)
        NSN = IPARI(5,N)
        DO J1=J+1,NINT2
         N1=IINT2(J1)
         NSN1 = IPARI(5,N1)
         JI1=IPARI(1,N1)
         L10=JI1-1
         L11=L10+4*IPARI(3,N1)
         L12=L11+4*IPARI(4,N1)
         L13=L12+NSN1
         L14=L13+IPARI(6,N1)
         DO I=1,NSN
          NI=INTBUF_TAB(N)%NSV(I)
          IF (NDOF(NI)>0) THEN
           NKE=INLOC(NI)
C------cherche-secnd pairs-entre int2---
           DO I1=1,NSN1
            NJ=INTBUF_TAB(N1)%NSV(I1)
            IF (NDOF(NJ)>0.AND.
     .          INTAB(NROWI(NKE),ICOK(1,NKE),NJ)) THEN
             L=INTBUF_TAB(N)%IRTLM(I)
             NL=4*(L-1)
             IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
              NNOD=3
             ELSE
              NNOD=4
             ENDIF
             L1=INTBUF_TAB(N1)%IRTLM(I1)
             NL1=4*(L1-1)
             IF (INTBUF_TAB(N1)%IRECTM(NL1+3)==INTBUF_TAB(N1)%IRECTM(NL1+4)) THEN
              NNOD1=3
             ELSE
              NNOD1=4
             ENDIF
             DO M=1,NNOD
              NM=INTBUF_TAB(N)%IRECTM(NL+M)
              DO M1=1,NNOD1
               NM1=INTBUF_TAB(N1)%IRECTM(NL1+M1)
               IF (NDOF(NM)>0.AND.NDOF(NM1)>0) THEN
                NROW(NM)=NROW(NM)+1
                NROW(NM1)=NROW(NM1)+1
               ENDIF
              ENDDO
             ENDDO
C----------endif NDOF(NJ)>0---- 
            ENDIF
C----------enddo DO I1=1,NSN1---- 
           ENDDO
C----------endif NDOF(NI)>0---- 
          ENDIF
C----------enddo DO I=1,NSN---- 
         ENDDO
C----------enddo DO J1=---- 
        ENDDO
       ENDDO
      ENDIF
C----- Rbe2 ------
      DO I=1,NRBE2
       K=IRBE2(1,I)
       M=IRBE2(3,I)
       NSN =IRBE2(5,I)
       IC = 7*512+7*64-IRBE2(4,I)
       DO J=1,NSN
        NS=LRBE2(K+J)
        IF (NDOF(NS)>0) THEN
         NKS=INLOC(NS)
         DO NK=1,NROWI(NKS)
          NJ=ICOK(NK,NKS)
          IF (NDOF(NJ)>0) THEN
           NROW(M)=NROW(M)+1
           NROW(NJ)=NROW(NJ)+1+NHRBE2
           IF (INLOC(NJ)==0) THEN
            NKINE=NKINE+1
            INLOC(NJ)=NKINE
           ENDIF
          ENDIF
         ENDDO
C-----Due to hierarchy----         
         IF (NROW(NS)>NROWI(NKS)) NROW(M)=NROW(M)+ NROW(NS)-NROWI(NKS)
C+++++++Knsns -> Kmns      
          IF (IC>0) THEN
	       NROW(M)=NROW(M)+1
	       NROW(NS)=NROW(NS)+1
          ENDIF
         ENDIF
       ENDDO
C+++estimation de couplage ----
       NROW(M)=NROW(M)+1
      ENDDO
C------RBE3-------------- 
      DO I=1,NRBE3
       NS  = IRBE3(3,I)
       IF (NS==0.OR.NDOF(NS)==0) CYCLE
       IAD = IRBE3(1,I)
       NNOD = IRBE3(5,I)
        NKS=INLOC(NS)
         DO M=1,NNOD
          NM=LRBE3(IAD+M)
          IF (NDOF(NM)>0)NROW(NM)=NROW(NM)+NNOD-1
         ENDDO
         DO NK=1,NROWI(NKS)
          NJ=ICOK(NK,NKS)
           IF (INLOC(NJ)==0.AND.NDOF(NJ)>0) THEN
            NKINE=NKINE+1
            INLOC(NJ)=NKINE
           ENDIF
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
            NROW(NM)=NROW(NM)+1
            NROW(NJ)=NROW(NJ)+1
           ENDDO
         ENDDO
C-----Due to hierarchy----         
         IF (NROW(NS)>NROWI(NKS)) THEN
           K12 = NROW(NS)-NROWI(NKS)
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
C  ----          not sufficient
            NROW(NM)=NROW(NM)+ K12 + K12
           ENDDO
C-----------bug fixing Pi_215563,216361          
C           DO NK=1,NROWI(NKS)
C            NJ=ICOK(NK,NKS)
C            NROW(NJ)=NROW(NJ)+ K12
C           END DO
         END IF
C-----with Kij (i,j -> NSi,NSj-----
        DO I1=I+1,NRBE3
         NJ=IRBE3(3,I1)
         IF (NJ==0.OR.NDOF(NJ)==0) CYCLE
         IF (INTAB(NROWI(NKS),ICOK(1,NKS),NJ)) THEN
            M1 = IRBE3(1,I1)
            N1 = IRBE3(5,I1)
            DO M=1,NNOD
             NM=LRBE3(IAD+M)
             DO J1=1,N1
              NM1=LRBE3(M1+J1)
              IF (NM/=NM1) THEN
               NROW(NM)=NROW(NM)+1
               NROW(NM1)=NROW(NM1)+1
              ENDIF
             ENDDO
            ENDDO
         ENDIF
        ENDDO
C	
        DO M=1,NNOD
         NM=LRBE3(IAD+M)
         IF (INLOC(NM)==0.AND.NDOF(NM)>0) THEN
          NKINE=NKINE+1
          INLOC(NM)=NKINE
         ENDIF
        ENDDO
      ENDDO
C----- rigid body ------
      DO I=1,NRBYAC
       N=IRBYAC(I)
       K=IRBYAC(I+NRBYKIN)
       M=NPBY(1,N)
       IF (INLOC(M)<0) INLOC(M)=-INLOC(M)
       NSN  =NPBY(2,N)
       IF (NDOF(M)>0) THEN
       DO J=1,NSN
        NS=LPBY(K+J)
        IF (NDOF(NS)>0) THEN
         NKS=INLOC(NS)
         DO NK=1,NROWI(NKS)
          NJ=ICOK(NK,NKS)
          IF (NDOF(NJ)>0) THEN
          IF (.NOT.INTAB(NSN,LPBY(K+1),NJ)) THEN
           NROW(M)=NROW(M)+1
           NROW(NJ)=NROW(NJ)+1
           IF (INLOC(NJ)==0) THEN
            NKINE=NKINE+1
            INLOC(NJ)=NKINE
           ENDIF
          ENDIF
          ENDIF
         ENDDO
         IF (NROW(NS)>NROWI(NKS)) NROW(M)=NROW(M)+ NROW(NS)-NROWI(NKS)
         END IF !IF (NDOF(NS)>0)
       ENDDO
       ENDIF
C+++estimation de couplage ----
       NROW(M)=NROW(M)+NSN
      ENDDO
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KINE_T                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINE_T(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOF      ,
     3    NNMAX     ,NROWI     ,NKINE     ,INLOC     ,ICOK      ,
     4    NSS       ,NSIJ      ,NMIJ      ,NSS2      ,NSIJ2     ,
     5    NMIJ2     ,NKMAX     ,ICOKM     ,INK       ,IRBE3     ,
     6    LRBE3     ,NSS3      ,IRBE2     ,LRBE2     ,NSB2      )
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),
     .        NDOF(*),NKINE,INLOC(*),NMIJ2,NSS3
      INTEGER NROWI(*),ICOK(NNMAX,*),NSS ,NSIJ ,NMIJ,NSS2 ,NSIJ2
      INTEGER NKMAX,ICOKM(NKMAX,*),INK,IRBE3(NRBE3L,*),LRBE3(*),
     .        IRBE2(NRBE2L,*),LRBE2(*),NSB2

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NKS,NKM,NKE1,NKE2,NKM1,NKE
      INTEGER NSN,NMN,I,J,K,N,M,EP,L,NI,NJ,JI,K10,K11,K12,K13,
     .        K14,KFI,ID,NS,NK,NM,NNOD,N1,L1,NL1,NL,J1,NM1,N2,
     .        JI1,L10,L11,L12,L13,L14,NNOD1,I1,NSN1,M1,IAD,IC
C     NROWI(NKINE) :INDICE LOCALE <=NNMAX 
C     INLOC(NUMNOD) :indice locale des "kinematic nodes"
C-----------------------------------------------
C------interface 2-------------- 
      NSS2=0
      NSIJ2=0
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
       KFI=K14+NSN
       DO 20 J=1,NSN
        NS=INTBUF_TAB(N)%NSV(J)
        IF (NDOF(NS)>0) THEN
         L=INTBUF_TAB(N)%IRTLM(J)
         ID=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(ID+3)==INTBUF_TAB(N)%IRECTM(ID+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         DO M=1,NNOD
          NM=INTBUF_TAB(N)%IRECTM(ID+M)
          IF (NDOF(NM)>0) THEN
           NKE1=INLOC(NM)
           DO J1=1,NNOD
            NM1=INTBUF_TAB(N)%IRECTM(ID+J1)
            IF (NM/=NM1) CALL REORDER_A(NROWI(NKE1),ICOK(1,NKE1),NM1)
           ENDDO
          ENDIF
         ENDDO
         NKS=INLOC(NS)
         DO NK=1,NROWI(NKS)
          NJ=ICOK(NK,NKS)
          IF (NDOF(NJ)>0.AND.
     .        (.NOT.INTAB(NSN,INTBUF_TAB(N)%NSV(1),NJ))) THEN
           NSS2=NSS2+1
           NKE2=INLOC(NJ)
           DO M=1,NNOD
             NM=INTBUF_TAB(N)%IRECTM(ID+M)
             IF (NDOF(NM)>0) THEN
              NKE1=INLOC(NM)
              CALL REORDER_A(NROWI(NKE1),ICOK(1,NKE1),NJ)
              CALL REORDER_A(NROWI(NKE2),ICOK(1,NKE2),NM)
             ENDIF
           ENDDO
          ENDIF
         ENDDO
C-----with Kij block-(i,j have the same M)-----
         DO N1=J+1,NSN
          NJ=INTBUF_TAB(N)%NSV(N1)
          L1=INTBUF_TAB(N)%IRTLM(N1)
           IF (NDOF(NJ)>0.AND.
     .         INTAB(NROWI(NKS),ICOK(1,NKS),NJ)) THEN
            NSIJ2=NSIJ2+2
            IF (L/=L1) THEN
             NL1=4*(L1-1)
             DO M=1,NNOD
              NM=INTBUF_TAB(N)%IRECTM(ID+M)
              IF (NDOF(NM)>0) THEN
              NKE1=INLOC(NM)
              DO J1=1,4
               NM1=INTBUF_TAB(N)%IRECTM(NL1+J1)
                IF (NM/=NM1.AND.NDOF(NM1)>0) THEN
                 NKE2=INLOC(NM1)
                 CALL REORDER_A(NROWI(NKE1),ICOK(1,NKE1),NM1)
                 CALL REORDER_A(NROWI(NKE2),ICOK(1,NKE2),NM)
                ENDIF
              ENDDO
              ENDIF
             ENDDO
            ENDIF
           ENDIF
         ENDDO
        ENDIF
 20    CONTINUE
      ENDDO
C------Rbe2------
      NSB2=0
      DO N=1,NRBE2
       K=IRBE2(1,N)
       M=IRBE2(3,N)
       NSN =IRBE2(5,N)
       NKE1=INLOC(M)
       IC = 7*512+7*64-IRBE2(4,N)
       DO J=1,NSN
        NS=LRBE2(K+J)
        IF (NDOF(NS)>0) THEN
         NKS=INLOC(NS)
         DO NK=1,NROWI(NKS)
          IF (NKS >INK) THEN
           NJ=ICOK(NK,NKS)
          ELSE
           NJ=ICOKM(NK,NKS)
          END IF
          NKE2=INLOC(NJ)
          IF (NDOF(NJ)>0.AND.NJ/=NS) THEN
            NSB2=NSB2+1
            CALL REORDER_A(NROWI(NKE1),ICOKM(1,NKE1),NJ)
            IF (NKE2>INK) THEN
               CALL REORDER_A(NROWI(NKE2),ICOK(1,NKE2),M)
            ELSEIF (NKE2>0) THEN
               CALL REORDER_A(NROWI(NKE2),ICOKM(1,NKE2),M)
            ENDIF
          ENDIF
         ENDDO
         IF (IC>0) THEN
	      CALL REORDER_A(NROWI(NKE1),ICOKM(1,NKE1),NS)
          IF (NKS > INK) THEN
           CALL REORDER_A(NROWI(NKS),ICOK(1,NKS),M)
          ELSEIF (NKS > 0) THEN		
           CALL REORDER_A(NROWI(NKS),ICOKM(1,NKS),M)
          END IF
	     ENDIF
        ENDIF
       ENDDO
      ENDDO
C------RBE3-------------- 
      NSS3=0
      DO I=1,NRBE3
       IAD=IRBE3(1,I)
       NS =IRBE3(3,I)
       IF (NS==0) CYCLE
       NNOD=IRBE3(5,I)
        IF (NDOF(NS)>0) THEN
C
         DO M=1,NNOD
          NM=LRBE3(IAD+M)
          IF (NDOF(NM)>0) THEN
           NKE1=INLOC(NM)
           DO J1=M+1,NNOD
            NM1=LRBE3(IAD+J1)
            IF (NKE1>INK.AND.NM/=NM1) THEN
             CALL REORDER_A(NROWI(NKE1),ICOK(1,NKE1),NM1)
	        ELSEIF (NKE1>0.AND.NM/=NM1) THEN
             CALL REORDER_A(NROWI(NKE1),ICOKM(1,NKE1),NM1)
	        ENDIF
            NKE2=INLOC(NM1)
            IF (NKE2>INK) THEN
             CALL REORDER_A(NROWI(NKE2),ICOK(1,NKE2),NM)
	        ELSEIF (NKE2>0) THEN
             CALL REORDER_A(NROWI(NKE2),ICOKM(1,NKE2),NM)
	        ENDIF
           ENDDO
          ENDIF
         ENDDO
         NKS=INLOC(NS)
         DO NK=1,NROWI(NKS)
C-----due to change of RBE2- (M used ICOKM now) --
          IF (NKS > INK) THEN
           NJ=ICOK(NK,NKS)
          ELSE
           NJ=ICOKM(NK,NKS)
          END IF
          IF (NDOF(NJ)>0 ) THEN
           NSS3=NSS3+1
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
             IF (NDOF(NM)>0) THEN
              NKE1=INLOC(NM)
	
              IF (NKE1>INK) THEN
               CALL REORDER_A(NROWI(NKE1),ICOK(1,NKE1),NJ)
	          ELSEIF (NKE1>0) THEN
               CALL REORDER_A(NROWI(NKE1),ICOKM(1,NKE1),NJ)
	          ENDIF
	
              IF (NKE2>INK) THEN
               CALL REORDER_A(NROWI(NKE2),ICOK(1,NKE2),NM)
              ELSEIF (NKE2>0) THEN
               CALL REORDER_A(NROWI(NKE2),ICOKM(1,NKE2),NM)
              ENDIF
	
             ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
      ENDDO
C+++couplage entre int2----
      NMIJ2=0
C
      NSS=0
      NSIJ=0
      DO I=1,NRBYAC
       N=IRBYAC(I)
       K=IRBYAC(I+NRBYKIN)
       M=NPBY(1,N)
       NSN  =NPBY(2,N)
       IF (NDOF(M)>0) THEN
        NKE1=INLOC(M)
       DO J=1,NSN
        NS=LPBY(K+J)
        IF (NDOF(NS)>0) THEN
         NKS=INLOC(NS)
         DO NK=1,NROWI(NKS)
          IF (NKS > INK) THEN
           NJ=ICOK(NK,NKS)
          ELSE
           NJ=ICOKM(NK,NKS)
          END IF
          NKE2=INLOC(NJ)
          IF (NDOF(NJ)>0.AND.
     .         (.NOT.INTAB(NSN,LPBY(K+1),NJ))) THEN
            NSS=NSS+1
           CALL REORDER_A(NROWI(NKE1),ICOKM(1,NKE1),NJ)
           IF (NKE2>INK) THEN
            CALL REORDER_A(NROWI(NKE2),ICOK(1,NKE2),M)
           ELSEIF (NKE2>0) THEN
            CALL REORDER_A(NROWI(NKE2),ICOKM(1,NKE2),M)
           ENDIF
          ENDIF
         ENDDO
C-----create rigid body secnd nodes with Kij block-(i,j have the same M)-----
         DO N1=J+1,NSN
           NJ=LPBY(K+N1)
          IF (NKS > INK) THEN
           IF (NDOF(NJ)>0.AND.
     .         (INTAB(NROWI(NKS),ICOK(1,NKS),NJ))) THEN
             NSIJ=NSIJ+2
           ENDIF
          ELSE
           IF (NDOF(NJ)>0.AND.
     .         (INTAB(NROWI(NKS),ICOKM(1,NKS),NJ))) THEN
             NSIJ=NSIJ+2
           ENDIF
          END IF
         ENDDO
        ENDIF
       ENDDO
       ENDIF
      ENDDO
C+++couplage entre rigid bodies----
      NMIJ=0
      IF (NRBYAC>1) THEN
       DO J=1,NRBYAC
        N=IRBYAC(J)
        K=IRBYAC(J+NRBYKIN)
        M  =NPBY(1,N)
        NS=NPBY(2,N)
        IF (NDOF(M)>0) THEN
         NKE1=INLOC(M)
        DO J1=J+1,NRBYAC
         N1=IRBYAC(J1)
         L1=IRBYAC(J1+NRBYKIN)
         NM =NPBY(1,N1)
         NSN =NPBY(2,N1)
         IF (INTAB(NROWI(NKE1),ICOKM(1,NKE1),NM)) THEN
           DO I=1,NSN
            ID = I+L1
            NI=LPBY(ID)
            IF (NDOF(NI)>0) THEN
             NKS=INLOC(NI)
             IF (NKS > INK) THEN
              IF (INTAB(NROWI(NKS),ICOK(1,NKS),M)) THEN
C------cherche-secnd pairs----
              DO N1=1,NS
               N2=LPBY(K+N1)
               IF (NDOF(N2)>0.AND.
     .             INTAB(NROWI(NKS),ICOK(1,NKS),N2)) THEN
                NMIJ=NMIJ+2
               ENDIF
              ENDDO
              ENDIF
             ELSE
              IF (INTAB(NROWI(NKS),ICOKM(1,NKS),M)) THEN
C------cherche-secnd pairs----
              DO N1=1,NS
               N2=LPBY(K+N1)
               IF (NDOF(N2)>0.AND.
     .             INTAB(NROWI(NKS),ICOKM(1,NKS),N2)) THEN
                NMIJ=NMIJ+2
               ENDIF
              ENDDO
              ENDIF
             END IF
            ENDIF
           ENDDO
         ENDIF
        ENDDO
        ENDIF
       ENDDO
      ENDIF
      NSIJ=NSIJ+NMIJ
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IND_KINE_K                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        RMIND_IMP                     source/model/remesh/rm_imp0.F 
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_KINE_K(NPBY,LPBY,
     1    ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,ISIJ      ,
     2    NMC       ,IMIJ      ,NSS       ,ISS       ,NINT2     ,
     3    IINT2     ,IPARI     ,INTBUF_TAB,NSC2      ,ISIJ2     ,
     4    NSS2      ,ISS2      ,NDOF      ,NNMAX     ,NKINE     ,
     5    INLOC     ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     6    NMC2      ,IMIJ2     ,INK       ,IRBE3     ,LRBE3     ,
     7    ISS3      ,IRBE2     ,LRBE2     ,ISB2      ,NSRB2     )
C----6---------------------------------------------------------------7---------8
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NSC(*),ISIJ(*),NSS(*),ISS(*),NINT2,IINT2(*),
     .        NSC2(*),ISIJ2(*),NSS2(*),ISS2(*),IPARI(NPARI,*),
     .        NMC,IMIJ(*),ICOK(NNMAX,*),ICOKM(NKMAX,*),NROWK(*),
     .        NMC2,IMIJ2(*),IRBE2(NRBE2L,*),LRBE2(*),ISB2(*),NSRB2(*)
      INTEGER
     .   NDOF(*),NKINE,INLOC(*),INK,IRBE3(NRBE3L,*),LRBE3(*),ISS3(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C------ICOK,ICOKM use the same NROWK------
      INTEGER NKE,NKE1,NKE2,IK,NKM1,IAD
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .        JI,K10,K11,K12,K13,K14,KFI,NS,NNOD,NM,L1,NL1,NM1,
     .        JI1,L10,L11,L12,L13,L14,NNOD1,I1,NSN1,M1,K1,IC
c----------------------
      K=0
      NS= 0
      NK=1
      DO J=1,NINT2
       N=IINT2(J)
       NSN = IPARI(5,N)
       JI=IPARI(1,N)
       K10=JI-1
       K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
       K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
       K13=K12+NSN
C------MSR(NMN)-----
       K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
       KFI=K14+NSN
       NSC2(J)=0
       DO I=1,NSN
        ID = I+K
        NSS2(ID)=0
        NI=INTBUF_TAB(N)%NSV(I)
        IF (NDOF(NI)>0) THEN
         L=INTBUF_TAB(N)%IRTLM(I)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         DO M=1,NNOD
          NM=INTBUF_TAB(N)%IRECTM(NL+M)
          IF (NDOF(NM)>0) THEN
           NKE1=INLOC(NM)
           DO J1=1,NNOD
            NM1=INTBUF_TAB(N)%IRECTM(NL+J1)
           IF (NM/=NM1) CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NM1)
           ENDDO
          ENDIF
         ENDDO
         NKE=INLOC(NI)
         DO N1=1,NROWK(NKE)
          NJ=ICOK(N1,NKE)
          IF (NDOF(NJ)>0.AND.
     .        (.NOT.INTAB(NSN,INTBUF_TAB(N)%NSV(1),NJ))) THEN
           J1=NS+NSS2(ID)+1
           ISS2(J1)=NJ
           NSS2(ID)=NSS2(ID)+1
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=INTBUF_TAB(N)%IRECTM(NL+M)
            IF (NDOF(NM)>0) THEN
             NKE1=INLOC(NM)
             CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NJ)
             CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),NM)
            ENDIF
           ENDDO
          ENDIF
         ENDDO
         NS=NS+NSS2(ID)
C-----with Kij block-(i,j secnd)-----
         DO N1=I+1,NSN
          NJ=INTBUF_TAB(N)%NSV(N1)
          L1=INTBUF_TAB(N)%IRTLM(N1)
           IF (NDOF(NJ)>0.AND.
     .         INTAB(NROWK(NKE),ICOK(1,NKE),NJ)) THEN
            NSC2(J)=NSC2(J)+1
            ID =NK+2*(NSC2(J)-1)
            ISIJ2(ID)=I
            ISIJ2(ID+1)=N1
            IF(L/=L1) THEN
             NL1=4*(L1-1)
             DO M=1,NNOD
              NM=INTBUF_TAB(N)%IRECTM(NL+M)
              IF (NDOF(NM)>0) THEN
               NKE1=INLOC(NM)
               DO J1=1,4
                NM1=INTBUF_TAB(N)%IRECTM(NL1+J1)
                IF (NM/=NM1.AND.NDOF(NM1)>0) THEN
                 NKE2=INLOC(NM1)
                 CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NM1)
                 CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),NM)
                ENDIF
               ENDDO
              ENDIF
             ENDDO
            ENDIF
           ENDIF
         ENDDO
        ENDIF
       ENDDO
       K=K+NSN
       NK=NK+2*NSC2(J)
      ENDDO
C+++couplage entre int2----
      NMC2=0
C-----RBE2------
      K=0
      DO N=1,NRBE2
       K1=IRBE2(1,N)
       M =IRBE2(3,N)
       NSN=IRBE2(5,N)
        NKE1=INLOC(M)
        NS = 0
       IC = 7*512+7*64-IRBE2(4,N)
        DO I=1,NSN
         NI=LRBE2(I+K1)
         NSRB2(I+K1)=0
         IF (NDOF(NI)>0) THEN
          NKE=INLOC(NI)
          DO N1=1,NROWK(NKE)
           IF (NKE <= INK) THEN
            NJ=ICOK(N1,NKE)
           ELSE
            NJ=ICOKM(N1,NKE-INK)
           END IF
           NKE2=INLOC(NJ)
           IF (NDOF(NJ)>0.AND.NJ/=NI) THEN
            CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKE1-INK),NJ)
            IF (NKE2<=INK) THEN
             CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),M)
            ELSEIF (NKE2>0) THEN
             CALL REORDER_A(NROWK(NKE2),ICOKM(1,NKE2-INK),M)
            ENDIF
            K=K+1
             NSRB2(I+K1)=NSRB2(I+K1)+1
             ISB2(K)=NJ
             NS=NS+1
           ENDIF
          ENDDO
          IF (IC>0) THEN
	       CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKE1-INK),NI)
           IF (NKE <= INK) THEN
            CALL REORDER_A(NROWK(NKE),ICOK(1,NKE),M)
           ELSEIF (NKE > 0) THEN		
            CALL REORDER_A(NROWK(NKE),ICOKM(1,NKE-INK),M)
           END IF
	      ENDIF
         ENDIF
        ENDDO
       IRBE2(8,N) = NS
      ENDDO
C------------RBE3-----
      K = 0
      DO I=1,NRBE3
       IAD=IRBE3(1,I)
       NI =IRBE3(3,I)
       IF (NI==0) CYCLE
       NNOD=IRBE3(5,I)
        NS=0
        IF (NDOF(NI)>0) THEN
         DO M=1,NNOD
          NM=LRBE3(IAD+M)
          IF (NDOF(NM)>0) THEN
           NKE1=INLOC(NM)
           DO J1=1,NNOD
            NM1=LRBE3(IAD+J1)
            IF (NKE1<=INK.AND.NM/=NM1) THEN
             CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NM1)
            ELSEIF (NM/=NM1) THEN
             CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKE1-INK),NM1)
            ENDIF
           ENDDO
          ENDIF
         ENDDO
         NKE=INLOC(NI)
         DO N1=1,NROWK(NKE)
          IF (NKE <= INK) THEN
           NJ=ICOK(N1,NKE)
          ELSE
           NJ=ICOKM(N1,NKE-INK)
          END IF
          IF (NDOF(NJ)>0.AND.NI/=NJ) THEN
           NS=NS+1
           K = K + 1
           ISS3(K)=NJ
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
            IF (NDOF(NM)>0) THEN
             NKE1=INLOC(NM)
	
             IF (NKE1<=INK) THEN
              CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NJ)
	         ELSE
              CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKE1-INK),NJ)
	         ENDIF
	
             IF (NKE2<=INK) THEN
              CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),NM)
	         ELSE
              CALL REORDER_A(NROWK(NKE2),ICOKM(1,NKE2-INK),NM)
	         ENDIF
	
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
        IRBE3(8,I) = NS
      ENDDO
C-----active rigid body main nodes------
      K=0
      NS= 0
      NK=1
      DO J=1,NRBYAC
       N=IRBYAC(J)
       K1=IRBYAC(J+NRBYKIN)
       M  =NPBY(1,N)
       NSN  =NPBY(2,N)
       NSC(J)=0
       IF (NDOF(M)>0) THEN
        NKE1=INLOC(M)
        NKM1=NKE1-INK
        DO I=1,NSN
         ID = I+K
         NI=LPBY(I+K1)
         NSS(ID)=0
         IF (NDOF(NI)>0) THEN
          NKE=INLOC(NI)
          DO N1=1,NROWK(NKE)
           IF (NKE <= INK) THEN
            NJ=ICOK(N1,NKE)
           ELSE
            NJ=ICOKM(N1,NKE-INK)
           END IF
           NKE2=INLOC(NJ)
           IF (NDOF(NJ)>0.AND.
     .         (.NOT.INTAB(NSN,LPBY(K1+1),NJ))) THEN
            CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKM1),NJ)
           IF (NKE2<=INK) THEN
             CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),M)
             J1=NS+NSS(ID)+1
             ISS(J1)=NJ
             NSS(ID)=NSS(ID)+1
            ELSE
             CALL REORDER_A(NROWK(NKE2),ICOKM(1,NKE2-INK),M)
            ENDIF
           ENDIF
          ENDDO
          NS=NS+NSS(ID)
C-----create rigid body secnd nodes with Kij block-(i,j have the same M)-----
          DO N1=I+1,NSN
           NJ=LPBY(K1+N1)
           IF (NKE <= INK) THEN
            IF (NDOF(NJ)>0.AND.
     .         (INTAB(NROWK(NKE),ICOK(1,NKE),NJ))) THEN
             NSC(J)=NSC(J)+1
             ID =NK+2*(NSC(J)-1)
             ISIJ(ID)=NI
             ISIJ(ID+1)=NJ
            ENDIF
           ELSE
            IF (NDOF(NJ)>0.AND.
     .         (INTAB(NROWK(NKE),ICOKM(1,NKE-INK),NJ))) THEN
             NSC(J)=NSC(J)+1
             ID =NK+2*(NSC(J)-1)
             ISIJ(ID)=NI
             ISIJ(ID+1)=NJ
            ENDIF
           END IF
          ENDDO
         ENDIF
        ENDDO
       ENDIF
       K=K+NSN
       NK=NK+2*NSC(J)
      ENDDO
C+++couplage entre rigid bodies----
      NMC=0
      IF (NRBYAC>1) THEN
       DO J=1,NRBYAC
        N=IRBYAC(J)
        K=IRBYAC(J+NRBYKIN)
        M  =NPBY(1,N)
        NS=NPBY(2,N)
C
        IF (NDOF(M)>0) THEN
         NKE1=INLOC(M)
         NKM1=NKE1-INK
        DO J1=J+1,NRBYAC
         N1=IRBYAC(J1)
         L1=IRBYAC(J1+NRBYKIN)
         NM =NPBY(1,N1)
         NSN =NPBY(2,N1)
         IF (NDOF(NM)>0.AND. NKMAX>0) THEN
           IF (INTAB(NROWK(NKE1),ICOKM(1,NKM1),NM)) THEN
           DO I=1,NSN
            ID = I+L1
            NI=LPBY(ID)
            IF (NDOF(NI)>0) THEN
             NKE=INLOC(NI)
             IF (NKE <= INK) THEN
              IF (INTAB(NROWK(NKE),ICOK(1,NKE),M)) THEN
              NJ=0
C------cherche-secnd pairs----
              DO N1=1,NS
               N2=LPBY(K+N1)
               IF (NDOF(N2)>0.AND.
     .             INTAB(NROWK(NKE),ICOK(1,NKE),N2)) THEN
                NJ=N2
                NMC=NMC+1
                ID =2*(NMC-1)+1
                IMIJ(ID)=M
                IMIJ(ID+1)=NM
                ISIJ(NK+ID)=NI
                ISIJ(NK+ID-1)=NJ
               ENDIF
              ENDDO
              ENDIF
             ELSE
              IF (INTAB(NROWK(NKE),ICOKM(1,NKE-INK),M)) THEN
              NJ=0
C------cherche-secnd pairs----
              DO N1=1,NS
               N2=LPBY(K+N1)
               IF (NDOF(N2)>0.AND.
     .             INTAB(NROWK(NKE),ICOKM(1,NKE-INK),N2)) THEN
                NJ=N2
                NMC=NMC+1
                ID =2*(NMC-1)+1
                IMIJ(ID)=M
                IMIJ(ID+1)=NM
                ISIJ(NK+ID)=NI
                ISIJ(NK+ID-1)=NJ
               ENDIF
              ENDDO
              ENDIF
             END IF !(NKE <= INK) THEN
            ENDIF
           ENDDO
           END IF !IF (INTAB(NROWK(NKE1)
         END IF !IF (NDOF(NM)>0.AND. NKMAX>0)
C
        ENDDO
        ENDIF
C
       ENDDO
      ENDIF
      IF (NADMESH > 0) CALL RMIND_IMP(NNMAX,INLOC,NROWK,ICOK )
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C------computation for NNMAX,NKMAX------
Chd|====================================================================
Chd|  DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        DIM_ELEMS2                    source/implicit/ind_glob_k.F  
Chd|        DIM_ELEMS3                    source/implicit/ind_glob_k.F  
Chd|        DIM_KINE_P                    source/implicit/ind_glob_k.F  
Chd|        DIM_KINE_S                    source/implicit/ind_glob_k.F  
Chd|        DIM_KINE_T                    source/implicit/ind_glob_k.F  
Chd|        KIN_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        KIN_NRMAX0                    source/mpi/implicit/imp_fri.F 
Chd|        RMDIM_IMP                     source/model/remesh/rm_imp0.F 
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINMAX(
     1    IGEO      ,NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     2    SENSOR    ,IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,
     3    INTBUF_TAB,IXS       ,IXQ       ,IXC       ,IXT       ,
     4    IXP       ,IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     5    IXS20     ,IXS16     ,IXUR      ,IPARG     ,NDOF      ,
     6    NSI2      ,NSRB      ,ELBUF     ,NKINE     ,INLOC     ,
     7    NROW      ,NNMAX     ,NKMAX     ,NSS       ,NSIJ      ,
     8    NMIJ      ,NSS2      ,NSIJ2     ,NMIJ2     ,FR_ELEM   ,
     9    IAD_ELEM  ,SH4TREE   ,SH3TREE   ,IRBE3     ,LRBE3     ,
     A    NSS3      ,IRBE2     ,LRBE2     ,NSB2      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IGEO(*),IRBE3(*),LRBE3(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),NSI2,NSRB
      INTEGER FR_ELEM(*),IAD_ELEM(2,*),SH4TREE(*),SH3TREE(*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),NDOF(*),NROW(*),NNMAX,NKINE,
     .   INLOC(*),NKMAX,NSS,NSIJ,NMIJ,NSS2,NSIJ2,NMIJ2,NSS3,
     .   IRBE2(*),LRBE2(*),NSB2
C     REAL
      my_real
     .   SENSOR(NSENSR,*),ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,M,L,NKINE0,NNMAX0,NK,L1,L2,IERR,LNK
      INTEGER, DIMENSION(:),ALLOCATABLE :: ICOK,NROWI
C-----------------------------------------------
      DO N =1,NUMNOD
       INLOC(N)=0
      ENDDO
c-----2. pepare icol for secnd nodes using ikine;
      CALL DIM_KINE_P(
     1    IGEO      ,NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     2    SENSOR    ,IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,
     3    NDOF      ,NSI2      ,NSRB      ,NKINE     ,
     7    INLOC     ,IRBE3     ,IRBE2     ,LRBE2     ,LNK       ,
     8    INTBUF_TAB       )
C----- pass for IND_GLOB_K, including RBE2 main	 
      LCOKM=LNK
      NKINE0=NKINE
      IF (NKINE0>0) THEN
       ALLOCATE(ICOK(NKINE*NNMAX))
       ALLOCATE(NROWI(NKINE))
      ENDIF
      DO N =1,NKINE0
       NROWI(N)=0
      ENDDO
      CALL DIM_ELEMS3(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROWI     ,
     4    INLOC     ,NNMAX     ,ICOK      ,IGEO      ,ELBUF_TAB )
      IF (IMACH==3.AND.NSPMD>1) THEN
         CALL KIN_NRMAX(
     1    NNMAX     ,NNMAX     ,NROWI     ,ICOK      ,ICOK      ,
     2    INLOC     ,NUMNOD    ,FR_ELEM   ,IAD_ELEM  )
      ENDIF
      DO N =1,NUMNOD
       NK=INLOC(N)
       IF (NK > 0) NROW(N) = MAX(NROW(N),NROWI(NK))
      ENDDO
c-----3. calcul NNMAX,NKMAX and creating INLOC,NKINE;
      CALL DIM_KINE_S(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOF      ,
     3    NNMAX     ,NROW      ,NROWI     ,NKINE     ,INLOC     ,
     4    ICOK      ,IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     )
      IF (NKINE0>0) THEN
       DEALLOCATE(ICOK)
       DEALLOCATE(NROWI)
      ENDIF
C
      NKMAX=NNMAX
      NNMAX0=NNMAX
      DO N =1,NUMNOD
       NK=INLOC(N)
       IF (NK>LNK) THEN
        NNMAX=MAX(NNMAX,NROW(N))
       ELSEIF (NK>0) THEN
        NKMAX=MAX(NKMAX,NROW(N))
       ENDIF
      ENDDO
C----for some special case (hierarchy kinematic RBE2/RBE3),NKMAX is underestimated 
        IF (NSPMD>1)CALL SPMD_MAX_I(NNMAX)
        NKMAX=MAX(NKMAX,NNMAX)
        IF (NSPMD>1)CALL SPMD_MAX_I(NKMAX)
C
      NKINE0=NKINE
      IF (NKINE0>0) THEN
       NK = LNK*NKMAX+NKINE*NNMAX
       ALLOCATE(ICOK(NK))
       ALLOCATE(NROWI(NKINE))
      ENDIF
      DO N =1,NKINE0
       NROWI(N)=0
      ENDDO
        L1 = 1
        L2 = 1 + LNK*NKMAX
         CALL DIM_ELEMS2(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROWI     ,
     4    INLOC     ,NNMAX     ,ICOK(L2)  ,NKMAX     ,ICOK(L1)  ,
     5    LNK       ,IGEO      ,ELBUF_TAB )
c
      IF (IMACH==3.AND.NSPMD>1) THEN
         CALL KIN_NRMAX0(
     1    NNMAX     ,NKMAX     ,NROWI     ,ICOK(L2)  ,ICOK(L1)  ,
     2    INLOC     ,LNK       ,FR_ELEM   ,IAD_ELEM  )
      ENDIF
c-----3. calcul NNMAX,NKMAX and creating INLOC,NKINE;
      CALL DIM_KINE_T(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOF      ,
     3    NNMAX     ,NROWI     ,NKINE     ,INLOC     ,ICOK(L2)  ,
     4    NSS       ,NSIJ      ,NMIJ      ,NSS2      ,NSIJ2     ,
     5    NMIJ2     ,NKMAX     ,ICOK(L1)  ,LNK       ,IRBE3     ,
     6    LRBE3     ,NSS3      ,IRBE2     ,LRBE2     ,NSB2      )
      NKMAX=0
      NNMAX=NNMAX0
      DO N =1,NUMNOD
       NK=INLOC(N)
       IF (NK>LNK) THEN
        NNMAX=MAX(NNMAX,NROWI(NK))
       ELSEIF (NK>0) THEN
        NKMAX=MAX(NKMAX,NROWI(NK))
       ENDIF
      ENDDO
      DO N =1,NUMNOD
       NK=INLOC(N)
       IF (NK>0)NROW(N)=NROWI(NK)
      ENDDO
      IF (NKINE0>0) THEN
       DEALLOCATE(ICOK)
       DEALLOCATE(NROWI)
      ENDIF
      IF (NADMESH > 0) CALL RMDIM_IMP(IXC ,IXTG  ,NDOF ,NNMAX,NKINE,
     1                                INLOC,NROW ,ITAB ,SH4TREE,SH3TREE)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C------estimation for NNMAX,NKMAX,NNZK, prepare data for IND_GLOB_K
Chd|====================================================================
Chd|  DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|        DIM_ELEMAX                    source/implicit/ind_glob_k.F  
Chd|        DIM_ELEMS1                    source/implicit/ind_glob_k.F  
Chd|        DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|        DIM_NDOF_D                    source/implicit/ind_glob_k.F  
Chd|        DIM_NDOF_I                    source/implicit/ind_glob_k.F  
Chd|        DIM_NDOF_II                   source/implicit/ind_glob_k.F  
Chd|        DIM_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        MONV_PREM                     source/airbag/monv_imp0.F     
Chd|        NDOF_FV                       source/implicit/ind_glob_k.F  
Chd|        SET_IKIN2G                    source/mpi/implicit/imp_fri.F 
Chd|        SPMD_NDOF                     source/mpi/implicit/imp_spmd.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_GLOB_K(
     1    GEO       ,NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     2    SENSOR    ,IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,
     3    IXS       ,IXQ       ,IXC       ,IXT       ,
     4    IXP       ,IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     5    IXS20     ,IXS16     ,IXUR      ,IPARG     ,NDOF      ,
     6    IDDL      ,NDDL      ,NNZK      ,ELBUF     ,INLOC     ,
     7    LSIZE     ,FR_ELEM   ,IAD_ELEM  ,FR_I2M    ,IAD_I2M   ,
     8    NPRW      ,NMONV     ,IMONV     ,MONVOL    ,IGRSURF   ,
     9               FR_MV     ,IPM       ,IGEO      ,IAD_RBY   ,
     A    FR_RBY    ,SH4TREE   ,SH3TREE   ,IRBE3     ,LRBE3     ,
     B    FR_RBE3M  ,IAD_RBE3M ,IRBE2     ,LRBE2     ,IBFV      ,
     C    VEL       ,ELBUF_TAB ,IFRAME    ,INTBUF_TAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
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
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),FR_ELEM(*)  ,IAD_ELEM(2,*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),
     .        FR_I2M(*),IAD_I2M(*),FR_RBY(*),IAD_RBY(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),
     .        FR_MV(NSPMD+2,NVOLU),NPRW(*),FR_RBE3M(*),IAD_RBE3M(*)
      INTEGER IPM(NPROPMI,*),IGEO(NPROPGI,*),IFRAME(LISKN,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),IDDL(*),NDOF(*),IRBE3(*),LRBE3(*),
     .   NDDL ,NNZK,INLOC(*),LSIZE(*),SH4TREE(*), SH3TREE(*),
     .   IRBE2(*),LRBE2(*),IBFV(*)
C     REAL
      my_real
     .   GEO(NPROPG,*),SENSOR(NSENSR,*),ELBUF(*),VEL(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NKINE,NKMAX,NNMAX,NSI2,NSRB,NSS,NSIJ,NMIJ,NSS2,NSIJ2,NSS3
      INTEGER I,J,K,N,M,L,NDOFI,NDOFJ,NKINE0,NMIJ2,IP,NPN,NPP,IER1
      INTEGER IAD_M(NSPMD+1),NSB2,NSRB2
      INTEGER, DIMENSION(:),ALLOCATABLE :: FR_M
C-----------------------------------------------
C-----NNMAX:max number of connected nodes(excepting main nodes of rbodies)
C  INLOC(NUMNOD) : global numnod in order of IDDL
C   actuel order : non kinematical nodes;kinematical nodes; main nodes of rbodies
C  ICOL(NKMAX,NRBYAC) for main nodes
C  ICOL(NNMAX,NKINE-NRBYAC) for kine. nodes
C  ICOL(NNMAX,NNSIZ) for other nodes
C  NROW(NUMNOD) number of connected nodes use the mem. of iddl
C----pour spmd on ne distinque plus nodes kine ---------------------------------------
C   et INLOC(NUMNOD) : local(pi) numnod in order of IDDL
C   actuel order : nodes fontieres avec pj(j<i); nodes; nodes fontieres avec pj(j>i)
c-----1. calcule NNMAX,NDOF;
      DO N =1,NUMNOD
       IDDL(N)=0
       NDOF(N)=0
      ENDDO
      CALL DIM_NDOF_I(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,SENSOR    ,
     2    IRBYAC    ,NDOF      ,NSRB      ,IPARI     ,
     3    NINT2     ,IINT2     ,NSI2      ,NPRW      ,IRBE3     ,
     4    IRBE2     ,NSRB2     ,FR_ELEM   ,IAD_ELEM  ,INTBUF_TAB  )
      CALL DIM_ELEMS1(
     1    IGEO      ,ELBUF     ,IPARG     ,IXS       ,IXQ       ,
     2    IXC       ,IXT       ,IXP       ,IXR       ,IXTG      ,
     3    IXTG1     ,IXS10     ,IXS20     ,IXS16     ,IXUR      ,
     4    NDOF      ,IDDL      ,ELBUF_TAB )
      CALL DIM_NDOF_II(
     1    NINT2     ,IINT2     ,IPARI     ,NDOF      ,
     2    NRBE3     ,IRBE3     ,LRBE3     ,NRBE2     ,IRBE2     ,
     3    LRBE2     ,INTBUF_TAB )
C+++ not allowing imposed rotations on solid element..---
      CALL NDOF_FV(IBFV  ,VEL   ,NDOF  ,IFRAME )
      IF (IMP_RBY==1) CALL DIM_NDOF_D(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NDOF      ,
     2    IAD_RBY   ,FR_RBY    )
      IF (IMACH==3.AND.NSPMD>1) THEN
       NNMAX=IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
       IF (NNMAX>0) CALL SPMD_NDOF(NDOF,FR_ELEM,IAD_ELEM,NNMAX)
      ENDIF
      CALL MONV_PREM(
     1    NMONV     ,IMONV     ,MONVOL    ,IGRSURF   ,
     2    FR_MV     ,INLOC     ,NPBY      ,LPBY      ,NRBYAC    ,
     3    IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,
     4    NDOF      ,IPREC     ,IRBE3     ,IRBE2     ,LRBE2     )
      NNMAX=0
      DO N =1,NUMNOD
       IF (NDOF(N)>0) NNMAX=MAX(NNMAX,IDDL(N))
      ENDDO
c-----raffine NNMAX;
      NKINE0=2*NNMAX
      IF (IMACH==3.AND.NSPMD>1) THEN
        NPP=IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        M = IAD_I2M(NSPMD+1)-IAD_I2M(1)+
     .      IAD_RBE3M(NSPMD+1)-IAD_RBE3M(1)
       ALLOCATE(FR_M(M))
       M = 0
       IAD_M(1)=1
       DO IP =1,NSPMD
        IAD_M(IP+1)=M+1
       ENDDO
C
       CALL DIM_FR_K(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NDOF      ,
     4    NKINE0    ,INLOC     ,FR_ELEM   ,IAD_ELEM  ,NPP       ,
     5    IGEO      ,FR_M      ,IAD_M     ,ELBUF_TAB )
       DEALLOCATE(FR_M)
      ENDIF
      CALL DIM_ELEMAX(
     1    IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     2    IXR       ,IXTG      ,IXTG1     ,IXS10     ,IXS20     ,
     3    IXS16     ,IXUR      ,IPARG     ,ELBUF     ,NDOF      ,
     4    IDDL      ,INLOC     ,NNMAX     ,NKINE0    ,NNSIZ     ,
     5    IGEO      ,ELBUF_TAB )
      IF (IMACH==3.AND.NSPMD>1) THEN
         CALL DIM_NRMAX(IDDL    ,FR_ELEM   ,IAD_ELEM ,NNMAX  )
      ENDIF
C
      CALL DIM_KINMAX(
     1    IGEO      ,NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     2    SENSOR    ,IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,
     3    INTBUF_TAB,IXS       ,IXQ       ,IXC       ,IXT       ,
     4    IXP       ,IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     5    IXS20     ,IXS16     ,IXUR      ,IPARG     ,NDOF      ,
     6    NSI2      ,NSRB      ,ELBUF     ,NKINE     ,INLOC     ,
     7    IDDL      ,NNMAX     ,NKMAX     ,NSS       ,NSIJ      ,
     8    NMIJ      ,NSS2      ,NSIJ2     ,NMIJ2     ,FR_ELEM   ,
     9    IAD_ELEM  ,SH4TREE   ,SH3TREE   ,IRBE3     ,LRBE3     ,
     A    NSS3      ,IRBE2     ,LRBE2     ,NSB2      ,ELBUF_TAB )
c-----4. calcul NNZK,;
      IF (IRODDL==0) THEN
       NDOFJ=3
      ELSE
C-------cela sur-dimensioner kij en cas du model mix (solide +coque) mais pas trop grace
       NDOFJ=6
      ENDIF
      NNZK = 0
      DO N=1,NUMNOD
        DO K=1,NDOF(N)
C-------termes knn-------
         DO J=1,NDOF(N)
          IF (J/=K) NNZK = NNZK+1
c          NNZK = NNZK+NDOF(N)-1
         ENDDO
C-------termes kn,nj-------
         DO J=1,IDDL(N)
          DO L=1,NDOFJ
           NNZK = NNZK+1
c           NNZK = NNZK+NDOFJ*IDDL(N)
          ENDDO
         ENDDO
        ENDDO
      ENDDO
      NNZK = NNZK/2+1
       NPN=0
       NPP=0
      IF (IMACH==3.AND.NSPMD>1) THEN
       CALL SET_IKIN2G(NKINE,INLOC)
       J=0
       L=0
       DO N =1,NUMNOD
        IDDL(N)=0
       ENDDO
C------d'abord frontieres avec precedent procs j<i
       DO IP =1,ISPMD
        DO M=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N=FR_ELEM(M)
         IF (IDDL(N)==0) THEN
          J=J+1
          INLOC(J)=N
          IDDL(N)=J
         ENDIF
        ENDDO
       ENDDO
C------frontieres avec procs derierre a la fin j>i
       DO IP =ISPMD+2,NSPMD
        DO M=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N=FR_ELEM(M)
         IF (IDDL(N)==0) THEN
          L=L+1
          IDDL(N)=-L
         ENDIF
        ENDDO
       ENDDO
       NPN=J
       NPP=L
       DO N =1,NUMNOD
        IF (IDDL(N)==0) THEN
          J=J+1
          INLOC(J)=N
        ELSEIF (IDDL(N)<0) THEN
          K=NUMNOD-L-IDDL(N)
          INLOC(K)=N
        ENDIF
       ENDDO
      ELSE
C---------set INLOC; pour kine. nodes---
       DO N =1,NUMNOD
        IDDL(N)=INLOC(N)
       ENDDO
       IF (IKPAT<=1) THEN
        J=0
        DO N =1,NUMNOD
         IF (IDDL(N)==0) THEN
          J=J+1
          INLOC(J)=N
         ELSE
          K=NUMNOD-IDDL(N)+1
          INLOC(K)=N
         ENDIF
        ENDDO
       ELSE
        J=NKINE
        DO N =1,NUMNOD
         IF (IDDL(N)==0) THEN
          J=J+1
          INLOC(J)=N
         ELSE
          K=IDDL(N)
          INLOC(K)=N
         ENDIF
        ENDDO
       ENDIF
      ENDIF
c-----4. calcul IDDL,;
      NDDL =0
      DO J=1,NUMNOD
       N=INLOC(J)
       IDDL(N)=NDDL
       NDOFI = NDOF(N)
       IF (NDOFI>0) NDDL = NDDL + NDOFI
      ENDDO
c-----dimensions divers;
      LSIZE(1)=NSRB
      LSIZE(2)=NSI2
      LSIZE(3)=NSS+1
      LSIZE(4)=NSIJ+1
      LSIZE(5)=NMIJ+1
      LSIZE(6)=NSS2+1
      LSIZE(7)=NSIJ2+1
      LSIZE(8)=NKINE
      LSIZE(9)=NNMAX
      LSIZE(10)=NKMAX
      LSIZE(11)=NMIJ2
      LSIZE(12)=NPN
      LSIZE(13)=NPP
      LSIZE(14)=NSS3
      LSIZE(15)=NSB2
      LSIZE(16)=NSRB2
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- stockage M.C.R.S + SYM- ----- 
Chd|====================================================================
Chd|  SET_IND_K                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_IND_K(
     1    IDDL      ,NDOF      ,IADK      ,JDIK      ,NDDL      ,
     2    NNZK      ,NROW      ,ICOL      ,N         ,IKPAT     )
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
      INTEGER
     .   IDDL(*),NDOF(*),IADK(*),JDIK(*),
     .   NDDL ,NNZK,NROW ,ICOL(*),N,IKPAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NL,NJ,NDOFI
c----- calcul IADK,JDIK,NNZK-----
          NDOFI = NDOF(N)
          DO K=1,NDOFI
C-------termes knn-------
           IF (IKPAT==0) THEN
            DO J=K+1,NDOFI
             NNZK = NNZK+1
             JDIK(NNZK) = IDDL(N)+J
            ENDDO
C-------termes kn,nj-------
            DO J=1,NROW
             NJ = ICOL(J)
             DO L=1,NDOF(NJ)
              NNZK = NNZK+1
              JDIK(NNZK) = IDDL(NJ)+L
             ENDDO
            ENDDO
           ELSE
C-------termes knj,n-------
            DO J=1,NROW
             NJ = ICOL(J)
             DO L=1,NDOF(NJ)
              NNZK = NNZK+1
              JDIK(NNZK) = IDDL(NJ)+L
             ENDDO
            ENDDO
            DO J=1,K-1
             NNZK = NNZK+1
             JDIK(NNZK) = IDDL(N)+J
            ENDDO
           ENDIF
           NDDL = NDDL +1
           IADK(NDDL) = NNZK+1
          ENDDO
C
      RETURN
      END
C   --------- stockage M.C.R.S + SYM- ----- 
Chd|====================================================================
Chd|  IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|        DIM_ELEMS3                    source/implicit/ind_glob_k.F  
Chd|        DIM_ELEMS4                    source/implicit/ind_glob_k.F  
Chd|        GET_IKIN2G                    source/mpi/implicit/imp_fri.F 
Chd|        IND_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        IND_KINE_K                    source/implicit/ind_glob_k.F  
Chd|        IND_KINE_KP                   source/mpi/implicit/imp_fri.F 
Chd|        IND_NRFR                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        KIN_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        REORDER_J                     source/implicit/ind_glob_k.F  
Chd|        REORDER_L                     source/implicit/ind_glob_k.F  
Chd|        SET_IND_K                     source/implicit/ind_glob_k.F  
Chd|        SPC_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        ZERO_IKIN2G                   source/mpi/implicit/imp_fri.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_GLOB_K(NPBY,LPBY,
     1    ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,ISIJ      ,
     2    NMC       ,IMIJ      ,NSS       ,ISS       ,NINT2     ,
     3    IINT2     ,IPARI     ,INTBUF_TAB,NSC2      ,ISIJ2     ,
     4    NSS2      ,ISS2      ,IPARG     ,ELBUF     ,ELBUF_TAB ,
     5    IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     6    IXR       ,IXTG      ,IXTG1     ,IXS10     ,IXS20     ,
     7    IXS16     ,IXUR      ,IDDL      ,NDOF      ,IADK      ,
     8    JDIK      ,NDDL      ,NNZK      ,NNMAX     ,NKINE     ,
     9    INLOC     ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     A    NMC2      ,IMIJ2     ,IRK       ,NPN       ,NPP       ,
     B    FR_ELEM   ,IAD_ELEM  ,IPM       ,IGEO      ,IRBE3     ,
     C    LRBE3     ,ISS3      ,FR_I2M    ,IAD_I2M   ,FR_RBE3M  ,
     D    IAD_RBE3M ,IRBE2     ,LRBE2     ,ISB2      ,NSRB2     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NNMAX,IRK,NKMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NSC(*),ISIJ(*),NSS(*),ISS(*),NINT2,IINT2(*),
     .        NSC2(*),ISIJ2(*),NSS2(*),ISS2(*),IPARI(NPARI,*),
     .        NMC,IMIJ(*),ICOK(NNMAX,*),ICOKM(NKMAX,*),NROWK(*)
      INTEGER IPM(NPROPMI,*),IGEO(NPROPGI,*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),IDDL(*),NDOF(*),IADK(*),JDIK(*),
     .   NDDL ,NNZK,NKINE,INLOC(*),NMC2,IMIJ2(*),NPN ,NPP,
     .   FR_ELEM(*),IAD_ELEM(2,*),IRBE3(*),LRBE3(*),ISS3(*),
     .   FR_I2M(*),IAD_I2M(*),FR_RBE3M(*),IAD_RBE3M(*),
     .   IRBE2(*),LRBE2(*),ISB2(*),NSRB2(*)
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C=======================================================================
C     stockage M.C.R.S (Modified Compressed Row Stockage)
C     stockage creux : diagonale + trangle en lignes: IKPAT=0:triang_sup IKPAT=1: inf
C        [K](id,jd)     -> DIAG(ND)+LT(IK)(exclue diag)
C        id = 1..nddl        : ID = IADK(ID)...IADK(ID+1)-1
C        jd = 1..NNZK        : JD = JDIK(IK)
C        NDOF(NUMNOD)        : nombre de ddl
C     DIAG(NDDL)
C     IADK(NDDL+1)
C     JDIK(NNZK)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICOL(NNMAX,NNSIZ),NROW(NNSIZ),JLT1,INK,NRMAX
      INTEGER I,J,K,N,L,NL,NJ,LENK,M,NK,ID,NFT,JLT,N_FR,IP
      INTEGER ILOC(NUMNOD)
      INTEGER IAD_M(NSPMD+1)
      INTEGER, DIMENSION(:),ALLOCATABLE :: FR_M
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ICOKFR
C     m:main,s:secnd NSS(NRBYAC):nombre de j: Kjm= sum(KjsCsm)
C     NSC(NRBYAC):nombre de "secnd line": ISIJ(2,NSC,NRBYAC)--
C     nrowk(NKINE), icok size: NNMAX*(NKINE-NRBYAC)+NKMAX*NRBYAC:icok,icokm
C------eventuellment actualiser NDOF due au OFF d'element-
C-----1. calcul IADK,JDIK,par each NNSIZ using INLOC.
C-----IRK=0, calcul des tableux kinematiques est dispense-----
      IF (NSPMD>1) THEN
       DO N = 1 , NUMNOD
         ILOC(N)=0
       ENDDO
       N_FR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        M = IAD_I2M(NSPMD+1)-IAD_I2M(1)+
     .      IAD_RBE3M(NSPMD+1)-IAD_RBE3M(1)
       ALLOCATE(FR_M(M))
       M = 0
       IAD_M(1)=1
       DO IP =1,NSPMD
        IAD_M(IP+1)=M+1
       ENDDO
       IF (NKINE>0) THEN
C-------cree ILOC()------
        INK=NKINE-LCOKM
c        INK=NKINE-NRBYAC
        CALL GET_IKIN2G(NKINE,INK,ILOC)
        IF (IRK == 1) THEN
         DO NK =1,NKINE
          NROWK(NK)=0
         ENDDO
         CALL DIM_ELEMS4(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROWK     ,
     4    ILOC      ,NNMAX     ,ICOK      ,NKMAX     ,ICOKM     ,
     5    INK       ,IGEO      ,ELBUF_TAB )
         CALL KIN_NRMAX(
     1    NNMAX     ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     2    ILOC      ,INK       ,FR_ELEM   ,IAD_ELEM  )
         CALL IND_KINE_K(NPBY,LPBY,
     1    ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,ISIJ      ,
     2    NMC       ,IMIJ      ,NSS       ,ISS       ,NINT2     ,
     3    IINT2     ,IPARI     ,INTBUF_TAB,NSC2      ,ISIJ2     ,
     4    NSS2      ,ISS2      ,NDOF      ,NNMAX     ,NKINE     ,
     5    ILOC      ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     6    NMC2      ,IMIJ2     ,INK       ,IRBE3     ,LRBE3     ,
     7    ISS3      ,IRBE2     ,LRBE2     ,ISB2      ,NSRB2     )
C
         CALL IND_KINE_KP(
     1    NROWK     ,ICOK      ,ICOKM     ,NNMAX     ,NKMAX     ,
     2    NKINE     ,INK       ,IKPAT     ,IDDL      )
C 
        ENDIF
        CALL ZERO_IKIN2G(NKINE,ILOC)
       ENDIF
       CALL INI_FR_K(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NDOF      ,
     4    NNMAX     ,ILOC      ,FR_ELEM   ,IAD_ELEM  ,N_FR      ,
     5    IGEO      ,FR_M      ,IAD_M     ,ELBUF_TAB ,NRMAX     )
       DEALLOCATE(FR_M)
       JLT1=NUMNOD
       LENK = 0
       NL = 1
       IADK(NL) = 1
       ALLOCATE(ICOKFR(NRMAX,N_FR))
C
       DO NFT = 0 , JLT1-1 , NNSIZ
        JLT = MIN( NNSIZ, JLT1 - NFT )
        DO NK=1,JLT
         N=NK+NFT
         K=INLOC(N)
         ILOC(K)=NK
         NROW(NK)=0
        ENDDO
        CALL DIM_ELEMS3(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW      ,
     4    ILOC      ,NNMAX     ,ICOL      ,IGEO      ,ELBUF_TAB )
        CALL IND_NRFR(
     1    NFT       ,JLT       ,NPN       ,NPP       ,NNMAX     ,
     2    NROW      ,ICOL      ,FR_ELEM   ,IAD_ELEM  ,N_FR      ,
     3    ICOKFR    )
        DO NK=1,JLT
         N=NK+NFT
         K=INLOC(N)
         ILOC(K)=0
        ENDDO
        IF (NKINE>0) THEN
         CALL GET_IKIN2G(NKINE,INK,ILOC)
          DO NK=1,JLT
           N=NK+NFT
           J=INLOC(N)
           IF (NDOF(J)>0) THEN
            K=ILOC(J)
            IF (K>INK) THEN
             NJ=K-INK
             CALL SET_IND_K(
     1       IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2       LENK      ,NROWK(K)  ,ICOKM(1,NJ),J        ,IKPAT     )
            ELSEIF (K>0) THEN
             CALL SET_IND_K(
     1       IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2       LENK      ,NROWK(K)  ,ICOK(1,K) ,J        ,IKPAT     )
            ELSE
             IF (IKPAT==0) THEN
              CALL REORDER_J(NROW(NK),ICOL(1,NK),J,IDDL)
             ELSE
              CALL REORDER_L(NROW(NK),ICOL(1,NK),J,IDDL)
             ENDIF
             CALL SET_IND_K(
     1       IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2       LENK      ,NROW(NK)  ,ICOL(1,NK),J         ,IKPAT     )
            ENDIF
           ENDIF
          ENDDO
         CALL ZERO_IKIN2G(NKINE,ILOC)
        ELSE
         IF (IKPAT==0) THEN
          DO NK=1,JLT
           N=NK+NFT
           J=INLOC(N)
           CALL REORDER_J(NROW(NK),ICOL(1,NK),J,IDDL)
           IF (NDOF(J)>0) THEN
            CALL SET_IND_K(
     1     IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2     LENK      ,NROW(NK)  ,ICOL(1,NK),J         ,IKPAT     )
           ENDIF
          ENDDO
         ELSE
          DO NK=1,JLT
           N=NK+NFT
           J=INLOC(N)
           CALL REORDER_L(NROW(NK),ICOL(1,NK),J,IDDL)
           IF (NDOF(J)>0) THEN
            CALL SET_IND_K(
     1     IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2     LENK      ,NROW(NK)  ,ICOL(1,NK),J         ,IKPAT     )
           ENDIF
          ENDDO
         ENDIF
        ENDIF
       ENDDO
C   
       CALL IND_FR_K(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NDOF      ,
     4    NNMAX     ,ILOC      ,FR_ELEM   ,IAD_ELEM  ,N_FR      ,
     5    IGEO      ,ELBUF_TAB )
       IF(IAUTSPC>0) THEN
         CALL SPC_FR_K(
     1    IADK     ,JDIK     ,NDOF      ,IDDL     ,FR_ELEM  ,
     2    IAD_ELEM )
       ENDIF
       DEALLOCATE(ICOKFR)
C   
       GOTO 100
      ENDIF
C----------mono-domaine-------------
      DO N = 1 , NUMNOD
       ILOC(N)=0
      ENDDO
      LENK = 0
      NL = 1
      IADK(NL) = 1
C
      IF (IKPAT<=1) THEN
      JLT1=NUMNOD-NKINE
      DO NFT = 0 , JLT1-1 , NNSIZ
       JLT = MIN( NNSIZ, JLT1 - NFT )
       DO NK=1,JLT
        N=NK+NFT
        K=INLOC(N)
        ILOC(K)=NK
        NROW(NK)=0
       ENDDO
       CALL DIM_ELEMS3(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW      ,
     4    ILOC      ,NNMAX     ,ICOL      ,IGEO      ,ELBUF_TAB )
       IF (IKPAT==0) THEN
        DO NK=1,JLT
         N=NK+NFT
         J=INLOC(N)
         IF (NDOF(J)>0) THEN
          CALL REORDER_J(NROW(NK),ICOL(1,NK),J,IDDL)
          CALL SET_IND_K(
     1     IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2     LENK      ,NROW(NK)  ,ICOL(1,NK),J         ,IKPAT     )
         ENDIF
        ENDDO
       ELSE
        DO NK=1,JLT
         N=NK+NFT
         J=INLOC(N)
         IF (NDOF(J)>0) THEN
          CALL REORDER_L(NROW(NK),ICOL(1,NK),J,IDDL)
          CALL SET_IND_K(
     1     IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2     LENK      ,NROW(NK)  ,ICOL(1,NK),J         ,IKPAT     )
         ENDIF
        ENDDO
       ENDIF
       DO NK=1,JLT
        N=NK+NFT
        K=INLOC(N)
        ILOC(K)=0
       ENDDO
      ENDDO
c-----2. if irk=1 create nrow,icol for all kine. nodes using INLOC()=-INLOC()
c        modifies nrow,icol .
      IF (NKINE==0) GOTO 100
      NFT=NUMNOD-NKINE
      JLT=NKINE
      INK=NKINE-LCOKM
C      INK=NKINE-NRBYAC
      DO NK =1,JLT
       N=NK+NFT
       K=INLOC(N)
       ILOC(K)=NK
      ENDDO
      IF (IRK == 1) THEN
       DO NK =1,JLT
        NROWK(NK)=0
       ENDDO
       CALL DIM_ELEMS4(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROWK     ,
     4    ILOC      ,NNMAX     ,ICOK      ,NKMAX     ,ICOKM     ,
     5    INK       ,IGEO      ,ELBUF_TAB )
C
       CALL IND_KINE_K(NPBY,LPBY,
     1    ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,ISIJ      ,
     2    NMC       ,IMIJ      ,NSS       ,ISS       ,NINT2     ,
     3    IINT2     ,IPARI     ,INTBUF_TAB,NSC2      ,ISIJ2     ,
     4    NSS2      ,ISS2      ,NDOF      ,NNMAX     ,NKINE     ,
     5    ILOC      ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     6    NMC2      ,IMIJ2     ,INK       ,IRBE3     ,LRBE3     ,
     7    ISS3      ,IRBE2     ,LRBE2     ,ISB2      ,NSRB2     )
C
       IF (IKPAT==0) THEN
        DO NK =1,JLT
         N=NK+NFT
         J=INLOC(N)
         IF (NK>INK.AND.NKMAX>0) THEN
          NJ=NK-INK
          CALL REORDER_J(NROWK(NK),ICOKM(1,NJ),J,IDDL)
         ELSE
          CALL REORDER_J(NROWK(NK),ICOK(1,NK),J,IDDL)
         ENDIF
        ENDDO
       ELSE
        DO NK =1,JLT
         N=NK+NFT
         J=INLOC(N)
         IF (NK>INK.AND.NKMAX>0) THEN
          NJ=NK-INK
          CALL REORDER_L(NROWK(NK),ICOKM(1,NJ),J,IDDL)
         ELSEIF (NNMAX>0) THEN
          CALL REORDER_L(NROWK(NK),ICOK(1,NK),J,IDDL)
         ENDIF
        ENDDO
       ENDIF
      ENDIF
C---main nodes of rbodies at last----
      DO NK=1,INK
        N=NK+NFT
        J=INLOC(N)
        IF (NDOF(J)>0) THEN
          CALL SET_IND_K(
     1    IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2    LENK      ,NROWK(NK) ,ICOK(1,NK),J         ,IKPAT     )
        ENDIF
      ENDDO
      DO NK=1+INK,JLT
        N=NK+NFT
        J=INLOC(N)
        IF (NDOF(J)>0) THEN
         NJ=NK-INK
         CALL SET_IND_K(
     1    IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2    LENK      ,NROWK(NK),ICOKM(1,NJ),J         ,IKPAT     )
        ENDIF
      ENDDO
C--------IKPAT=2, rigid body main first----------
      ELSE
       IF (NKINE>0) THEN
        NFT=0
        JLT=NKINE
        INK=NKINE-LCOKM
C        INK=NKINE-NRBYAC
        DO NK =1,LCOKM
         N=NK+NFT
         K=INLOC(N)
         ILOC(K)=NK+INK
        ENDDO
        DO NK =1+LCOKM,JLT
         N=NK+NFT
         K=INLOC(N)
         ILOC(K)=NK-LCOKM
        ENDDO
        IF (IRK == 1) THEN
         DO NK =1,JLT
          NROWK(NK)=0
         ENDDO
        CALL DIM_ELEMS4(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROWK     ,
     4    ILOC      ,NNMAX     ,ICOK      ,NKMAX     ,ICOKM     ,
     5    INK       ,IGEO      ,ELBUF_TAB )
C
        CALL IND_KINE_K(NPBY,LPBY,
     1    ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,ISIJ      ,
     2    NMC       ,IMIJ      ,NSS       ,ISS       ,NINT2     ,
     3    IINT2     ,IPARI     ,INTBUF_TAB,NSC2      ,ISIJ2     ,
     4    NSS2      ,ISS2      ,NDOF      ,NNMAX     ,NKINE     ,
     5    ILOC      ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     6    NMC2      ,IMIJ2     ,INK       ,IRBE3     ,LRBE3     ,
     7    ISS3      ,IRBE2     ,LRBE2     ,ISB2      ,NSRB2     )
         DO NK =1,JLT
          N=NK+NFT
          J=INLOC(N)
          ILOC(J)=0
          IF (NK>LCOKM) THEN
           NJ=NK-LCOKM
           CALL REORDER_L(NROWK(NJ),ICOK(1,NJ),J,IDDL)
          ELSEIF( NKMAX>0) THEN
           CALL REORDER_L(NROWK(NK+INK),ICOKM(1,NK),J,IDDL)
          ENDIF
         ENDDO
        ENDIF
C---main nodes of rbodies first----
        DO NK=1,LCOKM
          N=NK+NFT
          J=INLOC(N)
          ILOC(J)=0
          IF (NDOF(J)>0) THEN
           CALL SET_IND_K(
     1     IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2     LENK      ,NROWK(NK+INK) ,ICOKM(1,NK),J    ,IKPAT     )
          ENDIF
        ENDDO
        DO NK=LCOKM+1,JLT
          N=NK+NFT
          J=INLOC(N)
          ILOC(J)=0
          IF (NDOF(J)>0) THEN
           NJ=NK-LCOKM
           CALL SET_IND_K(
     1      IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2      LENK      ,NROWK(NJ) ,ICOK(1,NJ),J         ,IKPAT     )
          ENDIF
        ENDDO
       ENDIF
       DO NFT = NKINE , NUMNOD-1 , NNSIZ
        JLT = MIN( NNSIZ, NUMNOD - NFT )
        DO NK=1,JLT
         N=NK+NFT
         K=INLOC(N)
         ILOC(K)=NK
         NROW(NK)=0
        ENDDO
        CALL DIM_ELEMS3(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW      ,
     4    ILOC      ,NNMAX     ,ICOL      ,IGEO      ,ELBUF_TAB )
        DO NK=1,JLT
         N=NK+NFT
         J=INLOC(N)
         CALL REORDER_L(NROW(NK),ICOL(1,NK),J,IDDL)
         IF (NDOF(J)>0) THEN
          CALL SET_IND_K(
     1     IDDL      ,NDOF      ,IADK      ,JDIK      ,NL        ,
     2     LENK      ,NROW(NK)  ,ICOL(1,NK),J         ,IKPAT     )
         ENDIF
        ENDDO
        DO NK=1,JLT
         N=NK+NFT
         K=INLOC(N)
         ILOC(K)=0
        ENDDO
       ENDDO
      ENDIF
 100  IADK(NDDL+1) = LENK+1
        IF (LENK>NNZK.OR.NL/=(NDDL+1))
     .    WRITE(*,*)'--MEMERY PROBLEM 2--:',LENK,NL,NNZK,NDDL+1
C---remet positive----
      NNZK = LENK
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_I                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_ELEMS1                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_I(N,IC)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IMIN,IT,II
C
      IF (N<=0) RETURN
      DO I =1,N
       IMIN=IC(I)
       II=I
       DO J =I+1,N
        IF (IC(J)<IMIN) THEN
         IMIN=IC(J)
         II=J
        ENDIF
       ENDDO
       IT=IC(I)
       IC(I)=IMIN
       IC(II)=IT
      ENDDO
C----delete doubles----------------------------
      II=1
      DO I =2,N
       IF (IC(I)/=IC(I-1)) THEN
        II =II +1
        IC(II)=IC(I)
       ENDIF
      ENDDO
      N = II
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_A                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_ELEMS2                    source/implicit/ind_glob_k.F  
Chd|        DIM_ELEMS3                    source/implicit/ind_glob_k.F  
Chd|        DIM_ELEMS4                    source/implicit/ind_glob_k.F  
Chd|        DIM_ELEMSP                    source/implicit/ind_glob_k.F  
Chd|        DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|        DIM_KINE_T                    source/implicit/ind_glob_k.F  
Chd|        DIM_KTOT                      source/implicit/ind_glob_k.F  
Chd|        DIM_SPA2                      source/implicit/ind_glob_k.F  
Chd|        DIM_SPAN                      source/implicit/ind_glob_k.F  
Chd|        IND_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        IND_KINE_I                    source/implicit/ind_glob_k.F  
Chd|        IND_KINE_K                    source/implicit/ind_glob_k.F  
Chd|        IND_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        IND_NRFR                      source/mpi/implicit/imp_fri.F 
Chd|        IND_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        IND_SPA2                      source/implicit/ind_glob_k.F  
Chd|        IND_SPAN                      source/implicit/ind_glob_k.F  
Chd|        INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        KIN_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        KIN_NRMAX0                    source/mpi/implicit/imp_fri.F 
Chd|        RMDIM_IMP                     source/model/remesh/rm_imp0.F 
Chd|        RMIND_IMP                     source/model/remesh/rm_imp0.F 
Chd|        ROWFR_IND                     source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND11                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND24                   source/mpi/implicit/imp_fri.F 
Chd|        ROW_ADDS                      source/implicit/ind_glob_k.F  
Chd|        ROW_INT1                      source/implicit/ind_glob_k.F  
Chd|        ROW_INT111                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT2                      source/implicit/ind_glob_k.F  
Chd|        ROW_INT241                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT242                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT51                     source/implicit/ind_glob_k.F  
Chd|        ROW_INT52                     source/implicit/ind_glob_k.F  
Chd|        SPMD_ICOL                     source/mpi/implicit/imp_spmd.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_A(N,IC,ID)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IT
C
C----add ID--at end--------------------------
      DO I =1,N
       IF (IC(I)==ID) RETURN
      ENDDO
      N =N+1
      IC(N)=ID
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_A1                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_A1(N,IC,ID)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IT
C
C----add ID--in right order--------------------------
      IT =N+1
      DO I =1,N
       IF (IC(I)==ID) THEN
        RETURN
       ELSEIF (IC(I)>ID) THEN
        IT =I
        GOTO 10
       ENDIF
      ENDDO
 10   IF (IT==1) THEN
       DO I =N,IT,-1
        IC(I+1)=IC(I)
       ENDDO
       IC(IT)=ID
       N = N+1
      ELSEIF (ID/=IC(IT-1)) THEN
       DO I =N,IT,-1
        IC(I+1)=IC(I)
       ENDDO
       IC(IT)=ID
       N = N+1
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_J1                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_J1(N,IC,NI)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),NI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II
C
      II=0
      DO I =1,N
       IF (IC(I)>NI) THEN
        II =II +1
        IC(II)=IC(I)
       ENDIF
      ENDDO
      N = II
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_J                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|        IND_KINE_KP                   source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_J(N,IC,NI,IDDL)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),NI,IDDL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IT,IIC,IMIN,IDIC(N)
C
      II=0
      IT=IDDL(NI)
      DO I =1,N
       IIC=IDDL(IC(I))
       IF (IIC>IT) THEN
        II =II +1
        IC(II)=IC(I)
        IDIC(II)=IIC
       ENDIF
      ENDDO
      N = II
      IF (N==0) RETURN
C-----en ordre iddl croisante-----
      DO I =1,N
       IMIN=IDIC(I)
       II=I
       DO J =I+1,N
        IF (IDIC(J)<IMIN) THEN
         IMIN=IDIC(J)
         II=J
        ENDIF
       ENDDO
       IF (II/=I) THEN
        IT=IC(I)
        IC(I)=IC(II)
        IC(II)=IT
        IT=IDIC(I)
        IDIC(I)=IDIC(II)
        IDIC(II)=IT
       ENDIF
      ENDDO
C----delete doubles----------------------------
C      II=1
C      DO I =2,N
C       IF (IC(I)/=IC(I-1)) THEN
C        II =II +1
C        IC(II)=IC(I)
C       ENDIF
C      ENDDO
C      N = II
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_L                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_FR_K0                     source/mpi/implicit/imp_fri.F 
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|        IND_KINE_KP                   source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_L(N,IC,NI,IDDL)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),NI,IDDL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IT,IIC,IMIN,IDIC(N)
C
      II=0
      IT=IDDL(NI)
      DO I =1,N
       IIC=IDDL(IC(I))
       IF (IIC<IT) THEN
        II =II +1
        IC(II)=IC(I)
        IDIC(II)=IIC
       ENDIF
      ENDDO
      N = II
      IF (N==0) RETURN
C-----en ordre iddl croisante-----
      DO I =1,N
       IMIN=IDIC(I)
       II=I
       DO J =I+1,N
        IF (IDIC(J)<IMIN) THEN
         IMIN=IDIC(J)
         II=J
        ENDIF
       ENDDO
       IF (II/=I) THEN
        IT=IC(I)
        IC(I)=IC(II)
        IC(II)=IT
        IT=IDIC(I)
        IDIC(I)=IDIC(II)
        IDIC(II)=IT
       ENDIF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INTAB                         source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DEGENES8                      source/elements/solid/solide/sdlen3.F
Chd|        DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|        DIM_KINE_S                    source/implicit/ind_glob_k.F  
Chd|        DIM_KINE_T                    source/implicit/ind_glob_k.F  
Chd|        DIM_NDOF_I                    source/implicit/ind_glob_k.F  
Chd|        IMP_ICOMCRIT                  source/implicit/imp_int_k.F   
Chd|        IND_KINE_I                    source/implicit/ind_glob_k.F  
Chd|        IND_KINE_K                    source/implicit/ind_glob_k.F  
Chd|        INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      LOGICAL FUNCTION INTAB(NIC,IC,N)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,NIC,IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C----6---------------------------------------------------------------7---------8
       INTAB=.FALSE.
       DO I =1,NIC
        IF (N==IC(I)) THEN
         INTAB=.TRUE.
         RETURN
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DIM_INT7                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IMP_INTBUF_INI                source/implicit/imp_solv.F    
Chd|        INTBUF_TAB_C_INI              ../common_source/interf/copy_intbuf_tab.F
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DIM_INT7(
     1    NINTER    ,IPARI     ,INTBUF_TAB  ,NNMAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTBUF
      USE MESSAGE_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "tabsiz_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NINTER,NNMAX
C     REAL
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,NTY,NUM_IMP,NSN,NMN,NRTS,ILEV,NOINT
      INTEGER I,J,K,L,NDOFI,N,IAD,IERR,STAT,NIMP(NINTER)
      my_real
     .  STARTT,STOPT
C-----------------------------------------------
C------interface -------------- 
      NNMAX=0
      DO NIN=1,NINTER
       NSN   =IPARI(5,NIN)
       NMN   =IPARI(6,NIN)
       NTY   =IPARI(7,NIN)
       NIMP(NIN) = 0
C----------deleted int NTY->0       
       IF (NTY ==0 ) CYCLE
	   IF (NTY ==2 ) THEN
        ILEV   =IPARI(20,NIN)
        NOINT =IPARI(15,NIN)
        IF (ILEV>=10.AND.ILEV<=25) THEN
         CALL ANCMSG(MSGID=241,ANMODE=ANINFO,I1=ILEV,I2=NOINT )
         CALL ARRET(2)
        END IF
       ELSEIF (NTY/=5 .AND. NTY/=7 .AND. NTY/=10
     .         .AND. NTY/=11 .AND. NTY/=24) THEN
       STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
       STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
       IF(STARTT<TSTOP)
     .   CALL ANCMSG(MSGID=232,ANMODE=ANINFO,I1=NTY )
	   END IF
C-----as int5 uses only ISPMD=0	; some values are not initialized w/ ISPMD/=0   
       IF (ISPMD/=0.AND.(NTY<7.OR.NTY==8
     .                     .OR.NTY==14.OR.NTY==15)) CYCLE
       STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
       STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
       IF(STARTT<=TSTOP) THEN
        IF(NTY==3)THEN
        ELSEIF(NTY==4)THEN
        ELSEIF(NTY==5)THEN
         NNMAX=NNMAX+NSN
        ELSEIF(NTY==6)THEN

        ELSEIF(NTY==7.OR.NTY==10.OR.NTY==24)THEN
         NUM_IMP  = IPARI(18,NIN)*IPARI(23,NIN)
         NNMAX=NNMAX+NUM_IMP
C--------exceptionaly to deactivate kg         
         IF(NTY==24.AND.IIKGOFF==0.AND.IKG==0)THEN
          IIKGOFF = 1
         END IF
C-------dispense i24disk---         
         IF(NTY==24) NIMP(NIN) = NUM_IMP
C
        ELSEIF(NTY==11)THEN
         NUM_IMP  = IPARI(18,NIN)*IPARI(23,NIN)
         NNMAX=NNMAX+NUM_IMP
C
        ENDIF
       ENDIF
      ENDDO
C-----------Allocate INTBUF_TAB_CP for implicit
      ALLOCATE (INTBUF_TAB_CP(NINTER), STAT=Stat)
      CALL INTBUF_TAB_C_INI(INTBUF_TAB, INTBUF_TAB_CP)
C-----------Allocate IMP_INTBUF_TAB for implicit
      ALLOCATE (INTBUF_TAB_IMP(NINTER), STAT=Stat)
      CALL IMP_INTBUF_INI(INTBUF_TAB_IMP, NIMP)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|        NDDL_LOC                      source/implicit/ind_glob_k.F  
Chd|        ROW_INT                       source/implicit/ind_glob_k.F  
Chd|        ROW_INT11                     source/implicit/ind_glob_k.F  
Chd|        ROW_INT24                     source/implicit/ind_glob_k.F  
Chd|        ROW_INT5                      source/implicit/ind_glob_k.F  
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_INT_K(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    LNSS      ,NINT2     ,IINT2     ,IAINT2    ,LNSS2     ,
     4    NDDL      ,NNZK      ,IDDL      ,ILOCI     ,N_IMPN    ,
     5    N_IMPM    ,NNMAX     ,NKMAX     ,NDOF      ,NSREM     ,
     6    IRBE3     ,LRBE3     ,LNSS3     ,IRBE2     ,LRBE2     ,
     7    LNSB2     ,LNSRB2    ,IND_SUBT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE IMP_INTBUF
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
     .        NS_IMP(*),NE_IMP(*),ILOCI(*),NDOF(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        LNSS,NINT2,IINT2(*),IAINT2(*),LNSS2,NSREM
      INTEGER
     .   NDDL,IDDL(*) ,NNZK,N_IMPN,N_IMPM,NNMAX  ,NKMAX
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),LNSS3,IRBE2(NRBE2L,*),LRBE2(*),
     .        LNSB2,LNSRB2,IND_SUBT(*)
C     REAL

      TYPE (INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,NTY,NDOF1(NUMNOD),NSN
      INTEGER I,J,K,L,NDOFI,N,IAD,N_IMP,
     .        NRTS
C-----------------------------------------------
C------interface ----iddl used for nrow firstly---------- 
      NDDL =0
      NDOFI=3
      DO N =1,NUMNOD
       IDDL(N)=0
       ILOCI(N)=0
       NDOF1(N)=NDOFI
      ENDDO
C
      IAD=1
      N_IMP=0
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
        CALL ROW_INT5(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),INTBUF_TAB(NIN)%IRECTM,
     .               INTBUF_TAB(NIN)%NSV, INTBUF_TAB(NIN)%MSR,IDDL  ,ILOCI ,NDOFI,N_IMP ,
     .               NSN   ,NSREM  )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
C      IAD=1
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
C       MULTIMP=MAX(MULTIMP,IPARI(23,NIN))
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
       ELSEIF(NTY==6)THEN

       ELSEIF(NTY==7.OR.NTY==10)THEN
C
        CALL ROW_INT(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),INTBUF_TAB(NIN)%IRECTM,
     .               INTBUF_TAB(NIN)%NSV, IDDL     ,ILOCI   ,NDOFI,N_IMP  ,
     .               NSN   ,NSREM  )
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==24)THEN
C
c        CALL ROW_INT24(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),INTBUF_TAB(NIN)%IRECTM,
c     .               INTBUF_TAB(NIN)%NSV, IDDL     ,ILOCI   ,NDOFI,N_IMP  ,
c     .               NSN   ,NSREM  ,IND_SUBT,INTBUF_TAB(NIN)%NVOISIN)
        CALL ROW_INT24(INTBUF_TAB_IMP(NIN)%I_STOK(1),INTBUF_TAB_IMP(NIN)%CAND_N,
     .                 INTBUF_TAB_IMP(NIN)%CAND_E,INTBUF_TAB(NIN)%IRECTM,
     .               INTBUF_TAB(NIN)%NSV, IDDL     ,ILOCI   ,NDOFI,N_IMP  ,
     .               NSN   ,NSREM  ,INTBUF_TAB_IMP(NIN)%INDSUBT,
     .               INTBUF_TAB(NIN)%NVOISIN)
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==11)THEN
C
        NRTS   =IPARI(3,NIN)
        CALL ROW_INT11(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                 INTBUF_TAB(NIN)%IRECTS,INTBUF_TAB(NIN)%IRECTM, IDDL ,ILOCI,
     .                 NDOFI,N_IMP  ,NRTS        ,NSREM  )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
      NNMAX=0
      DO N =1,NUMNOD
       IF (ILOCI(N)>0) THEN
        NNMAX=MAX(NNMAX,IDDL(N))
        IDDL(N)=0
       ENDIF
      ENDDO
C-----couplage avec cond. kine ------
      N_IMPM=N_IMP
      CALL DIM_KINE_I(
     1    NUM_IMP   ,NS_IMP    ,NE_IMP    ,NPBY      ,LPBY      ,
     2    ITAB      ,NRBYAC    ,IRBYAC    ,NINT2     ,IINT2     ,
     3    IPARI     ,INTBUF_TAB,LNSS      ,LNSS2     ,IDDL      ,
     4    N_IMP     ,ILOCI     ,NNMAX     ,N_IMPM    ,NDOF      ,
     5    NDOF1     ,IAINT2    ,IRBE3     ,LRBE3     ,LNSS3     ,
     6    IRBE2     ,LRBE2     ,LNSB2     ,LNSRB2    ,IND_SUBT )
c-----1. calcule NNMAX,NKMAX;
      N_IMPN=N_IMP-N_IMPM
      NNMAX=0
      NKMAX=0
      DO N =1,NUMNOD
       IF (ILOCI(N)>0) THEN
        IF (ILOCI(N)>N_IMPN) THEN
         NKMAX=MAX(NKMAX,IDDL(N))
        ELSE
         NNMAX=MAX(NNMAX,IDDL(N))
         NDOF1(N)=MAX(3,NDOF1(N))
        ENDIF
       ENDIF
      ENDDO
c-----2. calcule NNZK;
      NNZK = 0
      DO N =1,NUMNOD
       IF (ILOCI(N)>0) THEN
        DO K=1,NDOF1(N)
C-------termes knn-------
         DO J=1,NDOF1(N)
          IF (J/=K) NNZK = NNZK+1
         ENDDO
C-------termes kn,nj-------
         DO J=1,IDDL(N)
          DO L=1,NDOF1(N)
           NNZK = NNZK+1
          ENDDO
         ENDDO
        ENDDO
       ENDIF
      ENDDO
      NNZK = NNZK/2+1
c      write(*,*)'int NNMAX,NKMAX,N_IMP=',NNMAX,NKMAX,N_IMP
c-----3. calcul NDDL,IDDL;
      CALL NDDL_LOC(NDDL,IDDL,ILOCI,N_IMP,NDOF1)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- stockage M.C.R.S + SYM- ----- 
Chd|====================================================================
Chd|  IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IND_KINE_I                    source/implicit/ind_glob_k.F  
Chd|        REORDER_J                     source/implicit/ind_glob_k.F  
Chd|        REORDER_L                     source/implicit/ind_glob_k.F  
Chd|        ROW_INT112                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT2                      source/implicit/ind_glob_k.F  
Chd|        ROW_INT242                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT52                     source/implicit/ind_glob_k.F  
Chd|        SET_IND_K                     source/implicit/ind_glob_k.F  
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_INT_K(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP      ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC      ,
     3    NSS       ,ISS       ,NINT2     ,IINT2     ,NSS2        ,
     4    ISS2      ,NDDLI     ,NNZI      ,IADI      ,JDII        ,
     5    IDDLI     ,ILOCI     ,N_IMPN    ,ITOK      ,IDDL        ,
     6    NNMAX     ,NKMAX     ,N_IMPM    ,NDOF      ,IAINT2      ,
     7    IRBE3     ,LRBE3     ,NSS3      ,ISS3      ,IRBE2       ,
     8    LRBE2     ,NSB2      ,ISB2      ,IND_SUBT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE IMP_INTBUF
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
#include      "com10_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUM_IMP(*),IPARI(NPARI,*),IND_SUBT(*),
     .        NS_IMP(*),NE_IMP(*) ,NDOF(*),IAINT2(*),LRB,LI2
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NSS(*),ISS(*),NINT2,IINT2(*),NSS2(*),ISS2(*)
      INTEGER
     .   IDDL(*),IADI(*),JDII(*),IDDLI(*),ITOK(*),
     .   ILOCI(*),NDDLI ,NNZI,NNMAX,N_IMPN,N_IMPM,NKMAX
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),NSS3(*),ISS3(*),
     .        IRBE2(NRBE2L,*),LRBE2(*),NSB2(*),ISB2(*)
C     REAL

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C=======================================================================
C     stockage M.C.R.S (Modified Compressed Row Stockage)
C     [K] pour interfaces : diagonale + trangle sup en lignes:
C        [K](id,jd)     -> DIAG(NDDL)+LT(IK)(exclue diag)
C        id = 1..nddl        : ID = IADI(ID)...IADI(ID+1)-1
C        jd = 1..NNZI        : JD = JDII(IK)
C     IADI(NDDLI+1)
C     JDII(NNZK) 
C     ITOK(NDDLI) : L'indice de [K] global:NDDLI->NDDL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NROW(N_IMPN+N_IMPM),ICOL(NNMAX,N_IMPN),
     .        NDOF1(N_IMPN+N_IMPM),ICOK(NKMAX,N_IMPM),IKP
      INTEGER I,J,K,L,N,KD(50), JFI, KFI,NDOFI,ND,N_IMPT,
     .        NTY,NL,NJ,NIN,LENK,IAD,ILOC(N_IMPN+N_IMPM),
     .        NSN,NRTS
C-----------------------------------------------
C------ILOCI est ILOC dans IND_GLOB_K -------------- 
C
      IKP=IKPAT
      NDOFI=3
      ND=0
      N_IMPT=N_IMPN+N_IMPM
      DO N =1,NUMNOD
       IF (ILOCI(N)>0) THEN
        I=ILOCI(N)
        ILOC(I)=N
        NDOF1(I)=NDOFI
       ENDIF
      ENDDO
      DO N =1,N_IMPT
       NROW(N)=0
      ENDDO
C
      IAD=1
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
        CALL ROW_INT52(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,
     .                INTBUF_TAB(NIN)%MSR,NROW  ,
     .                N_IMPN,ILOCI   ,ICOL     ,NNMAX   ,ICOK   ,
     .                NKMAX ,NSN     )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
C      IAD=1
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
       ELSEIF(NTY==6)THEN

       ELSEIF(NTY==7.OR.NTY==10)THEN
C
        CALL ROW_INT2(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,NROW   ,N_IMPN,
     .                ILOCI   ,ICOL     ,NNMAX   ,ICOK   ,NKMAX ,
     .                NSN     )
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==24)THEN
C
c        CALL ROW_INT242(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
c     .                INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,NROW   ,N_IMPN,
c     .                ILOCI   ,ICOL     ,NNMAX   ,ICOK   ,NKMAX ,
c     .                NSN     ,IND_SUBT,INTBUF_TAB(NIN)%NVOISIN)
        CALL ROW_INT242(INTBUF_TAB_IMP(NIN)%I_STOK(1),INTBUF_TAB_IMP(NIN)%CAND_N,
     .                 INTBUF_TAB_IMP(NIN)%CAND_E,INTBUF_TAB(NIN)%IRECTM,
     .                 INTBUF_TAB(NIN)%NSV,NROW   ,N_IMPN,
     .                ILOCI   ,ICOL     ,NNMAX   ,ICOK   ,NKMAX ,
     .                NSN     ,INTBUF_TAB_IMP(NIN)%INDSUBT,
     .               INTBUF_TAB(NIN)%NVOISIN)
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==11)THEN
C
        NRTS   =IPARI(3,NIN)
        CALL ROW_INT112(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                 INTBUF_TAB(NIN)%IRECTS ,INTBUF_TAB(NIN)%IRECTM, NROW   ,N_IMPN,
     .                 ILOCI   ,ICOL     ,NNMAX   ,ICOK   ,NKMAX ,
     .                 NRTS    )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
       CALL IND_KINE_I(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NSS       ,ISS       ,NINT2     ,IINT2     ,IPARI     ,
     3    INTBUF_TAB,NSS2      ,ISS2      ,NNMAX     ,ILOCI     ,
     4    NKMAX     ,NROW      ,ICOL      ,ICOK      ,N_IMPN    ,
     5    NDOF      ,NDOF1     ,IAINT2    ,IRBE3     ,LRBE3     ,
     6    NSS3      ,ISS3      ,IRBE2     ,LRBE2     ,NSB2      ,
     7    ISB2      )
      IF (IKP==0) THEN
       DO I =1,N_IMPN
        N=ILOC(I)
        CALL REORDER_J(NROW(I),ICOL(1,I),N,IDDLI)
       ENDDO
       DO I =N_IMPN+1,N_IMPT
        N=ILOC(I)
        NJ=I-N_IMPN
        CALL REORDER_J(NROW(I),ICOK(1,NJ),N,IDDLI)
       ENDDO
      ELSE
       DO I =1,N_IMPN
        N=ILOC(I)
        CALL REORDER_L(NROW(I),ICOL(1,I),N,IDDLI)
       ENDDO
       DO I =N_IMPN+1,N_IMPT
        N=ILOC(I)
        NJ=I-N_IMPN
        CALL REORDER_L(NROW(I),ICOK(1,NJ),N,IDDLI)
       ENDDO
      ENDIF
C-----revinir ndof---
      DO I =1,N_IMPN
       NDOF1(I)=MAX(3,NDOF1(I))
      ENDDO
      DO I =1,N_IMPT
        N=ILOC(I)
        ILOCI(N)=NDOF1(I)
      ENDDO
      ND =0
      LENK = 0
      NL = 1
      IADI(NL) = 1
      DO I =1,N_IMPN
        N=ILOC(I)
        DO K=1,NDOF1(I)
         ND = ND + 1
         ITOK(ND)=IDDL(N)+K
        ENDDO
        CALL SET_IND_K(
     1     IDDLI     ,ILOCI     ,IADI      ,JDII      ,NL        ,
     2     LENK      ,NROW(I)  ,ICOL(1,I)  ,N         ,IKP       )
      ENDDO
      DO I =N_IMPN+1,N_IMPT
        N=ILOC(I)
        NJ=I-N_IMPN
        DO K=1,NDOF1(I)
         ND = ND + 1
         ITOK(ND)=IDDL(N)+K
        ENDDO
        CALL SET_IND_K(
     1     IDDLI     ,ILOCI     ,IADI      ,JDII      ,NL        ,
     2     LENK      ,NROW(I)  ,ICOK(1,NJ) ,N         ,IKP       )
      ENDDO
        IF (LENK>NNZI.OR.NL/=(NDDLI+1))
     .        WRITE(*,*)'--MEMERY PROBLEM 5-- :',LENK,NNZI,NL,NDDLI+1
      NNZI = LENK
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  NDDL_LOC                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NDDL_LOC(
     1    NDDL      ,IDDL      ,ILOC      ,NLOC        ,NDOF       )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL ,IDDL(*)  ,ILOC(*) ,NLOC,NDOF(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,LOCI(NLOC)
C-----------------------------------------------
      NDDL=0
      DO N = 1, NUMNOD
       IF (ILOC(N)>0) THEN
        I=ILOC(N)
        LOCI(I)=N
       ENDIF
       IDDL(N)=NDDL
      ENDDO
      DO I=1,NLOC
       N=LOCI(I)
       IDDL(N)=NDDL
       NDDL = NDDL + NDOF(N)
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT                       source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROW_INT(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT    ,NSV       ,
     2    NROW      ,ILOC      ,NDOFI       ,N_IMPN   ,NSN       ,
     3    NSREM     )
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
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),NDOFI,N_IMPN,NSN,NSREM
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        IF (ILOC(N1)==0) THEN
         N_IMPN=N_IMPN+1
         ILOC(N1)=N_IMPN
        ENDIF
        NROW(N1)=NROW(N1)+3
       ELSE
        NSREM=NSREM+1
       ENDIF
        NE=NE_IMP(I)
        DO J=1,3
         N=IRECT(J,NE)
         IF (ILOC(N)==0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
         NROW(N)=NROW(N)+1
        ENDDO
        IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
         N=IRECT(4,NE)
         IF (ILOC(N)==0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
         NROW(N)=NROW(N)+1
         IF (IG<=NSN) NROW(N1)=NROW(N1)+1
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT1                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT1(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NSV        ,
     2    NROW      ,N_IMPN    ,ILOC      ,ICOL        ,NNMAX      ,
     3    NSN       )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),ICOL(NNMAX,*),N_IMPN,NSN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        NE=NE_IMP(I)
        NI=ILOC(N1)
         DO J=1,3
          N=IRECT(J,NE)
          N2=ILOC(N)
          CALL REORDER_A(NROW(N1),ICOL(1,NI),N)
          CALL REORDER_A(NROW(N),ICOL(1,N2),N1)
         ENDDO
         IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
          N=IRECT(4,NE)
          N2 =ILOC(N)
          CALL REORDER_A(NROW(N1),ICOL(1,NI),N)
          CALL REORDER_A(NROW(N),ICOL(1,N2),N1)
         ENDIF
       ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT2                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT2(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NSV        ,
     2    NROW      ,N_IMPN    ,ILOC      ,ICOL        ,NNMAX      ,
     3    ICOK      ,NKMAX     ,NSN       )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),ICOL(NNMAX,*),ICOK(NKMAX,*),N_IMPN,NSN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI,NM,NIM
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        NE=NE_IMP(I)
        NI=ILOC(N1)
        IF (NI<=N_IMPN) THEN
         DO J=1,3
          N=IRECT(J,NE)
          N2=ILOC(N)
          CALL REORDER_A(NROW(NI),ICOL(1,NI),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDDO
         IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
          N=IRECT(4,NE)
          N2 =ILOC(N)
          CALL REORDER_A(NROW(NI),ICOL(1,NI),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDIF
        ELSE
         NIM=NI-N_IMPN
         DO J=1,3
          N=IRECT(J,NE)
          N2=ILOC(N)
          CALL REORDER_A(NROW(NI),ICOK(1,NIM),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDDO
         IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
          N=IRECT(4,NE)
          N2 =ILOC(N)
          CALL REORDER_A(NROW(NI),ICOK(1,NIM),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDIF
        ENDIF
       END IF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT5                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROW_INT5(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT    ,NSV       ,
     2    MSR       ,NROW      ,ILOC      ,NDOFI       ,N_IMPN   ,
     3    NSN       ,NSREM     )
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
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        MSR(*),ILOC(*),NDOFI,N_IMPN,NSN,NSREM
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
        N1 = NSV(IG)
        IF (ILOC(N1)==0) THEN
         N_IMPN=N_IMPN+1
         ILOC(N1)=N_IMPN
        ENDIF
        NROW(N1)=NROW(N1)+3
        NE=NE_IMP(I)
        DO J=1,3
         N=MSR(IRECT(J,NE))
         IF (ILOC(N)==0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
         NROW(N)=NROW(N)+1
        ENDDO
        IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
         N=MSR(IRECT(4,NE))
         IF (ILOC(N)==0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
         NROW(N)=NROW(N)+1
         IF (IG<=NSN) NROW(N1)=NROW(N1)+1
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT51                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT51(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NSV        ,
     2    MSR       ,NROW      ,N_IMPN    ,ILOC       ,ICOL        ,
     3    NNMAX     ,NSN       )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),ICOL(NNMAX,*),N_IMPN,NSN,MSR(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
        N1 = NSV(IG)
        NE=NE_IMP(I)
        NI=ILOC(N1)
         DO J=1,3
          N=MSR(IRECT(J,NE))
          N2=ILOC(N)
          CALL REORDER_A(NROW(N1),ICOL(1,NI),N)
          CALL REORDER_A(NROW(N),ICOL(1,N2),N1)
         ENDDO
         IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
          N=MSR(IRECT(4,NE))
          N2 =ILOC(N)
          CALL REORDER_A(NROW(N1),ICOL(1,NI),N)
          CALL REORDER_A(NROW(N),ICOL(1,N2),N1)
         ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT52                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT52(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NSV        ,
     2    MSR       ,NROW      ,N_IMPN    ,ILOC        ,ICOL       ,
     3    NNMAX     ,ICOK      ,NKMAX     ,NSN       )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),ICOL(NNMAX,*),ICOK(NKMAX,*),N_IMPN,NSN,MSR(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI,NM,NIM
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
        N1 = NSV(IG)
        NE=NE_IMP(I)
        NI=ILOC(N1)
        IF (NI<=N_IMPN) THEN
         DO J=1,3
          N=MSR(IRECT(J,NE))
          N2=ILOC(N)
          CALL REORDER_A(NROW(NI),ICOL(1,NI),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDDO
         IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
          N=MSR(IRECT(4,NE))
          N2 =ILOC(N)
          CALL REORDER_A(NROW(NI),ICOL(1,NI),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDIF
        ELSE
         NIM=NI-N_IMPN
         DO J=1,3
          N=MSR(IRECT(J,NE))
          N2=ILOC(N)
          CALL REORDER_A(NROW(NI),ICOK(1,NIM),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDDO
         IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
          N=MSR(IRECT(4,NE))
          N2 =ILOC(N)
          CALL REORDER_A(NROW(NI),ICOK(1,NIM),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDIF
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROW_INT24                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        I24MSEGV                      source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT24(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT    ,NSV       ,
     2    NROW      ,ILOC      ,NDOFI       ,N_IMPN   ,NSN       ,
     3    NSREM     ,SUBTRIA   ,NVOISIN     )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),NDOFI,N_IMPN,NSN,NSREM,SUBTRIA(*),NVOISIN(8,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,IRTLM(4),NEI
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        IF (ILOC(N1)==0) THEN
         N_IMPN=N_IMPN+1
         ILOC(N1)=N_IMPN
        ENDIF
        NROW(N1)=NROW(N1)+3
C
       ELSE
        NSREM=NSREM+1
       ENDIF
        NE=NE_IMP(I)
        IF (NE<0) THEN
         NEI=-NE
         CALL I24MSEGV(NE,IRTLM ,SUBTRIA(I),IRECT(1,NEI),NVOISIN(1,NEI))
        ELSE
         IRTLM(1:4) = IRECT(1:4,NE)
        END IF
        DO J=1,3
         N=IRTLM(J)
         IF (ILOC(N)==0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
         NROW(N)=NROW(N)+1
        ENDDO
        IF (IRTLM(3)/=IRTLM(4)) THEN
         N=IRTLM(4)
         IF (ILOC(N)==0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
         NROW(N)=NROW(N)+1

         IF (IG<=NSN) NROW(N1)=NROW(N1)+1
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT241                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        I24MSEGV                      source/implicit/ind_glob_k.F  
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT241(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NSV        ,
     2    NROW      ,N_IMPN    ,ILOC      ,ICOL        ,NNMAX      ,
     3    NSN       ,SUBTRIA   ,NVOISIN     )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),ICOL(NNMAX,*),N_IMPN,NSN,SUBTRIA(*),NVOISIN(8,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI,IRTLM(4),NEI
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        NE=NE_IMP(I)
        IF (NE<0) THEN
         NEI=-NE
         CALL I24MSEGV(NE,IRTLM ,SUBTRIA(I),IRECT(1,NEI),NVOISIN(1,NEI))
        ELSE
         IRTLM(1:4) = IRECT(1:4,NE)
        END IF
        NI=ILOC(N1)
         DO J=1,3
          N=IRTLM(J)
          N2=ILOC(N)
          CALL REORDER_A(NROW(N1),ICOL(1,NI),N)
          CALL REORDER_A(NROW(N),ICOL(1,N2),N1)
         ENDDO
         IF (IRTLM(3)/=IRTLM(4)) THEN
          N=IRTLM(4)
          N2 =ILOC(N)
          CALL REORDER_A(NROW(N1),ICOL(1,NI),N)
          CALL REORDER_A(NROW(N),ICOL(1,N2),N1)
         ENDIF
       ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C   --------- ---- 
Chd|====================================================================
Chd|  ROW_INT242                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        I24MSEGV                      source/implicit/ind_glob_k.F  
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT242(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NSV        ,
     2    NROW      ,N_IMPN    ,ILOC      ,ICOL        ,NNMAX      ,
     3    ICOK      ,NKMAX     ,NSN       ,SUBTRIA   ,NVOISIN     )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),NROW(*),
     .        ILOC(*),ICOL(NNMAX,*),ICOK(NKMAX,*),N_IMPN,NSN,
     .        SUBTRIA(*),NVOISIN(8,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI,NM,NIM,IRTLM(4),NEI
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        NE=NE_IMP(I)
        NI=ILOC(N1)
        IF (NE<0) THEN
         NEI=-NE
         CALL I24MSEGV(NE,IRTLM ,SUBTRIA(I),IRECT(1,NEI),NVOISIN(1,NEI))
        ELSE
         IRTLM(1:4) = IRECT(1:4,NE)
        END IF
        IF (NI<=N_IMPN) THEN
         DO J=1,3
          N=IRTLM(J)
          N2=ILOC(N)
          CALL REORDER_A(NROW(NI),ICOL(1,NI),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDDO
         IF (IRTLM(3)/=IRTLM(4)) THEN
          N=IRTLM(4)
          N2 =ILOC(N)
          CALL REORDER_A(NROW(NI),ICOL(1,NI),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDIF
        ELSE
         NIM=NI-N_IMPN
         DO J=1,3
          N=IRTLM(J)
          N2=ILOC(N)
          CALL REORDER_A(NROW(NI),ICOK(1,NIM),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDDO
         IF (IRTLM(3)/=IRTLM(4)) THEN
          N=IRTLM(4)
          N2 =ILOC(N)
          CALL REORDER_A(NROW(NI),ICOK(1,NIM),N)
          IF (N2<=N_IMPN) THEN
           CALL REORDER_A(NROW(N2),ICOL(1,N2),N1)
          ELSE
           NM=N2- N_IMPN
           CALL REORDER_A(NROW(N2),ICOK(1,NM),N1)
          ENDIF
         ENDIF
        ENDIF
       END IF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        ROW_INT1                      source/implicit/ind_glob_k.F  
Chd|        ROW_INT111                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT241                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT51                     source/implicit/ind_glob_k.F  
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINE_I(
     1    NUM_IMP   ,NS_IMP    ,NE_IMP    ,NPBY      ,LPBY      ,
     2    ITAB      ,NRBYAC    ,IRBYAC    ,NINT2     ,IINT2     ,
     3    IPARI     ,INTBUF_TAB,LNSS      ,LNSS2     ,NROW      ,
     4    NKINE     ,INLOC     ,NNMAX     ,N_IMPM    ,NDOF      ,
     5    NDOFI     ,IAINT2    ,IRBE3     ,LRBE3     ,LNSS3     ,
     6    IRBE2     ,LRBE2     ,LNSB2     ,LNSRB2    ,IND_SUBT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE IMP_INTBUF
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
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKINE,N_IMPM
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),
     .        NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*)
      INTEGER NINT2,IINT2(*),IPARI(NPARI,*),IAINT2(*),
     .        INLOC(*),LNSS ,LNSS2,NROW(*),NDOF(*),NDOFI(*)
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),LNSS3,IRBE2(NRBE2L,*),
     .        LRBE2(*),LNSB2,LNSRB2,IND_SUBT(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IAD,NTY,NIN,KD(50),NKE,NKE2
      INTEGER IA(NRBYAC),NROW1(NUMNOD)
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .        JI,K10,K11,K12,K13,K14,KFI,NS,NNOD,NM,L1,NL1,NM1,
     .        NRTS,NKINE0,NMAX,NKE1,M1,IC
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ICOK,ICOK1
C-----------------------------------------------
      NKINE0 = NKINE
      IF (NKINE0>0) ALLOCATE(ICOK(NNMAX,NKINE0))
C
      DO I=1,NUMNOD
       NROW1(I) = 0
      ENDDO
C
      IAD=1
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
        CALL ROW_INT51(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,
     .                INTBUF_TAB(NIN)%MSR,NROW ,
     .                NKINE ,INLOC   ,ICOK     ,NNMAX   ,NSN     )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
       ELSEIF(NTY==6)THEN

       ELSEIF(NTY==7.OR.NTY==10)THEN
C
        CALL ROW_INT1(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,NROW   ,NKINE ,
     .                INLOC   ,ICOK     ,NNMAX   ,NSN     )
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==24)THEN
C
c        CALL ROW_INT241(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
c     .                INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,NROW   ,NKINE ,
c     .                INLOC   ,ICOK     ,NNMAX   ,NSN     ,IND_SUBT,
c     .                INTBUF_TAB(NIN)%NVOISIN)
        CALL ROW_INT241(INTBUF_TAB_IMP(NIN)%I_STOK(1),INTBUF_TAB_IMP(NIN)%CAND_N,
     .                 INTBUF_TAB_IMP(NIN)%CAND_E,INTBUF_TAB(NIN)%IRECTM,
     .                INTBUF_TAB(NIN)%NSV,NROW   ,NKINE ,
     .                INLOC   ,ICOK     ,NNMAX   ,NSN     ,
     .                INTBUF_TAB_IMP(NIN)%INDSUBT,INTBUF_TAB(NIN)%NVOISIN)
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==11)THEN
C
        NRTS   =IPARI(3,NIN)
        CALL ROW_INT111(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                 INTBUF_TAB(NIN)%IRECTS ,INTBUF_TAB(NIN)%IRECTM,NROW  ,NKINE ,
     .                 INLOC   ,ICOK     ,NNMAX   ,NRTS    )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
      K=0
      LNSS2=0
      DO J=1,NINT2
       N=IINT2(J)
       IAINT2(J)=0
       NSN = IPARI(5,N)
       JI=IPARI(1,N)
       K10=JI-1
       K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
       K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
       K13=K12+NSN
C------MSR(NMN)-----
       K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
       KFI=K14+NSN
       DO I=1,NSN
        ID = I+K
        NI=INTBUF_TAB(N)%NSV(I)
        IF (INLOC(NI)>0.AND.INLOC(NI)<=NKINE0) THEN
         IAINT2(J)=1
         L=INTBUF_TAB(N)%IRTLM(I)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         DO M=1,NNOD
          NM=INTBUF_TAB(N)%IRECTM(NL+M)
          NDOFI(NM)=NDOF(NM)
          IF (INLOC(NM)==0) THEN
            NKINE=NKINE+1
            INLOC(NM)=NKINE
            NROW1(NM)=NROW1(NM)+ NNOD + NNOD
          ENDIF
         ENDDO
         NKE=INLOC(NI)
         DO N1=1,NROW(NI)
          NJ=ICOK(N1,NKE)
          IF (INLOC(NJ)>0 ) THEN
           LNSS2=LNSS2+1
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=INTBUF_TAB(N)%IRECTM(NL+M)
            IF (INLOC(NM)>0) THEN
             NROW1(NM)=NROW1(NM)+1
             IF (NKE2>0) NROW1(NJ)=NROW1(NJ)+1
            ELSE
             NKINE=NKINE+1
             INLOC(NM)=NKINE
             NROW1(NM)=1
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
       ENDDO
       K=K+NSN
      ENDDO
C-----RBE2------
      LNSB2= 0
      LNSRB2= 0
      DO J=1,NRBE2
       K=IRBE2(1,J)
       M =IRBE2(3,J)
       NSN =IRBE2(5,J)
       LNSRB2= LNSRB2+NSN
       IC = 7*512+7*64-IRBE2(4,J)
        DO I=1,NSN
         NI=LRBE2(I+K)
         IF (INLOC(NI)>0) THEN
          NKE=INLOC(NI)
          DO N1=1,NROW(NI)
           NJ=ICOK(N1,NKE)
           IF (INLOC(NJ)>0.AND.NJ/=NI) NROW1(NJ)=NROW1(NJ)+1+NHRBE2
           LNSB2= LNSB2+1
          ENDDO
	      IF (INLOC(M)==0) THEN
           NKINE=NKINE+1
           INLOC(M)=NKINE
	      ENDIF
          NROW1(M)=NROW1(M)+NROW(NI)+NROW1(NI)
          IF (IC>0) THEN
           NROW1(M)=NROW1(M)+1
           NROW1(NI)=NROW1(NI)+1
          END IF
         ENDIF
C---------for the case with main node--------	 
         LNSB2= LNSB2+1
        ENDDO
        IF (INLOC(M)>0) NDOFI(M)=NDOF(M)
      ENDDO
C------------RBE3-------------
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NI  = IRBE3(3,N)
        IF (NI==0.OR.NDOFI(NI)==0) CYCLE
        NNOD = IRBE3(5,N)
        IF (INLOC(NI)>0) THEN
         DO M=1,NNOD
          NM=LRBE3(IAD+M)
          NDOFI(NM)=NDOF(NM)
          IF (INLOC(NM)==0) THEN
            NKINE=NKINE+1
            INLOC(NM)=NKINE
            NROW1(NM)=NROW1(NM)+ NNOD
          ENDIF
         ENDDO
         NKE=INLOC(NI)
         DO N1=1,NROW(NI)
          NJ=ICOK(N1,NKE)
          IF (INLOC(NJ)>0.AND.
     .         (.NOT.INTAB(NNOD,LRBE3(IAD+1),NJ))) THEN
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
            NROW1(NM)=NROW1(NM)+1
            IF (NKE2>0) NROW1(NJ)=NROW1(NJ)+1
           ENDDO
          ENDIF
         ENDDO
        ENDIF
      ENDDO
C--------------------
      IF (NKINE>NKINE0) THEN
       NMAX = NNMAX
       ALLOCATE(ICOK1(NNMAX,NKINE0))
       DO I=1,NUMNOD
        IF (INLOC(I)>0) THEN
         NMAX=MAX(NMAX,(NROW1(I)+NROW(I)))
         NKE = INLOC(I)
         DO J =1,NROW(I)
          ICOK1(J,NKE) = ICOK(J,NKE)
         ENDDO
        ENDIF
        NROW1(I) = 0
       ENDDO
       DEALLOCATE(ICOK)
       ALLOCATE(ICOK(NMAX,NKINE))
       DO I=1,NUMNOD
        IF (INLOC(I)>0.AND.NROW(I)>0) THEN
         NKE = INLOC(I)
         DO J =1,NROW(I)
          ICOK(J,NKE) = ICOK1(J,NKE)
         ENDDO
        ENDIF
       ENDDO
       DEALLOCATE(ICOK1)
       LNSS2=0
       DO J=1,NINT2
        IF(IAINT2(J)==1) THEN
         N=IINT2(J)
         NSN = IPARI(5,N)
         JI=IPARI(1,N)
         K10=JI-1
         K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
         K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
         K13=K12+NSN
C------MSR(NMN)-----
         K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
         KFI=K14+NSN
         DO I=1,NSN
          NI=INTBUF_TAB(N)%NSV(I)
          IF (INLOC(NI)>0) THEN
           L=INTBUF_TAB(N)%IRTLM(I)
           NL=4*(L-1)
           IF(INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4))THEN
            NNOD=3
           ELSE
            NNOD=4
           ENDIF
           DO M=1,NNOD
            NM=INTBUF_TAB(N)%IRECTM(NL+M)
            NKE1=INLOC(NM)
             DO M1=M+1,NNOD
               NM1=INTBUF_TAB(N)%IRECTM(NL+M1)
               NKE2=INLOC(NM1)
               CALL REORDER_A(NROW(NM),ICOK(1,NKE1),NM1)
               CALL REORDER_A(NROW(NM1),ICOK(1,NKE2),NM)
             ENDDO
           ENDDO
           NKE=INLOC(NI)
           DO N1=1,NROW(NI)
            NJ=ICOK(N1,NKE)
            IF (INLOC(NJ)>0) THEN
             LNSS2=LNSS2+1
             NKE2=INLOC(NJ)
             DO M=1,NNOD
              NM=INTBUF_TAB(N)%IRECTM(NL+M)
              IF (INLOC(NM)>0) THEN
               NKE1=INLOC(NM)
               CALL REORDER_A(NROW(NM),ICOK(1,NKE1),NJ)
               CALL REORDER_A(NROW(NJ),ICOK(1,NKE2),NM)
              ENDIF
             ENDDO
            ENDIF
           ENDDO
          ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDIF
C-----RBE2------
      DO J=1,NRBE2
       K=IRBE2(1,J)
       M =IRBE2(3,J)
       IF (INLOC(M)==0) CYCLE
       NSN =IRBE2(5,J)
       IC = 7*512+7*64-IRBE2(4,J)
       NKE1=INLOC(M)
        DO I=1,NSN
         NI=LRBE2(I+K)
         IF (INLOC(NI)>0) THEN
          NKE=INLOC(NI)
          DO N1=1,NROW(NI)
           NJ=ICOK(N1,NKE)
           NKE2=INLOC(NJ)
C------------case hierarchy w/ RBE3----           
           IF (NKE2>0.AND.NJ/=NI) THEN
            CALL REORDER_A(NROW(M),ICOK(1,NKE1),NJ)
            CALL REORDER_A(NROW(NJ),ICOK(1,NKE2),M)
           END IF
          ENDDO
	      IF (IC>0) THEN
	       CALL REORDER_A(NROW(M),ICOK(1,NKE1),NI)
           CALL REORDER_A(NROW(NI),ICOK(1,NKE),M)
	      END IF
         ENDIF
        ENDDO
      ENDDO
C------------RBE3-------------
      LNSS3=0
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NI  = IRBE3(3,N)
        IF (NI==0.OR.NDOFI(NI)==0) CYCLE
        NNOD = IRBE3(5,N)
        IF (INLOC(NI)>0) THEN
           NKE1=INLOC(NM)
           DO M=1,NNOD
             NM=LRBE3(IAD+M)
             DO M1=M+1,NNOD
              NM1=LRBE3(IAD+M1)
               NKE2=INLOC(NM1)
               CALL REORDER_A(NROW(NM),ICOK(1,NKE1),NM1)
               CALL REORDER_A(NROW(NM1),ICOK(1,NKE2),NM)
             ENDDO
           ENDDO
         NKE=INLOC(NI)
         DO N1=1,NROW(NI)
          NJ=ICOK(N1,NKE)
          IF (INLOC(NJ)>0.AND.
     .         (.NOT.INTAB(NNOD,LRBE3(IAD+1),NJ))) THEN
           LNSS3=LNSS3+1
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
            NKE1=INLOC(NM)
            CALL REORDER_A(NROW(NM),ICOK(1,NKE1),NJ)
            CALL REORDER_A(NROW(NJ),ICOK(1,NKE2),NM)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
      ENDDO
C-----active rigid body main nodes------
      LNSS= 0
      DO J=1,NRBYAC
       IA(J)=0
       N=IRBYAC(J)
       M  =NPBY(1,N)
C
       K=IRBYAC(J+NRBYKIN)
       NSN  =NPBY(2,N)
       IF (INLOC(M)>0) IA(J)=1
        DO I=1,NSN
         ID = I+K
         NI=LPBY(ID)
         IF (INLOC(NI)>0) THEN
          IA(J)=1
          NKE=INLOC(NI)
          DO N1=1,NROW(NI)
           NJ=ICOK(N1,NKE)
           IF (INLOC(NJ)>0) NROW1(NJ)=NROW1(NJ)+1
           LNSS= LNSS+1
          ENDDO
         ENDIF
        ENDDO
      ENDDO
C-----main nodes traitement spec.------
      DO J=1,NRBYAC
       IF (IA(J)==1) THEN
        N=IRBYAC(J)
        M  =NPBY(1,N)
        IF (INLOC(M)>0) THEN
         NKINE=NKINE-1
         DO I=1,NUMNOD
          IF (INLOC(I)>INLOC(M)) INLOC(I)=INLOC(I)-1
         ENDDO
        ENDIF
       ENDIF
      ENDDO
C
      DO I=1,NUMNOD
       NROW(I) = NROW(I)+NROW1(I)
      ENDDO
C      
      N_IMPM=NKINE
      DO J=1,NRBYAC
       N=IRBYAC(J)
       K=IRBYAC(J+NRBYKIN)
       M  =NPBY(1,N)
       NSN  =NPBY(2,N)
       IF (IA(J)==1) THEN
        NDOFI(M)=NDOF(M)
        NKINE=NKINE+1
        INLOC(M)=NKINE
        DO I=1,NSN
         ID = I+K
         NI=LPBY(ID)
         IF (INLOC(NI)>0) THEN
          NROW(M)=NROW(M)+NROW(NI)
         ENDIF
        ENDDO
       ENDIF
      ENDDO
      N_IMPM=NKINE-N_IMPM
      IF (NKINE0>0) DEALLOCATE(ICOK)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IND_KINE_I                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_KINE_I(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NSS       ,ISS       ,NINT2     ,IINT2     ,IPARI     ,
     3    INTBUF_TAB,NSS2      ,ISS2      ,NNMAX     ,INLOC     ,
     4    NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,INK       ,
     5    NDOF      ,NDOF1     ,IAINT2    ,IRBE3     ,LRBE3     ,
     6    NSS3      ,ISS3      ,IRBE2     ,LRBE2     ,NSB2      ,
     7    ISB2      )
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
#include      "scr05_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX,LRB       ,LI2
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NSS(*),ISS(*),NINT2,IINT2(*),
     .        NSS2(*),ISS2(*),IPARI(NPARI,*),NDOF(*),NDOF1(*),
     .        ICOK(NNMAX,*),ICOKM(NKMAX,*),NROWK(*)
      INTEGER
     .   INLOC(*),INK,IAINT2(*)
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),NSS3(*),ISS3(*),
     .        IRBE2(NRBE2L,*),LRBE2(*),NSB2(*),ISB2(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C------ICOK,ICOKM use the same NROWK------
      INTEGER NKE,NKE1,NKE2,IK,NKM1,IAD
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,K1,M1,
     .        JI,K10,K11,K12,K13,K14,KFI,NS,NNOD,NM,L1,NL1,NM1,IC
c----------------------
      K=0
      NS= 0
      DO J=1,NINT2
       IF(IAINT2(J)==1) THEN
       N=IINT2(J)
       NSN = IPARI(5,N)
       JI=IPARI(1,N)
       K10=JI-1
       K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
       K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
       K13=K12+NSN
C------MSR(NMN)-----
       K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
       KFI=K14+NSN
       DO I=1,NSN
        ID = I+K
        NSS2(ID)=0
        NI=INTBUF_TAB(N)%NSV(I)
        IF (INLOC(NI)>0) THEN
         L=INTBUF_TAB(N)%IRTLM(I)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         DO M=1,NNOD
          NM=INTBUF_TAB(N)%IRECTM(NL+M)
          NKE1=INLOC(NM)
          NDOF1(NKE1)=NDOF(NM)
             DO M1=M+1,NNOD
               NM1=INTBUF_TAB(N)%IRECTM(NL+M1)
               NKM1=INLOC(NM1)
               CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NM1)
               CALL REORDER_A(NROWK(NKM1),ICOK(1,NKM1),NM)
             ENDDO
         ENDDO
         NKE=INLOC(NI)
         DO N1=1,NROWK(NKE)
          NJ=ICOK(N1,NKE)
          IF (INLOC(NJ)>0.AND.
     .        (.NOT.INTAB(NSN,INTBUF_TAB(N)%NSV(1),NJ)).
     .   AND.(.NOT.INTAB(NNOD,INTBUF_TAB(N)%IRECTM(NL+1),NJ))) THEN
           J1=NS+NSS2(ID)+1
           ISS2(J1)=NJ
           NSS2(ID)=NSS2(ID)+1
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=INTBUF_TAB(N)%IRECTM(NL+M)
            IF (INLOC(NM)>0) THEN
             NKE1=INLOC(NM)
             CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NJ)
             CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),NM)
            ENDIF
           ENDDO
          ENDIF
         ENDDO
         NS=NS+NSS2(ID)
       ENDIF
       ENDDO
       K=K+NSN
       ENDIF
      ENDDO
C-----RBE2------
      K=0
      DO N=1,NRBE2
       K1=IRBE2(1,N)
       M  =IRBE2(3,N)
       NSN  =IRBE2(5,N)
       IC = 7*512+7*64-IRBE2(4,N)
       IF (INLOC(M)>0) THEN
        NKE1=INLOC(M)
        NDOF1(NKE1)=NDOF(M)
        NKM1=NKE1
        DO I=1,NSN
         ID = I+K1
         NI=LRBE2(ID)
         NSB2(ID)=0
         IF (INLOC(NI)>0) THEN
          NKE=INLOC(NI)
          DO N1=1,NROWK(NKE)
           NJ=ICOK(N1,NKE)
           NKE2=INLOC(NJ)
           IF (INLOC(NJ)>0.AND.NJ/=NI) THEN
            CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NJ)
            IF (NKE2<=INK) THEN
             CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),M)
            ELSE
             CALL REORDER_A(NROWK(NKE2),ICOKM(1,NKE2-INK),M)
            END IF
             K=K+1
             ISB2(K)=NJ
             NSB2(ID)=NSB2(ID)+1
           END IF
          END DO
	      IF (IC>0) THEN
	       CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NI)
           CALL REORDER_A(NROWK(NKE),ICOK(1,NKE),M)
	      END IF
         END IF
        END DO
       END IF
      END DO
c---------RBE3-------------
      K = 0
      DO N=1,NRBE3
        IAD = IRBE3(1,N)
        NI  = IRBE3(3,N)
        NSS3(N)= 0
        IF (NI==0) CYCLE
        NNOD = IRBE3(5,N)
        IF (INLOC(NI)>0) THEN
         DO M=1,NNOD
          NM=LRBE3(IAD+M)
          NKE1=INLOC(NM)
          NDOF1(NKE1)=NDOF(NM)
             DO M1=M+1,NNOD
              NM1=LRBE3(IAD+M1)
               NKM1=INLOC(NM1)
               IF (NKE1<=INK) THEN
                CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NM1)
               ELSE
                CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKE1-INK),NM1)
               ENDIF

               IF (NKM1<=INK) THEN
                CALL REORDER_A(NROWK(NKM1),ICOK(1,NKM1),NM)
               ELSE
                CALL REORDER_A(NROWK(NKM1),ICOKM(1,NKM1-INK),NM)
               ENDIF
             ENDDO
         ENDDO
         NKE=INLOC(NI)
         DO N1=1,NROWK(NKE)
          IF (NKE<=INK) THEN
           NJ=ICOK(N1,NKE)
          ELSE
           NJ=ICOKM(N1,NKE-INK)
          END IF
          IF (INLOC(NJ)>0.AND.
     .         (.NOT.INTAB(NNOD,LRBE3(IAD+1),NJ))) THEN
           NSS3(N)= NSS3(N)+1
           K= K+1
           ISS3(K)=NJ
           NKE2=INLOC(NJ)
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
            IF (INLOC(NM)>0) THEN
             NKE1=INLOC(NM)
             IF (NKE1<=INK) THEN
              CALL REORDER_A(NROWK(NKE1),ICOK(1,NKE1),NJ)
	         ELSE
              CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKE1-INK),NJ)
	         ENDIF
	
             IF (NKE2<=INK) THEN
              CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),NM)
	         ELSE
              CALL REORDER_A(NROWK(NKE2),ICOKM(1,NKE2-INK),NM)
	         ENDIF
            ENDIF
           ENDDO
          ENDIF
         ENDDO
       ENDIF
      ENDDO
C-----active rigid body main nodes------
      K=0
      NS= 0
      DO J=1,NRBYAC
       N=IRBYAC(J)
       K1=IRBYAC(J+NRBYKIN)
       M  =NPBY(1,N)
       NSN  =NPBY(2,N)
       IF (INLOC(M)>0) THEN
        NKE1=INLOC(M)
        NDOF1(NKE1)=NDOF(M)
        NKM1=NKE1-INK
        DO I=1,NSN
         ID = I+K
         NI=LPBY(I+K1)
         NSS(ID)=0
         IF (INLOC(NI)>0) THEN
          NKE=INLOC(NI)
          DO N1=1,NROWK(NKE)
           NJ=ICOK(N1,NKE)
           NKE2=INLOC(NJ)
           IF (INLOC(NJ)>0.AND.
     .         (.NOT.INTAB(NSN,LPBY(K1+1),NJ))) THEN
            CALL REORDER_A(NROWK(NKE1),ICOKM(1,NKM1),NJ)
            IF (NKE2<=INK) THEN
             CALL REORDER_A(NROWK(NKE2),ICOK(1,NKE2),M)
             J1=NS+NSS(ID)+1
             ISS(J1)=NJ
             NSS(ID)=NSS(ID)+1
            ELSE
             CALL REORDER_A(NROWK(NKE2),ICOKM(1,NKE2-INK),M)
            ENDIF
           ENDIF
          ENDDO
          NS=NS+NSS(ID)
         ENDIF
        ENDDO
       ENDIF
       K=K+NSN
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROW_INT11                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROW_INT11(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTS   ,IRECTM    ,
     2    NROW      ,ILOC      ,NDOFI       ,N_IMPN   ,NSN       ,
     3    NSREM     )
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
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTS(2,*),IRECTM(2,*),NROW(*),
     .        ILOC(*),NDOFI,N_IMPN,NSN,NSREM
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,M1,M2
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = IRECTS(1,IG)
        N2 = IRECTS(2,IG)
        IF (ILOC(N1)==0) THEN
         N_IMPN=N_IMPN+1
         ILOC(N1)=N_IMPN
        ENDIF
        NROW(N1)=NROW(N1)+2
        IF (ILOC(N2)==0) THEN
         N_IMPN=N_IMPN+1
         ILOC(N2)=N_IMPN
        ENDIF
        NROW(N2)=NROW(N2)+2
       ELSE
        NSREM = NSREM + 2
       ENDIF
        NE=NE_IMP(I)
        M1 = IRECTM(1,NE)
        M2 = IRECTM(2,NE)
        IF (ILOC(M1)==0) THEN
         N_IMPN=N_IMPN+1
         ILOC(M1)=N_IMPN
        ENDIF
        NROW(M1)=NROW(M1)+2
        IF (ILOC(M2)==0) THEN
         N_IMPN=N_IMPN+1
         ILOC(M2)=N_IMPN
        ENDIF
        NROW(M2)=NROW(M2)+2
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROW_INT111                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT111(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTS    ,IRECTM     ,
     2    NROW      ,N_IMPN    ,ILOC        ,ICOL      ,NNMAX      ,
     3    NSN       )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTS(2,*),IRECTM(2,*),
     .        NROW(*),ILOC(*),ICOL(NNMAX,*),N_IMPN,NSN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI,M1,M2,NJ,MI,MJ
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = IRECTS(1,IG)
        N2 = IRECTS(2,IG)
        NE=NE_IMP(I)
        M1 = IRECTM(1,NE)
        M2 = IRECTM(2,NE)
        NI=ILOC(N1)
        MI=ILOC(M1)
        MJ=ILOC(M2)
         CALL REORDER_A(NROW(N1),ICOL(1,NI),M1)
         CALL REORDER_A(NROW(M1),ICOL(1,MI),N1)
         CALL REORDER_A(NROW(N1),ICOL(1,NI),M2)
         CALL REORDER_A(NROW(M2),ICOL(1,MJ),N1)
        NJ=ILOC(N2)
         CALL REORDER_A(NROW(N2),ICOL(1,NJ),M1)
         CALL REORDER_A(NROW(M1),ICOL(1,MI),N2)
         CALL REORDER_A(NROW(N2),ICOL(1,NJ),M2)
         CALL REORDER_A(NROW(M2),ICOL(1,MJ),N2)
       ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROW_INT112                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        ROW_ADDS                      source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_INT112(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTS    ,IRECTM    ,
     2    NROW      ,N_IMPN    ,ILOC        ,ICOL      ,NNMAX      ,
     3    ICOK      ,NKMAX     ,NSN       )
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
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTS(2,*),IRECTM(2,*),NROW(*),
     .        ILOC(*),ICOL(NNMAX,*),ICOK(NKMAX,*),N_IMPN,NSN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,NI,NM,NIM,M1,M2
C-----------------------------------------------
C---------ICOL : LOCAL NODE NUMBER--------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = IRECTS(1,IG)
        N2 = IRECTS(2,IG)
        NE=NE_IMP(I)
        M1 = IRECTM(1,NE)
        M2 = IRECTM(2,NE)
        CALL ROW_ADDS(N1   ,M1    ,ILOC  ,N_IMPN  ,ICOL   ,
     1                ICOK ,NROW  ,NNMAX ,NKMAX )
        CALL ROW_ADDS(N1   ,M2    ,ILOC  ,N_IMPN  ,ICOL   ,
     1                ICOK ,NROW  ,NNMAX ,NKMAX )
        CALL ROW_ADDS(N2   ,M1    ,ILOC  ,N_IMPN  ,ICOL   ,
     1                ICOK ,NROW  ,NNMAX ,NKMAX )
        CALL ROW_ADDS(N2   ,M2    ,ILOC  ,N_IMPN  ,ICOL   ,
     1                ICOK ,NROW  ,NNMAX ,NKMAX )
       END IF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROW_ADDS                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        ROW_INT112                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE ROW_ADDS(NS    ,NM    ,ILOC  ,ISHF    ,ICOL   ,
     1                    ICOK  ,NROW  ,NNMAX ,NKMAX )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX,NS,NM
      INTEGER NROW(*) ,ILOC(*)  ,ISHF  ,ICOL(NNMAX,*),ICOK(NKMAX,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N
C
C----6---------------------------------------------------------------7---------8
       N1 =ILOC(NS)
       N2 =ILOC(NM)
       IF (N1<=ISHF) THEN
        CALL REORDER_A(NROW(N1),ICOL(1,N1),NM)
       ELSE
        N=N1- ISHF
        CALL REORDER_A(NROW(N1),ICOK(1,N),NM)
       ENDIF
       IF (N2<=ISHF) THEN
        CALL REORDER_A(NROW(N2),ICOL(1,N2),NS)
       ELSE
        N=N2- ISHF
        CALL REORDER_A(NROW(N2),ICOK(1,N),NS)
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  IDEL_INT                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        NDOF_INT                      source/implicit/ind_glob_k.F  
Chd|        NDOF_INT11                    source/implicit/ind_glob_k.F  
Chd|        NDOF_INT5                     source/implicit/ind_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IDEL_INT(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    IND_IMP   ,NDOF      ,NT_IMP    )
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
#include      "scr05_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NUM_IMP(*),IND_IMP(*),
     .        NS_IMP(*),NE_IMP(*),NDOF(*),NT_IMP
C     REAL

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,NTY,NSN
      INTEGER I,J,K,L,NDOFI,N,IAD,N_IMP,
     .        NRTS,IDEL(NT_IMP),NEW_IAD
C--------ramener NDOF des secnds remotes-----------------------------------
      IF (IMACH==3.AND.NSPMD>1) THEN
      ENDIF
C
      IAD=1
C      MULTIMP=1
      N_IMP=0
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
C       MULTIMP=MAX(MULTIMP,IPARI(23,NIN))
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
       CALL NDOF_INT5(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),INTBUF_TAB(NIN)%IRECTM,
     .                 INTBUF_TAB(NIN)%NSV,NSN        ,NDOF       ,IDEL(IAD)   ,
     .                 INTBUF_TAB(NIN)%MSR)
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
      DO NIN=1,NINTER
       NTY   =IPARI(7,NIN)
       NSN   =IPARI(5,NIN)
C       MULTIMP=MAX(MULTIMP,IPARI(23,NIN))
       IF(NTY==3)THEN
       ELSEIF(NTY==4)THEN
       ELSEIF(NTY==5)THEN
       ELSEIF(NTY==6)THEN

       ELSEIF(NTY==7.OR.NTY==10.OR.NTY==24)THEN
C
        CALL NDOF_INT(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),INTBUF_TAB(NIN)%IRECTM,
     .                 INTBUF_TAB(NIN)%NSV,NSN        ,NDOF       ,IDEL(IAD)   )
        IAD=IAD+NUM_IMP(NIN)
       ELSEIF(NTY==11)THEN
C
        NRTS   =IPARI(3,NIN)
        CALL NDOF_INT11(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                 INTBUF_TAB(NIN)%IRECTS,INTBUF_TAB(NIN)%IRECTM,NRTS       ,
     .                 NDOF       ,IDEL(IAD)    )
        IAD=IAD+NUM_IMP(NIN)
       ENDIF
      ENDDO
C-------actualise NUM_IMP,NS_IMP,NE_IMP,IND_IMP-------
      NEW_IAD = 0
      IAD=1
C-------int5 first------
      DO NIN=1,NINTER
       N_IMP=0
       NTY   =IPARI(7,NIN)
       IF (NTY==5) THEN
        DO I= 1,NUM_IMP(NIN)
         IF (IDEL(IAD+I)>0) THEN
          NEW_IAD = NEW_IAD + 1
          NS_IMP(NEW_IAD)=NS_IMP(IAD+I)
          NE_IMP(NEW_IAD)=NE_IMP(IAD+I)
          IND_IMP(NEW_IAD)=IND_IMP(IAD+I)
          N_IMP = N_IMP + 1
         ENDIF
        ENDDO
        IAD=IAD+NUM_IMP(NIN)
        NUM_IMP(NIN) = N_IMP
       END IF
      ENDDO
      DO NIN=1,NINTER
       N_IMP=0
       IF (NTY/=5) THEN
        DO I= 1,NUM_IMP(NIN)
        IF (IDEL(IAD+I)>0) THEN
         NEW_IAD = NEW_IAD + 1
         NS_IMP(NEW_IAD)=NS_IMP(IAD+I)
         NE_IMP(NEW_IAD)=NE_IMP(IAD+I)
         IND_IMP(NEW_IAD)=IND_IMP(IAD+I)
         N_IMP = N_IMP + 1
        ENDIF
        ENDDO
        IAD=IAD+NUM_IMP(NIN)
        NUM_IMP(NIN) = N_IMP
       END IF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  NDOF_INT                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IDEL_INT                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NDOF_INT(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT    ,NSV       ,
     2    NSN       ,NDOF      ,IDEL_INT  )
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
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),
     .        IDEL_INT(*),NDOF(*),NSN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = NSV(IG)
        IDEL_INT(I) = NDOF(N1)
       ELSE
       ENDIF
        NE=NE_IMP(I)
        DO J=1,3
         N=IRECT(J,NE)
         IDEL_INT(I) = MIN(IDEL_INT(I),NDOF(N))
        ENDDO
        IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
         N=IRECT(4,NE)
         IDEL_INT(I) = MIN(IDEL_INT(I),NDOF(N))
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  NDOF_INT11                    source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IDEL_INT                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NDOF_INT11(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTS   ,IRECTM    ,
     2    NSN       ,NDOF      ,IDEL_INT  )
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
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTS(2,*),IRECTM(2,*),
     .        IDEL_INT(*),NDOF(*),NSN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,M1,M2
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG<=NSN) THEN
        N1 = IRECTS(1,IG)
        IDEL_INT(I) = NDOF(N1)
        N2 = IRECTS(2,IG)
        IDEL_INT(I) = MIN(IDEL_INT(I),NDOF(N2))
       ELSE
       ENDIF
        NE=NE_IMP(I)
        M1 = IRECTM(1,NE)
        M2 = IRECTM(2,NE)
        IDEL_INT(I) = MIN(IDEL_INT(I),NDOF(M1))
        IDEL_INT(I) = MIN(IDEL_INT(I),NDOF(M2))
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  NDOF_INT5                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IDEL_INT                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NDOF_INT5(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT    ,NSV       ,
     2    NSN       ,NDOF      ,IDEL_INT    ,MSR      )
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
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),
     .        IDEL_INT(*),NDOF(*),NSN,MSR(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
        N1 = NSV(IG)
        IDEL_INT(I) = NDOF(N1)
        NE=NE_IMP(I)
        DO J=1,3
         N=MSR(IRECT(J,NE))
         IDEL_INT(I) = MIN(IDEL_INT(I),NDOF(N))
        ENDDO
        IF (IRECT(3,NE)/=IRECT(4,NE)) THEN
         N=MSR(IRECT(4,NE))
         IDEL_INT(I) = MIN(IDEL_INT(I),NDOF(N))
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_SPA2                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE DIM_SPA2(NDDL,IADK,JDIK,L_NZ)
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
      INTEGER  NDDL,IADK(*),JDIK(*),L_NZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,JD,ICOL(NDDL),NRI
C----6---------------------------------------------------------------7---------8
      L_NZ = 0
      DO I = 1,NDDL
       NRI = IADK(I+1)-IADK(I)
       CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
       DO J=IADK(I),IADK(I+1)-1
        JD = JDIK(J)
        DO K = IADK(JD),IADK(JD+1)-1
         CALL REORDER_A(NRI,ICOL,JDIK(K))
        ENDDO
       ENDDO
       L_NZ = L_NZ + NRI
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IND_SPA2                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        K_BAND                        source/implicit/imp_solv.F    
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        REORDER_M                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE IND_SPA2(NDDL,IADK,JDIK,IADM,JDIM,L_MAX)
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
      INTEGER  NDDL,IADK(*),JDIK(*),IADM(*),JDIM(*),L_MAX
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,ICOL(NDDL),L_NZ,NRI,JD
C----6--------opt. envisager-creer a partir de tableux nodales--,iadm,jdim+upd_(chaque isetk)
      L_NZ = 0
      IADM(L_NZ+1) = L_NZ+1
      DO I = 1,NDDL
       NRI = IADK(I+1)-IADK(I)
       CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
       DO J=IADK(I),IADK(I+1)-1
        JD = JDIK(J)
        DO K = IADK(JD),IADK(JD+1)-1
         CALL REORDER_A(NRI,ICOL,JDIK(K))
        ENDDO
       ENDDO
       CALL REORDER_M(NRI,ICOL)
       DO J=1,NRI
         L_NZ = L_NZ + 1
         JDIM(L_NZ) = ICOL(J)
       ENDDO
       IADM(I+1) = L_NZ+1
      ENDDO
      CALL K_BAND(NDDL,IADM,JDIM,L_MAX)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_M                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_SPAN                      source/implicit/ind_glob_k.F  
Chd|        IND_SPA2                      source/implicit/ind_glob_k.F  
Chd|        IND_SPAN                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_M(N,IC)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IT,IMIN
C
      IF (N==0) RETURN
C-----en ordre croisante-----
      DO I =1,N
       IMIN=IC(I)
       II=I
       DO J =I+1,N
        IF (IC(J)<IMIN) THEN
         IMIN=IC(J)
         II=J
        ENDIF
       ENDDO
       IF (II/=I) THEN
        IT=IC(I)
        IC(I)=IC(II)
        IC(II)=IT
       ENDIF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_SPAN                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        INI_K0H                       source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        REORDER_M                     source/implicit/ind_glob_k.F  
Chd|        IMP_PPAT                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DIM_SPAN(NN,NDDL,IADK,JDIK,L_NZ,NDMAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_PPAT
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,IADK(*),JDIK(*),L_NZ,NN,NDMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,JD,ICOL(NDDL),ICRI(NDDL),NRI,NR0
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADK0,JDIK0
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADL,JDIL
C----6---d'abord---K0:matrice complete(non triang)
      L_NZ = 2*(IADK(NDDL+1)-IADK(1))
      ALLOCATE(IADK0(NDDL+1),JDIK0(L_NZ))
      DO I = 1, NDDL
        ICOL(I) = IADK(I+1) - IADK(I)
        DO J = IADK(I),IADK(I+1)-1
         JD = JDIK(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
      ENDDO
      IADK0(1) = 1
      DO I = 1,NDDL
       IADK0(I+1) = IADK0(I)+ICOL(I)
       ICRI(I) = PRE_FPAT(I)
      ENDDO
      DO I = 1,NDDL
        NRI = IADK(I+1)-IADK(I)
        CALL CP_INT(NRI,JDIK(IADK(I)),JDIK0(IADK0(I)))
        ICOL(I) = NRI
        DO J=IADK(I),IADK(I+1)-1
         JD = JDIK(J)
         K = IADK0(JD) + ICOL(JD)
         JDIK0(K) = I
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
      ENDDO
C
      L_NZ = 0
      DO I = 1,NDDL
       NRI = IADK(I+1)-IADK(I)
       IF (ICRI(I)==1) THEN
        CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
        DO J=IADK(I),IADK(I+1)-1
         JD = JDIK(J)
         DO K = IADK0(JD),IADK0(JD+1)-1
          IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
         ENDDO
        ENDDO
       ENDIF
       L_NZ = L_NZ + NRI
      ENDDO
C
      SELECT CASE(NN)
       CASE (2)
C 
       CASE (3)
C-----------L->K^2----------
       ALLOCATE(IADL(NDDL+1),JDIL(L_NZ))
       IADL(1) = 1
       L_NZ = 0
       DO I = 1,NDDL
        NRI = IADK(I+1)-IADK(I)
        CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
        IF (ICRI(I)==1) THEN
         NR0 = NRI
         DO J=IADK(I),IADK(I+1)-1
          JD = JDIK(J)
          DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
          ENDDO
         ENDDO
         IF (NRI>NR0) CALL REORDER_M(NRI,ICOL)
        ENDIF
        DO J=1,NRI
         L_NZ = L_NZ + 1
         JDIL(L_NZ) = ICOL(J)
        ENDDO
        IADL(I+1) = L_NZ+1
       ENDDO
c       print *,'nddl,L_NZ,NDMAX=',nddl,L_NZ,NDMAX
C---- ---------*K0------------
       L_NZ = 0
       DO I = 1,NDDL
        NRI = IADL(I+1)-IADL(I)
        IF (ICRI(I)==1) THEN
         CALL CP_INT(NRI,JDIL(IADL(I)),ICOL)
         DO J=IADL(I),IADL(I+1)-1
          JD = JDIL(J)
          DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
          ENDDO
         ENDDO
        ENDIF
        L_NZ = L_NZ + NRI
       ENDDO
       DEALLOCATE(IADL,JDIL)
C
       CASE (4)
C-----------L->K^2----------
       ALLOCATE(IADL(NDDL+1),JDIL(L_NZ))
       IADL(1) = 1
       L_NZ = 0
       DO I = 1,NDDL
        NRI = IADK(I+1)-IADK(I)
        CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
        IF (ICRI(I)==1) THEN
         NR0 = NRI
         DO J=IADK(I),IADK(I+1)-1
          JD = JDIK(J)
          DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
          ENDDO
         ENDDO
         IF (NRI>NR0) CALL REORDER_M(NRI,ICOL)
        ENDIF
        DO J=1,NRI
         L_NZ = L_NZ + 1
         JDIL(L_NZ) = ICOL(J)
        ENDDO
        IADL(I+1) = L_NZ+1
       ENDDO
C-----------K0-> K^2-complet---------
       DEALLOCATE(JDIK0)
       ALLOCATE(JDIK0(2*L_NZ))
       DO I = 1, NDDL
        ICOL(I) = IADL(I+1) - IADL(I)
        DO J = IADL(I),IADL(I+1)-1
         JD = JDIL(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
       ENDDO
       IADK0(1) = 1
       DO I = 1,NDDL
        IADK0(I+1) = IADK0(I)+ICOL(I)
       ENDDO
       DO I = 1,NDDL
        NRI = IADL(I+1)-IADL(I)
        CALL CP_INT(NRI,JDIL(IADL(I)),JDIK0(IADK0(I)))
        ICOL(I) = NRI
        DO J=IADL(I),IADL(I+1)-1
         JD = JDIL(J)
         K = IADK0(JD) + ICOL(JD)
         JDIK0(K) = I
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
       ENDDO
C-----------* K^2----------
       L_NZ = 0
       DO I = 1,NDDL
        NRI = IADL(I+1)-IADL(I)
        CALL CP_INT(NRI,JDIL(IADL(I)),ICOL)
        IF (ICRI(I)==1) THEN
         DO J=IADL(I),IADL(I+1)-1
          JD = JDIL(J)
          DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
          ENDDO
         ENDDO
        ENDIF
        L_NZ = L_NZ + NRI
       ENDDO
       DEALLOCATE(IADL,JDIL)
      END SELECT
      DEALLOCATE(IADK0,JDIK0)
c      print *,'DIM_NZ,nddl=',L_NZ,nddl
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IND_SPAN                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        K_BAND                        source/implicit/imp_solv.F    
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        REORDER_M                     source/implicit/ind_glob_k.F  
Chd|        IMP_PPAT                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IND_SPAN(NN,NDF,NDDL,IADK,JDIK,IADM,JDIM,L_MAX,NDMAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_PPAT
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,IADK(*),JDIK(*),IADM(*),JDIM(*),L_MAX,NN,NDF,NDMAX
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,ICOL(NDDL),ICRI(NDDL),L_NZ,NRI,JD,NR0
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADK0,JDIK0
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADL,JDIL
C----6--opt. envisager-creer a partir de tableux nodales--,iadm,jdim+upd_(chaque isetk)
      L_NZ = 2*(IADK(NDDL+1)-IADK(1))
C       
      ALLOCATE(IADK0(NDDL+1),JDIK0(L_NZ))
      DO I = 1, NDDL
        ICOL(I) = IADK(I+1) - IADK(I)
        DO J = IADK(I),IADK(I+1)-1
         JD = JDIK(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
      ENDDO
      IADK0(1) = 1
      DO I = 1,NDDL
       IADK0(I+1) = IADK0(I)+ICOL(I)
       ICRI(I) = PRE_FPAT(I)
      ENDDO
      DO I = 1,NDDL
        NRI = IADK(I+1)-IADK(I)
        CALL CP_INT(NRI,JDIK(IADK(I)),JDIK0(IADK0(I)))
        ICOL(I) = NRI
        DO J=IADK(I),IADK(I+1)-1
         JD = JDIK(J)
         K = IADK0(JD) + ICOL(JD)
         JDIK0(K) = I
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
      ENDDO
C
      SELECT CASE(NN)
       CASE (2)
C
       IADM(1) = IADK(1)
       DO I = 1,NDF
        IADM(I+1) = IADK(I+1)
       ENDDO
       DO J=IADK(1),IADK(NDF+1)-1
        JDIM(J) = JDIK(J)
       ENDDO
       L_NZ = IADK(NDF+1)-IADK(1)
C
       DO I = NDF+1,NDDL
        NRI = IADK(I+1)-IADK(I)
        CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
	IF (ICRI(I)==1) THEN
         NR0 = NRI
         DO J=IADK(I),IADK(I+1)-1
          JD = JDIK(J)
          DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
          ENDDO
         ENDDO
         IF (NRI>NR0) CALL REORDER_M(NRI,ICOL)
	ENDIF
        DO J=1,NRI
         L_NZ = L_NZ + 1
         JDIM(L_NZ) = ICOL(J)
        ENDDO
        IADM(I+1) = L_NZ+1
       ENDDO
C
       CASE (3)
C
       IADM(1) = IADK(1)
       DO I = 1,NDF
        IADM(I+1) = IADK(I+1)
       ENDDO
       DO J=IADK(1),IADK(NDF+1)-1
        JDIM(J) = JDIK(J)
       ENDDO
       L_NZ = IADK(NDF+1)-IADK(1)
C
       DO I = NDF+1,NDDL
        NRI = IADK(I+1)-IADK(I)
        CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
	IF (ICRI(I)==1) THEN
         NR0 = NRI
         DO J=IADK(I),IADK(I+1)-1
          JD = JDIK(J)
          DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
          ENDDO
         ENDDO
         IF (NRI>NR0) CALL REORDER_M(NRI,ICOL)
	ENDIF
        DO J=1,NRI
         L_NZ = L_NZ + 1
         JDIM(L_NZ) = ICOL(J)
        ENDDO
        IADM(I+1) = L_NZ+1
       ENDDO
C
        ALLOCATE(IADL(NDDL+1),JDIL(L_NZ))
        CALL CP_INT(NDDL+1,IADM,IADL)
        CALL CP_INT(L_NZ,JDIM,JDIL)
        L_NZ = IADK(NDF+1)-IADK(1)
        DO I = NDF+1,NDDL
         NRI = IADL(I+1)-IADL(I)
         CALL CP_INT(NRI,JDIL(IADL(I)),ICOL)
	 IF (ICRI(I)==1) THEN
          NR0 = NRI
          DO J=IADL(I),IADL(I+1)-1
           JD = JDIL(J)
           DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
           ENDDO
          ENDDO
          IF (NRI>NR0) CALL REORDER_M(NRI,ICOL)
	 ENDIF
         DO J=1,NRI
           L_NZ = L_NZ + 1
           JDIM(L_NZ) = ICOL(J)
         ENDDO
         IADM(I+1) = L_NZ+1
        ENDDO
        DEALLOCATE(IADL,JDIL)
       CASE (4)
C
       L_NZ = 0
       IADM(1) = IADK(1)
       DO I = 1,NDDL
        NRI = IADK(I+1)-IADK(I)
        CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
	IF (ICRI(I)==1) THEN
         NR0 = NRI
         DO J=IADK(I),IADK(I+1)-1
          JD = JDIK(J)
          DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
          ENDDO
         ENDDO
         IF (NRI>NR0) CALL REORDER_M(NRI,ICOL)
	ENDIF
        DO J=1,NRI
         L_NZ = L_NZ + 1
         JDIM(L_NZ) = ICOL(J)
        ENDDO
        IADM(I+1) = L_NZ+1
       ENDDO
        ALLOCATE(IADL(NDDL+1),JDIL(L_NZ))
        CALL CP_INT(NDDL+1,IADM,IADL)
        CALL CP_INT(L_NZ,JDIM,JDIL)
C-----------K0-> K^2-complet---------
        DEALLOCATE(JDIK0)
        ALLOCATE(JDIK0(2*L_NZ))
        DO I = 1, NDDL
         ICOL(I) = IADL(I+1) - IADL(I)
         DO J = IADL(I),IADL(I+1)-1
          JD = JDIL(J)
          ICOL(JD) = ICOL(JD) + 1
         ENDDO
        ENDDO
        IADK0(1) = 1
        DO I = 1,NDDL
         IADK0(I+1) = IADK0(I)+ICOL(I)
        ENDDO
        DO I = 1,NDDL
         NRI = IADL(I+1)-IADL(I)
         CALL CP_INT(NRI,JDIL(IADL(I)),JDIK0(IADK0(I)))
         ICOL(I) = NRI
         DO J=IADL(I),IADL(I+1)-1
          JD = JDIL(J)
          K = IADK0(JD) + ICOL(JD)
          JDIK0(K) = I
          ICOL(JD) = ICOL(JD) + 1
         ENDDO
        ENDDO
C
        IADM(1) = IADK(1)
        DO I = 1,NDF
         IADM(I+1) = IADK(I+1)
        ENDDO
        DO J=IADK(1),IADK(NDF+1)-1
         JDIM(J) = JDIK(J)
        ENDDO
        L_NZ = IADK(NDF+1)-IADK(1)
        DO I = NDF+1,NDDL
         NRI = IADL(I+1)-IADL(I)
         CALL CP_INT(NRI,JDIL(IADL(I)),ICOL)
	 IF (ICRI(I)==1) THEN
          NR0 = NRI
          DO J=IADL(I),IADL(I+1)-1
           JD = JDIL(J)
           DO K = IADK0(JD),IADK0(JD+1)-1
           IF (JDIK0(K)<I) CALL REORDER_A(NRI,ICOL,JDIK0(K))
           ENDDO
          ENDDO
          IF (NRI>NR0) CALL REORDER_M(NRI,ICOL)
	 ENDIF
         DO J=1,NRI
           L_NZ = L_NZ + 1
           JDIM(L_NZ) = ICOL(J)
         ENDDO
         IADM(I+1) = L_NZ+1
        ENDDO
        DEALLOCATE(IADL,JDIL)
      END SELECT
      DEALLOCATE(IADK0,JDIK0)
      CALL K_BAND(NDDL,IADM,JDIM,L_MAX)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  FIL_SPAN0                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|        IMP_PPAT                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FIL_SPAN0(NRBYAC,IRBYAC,NPBY,IDDL,NDOF,NDDL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_PPAT
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
      INTEGER  NDDL,NPBY(NNPBY,*),IDDL(*),NRBYAC,IRBYAC(*),NDOF(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,M,N,ID
C----6---------------------------------------------------
C       
      ALLOCATE(PRE_FPAT(NDDL))
      PRE_FPAT = 1
      DO I=1,NRBYAC
       N=IRBYAC(I)
       M  =NPBY(1,N)
       ID = IDDL(M)
       DO J=1,NDOF(M)
        PRE_FPAT(ID+J) = 0
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  FIL_SPAN1                     source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_PPAT                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FIL_SPAN1(NRBYAC,IRBYAC,NPBY,IDDL,NDDL,IKC,NDOF,INLOC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_PPAT
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NPBY(NNPBY,*),IDDL(*),NRBYAC,IRBYAC(*),
     +         IKC(*),NDOF(*),INLOC(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,M,N,ID,IDDLM(NDDL),IFIX,IDM
C----6---------------------------------------------------
C       
      DO I=1,NDDL
       PRE_FPAT(I) = 1
      ENDDO
C
      IFIX=0
       DO N = 1,NUMNOD
         I=INLOC(N)
         IDDLM(I)=IDDL(I)-IFIX
         DO J=1,NDOF(I)
          ID = IDDL(I)+J
          IF (IKC(ID)/=0) IFIX=IFIX+1
         ENDDO
       ENDDO
      DO I=1,NRBYAC
       N=IRBYAC(I)
       M  =NPBY(1,N)
       ID = IDDL(M)
       IDM = IDDLM(M)
       IFIX=0
       DO J=1,NDOF(M)
        IF (IKC(ID+J)==0) THEN
	 IFIX=IFIX+1
         PRE_FPAT(IDM+IFIX) = 0
        ENDIF
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KTOT                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        L2G_KLOC                      source/implicit/ind_glob_k.F  
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE DIM_KTOT(NDDL  ,IADK  ,JDIK  ,IADI  ,JDII  ,
     1                    ITOK  ,NDDLI ,L_NZ  ,LT_I  )
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
      INTEGER  NDDL,NDDLI,IADK(*),JDIK(*),IADI(*),JDII(*),
     .         ITOK(*),L_NZ
      my_real
     .         LT_I(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,JD,JK,K2I(NDDL),ICOL(NDDL),NRI
C----6---------------------------------------------------------------7---------8
      CALL L2G_KLOC(NDDLI ,IADI  ,JDII  ,ITOK  ,LT_I  )
C
      DO I = 1,NDDL
       K2I(I) = 0
      ENDDO
      DO I = 1,NDDLI
       J = ITOK(I)
       K2I(J) = I
      ENDDO
      L_NZ = 0
      DO I = 1,NDDL
       NRI = IADK(I+1)-IADK(I)
       IF (K2I(I)>0) THEN
        CALL CP_INT(NRI,JDIK(IADK(I)),ICOL)
        K = K2I(I)
        DO J=IADI(K),IADI(K+1)-1
         JD = JDII(J)
         JK = ITOK(JD)
         CALL REORDER_A(NRI,ICOL,JK)
        ENDDO
       ENDIF
       L_NZ = L_NZ + NRI
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IND_KTOT                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IND_KTOT(NDDL  ,IADK  ,JDIK  ,IADI  ,JDII  ,
     1                    ITOK  ,NDDLI ,IADT  ,JDIT  ,LT_K  ,
     2                    LT_I  ,LT_T  ,NZL   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDLI,IADK(*),JDIK(*),IADI(*),JDII(*),
     .         ITOK(*),IADT(*)  ,JDIT(*),NZL
      my_real
     .         LT_K(*), LT_I(*), LT_T(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,N,L,JD,JK,K2I(NDDL),IFT,NZ
C----6---------------------------------------------------------------7---------8
C
      DO I = 1,NDDL
       K2I(I) = 0
      ENDDO
      DO I = 1,NDDLI
       J = ITOK(I)
       K2I(J) = I
       IF (IADI(I)==IADI(I+1)) K2I(J) = 0
      ENDDO
C
      NZ = 0
      IADT(1) = NZ + 1
      DO I = 1,NDDL
       IF (K2I(I)==0) THEN
        DO J=IADK(I),IADK(I+1)-1
         NZ = NZ + 1
         JDIT(NZ) = JDIK(J)
         LT_T(NZ) = LT_K(J)
        ENDDO
       ELSE
        N = K2I(I)
C---- first for [k]----
         K=IADI(N)
         JD = JDII(K)
         JK = ITOK(JD)
         IFT = IADK(I)
        DO K=IADI(N),IADI(N+1)-1
         JD = JDII(K)
         JK = ITOK(JD)
         DO J=IFT,IADK(I+1)-1
          IF (JK==JDIK(J)) THEN
           NZ = NZ + 1
           JDIT(NZ) = JDIK(J)
           LT_T(NZ) = LT_K(J)+LT_I(K)
           IFT = J + 1
           GOTO 100
          ELSEIF (JK<JDIK(J)) THEN
           NZ = NZ + 1
           JDIT(NZ) = JK
           LT_T(NZ) = LT_I(K)
           GOTO 100
          ELSE
           NZ = NZ + 1
           JDIT(NZ) = JDIK(J)
           LT_T(NZ) = LT_K(J)
           IFT = J + 1
          ENDIF
         ENDDO
C---- end of insert-----
         J = IADK(I+1)-1
         IF (JK>JDIK(J)) THEN
           NZ = NZ + 1
           JDIT(NZ) = JK
           LT_T(NZ) = LT_I(K)
         ENDIF
 100     CONTINUE
         IF (K==(IADI(N+1)-1)) THEN
          DO J=IFT,IADK(I+1)-1
           NZ = NZ + 1
           JDIT(NZ) = JDIK(J)
           LT_T(NZ) = LT_K(J)
          ENDDO
         ENDIF
        ENDDO
       ENDIF
       IADT(I+1) = NZ + 1
      ENDDO
      IF (NZ/=NZL) THEN
         CALL ANCMSG(MSGID=80,ANMODE=ANINFO,
     .               C1='ASSEMBLY')
         IF (NZ>NZL) CALL ARRET(2)
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  L2G_KLOC                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_KTOT                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        REORDER_KIJ                   source/implicit/ind_glob_k.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE L2G_KLOC(NDDLI ,IADI  ,JDII  ,ITOK  ,LT_I  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include "impl1_c.inc"
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDLI,IADI(*),JDII(*),ITOK(*),NZI
      my_real
     .         LT_I(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,ICOL(NDDLI),L_NZ,NRI,NZ,JD,GI,GJ,IFT
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADK0,JDIK0
      my_real,
     .         DIMENSION(:),ALLOCATABLE ::  LT_K0
C----6---------------
      L_NZ = 2*(IADI(NDDLI+1)-IADI(1))
C -------------[K0]-locale complete----------    
       ALLOCATE(IADK0(NDDLI+1),JDIK0(L_NZ),LT_K0(L_NZ))
       DO I = 1,NDDLI
        ICOL(I) = 0
       ENDDO
       DO I = 1, NDDLI
        ICOL(I) = ICOL(I) + IADI(I+1) - IADI(I)
        DO J = IADI(I),IADI(I+1)-1
         JD = JDII(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
       ENDDO
       IADK0(1) = 1
       DO I = 1,NDDLI
        IADK0(I+1) = IADK0(I)+ICOL(I)
       ENDDO
       NZ=IADK0(NDDLI+1) - IADK0(1)
       DO I = 1,NDDLI
        NRI = IADI(I+1)-IADI(I)
        CALL CP_INT(NRI,JDII(IADI(I)),JDIK0(IADK0(I)))
        CALL CP_REAL(NRI,LT_I(IADI(I)),LT_K0(IADK0(I)))
        ICOL(I) =  NRI
       ENDDO
       DO I = 1,NDDLI
        DO J=IADI(I),IADI(I+1)-1
         JD = JDII(J)
         K = IADK0(JD) + ICOL(JD)
         JDIK0(K) = I
         LT_K0(K) = LT_I(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
       ENDDO
C
       NZ = 0
       IADI(1) = NZ + 1
      IF (IKPAT==0 )THEN
C -------------trang_sup----------    
       DO I = 1, NDDLI
        GI = ITOK(I)
        DO J = IADK0(I),IADK0(I+1)-1
         JD = JDIK0(J)
         GJ = ITOK(JD)
         IF (GJ>GI)THEN
          NZ = NZ + 1
          JDII(NZ) = JD
          LT_I(NZ) = LT_K0(J)
         ENDIF
        ENDDO
        IADI(I+1) = NZ + 1
       ENDDO
      ELSE
C -------------trang_inf----------    
       DO I = 1, NDDLI
        GI = ITOK(I)
        DO J = IADK0(I),IADK0(I+1)-1
         JD = JDIK0(J)
         GJ = ITOK(JD)
         IF (GJ<GI)THEN
          NZ = NZ + 1
          JDII(NZ) = JD
          LT_I(NZ) = LT_K0(J)
         ENDIF
        ENDDO
        IADI(I+1) = NZ + 1
       ENDDO
      ENDIF
      DEALLOCATE(IADK0,JDIK0,LT_K0)
      IF (NZ>L_NZ/2) THEN
        CALL ANCMSG(MSGID=80,ANMODE=ANINFO,
     .              C1='TRANSLATION')
        CALL ARRET(2)
      ENDIF
C -------------in order----------    
      DO I = 1,NDDLI
       NZ = IADI(I+1)-IADI(I)
       IFT = IADI(I)
       CALL REORDER_KIJ(NZ,JDII(IFT),LT_I(IFT),ITOK)
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_KIJ                   source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        L2G_KLOC                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_KIJ(N,IC,RC,IDDL)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),IDDL(*)
      my_real
     .        RC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IT,IIC,IMIN,IDIC(N)
      my_real
     .        S
C
      DO I =1,N
       IDIC(I)=IDDL(IC(I))
      ENDDO
      IF (N==0) RETURN
C-----en ordre iddl croisante-----
      DO I =1,N
       IMIN=IDIC(I)
       II=I
       DO J =I+1,N
        IF (IDIC(J)<IMIN) THEN
         IMIN=IDIC(J)
         II=J
        ENDIF
       ENDDO
       IF (II/=I) THEN
        IT=IC(I)
        S =RC(I)
        IC(I)=IC(II)
        IC(II)=IT
        RC(I)=RC(II)
        RC(II)=S
        IT=IDIC(I)
        IDIC(I)=IDIC(II)
        IDIC(II)=IT
       ENDIF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  NDOF_FV                       source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE NDOF_FV(IBFV  ,VEL   ,NDOF  ,IFRAME)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),NDOF(*),IFRAME(LISKN,*)
C     REAL
      my_real
     .  VEL(LFXVELR,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, ISENS,K,
     .        II, IC, NN, IDEB, NR, NSK, NFK, IFM,INDEX(MVSIZ)
C     REAL
      my_real
     .   FAC, STARTT, STOPT, TS
C IBFV(7,N):1 V;2 D ;0 A ;
C-------------------------------
      IDEB = 0
C----is there is Du in [TT,TSTOP] TT-> Tstart
      DO NN=1,NFXVEL,NVSIZ
        IF (IBFV(8,NN)==1) GOTO 100
        IC = 0
C        IF (NSENSOR>0) : by sensor will be ignoned (if not activated)
          DO 20 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            IF(TSTOP<=STARTT)GOTO 20
            IF(TT>=STOPT) GOTO 20
            I=IABS(IBFV(1,N))
            IC = IC + 1
            INDEX(IC) = N
 20       CONTINUE
        IDEB = IDEB + MIN(NFXVEL-IDEB,NVSIZ)
C
         DO II=1,IC
          N = INDEX(II)
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM<=1) J=J-10*ISK
          IF(J<=3)THEN
            IF (NDOF(I)==0) NDOF(I)=3
          ELSEIF(J<=6)THEN
            IF (NDOF(I)==0) NDOF(I)=6
C        stop erroring out            
           IF (NDOF(I) <=3) THEN
             CALL ANCMSG(MSGID=253,ANMODE=ANINFO)
             CALL ARRET(2)
           ENDIF
          ENDIF
C---------Otherwise Rotation will not be transforted
C          IF (IFM >1) THEN
C           I = IFRAME(1,IFM)
C           IF (NDOF(I)==0) NDOF(I)=3
C           IF (NDOF(I)==0.AND.J>3) NDOF(I)=6
C          END IF
         ENDDO
 100    CONTINUE
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I24MSEGV                      source/implicit/ind_glob_k.F  
Chd|-- called by -----------
Chd|        ROWFR_DIM24                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND24                   source/mpi/implicit/imp_fri.F 
Chd|        ROW_INT24                     source/implicit/ind_glob_k.F  
Chd|        ROW_INT241                    source/implicit/ind_glob_k.F  
Chd|        ROW_INT242                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24MSEGV(IE    ,IRTLMV  ,SUBTRIA,IRTLM ,NVOISIN)
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IE,IRTLMV(4),IRTLM(4),SUBTRIA,NVOISIN(8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX1, IX2, IX3, IX4
C-----------------------------------------------
C        11-------10       
C         |\ 19  /|        
C         | \   / |                                    
C         |  \ /  |           
C         |  16   |           
C         |15/ \11|        
C         | /   \ |      
C         |/  7  \|       
C12-------4-------3-------9
C |\ 12  /|\     /|\ 14  /|
C | \   / | \ 3 / | \   / |
C |  \ /  |  \ /2 |6 \ /18|
C |  17   |   5   |  15   |
C |20/ \ 8| 4/ \  |  / \  |
C | /   \ | / 1 \ | /   \ |
C |/ 16  \|/     \|/ 10  \|
C13-------1-------2-------8
C         |\  5  /|       
C         | \   / |      
C         |9 \ /13|    
C         |  14   |    
C         |  / \  |   
C         | /   \ |  
C         |/ 17  \| 
C         6-------7
C-----------------------------------------
        SELECT CASE (SUBTRIA)
C-----------------------------------------
         CASE(5,9,13,17)
          IX1 = IRTLM(2)
          IX2 = IRTLM(1)
          IX3 = IABS(NVOISIN(1))
          IX4 = IABS(NVOISIN(2))
C-----------------------------------------
         CASE(6,10,14,18)
          IX1 = IRTLM(3)
          IX2 = IRTLM(2)
          IX3 = IABS(NVOISIN(3))
          IX4 = IABS(NVOISIN(4))
C-----------------------------------------
         CASE(7,11,15,19)
          IX1 = IRTLM(4)
          IX2 = IRTLM(3)
          IX3 = IABS(NVOISIN(5))
          IX4 = IABS(NVOISIN(6))
C-----------------------------------------
         CASE(8,12,16,20)
          IX1 = IRTLM(1)
          IX2 = IRTLM(4)
          IX3 = IABS(NVOISIN(7))
          IX4 = IABS(NVOISIN(8))
       END SELECT
       IRTLMV(1) = IX1
       IRTLMV(2) = IX2
       IRTLMV(3) = IX3
       IRTLMV(4) = IX4
       IF (IRTLMV(2)==0) IRTLMV(2)=IRTLMV(1)
       IF (IRTLMV(4)==0) IRTLMV(4)=IRTLMV(3)
C
       RETURN
      END

