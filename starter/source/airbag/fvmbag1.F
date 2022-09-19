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
Chd|  ALENO                         source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE ALENO(NN  , SURF_NODES, NNB, ITABINV,
     2                 TAGE, TAGVENT ,TAGNODBR, T_MONVOLN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, NNB,
     .        ITABINV(*), TAGE(*), TAGVENT(NUMNOD),
     .        TAGNODBR(NUMNOD),SURF_NODES(NN,4)
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITAG(NUMNOD), I, I1, I2, I3, I4, IAD1,
     .        J,K,KK,NA,NB,NC,NA2,NC2,
     .        IADC, IADFIN, JAD1, NALL, NODESURF(4*NN)
C
      INTEGER IADCH(NUMNOD),CHAIN(2,4*NN),NEXT(4*NN),
     .        IP2(0:3),INVP2(15)
C-----------------------------------------------
C
C elimination des elements doubles
C

      DO I=1,NUMNOD
         IADCH(I)=0
         ITAG(I)=0
         TAGVENT(I)=0
      ENDDO

! temporary nodal surface array

      IAD1 = 1
      DO I=1,NN
        DO K=1,4
          NODESURF(IAD1) = SURF_NODES(I,K)
          IAD1=IAD1+1
        ENDDO
      ENDDO

C construction liste chaine connectivites inverses (noeuds/elts)

      IAD1=1
      IADFIN = 1
      DO I=1,NN
        TAGE(I)=0
        DO K = 0,3
          J=NODESURF(IAD1+K)
          IF(K == 3) THEN
            IF(J == NODESURF(IAD1+K-1)) J=0
          END IF
          IF(J == 0) CYCLE
          IADC = IADCH(J)
          IF(IADC == 0)THEN
            IADCH(J) = IADFIN
          ELSE
            DO WHILE (NEXT(IADC) /= 0)
              IADC = NEXT(IADC)
            ENDDO
            NEXT(IADC) = IADFIN
          ENDIF
          CHAIN(1,IADFIN)=IAD1
          CHAIN(2,IADFIN)=I
          NEXT(IADFIN) =0
          IADFIN = IADFIN+1
        ENDDO
        IAD1=IAD1 + 4
      ENDDO
C
      IP2(0) = 1
      IP2(1) = 2
      IP2(2) = 4
      IP2(3) = 8
      INVP2(7)=4
      INVP2(11)=3
      INVP2(13)=2
      INVP2(14)=1
C 

C  Recherche elts doubles

      IAD1=1
      DO I=1,NN
        NA=0
        NALL=1
C tag noeuds 1er elt
        DO K = 0,3
          J = NODESURF(IAD1+K)
          IF(K == 3) THEN
            IF(J == NODESURF(IAD1+K-1)) J=0
          END IF
          IF(J /= 0) THEN
            ITAG(J)=100+IP2(K)
            NA = NA+1
            NALL=NALL*TAGNODBR(J)
          ENDIF
        ENDDO
        IF(NALL == 1) GO TO 10
C
        DO K = 0,3
          J=NODESURF(IAD1+K)
          IF(K == 3) THEN
            IF(J == NODESURF(IAD1+K-1)) J=0
          END IF
          IF(J == 0) CYCLE
          IADC = IADCH(J)
C boucle sur les elts connecte au noeud k+1 du 1er elt
          DO WHILE (IADC /= 0)
            NC=0
            NB=0
            JAD1=CHAIN(1,IADC)
            IF(JAD1 /= IAD1) THEN
C Denombrement des tags des noeuds du 2eme elt
              DO KK = 0,3
                J = NODESURF(JAD1+KK)
                IF(KK == 3) THEN
                  IF(J == NODESURF(JAD1+KK-1)) J=0
                END IF
                IF(J /= 0)THEN
                  NC = NC+ITAG(J)
                  NB = NB+1
                ENDIF
              ENDDO
              NC2 = NC
              NC = NC / 100
              NC2 = NC2 - 100*NC
C test pour 2 triangles ou 2 quadrangles avec ts les noeuds communs
              IF(NC == NA .and. NA == NB)THEN
                TAGE(I)=5
C test pour 2 elts (2 quadrangles ou 1 triangle et 1 quadrangle) avec 3 noeuds communs
              ELSEIF(NC == 3)THEN
                IF(NA == 4)THEN
C le 1er est un quadrangle  
                  TAGE(I)=INVP2(NC2) ! tag numero noeud libre
                ELSE
C le 1er est un triangle  
                  TAGE(I)=5
                ENDIF
              ENDIF
            ENDIF
            IADC = NEXT(IADC)
          ENDDO
        ENDDO
 10     CONTINUE
        DO K = 0,3
          J = NODESURF(IAD1+K)
          IF(K == 3) THEN
            IF(J == NODESURF(IAD1+K-1)) J=0
          END IF
          IF(J /= 0) ITAG(J)=0
        ENDDO
        IAD1=IAD1 + 4
      ENDDO

      NC = 0
      NC2 = 0
      DO I=1,NN
        IF(TAGE(I) == 5)THEN
          NC = NC + 1
        ELSEIF(TAGE(I) /= 0)THEN
          NC2 = NC2 + 1
        END IF
      END DO

c fin elimination des elements doubles

      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      IAD1=1
      DO I=1,NN
        I1=NODESURF(IAD1)
        I2=NODESURF(IAD1+1)
        I3=NODESURF(IAD1+2)
        I4=NODESURF(IAD1+3)
        IF(TAGE(I) == 0)THEN
          ITAG(I1)=1
          ITAG(I2)=1
          ITAG(I3)=1
          IF (I4 /= 0) ITAG(I4)=1
        ELSEIF(TAGE(I) == 1)THEN
          ITAG(I1)=1
          ITAG(I2)=1
          ITAG(I4)=1
          TAGVENT(I3)=1
        ELSEIF(TAGE(I) == 2)THEN
          ITAG(I2)=1
          ITAG(I3)=1
          ITAG(I1)=1
          IF (I4 /= 0) TAGVENT(I4)=1
        ELSEIF(TAGE(I) == 3)THEN
          ITAG(I3)=1
          ITAG(I4)=1
          ITAG(I2)=1
          TAGVENT(I1)=1
        ELSEIF(TAGE(I) == 4)THEN
          ITAG(I4)=1
          ITAG(I1)=1
          ITAG(I3)=1
          TAGVENT(I2)=1
        ELSEIF(TAGE(I) == 5)THEN
          TAGVENT(I1)=1
          TAGVENT(I2)=1
          TAGVENT(I3)=1
          IF (I4 /= 0) TAGVENT(I4)=1
        ENDIF
        IAD1=IAD1 + 4
      ENDDO
      NNB=0
      DO I=1,NUMNOD
         IF (ITAG(I)==1) THEN
           NNB=NNB+1
           ITABINV(I)=NNB
         ENDIF
      END DO
      T_MONVOLN%NNS = NNB
      ALLOCATE(T_MONVOLN%NODES(NNB))
      NNB = 0
      DO I=1,NUMNOD
         IF (ITAG(I)==1) THEN
           NNB=NNB+1
           T_MONVOLN%NODES(NNB)=I
         ENDIF
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  ALETG                         source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE ALETG(T_MONVOLN, ELEM_ID, NN, SURF, NTG,
     .     ITABINV, TAGE, NNO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, NTG, NNO
      INTEGER ITABINV(*), TAGE(*)
      INTEGER, INTENT(INOUT) :: ELEM_ID(*)
      TYPE(SURF_), INTENT(IN) :: SURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, I3, I4, IAD1
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: TBEMTG
C
      ALLOCATE(TBEMTG(3, 2 * NN))
      TBEMTG(1:3, 1:2 * NN) = 0
      NTG=0
      DO I=1,NN
        IF(TAGE(I) == 5)CYCLE
        I1=SURF%NODES(I,1)
        I2=SURF%NODES(I,2)
        I3=SURF%NODES(I,3)
        I4=SURF%NODES(I,4)
        IF(I3 == I4) I4 = 0
        IF(TAGE(I) == 0)THEN
          IF (I4==0) THEN
C conservation du triangle
            NTG=NTG+1
            TBEMTG(1,NTG)=ITABINV(I1)
            TBEMTG(2,NTG)=ITABINV(I2)
            TBEMTG(3,NTG)=ITABINV(I3)
            ELEM_ID(NTG) = SURF%ELEM(I)
          ELSE
C decoupage du quadrangle en 2 triangles
            NTG=NTG+1
            TBEMTG(1,NTG)=ITABINV(I1)
            TBEMTG(2,NTG)=ITABINV(I2)
            TBEMTG(3,NTG)=ITABINV(I4)
            ELEM_ID(NTG) = SURF%ELEM(I)
            IF (ITABINV(I4)/=ITABINV(I3)) THEN
              NTG=NTG+1
              TBEMTG(1,NTG)=ITABINV(I2)
              TBEMTG(2,NTG)=ITABINV(I3)
              TBEMTG(3,NTG)=ITABINV(I4)
              ELEM_ID(NTG) = SURF%ELEM(I)
            ENDIF
          ENDIF
C Remplacement du quadrangle en 1 triangle en conservant le noeud libre et ses 2 noeuds adjacents
        ELSEIF(TAGE(I) == 1)THEN
          NTG=NTG+1
          TBEMTG(1,NTG)=ITABINV(I1)
          TBEMTG(2,NTG)=ITABINV(I2)
          TBEMTG(3,NTG)=ITABINV(I4)
          ELEM_ID(NTG) = SURF%ELEM(I)
       ELSEIF(TAGE(I) == 2)THEN
          NTG=NTG+1
          TBEMTG(1,NTG)=ITABINV(I2)
          TBEMTG(2,NTG)=ITABINV(I3)
          TBEMTG(3,NTG)=ITABINV(I1)
          ELEM_ID(NTG) = SURF%ELEM(I)
        ELSEIF(TAGE(I) == 3)THEN
          NTG=NTG+1
          TBEMTG(1,NTG)=ITABINV(I3)
          TBEMTG(2,NTG)=ITABINV(I4)
          TBEMTG(3,NTG)=ITABINV(I2)
          ELEM_ID(NTG) = SURF%ELEM(I)
        ELSEIF(TAGE(I) == 4)THEN
          NTG=NTG+1
          TBEMTG(1,NTG)=ITABINV(I4)
          TBEMTG(2,NTG)=ITABINV(I1)
          TBEMTG(3,NTG)=ITABINV(I3)
          ELEM_ID(NTG) = SURF%ELEM(I)
        ENDIF
      ENDDO
      T_MONVOLN%NTG = NTG
      ALLOCATE(T_MONVOLN%ELEM(3, NTG))
      DO I = 1, NTG
         T_MONVOLN%ELEM(1:3, I) = TBEMTG(1:3, I)
      ENDDO
C     
      DEALLOCATE(TBEMTG)
      RETURN
      END
Chd|====================================================================
Chd|  FVBAG_TAGELE                  source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE FVBAG_TAGELE(ELEM_ID, NN, SURF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN
      INTEGER, INTENT(INOUT) :: ELEM_ID(*)
      TYPE(SURF_), INTENT(IN) :: SURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, I3, I4, IAD1, NTG
C
      NTG=0
      DO I=1,NN
        I1=SURF%NODES(I,1)
        I2=SURF%NODES(I,2)
        I3=SURF%NODES(I,3)
        I4=SURF%NODES(I,4)
        IF(I3 == I4) I4 = 0
        IF (I4==0) THEN
C     conservation du triangle
           NTG=NTG+1
           ELEM_ID(NTG) = SURF%ELEM(I)
        ELSE
C     decoupage du quadrangle en 2 triangles
           NTG=NTG+1
           ELEM_ID(NTG) = SURF%ELEM(I)
           NTG=NTG+1
           ELEM_ID(NTG) = SURF%ELEM(I)
        ENDIF
      ENDDO
C     
      RETURN
      END
Chd|====================================================================
Chd|  FVVERIF                       source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVVERIF(NELA, ELEMA, X    , MONVID,
     .     VX3 , VY3  , VZ3  , VX1   , VY1,
     .     VZ1 , XB0  , YB0  , ZB0   ,
     .     LX , LY , LZ, IBUF , IBUFA, TAGELA,
     .     TITR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NELA, MONVID
      INTEGER, DIMENSION(3, NELA), INTENT(IN) :: ELEMA
      INTEGER, INTENT(IN) :: IBUF(*), IBUFA(*), TAGELA(*)
      my_real, INTENT(INOUT) :: LX, LY, LZ
      CHARACTER(LEN = nchartitle), INTENT(IN) :: TITR
      my_real
     .     X(3,*), VX3, VY3, VZ3, VX1, VY1, VZ1, XB0, YB0, ZB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NUMNOD), IAD1, I1, I2, I3, I4, ISH34
      my_real
     .        NORM, SS, VX2, VY2, VZ2, LXMAX, LYMAX, XX, YY, ZZ, XL, YL,
     .        LZMAX, ZL, LX_OLD, LY_OLD, LZ_OLD
C
      NORM=SQRT(VX3**2+VY3**2+VZ3**2)
      IF (NORM==ZERO) THEN
         CALL ANCMSG(MSGID=630,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MONVID,
     .               C1=TITR)
         RETURN
      ENDIF
      VX3=VX3/NORM
      VY3=VY3/NORM
      VZ3=VZ3/NORM
      SS=VX3*VX1+VY3*VY1+VZ3*VZ1
      VX1=VX1-SS*VX3
      VY1=VY1-SS*VY3
      VZ1=VZ1-SS*VZ3
      NORM=SQRT(VX1**2+VY1**2+VZ1**2)
      IF (NORM==ZERO) THEN
         CALL ANCMSG(MSGID=630,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MONVID,
     .               C1=TITR)
         RETURN
      ENDIF
      VX1=VX1/NORM
      VY1=VY1/NORM
      VZ1=VZ1/NORM
      VX2=VY3*VZ1-VZ3*VY1
      VY2=VZ3*VX1-VX3*VZ1
      VZ2=VX3*VY1-VY3*VX1
C Limites de l'airbag dans les directions de decoupage
      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      DO I=1,NELA
         I1=ELEMA(1,I)
         I2=ELEMA(2,I)
         I3=ELEMA(3,I)
         IF (TAGELA(I)>0) THEN
            I1=IBUF(I1)
            I2=IBUF(I2)
            I3=IBUF(I3)
         ELSE
            I1=IBUFA(I1)
            I2=IBUFA(I2)
            I3=IBUFA(I3)
         ENDIF
         ITAG(I1)=1
         ITAG(I2)=1
         ITAG(I3)=1
      ENDDO
C
      LXMAX=ZERO
      LYMAX=ZERO
      LZMAX=ZERO
      DO I=1,NUMNOD
         IF (ITAG(I)==1) THEN
            XX=X(1,I)
            YY=X(2,I)
            ZZ=X(3,I)
C
            XL=(XX-XB0)*VX1+(YY-YB0)*VY1+(ZZ-ZB0)*VZ1
            YL=(XX-XB0)*VX2+(YY-YB0)*VY2+(ZZ-ZB0)*VZ2
            ZL=(XX-XB0)*VX3+(YY-YB0)*VY3+(ZZ-ZB0)*VZ3
            LXMAX=MAX(LXMAX,ABS(XL))
            LYMAX=MAX(LYMAX,ABS(YL))
            LZMAX=MAX(LZMAX,ABS(ZL))
         ENDIF
      ENDDO
C
      IF (LXMAX>LX) THEN
         LX_OLD=LX
         LX=LXMAX*ONEP01
         CALL ANCMSG(MSGID=631,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MONVID,
     .               C1=TITR,
     .               R1=LX_OLD,
     .               R3=LX,
     .               R2=LXMAX,
     .               I2=1)
      ENDIF
      IF (LYMAX>LY) THEN
         LY_OLD=LY
         LY=LYMAX*ONEP01
         CALL ANCMSG(MSGID=631,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MONVID,
     .               C1=TITR,
     .               R3=LY,
     .               R1=LY_OLD,
     .               R2=LYMAX,
     .               I2=2)
      ENDIF
CFA je ne comprend pas pourquoi cette partie est en commentaire: cela pose un probl   e si l'on rentre LZ=0
C   LZ et DZ restent nul dans FVMESH
C On autorise le decoupage horizontal d'une bande du maillage uniquement
C      IF (LZMAX>LZ) THEN
C         CALL ANSTCKI(ID)
C         CALL ANSTCKI(3)
C         CALL ANSTCKR(LZ)
C         CALL ANSTCKR(LZMAX)
C         LZ=LZMAX*ONEP01
C         CALL ANSTCKR(LZ)
C         CALL ANCWARN(631,ANINFO_BLIND_1)
C      ENDIF
      IF (LZMAX>LZ) THEN
         LZ_OLD=LZ
         LZ=LZMAX*ONEP01
         CALL ANCMSG(MSGID=631,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MONVID,
     .               C1=TITR,
     .               R3=LZ,
     .               R1=LZ_OLD,
     .               R2=LZMAX,
     .               I2=3)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  FVNODI                        source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE FVNODI(NN, SURF_NODES, NNB, ITABINV, T_MONVOLN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NN
      INTEGER, DIMENSION(NN, 4), INTENT(IN) :: SURF_NODES
      INTEGER, INTENT(OUT) ::  NNB
      INTEGER, DIMENSION(NUMNOD), INTENT(INOUT) :: ITABINV
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, I3, I4, IAD1, NNS
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFNODE
      INTEGER :: IFOUND
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG, ITAG_SURFEXT, ITABINV_SURFEXT
      INTEGER(8) :: VEC_PTR_SURFINT

      ALLOCATE(ITAG(NUMNOD), ITAG_SURFEXT(NUMNOD), ITABINV_SURFEXT(NUMNOD))

      DO I = 1, NUMNOD
         ITAG(I) = 0
         ITAG_SURFEXT(I) = 0
         ITABINV_SURFEXT(I) = 0
      ENDDO

      NNS = T_MONVOLN%NNS
      ALLOCATE(BUFNODE(NNS))
      DO I = 1, NNS
         BUFNODE(I) = T_MONVOLN%NODES(I)
!     FLAG NODES OF EXTERNAL SURFACE
         ITAG_SURFEXT(T_MONVOLN%NODES(I)) = 1
!     KEEP INDEX IN ITABINV_SURFEXT TAB
         ITABINV_SURFEXT(T_MONVOLN%NODES(I)) = I
      ENDDO

!     FLAG NODES OF INTERNAL SURFACE
      DO I=1,NN
        I1=SURF_NODES(I,1)
        I2=SURF_NODES(I,2)
        I3=SURF_NODES(I,3)
        I4=SURF_NODES(I,4)
        ITAG(I1)=1
        ITAG(I2)=1
        ITAG(I3)=1
        IF (I4 /= 0) ITAG(I4)=1
      ENDDO

!     COUNT AND BUILD INDIRECTION TAB OF INTERNAL SURFACE NODES
!     THAT DO NOT ALREADY APPEAR IN EXTERNAL SURFACE
      CALL INTVECTOR_CREATE(VEC_PTR_SURFINT)
      NNB = 0
      DO I = 1, NUMNOD
         IF (ITAG(I) == 1) THEN
            IF (ITAG_SURFEXT(I) /= 1) THEN
!     Node that does not already appear in external surface
               NNB = NNB + 1
               ITABINV(I) = NNB + NNS
               CALL INTVECTOR_PUSH_BACK(VEC_PTR_SURFINT, I)
            ELSE
               ITABINV(I) = ITABINV_SURFEXT(I)
            ENDIF
         ENDIF
      END DO

      DEALLOCATE(T_MONVOLN%NODES)
      ALLOCATE(T_MONVOLN%NODES(NNB + NNS))
      DO I = 1, NNS
         T_MONVOLN%NODES(I) = BUFNODE(I)
      ENDDO
      T_MONVOLN%NNI = NNB

      CALL INTVECTOR_COPY_TO(VEC_PTR_SURFINT, T_MONVOLN%NODES(NNS + 1))

      CALL INTVECTOR_DELETE(VEC_PTR_SURFINT)
      DEALLOCATE(BUFNODE)
      DEALLOCATE(ITAG)
      DEALLOCATE(ITAG_SURFEXT)
      DEALLOCATE(ITABINV_SURFEXT)
C
      RETURN
      END
Chd|====================================================================
Chd|  FVTGI                         source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE FVTGI(T_MONVOLN, ELEM_Id, NN, SURF, NTG, ITABINV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, NTG
      INTEGER ITABINV(*),ELEM_ID(*)
      TYPE(SURF_), INTENT(IN) :: SURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, I3, I4, IAD1, J
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: ELEMTG, BUFELEM
      ALLOCATE(ELEMTG(3, 2 * NN), BUFELEM(3, T_MONVOLN%NTG))
      DO I = 1, T_MONVOLN%NTG
         BUFELEM(1:3, I) = T_MONVOLN%ELEM(1:3, I)
      ENDDO
      DEALLOCATE(T_MONVOLN%ELEM)
C
      NTG=0
      DO I=1,NN
        I1=SURF%NODES(I,1)
        I2=SURF%NODES(I,2)
        I3=SURF%NODES(I,3)
        I4=SURF%NODES(I,4)
        IF(I3 == I4) I4 = 0
          IF (I4==0) THEN
C conservation du triangle
            NTG=NTG+1
            ELEMTG(1,NTG)=ITABINV(I1)
            ELEMTG(2,NTG)=ITABINV(I2)
            ELEMTG(3,NTG)=ITABINV(I3)
            ELEM_ID(NTG) = SURF%ELEM(I)
          ELSE
C decoupage du quadrangle en 2 triangles
            NTG=NTG+1
            ELEMTG(1,NTG)=ITABINV(I1)
            ELEMTG(2,NTG)=ITABINV(I2)
            ELEMTG(3,NTG)=ITABINV(I3)
            ELEM_ID(NTG) = SURF%ELEM(I)
            IF (ITABINV(I4)/=ITABINV(I3)) THEN
              NTG=NTG+1
              ELEMTG(1,NTG)=ITABINV(I1)
              ELEMTG(2,NTG)=ITABINV(I3)
              ELEMTG(3,NTG)=ITABINV(I4)
              ELEM_ID(NTG) = SURF%ELEM(I)
            ENDIF
          ENDIF
      ENDDO
      T_MONVOLN%NTGI = NTG
      ALLOCATE(T_MONVOLN%ELEM(3, T_MONVOLN%NTG + T_MONVOLN%NTGI))
      DO I = 1, T_MONVOLN%NTG
         T_MONVOLN%ELEM(1:3, I) = BUFELEM(1:3, I)
      ENDDO
      DO I = 1, T_MONVOLN%NTGI
         T_MONVOLN%ELEM(1:3, I + T_MONVOLN%NTG) = ELEMTG(1:3, I)
      ENDDO
C
      DEALLOCATE(ELEMTG, BUFELEM)
      RETURN
      END
Chd|====================================================================
Chd|  FVNODBR                       source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|====================================================================
      SUBROUTINE FVNODBR(IBUFA, NNA, NNFV, IFV, NB_NODE)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUFA(*), NNA, NNFV, IFV, NB_NODE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, ITAB(NB_NODE)
C
C Save local number of a brick node in IFVNOD(3)
C
      DO I=1,NB_NODE
         ITAB(I)=0
      ENDDO
C
      DO I=1,NNA
        N=IBUFA(I)
        ITAB(N)=I
      ENDDO
C
      DO I=1,NNFV
        IF(FVDATA(IFV)%IFVNOD(1,I)/=2) CYCLE
          N=FVDATA(IFV)%IFVNOD(2,I)
          FVDATA(IFV)%IFVNOD(3,I)=ITAB(N)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FVNORMAL                      source/airbag/fvmbag1.F       
Chd|-- called by -----------
Chd|        FVBRIC1                       source/airbag/fvbric1.F       
Chd|        FVELAREA                      source/airbag/fvelarea.F      
Chd|        FVLENGTH                      source/airbag/fvlength.F      
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVVOLU                        source/airbag/fvvolu.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVNORMAL(X, N1, N2, N3, N4, NX, NY, NZ)
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
      INTEGER N1, N2, N3, N4
      my_real
     .        X(3,*), NX, NY, NZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      my_real
     .        X1,  Y1,  Z1,  X2, Y2, Z2, X3,  Y3,  Z3, X12, Y12, Z12,
     .        X13, Y13, Z13, X4, Y4, Z4, X24, Y24, Z24
C-----------------------------------------------------------------------
C Compute normal vector for a triangle and a quad
C-----------------------------------------------------------------------
      IF(N4==0) THEN
            X1=X(1,N1)
            Y1=X(2,N1)
            Z1=X(3,N1)
            X2=X(1,N2)
            Y2=X(2,N2)
            Z2=X(3,N2)
            X3=X(1,N3)
            Y3=X(2,N3)
            Z3=X(3,N3)
            X12=X2-X1
            Y12=Y2-Y1
            Z12=Z2-Z1
            X13=X3-X1
            Y13=Y3-Y1
            Z13=Z3-Z1
            NX=Y12*Z13-Z12*Y13
            NY=Z12*X13-X12*Z13
            NZ=X12*Y13-Y12*X13
      ELSE
            X1=X(1,N1)
            Y1=X(2,N1)
            Z1=X(3,N1)
            X2=X(1,N2)
            Y2=X(2,N2)
            Z2=X(3,N2)
            X3=X(1,N3)
            Y3=X(2,N3)
            Z3=X(3,N3)
            X4=X(1,N4)
            Y4=X(2,N4)
            Z4=X(3,N4)
            X13=X3-X1
            Y13=Y3-Y1
            Z13=Z3-Z1
            X24=X4-X2
            Y24=Y4-Y2
            Z24=Z4-Z2
            NX=Y13*Z24-Z13*Y24
            NY=Z13*X24-X13*Z24
            NZ=X13*Y24-Y13*X24
      ENDIF
      RETURN
      END
