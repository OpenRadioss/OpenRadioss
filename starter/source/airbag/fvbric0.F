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
Chd|  FVBRIC0                       source/airbag/fvbric0.F       
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVBRIC0(IBUF   ,IXS    ,ITYP    ,NBRIC   ,
     .                   MONVID ,ILVOUT ,TITR    ,TAGNODBR,
     .                   TBRIC  ,TFAC   ,NB_NODE ,IGRBRIC,IBRIC,FVTYPE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*),IXS(NIXS,*), TAGNODBR(NB_NODE),
     .        NBRIC, MONVID, ILVOUT,
     .        NB_NODE,IBRIC,ITYP, FVTYPE
      INTEGER, DIMENSION(2, NBRIC), INTENT(INOUT) :: TBRIC
      INTEGER, DIMENSION(12, NBRIC), INTENT(INOUT) :: TFAC
      CHARACTER*nchartitle,
     .        TITR
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: ADSKY, ADDCNEL, CNE
      INTEGER I, J, NG, NNO, NFAC, NV, NG2, NNF, NNF2, NNOF, NNOF2, 
     .        NC, NNO2, K, TNC(4), TNV(5,6), KK, L, NN1, K2, 
     .        LL, NTYPE, NTYPE2, NODEID 
      INTEGER, TARGET :: REDIRT(4), REDIRP(6), REDIRB(8), REDIRPY(5)
      INTEGER, DIMENSION(:), POINTER :: REDIR2, REDIR
C
      INTEGER, TARGET :: FAC4(3,4), FAC8(4,6), FAC6(4,5), NOD6(5)
      INTEGER, TARGET ::  FAC5(4,5), NOD5(5), NFACE(4), NNODE(4), KFACE, KFACE2, NOD8(6), NOD3(4)
      INTEGER :: IAD, IAD1, IAD2, NNODEF(4), N1(8), N2(8), IP1(8), IP2(8), IS, NB_COMMON_NODE
      INTEGER, DIMENSION(:, :), POINTER :: FAC, FAC2
      INTEGER, DIMENSION(:), POINTER :: NOD, NOD2
      DATA FAC4 /1,5,3,
     .           3,5,6,
     .           6,5,1,
     .           1,3,6/
      DATA FAC8 /1,4,3,2,
     .           5,6,7,8,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7,
     .           4,1,5,8/
      DATA FAC6 /1,3,2,0,
     .           5,6,7,0,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7/
      DATA NOD6 /3,3,4,4,4/
      DATA NOD8 /4,4,4,4,4,4/
      DATA NOD3 /3,3,3,3/
      DATA FAC5 /1,2,5,0,
     .           2,3,5,0,
     .           3,4,5,0,
     .           4,1,5,0,
     .           1,4,3,2/
      DATA NOD5 /3,3,3,3,4/
      DATA NFACE/6,4,5,5/
      DATA NNODE/8,4,6,5/
      DATA NNODEF/4, 3, 4, 4/
      LOGICAL :: ERROR_RAISED, FACE_OK, FACE2_OK
C     INITIALIZE ERROR FLAG
      ERROR_RAISED = .FALSE.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C     TETRA indirection
      REDIRT(1)=1
      REDIRT(2)=3
      REDIRT(3)=5
      REDIRT(4)=6
C     BRICK indirection
      DO K = 1, 8
         REDIRB(K) = K
      ENDDO
C     PENTA indirection
      REDIRP(1)=1
      REDIRP(2)=2
      REDIRP(3)=3
      REDIRP(4)=5
      REDIRP(5)=6
      REDIRP(6)=7
C     PYRAMIDE indirection
      DO K = 1, 5
         REDIRPY(K) = K
      ENDDO

      ALLOCATE(ADDCNEL(NB_NODE + 1), ADSKY(NB_NODE + 1))
      DO I = 1, NB_NODE
         ADDCNEL(I) = 0
         TAGNODBR(I) = 0
      ENDDO

      ADDCNEL(NB_NODE + 1)  = 0

      IF (ILVOUT >=1 ) THEN
        WRITE(ISTDO,'(A,I8)') '    --> FVMBAG ID: ',MONVID
        WRITE(ISTDO,'(8X,A)') 'BUILDING ELEMENT CONNECTIVITY'
      ENDIF

      DO I = 1, NBRIC
         IF (ITYP == 1) THEN
!   brick
           NG = IGRBRIC(IBRIC)%ENTITY(I)
         ELSEIF (ITYP == 2) THEN
!   tetra
           NG = IBUF(I)
         ENDIF
         TFAC(1:12, I) = 0
         IF (IXS(9,NG) == IXS(6,NG).AND.IXS(8,NG) == IXS(7,NG).AND.
     .        IXS(5,NG) == IXS(4,NG).AND.IXS(3,NG) == IXS(2,NG)) THEN
            NNO=4               !TETRAEDRE
            NTYPE=2
            REDIR => REDIRT(1:4)
         ELSEIF (IXS(9,NG) == IXS(6,NG).AND.IXS(5,NG) == IXS(2,NG)) THEN
            NNO=6               !PENTAEDRE 6 NOEUDS
            NTYPE=3
            REDIR => REDIRP(1:6)
         ELSEIF ( IXS(6,NG) == IXS(9,NG).AND.IXS(7,NG) == IXS(9,NG).AND.
     .           IXS(8,NG) == IXS(9,NG)) THEN
            NNO=5               !PYRAMIDE
            NTYPE=4
            REDIR => REDIRPY(1:5)
         ELSEIF( (IXS(2,NG) == IXS(6,NG).AND.IXS(5,NG) == IXS(9,NG))
     .           .OR.(IXS(2,NG) == IXS(3,NG).AND.IXS(6,NG) == IXS(7,NG))
     .           .OR.(IXS(2,NG) == IXS(6,NG).AND.IXS(3,NG) == IXS(7,NG))
     .           .OR.(IXS(3,NG) == IXS(7,NG).AND.IXS(4,NG) == IXS(8,NG))
     .           .OR.(IXS(3,NG) == IXS(4,NG).AND.IXS(7,NG) == IXS(8,NG))
     .           .OR.(IXS(4,NG) == IXS(5,NG).AND.IXS(8,NG) == IXS(9,NG))
     .           .OR.(IXS(4,NG) == IXS(8,NG).AND.IXS(5,NG) == IXS(9,NG))
     .           .OR.(IXS(6,NG) == IXS(7,NG).AND.IXS(8,NG) == IXS(9,NG))
     .           .OR.(IXS(7,NG) == IXS(8,NG).AND.IXS(6,NG) == IXS(9,NG))
     .           .OR.(IXS(2,NG) == IXS(3,NG).AND.IXS(4,NG) == IXS(5,NG))
     .           .OR.(IXS(2,NG) == IXS(5,NG).AND.IXS(3,NG) == IXS(4,NG)))THEN
            CALL ANCMSG(MSGID=633,
     .           MSGTYPE=MSGERROR,
     .           ANMODE=ANINFO,
     .           I1=MONVID,
     .           C1=TITR)
         ELSE
            NNO=8               !BRIQUE
            NTYPE=1
            REDIR => REDIRB(1:8)
         ENDIF
C     Stode Brick ID and type
         TBRIC(1,I) = NG
         TBRIC(2,I) = NTYPE
         DO K = 1, NNO 
            NODEID = IXS(1 + REDIR(K), NG) + 1
            TAGNODBR(NODEID - 1) = 1
            ADDCNEL(NODEID) = ADDCNEL(NODEID) + 1
         ENDDO
      ENDDO

C     
      ADDCNEL(1) = 1
      DO I = 2, NB_NODE + 1
         ADDCNEL(I) = ADDCNEL(I) + ADDCNEL(I - 1)
      ENDDO
      DO I =  1, NB_NODE
         ADSKY(I) = ADDCNEL(I)
      ENDDO

C     ============================
C     Node -> element connectivity
C     ============================
      ALLOCATE(CNE(ADDCNEL(NB_NODE + 1)))

      DO I = 1, NBRIC
         NG = TBRIC(1, I)
         NTYPE = TBRIC(2, I)
         SELECT CASE(NTYPE)
         CASE (1)
C     BRICK
            DO K = 1, 8
               NODEID = IXS(1 + REDIRB(K), NG)
               CNE(ADSKY(NODEID)) = I
               ADSKY(NODEID) = ADSKY(NODEID) + 1
            ENDDO
         CASE (2)
C     TETRA
            DO K = 1, 4
               NODEID = IXS(1 + REDIRT(K), NG)
               CNE(ADSKY(NODEID)) = I
               ADSKY(NODEID) = ADSKY(NODEID) + 1
            ENDDO
         CASE (3)
C     PENTA
            DO K = 1, 6
               NODEID = IXS(1 + REDIRP(K), NG)
               CNE(ADSKY(NODEID)) = I
               ADSKY(NODEID) = ADSKY(NODEID) + 1
            ENDDO
         CASE (4)
C     PYRA
            DO K = 1, 5
               NODEID = IXS(1 + REDIRPY(K), NG)
               CNE(ADSKY(NODEID)) = I
               ADSKY(NODEID) = ADSKY(NODEID) + 1
            ENDDO
         CASE DEFAULT
C     ERROR
         END SELECT
      ENDDO

C     =========================
C     Finding adjacent elements
C     =========================
      DO I = 1, NBRIC
         !IF (ILVOUT >=1 ) CALL PROGALE_C(I, NBRIC, 4)    !dynamic screen output
         NG = TBRIC(1, I)
         NTYPE = TBRIC(2, I)
         NNO = NNODE(NTYPE)
         NNF = NFACE(NTYPE)
         NNOF = NNODEF(NTYPE)
         SELECT CASE (NTYPE)
         CASE (1) 
            REDIR => REDIRB(1:8)
            FAC => FAC8(1:4, 1:6)
            NOD => NOD8(1:6)
         CASE (2)
            REDIR => REDIRT(1:4)
            FAC => FAC4(1:3, 1:4)
            NOD => NOD3(1:4)
         CASE (3)
            REDIR => REDIRP(1:6)
            FAC => FAC6(1:4, 1:5)
            NOD => NOD6(1:5)
         CASE (4)
            REDIR => REDIRPY(1:5)
            FAC => FAC5(1:4, 1:5)
            NOD => NOD5(1:5)
         CASE DEFAULT
C     ERROR
         END SELECT
C     Keep track of the nodes
         DO K = 1, NNO
            N1(K) = IXS(1 + REDIR(K), NG)
         ENDDO
C     Loop over the nodes
         DO K = 1, NNO
            NODEID = N1(K)
            IAD1 = ADDCNEL(NODEID)
            IAD2 = ADDCNEL(NODEID+1) - 1
C     Loop over the adjacent elements
            DO IAD = IAD1, IAD2
               J = CNE(IAD)
               IF (J  /=  I) THEN
                  NG2 = TBRIC(1, J)
                  NTYPE2 = TBRIC(2, J)
                  NNO2 = NNODE(NTYPE2)
                  NNF2 = NFACE(NTYPE2)
                  NNOF2 = NNODEF(NTYPE2)
                  SELECT CASE (NTYPE2)
                  CASE (1) 
                     REDIR2 => REDIRB(1:8)
                     FAC2 => FAC8(1:4, 1:6)
                     NOD2 => NOD8(1:6)
                  CASE (2)
                     REDIR2 => REDIRT(1:4)
                     FAC2 => FAC4(1:3, 1:4)
                     NOD2 => NOD3(1:4)
                  CASE (3)
                     REDIR2 => REDIRP(1:6)
                     FAC2 => FAC6(1:4, 1:5)
                     NOD2 => NOD6(1:5)
                  CASE (4)
                     REDIR2 => REDIRPY(1:5)
                     FAC2 => FAC5(1:4, 1:5)
                     NOD2 => NOD5(1:5)
                  CASE DEFAULT
C     ERROR
                  END SELECT
                  
                  DO K2 = 1, NNO2
                     N2(K2) = IXS(1 + REDIR2(K2), NG2)
                  ENDDO
                  IP1(1:8) = 0
                  IP2(1:8) = 0
                  NB_COMMON_NODE = 0
                  DO KK = 1, NNO
                     DO K2 = 1, NNO2
                        IF (N1(KK)  ==  N2(K2)) THEN
                           NB_COMMON_NODE = NB_COMMON_NODE + 1
                           IP1(REDIR(KK)) = 1
                           IP2(REDIR2(K2)) = 1
                        ENDIF
                     ENDDO
                  ENDDO
                  IF (NB_COMMON_NODE  >=  3) THEN
C     A common neighbor has been found, now find the face number for I and J
                     DO KFACE = 1, NNF
                        FACE_OK = .FALSE.
                        IS = 0
                        DO KK = 1, NOD(KFACE)
                           IS = IS + IP1(FAC(KK, KFACE))
                        ENDDO
                        IF (IS  ==  NOD(KFACE)) THEN
                           FACE_OK = .TRUE.
                           EXIT
                        ENDIF
                     ENDDO
                     DO KFACE2 = 1, NNF2
                        FACE2_OK = .FALSE.
                        IS = 0
                        DO KK = 1, NOD2(KFACE2)
                           IS = IS + IP2(FAC2(KK, KFACE2))
                        ENDDO
                        IF (IS  ==  NOD2(KFACE2)) THEN
                           FACE2_OK = .TRUE.
                           EXIT
                        ENDIF
                     ENDDO
                     ERROR_RAISED = (NOD(KFACE) == NOD2(KFACE2)) .AND. (NB_COMMON_NODE == NOD(KFACE))
                     ERROR_RAISED = .NOT. ERROR_RAISED
                     IF (FVTYPE  /=  8) THEN 
                        ERROR_RAISED = .FALSE.
                     ENDIF
                     IF (FACE_OK .AND. FACE2_OK .AND. .NOT. ERROR_RAISED) THEN
                        TFAC(2 * (KFACE - 1) + 1, I) = 1
                        TFAC(2 * (KFACE - 1) + 2, I) = J
                        TFAC(2 * (KFACE2 - 1) + 1, J) = 1
                        TFAC(2 * (KFACE2 - 1) + 2, J) = I                     
                     ENDIF
                     IF (ERROR_RAISED) THEN
                           CALL ANCMSG(MSGID=1625,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND,
     .                          I1 = MONVID, C1=TITR, 
     .                          I2 = IXS(NIXS, NG), I3 = IXS(NIXS, NG2))                     
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO

C     ===================
C     Memory deallocation
C     ===================

      DEALLOCATE(ADSKY, ADDCNEL)
      DEALLOCATE(CNE)

C     ===
C     End
C     ===
C
      IF (ERROR_RAISED) THEN
         CALL ARRET(2)
      ENDIF
      RETURN
      END
