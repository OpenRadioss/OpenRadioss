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
C
Chd|====================================================================
Chd|  INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FMT_DEF                       source/starter/fmt_def.F      
Chd|        FREDEC_2KEY_4ID_T             source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        NEXTSLA                       source/starter/freform.F      
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        USRTOS                        source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE INIVEL(V         ,W      ,ITAB     ,ITABM1 ,VR       ,
     .                  IGRNOD    ,IGRBRIC,ISKN     ,SKEW   ,INIVIDS  ,
     .                  X         ,UNITAB ,LSUBMODEL,RTRANS ,XFRAME   ,
     .                  IFRAME    ,VFLOW  ,WFLOW    ,KXSP   ,MULTI_FVM,
     .                  FVM_INIVEL,IGRQUAD,IGRSH3N  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ITAB(*), ITABM1(*),ISKN(LISKN,*),
     .        INIVIDS(*),IFRAME(LISKN,*),KXSP(NISP,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real
     .   V(3,*),W(3,*),VR(3,*),SKEW(LSKEW,*),BID,X(3,*),
     .   RTRANS(NTRANSF,*),XFRAME(NXFRAME,*),VFLOW(3,*) ,WFLOW(3,*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(FVM_INIVEL_STRUCT), INTENT(INOUT) :: FVM_INIVEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,KPRI,KROT,NNOD,NOSYS,ITYPE,ID,ISK,IGR,IGRS,NBVEL,
     .        UID,IFLAGUNIT,NODID,NODID1,SUB_ID,IDIR,
     .        IDGRBRICK, IDGRQUAD, IDGRSH3N, IDGRBRICK_LOC, IDGRQUAD_LOC, IDGRSH3N_LOC,
     .        IAD1, IAD2,NODE,NL,JREC,NOD_COUNT,IAD,NODINIVEL
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IFRA,IFM,IUN,JJ,K1,K2,K3,INOD
      my_real
     .   V1, V2, V3, VL1, VL2, VL3,
     .   NIXJ(6), FAC_L,FAC_T,FAC_V,VR1,VR2,VR3,VRL1,VRL2,VRL3
      LOGICAL LV,LVR
      CHARACTER MESS*40,LLINE*ncharline,TITR*nchartitle,KEY*ncharkey,
     .          XYZ*ncharfield
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_VEL,NOD_VEL,NVEL
      my_real, DIMENSION(:,:), ALLOCATABLE :: V_NODE,VR_NODE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS, USRTOS, KFRAME
      DATA MESS/'INITIAL VELOCITIES DEFINITION           '/
      DATA IUN/1/
C=======================================================================
      FLAG_FMT = 0
      NBVEL = 0
      ISK   = 0
      IFRA  = 0
      IFM   = 0
      K1    = 0
      K2    = 0
      K3    = 0
      IDIR  = 0
C--------------------------------------------------
C     V INI DANS FICHIER Ynn
C--------------------------------------------------
      IF (ISIGI==3.OR.ISIGI==4.OR.ISIGI==5) THEN
  120   READ(IIN4,FMT='(A)',END=199,ERR=199)LLINE
  122   IF(LLINE(1:31)/='/NODAL     /VECTOR    /VELOCITY')GOTO 120
        READ(IIN4,FMT='(A)',END=199,ERR=199)LLINE
C
  125   READ(IIN4,FMT='(A)',END=130,ERR=199)LLINE
        IF(LLINE(1:1)=='#')GOTO 125
        IF(LLINE(1:1)=='/')GOTO 130
          IF (IOUTP_FMT==2) THEN
            READ(LLINE,'(I8,3F16.0)')N,V1,V2,V3
          ELSE
            READ(LLINE,'(I10,3F20.0)')N,V1,V2,V3
          ENDIF
          I = USRTOS(N,ITABM1)
          IF(I/=0)THEN
            V(1,I) = V1
            V(2,I) = V2
            V(3,I) = V3
! we don't have the velocity outp for gaz for law 77            
!!            IF(IALELAG > 0) THEN
!!              VFLOW(1,I) = V1
!!              VFLOW(2,I) = V2
!!              VFLOW(3,I) = V3
C              
!!              WFLOW(1,I) = V1
!!              WFLOW(2,I) = V2
!!              WFLOW(3,I) = V3
!!            ENDIF
          END IF
        GOTO 125
 130    CONTINUE
 199    CONTINUE
        REWIND(IIN4)
C-------------------
  220   READ(IIN4,FMT='(A)',END=299,ERR=299)LLINE
  222   IF(LLINE(1:31)/='/NODAL     /VECTOR    /VELO_ROT')GOTO 220
        READ(IIN4,FMT='(A)',END=299,ERR=299)LLINE
C
  225   READ(IIN4,FMT='(A)',END=230,ERR=299)LLINE
        IF(LLINE(1:1)=='#')GOTO 225
        IF(LLINE(1:1)=='/')GOTO 230
          IF (IOUTP_FMT==2) THEN
            READ(LLINE,'(I8,3F16.0)')N,V1,V2,V3
          ELSE
            READ(LLINE,'(I10,3F20.0)')N,V1,V2,V3
          ENDIF
          I = USRTOS(N,ITABM1)
          IF(I/=0)THEN
            VR(1,I) = V1
            VR(2,I) = V2
            VR(3,I) = V3
          END IF
        GOTO 225
 230    CONTINUE
 299    CONTINUE
        REWIND(IIN4)
      ENDIF
!
C--------------------------------------------------
C     V INI DANS FICHIER D00
C--------------------------------------------------
!---
!     KEY = 'NODE', temporary velocity table allocation
!
!  start count
      NOD_COUNT = 0
      ALLOCATE(IAD_VEL(NINVEL_FILE),NVEL(NINVEL_FILE))
      IAD_VEL = 0
      NVEL = 0
      KCUR = KINIV
!
      IREC = KOPTAD(KCUR)
      IREC=IREC+1
      JREC=IREC
      DO I=1,NINVEL_FILE
        KLINE=' '
        JREC=JREC+2
        N = 0
        DO WHILE(KLINE(1:1)/='/')
          READ(IIN,REC=JREC,ERR=999,FMT='(A)')KLINE
          JREC=JREC+1
          N = N + 1
        ENDDO
        NVEL(I) = N/2
        NOD_COUNT = NOD_COUNT + NVEL(I)
      ENDDO ! DO I=1,NINVEL
!  end count
      IF (NOD_COUNT > 0) THEN
        ALLOCATE(NOD_VEL(NOD_COUNT),V_NODE(3,NOD_COUNT),VR_NODE(3,NOD_COUNT))
        NOD_VEL(1:NOD_COUNT) = 0
        V_NODE(1:3,1:NOD_COUNT)  = ZERO
        VR_NODE(1:3,1:NOD_COUNT) = ZERO
      ELSE
        ALLOCATE(NOD_VEL(0),V_NODE(0,0),VR_NODE(0,0))
      ENDIF
!---
      KROT = 0
      KCUR = KINIV
      IREC = KOPTAD(KCUR)
      DO I=1,NINVEL_FILE
        CALL NEXTSLA
        CALL FREDEC_2KEY_4ID_T(KEY,ID,UID,FLAG_FMT,SUB_ID,TITR)
        IF (FLAG_FMT /= 0) THEN
          FLAG_FMT_TMP = 44
          IFIX_TMP = 2
          CALL FMT_DEF(FLAG_FMT_TMP,IFIX_TMP)
        ELSE
          CALL FMT_DEF(INVERS_INIT,IFIX_INIT)
        ENDIF
        IFLAGUNIT = 0
        DO J=1,NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN    
            FAC_L = UNITAB%FAC_L(J)
            FAC_T = UNITAB%FAC_T(J)
            FAC_V = FAC_L/FAC_T
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='INITIAL VELOCITY',
     .                 C2='INITIAL VELOCITY',
     .                 C3=TITR) 
        ENDIF
C
        IREC=IREC+1
C
       
        IF(KEY(1:4)=='NODE')THEN
          ITYPE=6
        ELSE
          GOTO 999
        ENDIF

        IF(ITYPE <= 5) THEN
          CYCLE
        ENDIF

        NBVEL = NBVEL+1
        INIVIDS(NBVEL)=ID
!
        IF (ITYPE == 5) THEN
C     Finite volume velocity
           READ(IIN,REC=IREC,ERR=999,FMT=FMT_3F_4I) VL1,VL2,VL3, IDGRBRICK, IDGRQUAD, IDGRSH3N,ISK
        ELSEIF (ITYPE == 6) THEN
!========================================================================
!     NODAL INITIAL VOLOCITY
          JREC = IREC
          IAD = IAD_VEL(I)
          DO NL=1,NVEL(I)
!           translation
            READ(IIN,REC=JREC,ERR=999,FMT=FMT_2I_3F) NODE,ISK,VL1,VL2,VL3
            NOSYS = USR2SYS(NODE,ITABM1,MESS,ID)
            NOD_VEL(IAD+NL) = NOSYS
            V_NODE(1,IAD+NL) = VL1
            V_NODE(2,IAD+NL) = VL2
            V_NODE(3,IAD+NL) = VL3
            JREC=JREC+1
!           rotation
            READ(IIN,REC=JREC,ERR=999,FMT=FMT_2X_3F) VRL1,VRL2,VRL3
            VR_NODE(1,IAD+NL) = VRL1
            VR_NODE(2,IAD+NL) = VRL2
            VR_NODE(3,IAD+NL) = VRL3
            JREC=JREC+1
          ENDDO ! NL=1,NVEL(I)
          IREC = JREC
!========================================================================
        ENDIF

        IF (ITYPE == 6) THEN
          IAD = IAD_VEL(I)
          DO NL=1,NVEL(I)
            V_NODE(1,IAD+NL) = V_NODE(1,IAD+NL) * FAC_V
            V_NODE(2,IAD+NL) = V_NODE(2,IAD+NL) * FAC_V
            V_NODE(3,IAD+NL) = V_NODE(3,IAD+NL) * FAC_V
            VR_NODE(1,IAD+NL) = VR_NODE(1,IAD+NL) / FAC_T
            VR_NODE(2,IAD+NL) = VR_NODE(2,IAD+NL) / FAC_T
            VR_NODE(3,IAD+NL) = VR_NODE(3,IAD+NL) / FAC_T
          ENDDO ! NL=1,NVEL(I)
        ENDIF

        IF (ISK > 0) THEN
          IF (ITYPE == 6) THEN
            IAD = IAD_VEL(I)
            DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
              IF (ISK == ISKN(4,J+1)) THEN
                ISK=J+1
                DO NL=1,NVEL(I)
                  VL1 = V_NODE(1,IAD+NL)
                  VL2 = V_NODE(2,IAD+NL)
                  VL3 = V_NODE(3,IAD+NL)
                  V1 = SKEW(1,ISK)*VL1+SKEW(4,ISK)*VL2+SKEW(7,ISK)*VL3
                  V2 = SKEW(2,ISK)*VL1+SKEW(5,ISK)*VL2+SKEW(8,ISK)*VL3
                  V3 = SKEW(3,ISK)*VL1+SKEW(6,ISK)*VL2+SKEW(9,ISK)*VL3
                  VRL1 = VR_NODE(1,IAD+NL)
                  VRL2 = VR_NODE(2,IAD+NL)
                  VRL3 = VR_NODE(3,IAD+NL)
                  VR1 = SKEW(1,ISK)*VRL1+SKEW(4,ISK)*VRL2+SKEW(7,ISK)*VRL3
                  VR2 = SKEW(2,ISK)*VRL1+SKEW(5,ISK)*VRL2+SKEW(8,ISK)*VRL3
                  VR3 = SKEW(3,ISK)*VRL1+SKEW(6,ISK)*VRL2+SKEW(9,ISK)*VRL3
!
                  V_NODE(1,IAD+NL) = V1
                  V_NODE(2,IAD+NL) = V2
                  V_NODE(3,IAD+NL) = V3
                  VR_NODE(1,IAD+NL) = VR1
                  VR_NODE(2,IAD+NL) = VR2
                  VR_NODE(3,IAD+NL) = VR3
                ENDDO ! NL=1,NVEL(I)
                GO TO 100
              ENDIF
            ENDDO
            CALL ANCMSG(MSGID=184,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1='INITIAL VELOCITY',
     .                  I1=ID,
     .                  C2='INITIAL VELOCITY',
     .                  C3=TITR,
     .                  I2=ISK)
100         CONTINUE
          ELSE
            DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
              IF (ISK == ISKN(4,J+1)) THEN
                ISK=J+1
                V1 = SKEW(1,ISK)*VL1+SKEW(4,ISK)*VL2+SKEW(7,ISK)*VL3
                V2 = SKEW(2,ISK)*VL1+SKEW(5,ISK)*VL2+SKEW(8,ISK)*VL3
                V3 = SKEW(3,ISK)*VL1+SKEW(6,ISK)*VL2+SKEW(9,ISK)*VL3
                GO TO 200
              ENDIF
            ENDDO
            CALL ANCMSG(MSGID=184,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1='INITIAL VELOCITY',
     .                  I1=ID,
     .                  C2='INITIAL VELOCITY',
     .                  C3=TITR,
     .                  I2=ISK)
200         CONTINUE
          ENDIF ! IF (ITYPE == 6)
        ELSEIF (IFRA > 0) THEN
          DO K=1,NUMFRAM
            J=K+1
            IF(IFRA==IFRAME(4,J)) THEN
              V1 = XFRAME(1,J)*VL1+XFRAME(4,J)*VL2+XFRAME(7,J)*VL3
              V2 = XFRAME(2,J)*VL1+XFRAME(5,J)*VL2+XFRAME(8,J)*VL3
              V3 = XFRAME(3,J)*VL1+XFRAME(6,J)*VL2+XFRAME(9,J)*VL3
              GO TO 110
            ENDIF
          ENDDO
          CALL ANCMSG(MSGID=490,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1='INITIAL VELOCITY',
     .                I1=ID,
     .                C2='INITIAL VELOCITY',
     .                C3=TITR,
     .                I2=IFRA)
110       CONTINUE
          IFM = J
        ELSEIF (ISK == 0 .AND. IFRA == 0) THEN
          V1 = VL1
          V2 = VL2
          V3 = VL3
        ENDIF
C
        IF (ITYPE == 5) THEN
C     /INIVEL type 5 : FVM
           IF (.NOT. MULTI_FVM%IS_USED) THEN
              CALL ANCMSG(MSGID=1554,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO,
     .             C1='IN /INIVEL OPTION')
           ELSE
              IDGRBRICK_LOC = -1
              IDGRQUAD_LOC  = -1
              IDGRSH3N_LOC  = -1
              IF (IDGRBRICK + IDGRQUAD + IDGRSH3N == 0) THEN
                 CALL ANCMSG(MSGID=1553, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .                C1='IN /INIVEL OPTION')
              ELSE
                 IF (IDGRBRICK /= 0) THEN
                    DO J = 1,NGRBRIC
                       IF (IDGRBRICK == IGRBRIC(J)%ID) IDGRBRICK_LOC = J
                    ENDDO
                    IF (IDGRBRICK_LOC == -1) THEN
                       CALL ANCMSG(MSGID=1554,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='IN /INIVEL OPTION',
     .                      I1=IDGRBRICK)
                    ENDIF
                 ENDIF
                 IF (IDGRQUAD /= 0) THEN
                    DO J = 1,NGRQUAD
                       IF (IDGRQUAD == IGRQUAD(J)%ID) IDGRQUAD_LOC = J
                    ENDDO    
                    IF (IDGRQUAD_LOC == -1) THEN
                       CALL ANCMSG(MSGID=1554,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='IN /INIVEL OPTION',
     .                      I1=IDGRQUAD)
                    ENDIF
                 ENDIF
                 IF (IDGRSH3N /= 0) THEN
                    DO J = 1,NGRSH3N
                       IF (IDGRSH3N == IGRSH3N(J)%ID) IDGRSH3N_LOC = J
                    ENDDO      
                    IF (IDGRSH3N_LOC == -1) THEN
                       CALL ANCMSG(MSGID=1554,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='IN /INIVEL OPTION',
     .                      I1=IDGRSH3N)                   
                    ENDIF
                 ENDIF
              ENDIF
C     Going on
C     Brick groups
              FVM_INIVEL(I)%FLAG = .TRUE.
              FVM_INIVEL(I)%GRBRICID = IDGRBRICK_LOC
              FVM_INIVEL(I)%GRQUADID = IDGRQUAD_LOC
              FVM_INIVEL(I)%GRSH3NID = IDGRSH3N_LOC
              FVM_INIVEL(I)%VX = V1
              FVM_INIVEL(I)%VY = V2
              FVM_INIVEL(I)%VZ = V3
           ENDIF                ! MULTI_FVM%IS_USED
C     /INIVEL type 5 : FVM
        ELSEIF (ITYPE == 6) THEN
          IAD = IAD_VEL(I)
          DO NL=1,NVEL(I)
            NOSYS = NOD_VEL(IAD+NL)
            V(1,NOSYS)  = V_NODE(1,IAD+NL)
            V(2,NOSYS)  = V_NODE(2,IAD+NL)
            V(3,NOSYS)  = V_NODE(3,IAD+NL)
            KROT = 1
            VR(1,NOSYS) = VR_NODE(1,IAD+NL)
            VR(2,NOSYS) = VR_NODE(2,IAD+NL)
            VR(3,NOSYS) = VR_NODE(3,IAD+NL)
          ENDDO
        ENDIF ! IF (ITYPE /= 5 .AND. ITYPE /= 6)
      ENDDO

      IF (FLAG_FMT /= 0) THEN
        CALL FMT_DEF(INVERS_INIT,IFIX_INIT)
      ENDIF
C---
      CALL UDOUBLE(INIVIDS,1,NBVEL,MESS,0,BID)
C
C--- RAZ vitesses for SPH Reserve particles
      IF (NSPHRES>0) THEN
        DO N=1,NSPHRES
          INOD = KXSP(3,FIRST_SPHRES+N-1)
          V(1,INOD) = ZERO
          V(2,INOD) = ZERO
          V(3,INOD) = ZERO
          IF (IRODDL>0) THEN
            VR(1,INOD) = ZERO
            VR(2,INOD) = ZERO
            VR(3,INOD) = ZERO
          ENDIF
        END DO   
      ENDIF
C--------------------------------------------------
C     PRINT
C--------------------------------------------------
      IF (NINVEL_FILE > 0) THEN
      J=0
      NODINIVEL=0
      IF(IPRI>=2)THEN
       IF(IALE/=0) THEN
         WRITE(IOUT,2100)
       ELSEIF(KROT==0) THEN
         WRITE(IOUT,2000)
       ELSE
         WRITE(IOUT,2200)
       ENDIF
       KPRI=0
       DO 340 N=1,NUMNOD,50
       J=J+50
       J=MIN(J,NUMNOD)
       IF(IALE==0) THEN
        DO 330 I=N,J
          IF(KPRI>=50) THEN
            IF(KROT==0) THEN
              WRITE(IOUT,2000)
            ELSE
              WRITE(IOUT,2200)
            ENDIF
            KPRI=0
          ENDIF
          IF(IRODDL/=0) THEN
            IF (V(1,I)/=ZERO.OR.V(2,I)/=ZERO.OR.V(3,I)/=ZERO.OR.
     .        VR(1,I)/=ZERO.OR.VR(2,I)/=ZERO.OR.VR(3,I)/=ZERO)THEN
              NODINIVEL=NODINIVEL+1
              IF (VR(1,I)/=ZERO.OR.VR(2,I)/=ZERO.OR.
     .            VR(3,I)/=ZERO) THEN
                WRITE(IOUT,'(3X,I10,8X,1P6G20.13)')
     .              ITAB(I),V(1,I),V(2,I),V(3,I),VR(1,I),VR(2,I),VR(3,I)
              ELSE
                WRITE(IOUT,'(3X,I10,8X,1P6G20.13)')
     .               ITAB(I),V(1,I),V(2,I),V(3,I)
              ENDIF
              KPRI=KPRI+1
            ENDIF
         ELSEIF(V(1,I)/=ZERO.OR.V(2,I)/=ZERO.OR.V(3,I)/=ZERO) THEN
           NODINIVEL=NODINIVEL+1
           WRITE(IOUT,'(3X,I10,8X,1P6G20.13)')
     .           ITAB(I),V(1,I),V(2,I),V(3,I)
           KPRI=KPRI+1
         ENDIF
 330    CONTINUE
       ELSE
        DO 335 I=N,J
          IF(KPRI==50) THEN
            WRITE(IOUT,2100)
            KPRI=0
          ENDIF
        IF(V(1,I)/=ZERO.OR.V(2,I)/=ZERO.OR.V(3,I)/=ZERO.OR.
     .     W(1,I)/=ZERO.OR.W(2,I)/=ZERO.OR.W(3,I)/=ZERO) THEN
          NODINIVEL=NODINIVEL+1
          WRITE(IOUT,'(5X,I10,8X,1P6G20.13)') ITAB(I),
     +                        V(1,I),V(2,I),V(3,I),W(1,I),W(2,I),W(3,I)
          KPRI=KPRI+1
        ENDIF
 335    CONTINUE
       ENDIF
 340  CONTINUE
      WRITE(IOUT,'(/,A,I10,//)')
     +           ' NUMBER OF NODES WITH INITIAL VELOCITY:',NODINIVEL
      ENDIF

      ENDIF
!
      IF(ALLOCATED(IAD_VEL))DEALLOCATE(IAD_VEL)
      IF(ALLOCATED(NVEL))DEALLOCATE(NVEL)
      IF(ALLOCATED(NOD_VEL))DEALLOCATE(NOD_VEL)
      IF(ALLOCATED(V_NODE)) DEALLOCATE(V_NODE)
      IF(ALLOCATED(VR_NODE))DEALLOCATE(VR_NODE)

!-----------
      RETURN
!-----------
2000  FORMAT(//
     .'     INITIAL VELOCITIES    '/
     .'     -------------------    '/
     + 9X,'NODE',22X,'VX   ',15X,'VY   ',15X,'VZ   '/)
2100  FORMAT(//
     .'     INITIAL VELOCITIES    '/
     .'     -------------------    '/
     + 9X,'NODE',22X,'VX   ',15X,'VY   ',15X,'VZ   ',
     +           14X,'WX   ',15X,'WY   ',15X,'WZ   '/)
2200  FORMAT(//
     .'     INITIAL VELOCITIES    '/
     .'     -------------------    '/
     + 9X,'NODE',22X,'VX   ',15X,'VY   ',15X,'VZ   ',
     +           14X,'VRX  ',15X,'VRY  ',15X,'VRZ'/)
 999  CALL FREERR(1)
      RETURN
      END

Chd|====================================================================
Chd|  PREREAD_INIVEL                source/initial_conditions/general/inivel/inivel.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FREDEC4                       source/starter/freform.F      
Chd|        NEXTSLA                       source/starter/freform.F      
Chd|        STACK_VAR_MOD                 share/modules1/stack_var_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE PREREAD_INIVEL(KROT)
C============================================================================
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      USE STACK_VAR_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KROT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      CHARACTER IDTITL*nchartitle,KEY*ncharkey, 
     .          SOLVERKEYWORD*ncharline
C=======================================================================
C--------------------------------------------------
! rotational inivel at nodes
      KCUR = KINIV
      IREC = KOPTAD(KCUR)
      DO I=1,NINVEL_FILE
        CALL NEXTSLA
        IREC=IREC+1
        CALL FREDEC4(KEY)
        IF (KEY(1:4) == 'NODE') KROT = 1
      ENDDO
C-----------
C      
      RETURN
      END

