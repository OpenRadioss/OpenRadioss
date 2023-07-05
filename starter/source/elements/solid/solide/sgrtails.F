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
Chd|  SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE SGRTAILS(
     1       IXS    ,PM     ,IPARG  ,GEO     ,
     2       EADD   ,ND     ,NELS   ,IPARTS  ,DD_IAD  ,
     3       IDX    ,ISOLNOD,LB_MAX ,INUM    ,INDEX   ,
     4       CEP    ,ITR1   ,IXS10  ,IGRSURF ,IGRBRIC ,
     5       IXS20  ,IXS16  ,IGEO    , IDDLEVEL,
     6       IPM    ,NOD2ELS,ISOLOFF,ISOLNOD1,
     7       TAGPRT_SMS,INIVOL,SPH2SOL ,SOL2SPH,SOL2SPH_TYP,
     8       IFLAG_BPRELOAD,CLUSTERS,MATPARAM_TAB,RNOISE)
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE CLUSTER_MOD
      USE MATPARAM_DEF_MOD
      USE INIVOL_DEF_MOD
      USE INIVOL_ARRAY_MOD
      USE QA_OUT_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXS(11,NUMELS)       ARRAY: CONECS+PID+MID+NOS SOLIDS          E
C     PM(NPROPM,NUMMAT)    ARRAY: MATERIAL PARAMETERS (real)         E
C     IPM(NPROPMI,NUMMAT)  ARRAY: MATERIAL PARAMETERS (integer)      E
C     GEO(NPROPG,NUMGEO)   ARRAY: PROPERTY PARAMETERS (real)         E
C     IGEO(NPROPGI,NUMGEO) ARRAY: PROPERTY PARAMETERS (integer)      E
C     IPARG(NPARG,NGROUP)  ARRAY: GROUP PARAMETERS (itneger)         E/S
C     EADD(NUMELS)         ARRAY: IDAM indexes / checkboard            S
C     DD_IAD               ARRAY: DE LA DD EN SUPER GROUPES            S
C     IPARTS                                                         E/S
C     INUM(13,NUMELS)      ARRAY:WORKING                             E/S
C     INDEX(NUMELS)        ARRAY:WORKING                             E/S
C     CEP(NUMELS)          ARRAY:WORKING                             E/S
C     ITR1(NSELS)          ARRAY:WORKING                             E/S
C     ISOLOFF(NUMELS)      FLAG ELEM RBY ON/OFF                      E/S
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr21_c.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
#include      "boltpr_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER ND, IDX, LB_MAX
      INTEGER IXS(11,*),IPARG(NPARG,*),EADD(*), ISOLNOD(*),
     .        DD_IAD(NSPMD+1,*),IPARTS(*), NELS(*),
     .        INUM(16,*), INDEX(*),CEP(*),ITR1(*),IXS10(6,*),IXS20(12,*),
     .        IXS16(8,*),IGEO(NPROPGI,NUMGEO),IPM(NPROPMI,NUMMAT),
     .        NOD2ELS(*), ISOLOFF(*),ISOLNOD1(*),
     .        TAGPRT_SMS(*),SPH2SOL(*),
     .        SOL2SPH(2,*),SOL2SPH_TYP(*),IFLAG_BPRELOAD(*)
      INTEGER, INTENT(IN) :: IDDLEVEL
      TYPE(MATPARAM_STRUCT_) , TARGET, DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM_TAB
      MY_REAL  PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO), RNOISE(NPERTURB,NUMELS)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (INIVOL_) , DIMENSION(NINIVOL) :: INIVOL
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .   NGR1, MLN, NG, ISSN, ISSN_, N, MID, PID, II,ILOC,IL,NEL, NE1,IREP,IINT,
     .   P, NEL_PREC, IGT, JHBE, I, IKSNOD0,NB,IEOS,NLAY,
     .   MODE, WORK(70000), NN, J, NPTR,NPTS,NPTT,NPG,
     .   IPLAST,NUVARP, IALEL, MT, INEG, JIVF,ICPRE,ICSTR,
     .   IFAIL,IMATVIS,NLY,NL,ILAW,IM,IPMAT,ITET4,ITET10,
     .   NGP(NSPMD+1),JJ,IFAILMODEL,NFAIL,ISVIS,IVISC,IMAT,
     .   INUM_R2R(1+R2R_SIU*NUMELS),IPARTSPH,IPARTR2R,MFT,IBOLTP,ICP0,ISM0,
     .   IPERT,STAT,ITSH,IT10,ICPT10,JALE_FROM_MAT,JALE_FROM_PROP
       DATA IPMAT /100/
       INTEGER, DIMENSION(:), ALLOCATABLE :: INDEXS2
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER :: CLUSTER_TYP,CLUSTER_NEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_CLUSTER
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
      LOGICAL lFOUND
      INTEGER :: NB_NODES, LDIM, OFFSET
C--------------------------------------------------------------
C         BORNAGE DES GROUPES DE MVSIZ
C--------------------------------------------------------------
      MY_ALLOCATE(INDEXS2,NUMELS)
      nullify(MATPARAM)
      INDEXS2(1:NUMELS)=PERMUTATION%SOLID(1:NUMELS)
      NGR1 = NGROUP + 1
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELS),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
      ENDIF
C
C phase 1 : domain decomposition
C
      IDX=IDX+ND*(NSPMD+1)
      CALL ZEROIN(1,ND*(NSPMD+1),DD_IAD(1,NSPGROUP+1))
C     NSPGROUP = NSPGROUP + ND
      NFT = 0  
C initialisation dd_iad
      DO N=1,ND
       DO P=1,NSPMD+1
         DD_IAD(P,NSPGROUP+N) = 0
       END DO
      ENDDO
C
      DO I=1,NUMELS
        ISOLNOD1(I) = ISOLNOD(I)  
      ENDDO
C
      DO N=1,ND
        NEL = EADD(N+1)-EADD(N)
C
        DO I = 1, NEL
          INDEX(I) = I
          INUM(1,I)=IPARTS(NFT+I)
          INUM(2,I)=IXS(1,NFT+I)
          INUM(3,I)=IXS(2,NFT+I)
          INUM(4,I)=IXS(3,NFT+I)
          INUM(5,I)=IXS(4,NFT+I)
          INUM(6,I)=IXS(5,NFT+I)
          INUM(7,I)=IXS(6,NFT+I)
          INUM(8,I)=IXS(7,NFT+I)
          INUM(9,I)=IXS(8,NFT+I)
          INUM(10,I)=IXS(9,NFT+I)
          INUM(11,I)=IXS(10,NFT+I)
          INUM(12,I)=IXS(11,NFT+I)
          INUM(13,I)=ISOLNOD(NFT+I)
          IF (NSUBDOM>0) INUM_R2R(I) = TAG_ELSF(NFT+I)
          IF (NPERTURB > 0) THEN
            DO IPERT = 1, NPERTURB
              XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,NFT+I) 
            ENDDO
          ENDIF
        ENDDO
C
        IF(NSPHSOL /= 0 .AND. NFT < NUMELS8)THEN
          DO I=1, NEL
            INUM(14,I)=SOL2SPH(1,NFT+I)
            INUM(15,I)=SOL2SPH(2,NFT+I)
            INUM(16,I)=SOL2SPH_TYP(NFT+I)
          ENDDO
        END IF
      
        IF(DOQA .NE. 0 .OR. IDDLEVEL == 0) THEN
          MODE=0
          CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        ELSE
          NB_NODES = 8 ! 8 nodes for solids
          LDIM = 16 ! fist dimension of INUM
          OFFSET = 2 ! nodes starts at INUM(3,I)
          CALL CPP_REORDER_ELEMENTS(NEL, NSPMD, NB_NODES, OFFSET, LDIM , CEP(NFT+1), INUM, INDEX)
        ENDIF

        DO I = 1, NEL
          PERMUTATION%SOLID(I+NFT)=INDEXS2(INDEX(I)+NFT)
          IPARTS(I+NFT)=INUM(1,INDEX(I))
          IXS(1,I+NFT)=INUM(2,INDEX(I))
          IXS(2,I+NFT)=INUM(3,INDEX(I))
          IXS(3,I+NFT)=INUM(4,INDEX(I))
          IXS(4,I+NFT)=INUM(5,INDEX(I))
          IXS(5,I+NFT)=INUM(6,INDEX(I))
          IXS(6,I+NFT)=INUM(7,INDEX(I))
          IXS(7,I+NFT)=INUM(8,INDEX(I))
          IXS(8,I+NFT)=INUM(9,INDEX(I))
          IXS(9,I+NFT)=INUM(10,INDEX(I))
          IXS(10,I+NFT)=INUM(11,INDEX(I))
          IXS(11,I+NFT)=INUM(12,INDEX(I))
          ISOLNOD(I+NFT)=INUM(13,INDEX(I))
          ITR1(NFT+INDEX(I)) = NFT+I
          IF (NSUBDOM>0) TAG_ELSF(NFT+I) = INUM_R2R(INDEX(I))
          IF (NPERTURB > 0) THEN
            DO IPERT = 1, NPERTURB
              RNOISE(IPERT,I+NFT) = XNUM_RNOISE(IPERT,INDEX(I))
            ENDDO
          ENDIF
        ENDDO
C
C Renumarotation ISOLOFF
C
        DO I = 1, NEL
          INUM(3,I) = ISOLOFF(NFT+I)
        END DO
C
        DO I = 1, NEL
          ISOLOFF(NFT+I) = INUM(3,INDEX(I))
        END DO
c
c Renum. BoltPreload
c
        IF (NPRELOAD > 0) THEN
         DO I=1,NEL
           INUM(4,I)=IFLAG_BPRELOAD(NFT+I)
         ENDDO

         DO I=1,NEL
           IFLAG_BPRELOAD(NFT+I)=INUM(4,INDEX(I))
         ENDDO
        ENDIF

C        
        IF (NFT>=NUMELS8+NUMELS10+NUMELS20) THEN
         DO I = 1, NEL
          II = I+NFT-(NUMELS8+NUMELS10+NUMELS20)
          INUM(1,I)=IXS16(1,II)
          INUM(2,I)=IXS16(2,II)
          INUM(3,I)=IXS16(3,II)
          INUM(4,I)=IXS16(4,II)
          INUM(5,I)=IXS16(5,II)
          INUM(6,I)=IXS16(6,II)
          INUM(7,I)=IXS16(7,II)
          INUM(8,I)=IXS16(8,II)
         ENDDO
         DO I = 1, NEL
          II = I+NFT-(NUMELS8+NUMELS10+NUMELS20)
          IXS16(1,II)=INUM(1,INDEX(I))
          IXS16(2,II)=INUM(2,INDEX(I))
          IXS16(3,II)=INUM(3,INDEX(I))
          IXS16(4,II)=INUM(4,INDEX(I))
          IXS16(5,II)=INUM(5,INDEX(I))
          IXS16(6,II)=INUM(6,INDEX(I))
          IXS16(7,II)=INUM(7,INDEX(I))
          IXS16(8,II)=INUM(8,INDEX(I))
         ENDDO
        ELSEIF (NFT>=NUMELS8+NUMELS10) THEN
         DO I = 1, NEL
          II = I+NFT-(NUMELS8+NUMELS10)
          INUM(1,I)=IXS20(1,II)
          INUM(2,I)=IXS20(2,II)
          INUM(3,I)=IXS20(3,II)
          INUM(4,I)=IXS20(4,II)
          INUM(5,I)=IXS20(5,II)
          INUM(6,I)=IXS20(6,II)
          INUM(7,I)=IXS20(7,II)
          INUM(8,I)=IXS20(8,II)
          INUM(9,I)=IXS20(9,II)
          INUM(10,I)=IXS20(10,II)
          INUM(11,I)=IXS20(11,II)
          INUM(12,I)=IXS20(12,II)
         ENDDO
         DO I = 1, NEL
          II = I+NFT-(NUMELS8+NUMELS10)
          IXS20(1,II)=INUM(1,INDEX(I))
          IXS20(2,II)=INUM(2,INDEX(I))
          IXS20(3,II)=INUM(3,INDEX(I))
          IXS20(4,II)=INUM(4,INDEX(I))
          IXS20(5,II)=INUM(5,INDEX(I))
          IXS20(6,II)=INUM(6,INDEX(I))
          IXS20(7,II)=INUM(7,INDEX(I))
          IXS20(8,II)=INUM(8,INDEX(I))
          IXS20(9,II)=INUM(9,INDEX(I))
          IXS20(10,II)=INUM(10,INDEX(I))
          IXS20(11,II)=INUM(11,INDEX(I))
          IXS20(12,II)=INUM(12,INDEX(I))
         ENDDO
        ELSEIF (NFT>=NUMELS8) THEN
         DO I = 1, NEL
          II = I+NFT-NUMELS8
          INUM(1,I)=IXS10(1,II)
          INUM(2,I)=IXS10(2,II)
          INUM(3,I)=IXS10(3,II)
          INUM(4,I)=IXS10(4,II)
          INUM(5,I)=IXS10(5,II)
          INUM(6,I)=IXS10(6,II)
         ENDDO
         DO I = 1, NEL
          II = I+NFT-NUMELS8
          IXS10(1,II)=INUM(1,INDEX(I))
          IXS10(2,II)=INUM(2,INDEX(I))
          IXS10(3,II)=INUM(3,INDEX(I))
          IXS10(4,II)=INUM(4,INDEX(I))
          IXS10(5,II)=INUM(5,INDEX(I))
          IXS10(6,II)=INUM(6,INDEX(I))
         ENDDO
        ENDIF
C
C RENUMEROTATION POUR SPH CONVERSION
C
        IF(NSPHSOL /= 0 .AND. NFT < NUMELS8)THEN
C
C rebuild SOL2SPH
          DO I=1,NEL
            SOL2SPH(1,NFT+I)=INUM(14,INDEX(I))
            SOL2SPH(2,NFT+I)=INUM(15,INDEX(I))
            SOL2SPH_TYP(NFT+I)=INUM(16,INDEX(I))
          END DO
        END IF
C
        P = CEP(NFT+INDEX(1))
        NB = 1
        DO I = 2, NEL
          IF (CEP(NFT+INDEX(I))/=P) THEN
            DD_IAD(P+1,NSPGROUP+N) = NB
            NB = 1
            P = CEP(NFT+INDEX(I))
          ELSE
            NB = NB + 1
          ENDIF
        ENDDO
        DD_IAD(P+1,NSPGROUP+N) = NB
        DO P = 2, NSPMD
          DD_IAD(P,NSPGROUP+N) = DD_IAD(P,NSPGROUP+N)
     .                         + DD_IAD(P-1,NSPGROUP+N)
        ENDDO
        DO P = NSPMD+1,2,-1
          DD_IAD(P,NSPGROUP+N) = DD_IAD(P-1,NSPGROUP+N)+1
        ENDDO
        DD_IAD(1,NSPGROUP+N) = 1
C          
C maj CEP
C
        DO I = 1, NEL
          INDEX(I) = CEP(NFT+INDEX(I))          
        ENDDO
        DO I = 1, NEL
          CEP(NFT+I) = INDEX(I)          
        ENDDO
        NFT = NFT + NEL
      ENDDO
       
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF (IGRSURF(I)%ELTYP(J) == 1)
     .        IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SOLIDES
C
      DO I=1,NGRBRIC
        NN=IGRBRIC(I)%NENTITY
        DO J=1,NN
          IGRBRIC(I)%ENTITY(J) = ITR1(IGRBRIC(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR SPH CONVERSION
C
      IF(NSPHSOL /= 0)THEN
        DO I=1,NUMSPH
          IF(SPH2SOL(I) /= 0)SPH2SOL(I)=ITR1(SPH2SOL(I))
        ENDDO
      END IF
C
C renumerotation CONNECTIVITE INVERSE
C
      DO I=1,8*NUMELS+6*NUMELS10+12*NUMELS20+8*NUMELS16
        IF(NOD2ELS(I) /= 0)NOD2ELS(I)=ITR1(NOD2ELS(I))
      END DO


!   -----------------------
!   reordering for cluster typ=1 (solid cluster)
      DO I=1,NCLUSTER
        CLUSTER_TYP = CLUSTERS(I)%TYPE
        IF(CLUSTER_TYP==1) THEN
            CLUSTER_NEL = CLUSTERS(I)%NEL
            ALLOCATE( SAVE_CLUSTER( CLUSTER_NEL ) )
            SAVE_CLUSTER( 1:CLUSTER_NEL ) = CLUSTERS(I)%ELEM( 1:CLUSTER_NEL )
            DO J=1,CLUSTER_NEL
                CLUSTERS(I)%ELEM(J) = ITR1( SAVE_CLUSTER( J ) )
            ENDDO
            DEALLOCATE( SAVE_CLUSTER )
        ENDIF
      ENDDO
!   -----------------------

C
C phase 2 : bornage en groupe de mvsiz
C ngroup est global, iparg est global mais organise en fonction de dd
C

      INEG =  0
      DO 300 N=1,ND
       NFT = 0
       DO P = 1, NSPMD
        NGP(P)=0
        NEL = DD_IAD(P+1,NSPGROUP+N)-DD_IAD(P,NSPGROUP+N)
        IF (NEL>0) THEN
         NEL_PREC = DD_IAD(P,NSPGROUP+N)-DD_IAD(1,NSPGROUP+N)
         NGP(P)=NGROUP
         NG  = (NEL-1)/NVSIZ + 1
         DO 220 I=1,NG
C ngroup global
          NGROUP=NGROUP+1
          CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))
          II = EADD(N)+NFT
          MID = IXS(1,II)
          PID = IXS(NIXS-1,II)
C Bolt preloading
          IBOLTP=0
          IF (NPRELOAD > 0) THEN
           IBOLTP=IFLAG_BPRELOAD(II)
          ENDIF
          IF (NSUBDOM>0) IPARTR2R = TAG_MAT(MID)
          ID = IGEO(1,PID)
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,PID),LTITR)
          IKSNOD0=ISOLNOD(II)          
C on peut avoir besoin de isolnod en cas de retour a la d.d.
C de plus ISOLNOD remis a 0 real dans lectur
          NPT=1
          JIVF=0
          NUVARP=0
          JHBE=IHBE_D
          JPOR=0
          JCLOS=0
          IPLAST=IPLA_DS-1
          ICPRE=0
          ICSTR=0
          IREP = 0
          IINT = 0
          JCVT = 0
          ISORTH=0
          ISTRAIN=0
          ISVIS = 0
          IPARTSPH=0
          IVISC = 0
          ITSH = 0
          IT10 = 0
C-------------------------
C         PARAMETRE GROUPE
          IF (PID/=0) THEN
            IREP   = IGEO(6,PID)
            JHBE   = IGEO(10,PID)
            IGT    = IGEO(11,PID)
            NPT    = IGEO(4,PID)
            ISSN   = IGEO(5,PID)
            IINT   = IGEO(15,PID)
            JCVT   = IGEO(16,PID)
            ISORTH = IGEO(17,PID)
            ISTRAIN= IGEO(12,PID)
            ITET4  = IGEO(20,PID)
            ITET10  = IGEO(50,PID)
c
            ISVIS  = IGEO(33,PID)                       
C   is a material flag   IVISC  = IGEO(34,PID)
            IPARTSPH=IGEO(38,PID)
C  material flag for viscosity
            IF (IGT == 22) THEN                   
              NLAY = IGEO(30,PID)                 
              DO IL=1,NLAY                        
                IMAT = IGEO(100+IL,PID)
                IF (IPM(222,IMAT) > 0) IVISC = 1
              ENDDO
            ELSEIF (IPM(222,IABS(MID)) > 0) THEN
               IVISC = 1
            ENDIF
C            
            IF (ISSN == 10.OR.ISSN == 11) THEN
               IF ((IKSNOD0/=4 .AND. IKSNOD0/=8.AND. IKSNOD0/=10) .OR. JHBE == 15 ) THEN
                  CALL ANCMSG(MSGID=1159,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
C     .                    I2=IKSNOD0,
     .                    I3=JHBE)
               ENDIF 
            ENDIF
C
            IF (JHBE == 222) THEN
              IF (IKSNOD0 == 8) THEN
                JHBE= 14
              ELSEIF (IKSNOD0 == 16 .OR. IKSNOD0 == 20) THEN
                JHBE= 16
              ENDIF
            ENDIF
C
            IF (JHBE == 12) THEN
              IF (ISSN == 10.OR.ISSN == 11) THEN
                  CALL ANCMSG(MSGID=1159,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=JHBE)
              ENDIF
              IF (IKSNOD0 == 4 .OR. IKSNOD0 == 10) THEN
                JHBE= 1
              ELSE
              CALL ANCMSG(MSGID=1160,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  PRMOD=MSG_CUMU)
              ENDIF
            ENDIF

            IF (IGT == 15) JPOR = 2*NINT(GEO(28,PID))
            IF (GEO(130,PID) > 0) JCLOS=1
            IF (IGT > 28) NUVARP = NINT(GEO(25,PID))
            IF (IGT /= 15) THEN
              IPLAST = IGEO(9,PID)
              ICPRE  = IGEO(13,PID) 
              ICSTR  = IGEO(14,PID)
              ICPT10 = ICPRE
            ENDIF
            IF (IGT == 14.OR.IGT == 6) THEN
              IF (ITET4/=0.AND.IKSNOD0 == 8) THEN
                ITET4=0
              END IF
              IF (ITET10/=0.AND.IKSNOD0 == 8) THEN
                ITET10=0
              END IF
            END IF
          ENDIF
C
          MLN = NINT(PM(19,ABS(MID))) 
         IF(MLN == 20)THEN
               IF(IPARG(5,NGROUP)/=2)THEN
                        CALL ANCMSG(MSGID=129,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO)
                     CALL ARRET(2)      
                     ENDIF
         ENDIF
C          
          IF(MID<0)THEN
            IF(MLN == 6.AND.JPOR/=2)MLN=17
            IF(MLN == 46)MLN=47
            MID=ABS(MID)
            IXS(1,II)=MID
            INEG = 1
          ENDIF
c          
          MATPARAM => MATPARAM_TAB(MID)
          IF(IGT == 6 .AND. MLN == 70) THEN
                CALL ANCMSG(MSGID=1221,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IGT,
     .                   I3=MLN)
          
          ENDIF
          IF(IGT == 0 .AND. MLN /= 0) THEN
                CALL ANCMSG(MSGID=1586,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IPM(1,MID),
     .                   I2=ID,
     .                   PRMOD=MSG_CUMU)
          ENDIF
          IF (JHBE == 24 .AND. IGT == 6) THEN
            IF(MLN /= 14 .AND. MLN /= 12 .AND. MLN /= 25  .AND.
     .         MLN /= 28 .AND. MLN /= 50 .AND. MLN /= 68  .AND. 
     .         MLN /= 53 .AND. MLN /= 93 .AND. MLN /= 107 .AND.
     .         MLN /= 112) THEN
            CALL ANCMSG(MSGID=1225,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=MLN,
     .                  PRMOD=MSG_CUMU)
            END IF
          ENDIF
C
          ! Compatibility with /MAT/LAW115 statistic formulation
          IF ((MLN == 115).AND.(((JHBE>2).AND.(JHBE<21)).OR.((ITET4>0).AND.(ITET4<3)))) THEN
            CALL ANCMSG(MSGID=1905,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=MLN,
     .                  PRMOD=MSG_CUMU)
          ENDIF
C
          IFAIL = 0
          NFAIL = 0
          IF (IGT == 22) THEN                   
            NLAY = IGEO(30,PID)                 
            DO IL=1,NLAY                        
              IMAT  = IGEO(100+IL,PID)           
              NFAIL = MAX(NFAIL,IPM(220, IMAT))
            ENDDO
          ELSE
            NFAIL = IPM(220,MID)
          ENDIF        
c
          IF (NFAIL > 0)THEN
            IFAIL = 1
            IF(MLN /= 24 .AND. MLN /= 25 .AND. MLN < 28) THEN
               DO J=1,NFAIL  
                  IFAILMODEL = IPM(111 + 15*(J - 1) ,MID)
                  IF  (IFAILMODEL == 10 .OR. IFAILMODEL == 4 
     .             .OR.IFAILMODEL == 5  .OR. IFAILMODEL == 6)ISTRAIN = 1
                ENDDO
             ENDIF   
          ENDIF
          IF(IKSNOD0 == 10.OR.
     .      (IKSNOD0 == 4.AND.ITET4 == 1))THEN
            NPT=4
            IT10 =1
          ELSEIF(IKSNOD0 == 4)THEN
            NPT=1
          ENDIF
          IF ((JHBE/=1 .AND. JHBE/=2) .AND. MLN==68 ) THEN
            CALL ANCMSG(MSGID=672,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,     
     .                  I2=IGEO(1,PID))
          ENDIF
          IF (JHBE ==  2) JHBE=0     ! Hourglass Halquist
          IF (JHBE == 15) THEN
            IF(MLN == 1 .AND. NPT /= 1.AND.IGT /= 22) THEN
              CALL ANCMSG(MSGID=791,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
            ENDIF
            IF(NPT ==  0) NPT = 3
            IF(MLN == 1 .AND. NPT /= 1.AND.IGT /= 22) NPT = 2
          ENDIF
          IF(JHBE==14 .AND. MLN==1.AND.IGT /= 22) NPT = 222
C
          JALE_FROM_MAT = NINT(PM(72,MID))
          JALE_FROM_PROP = IGEO(62,PID)
          JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if inconsistent, error message was displayed in PART reader
          JLAG=0
          IF(JALE == 0.AND.MLN /= 18)JLAG=1
          JEUL=0
          IF(JALE == 2)THEN
            JALE=0
            JEUL=1
C foam + air
          ELSEIF(JALE == 3 .AND. MLN == 77) THEN
            JLAG=1          
          ENDIF
          IF(MLN/=50)JTUR=NINT(PM(70,MID)) 
          JTHE=NINT(PM(71,MID))
C     Multifluid law
          ISRAT=IPM(3,MID)
          IMATVIS = IPM(216, MID)
          ISSN=0
          IF(JLAG/=0.AND.PID/=0)THEN
            ISSN=IGEO(5,PID)
C            IF (ITET4 == 1 .AND. IKSNOD0 == 4 .AND. ISSN /= 0) ISSN=0
          ELSE
            ISSN=4
          ENDIF
          IF(MLN == 13) IRIGID_MAT = 1
C
          JSMS=0
          IF(ISMS/=0)THEN
            IF(IDTGRS/=0)THEN
              IF(TAGPRT_SMS(IPARTS(II))/=0)JSMS=1
            ELSE
              JSMS=1
            END IF
          END IF
          IEOS = IPM(4,MID)
C--------------------
C-   ICPRE,ISMSTR JCVT Automatic           
C--------------------
          IF (IGT == 20 .OR. IGT == 21 .OR. IGT == 22) ITSH = 1
C---add ICPRE auto for thick-shell          
          IF (ITSH == 1 ) THEN
             ICP0=MATPARAM%COMPRESSIBILITY
             SELECT CASE (ICP0)
C-------compressible       
              CASE(1)                                             
               ICPRE = 0        
C-------uncompressible       
              CASE(2)                                             
               ICPRE = 1
C--------elasto-plastic       
              CASE(3)                                             
               ICPRE = 2
             END SELECT 
C--------message out       
             IF (ICPRE>0) CALL ANCMSG(MSGID=1741,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ICPRE,
     .                      PRMOD=MSG_CUMU)
          END IF !IF (ITSH == 1 )
C          
          IF (IGT == 14 .OR. IGT == 6) THEN
           IF (ICPRE<0) THEN
            IF (IKSNOD0 ==8.AND.JHBE>10) THEN
             ICP0=MATPARAM%COMPRESSIBILITY
            SELECT CASE (ICP0)
C-------compressible       
             CASE(1)                                             
              ICPRE = 3        
C-------uncompressible       
             CASE(2)                                             
              ICPRE = 1
C--------elasto-plastic       
             CASE(3)                                             
              ICPRE = 2
            END SELECT 
C--------message out       
                CALL ANCMSG(MSGID=1741,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ICPRE,
     .                      PRMOD=MSG_CUMU)
            ELSE
             ICPRE = 0        
            END IF !(IKSNOD0 ==8.AND.JHBE>10) THEN
           END IF !IF (ICPRE<0)
C------ quadratic S16,S20 not compatible with total strain--           
           IF (ISSN<0) THEN
C---  ISM0 : 2 large, 1: small  ;   ICP0 : 1 inc, 2 total     
             ISM0 = MATPARAM%SMSTR
             ICP0 = MATPARAM%STRAIN_FORMULATION
             IF (ICP0 ==2.AND.JHBE/=16) THEN
              IF (ISM0==1) THEN
                ISSN = 11
              ELSE
                ISSN = 10
C                IF (MATPARAM%COMPRESSIBILITY==2) ISSN = 10
              END IF
             ELSE
              IF (ISM0==1) THEN
                ISSN = 1
              ELSE
                ISSN = 2
              END IF
             END IF
C---  ISMSTR=12 for law1     
             IF (MLN == 1.AND.JHBE/=16) ISSN = 12
C--------message out       
                CALL ANCMSG(MSGID=1742,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ISSN,
     .                      PRMOD=MSG_CUMU)
           END IF
C----- Iframe automatic JCVT=2 excpeting law58&fluide           
           IF (JCVT<0) THEN
            JCVT = 0 
            IF (IKSNOD0==8.AND.JLAG>0.AND.MLN/=68) JCVT = 1
C--------message out       
            ISM0 = JCVT+1
                CALL ANCMSG(MSGID=1764,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ISM0,
     .                      PRMOD=MSG_CUMU)
           END IF
          END IF
C---add ICPRE auto for T10, firsly limited by Large Strain          
          IF (IT10 ==1 .AND. (ISSN==4 .OR. ISSN==10)
     .                 .AND. IKSNOD0==10 ) THEN
             ICP0=MATPARAM%COMPRESSIBILITY
             SELECT CASE (ICP0)
C-------compressible       
              CASE(1)                                             
               ICPRE = 3        
C-------uncompressible       
              CASE(2)                                             
               ICPRE = 1
               IF (MLN == 1.OR.MLN == 92) ICPRE = 3
C--------elasto-plastic       
              CASE(3)                                             
               ICPRE = 2
             END SELECT 
C--------message out       
             IF (ICPRE ==1 .OR. ICPRE ==2)  CALL ANCMSG(
     .                      MSGID=1741,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ICPRE,
     .                      PRMOD=MSG_CUMU)
          ELSEIF (IT10 ==1) THEN
            ICPRE = 0
          END IF !IF (IT10 == 1 )
c          
          IF (MLN == 1.AND.ISSN<10) CALL INIT_MAT_KEYWORD(MATPARAM,"HYDROSTATIC")
          IF (ITSH == 1.AND. MATPARAM%IPRES/=1)   CALL ANCMSG(
     .                      MSGID=3012,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=MLN,
     .                      PRMOD=MSG_CUMU)
C--------------------
C TEST COMPATIBILITE
C--------------------

          !---------------------------------!
          !   ALE / EULER compatibility     !
          !---------------------------------!  
      IF (JALE+JEUL /= 0) THEN

            IF (JHBE>=2.AND.JHBE/=24) THEN   !FORMULATION CHECK
             CALL ANCMSG(MSGID=608,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=JHBE)            !  allows only: isolid=1(JHBE=1)
        ELSEIF (JCVT==1) THEN                !COROTATIONAL NOT ALLOWED  
              IF(JHBE==0) THEN
              CALL ANCMSG(MSGID=246,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=2)
          ELSE
              CALL ANCMSG(MSGID=246,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=JHBE)
          END IF
        END IF    

      END IF
          !---------------------------------!
C
          IF (IGT == 14.AND.MLN == 28 .AND.
     .        (JHBE == 0.OR.JHBE == 1.OR.JHBE == 12.OR.JHBE == 17)
     .         .AND.JCVT == 1) THEN
             CALL ANCMSG(MSGID=247,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=JHBE)
          ENDIF
          IF (IGT == 14.AND.(MLN == 14.OR.MLN == 24)
     .        .AND.(JHBE == 12.AND.JCVT == 1)) THEN
             CALL ANCMSG(MSGID=248,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=JHBE,
     .                   I3=MLN)
          ENDIF
C
          IF(IKSNOD0 == 4.AND.
     .     (JHBE==0.OR.JHBE==1.OR.JHBE==12.OR.JHBE==17).AND.JCVT==1)THEN
            CALL ANCMSG(MSGID=340,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGEO(1,PID))
        JCVT = 0 
          ELSEIF(IKSNOD0 == 4.OR.IKSNOD0 == 10)THEN
            JCVT = 0 
          ENDIF
          IF(IKSNOD0 == 4 .AND. JHBE /= 1 .AND. JHBE /= 2) THEN
           JHBE = 1
           NPT=1
           IF(ITET4 == 1) NPT=4
          ENDIF
          IF(IKSNOD0 == 10 .AND. JHBE /= 1 .AND. JHBE /= 2) THEN
c            CALL ANCMSG(MSGID=341,
c     .                  MSGTYPE=MSGWARNING,
c     .                  ANMODE=ANINFO_BLIND_2,
c     .                  I1=ID,
c     .                  C1=TITR,
c     .                  I2=JHBE)
          JCVT = 0     
          JHBE = 1
           IF (NPT/=4) NPT=4
          ENDIF
C
          IF(IKSNOD0 == 10 .AND. (JHBE == 1 .OR. JHBE == 2)
     .                     .AND. JCVT == 1) THEN
            CALL ANCMSG(MSGID=609,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR)
          JCVT = 0       
          ENDIF
c          IF (JHBE == 14 .AND. (IKSNOD0 == 16 .OR. IKSNOD0 == 20)) THEN
          IF (JHBE /= 16 .AND. (IKSNOD0 == 16 .OR. IKSNOD0 == 20)) THEN
            CALL ANCMSG(MSGID=860,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JHBE)
            JHBE = 16
            NPT = MAX(NPT, 222)
          ENDIF
          IF (JHBE == 14 .AND. IKSNOD0 /= 8) THEN
           IF(IKSNOD0 /= 4.AND.IKSNOD0 /= 10)THEN
            CALL ANCMSG(MSGID=758,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JHBE,
     .                  PRMOD=MSG_CUMU)
           ENDIF
            JHBE = 0
          ENDIF
          IF (JHBE == 15 .AND. IKSNOD0 /= 6 .AND. IKSNOD0 /= 8) THEN
           IF(IKSNOD0 /= 4.AND.IKSNOD0 /= 10)THEN
            CALL ANCMSG(MSGID=547,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JHBE)
           ENDIF
            JHBE = 0
          ENDIF
          IF (JHBE == 16 .AND. IKSNOD0 /= 16 .AND. IKSNOD0 /= 20) THEN
           IF(IKSNOD0 /= 4.AND.IKSNOD0 /= 10)THEN
            CALL ANCMSG(MSGID=548,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JHBE)
           ENDIF
            JHBE = 0
          ENDIF
          IF (JHBE == 24 .AND. IKSNOD0 /= 8) THEN
           IF(IKSNOD0 /= 4.AND.IKSNOD0 /= 10)THEN
            CALL ANCMSG(MSGID=758,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JHBE,
     .                  PRMOD=MSG_CUMU)
           ENDIF
            JHBE = 0
          ENDIF
          IF (JHBE==15 .AND. IGT/=20 .AND. IGT/=21 .AND. IGT/=22) THEN
            CALL ANCMSG(MSGID=549,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JHBE,
     .                  I3=IGT)
          ENDIF
          IF (JHBE/=15 .AND. IKSNOD0==6 ) THEN
            CALL ANCMSG(MSGID=639,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IXS(11,II))
          ENDIF
          IF ((JHBE == 17 .OR.JHBE == 18) .AND. IKSNOD0 /= 8) THEN
           IF(IKSNOD0 /= 4.AND.IKSNOD0 /= 10)THEN
            CALL ANCMSG(MSGID=758,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JHBE,
     .                  PRMOD=MSG_CUMU)
           ENDIF
            JHBE = 0
C-----    could happen when tet_hexa used the same pid            
            IF (ISSN==0) ISSN=4
          ENDIF
C------ default IHKT of HEPH 
          IF (JHBE == 24 .AND. IINT==0) THEN
           IINT =1
           IF (IMATVIS > 0 .OR.MLN==24) THEN
            SELECT CASE (MLN)
C--------hyperelastic, visco-elastic           
             CASE(38,42,62,69,70,82,88,90,92,94)             
              IINT =2
C--------SUPERELASTIC           
             CASE(71)                                             
              IINT =2
C--------compo (damage)          
             CASE(24)                                             
              IINT =2
            END SELECT 
           END IF
          END IF !(JHBE == 24 ) THEN
C------ new element like Is17  ,nu=PM(21,MID)        
          IF (JHBE == 18 ) THEN
           ICP0 = ICPRE
C -------------------          
            ICPRE = 1
            SELECT CASE (MLN)
C--------elastic ,visco-elastic           
             CASE(1,13,16,33,34,35,38,40,41,70,77,90)                                             
              ICPRE = 0
              IF (PM(21,MID)>=0.49) ICPRE = 1
C-----Keep IINT=2 for message and output (Isolid=18->17), remove free-shear in Engine               
C-------  free-shear locking removed            
C              IF (ICPRE==1.OR.ICP0==1) IINT =0
C--------hyper-elastic           
             CASE(42,62,69,82,88,92,94,100,101,111)                                             
C              IF (ICP0/=3) IINT =0
C--------case Icpre=2 of elasto-plastic            
             CASE(2,36)                                             
              ICPRE = 2
C--------elasto-plastic (not large plasticity)           
             CASE(21,22,23,24,27,52,79,81,84)                                             
              ICPRE = 2
C--------orhtotropic,composite (P isn't calculated independently)           
             CASE(12,14,15,25,28,50,53,68,76,93,107,112)                                             
              ICPRE = 0
            END SELECT 
           IF (ICP0 ==3) THEN
            IF (ICPRE/=0) THEN
C-------- warning out for no-zero input           
                CALL ANCMSG(MSGID=1573,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ICPRE,
     .                      PRMOD=MSG_CUMU)
              ICPRE = 0
            END IF 
           ELSE
            IF (ICP0 /=0 .AND. ICPRE/=ICP0) THEN
C-------- warning out for no-zero input           
                CALL ANCMSG(MSGID=1573,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ICPRE,
     .                      PRMOD=MSG_CUMU)
              ICPRE = ICP0
            END IF 
           END IF !(ICP0 ==3) THEN
C -------------------          
           ISM0 = ISSN
              ISSN=2
            SELECT CASE (MLN)
C--------elastic can be both 4,10          
             CASE(1)                                             
C--------hyper-elastic
              IF (ISM0 == 10.OR.ISM0 == 12) ISSN = ISM0         
             CASE(42,62,69,82,88,92,94,95,100,101,111)                                             
              ISSN=10
C--------visco_elastic  add 90       
             CASE(38,90)                                             
              ISSN=10
C--------special          
             CASE(28)                                             
              ISSN=1
C--------visco_elastic          
             CASE(70)                                             
              ISSN=11
            END SELECT 
           IF (ISM0 /=0 .AND. ISSN/=ISM0) THEN
C-------- warning out for no-zero input           
                CALL ANCMSG(MSGID=1574,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ISSN,
     .                      PRMOD=MSG_CUMU)
            ISSN = ISM0
           END IF 
           JHBE =17
           IF (ICP0==0.OR.ISM0==0) THEN
                CALL ANCMSG(MSGID=1575,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ISSN,
     .                      I3=ICPRE,
     .                      PRMOD=MSG_CUMU)
           END IF 
          ENDIF !IF (JHBE == 18 )
C-----due to ICPRE automatic          
          IF (ICPRE == 3.AND.(IGT == 14.OR.IGT == 6)) ICPRE =0
          IF((MLN == 95 .OR. MLN == 100 .OR. MLN == 101 .OR. MLN == 111) .AND. ISSN /= 10 ) THEN
            ISSN = 10
                CALL ANCMSG(MSGID=1200,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=MLN)
         
          ENDIF   
          IF (IKSNOD0 == 16 .OR. IKSNOD0 == 20) THEN
          JCVT = 0                      
          ENDIF


C------For Incompatibility w/ Isolid=12, don't add new law    
          IF( IKSNOD0 == 8        .AND.IABS(JHBE) < 200 .AND. NPT == 8
     .       .AND.IABS(JHBE) /= 14.    AND.IABS(JHBE) /= 15 
     .       .AND.IABS(JHBE) /= 24    .AND.IABS(JHBE) /= 17
     .       .AND.IABS(JHBE) /= 18) THEN
C     
            IF(MLN /= 1  .AND. MLN/= 2   .AND. MLN /= 3  .AND.  
     .         MLN /= 28 .AND. MLN /= 29 .AND. MLN /= 30 .AND.
     .         MLN /=31  .AND. MLN/= 33  .AND. MLN /= 34 .AND.  
     .         MLN /= 35 .AND. MLN /= 36 .AND. MLN /= 38 .AND.
     .         MLN /= 39 .AND. MLN /= 40 .AND. MLN /= 41 .AND.  
     .         MLN /= 42 .AND. MLN /= 44 .AND. MLN /= 45 .AND.
     .         MLN /= 48 .AND. MLN /= 50 .AND. MLN /= 52 .AND.  
     .         MLN /= 53 .AND. MLN /= 56 .AND. MLN /= 60 .AND.
     .         MLN /= 62 )THEN
               JHBE = 17  
               ICPRE = 1
               IGEO(10, PID) = 17  
               IGEO(13, PID) = 1 
                CALL ANCMSG(MSGID=869,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=MLN)
            ENDIF              
c     .     AND.(MLN == 4.OR.MLN == 6.OR.MLN == 10.OR.MLN == 21.
c     .     OR.MLN == 22.OR.MLN == 23.OR.MLN == 24.OR.MLN == 49))THEN
cc           CALL ANSTCKI(MLN)
cc          CALL ANCERR(601,ANINFO_BLIND_2)      
          ENDIF   
          IF (ISSN == 10 .OR. ISSN == 12) THEN
            IF(MLN /= 38 .AND. MLN /= 42 .AND. MLN /= 62  .AND.
     .         MLN /= 69 .AND. MLN /= 82 .AND. MLN /= 92  .AND. 
     .         MLN /= 99 .AND. MLN /= 1  .AND. MLN /= 88  .AND. 
     .         MLN /= 71 .AND. MLN /= 94 .AND. MLN /= 90  .AND. 
     .         MLN /= 95 .AND. MLN /=100 .AND. MLN /= 101 .AND.
     .         MLN /= 111 ) THEN
C--------------warning out     
                CALL ANCMSG(MSGID=1092,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=MLN)
                IF (ISSN == 12) THEN
                 ISSN = 2
                ELSE                 
                 ISSN = ISST_DS                           
                 IF (ISSN == 0 .OR. ISSN == 10) ISSN = 4                            
                END IF
            END IF
          ELSE IF (ISSN == 11) THEN
            IF(MLN /= 1 .AND.MLN /= 38 .AND. MLN /= 70 .AND. MLN /= 77 
     .         .AND. MLN /= 90  )THEN
C--------------error out     
                CALL ANCMSG(MSGID=1093,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=MLN)
            END IF
          ENDIF
          IF((MLN == 70 .OR. MLN == 77) .AND. ISSN /= 11 .AND. ISSN /= 1) ISSN = 11
C---------------remove Icpre=11 excepting for certain visco-elastic 34
          IF( ICPRE==1.AND.IKSNOD0 ==8.AND.(JHBE==14.OR.JHBE==17))THEN
C
            IF(MLN == 34 )THEN
C---------- no warnning message as ICPRE = 11 is the internal flag 
               ICPRE = 11
               IGEO(13, PID) = 11
            END IF
            IF(MLN == 28 )THEN
               ICPRE = 0
               IGEO(13, PID) = 0
               CALL ANCMSG(MSGID=1585,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR)
            END IF
          END IF
C
          IF (JHBE == 16.AND.MLN == 25)THEN
             CALL ANCMSG(MSGID=855,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=JHBE,
     .                   I3=MLN)
          ENDIF
          IF (MLN ==200.AND.(IGT == 14.OR.IGT == 20))THEN
             CALL ANCMSG(MSGID=2035,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=MLN)
          ENDIF
C-------------------------------------------------          
C         composite------------- 
          IF (JHBE==14 .OR. JHBE==16) THEN
            NPTR=ABS(NPT)/100
            NPTS=MOD(ABS(NPT)/10,10)
            NPTT=MOD(ABS(NPT),10)
            NPG = NPTR*NPTS*NPTT
               NLY = NPTS
          ELSE
            NPG = NPT
               NLY = NPT
          ENDIF  
C   -----for mix hardening------------          
          IF (IGT==22) THEN
           IF (JHBE==14 ) THEN
            SELECT CASE (ICSTR)
             CASE(100)                                             
              NLY = NPTR
              IF (NLY ==0) THEN
                  NLY =IINT 
               NPG = NLY*NPTS*NPTT
                 ENDIF 
             CASE(10)                                          
              NLY = NPTS
              IF (NLY ==0) THEN
                  NLY =IINT 
               NPG = NLY*NPTR*NPTT
                 ENDIF 
             CASE(1)                                          
              NLY = NPTT
              IF (NLY ==0) THEN
                  NLY =IINT 
               NPG = NLY*NPTR*NPTS
                 ENDIF 
            END SELECT 
           ENDIF  
           DO NL=1,NLY
            IM=IGEO(IPMAT+NL,PID)
            ILAW=NINT(PM(19,IM))
C-------for the case where we use ISRAT=0 law in Part define and ISRAT=1 laws in certain layers
            ISRAT=MAX(ISRAT,IPM(3,IM))
           ENDDO 
          ENDIF 

C-------
        NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT) ! Nb of elements in the group
        IF(ISSN > 4 .AND. IBOLTP /= 0)THEN
C
          ISSN_ = ISSN
C
          IF(ISSN_==10)THEN
            ISSN=4
          ELSEIF(ISSN_==11)THEN
            ISSN=1
          ELSEIF(ISSN_==12)THEN
            ISSN=2
          END IF
C
          DO J=1,NE1
            II=EADD(N)+NFT+J-1
C 
C           Preloaded elements are not compatible with Total Strain formulations
            ID=IXS(NIXS,II)
            CALL ANCMSG(MSGID=1775,
     .                  MSGTYPE=MSGWARNING,
     .                  I1=ID,
     .                  I2=IPARTS(II),
     .                  I3=ISSN_,
     .                  I4=ISSN ,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  PRMOD=MSG_CUMU)
          END DO
        ENDIF
        IF(IKSNOD0 == 10.AND.ITET10==3)THEN
         IINT = ITET10
         ITET10 = 2
        ELSEIF(IKSNOD0 == 10.AND.ITET10==2)THEN
         IINT = 0
        ENDIF
C------ compatibility w/ AMS        
        IF (IKSNOD0 == 10.AND.ITET10==2.AND.ISMS>0) THEN
              CALL ANCMSG(MSGID=2024,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  PRMOD=MSG_CUMU)
        END IF
C-------
        ! remove incompatibility for Itet4 = 3 
        IF (ISSN < 10 .AND. ITET4 == 3 .AND. ASSOCIATED(MATPARAM)) THEN
          IF(MATPARAM%STRAIN_FORMULATION==2) THEN 
          CALL ANCMSG(MSGID=2037,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=IGEO(1,PID),    
     .                I2=ISSN,    
     .                PRMOD=MSG_CUMU)
          ISSN = 10
          ENDIF
        ENDIF
C-------------------------------------------------
C STOCKAGE IPARG
C-------------------------------------------------
          IPARG(1,NGROUP) = MLN
          IPARG(2,NGROUP) = NE1
          IPARG(3,NGROUP) = EADD(N)-1 + NFT
          IPARG(4,NGROUP) = LBUFEL+1 !  kept in place for compatibility with other groups using old buffer
          IPARG(5,NGROUP) = 1
          IPARG(7,NGROUP) = JALE
          IPARG(9,NGROUP) = ISSN
          IPARG(11,NGROUP)= JEUL
          IPARG(12,NGROUP)= JTUR
          IPARG(13,NGROUP)= -ABS(JTHE)
          IF(JALE+JEUL /= 0)THEN
            IPARG(13,NGROUP)= +ABS(JTHE)
          ENDIF
          IPARG(14,NGROUP)= JLAG
          IPARG(10,NGROUP)= ICPRE  
          IPARG(17,NGROUP)= ICSTR  
          IPARG(6,NGROUP) = NPT
          IPARG(18,NGROUP)= MID 
          IPARG(20,NGROUP) = 0
          IF (MLN == 151) IPARG(20,NGROUP) = IPM(20, MID)
          IPARG(23,NGROUP)= JHBE
          IPARG(24,NGROUP)= JIVF
          IPARG(27,NGROUP)= JPOR
          IPARG(28,NGROUP)= IKSNOD0
          IPARG(29,NGROUP)= IPLAST
          IPARG(34,NGROUP)= NINT(PM(10,MID))
C reperage groupe/processeur
          IPARG(32,NGROUP)= P-1
          IPARG(33,NGROUP)= JCLOS
          IPARG(35,NGROUP)= IREP
          IPARG(36,NGROUP)= IINT
          IPARG(37,NGROUP)= JCVT
          IPARG(38,NGROUP)= IGT
          IPARG(42,NGROUP)= ISORTH
          IPARG(40,NGROUP)= ISRAT
          IPARG(43,NGROUP)= IFAIL
          IF(MLN == 68)THEN
            IPARG(41,NGROUP)=1
          ELSE
            IPARG(41,NGROUP)=ITET4
          ENDIF
          IF(MLN/=25.AND.MLN<28)THEN
          IPARG(44,NGROUP)= ISTRAIN
          ELSEIF(MLN>=28)THEN
           ISTRAIN=2
           IPARG(44,NGROUP)=ISTRAIN
          ENDIF
C
          IPARG(45,NGROUP ) = IMATVIS
C thermal material expansion 
          IF(IPM(218,MID) > 0 .AND. MLN /= 0 .AND. MLN /= 13) 
     .    IPARG(49,NGROUP)= 1  
          IPARG(52,NGROUP)= JSMS
C
C   - initial volume fraction -
C
          IPARG(53,NGROUP) = 0
          lFOUND=.FALSE.
          IF(NINIVOL > 0)THEN
          ! Warning : In same group you can have different PArts, A loop over elem in groups has to be introduced to check if INIVOL PART is there.
            MFT = IPARG(3,NGROUP)
            DO ILOC = 1 ,IPARG(2,NGROUP)
              DO JJ=1,NINIVOL
                IF(INIVOL(JJ)%IPARTFILL == IPARTS(ILOC+MFT)) THEN
                  IPARG(53,NGROUP) = 1
                  lFOUND=.TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(lFOUND)EXIT
            END DO
          END IF
C         equation of state
          IPARG(55,NGROUP)= IEOS
C         flag for vis stress
          IPARG(60,NGROUP)= ISVIS
          IPARG(61,NGROUP)= IVISC
          IPARG(62,NGROUP)= PID  ! property number
          IPARG(69,NGROUP)= IPARTSPH
C         flag for group of duplicated elements in multidomains
          IF (NSUBDOM>0) IPARG(77,NGROUP)= IPARTR2R
C         flag for bolt preloading
          IPARG(72,NGROUP)= IBOLTP
C
C         Formulation level for the solid elements time step computation
          IPARG(73,NGROUP)=IPM(252,MID)
          IPARG(74,NGROUP)=ITET10
c         non-local variable regularization flag for failure models
          IPARG(78,NGROUP) = IPM(253,MID)   ! NLOC_FAIL
C------------------------------------------------------
C LONGUEUR BUFFER
C------------------------------------------------------
          IF (NPG > 1) NPG = NPG + 1
          IF (NPG == 1 .AND. JHBE == 15) NPG = NPG + 1
C
          NFT = NFT + NE1
  220    CONTINUE
         NGP(P)=NGROUP-NGP(P)
        ENDIF
       ENDDO
C DD_IAD => nb groupes par sous domaine
       NGP(NSPMD+1)=0
       DO P = 1, NSPMD
         NGP(NSPMD+1)=NGP(NSPMD+1)+NGP(P)
         DD_IAD(P,NSPGROUP+N)=NGP(P)
       END DO
       DD_IAD(NSPMD+1,NSPGROUP+N)=NGP(NSPMD+1)
C
  300 CONTINUE

      NSPGROUP = NSPGROUP + ND
C
C Traitement CFD sur MID negatif sur tous les solides si ineg=1
C
      IF (INEG == 1) THEN
        DO I = 1, NUMELS
          IXS(1,I) = ABS(IXS(1,I))
        ENDDO
      ENDIF
      WRITE(IOUT,1010) 
c-----------
      DO N=NGR1,NGROUP
        JHBE=IPARG(23,N)
        NPT =IPARG(6,N)
        IF (JHBE==14 .OR. JHBE==16) THEN          
         NPTS=NPT/100*MOD(NPT/10,10)*MOD(NPT,10)
          IF (NPTS == ZERO) THEN          
            NPTS=ABS(NPT)/100
            IINT=IPARG(36,N)
            IF (NPTS==0) NPTS=IINT
            NPTT=MOD(ABS(NPT)/10,10)
            IF (NPTT==0) NPTT=IINT
            NPTR=MOD(ABS(NPT),10)
            IF (NPTR==0) NPTR=IINT
            NPG = NPTS*NPTT*NPTR
            NPTS=NPG                
          ENDIF         
        ELSE
         NPTS=NPT
        ENDIF
        WRITE(IOUT,1011)N,IPARG(1,N),IPARG(2,N),IPARG(3,N)+1,
     +                IPARG(4,N),NPTS,IPARG(7,N),IPARG(11,N),    
     +                IPARG(12,N),IPARG(13,N),IPARG(23,N),
     +                IPARG(24,N),IPARG(18,N),IPARG(27,N),
     +                IPARG(29,N)+1,
     +                IPARG(33,N),IPARG(34,N),IPARG(43,N),IPARG(55,N)
      ENDDO
      CALL ANCMSG(MSGID=758,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1112,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1160,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1225,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1905,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1573,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1574,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1575,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1586,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1741,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1742,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1764,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
C-----------
      CALL ANCMSG(MSGID=1775,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=2024,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=2037,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3012,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
c-----
      DEALLOCATE(INDEXS2)
      IF (NPERTURB > 0) THEN
        IF (ALLOCATED(XNUM_RNOISE)) DEALLOCATE(XNUM_RNOISE) 
      ENDIF
c------------------------------------------------------------
 1010 FORMAT(//,
     + 7X,'8-NODE ELEMENT GROUPS'/
     + 7X,'---------------------'//
     +'     GROUP      MAT.     ELEM.     FIRST    BUFFER     GAUSS',
     +'    A.L.E.     EULER    TURBU.    THERM.     HOUR-     INTEG',
     +'       VAR    POROUS   PLASTI.     CLOS.      CODV  FAILURE',
     +'     IEOS',/     
     +'         #       LAW    NUMBER     ELEM.   ADDRESS    POINTS',
     +'      FLAG      FLAG      FLAG      FLAG     GLASS      FLAG',
     +'       MID    MEDIUM      FLAG      FLAG              FLAG',
     +'      TYPE' )
 1011 FORMAT(19(I10))
 1002 FORMAT(' BUFFER LENGTH : ',I10 )
c-----------
      RETURN
      END
