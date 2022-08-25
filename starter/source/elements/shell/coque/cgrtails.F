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
Chd|  CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CGRTAILS(
     1       IXC    ,PM      ,IPARG     ,GEO     ,
     2       EADD   ,ND      ,NELC      ,IPARTC  ,DD_IAD,
     3       IDX    ,LB_MAX  ,INUM      ,ITR1    ,ITR2,
     4       INDEX  ,ITRI2   ,CEP       ,THK     ,XNUM,
     5       IGRSURF,IGRSH4N ,IGEO      ,IPM     ,
     6       IPART  ,SH4TREE ,NOD2ELC   ,ISHEOFF ,
     7       SH4TRIM ,TAGPRT_SMS,LGAUGE  ,IWORKSH ,
     8       STACK   ,DRAPE    ,RNOISE  ,MATPARAM_TAB,
     9       SH4ANG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD 
      USE STACK_MOD     
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE MATPARAM_DEF_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXC(NIXC,NUMELC)   TABLEAU MID(1)+CONECS(2-5)+PID(6)+         E
C                        N GLOBAL(7)                               E
C     PM(NPROPM,NUMMAT)  TABLEAU DES CARACS DES MATERIAUX           E
C     IPARG(NPARG,NGROUP)TABLEAU DES CARACTERISTIQUES DES GROUPES   E/S
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     EADD(NUMELC)       TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER E 
C     DD_IAD             TABLEAU DE LA DD EN SUPER GROUPES          S
C     IPARTC                                                        E/S
C     INUM(9,NUMELC)     TABLEAU DE TRAVAIL                         E/S
C     ITR1(NSELC)        TABLEAU DE TRAVAIL                         E/S
C     ITR2(NSELC)        TABLEAU DE TRAVAIL                         E/S
C     INDEX(NUMELC)      TABLEAU DE TRAVAIL                         E/S
C     ITRI2(NUMELC)      TABLEAU DE TRAVAIL                         E/S
C     THK(NUMELC)        TABLEAU EPAISSEUR                          E/S
C     XNUM(NUMELC)       TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELC)        TABLEAU DE TRAVAIL                         E/S
C     ISHEOFF(NUMELC)   FLAG ELEM RBY ON/OFF                        E/S
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "remesh_c.inc"
#include      "sms_c.inc"
#include      "r2r_c.inc"
#include      "drape_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER ND, IDX, LB_MAX
      INTEGER IXC(NIXC,*),IPARG(NPARG,*),EADD(*),IGEO(NPROPGI,*),
     .    DD_IAD(NSPMD+1,*),IPARTC(*), NELC(*), SH4TRIM(*),
     .    INUM(9,*),ITR1(*),ITR2(*),INDEX(*),ITRI2(*),CEP(*),
     .    IPM(NPROPMI,*), IPART(LIPART1,*), SH4TREE(KSH4TREE,*), 
     .    ISHEOFF(*),MINDEXC(NUMELC), TAGPRT_SMS(*),LGAUGE(3,*),
     .    NOD2ELC(*),IWORKSH(3,*)
C     REAL OU REAL*8
      MY_REAL
     .    PM(NPROPM,*), GEO(NPROPG,*), XNUM(*),THK(*),RNOISE(NPERTURB,*),
     .    SH4ANG(*)
      TYPE(MATPARAM_STRUCT_) , TARGET, DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM_TAB
C-----------------------------------------------     
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE (*)
      TYPE (DRAPE_) , DIMENSION(:), ALLOCATABLE :: XNUM_DRAPE
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSHEL)  :: IGRSH4N
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, K, NGR1, MLN, NG, ISMST, ICSEN, JLEV, MY_NVSIZ, IADM,NLEVXF,
     .   NPN, N, MID, PID, IHBE,NPT1,NPT2,NPG,IXFEM_ERR,
     .   II, J, MIDN, PIDN, NSG, NEL, NE1, ITHK, ITHK0,
     .   IPLA, IGTYP, KFTS, P, NEL_PREC,
     .   NELGR,IDEBGR,IFINGR,IDEB,IFIN,NPASS,NELTRI,L,NEL1,NB,
     .   NN,IAD1,NGROU,PRT,PRTN,
     .   IMATLY, IPT,MLNLY, ILEV,MPT, IE, NUVARR,IPIDL,
     .   NGP(NSPMD+1),N1,IRUP,NVARV,IVISC,IFWV,IXFEM,IPTUN,IMID,IREP,
     .   INUM_R2R(1+R2R_SIU*NUMELC),ISUBSTACK,IIGEO,IADI,IPMAT, IPPID,
     .   IPARTR2R,NB_LAW58,IPERT,STAT,IGMAT,IPINCH,ISM0,ISEATBELT
      my_real
     .  ANGLE(NUMELC)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEXS2

      INTEGER MODE,WORK(70000),NPTEL(NUMELC)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISTOR
C     REAL OU REAL*8
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR,TITR1,TITR2
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--

      IF(NADMESH /= 0)THEN
        ALLOCATE( ISTOR(KSH4TREE+1,NUMELC) )
      ELSE
        ALLOCATE( ISTOR(0,0) )
      ENDIF

      MY_ALLOCATE(INDEXS2,NUMELC)
      INDEXS2(1:NUMELC)=PERMUTATION%SHELL(1:NUMELC)
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELC),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
      ENDIF
C
      IPTUN = 1
      IXFEM_ERR = 0
C--------------------------------------------------------------
C         BORNAGE DES GROUPES DE MVSIZ
C--------------------------------------------------------------
      NGR1 = NGROUP + 1
C
C phase 1 : domain decompostition
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
      DO N=1,ND
        NEL = EADD(N+1)-EADD(N)
C      
          IF(NDRAPE > 0) THEN
             ALLOCATE( XNUM_DRAPE(NEL))
             DO I =1, NEL
               NPT = DRAPE(I+NFT)%NPT
               ALLOCATE(XNUM_DRAPE(I)%RDRAPE(NPT,2),XNUM_DRAPE(I)%IDRAPE(NPT))
               XNUM_DRAPE(I)%RDRAPE = 0
               XNUM_DRAPE(I)%IDRAPE = 0
             ENDDO 
          ENDIF 
C          
        DO I = 1, NEL
          INDEX(I) = I
          INUM(1,I)=IPARTC(NFT+I)
          INUM(2,I)=ISHEOFF(NFT+I)
          INUM(3,I)=IXC(1,NFT+I)
          INUM(4,I)=IXC(2,NFT+I)
          INUM(5,I)=IXC(3,NFT+I)
          INUM(6,I)=IXC(4,NFT+I)
          INUM(7,I)=IXC(5,NFT+I)
          INUM(8,I)=IXC(6,NFT+I)
          INUM(9,I)=IXC(7,NFT+I)
          XNUM(I)=THK(NFT+I)
          IF (NSUBDOM.GT.0) INUM_R2R(I) = TAG_ELCF(NFT+I)
!!          NPTEL(I) = IWORKC(1,NFT+I)
          IF (NDRAPE > 0) THEN
            NPT = DRAPE(NFT + I)%NPT
            XNUM_DRAPE(I)%NPT = NPT
            DO IPT = 1, NPT
              XNUM_DRAPE(I)%IDRAPE(IPT)   =DRAPE(NFT+I)%IDRAPE(IPT)
              XNUM_DRAPE(I)%RDRAPE(IPT,1) =DRAPE(NFT+I)%RDRAPE(IPT,1)
              XNUM_DRAPE(I)%RDRAPE(IPT,2) =DRAPE(NFT+I)%RDRAPE(IPT,2)
            ENDDO
          ENDIF
          IF (NPERTURB > 0) THEN
            DO IPERT = 1, NPERTURB
              XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,NFT+I) 
            ENDDO
          ENDIF
          !!
          ANGLE(I) = SH4ANG(NFT+I)          
        ENDDO
C
 	IF(NADMESH.NE.0)THEN
 	  DO  K=1,KSH4TREE
 	    DO  I=1,NEL
 	      ISTOR(K,I)=SH4TREE(K,NFT+I)
 	    ENDDO
 	  ENDDO
 	  IF(LSH4TRIM/=0)THEN
 	    DO  I=1,NEL
 	      ISTOR(KSH4TREE+1,I)=SH4TRIM(NFT+I)
 	    ENDDO
 	  END IF
 	END IF
C
      
        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          PERMUTATION%SHELL(I+NFT)=INDEXS2(INDEX(I)+NFT)
          IPARTC(I+NFT) =INUM(1,INDEX(I))
          ISHEOFF(I+NFT)=INUM(2,INDEX(I))
          THK(I+NFT)   =XNUM(INDEX(I))
          IXC(1,I+NFT)=INUM(3,INDEX(I))
          IXC(2,I+NFT)=INUM(4,INDEX(I))
          IXC(3,I+NFT)=INUM(5,INDEX(I))
          IXC(4,I+NFT)=INUM(6,INDEX(I))
          IXC(5,I+NFT)=INUM(7,INDEX(I))
          IXC(6,I+NFT)=INUM(8,INDEX(I))
          IXC(7,I+NFT)=INUM(9,INDEX(I))
          IF (NSUBDOM.GT.0) TAG_ELCF(NFT+I) = INUM_R2R(INDEX(I))
          ITR1(NFT+INDEX(I)) = NFT+I
C         
!!          IWORKSH(1,NFT+I) = NPTEL(INDEX(I))
          IF (NDRAPE > 0) THEN
             NPT = XNUM_DRAPE(INDEX(I))%NPT 
             DRAPE(NFT + I)%NPT = NPT
            DO IPT = 1, NPT
              DRAPE(NFT + I)%IDRAPE(IPT)   = XNUM_DRAPE(INDEX(I))%IDRAPE(IPT)
              DRAPE(NFT + I)%RDRAPE(IPT,1) = XNUM_DRAPE(INDEX(I))%RDRAPE(IPT,1)
              DRAPE(NFT + I)%RDRAPE(IPT,2) = XNUM_DRAPE(INDEX(I))%RDRAPE(IPT,2)
            ENDDO
          ENDIF
          IF (NPERTURB > 0) THEN
            DO IPERT = 1, NPERTURB
              RNOISE(IPERT,I+NFT) = XNUM_RNOISE(IPERT,INDEX(I)) 
            ENDDO
          ENDIF
          !!
           SH4ANG(NFT+I) = ANGLE(INDEX(I))
        ENDDO

 	IF(NADMESH.NE.0)THEN
 	  DO  K=1,KSH4TREE
 	    DO  I=1,NEL
 	      SH4TREE(K,I+NFT)=ISTOR(K,INDEX(I))
 	    ENDDO
 	  ENDDO
 	  IF(LSH4TRIM/=0)THEN
 	    DO  I=1,NEL
 	      SH4TRIM(I+NFT)=ISTOR(KSH4TREE+1,INDEX(I))
 	    ENDDO
 	  END IF
 	END IF

C dd-iad
        P = CEP(NFT+INDEX(1))
        NB = 1
        DO I = 2, NEL
          IF (CEP(NFT+INDEX(I)).NE.P) THEN
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
C
         IF(NDRAPE > 0) THEN
             DO I =1, NEL
               DEALLOCATE(XNUM_DRAPE(I)%RDRAPE,XNUM_DRAPE(I)%IDRAPE)
            ENDDO 
            DEALLOCATE( XNUM_DRAPE ) 
         ENDIF     
      ENDDO   ! ND       
C
C RENUMEROTATION DE L ARBRE
C
      IF(NADMESH.NE.0)THEN
        DO  I=1,NUMELC
          IF(SH4TREE(1,I).NE.0)
     .       SH4TREE(1,I)=ITR1(SH4TREE(1,I))
          IF(SH4TREE(2,I).NE.0)
     .       SH4TREE(2,I)=ITR1(SH4TREE(2,I))
        ENDDO
      END IF
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF (IGRSURF(I)%ELTYP(J) == 3)
     .        IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR shell in Accel (gauge)
C
      DO I=1,NBGAUGE
         N1 = LGAUGE(1,I)
         IF(N1 <= 0) THEN
            N1=-LGAUGE(3,I)
            IF(N1 > 0) LGAUGE(3,I)=-ITR1(N1)
         ENDIF
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SHELL
C
      DO I=1,NGRSHEL
        NN=IGRSH4N(I)%NENTITY
        DO J=1,NN
          IGRSH4N(I)%ENTITY(J) = ITR1(IGRSH4N(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C renumerotation CONNECTIVITE INVERSE
C
      DO I=1,4*NUMELC
        IF(NOD2ELC(I) /= 0)NOD2ELC(I)=ITR1(NOD2ELC(I))
      END DO
C              
C-------------------------------------------------------------------------
C phase 2 : bornage en groupe de mvsiz
C ngroup est global, iparg est global mais organise en fonction de dd
C
      DO 300 N=1,ND
       NFT = 0
       DO P = 1, NSPMD
        NGP(P)=0
        NEL = DD_IAD(P+1,NSPGROUP+N)-DD_IAD(P,NSPGROUP+N)
        IF (NEL.GT.0) THEN
         NEL_PREC = DD_IAD(P,NSPGROUP+N)-DD_IAD(1,NSPGROUP+N)
         NGP(P)=NGROUP
         DO WHILE (NFT < NEL_PREC+NEL)
C ngroup global
          NGROUP=NGROUP+1
          II = EADD(N)+NFT
          MID = IXC(1,II)
          MLN = NINT(PM(19,MID))
          PID = IXC(6,II)
          IF (NSUBDOM.GT.0) IPARTR2R = TAG_MAT(MID)
c          NPN = NINT(GEO(6,PID))
          NPN   = IGEO(4,PID)
          ISMST = IGEO(5,PID)
          IGTYP = IGEO(11,PID)
          ISROT = IGEO(20,PID)
          IPINCH= IGEO(51,PID)
C          IGTYP=NINT(GEO(12,PID))
          ISHXFEM_PLY = IGEO(19,PID)
          IREP = IGEO(6,PID)
          IHBE = NINT(GEO(171,PID))
          ITHK = NINT(GEO(35,PID))
          IPLA = NINT(GEO(39,PID))
          ISTRAIN = NINT(GEO(11,PID))
          ICSEN= IGEO(3,PID)
          IGMAT = IGEO(98 ,PID)
          NLEVXF = 0
          IXFEM = 0
          ISUBSTACK = 0
          IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            NPN = IWORKSH(1,II)
            ISUBSTACK =IWORKSH(3,II)
            IF(NPN == 0) THEN
              ID = IGEO(1,PID)
              CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,PID),LTITR)
              CALL ANCMSG(MSGID=1241,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,II))
               CALL ARRET(2)
            ENDIF
          ENDIF  
c-------- check xfem compatibility
c          IF (ICRACK3D > 0) THEN
            IF (IGTYP == 11 .or. IGTYP == 16) THEN
              DO IPT = 1, NPN                                    
                IMATLY  = IGEO(100+IPT,PID)
                IF (IPM(220,IMATLY) > 0) THEN
                  IXFEM = IPM(236,IMATLY)
                ENDIF
              ENDDO                                              
              IF (IXFEM  > 0) IXFEM = 1
              IF (IXFEM == 1) NLEVXF = NXEL*NPN
            ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
               IPPID   = 2
               IPMAT   = IPPID + NPN
               DO IPT = 1, NPN
                 IMATLY  = STACK%IGEO(IPMAT + IPT ,ISUBSTACK)
                 IF (IPM(220,IMATLY) > 0) IXFEM = IPM(236,IMATLY)
                 IF (IXFEM  > 0) IXFEM = 1
                 IF (IXFEM == 1) NLEVXF = NXEL*NPN
               ENDDO
            ELSEIF (IGTYP == 1 .or. IGTYP == 9 .or. IGTYP == 10 .or. IGTYP == 17) THEN
              IXFEM = IPM(236,MID)
              IF (IXFEM == 1) THEN
                IXFEM  = 2
                NLEVXF = NXEL 
              ENDIF
            ENDIF
            NLEVMAX = MAX(NLEVMAX, NLEVXF)
c          ENDIF
c
          IF (IHBE == 11 .and. IXFEM > 0) THEN  ! not compatible with Batoz shells
            IXFEM     = 0
            NLEVXF    = 0
            NLEVMAX   = 0
            NUMELCRK  = 0
            ICRACK3D  = 0
            IXFEM_ERR = 1
            CALL ANCMSG(MSGID=1601,
     .                  ANMODE=ANINFO,
     .                  MSGTYPE=MSGERROR,
     .                  I1=IGEO(1,PID),
     .                  C1=TITR,
     .                  PRMOD=MSG_CUMU)
          ENDIF
c--------
C         
          ID=IGEO(1,PID)
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,PID),LTITR)
C--------------------
C-   ISMSTR IPLAST,ITHICK Automatic           
C-----------all shell prop
          IF (IGTYP > 0)  THEN
           MATPARAM => MATPARAM_TAB(MID)
C----- fixed to 1 excepting small strain case          
           IF (ITHK<0) THEN
            ITHK = 1 
            IF (MATPARAM%SMSTR==1 .OR. MLN == 1) ITHK = 0
C--------message out       
            ISM0 = ITHK
            IF (ITHK == 0) ISM0=2
                CALL ANCMSG(MSGID=1770,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ISM0,
     .                      PRMOD=MSG_CUMU)
           END IF
C----- fixed to iterative, but can be changed in fonction of law           
           IF (IPLA<0) THEN
             IPLA = 1
C--------message out       
                CALL ANCMSG(MSGID=1771,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IPLA,
     .                      PRMOD=MSG_CUMU)
           END IF 
C------ --           
           IF (ISMST<0) THEN
C---  MATPARAM%SMSTR : 2 large, 1: small  ;   MATPARAM%STRAIN_FORMULATION : 1 inc, 2 total     
c       there is no recommended total strain for shell for the moment
              IF (MATPARAM%SMSTR==1) THEN
                ISMST = 1
              ELSE
                ISMST = 2
C----certain laws to use 4 to see one by one
                IF (MATPARAM%STRAIN_FORMULATION==2) ISMST =4             
                IF (MLN == 58 ) ISMST =4
                IF (MLN == 19 .AND. NPN==1) ISMST =11
              END IF
              GEO(3,PID) = ISMST
C--------message out       
                CALL ANCMSG(MSGID=1772,
     .                      MSGTYPE=MSGINFO,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ISMST,
     .                      PRMOD=MSG_CUMU)
           END IF
          END IF !(IGTYP > 0)  THEN
C-----
          IF (IGTYP == 16 .and. MLN == 58 .and. ISMST /= 4) THEN
            ISMST = 4
            CALL ANCMSG(MSGID=772,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   PRMOD=MSG_CUMU)
          ENDIF
          IF (IGTYP == 1 .AND. (MLN == 25 .OR. 
     .        MLN == 15  )) THEN
            CALL ANCMSG(MSGID=1052,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IPM(1,MID))
          ELSEIF (IGTYP == 1 .AND. (MLN ==57.OR. MLN ==78 .OR.
     .        MLN == 32 .OR. MLN == 43 .OR. MLN == 73.OR.MLN == 87
     .        .OR.MLN == 107.OR.MLN == 112) ) THEN
            CALL ANCMSG(MSGID=1065,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IPM(1,MID))
          ELSEIF (IGTYP == 1 .AND. MLN ==200)THEN
             CALL ANCMSG(MSGID=2035,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=MLN)
          ENDIF
c          IF (MLN == 71 .and. ISMST /= 10 ) THEN
           ! ISMST = 10
c                CALL ANCMSG(MSGID=1200,
c     .                      MSGTYPE=MSGWARNING,
c     .                      ANMODE=ANINFO_BLIND_2,
c     .                      I1=ID,
c     .                      C1=TITR,
c     .                      I2=MLN)
c          ENDIF
          IF (IGTYP == 1 .and. ISMST == 11 ) THEN
           ! ISMST = 11 compatibility
            ISMST = 2
                CALL ANCMSG(MSGID=1876,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=MLN,
     .                      I3=ISMST,
     .                      PRMOD=MSG_CUMU)
          ENDIF

          IF(IGTYP == 0)MLN=0
          IF(NADMESH == 0)THEN
            ILEV=0
            MY_NVSIZ=NVSIZ
          ELSE
            PRT = IPARTC(II)
            IADM= IPART(10,PRT)
            IF(IADM==0)THEN
              ILEV = 0
              MY_NVSIZ=NVSIZ
            ELSE
              ILEV= SH4TREE(3,II)
              IF(ILEV.LT.0)ILEV=-ILEV-1
              MY_NVSIZ=MAX(4,MIN(4**ILEV,NVSIZ))
            END IF
          END IF
c------
c         global integration 
          IF (NPN > 1  .and. MLN == 1) THEN
            NPN = 0
            CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MID),LTITR)             
            CALL ANCMSG(MSGID=1084,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  MSGTYPE=MSGWARNING,   
     .                  I1=ID,
     .                  C1=TITR,     
     .                  I2=IPM(1,MID),
     .                  C2=TITR2,
     .                  PRMOD=MSG_CUMU)
          ENDIF
          IF (NPN > 1  .and. MLN == 91) THEN
            NPN = 0
          ENDIF
c         swtich global integration to npt=3 
          IF (NPN == 0  .and. MLN /= 0 .and. MLN /= 1 .and. MLN /= 91) THEN
              CALL ANCMSG(MSGID=1912,
     .                    ANMODE=ANINFO,
     .                    MSGTYPE=MSGWARNING,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=MLN,
     .                    PRMOD=MSG_CUMU)
            NPN = 3
          ENDIF
          IF (NPN == 0  .and. MLN > 2   .and. MLN /= 22 .and. 
     .        MLN /= 36 .and. MLN /= 43 .and. MLN /= 60 .and.
     .        MLN /= 86 .and. MLN /= 13 .and. MLN /= 91) THEN
              CALL FRETITL2(TITR1,
     .                      IPM(NPROPMI-LTITR+1,MID),
     .                      LTITR)
              CALL ANCMSG(MSGID=23,
     .                    ANMODE=ANINFO,
     .                    MSGTYPE=MSGERROR,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IPM(1,MID),
     .                    C2=TITR1,
     .                    I3=MLN)
          ENDIF
c
          IF (NPN == 0.AND.(MLN == 36.OR.MLN == 86))THEN
            IF(IPLA == 0) IPLA=1
            IF(IPLA == 2) IPLA=0
          ELSEIF(NPN == 0.AND.MLN == 2)THEN
            IF(IPLA == 2) IPLA=0
          ELSE
            IF(IPLA == 2) IPLA=0
            IF(IPLA == 3) IPLA=2
          ENDIF
C
          IF(ITHK == 2)THEN
            ITHK = 0
          ELSEIF(MLN == 32)THEN
            ITHK = 1
          ENDIF
C---------Drilling dof---ISHELL=12(QBAT uses NB4)--------         
         IF (ISROT.GT.0.AND.IHBE.LT.11) THEN    	
          CALL ANCMSG(MSGID=854,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR)
          ISROT=0
         END IF
C------
          CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))
          IPARG(1,NGROUP) = MLN
          NE1 = MIN( MY_NVSIZ, NEL + NEL_PREC - NFT)
          IPARG(2,NGROUP) = NE1
          IPARG(3,NGROUP)=  EADD(N)-1 + NFT
          IPARG(4,NGROUP) = LBUFEL+1  !  kept in place for compatibility with
c                                        other groups using old buffer
          IPARG(43,NGROUP) = 0
C
         NVARV = 0
         IVISC = 0
         IFWV  = 0
C-------------
        
         IF (IGTYP == 11) THEN
           DO IPT = 1, NPN                                         
              IMATLY  = IGEO(100+IPT,PID)                          
              IF(IPM(220,IMATLY) >  0)THEN                         
                 IPARG(43,NGROUP) = 1                              
              ENDIF                                                
              IF (IPM(222,IMATLY) > 0 ) IVISC =  1                 
              IF (IPM(254,IMATLY) > 0)  IFWV = 1                         
           ENDDO                                                   
C-------------
         ELSEIF(IGTYP == 17) THEN
!!               IIGEO   =  40 + 5*(ISUBSTACK - 1) 
!!               IADI    = IGEO(IIGEO + 3,PID)
!!               IPPID   = IADI
!!               IPMAT   = IPPID + NPN
!!               IPMAT_IPLY   = IPMAT + NPN
               IPPID   = 2
               IPMAT   = IPPID + NPN
               DO IPT = 1, NPN
                IMATLY  = STACK%IGEO(IPMAT + IPT ,ISUBSTACK)
                IF(IPM(220,IMATLY) >  0)THEN
                 IPARG(43,NGROUP) = 1
                ENDIF
                IF( IPM(222,IMATLY) > 0 ) IVISC =  1  
               ENDDO 
C---
C new shell property (variable NPT through each layer)
C---
         ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
               NB_LAW58 = 0
               IPPID   = 2
               IPMAT   = IPPID + NPN
               DO IPT = 1, NPN
                 IMATLY  = STACK%IGEO(IPMAT + IPT ,ISUBSTACK)
                 IF (IPM(220,IMATLY) >  0) THEN
                   IPARG(43,NGROUP) = 1
                 ENDIF
                 IF (IPM(222,IMATLY) > 0) IVISC =  1  
                 IF (IPM(254,IMATLY) > 0) IFWV = 1                         
C --- PID 51 combined with LAW58 ---
                 IF (NINT(PM(19,IMATLY)) == 58) NB_LAW58 = NB_LAW58 + 1
               ENDDO
C --- set new IREP for groups:
               IF (NB_LAW58 == NPN) THEN
                 IREP = 2
               ELSEIF (NB_LAW58 > 0) THEN
                 IREP = IREP + 3
               ENDIF
C-------------
         ELSE  ! IGTYP = 1
            IF(IPM(220 ,MID) > 0.AND.MLN /= 0 .AND. MLN /=13)THEN
              IPARG(43,NGROUP) = 1
            ENDIF
            IF (IPM(222,MID) > 0 ) IVISC =  1  
            IF (IPM(254,MID) > 0)  IFWV = 1                         
         ENDIF ! IGTYP 
C-------------
C         
         IF (MLN == 13) IRIGID_MAT = 1
         JTHE = ABS(NINT(PM(71,MID)))
         IF(ITHERM_FE == 0 .AND. JTHE > 0) THEN
           ID=IPM(1,MID)
           CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,MID),LTITR)
           CALL ANCMSG(MSGID=667,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR)
         ENDIF
C        thermal material expansion 
         IPARG(49,NGROUP) = 0
         IF(IPM(218,MID) > 0 .AND. MLN /= 0 .AND. MLN /=13) THEN
          IPARG(49,NGROUP) = 1
         ENDIF
C        Visco model using /VISC
         IF (IVISC > 0 .AND. MLN /= 0 .AND. MLN /=13) THEN
           IPARG(61,NGROUP) = 1
         ENDIF   
C          
         JSMS=0
         IF(ISMS/=0)THEN
           IF(IDTGRS/=0)THEN
             IF(TAGPRT_SMS(IPARTC(II))/=0)JSMS=1
           ELSE
             JSMS=1
           END IF
         END IF
         IPARG(52,NGROUP)=JSMS
C---------
          IPARG(54,NGROUP) = IXFEM
          IPARG(65,NGROUP) = NLEVXF
C         flag for group of dupplicated elements in multidomains
          IF (NSUBDOM.GT.0) IPARG(77,NGROUP)= IPARTR2R
          IPARG(5,NGROUP)  = 3          ! ITY
          IPARG(6,NGROUP)  = NPN
          IPARG(9,NGROUP)  = ISMST
          IPARG(13,NGROUP) = JTHE
          IPARG(23,NGROUP) = IHBE
          IPARG(28,NGROUP) = ITHK
          IPARG(29,NGROUP) = IPLA
          IPARG(41,NGROUP) = ISROT
          IPARG(44,NGROUP) = ISTRAIN
          IPARG(62,NGROUP) = PID
          IPARG(90,NGROUP) = IPINCH
C
          ISEATBELT = 0
          IF(MLN == 119) ISEATBELT = 1
          IPARG(91,NGROUP) = ISEATBELT
C
          NSG = 1
          KFTS= 0
          DO 210 J = 2,NE1
            MIDN = IXC(1,J+EADD(N)+NFT-1)
            PIDN = IXC(6,J+EADD(N)+NFT-1)
            IF(MID.NE.MIDN.OR.PID.NE.PIDN)THEN
              PID = PIDN
              MID = MIDN
              NSG = NSG + 1
              KFTS= J
            ENDIF
  210     CONTINUE
C
          IPARG(10,NGROUP)= NSG
          IPARG(18,NGROUP)= MID
          IPARG(30,NGROUP)= KFTS
          IPARG(35,NGROUP)= IREP
          IPARG(38,NGROUP)= IGTYP
          IPARG(39,NGROUP)= ICSEN
          IPARG(45,NGROUP)= ILEV
          IF(NADMESH/=0)THEN
            IPARG(8,NGROUP)=1
            DO J=1,NE1
              SH4TREE(4,J+EADD(N)+NFT-1)=NGROUP
              JLEV=SH4TREE(3,J+EADD(N)+NFT-1)
              IF(JLEV >= 0)IPARG(8,NGROUP)=0
            END DO
          END IF

          NUVARR = 0
          IF (IGTYP == 11) THEN
            MPT  = IABS(NPN)
            DO IPT= 1,MPT
             DO J=1,NE1
              IE=J+EADD(N)+NFT-1
              IMATLY  = IGEO(100+IPT,IXC(6,IE))
              NUVARR = MAX(NUVARR,IPM(221,IXC(1,IE)))
             ENDDO
            ENDDO
          ELSE
            DO J=1,NE1
             IE=J+EADD(N)+NFT-1
             NUVARR = MAX(NUVARR,IPM(221,IXC(1,IE)))
            ENDDO
          END IF
          IPARG(47,NGROUP)=NUVARR


          IF(IHBE == 11)THEN
            NPG=4
          ELSE
            NPG=1
          END IF
          IPARG(48,NGROUP)=NPG
C          IPARG(49,NGROUP) = ICXFEM
C reperage groupe/processeur
          IPARG(32,NGROUP) = P-1
          IPARG(50,NGROUP) = ISHXFEM_PLY  
C for stack         
          IPARG(71,NGROUP) = ISUBSTACK 
          IPARG(75,NGROUP) = IGMAT
c         non-local variable regularization flag for failure models
          IPARG(78,NGROUP) = IPM(253,MID)   ! NLOC_FAIL 
          IPARG(79,NGROUP) = IFWV
C
          NFT = NFT + NE1  
C         
         END DO
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
c
      IF (IXFEM_ERR == 1) ICRACK3D = 0
C
      NSPGROUP = NSPGROUP + ND
C-----------
      CALL ANCMSG(MSGID=1084,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  MSGTYPE=MSGWARNING,
     .                  PRMOD=MSG_PRINT) 
c
      CALL ANCMSG(MSGID=1601,                 
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            I1=PID,
     .            C1=TITR ,
     .            PRMOD=MSG_PRINT)       
      CALL ANCMSG(MSGID=1770,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1771,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1772,
     .                  MSGTYPE=MSGINFO,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1876,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1912,                 
     .            ANMODE=ANINFO_BLIND_2,
     .            MSGTYPE=MSGWARNING,
     .            PRMOD=MSG_PRINT)       
      CALL ANCMSG(MSGID=772,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
C-----------
      WRITE(IOUT,1000) 
      DO  N=NGR1,NGROUP
        MLN = IPARG(1,N)       
c
        WRITE(IOUT,1001)N,MLN,IPARG(2,N),IPARG(3,N)+1,
     +                IPARG(5,N),IABS(IPARG(6,N)),
     +                IPARG(9,N),IPARG(10,N),IPARG(44,N),
     +                IPARG(23,N),IPARG(43,N),IPARG(90,N)   
      ENDDO
C-----------
 1000 FORMAT(/
     +       /6X,'3D - SHELL ELEMENT GROUPS'/
     +        6X,'-------------------------'/
     +'      GROUP   MATERIAL    ELEMENT      FIRST',
     +'    ELEMENT      INTEG',
     +'      SMALL        SUB     STRAIN  HOURGLASS    FAILURE   PINCHING'/
     +'                   LAW     NUMBER    ELEMENT',
     +'       TYPE        PTS',
     +'     STRAIN     GROUPS     OUTPUT       FLAG       FLAG       FLAG'/)
 1001 FORMAT(12(1X,I10))
cc 1002 FORMAT(/6X,'BUFFER LENGTH : ',I10 )
C
      IF (NPERTURB > 0) THEN
        IF (ALLOCATED(XNUM_RNOISE)) DEALLOCATE(XNUM_RNOISE) 
      ENDIF
C

      DEALLOCATE(INDEXS2)
      DEALLOCATE( ISTOR )
      RETURN
      END
