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
Chd|  QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE QGRTAILS(
     1       IXQ    ,PM      ,IPARG  ,GEO    ,
     2       EADD   ,ND      ,DD_IAD ,IDX    ,LB_MAX ,
     3       INUM   ,INDEX   ,CEP    ,IPARTQ ,NELQ   ,
     4       ITR1   ,IGRSURF ,IGRQUAD,
     5       IGEO   ,IPM     ,IQUAOFF,INIVOL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE INIVOL_DEF_MOD
      USE INIVOL_ARRAY_MOD
C-----------------------------------------------M
C            A R G U M E N T S
C-----------------------------------------------
C     IXQ(7,NUMELQ)      TABLEAU CONECS+PID+MID+NOS SOLIDES 4N      E
C     PM(NPROPM,NUMMAT)  TABLEAU DES CARACS DES MATERIAUX           E
C     IPARG(NPARG,NGROUP)TABLEAU DES CARACTERISTIQUES DES GROUPES   E/S
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     EADD(NUMELQ)       TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER E 
C     DD_IAD             TABLEAU DE LA DD EN SUPER GROUPES          S
C     INUM(8,NUMELQ)     TABLEAU DE TRAVAIL                         E/S
C     INDEX(NUMELQ)      TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELQ)        TABLEAU DE TRAVAIL                         E/S
C     IPARTQ(NUMELQ)     TABLEAU DE TRAVAIL                         E/S
C     NELQ(NUMELQ)       TABLEAU DE TRAVAIL                         E/S
C     ITR1(NSELQ)        TABLEAU DE TRAVAIL                         E/S
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
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER ND, IDX, LB_MAX
      INTEGER IGEO(NPROPGI,NUMGEO),IPM(NPROPMI,NUMMAT), IXQ(NIXQ,NUMELQ),IPARG(NPARG,*),
     .        EADD(*),DD_IAD(NSPMD+1,*),INUM(9,*),INDEX(*),
     .        CEP(*),IPARTQ(*),NELQ(*),ITR1(*),
     .        IQUAOFF(*)
      TYPE (INIVOL_) , DIMENSION(NINIVOL) :: INIVOL
      MY_REAL PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRQUAD)  :: IGRQUAD
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .   NGR1, MLN, NG, N, MID, PID, II, NEL, NE1,
     .   P, NEL_PREC, LB_L, IGT, JHBE, I,
     .   ML1, ML2, MT1, MT2,NB,INEG,IEOS,
     .   MODE, WORK(70000),NN,J,
     .   IPLAST, IALEL, MT,IFAIL,NFAIL,FAILMODEL,
     .   NGP(NSPMD+1),ICPRE,NMTV_MAX,IPARTR2R,ISMST,TAG_INVOL,
     .   JALE_FROM_MAT,JALE_FROM_PROP
      INTEGER ID,MFT,ILOC,JJ
      CHARACTER*nchartitle, TITR
      LOGICAL lFOUND
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
C phase 2 : bornage en groupe de mvsiz
C ngroup est global, iparg est global mais organise en fonction de dd
C
      DO N=1,ND
        NEL = EADD(N+1)-EADD(N)
C
        DO I = 1, NEL
          INDEX(I) = I
          INUM(1,I)=IPARTQ(NFT+I)
          INUM(2,I)=IXQ(1,NFT+I)
          INUM(3,I)=IXQ(2,NFT+I)
          INUM(4,I)=IXQ(3,NFT+I)
          INUM(5,I)=IXQ(4,NFT+I)
          INUM(6,I)=IXQ(5,NFT+I)
          INUM(7,I)=IXQ(6,NFT+I)
          INUM(8,I)=IXQ(7,NFT+I)
          INUM(9,I)=IQUAOFF(NFT+I)
        ENDDO
      
        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          IPARTQ(I+NFT)=INUM(1,INDEX(I))
          IXQ(1,I+NFT)=INUM(2,INDEX(I))
          IXQ(2,I+NFT)=INUM(3,INDEX(I))
          IXQ(3,I+NFT)=INUM(4,INDEX(I))
          IXQ(4,I+NFT)=INUM(5,INDEX(I))
          IXQ(5,I+NFT)=INUM(6,INDEX(I))
          IXQ(6,I+NFT)=INUM(7,INDEX(I))
          IXQ(7,I+NFT)=INUM(8,INDEX(I))
          IQUAOFF(I+NFT)=INUM(9,INDEX(I))
          ITR1(NFT+INDEX(I)) = NFT+I
        ENDDO

C dd-iad
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
          IF(IGRSURF(I)%ELTYP(J) == 2)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SHELL
C
      DO I=1,NGRQUAD
        NN=IGRQUAD(I)%NENTITY
        DO J=1,NN
          IGRQUAD(I)%ENTITY(J) = ITR1(IGRQUAD(I)%ENTITY(J))
        ENDDO
      ENDDO
C
      INEG =  0
      DO 300 N=1,ND
      
       NFT = 0
       LB_L = LBUFEL
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
          II = EADD(N)+NFT
          MID = IXQ(1,II)
          PID = IXQ(6,II)
          IF (NSUBDOM>0) IPARTR2R = TAG_MAT(MID)
          NPT =1
          JHBE=0
          JPOR=0
          JCVT = 0
          ISORTH=0
          IPLAST=IPLA_DS-1
          ICPRE=0
          IF(PID/=0)THEN
            IF(IGEO(10,PID)==17 .OR.
     .         (N2D==1.AND.IGEO(10,PID)==22)) THEN
              NPT = IGEO(4,PID)
              JHBE = IGEO(10,PID)
            ENDIF
            ICPRE  = IGEO(13,PID)
            IGT    = IGEO(11,PID)
            ISTRAIN= IGEO(12,PID)
            JCVT   = IGEO(16,PID)
            ISORTH = IGEO(17,PID)
            ISMST  = IGEO(5,PID)
            IF (IGT /= 15) IPLAST = IGEO(9,PID)
            IF(IGT==15) JPOR=2*NINT(GEO(28,PID))
          ENDIF
          MLN = NINT(PM(19,ABS(MID)))
          IF(MID<0)THEN
            IF(MLN==6.AND.JPOR/=2)MLN=17
            IF(MLN==46)MLN=47
            MID=ABS(MID)
            IXQ(1,II)=MID
            INEG = 1
          ENDIF
          JALE_FROM_MAT = NINT(PM(72,MID))
          JALE_FROM_PROP = IGEO(62,PID)
          JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if inconsistent, error message was displayed in PART reader
          JLAG=0
          IF(JALE==0.AND.MLN/=18)JLAG=1
          JEUL=0
          IF(JALE==2)THEN
            JALE=0
            JEUL=1
          ENDIF
          JTUR=NINT(PM(70,MID))
          JTHE=NINT(PM(71,MID))
                    
          JMULT=0
          IF(MLN==20)THEN
            JMULT=NINT(PM(20,MID))
            MT1=NINT(PM(21,MID))
            MT2=NINT(PM(22,MID))
            ML1=NINT(PM(19,MT1))
            ML2=NINT(PM(19,MT2))
          ELSE
            JMULT=0
            ML1=0
            ML2=0
          ENDIF
C--------------------
C-   ICPRE,ISMSTR JCVT Automatic           
C--------------------
          IF (IGT == 14.OR.IGT == 6) THEN
            IF (ICPRE < 0) ICPRE =0
            IF (ISMST < 0) ISMST =4
            IF (JCVT<0) THEN
             JCVT = 0 
             IF (JLAG>0) JCVT = 1
            END IF
          END IF
C--------------------
C TEST COMPATIBILITE
C--------------------

          ID=IGEO(1,PID)
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,PID),LTITR)
          IF (ISMST /= 2 .AND. ISMST /= 4) THEN
            CALL ANCMSG(MSGID=1223,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  PRMOD=MSG_CUMU)
          ENDIF
          IF (MLN==68 ) THEN
            CALL ANCMSG(MSGID=1224,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
          ENDIF
          IF (JHBE==17.AND.(JALE+JEUL /= 0)) THEN   
            CALL ANCMSG(MSGID=1222,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  PRMOD=MSG_CUMU)
            JHBE = 2
            IF (GEO(13,PID) == ZERO) GEO(13,PID) = EM01
            NPT = 1
            IGEO(4,PID) = NPT
            IGEO(10,PID) = JHBE
          END IF
C
          IF(JCVT/=0.AND.(JLAG==0.OR.MLN==20))THEN
            CALL FRETITL2(TITR,
     .                    IGEO(NPROPGI-LTITR+1,PID),LTITR)
            CALL ANCMSG(MSGID=610,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IXQ(7,II))
            JCVT=0
          END IF
          ISRAT=IPM(3,MID)
          IFAIL = 0
          NFAIL = IPM(220,MID)
          ISTRAIN = 1
          IEOS=IPM(4,MID)

C
C   - initial volume franction -
C
          lFOUND=.FALSE.
          TAG_INVOL=0
          IF(NINIVOL > 0)THEN
          ! Warning : In same group you can have different PArts, A loop over elem in groups has to be introduced to check if INIVOL PART is there.
            MFT = EADD(N)-1 + NFT
            NE1=MIN( NVSIZ, NEL + NEL_PREC - NFT)
            DO ILOC = 1 ,NE1
              DO JJ=1,NINIVOL
                IF(INIVOL(JJ)%IPARTFILL == IPARTQ(ILOC+MFT)) THEN
                  TAG_INVOL = 1
                  lFOUND=.TRUE.
                  EXIT
                ENDIF
                IF(lFOUND)EXIT
              ENDDO
            END DO
          END IF
                      
C-------------------------------------------------
C STOCKAGE IPARG
C-------------------------------------------------
          CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))
C
          IPARG(1,NGROUP) = MLN
          NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT)
          IPARG(2,NGROUP) = NE1
          IPARG(3,NGROUP)=  EADD(N)-1 + NFT
          IPARG(4,NGROUP) = 1   ! obsolete
c          IPARG(4,NGROUP) = LBUFEL+1
          IPARG(5,NGROUP) = 2
          IPARG(6,NGROUP) = NPT
          IPARG(7,NGROUP) = JALE
          IPARG(11,NGROUP)= JEUL
          IPARG(12,NGROUP)= JTUR
          IF(JALE==0 .AND. JEUL==0)THEN
            IPARG(13,NGROUP)=-ABS(JTHE)  ! -1 nodal temperature    +1 centroid temperature
          ELSE
            IPARG(13,NGROUP)=+ABS(JTHE)  ! -1 nodal temperature    +1 centroid temperature
          ENDIF
          IPARG(14,NGROUP)= JLAG
          IPARG(18,NGROUP)= MID
          IPARG(20,NGROUP)= JMULT
          ! Multifluid law, setting NLAY
          IF (MLN == 151) IPARG(20, NGROUP) = IPM(20, MID)
          IPARG(10,NGROUP)= ICPRE
          IPARG(23,NGROUP)= JHBE
          IPARG(24,NGROUP)= 0
          IPARG(25,NGROUP)= ML1
          IPARG(26,NGROUP)= ML2
          IPARG(27,NGROUP)= JPOR
          IPARG(29,NGROUP)= IPLAST
C reperage groupe/processeur
          IPARG(32,NGROUP)= P-1
C  attention en toute rigeur >=46
          IPARG(34,NGROUP)= NINT(PM(10,MID))
          IPARG(37,NGROUP)= JCVT
          IPARG(38,NGROUP)= IGT
          IPARG(40,NGROUP)= ISRAT
          IPARG(42,NGROUP)= ISORTH
          IPARG(43,NGROUP)= IFAIL
          IPARG(44,NGROUP)= ISTRAIN
          !inivol
          IPARG(53,NGROUP) = TAG_INVOL
C         equation of state
          IPARG(55,NGROUP)= IEOS
          IPARG(62,NGROUP)= PID
C         flag for group of duplicated elements in multidomains
          IF (NSUBDOM>0) IPARG(77,NGROUP)= IPARTR2R 
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
C                                      
      NSPGROUP = NSPGROUP + ND
C Traitement CFD sur MID negatif sur tous les solides si ineg=1
      IF (INEG==1) THEN
        DO I = 1, NUMELQ
          IXQ(1,I) = ABS(IXQ(1,I))
        ENDDO
      ENDIF
C
      WRITE(IOUT,1000)
      WRITE(IOUT,1001)(N,IPARG(1,N),IPARG(2,N),IPARG(3,N)+1,
     +                IPARG(4,N),IPARG(6,N),IPARG(7,N),IPARG(11,N),    
     +                IPARG(12,N),IPARG(13,N),IPARG(23,N),
     +                IPARG(24,N),IPARG(18,N),IPARG(27,N),
     +                IPARG(29,N)+1,IPARG(43,N),IPARG(55,N),
     +                N=NGR1,NGROUP)
c      WRITE(IOUT,1002) LBUFEL
       CALL ANCMSG(MSGID=1222,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=1223,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  PRMOD=MSG_PRINT)
C
 1000 FORMAT(//,7X,'4-NODE 2D SOLID ELEMENT GROUPS'/
     +          7X,'---------------------'//
     +'     GROUP      MAT.     ELEM.     FIRST    BUFFER     GAUSS',
     +'    A.L.E.     EULER    TURBU.    THERM.     HOUR-     INTEG',
     +'       VAR    POROUS   PLASTI.   FAILURE      IEOS   '/
     +'         #       LAW    NUMBER     ELEM.   ADDRESS    POINTS',
     +'      FLAG      FLAG      FLAG      FLAG     GLASS      FLAG',
     +'       MID    MEDIUM      FLAG      FLAG      TYPE   '/)
 1001 FORMAT(17(I10))
 1002 FORMAT(' BUFFER LENGTH : ',I10 )
C
      RETURN
      END
