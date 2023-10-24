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
Chd|  PGRTAILS                      source/elements/beam/pgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE PGRTAILS(
     1    IXP    ,IPARG  ,PM       ,GEO      ,
     2    EADD   ,ND     ,DD_IAD   ,IDX      ,
     3    INUM   ,INDEX  ,CEP      ,IPARTP   ,
     4    ITR1   ,IGRSURF,IGRBEAM  ,IGEO     ,
     5    IPM    ,IPOUOFF,TAGPRT_SMS ,
     6    NOD2EL1D,PRINT_FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE REORDER_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXP(6,NUMELP)      TABLEAU CONECS+PID+MID+NOS POUTRES         E
C     IPARG(NPARG,NGROUP)TABLEAU DES CARACTERISTIQUES DES GROUPES   E/S
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     EADD(NUMELP)       TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER E
C     DD_IAD             TABLEAU DE LA DD EN SUPER GROUPES          S
C     INUM(9,NUMELP)     TABLEAU DE TRAVAIL                         E/S
C     INDEX(NUMELP)      TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELP)        TABLEAU DE TRAVAIL                         E/S
C     IPARTP(NUMELP)        TABLEAU DE PART                         E/S
C     ITR1(NSELP)        TABLEAU DE TRAVAIL                         E/S
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDX,LB_MAX,ND
      INTEGER ITR1(*),IXP(6,*),IPARG(NPARG,*),EADD(*),IPARTP(*),
     .        DD_IAD(NSPMD+1,*),CEP(*),INUM(8,*),INDEX(*),
     .        IPM(NPROPMI,*),IPOUOFF(*),
     .        TAGPRT_SMS(*),NOD2EL1D(*),IGEO(NPROPGI,*)
      INTEGER, INTENT(IN) :: PRINT_FLAG !< flag to print the element group data
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBEAM)  :: IGRBEAM
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGR1, NG, ISSN, MLN, I, NE1, N, NFIX,
     .        MID, PID, NEL_PREC, II, P, NEL,NB,NIP,IGTYP,
     .        MODE,NN, J,
     .        ITAG(2*NUMELT+2*NUMELP+3*NUMELR),
     .        NGP(NSPMD+1),IPARTR2R,NUVAR,IE,ID1
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR,TITR1
      INTEGER WORK(70000)
      DATA NFIX/13/
C=======================================================================
      NGR1 = NGROUP + 1
C
C phase 1 : decompostition canonique

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

      DO N=1,ND
        NEL = EADD(N+1)-EADD(N)

        DO I = 1, NEL
          INDEX(I) = I
          INUM(1,I)=IPARTP(NFT+I)
          INUM(2,I)=IXP(1,NFT+I)
          INUM(3,I)=IXP(2,NFT+I)
          INUM(4,I)=IXP(3,NFT+I)
          INUM(5,I)=IXP(4,NFT+I)
          INUM(6,I)=IXP(5,NFT+I)
          INUM(7,I)=IXP(6,NFT+I)
          INUM(8,I)=IPOUOFF(NFT+I)
        ENDDO

        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          IPARTP(I+NFT)=INUM(1,INDEX(I))
          IXP(1,I+NFT)=INUM(2,INDEX(I))
          IXP(2,I+NFT)=INUM(3,INDEX(I))
          IXP(3,I+NFT)=INUM(4,INDEX(I))
          IXP(4,I+NFT)=INUM(5,INDEX(I))
          IXP(5,I+NFT)=INUM(6,INDEX(I))
          IXP(6,I+NFT)=INUM(7,INDEX(I))
          IPOUOFF(NFT+I)=INUM(8,INDEX(I))
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
          IF(IGRSURF(I)%ELTYP(J) == 5)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SHELL
C
      DO I=1,NGRBEAM
        NN=IGRBEAM(I)%NENTITY
        DO J=1,NN
          IGRBEAM(I)%ENTITY(J) = ITR1(IGRBEAM(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C renumerotation CONNECTIVITE INVERSE
C
      ITAG = 0
      DO I=1,2*NUMELT+2*NUMELP+3*NUMELR
        IF(NOD2EL1D(I) /= 0 .AND. NUMELT      <  NOD2EL1D(I)
     .                      .AND. NOD2EL1D(I) <= NUMELT+NUMELP)THEN
          IF(ITAG(NOD2EL1D(I)) == 0) THEN
            NOD2EL1D(I)=ITR1(NOD2EL1D(I)-NUMELT)
            NOD2EL1D(I)=NOD2EL1D(I)+NUMELT
            ITAG(NOD2EL1D(I)) = 1
          END IF
        END IF
      END DO
C
C-------------------------------------------------------------------------
C phase 2 : bornage en groupe de mvsiz
C ngroup est global, iparg est global mais organise en fonction de dd
C
      DO 300 N=1,ND
        NFT = 0
cc       LB_L = LBUFEL
        DO P = 1, NSPMD
          NGP(P)=0
          NEL = DD_IAD(P+1,NSPGROUP+N)-DD_IAD(P,NSPGROUP+N)
          IF (NEL>0) THEN
            NEL_PREC = DD_IAD(P,NSPGROUP+N)-DD_IAD(1,NSPGROUP+N)
            NGP(P)=NGROUP
            NG  = (NEL-1)/NVSIZ + 1
            DO I=1,NG
              !---ngroup global
              NGROUP=NGROUP+1
              II = EADD(N)+NFT
              MID= IXP(1,II)
              MLN= INT(PM(19,MID))
              PID= IXP(5,II)
              IPARTR2R = 0
              IF (NSUBDOM>0) IPARTR2R = TAG_MAT(MID)
              ISSN=0
              IF(GEO(5,PID)/=ZERO)ISSN=1
              NIP  = 1
              IGTYP = IGEO(11,PID)
              IF (IGTYP == 18) NIP = IGEO(3,PID)
              CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))

              NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT)
C---
              JTHE = NINT(PM(71,MID))

              !!
              IF(IGTYP == 3 .AND. MLN == 34 ) THEN
                ID=IPM(1,MID)
                ID1= IGEO(1,PID)
                CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,PID),LTITR)
                CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,MID),LTITR)

                CALL ANCMSG(MSGID=2050,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID1,
     .                      C1=TITR1,
     .                      I2=ID,
     .                      C2=TITR)
              ENDIF
              !!
              NUVAR  = 0
              DO J = 1,NE1
                IE=J+EADD(N)+NFT-1
                NUVAR  = MAX(NUVAR,IPM(8,IXP(1,IE)))
              END DO
              IPARG(46,NGROUP) = NUVAR
C---
              IPARG(1,NGROUP) = MLN
              IPARG(2,NGROUP) = NE1
              IPARG(3,NGROUP) = EADD(N)-1 + NFT
              IPARG(4,NGROUP) = LBUFEL+1  !  kept in place for compatibility with
c                                        other groups using old buffer
              IPARG(5,NGROUP) = 5
              IPARG(6,NGROUP) = NIP
              IPARG(9,NGROUP) = NINT(GEO(3,PID))
              IPARG(13,NGROUP) = JTHE  !beam : 0 or 1 only
              IPARG(38,NGROUP) = IGTYP
              !---reperage groupe/processeur
              IPARG(32,NGROUP)= P-1
C             flag for group of duplicated elements in multidomains
              IF (NSUBDOM>0) IPARG(77,NGROUP)= IPARTR2R
C             thermal material expansion
              IPARG(49,NGROUP) = 0
              IF(IPM(218,MID) > 0 .AND. MLN /= 0 .AND. MLN /=13) THEN
                IPARG(49,NGROUP) = 1
              ENDIF
              ! check failure models
              IF (IPM(220,MID) >  0) THEN                                
                IPARG(43,NGROUP) = 1   ! IFAIL flag                      
              ENDIF

              JSMS=0
              IF(ISMS/=0)THEN
                IF(IDTGRS/=0)THEN
                  IF(TAGPRT_SMS(IPARTP(II))/=0)JSMS=1
                ELSE
                  JSMS=1
                END IF
              END IF
              IPARG(52,NGROUP)=JSMS
c
              NFT = NFT + NE1
            END DO !I=1,NG
            NGP(P)=NGROUP-NGP(P)
          ENDIF
        ENDDO
        !--- DD_IAD => nb groupes par sous domaine
        NGP(NSPMD+1)=0
        DO P = 1, NSPMD
          NGP(NSPMD+1)=NGP(NSPMD+1)+NGP(P)
          DD_IAD(P,NSPGROUP+N)=NGP(P)
        END DO
        DD_IAD(NSPMD+1,NSPGROUP+N)=NGP(NSPMD+1)

  300 CONTINUE

      NSPGROUP = NSPGROUP + ND

      IF(PRINT_FLAG>6) THEN
          WRITE(IOUT,1000)
          WRITE(IOUT,1001)(N,IPARG(1,N),IPARG(2,N),IPARG(3,N)+1,
     +                 IPARG(5,N),
     +                 N=NGR1,NGROUP)
      ENDIF
 1000 FORMAT(/
     +       /6X,'3D - BEAM ELEMENT GROUPS'/
     +        6X,'-------------------------'/
     +'      GROUP   MATERIAL    ELEMENT      FIRST    ELEMENT'/
     +'                   LAW     NUMBER    ELEMENT       TYPE'/)
 1001 FORMAT(5(1X,I10))


      RETURN
      END
