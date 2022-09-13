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
Chd|  XGRTAILS                      source/elements/xelem/xgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE XGRTAILS(
     1    KXX    ,IPARG  ,GEO     ,EADD,
     2    ND     ,DD_IAD ,IDX     ,LB_MAX, INUM,
     3    INDEX  ,CEP    ,IPARTX  ,ITR1,   IGRSURF,
     4    IXX ,   IGEO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     KXX(5,NUMELX)      TABLEAU CONECS+PID+NOS RESSORTS            E
C     IPARG(NPARG,NGROUP)TABLEAU DES CARACTERISTIQUES DES GROUPES   E/S
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     EADD(NUMELX)       TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER E 
C     DD_IAD             TABLEAU DE LA DD EN SUPER GROUPES          S
C     INDEX(NUMELX)      TABLEAU DE TRAVAIL                         E/S
C     INUM (6*NUMELX)      TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELX)        TABLEAU DE TRAVAIL                         E/S
C     ITR1(NUMELX)        TABLEAU DE TRAVAIL                         E/S
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(5,*),IPARG(NPARG,*),EADD(*),
     .        ND, DD_IAD(NSPMD+1,*),IDX,IGEO(NPROPGI,*),
     .        LB_MAX, INUM(6,*), INDEX(*),CEP(*),
     .        IPARTX(*), ITR1(*)
      my_real
     .   GEO(NPROPG,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGR1, NG, ISSN, MTNN, I, NE1, N, NFIX,
     .        PID, NEL_PREC, LB_L, P, NEL, IGTYP,NB,
     .        MODE, WORK(70000),NN,NGROU, J,MID,IETYP,
     .        MT,IXX(*),NUVAR,NUVARN,NXVIE,NXVIN,INND,II,inno,
     .        NGP(NSPMD+1),IPARTR2R
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: MINDEXX2
      DATA NXVIE/3/, NXVIN/0/
C----------------------------------------------------------
      NGR1 = NGROUP + 1
C
C phase 1 : decompostition canonique
C
      IDX=IDX+ND*(NSPMD+1)
      CALL ZEROIN(1,ND*(NSPMD+1),DD_IAD(1,NSPGROUP+1))
      NFT = 0
C initialisation dd_iad
      DO N=1,ND
       DO P=1,NSPMD+1
         DD_IAD(P,NSPGROUP+N) = 0
       END DO
      ENDDO

      DO N=1,ND
        NEL = EADD(N+1)-EADD(N)
C
        DO I = 1, NEL 
          INDEX(I) = I
          INUM(1,I)=IPARTX(NFT+I)
          INUM(2,I)=KXX(1,NFT+I)
          INUM(3,I)=KXX(2,NFT+I)
          INUM(4,I)=KXX(3,NFT+I)
          INUM(5,I)=KXX(4,NFT+I)
          INUM(6,I)=KXX(5,NFT+I)
        ENDDO
      
        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          IPARTX(I+NFT)=INUM(1,INDEX(I))
          KXX(1,I+NFT)=INUM(2,INDEX(I))
          KXX(2,I+NFT)=INUM(3,INDEX(I))
          KXX(3,I+NFT)=INUM(4,INDEX(I))
          KXX(4,I+NFT)=INUM(5,INDEX(I))
          KXX(5,I+NFT)=INUM(6,INDEX(I))
          ITR1(NFT+INDEX(I)) = NFT+I
        ENDDO
C   dd-iad
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
C RENUMEROTATION POUR SURFACE voir pour 100==ITYP
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
         IF(IGRSURF(I)%ELTYP(J) == 100)
     .      IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO

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
         DO 220 I=1,NG
C xgroup global
          NGROUP=NGROUP+1
          II = EADD(N)+NFT
C Multidomains - xelem not duplicated
          IF (NSUBDOM>0) IPARTR2R = 1
          PID =  KXX(2,II)
          innd = kxx(3,II)
          MTNN=GEO(8,PID)
          IGTYP=NINT(GEO(12,PID))
 	  IF(IGTYP<28.OR.IGTYP>31) THEN
      CALL ANCMSG(MSGID=413,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO_BLIND_1,
     .                I1=KXX(5,I),
     .                C1='PROPERTY',
     .                I2=IGEO(1,PID),
     .                C2='PROPERTY',
     .                I3=IGTYP)
 	  ENDIF
          ISSN=0
          IETYP = 100
          GEO(8,PID)=IETYP + EM01
          IF(GEO(5,PID)/=ZERO)ISSN=1

C
          CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))
C
          NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT)
          NUVAR =NINT( GEO(25,PID))
          NUVARN=NINT( GEO(35,PID))

          IPARG(1,NGROUP) = MTNN
          IPARG(2,NGROUP) = NE1
          IPARG(3,NGROUP) = II-1
          IPARG(4,NGROUP) = LBUFEL+1  !  kept in place for compatibility with
c                                        other groups using old buffer
          IPARG(5,NGROUP) = IETYP
          IPARG(9,NGROUP) = ISSN
C         flag for group of duplicated elements in multidomains
          IF (NSUBDOM>0) IPARG(77,NGROUP)= IPARTR2R
C
cc          LBUFEL= IPARG(4,NGROUP)+NE1*
cc     .            (NXVIE+NUVAR+INND*(NXVIN+NUVARN))-1  ! Cmi --> a virer
C reperage groupe/processeur
          IPARG(32,NGROUP)= P-1
          NFT = NFT + NE1
  220    CONTINUE
         NGP(P)=NGROUP-NGP(P)
        ENDIF
       ENDDO
cc       LB_L = LBUFEL - LB_L
cc       LB_MAX = MAX(LB_MAX,LB_L)
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
C 
      WRITE(IOUT,1000)
      WRITE(IOUT,1001)(N,IPARG(1,N),IPARG(2,N),IPARG(3,N)+1,
     +                IPARG(4,N),IPARG(5,N),
     +              N=NGR1,NGROUP)
      WRITE(IOUT,1002) LBUFEL
C
 1000 FORMAT(10X,' 3D - MULTI-PURPOSE ELEMENT GROUPS '/
     +       10X,' ----------------------------------'/
     +' GROUP   ELEMENT    ELEMENT   FIRST    BUFFER   ELEMENT  '/
     +'         MATERIAL   NUMBER    ELEMENT  ADDRESS  TYPE     '/)
 1001 FORMAT(6(1X,I7,1X))
 1002 FORMAT(' BUFFER LENGTH : ',I10 )
C

      RETURN
      END
