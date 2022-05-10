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
C
Chd|====================================================================
Chd|  RGRTAILS                      source/elements/spring/rgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE RGRTAILS(
     1    IXR     ,IPARG    ,GEO     ,EADD     ,IGEO    ,
     2    ND      ,DD_IAD   ,IDX     ,LB_MAX   ,INUM    ,
     3    INDEX   ,CEP      ,IPARTR  ,NELR     ,ITR1    ,
     4    IGRSURF ,IGRSPRING,IRESOFF ,TAGPRT_SMS,NOD2EL1D,
     5    IPM     ,CLUSTERS ,R_SKEW)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE R2R_MOD
      USE CLUSTER_MOD
      USE SEATBELT_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXR(6,NUMELR)      TABLEAU CONECS+PID+NOS RESSORTS            E
C     IPARG(NPARG,NGROUP)TABLEAU DES CARACTERISTIQUES DES GROUPES   E/S
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     EADD(NUMELR)       TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER E 
C     DD_IAD             TABLEAU DE LA DD EN SUPER GROUPES          S
C     INDEX(NUMELR)      TABLEAU DE TRAVAIL                         E/S
C     INUM (6*NUMELR)      TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELR)        TABLEAU DE TRAVAIL                         E/S
C     ITR1(NUMELR)        TABLEAU DE TRAVAIL                         E/S
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
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDX,LB_MAX, ND,NELR(*),ITR1(*), IGEO(NPROPGI,*),
     .        IXR(NIXR,*), IPARG(NPARG,*),EADD(*),IPARTR(*),
     .        DD_IAD(NSPMD+1,*),INUM(9,*),INDEX(*),CEP(*),
     .        IRESOFF(*),TAGPRT_SMS(*),NOD2EL1D(*),IPM(NPROPMI,*),R_SKEW(*)
      my_real
     .   GEO(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSPRI)  :: IGRSPRING
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C      INTEGER NMTV(4), NGR1, NG, ISSN, MTNN, I, NE1, N, NFIX,
      INTEGER NGR1, NG, ISSN, MTNN, I, NE1, N, NFIX,
     .        PID, NEL_PREC, LB_L, II, P, NEL, IGTYP,NB,
     .        MODE, WORK(70000),NN,J,MID,
     .        ITAG(2*NUMELT+2*NUMELP+3*NUMELR),
     .        NGP(NSPMD+1),IPARTR2R
      INTEGER :: CLUSTER_TYP,CLUSTER_NEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_CLUSTER

C=======================================================================

      NGR1 = NGROUP + 1
C
C phase 1 : decompostition canonique
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
        DO I = 1, NEL
          INDEX(I) = I
          INUM(1,I)=IPARTR(NFT+I)
          INUM(2,I)=IXR(1,NFT+I)
          INUM(3,I)=IXR(2,NFT+I)
          INUM(4,I)=IXR(3,NFT+I)
          INUM(5,I)=IXR(4,NFT+I)
          INUM(6,I)=IXR(5,NFT+I)
          INUM(7,I)=IXR(6,NFT+I)
          INUM(8,I)=IRESOFF(NFT+I)
          INUM(9,I)=R_SKEW(NFT+I)
        ENDDO
      
        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          IPARTR(I+NFT)=INUM(1,INDEX(I))
          IXR(1,I+NFT)=INUM(2,INDEX(I))
          IXR(2,I+NFT)=INUM(3,INDEX(I))
          IXR(3,I+NFT)=INUM(4,INDEX(I))
          IXR(4,I+NFT)=INUM(5,INDEX(I))
          IXR(5,I+NFT)=INUM(6,INDEX(I))
          IXR(6,I+NFT)=INUM(7,INDEX(I))
          IRESOFF(NFT+I)=INUM(8,INDEX(I))
          R_SKEW(NFT+I)=INUM(9,INDEX(I))
          ITR1(NFT+INDEX(I)) = NFT+I
        ENDDO
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
      ENDDO          
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP(J) == 6)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SHELL
C
      DO I=1,NGRSPRI
        NN=IGRSPRING(I)%NENTITY
        DO J=1,NN
          IGRSPRING(I)%ENTITY(J) = ITR1(IGRSPRING(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C renumerotation CONNECTIVITE INVERSE
C
      ITAG = 0
      DO I=1,2*NUMELT+2*NUMELP+3*NUMELR   
        IF(NOD2EL1D(I) /= 0 .AND. NOD2EL1D(I)   > NUMELT+NUMELP)THEN
          IF(ITAG(NOD2EL1D(I)) == 0) THEN
            NOD2EL1D(I)=ITR1(NOD2EL1D(I)-NUMELT-NUMELP)
            NOD2EL1D(I)=NOD2EL1D(I)+NUMELT+NUMELP
            ITAG(NOD2EL1D(I)) = 1
          END IF
	END IF
      END DO   

!   -----------------------
!   reordering for cluster typ=2 or 3 (spring cluster)
      DO I=1,NCLUSTER
        CLUSTER_TYP = CLUSTERS(I)%TYPE
        IF(CLUSTER_TYP==2.OR.CLUSTER_TYP==3) THEN
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
C REORDERING FOR SEATBELTS
C
      DO I=1,N_SEATBELT
        NN=SEATBELT_TAB(I)%NSPRING
        DO J=1,NN
          SEATBELT_TAB(I)%SPRING(J) = ITR1(SEATBELT_TAB(I)%SPRING(J))
        ENDDO
      ENDDO
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
        IF (NEL.GT.0) THEN
         NEL_PREC = DD_IAD(P,NSPGROUP+N)-DD_IAD(1,NSPGROUP+N)
         NGP(P)=NGROUP
         NG  = (NEL-1)/NVSIZ + 1
         DO 220 I=1,NG
C ngroup global
          NGROUP=NGROUP+1
          II = EADD(N)+NFT
          PID= IXR(1,II)
C Multidomains - spring not duplicated
          IF (NSUBDOM.GT.0) IPARTR2R = 1
          MTNN = NINT(GEO(8,PID))
          IGTYP= IGEO(11,PID)
          ISSN=0
          IF(GEO(5,PID).NE.0.)ISSN=1
          IF(IGTYP == 23) THEN
             MID  = IXR(5,II)
             MTNN = IPM(2,MID)
          ENDIF
C
          CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))
C
          NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT)
          IPARG(1,NGROUP) = MTNN
          IPARG(2,NGROUP) = NE1
          IPARG(3,NGROUP) = EADD(N)-1 + NFT
          IPARG(4,NGROUP) = LBUFEL+1  !  kept in place for compatibility with
c                                        other groups using old buffer
          IPARG(5,NGROUP) = 6
          IPARG(9,NGROUP) = ISSN
C reperage groupe/processeur
          IPARG(32,NGROUP)= P-1
          IPARG(38,NGROUP)= IGTYP
C         flag for group of dupplicated elements in multidomains
          IF (NSUBDOM.GT.0) IPARG(77,NGROUP)= IPARTR2R
C
          JSMS=0
          IF(ISMS/=0)THEN
            IF(IDTGRS/=0)THEN
              IF(TAGPRT_SMS(IPARTR(II))/=0)JSMS=1
            ELSE
              JSMS=1
            END IF
          END IF
          IPARG(52,NGROUP)=JSMS
C
C         LBUFEL= LBUFEL + NE1 * NMTV(MTNN)
cc          LBUFEL= LBUFEL + NE1 * NPTVC(IGTYP)  ! Cmi --> a virer
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
      WRITE(IOUT,1001)(N,IGTYP,IPARG(2,N),IPARG(3,N)+1,IPARG(5,N),
     +                 N=NGR1,NGROUP)
C
 1000 FORMAT(/
     +       /6X,'3D - SPRING ELEMENT GROUPS'/
     +        6X,'-------------------------'/
     +'      GROUP     SPRING    ELEMENT      FIRST    ELEMENT'/
     +'                  TYPE     NUMBER    ELEMENT       TYPE'/)
 1001 FORMAT(5(1X,I10))
C
      RETURN
      END
