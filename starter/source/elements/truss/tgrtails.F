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
Chd|  TGRTAILS                      source/elements/truss/tgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE TGRTAILS(
     1    IXT     ,IPARG  ,PM      ,GEO       ,
     2    EADD    ,ND     ,DD_IAD  ,IDX       ,LB_MAX  ,
     3    INUM    ,INDEX  ,CEP     ,IPARTT    ,NELT    ,
     4    ITR1    ,IGRSURF,IGRTRUSS,ITRUOFF   ,
     5    TAGPRT_SMS,NOD2EL1D)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE R2R_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXT(5,NUMELT)      TABLEAU CONECS+PID+MID+NOS TRUSSES         E
C     IPARG(NPARG,NGROUP)TABLEAU DES CARACTERISTIQUES DES GROUPES   E/S
C     GEO(NPROPG,NUMGEO) TABLEAU DES CARACS DES PID                 E
C     EADD(NUMELT)       TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER E 
C     DD_IAD             TABLEAU DE LA DD EN SUPER GROUPES          S
C     INDEX(NUMELT)      TABLEAU DE TRAVAIL                         E/S
C     INUM(NUMELT)       TABLEAU DE TRAVAIL                         E/S
C     CEP(NUMELT)        TABLEAU DE TRAVAIL                         E/S
C     IPARTT(NUMELT)        TABLEAU DE PART                         E/S
C     ITR1(NUMELT)       TABLEAU DE TRAVAIL                         E/S
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
      INTEGER IDX,LB_MAX, ND,NELT(*),ITR1(*),
     .        IXT(5,*), IPARG(NPARG,*),EADD(*),IPARTT(*),
     .        DD_IAD(NSPMD+1,*), INUM(7,*), INDEX(*), CEP(*),
     .        ITRUOFF(*),
     .        TAGPRT_SMS(*),NOD2EL1D(*)
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRTRUS)  :: IGRTRUSS
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGR1, NG, ISSN, MLN, I, NE1, N, NFIX,
     .        MID, PID, NEL_PREC, LB_L, II, P, NEL,NB,
     .        MODE, WORK(70000),NN,NGROU, J, K,
     .        ITAG(2*NUMELT+2*NUMELP+3*NUMELR),
     .        NGP(NSPMD+1),IPARTR2R
      DATA NFIX/5/
C=======================================================================
C
      NGR1 = NGROUP + 1
C
C phase 1 : decompostition canonique
C
      IDX = IDX+ND*(NSPMD+1)
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
          INUM(1,I)=IPARTT(NFT+I)
          INUM(2,I)=ITRUOFF(NFT+I)
          INUM(3,I)=IXT(1,NFT+I)
          INUM(4,I)=IXT(2,NFT+I)
          INUM(5,I)=IXT(3,NFT+I)
          INUM(6,I)=IXT(4,NFT+I)
          INUM(7,I)=IXT(5,NFT+I)
        ENDDO
        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          IPARTT(I+NFT)=INUM(1,INDEX(I))
          ITRUOFF(NFT+I) = INUM(2,INDEX(I))
          IXT(1,I+NFT)=INUM(3,INDEX(I))
          IXT(2,I+NFT)=INUM(4,INDEX(I))
          IXT(3,I+NFT)=INUM(5,INDEX(I))
          IXT(4,I+NFT)=INUM(6,INDEX(I))
          IXT(5,I+NFT)=INUM(7,INDEX(I))
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
          IF(IGRSURF(I)%ELTYP(J) == 4)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SHELL
C
      DO I=1,NGRTRUS
        NN=IGRTRUSS(I)%NENTITY
        DO J=1,NN
          IGRTRUSS(I)%ENTITY(J) = ITR1(IGRTRUSS(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C renumerotation CONNECTIVITE INVERSE
C
      ITAG = 0
      DO I=1,2*NUMELT+2*NUMELP+3*NUMELR
        IF(NOD2EL1D(I) /= 0 .AND. NOD2EL1D(I) <= NUMELT)THEN
          IF(ITAG(NOD2EL1D(I)) == 0) THEN
            NOD2EL1D(I)=ITR1(NOD2EL1D(I))
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
         DO 220 I=1,NG
C ngroup global
          NGROUP=NGROUP+1
          II = EADD(N)+NFT
          MID= IXT(1,II)
          MLN= PM(19,MID)
          PID= IXT(4,II)
          IF (NSUBDOM>0) IPARTR2R = TAG_MAT(MID)
          ISSN=0
          IF(GEO(5,PID)/=0.)ISSN=1
C
          CALL ZEROIN(1,NPARG,IPARG(1,NGROUP))
C
          NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT)
          IPARG(1,NGROUP) = MLN
          IPARG(2,NGROUP) = NE1
          IPARG(3,NGROUP) = EADD(N)-1 + NFT
          IPARG(4,NGROUP) = LBUFEL+1  !  kept in place for compatibility with
c                                        other groups using old buffer
          IPARG(5,NGROUP) = 4
          IPARG(9,NGROUP) = ISSN
C reperage groupe/processeur
          IPARG(32,NGROUP)= P-1
C         flag for group of duplicated elements in multidomains
          IF (NSUBDOM>0) IPARG(77,NGROUP)= IPARTR2R
C
          JSMS=0
          IF(ISMS/=0)THEN
            IF(IDTGRS/=0)THEN
              IF(TAGPRT_SMS(IPARTT(II))/=0)JSMS=1
            ELSE
              JSMS=1
            END IF
          END IF
          IPARG(52,NGROUP)=JSMS
C
cc          LBUFEL= LBUFEL + NE1 * (NFIX+NMTVT(MLN))  ! Cmi --> a virer
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
      WRITE(IOUT,1001)(N,IPARG(1,N),IPARG(2,N),IPARG(3,N)+1,IPARG(5,N),
     +                 N=NGR1,NGROUP)
C
 1000 FORMAT(/
     +       /6X,'3D - TRUSS ELEMENT GROUPS'/
     +        6X,'-------------------------'/
     +'      GROUP   MATERIAL    ELEMENT      FIRST    ELEMENT'/
     +'                   LAW     NUMBER    ELEMENT       TYPE'/)
 1001 FORMAT(5(1X,I10))
C

      RETURN
      END
