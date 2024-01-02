Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  RGRHEAD                       source/elements/spring/rgrhead.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE RGRHEAD(
     1       IXR     ,GEO    ,INUM   ,ISEL     ,IGEO   ,
     2       ITR1    ,EADD   ,INDEX  ,ITRI     ,
     3       IPARTR  ,ND     ,IGRSURF,IGRSPRING,
     4       CEP     ,XEP    ,IRESOFF,TAGPRT_SMS, CLUSTERS,
     5       IPM     ,R_SKEW)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE CLUSTER_MOD
      USE SEATBELT_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXR(6,NUMELR)     TABLEAU CONECS+PID+NOS RESSORTS            E
C     GEO(NPROPG,NUMGEO)TABLEAU DES CARACS DES PID                 E
C     INUM(6,NUMELR)    TABLEAU DE TRAVAIL                         E/S
C     ISEL(NSELR)       TABLEAU DES RESSORTS CHOISIS POUR TH       E/S
C     ITR1(NSELR)       TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELR)      TABLEAU DES ADRESSES DANS IDAM CHGT DAMIER   S
C     INDEX(NUMELR)     TABLEAU DE TRAVAIL                         E/S
C     ITRI(4,NUMELR)    TABLEAU DE TRAVAIL                         E/S
C     IPARTR(NUMELR)    TABLEAU PART                               E/S
C     CEP(NUMELR)    TABLEAU PROC                               E/S
C     XEP(NUMELR)    TABLEAU PROC                               E/S
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXR(NIXR,*),ISEL(*),INUM(9,*),IPARTR(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(4,*),
     .        ND, CEP(*), XEP(*),IRESOFF(*),R_SKEW(*),
     .        TAGPRT_SMS(*), IGEO(NPROPGI,*),IPM(NPROPMI,*)
C     REAL OU REAL*8
      my_real
     .    GEO(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSPRI)  :: IGRSPRING
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .        I, K, NG, ISSN, NPN, NN, N, MID, PID ,
     .        II, J, II3,JJ3,II2,JJ2,JJ,L,II4,JJ4,IGTYP,
     .        IFLAG_GTYP,
     .        MSKMTN,MSKISN,MSKPID, MODE, WORK(70000)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER :: CLUSTER_TYP,CLUSTER_NEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_CLUSTER

C
      DATA MSKMTN /O'07770000000'/
      DATA MSKISN /O'00000000700'/
      DATA MSKPID /O'07777777777'/
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 
C----------------------------------------------------------
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
C
      DO I=1,NUMELR
        EADD(I)=1
        ITRI(4,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTR(I)
        INUM(2,I)=IXR(1,I)
        INUM(3,I)=IXR(2,I)
        INUM(4,I)=IXR(3,I)
        INUM(5,I)=IXR(4,I)
        INUM(6,I)=IXR(5,I)
        INUM(7,I)=IXR(6,I)
        INUM(8,I)=IRESOFF(I)
        INUM(9,I)=R_SKEW(I)
      ENDDO

      IF (IMACH==3) THEN
        DO I=1,NUMELR
          XEP(I)=CEP(I)
        ENDDO
      ENDIF
C
C
      
      DO I = 1, NUMELR
        II = I
        PID= IXR(1,II)
        MTN= NINT(GEO(8,PID))
        ISSN = 0
        IF(GEO(5,PID)/=ZERO) ISSN=1
        IGTYP = IGEO(11,PID)
        IFLAG_GTYP = 0
        IF(IGTYP == 23)THEN
          IFLAG_GTYP = 1
          MID = IXR(5,II)  
          MTN = IPM(2,MID)      
        ENDIF  
          
!!        MID = 0
!!        IF(IGTYP == 23) MID = IXR(5,II)
C
        JSMS = 0
        IF(ISMS/=0)THEN
          IF(IDTGRS/=0)THEN
            IF(TAGPRT_SMS(IPARTR(II))/=0)JSMS=1
          ELSE
            JSMS=1
          END IF
        END IF
C       JSMS=MY_SHIFTL(JSMS,0)
C       NEXT=MY_SHIFTL(NEXT,1)
        ISSN=MY_SHIFTL(ISSN,6)
        MTN=MY_SHIFTL(MTN,21)
C
        ITRI(1,I)=JSMS + ISSN + MTN
 
        ITRI(2,I)=PID 
C
        ITRI(3,I)=IRESOFF(I)   
C        
        ITRI(4,I)=IFLAG_GTYP
      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELR , 4)
C
      DO I=1,NUMELR
        IPARTR(I) =INUM(1,INDEX(I))
        IRESOFF(I)=INUM(8,INDEX(I))
        R_SKEW(I)=INUM(9,INDEX(I))
      ENDDO
C
      IF (IMACH==3) THEN
        DO I=1,NUMELR
          CEP(I)=XEP(INDEX(I))
        ENDDO
      ENDIF
      DO  K=1,6
        DO  I=1,NUMELR
          IXR(K,I)=INUM(K+1,INDEX(I))
        ENDDO
      ENDDO
C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELR
        ITR1(INDEX(I))=I
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
C RENUMEROTATION POUR GROUPES DE RESSORTS
C
      DO I=1,NGRSPRI
        NN=IGRSPRING(I)%NENTITY
        DO J=1,NN
          IGRSPRING(I)%ENTITY(J) = ITR1(IGRSPRING(I)%ENTITY(J))
        ENDDO
      ENDDO

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

C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELR
        II=ITRI(1,INDEX(I))
        JJ=ITRI(1,INDEX(I-1))

        II2=ITRI(2,INDEX(I))
        JJ2=ITRI(2,INDEX(I-1))

        II3=ITRI(3,INDEX(I))
        JJ3=ITRI(3,INDEX(I-1))
C        
        II4=ITRI(4,INDEX(I))
        JJ4=ITRI(4,INDEX(I-1))

        MTN = NINT(GEO(8,II2))       
        IF(II/=JJ.OR.(II2/=JJ2 .AND.MTN==5).OR.(II3/=JJ3) .OR.
     .                                                II4/=JJ4) THEN
          ND=ND+1
          EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELR+1
C
      RETURN
      END
