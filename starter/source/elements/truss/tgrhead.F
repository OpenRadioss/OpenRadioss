Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    tgrhead        ../starter/source/elements/truss/tgrhead.F
!||--- called by ------------------------------------------------------
!||    lectur         ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE TGRHEAD(
     1       IXT ,   PM  , GEO  ,   INUM    ,  ISEL,
     2       ITR1,   EADD, INDEX,   ITRI    ,      
     3       IPARTT, ND,   IGRSURF, IGRTRUSS,
     4       CEP,    XEP,  ITRUOFF ,
     5       TAGPRT_SMS,ITAGPRLD_TRUSS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXT(5,NUMELT)     TABLEAU CONECS+MID+PID+NOS TRUSSES         E
C     GEO(NPROPG,NUMGEO)TABLEAU DES CARACS DES PID                 E
C     INUM(6,NUMELT)    TABLEAU DE TRAVAIL                         E/S
C     ISEL(NSELT)       TABLEAU DESPOUTRES CHOISIS POUR TH         E/S
C     ITR1(NSELT)       TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELT)      TABLEAU DES ADRESSES DANS IDAM CHGT DAMIER   S
C     INDEX(NUMELT)     TABLEAU DE TRAVAIL                         E/S
C     ITRI(5,NUMELT)    TABLEAU DE TRAVAIL                         E/S
C     IPARTT(NUMELT)    TABLEAU PART                               E/S
C     CEP(NUMELT)    TABLEAU PROC                               E/S
C     XEP(NUMELT)    TABLEAU PROC                               E/S
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
#include      "sms_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXT(5,*),ISEL(*),INUM(7,*),IPARTT(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(5,*),
     .        ND, CEP(*), XEP(*),
     .        ITRUOFF(*), TAGPRT_SMS(*)
      INTEGER ,INTENT(INOUT), DIMENSION(NUMELT) ::ITAGPRLD_TRUSS
      my_real :: PM(NPROPM,*), GEO(NPROPG,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRTRUS)  :: IGRTRUSS
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER :: I,J,K,L,NG, ISSN, NN,N,MLN,MID,PID ,NGROU
      INTEGER :: II,JJ,II2,JJ2,II3,JJ3,II4,JJ4,II5,JJ5
      INTEGER :: MSKMLN,MSKISN,MSKPID, MSKMID, MODE
      INTEGER :: WORK(70000),IPRLD
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND

      DATA MSKMLN /O'07770000000'/
      DATA MSKISN /O'00000000700'/
      DATA MSKMID /O'07777777777'/
      DATA MSKPID /O'07777777777'/
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 
C----------------------------------------------------------
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
C
      DO I=1,NUMELT
        EADD(I)=1
        INDEX(I)=I
        INUM(1,I)=IPARTT(I)
        INUM(2,I)=ITRUOFF(I)
        INUM(3,I)=IXT(1,I)
        INUM(4,I)=IXT(2,I)
        INUM(5,I)=IXT(3,I)
        INUM(6,I)=IXT(4,I)
        INUM(7,I)=IXT(5,I)
      ENDDO

      DO I=1,NUMELT
        XEP(I)=CEP(I)
      ENDDO
C
C
      DO I = 1, NUMELT
        II = I
        MID= IXT(1,II)
        MLN= PM(19,MID)
        PID= IXT(4,II)
        ISSN = 0
        IF(GEO(5,PID)/=0.) ISSN=1
        IPRLD = ITAGPRLD_TRUSS(II)
        IF (IPRLD>0) MLN = MLN+IPRLD
C
        ISSN=MY_SHIFTL(ISSN,6)
        MLN=MY_SHIFTL(MLN,21)
C
        ITRI(1,I)=MLN+ISSN
        ITRI(2,I)=ITRUOFF(I)
C
        JSMS = 0
        IF(ISMS/=0)THEN
          IF(IDTGRS/=0)THEN
            IF(TAGPRT_SMS(IPARTT(II))/=0)JSMS=1
          ELSE
            JSMS=1
          END IF
        END IF
C       JSMS=MY_SHIFTL(JSMS,0)
        ITRI(3,I) = JSMS
C       NEXT=MY_SHIFTL(NEXT,1)
c
C       key4
        ITRI(4,I) = MID
C       key5
        ITRI(5,I) = PID

      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELT , 4)
C
      DO I=1,NUMELT
        IPARTT(I) =INUM(1,INDEX(I))
        ITRUOFF(I) =INUM(2,INDEX(I))
      ENDDO
      DO  K=1,5
        DO  I=1,NUMELT
          IXT(K,I)=INUM(K+2,INDEX(I))
        ENDDO
      ENDDO

      DO I=1,NUMELT
        CEP(I)=XEP(INDEX(I))
      ENDDO
C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELT
        ITR1(INDEX(I))=I
      ENDDO
C
C RENUMEROTATION POUR TH
C
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
C RENUMEROTATION POUR GROUPES DE TRUSSES
C
      DO I=1,NGRTRUS
        NN=IGRTRUSS(I)%NENTITY
        DO J=1,NN
          IGRTRUSS(I)%ENTITY(J) = ITR1(IGRTRUSS(I)%ENTITY(J))
        ENDDO
      ENDDO
C REORDERING FOR ITAGPRLD_TRUSS
      INUM(2,1:NUMELT)=ITAGPRLD_TRUSS(1:NUMELT)
      DO I=1,NUMELT
        ITAGPRLD_TRUSS(I) =INUM(2,INDEX(I))
      ENDDO
C
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELT
        II=ITRI(1,INDEX(I))
        JJ=ITRI(1,INDEX(I-1))
        II2=ITRI(2,INDEX(I))
        JJ2=ITRI(2,INDEX(I-1))
        II3=ITRI(3,INDEX(I))
        JJ3=ITRI(3,INDEX(I-1))
        II4=ITRI(4,INDEX(I))
        JJ4=ITRI(4,INDEX(I-1))
        II5=ITRI(5,INDEX(I))
        JJ5=ITRI(5,INDEX(I-1))
        
        IF (II/=JJ   .OR. II2/=JJ2 .OR. II3/=JJ3  .OR.
     .      II4/=JJ4 .OR. II5/=JJ5) THEN
          ND=ND+1
          EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELT+1
c-----------
      RETURN
      END
