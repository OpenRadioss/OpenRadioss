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
!||    pgrhead        ../starter/source/elements/beam/pgrhead.F
!||--- called by ------------------------------------------------------
!||    lectur         ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE PGRHEAD(
     1       IXP    ,PM     ,GEO     ,INUM    ,
     2       ITR1   ,EADD   ,INDEX   ,ITRI    ,IPARTP  ,      
     3       ND     ,IGRSURF,IGRBEAM ,CEP     ,
     4       XEP    ,IGEO   ,IPOUOFF ,TAGPRT_SMS ,IPM  ,
     5       ITAGPRLD_BEAM,IBEAM_VECTOR,RBEAM_VECTOR,XNUM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXP(6,NUMELP)     TABLEAU CONECS+MID+PID+NOS POUTRES         E
C     GEO(NPROPG,NUMGEO)TABLEAU DES CARACS DES PID                 E
C     INUM(7,NUMELP)    TABLEAU DE TRAVAIL                         E/S
C     ITR1(NSELP)       TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELP)      TABLEAU DES ADRESSES DANS IDAM CHGT DAMIER   S
C     INDEX(NUMELP)     TABLEAU DE TRAVAIL                         E/S
C     ITRI(5,NUMELP)    TABLEAU DE TRAVAIL                         E/S
C     IPARTP(NUMELP)    TABLEAU PART                               E/S
C     CEP(NUMELP)    TABLEAU PROC                               E/S
C     XEP(NUMELP)    TABLEAU PROC                               E/S
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
      INTEGER IXP(6,*),INUM(9,*),IPARTP(*),IGEO(NPROPGI,NUMGEO),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(5,NUMELP),
     .        ND, CEP(*), XEP(*),IPOUOFF(*),
     .        TAGPRT_SMS(*),IPM(NPROPMI,NUMMAT)
      INTEGER ,INTENT(INOUT), DIMENSION(NUMELP) ::ITAGPRLD_BEAM
      INTEGER ,INTENT(INOUT) :: IBEAM_VECTOR(NUMELP)
      my_real PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO)
      my_real ,INTENT(INOUT) :: RBEAM_VECTOR(3,NUMELP)
      my_real ,INTENT(INOUT) :: XNUM(3,NUMELP)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBEAM)  :: IGRBEAM
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER :: I, K,  MLN, ISSN, NN, MID, PID,IGTYP
      INTEGER :: II, J, II2,JJ2,JJ,II3,JJ3,II4,JJ4,II5,JJ5,NPG,MODE
      INTEGER ::  WORK(70000)
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND,IPRLD
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
C
      DO I=1,NUMELP
        EADD(I)=1
        INDEX(I)=I
        INUM(1,I)=IPARTP(I)
        INUM(2,I)=IXP(1,I)
        INUM(3,I)=IXP(2,I)
        INUM(4,I)=IXP(3,I)
        INUM(5,I)=IXP(4,I)
        INUM(6,I)=IXP(5,I)
        INUM(7,I)=IXP(6,I)
        INUM(8,I)=IPOUOFF(I)
        INUM(9,I)=IBEAM_VECTOR(I)  
        XNUM(1:3,I)=RBEAM_VECTOR(1:3,I)             
      ENDDO

      DO I=1,NUMELP
        XEP(I)=CEP(I)
      ENDDO
C
C
      DO I = 1, NUMELP
        II = I
        MID= IXP(1,II)
        MLN= INT(PM(19,MID))
        PID= IXP(5,II)
        ISSN = 0
        IF(GEO(5,PID)/=ZERO) ISSN=1
        IPRLD = ITAGPRLD_BEAM(II)
        NPG   = IGEO(3,PID)
        IGTYP = IGEO(11,PID) + IPRLD
        JTHE = NINT(PM(71,MID))
        IEXPAN = IPM(218,MID)
c
        IGTYP = MY_SHIFTL(IGTYP,12)
        ISSN  = MY_SHIFTL(ISSN,6)
        JTHE  = MY_SHIFTL(JTHE,9)
        IEXPAN  = MY_SHIFTL(IEXPAN,12)
        MLN   = MY_SHIFTL(MLN,21)
C
        ITRI(1,I) = MLN + IGTYP + ISSN + JTHE + IEXPAN
        ITRI(2,I) = IPOUOFF(I)
C
        JSMS = 0
        IF(ISMS/=0)THEN
          IF(IDTGRS/=0)THEN
            IF(TAGPRT_SMS(IPARTP(II))/=0)JSMS=1
          ELSE
            JSMS=1
          END IF
        END IF
C       JSMS = MY_SHIFTL(JSMS,0)
        NPG  = MY_SHIFTL(NPG,12)
        ITRI(3,I) = JSMS+NPG
C       NEXT = MY_SHIFTL(NEXT,1)
     
C---     Key 4 
C       MID = MY_SHIFTL(MID,0)
        ITRI(4,I) = MID
C---     Key 5 
C       PID = MY_SHIFTL(PID,0)
        ITRI(5,I) = PID

      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELP , 5)
C
      DO I=1,NUMELP
        IPARTP(I) = INUM(1,INDEX(I))
        IPOUOFF(I)= INUM(8,INDEX(I))
        IBEAM_VECTOR(I)= INUM(9,INDEX(I))
        RBEAM_VECTOR(1:3,I)= XNUM(1:3,INDEX(I))
      ENDDO
      DO I=1,NUMELP
        CEP(I)=XEP(INDEX(I))
      ENDDO
      DO  K=1,6
        DO  I=1,NUMELP
          IXP(K,I)=INUM(K+1,INDEX(I))
        ENDDO
      ENDDO
C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELP
        ITR1(INDEX(I))=I
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
C RENUMEROTATION POUR GROUPES DE POUTRES
C
      DO I=1,NGRBEAM
        NN=IGRBEAM(I)%NENTITY
        DO J=1,NN
          IGRBEAM(I)%ENTITY(J) = ITR1(IGRBEAM(I)%ENTITY(J))
        ENDDO
      ENDDO
C REORDERING FOR ITAGPRLD_BEAM
      INUM(8,1:NUMELP)=ITAGPRLD_BEAM(1:NUMELP)
      DO I=1,NUMELP
        ITAGPRLD_BEAM(I) = INUM(8,INDEX(I))
      ENDDO
C
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELP
        II  = ITRI(1,INDEX(I))
        JJ  = ITRI(1,INDEX(I-1))
        II2 = ITRI(2,INDEX(I))
        JJ2 = ITRI(2,INDEX(I-1))
        II3 = ITRI(3,INDEX(I))
        JJ3 = ITRI(3,INDEX(I-1))
        II4 = ITRI(4,INDEX(I))
        JJ4 = ITRI(4,INDEX(I-1))
        II5 = ITRI(5,INDEX(I))
        JJ5 = ITRI(5,INDEX(I-1))
        IF (II/=JJ .OR. II2/=JJ2 .OR. II3/=JJ3 .OR. II4/=JJ4 .OR. II5/=JJ5) THEN
          ND=ND+1
          EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELP+1
C
      RETURN
      END
