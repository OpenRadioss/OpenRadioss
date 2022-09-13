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
Chd|  XGRHEAD                       source/elements/xelem/xgrhead.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE XGRHEAD(
     1       KXX,  GEO,     INUM,    ITR1,
     2       EADD, INDEX,   ITRI,    IPARTX,     
     3       ND,   IGRSURF,
     4       CEP,  XEP,IPM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     KXX(5,NUMELX)     TABLEAU CONECS+PID+No ElEMENTS MULTIBRIN   E
C     GEO(NPROPG,NUMGEO)TABLEAU DES CARACS DES PID                 E
C     INUM(6,NUMELX)    TABLEAU DE TRAVAIL                         E/S
C     ITR1(NSELR)       TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELX)      TABLEAU DES ADRESSES DANS IDAM CHGT DAMIER   S
C     INDEX(NUMELX)     TABLEAU DE TRAVAIL                         E/S
C     ITRI(4,NUMELX)    TABLEAU DE TRAVAIL                         E/S
C     IPARTX(NUMELX)    TABLEAU PART                               E/S
C     CEP(NUMELX)    TABLEAU PROC                                  E/S
C     XEP(NUMEX)    TABLEAU PROC                                  E/S
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
#include      "com10_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr21_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER KXX(5,*),INUM(6,*),IPARTX(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(4,*),
     .        ND, CEP(*), XEP(*),
     .        IPM(NPROPMI,*)
C     REAL OU REAL*8
      my_real
     .    GEO(NPROPG,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .        I, K, NG, ISSN, NPN, NN, N, MID, PID ,
     .        II, J, II2,JJ2,JJ, II3, JJ3, L,NGROU,
     .        MSKMTN,MSKISN,MSKPID, MODE, WORK(70000)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
C
      DATA MSKMTN /O'07770000000'/
      DATA MSKISN /O'00000000700'/
      DATA MSKPID /O'07777777777'/
C
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C----------------------------------------------------------
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
C
      DO I=1,NUMELX
        EADD(I)=1
        ITRI(4,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTX(I)
        INUM(2,I)=KXX(1,I)
        INUM(3,I)=KXX(2,I)
        INUM(4,I)=KXX(3,I)
        INUM(5,I)=KXX(4,I)
        INUM(6,I)=KXX(5,I)
      ENDDO

      IF (IMACH==3) THEN
        DO I=1,NUMELX
          XEP(I)=CEP(I)
        ENDDO
      ENDIF
C
      DO I = 1, NUMELX
        MID= KXX(1,I)
        PID= KXX(2,I)
        MTN= NINT(GEO(12,PID))
        IF (MTN<28.OR.MTN>31) THEN
          CALL ANCMSG(MSGID=413,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=KXX(5,I),
     .                C1='MATERIAL',
     .                I2=IPM(1,MID),
     .                C2='MATERIAL',
     .                I3=MTN)
        ENDIF

        ISSN = 0
        IF(GEO(5,PID)/=ZERO) ISSN=1
C        
        ISSN=MY_SHIFTL(ISSN,6)
        MTN=MY_SHIFTL(MTN,21)
C
        ITRI(1,I)=MTN+ISSN
        ITRI(2,I)=PID
        ITRI(3,I)=KXX(3,I)
        ITRI(4,I)=0
      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELX , 4)
C
      DO I=1,NUMELX
        IPARTX(I) =INUM(1,INDEX(I))
      ENDDO
      IF (IMACH==3) THEN
        DO I=1,NUMELX
          CEP(I)=XEP(INDEX(I))
        ENDDO
      ENDIF
      DO  K=1,5
        DO  I=1,NUMELX
          KXX(K,I)=INUM(K+1,INDEX(I))
        ENDDO
      ENDDO
C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELX
        ITR1(INDEX(I))=I
      ENDDO


C
C RENUMEROTATION POUR SURFACES
C ow a verifier IBUFSSG - ITYP == 100
       DO I=1,NSURF
         NN=IGRSURF(I)%NSEG
         DO J=1,NN 
           IF(IGRSURF(I)%ELTYP(J) == 100)
     .        IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
         ENDDO
       ENDDO
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELX
        II=ITRI(1,INDEX(I))
        JJ=ITRI(1,INDEX(I-1))
        II2=ITRI(2,INDEX(I))
        JJ2=ITRI(2,INDEX(I-1))
        II3=ITRI(3,INDEX(I))
        JJ3=ITRI(3,INDEX(I-1))
        IF(II/=JJ.OR.II2/=JJ2.OR.II3/=JJ3) THEN
          ND=ND+1
          EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELX+1
C
      RETURN
      END
