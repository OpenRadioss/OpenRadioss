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
Chd|  IG3DGRHEAD                    source/elements/ige3d/ig3dgrhead.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IG3DGRHEAD(
     1       KXIG3D,  GEO,   INUM,    ITR1,
     2       EADD, INDEX, ITRI,    IPARTIG3D,
     3       ND,   IGRSURF,
     4       CEP,  XEP,IGEO, IPM, PM, NIGE, KNOTLOCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     KXIG3D(NIXIG3D,NUMELIG3D)  TABLEAU CONECS+PID+No ElEMENTS         E
C     GEO(NPROPG,NUMGEO)TABLEAU DES CARACS DES PID                      E
C     INUM(8,NUMELIG3D)    TABLEAU DE TRAVAIL                         E/S
C     ITR1(NSELR)       TABLEAU DE TRAVAIL                            E/S
C     EADD(NUMELIG3D)      TABLEAU DES ADRESSES DANS IDAM CHGT DAMIER   S
C     INDEX(NUMELIG3D)     TABLEAU DE TRAVAIL                         E/S
C     ITRI(4,NUMELIG3D)    TABLEAU DE TRAVAIL                         E/S
C     IPARTIG3D(NUMELIG3D)    TABLEAU PART                            E/S
C     CEP(NUMELIG3D)    TABLEAU PROC                                  E/S
C     XEP(NUMEX)    TABLEAU PROC                                      E/S
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr21_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*),INUM(NIXIG3D+1,*),IPARTIG3D(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(4,*),
     .        ND, CEP(*), XEP(*),
     .        IPM(NPROPMI,NUMMAT),NIGE(*)
      INTEGER,INTENT(IN) :: IGEO(NPROPGI,NUMGEO)
      my_real GEO(NPROPG,NUMGEO),PM(NPROPM,NUMMAT),KNOTLOCEL(*)
C
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER
     .        I, K, NG, ISSN, NPN, NN, N, MID, PID ,
     .        II, J, II2,JJ2,JJ, II3, JJ3, L,NGROU,
     .        MODE, WORK(70000),NCTRL,MLN,JALE_FROM_MAT,JALE_FROM_PROP
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
      my_real
     .    KNOTLOCELINDX(SKNOTLOCEL)
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C----------------------------------------------------------
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
C
      DO I=1,NUMELIG3D
        EADD(I)=1
        ITRI(4,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTIG3D(I)
        DO J=1,NIXIG3D
          INUM(J+1,I)=KXIG3D(J,I)
        ENDDO
        DO J=1,6
          KNOTLOCELINDX((I-1)*6+J)=KNOTLOCEL((I-1)*6+J)
        ENDDO
      ENDDO

c      IF (IMACH==3) THEN
c        DO I=1,NUMELIG3D
c          XEP(I)=CEP(I)
c        ENDDO
c      ENDIF
C
      DO I = 1, NUMELIG3D
        MID= KXIG3D(1,I)
        MLN = NINT(PM(19,ABS(MID)))
        PID= KXIG3D(2,I)
        NCTRL= KXIG3D(3,I)
C
        JALE_FROM_MAT = NINT(PM(72,MID))
        JALE_FROM_PROP = IGEO(62,PID)
        JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if unconsistent, error message was displayed in PART reader

        JLAG=0
        IF(JALE==0.AND.MLN/=18)JLAG=1
        JEUL=0
        IF(JALE==2)THEN
          JALE=0
          JEUL=1
        ELSEIF(JALE == 3) THEN
          JLAG = 1
          JALE = 1
        ENDIF
        IF(MLN/=50)JTUR=NINT(PM(70,MID))
        JTHE=NINT(PM(71,MID))
        JTHE = ABS(JTHE)
C
        ITRI(1,I)=NCTRL
        ITRI(2,I)=MID
        ITRI(3,I)=PID
        ITRI(4,I)=0
      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELIG3D , 4)
C
      DO I=1,NUMELIG3D
        IPARTIG3D(I) =INUM(1,INDEX(I))
        DO J=1,6
          KNOTLOCEL((I-1)*6+J)=KNOTLOCELINDX((INDEX(I)-1)*6+J)
        ENDDO
      ENDDO
c      IF (IMACH==3) THEN
c        DO I=1,NUMELIG3D
c          CEP(I)=XEP(INDEX(I))
c        ENDDO
c      ENDIF
      DO  K=1,NIXIG3D
        DO  I=1,NUMELIG3D
          KXIG3D(K,I)=INUM(K+1,INDEX(I))
        ENDDO
      ENDDO
C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELIG3D
        ITR1(INDEX(I))=I
      ENDDO
C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG_IGE
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP_IGE(J) == 101)
     .       IGRSURF(I)%ELEM_IGE(J) = ITR1(IGRSURF(I)%ELEM_IGE(J))
        ENDDO
      ENDDO
C
      DO I=1,NUMFAKENODIGEO
        NIGE(I)=ITR1(NIGE(I))
      ENDDO
C
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELIG3D
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
      EADD(ND+1) = NUMELIG3D+1
C
      RETURN
      END
