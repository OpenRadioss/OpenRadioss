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
Chd|  PRINTBCS                      source/constraints/general/bcs/printbcs.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PRINTBCS(ICODE     ,ISKEW   ,ITAB    ,ITABM1 ,IKINE  ,
     .                  IGRNOD    ,IBCSLAG ,LAG_NCF ,LAG_NKF,LAG_NHF,
     .                  IKINE1LAG ,ISKN    ,NOM_OPT , NBCSLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "titr_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE(*), ISKEW(*), ITAB(*), ITABM1(*), IKINE(*),
     .        IBCSLAG(5,*),
     .        LAG_NCF,LAG_NKF,LAG_NHF,IKINE1LAG(*),ISKN(LISKN,*)
      INTEGER NOM_OPT(LNOPT1,*), NBCSLAG
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JJ(12), IC, NC, N, NUSR, IS, IC1, IC2, IC3, IC4,
     .        NOSYS, J,J10(10),IGR,IGRS,ISU,IBCALE,J6(6),K,
     .        IC0, IC01, IC02, IC03, IC04, ID ,ILAGM,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IKINE1(3*NUMNOD),SUB_ID,
     .        CHKCOD,ISERR,NOD
      INTEGER IUN
      CHARACTER MESS*40,KEY*ncharkey,STRING*ncharfield,CODE*7,
     .        KEY2*ncharkey
      CHARACTER TITR*nchartitle,OPT*8
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,MY_OR,CHECK_NEW,NGR2USR
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA IUN/1/
      DATA MESS/'BOUNDARY CONDITIONS                     '/
C======================================================================|
C
      DO I=1,NUMNOD
        IF(ISKEW(I)==-1)ISKEW(I)=0
      ENDDO
C
      IF(IALE==0)THEN
        WRITE(IOUT,'(/A/A/A/)')TITRE(80),TITRE(81),
     .   '       NODE  TRANS. ROTAT.         SKEW'
C
      ELSE
        WRITE(IOUT,'(/A/A/A/)')TITRE(80),TITRE(81),TITRE(82)
      ENDIF
      IF(IPRI>=2)THEN
       DO 500 N=1,NUMNOD
       IC=ICODE(N)
       IF (IC==0) GO TO 500
       IC1=IC/512
       IC2=(IC-512*IC1)/64
       IC3=(IC-512*IC1-64*IC2)/8
       IC4=(IC-512*IC1-64*IC2-8*IC3)
       J6(1)=IC1/4
       J6(2)=(IC1-4*J6(1))/2
       J6(3)=(IC1-4*J6(1)-2*J6(2))
       J6(4)=IC2/4
       J6(5)=(IC2-4*J6(4))/2
       J6(6)=(IC2-4*J6(4)-2*J6(5))
       IF(IALE==0)THEN
C
         WRITE(IOUT,'(1X,I10,2(1X,3I2),3X,I10)')ITAB(N),J6,
     .           ISKN(4,ISKEW(N))
       ELSE
         JJ(1)=J6(1)
         JJ(2)=J6(2)
         JJ(3)=J6(3)
         JJ(4)=J6(4)
         JJ(5)=J6(5)
         JJ(6)=J6(6)
         JJ(7)=IC3/4
         JJ(8)=(IC3-4*JJ(7))/2
         JJ(9)=(IC3-4*JJ(7)-2*JJ(8))
         JJ(10)=IC4/4
         JJ(11)=(IC4-4*JJ(10))/2
         JJ(12)=(IC4-4*JJ(10)-2*JJ(11))
C
       WRITE(IOUT,'(1X,I10,4(1X,3I2),3X,I10)')ITAB(N),JJ,
     .           ISKN(4,ISKEW(N))
       ENDIF
 500   CONTINUE
      ENDIF
      IF (NBCSLAG>0) THEN
        WRITE(IOUT,1000)
        DO I = 1, NBCSLAG
          IGRS=IBCSLAG(1,I)
          IS = IBCSLAG(4,I)
          ID = IBCSLAG(2,I)
          IC = IBCSLAG(3,I)
          IC1=IC/512
          IC2=(IC-512*IC1)/64
          IC3=(IC-512*IC1-64*IC2)/8
          IC4=(IC-512*IC1-64*IC2-8*IC3)
          J6(1)=IC1/4
          J6(2)=(IC1-4*J6(1))/2
          J6(3)=(IC1-4*J6(1)-2*J6(2))
          J6(4)=IC2/4
          J6(5)=(IC2-4*J6(4))/2
          J6(6)=(IC2-4*J6(4)-2*J6(5))
          IF (IPRI>=2) THEN
            DO J=1,IGRNOD(IGRS)%NENTITY
              NOSYS=IGRNOD(IGRS)%ENTITY(J)
C
              WRITE(IOUT,'(1X,I10,2(1X,3I2),3X,I10)')ITAB(NOSYS),J6,
     .                ISKN(4,IS)
            ENDDO
          ENDIF
          IC1=J6(1)*4 +J6(2)*2 +J6(3)
          IC2=J6(4)*4 +J6(5)*2 +J6(6)
          IBCSLAG(2,I) = IC1
          IBCSLAG(3,I) = IC2
        ENDDO
      ENDIF

1000  FORMAT(/,
     . '     BOUNDARY CONDITIONS BY LAGRANGE MULTIPLIERS'/
     . '     ----------------------- '/
     . '     NODE  TRANS. ROTAT.       SKEW'/)
      RETURN
      END
