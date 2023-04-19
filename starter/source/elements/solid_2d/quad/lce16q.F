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
Chd|  LCE16Q3                       source/elements/solid_2d/quad/lce16q.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        REORDR                        source/elements/solid_2d/quad/reordr.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE LCE16Q3(IXQ    ,ISEL  ,PM     ,IPOINT ,ITAB ,ITABM1,
     .                   ICODE  ,IPARTQ,IGRQUAD,IPM    ,IGEO )
C--------------------------------------------------------
C     LECTURE DES ELEMENTS 2D
C     VERSION NUMEROTATION DES NOEUDS LIBRE/MARS 90/DIM
C     LECTURE FORMAT IDENTIQUE AUX COQUES
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "titr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), ISEL(*), IPOINT(2,*), ITAB(*),
     .    ITABM1(*), ICODE(*),IPARTQ(*),
     .    IPM(NPROPMI,*),IGEO(NPROPGI,*)
C     REAL
      my_real
     .   PM(NPROPM,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRQUAD)  :: IGRQUAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MT, MLAW, JTUR, ICODT, I1, I2, INEW
      INTEGER IC,IC1,IC2,IC3,IC4,MID,PID
      CHARACTER MESS*40, MESS2*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'2D SOLID ELEMENTS DEFINITION            '/
      DATA MESS2/'2D SOLID ELEMENTS SELECTION FOR TH PLOT '/
C
C----------------------------------------------------
C     DETECTION DES ELEMENTS LOIS 6 PAROI---->LOI 17
C----------------------------------------------------
C      LOI 17 SI LA BCS EST  011  OU 111
      DO I=1,NUMELQ
          MT=IXQ(1,I)
          MLAW=NINT(PM(19,MT))
          JTUR=NINT(PM(70,MT))
          DO J=2,5
           IF((MLAW==6.AND.JTUR/=0) .OR. MLAW==46)THEN
            IC=ICODE(IXQ(J,I))
            IC1=IC/512
            IC2=(IC-512*IC1)/64
            IC3=(IC-512*IC1-64*IC2)/8
            IC4=(IC-512*IC1-64*IC2-8*IC3)
            IF(IC1==3.OR.IC1==7.OR.IC4==3.OR.IC4==7)
     .        IXQ(1,I)=-IABS(IXQ(1,I))
           ENDIF
          ENDDO
      ENDDO
C----------------------------------------------------
C     CLASSEMENT DES ELEMENTS PAR LOI DE MATERIAU
C----------------------------------------------------
      CALL REORDR(IXQ   ,NIXQ   ,NUMELQ ,PM     ,IPOINT ,
     .            IPARTQ,NGRQUAD,IGRQUAD)
C
      I1=1
      I2=MIN0(50,NUMELQ)
C
      IF(IPRI>=5)THEN
       WRITE (IOUT,'(//A//)') TITRE(206)
   90  WRITE (IOUT,'(//A/A//A/)')TITRE(100),TITRE(101),TITRE(102)
       DO 100 I=I1,I2
       INEW=IPOINT(1,I)
       IF(INEW < 1)CYCLE
       IF(IXQ(1,INEW)<1)CYCLE
       MID =IPM (1,IXQ(1,INEW))
       PID =IGEO(1,IXQ(6,INEW))
  100  WRITE (IOUT,'(8I10)')IXQ(NIXQ,INEW),INEW,MID,PID,(ITAB(IXQ(J,INEW)),J=2,5)
       IF(I2==NUMELQ)GOTO 200
       I1=I1+50
       I2=MIN0(I2+50,NUMELQ)
       GOTO 90
      ENDIF
C
 200  CONTINUE
C
      RETURN
      END
