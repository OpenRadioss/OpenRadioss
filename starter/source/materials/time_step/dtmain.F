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
      !||====================================================================
      !||    dtmain             ../starter/source/materials/time_step/dtmain.F
      !||--- called by ------------------------------------------------------
      !||    ig3dinit3          ../starter/source/elements/ige3d/ig3dinit3.F
      !||    inirig_mat         ../starter/source/elements/initia/inirig_mat.F
      !||    multifluid_init3   ../starter/source/multifluid/multifluid_init3.F
      !||    q4init2            ../starter/source/elements/solid_2d/quad4/q4init2.F
      !||    qinit2             ../starter/source/elements/solid_2d/quad/qinit2.F
      !||    s10init3           ../starter/source/elements/solid/solide10/s10init3.F
      !||    s16init3           ../starter/source/elements/thickshell/solide16/s16init3.F
      !||    s20init3           ../starter/source/elements/solid/solide20/s20init3.F
      !||    s4init3            ../starter/source/elements/solid/solide4/s4init3.F
      !||    s6cinit3           ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    s8cinit3           ../starter/source/elements/thickshell/solide8c/s8cinit3.F
      !||    s8zinit3           ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    scinit3            ../starter/source/elements/thickshell/solidec/scinit3.F
      !||    sinit3             ../starter/source/elements/solid/solide/sinit3.F
      !||    spinit3            ../starter/source/elements/sph/spinit3.F
      !||    suinit3            ../starter/source/elements/elbuf_init/suinit3.F
      !||--- calls      -----------------------------------------------------
      !||    dt105law           ../starter/source/materials/time_step/dt105law.F
      !||    dt10law            ../starter/source/materials/time_step/dt10law.F
      !||    dt11law            ../starter/source/materials/time_step/dt11law.F
      !||    dt14law            ../starter/source/materials/time_step/dt14law.F
      !||    dt15law            ../starter/source/materials/time_step/dt15law.F
      !||    dt16law            ../starter/source/materials/time_step/dt16law.F
      !||    dt18law            ../starter/source/materials/time_step/dt18law.F
      !||    dt1law             ../starter/source/materials/time_step/dt1law.F
      !||    dt21law            ../starter/source/materials/time_step/dt21law.F
      !||    dt25law            ../starter/source/materials/time_step/dt25law.F
      !||    dt34law            ../starter/source/materials/time_step/dt34law.F
      !||    dt37law            ../starter/source/materials/time_step/dt37law.F
      !||    dt38law            ../starter/source/materials/time_step/dt138aw.F
      !||    dt3law             ../starter/source/materials/time_step/dt3law.F
      !||    dt42law            ../starter/source/materials/time_step/dt42law.F
      !||    dt51law            ../starter/source/materials/time_step/dt51law.F
      !||    dt5law             ../starter/source/materials/time_step/dt5law.F
      !||    dt6law             ../starter/source/materials/time_step/dt6law.F
      !||    dt70law            ../starter/source/materials/time_step/dt70law.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE DTMAIN(GEO  ,PM   ,IPM   ,PID   ,MAT   ,FV    ,
     .                  EINT ,TEMP ,DELTAX,RK    ,RE    ,BUFMAT, DDELTAX, AIRE, VOL, DTX,
     .                  IGEO ,IGTYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IGTYP
      INTEGER             :: IPM(*), PID(*), MAT(*),IGEO(NPROPGI,*)
      my_real             :: PM(*),GEO(*),FV,EINT(*),TEMP(*),DELTAX(*),RK(*),RE(*), DDELTAX(*),
     .     AIRE(*), VOL(*), DTX(*)
      my_real, INTENT(IN) :: BUFMAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s 
C-----------------------------------------------
C=======================================================================
      SELECT CASE(MTN)
c
      CASE (1,2,13,22,28,29,30,31,33,35)
        CALL DT1LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (3,4,23)
        CALL DT3LAW(PM,EINT,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (5,97)
        CALL DT5LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (6,17)
        CALL DT6LAW(PM,EINT,RK,RE,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (7,8,9)
      CASE (10,102)
        CALL DT10LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (11)
        CALL DT11LAW(DDELTAX, DTX)
      CASE (12,14)
        CALL DT14LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (15,24)
        CALL DT15LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (16)
        CALL DT16LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (18)
        CALL DT18LAW(PM,IPM,MAT,EINT,TEMP,DELTAX, DTX)
      CASE (21)
        CALL DT21LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (25)
        CALL DT25LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (34)
        CALL DT34LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (19,20,26,27,32)
        CONTINUE
      CASE (37)
        CALL DT37LAW(PM,GEO,PID,MAT,BUFMAT,IPM, DDELTAX, AIRE, VOL, DTX)
      CASE (38,88)
        CALL DT38LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (42,69)
        CALL DT42LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX, LLT )
      CASE (51)
        CALL DT51LAW(PM,GEO,PID,MAT,BUFMAT,IPM, DDELTAX, AIRE, VOL, DTX)
      CASE (59,83,116,117)
          DTX(LFT:LLT) = EP20
          WRITE(IOUT,1000) MTN
      CASE (120)
        IF (IGTYP == 43)THEN
          DTX(LFT:LLT) = EP20
          WRITE(IOUT,1000) MTN
        ELSE
          CALL DT1LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX )
        ENDIF
      CASE (105)
        CALL DT105LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX)
      CASE (70)
        CALL DT70LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX,IGEO )
      CASE DEFAULT
        CALL DT1LAW(PM,GEO,PID,MAT, DDELTAX, AIRE, VOL, DTX )
      END SELECT
C-----------
 1000 FORMAT(/,1X,' ELEMENT TIME STEP IS NOT COMPUTED FOR MATERIAL LAW NO',I4,/)
C-----------
      RETURN
      END
