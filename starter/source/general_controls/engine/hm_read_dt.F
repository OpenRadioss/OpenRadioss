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
Chd|  HM_READ_DT                    source/general_controls/engine/hm_read_dt.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE HM_READ_DT(IGRPART)
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "scr02_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sphcom.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(GROUP_)  ,DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
        my_real
     .          DT_CRIT
C-----------------------------------------------
C     Initializations ../..
C-----------------------------------------------
        DO I=1,102
          DTFAC1(I) = ZERO
          DTMIN1(I) = ZERO
          IDTMIN(I) = 0
          IDTGR(I)  = 0
        ENDDO
        NODADT = 0
C-----
        MIN_ASPECT = ZERO
        MIN_DEFV   = ZERO
C-----
C     /DTIX
        DTIN   = ZERO
        DTMX   = ZERO
C-----
C     /DT/AMS & /DT/INTER/AMS
        DTFACS = ZERO
        DTMINS = ZERO
        DTFACS_INT = ZERO
        DTMINS_INT = ZERO
C-----------------------------------------------
C     Read options ../..
C-----------------------------------------------
C
C-----------------------------------------------
C     Finitializations after reading
C-----------------------------------------------
        DO I=1,51
          IF(DTMIN1(I) == ZERO)DTMIN1(I) = DTMIN
          IF(DTFAC1(I) == ZERO)DTFAC1(I) = DTFAC
        ENDDO
C-----
        IF(NODADT == 0)THEN
          IF(IDTMIN(1) == 0)IDTMIN(1) = 1
          IF(IDTMIN(2) == 0)IDTMIN(2) = 1
          IF(IDTMIN(3) == 0)IDTMIN(3) = 2
          IF(IDTMIN(7) == 0)IDTMIN(7) = 2
        ENDIF
C-----
        IF(IDTMINS/=0)THEN
          IF(DTMINS == ZERO)DTMINS = DTMIN
          IF(DTFACS == ZERO)DTFACS = DTFAC
          IF(TOL_SMS == ZERO) TOL_SMS = EM03
          IF(NSMSPCG==0)NSMSPCG=1000
        ENDIF
        IF(IDTMINS_INT/=0)THEN
          IF(DTMINS_INT == ZERO)DTMINS_INT = DTMIN
          IF(DTFACS_INT == ZERO)DTFACS_INT = DTFAC
          IF(TOL_SMS == ZERO) TOL_SMS = EM03
          IF(NSMSPCG==0)NSMSPCG=1000
        ENDIF
C-----
C-----------------------------------------------
C     Printing
C-----------------------------------------------
        WRITE(IOUT,1000)
        IF(IDT1SH /=0)   WRITE(IOUT,4720)
        IF(IDT1SOL /= 0) WRITE(IOUT,4710)
        IF(IDT1SOL/=0)   WRITE(IOUT,4730)
        IF(IDT1TET10/=0) WRITE(IOUT,4740)
        IF(NODADT == 0)THEN
          IF (MIN_ASPECT+MIN_DEFV > ZERO) THEN
            WRITE(IOUT,1115) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                  MIN_ASPECT,MIN_DEFV,
     +                  DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                  DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                  DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                  DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                  DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                  DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                  DTFAC1(10),DTMIN1(10),IDTMIN(10)
            ! WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
          ELSE
            WRITE(IOUT,1105) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                   DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                   DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                   DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                   DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                   DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                   DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                   DTFAC1(10),DTMIN1(10),IDTMIN(10)
            ! WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
          END IF
          IF(IDTMIN(11) == 3 .OR. IDTMIN(11) == 8) THEN
            WRITE(IOUT,1107)DTFAC1(11),DTMIN1(11),IDTMIN(11)
          ENDIF
          IF(IDTMINS_INT/=0)THEN
            WRITE(IOUT,1208) DTFACS_INT,DTMINS_INT
          END IF
          IF(IDTMINS/=0)THEN
            IF(IDTGRS <= 0)THEN
              WRITE(IOUT,1108) DTFACS,DTMINS,TOL_SMS,
     .                         NSMSPCG,NCPRISMS,-IDTGRS
            ELSE
              WRITE(IOUT,1108) DTFACS,DTMINS,TOL_SMS,
     .                         NSMSPCG,NCPRISMS,
     .                         IGRPART(IDTGRS)%ID
            END IF
          END IF
        ELSE
          IF(IDTMIN(11)==3.OR.IDTMIN(11) == 8)THEN
            WRITE(IOUT,1106) DTFAC1(11),DTMIN1(11),IDTMIN(11)
          END IF
          IF (MIN_ASPECT+MIN_DEFV > ZERO) THEN
            WRITE(IOUT,1115) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                  MIN_ASPECT,MIN_DEFV,
     +                  DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                  DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                  DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                  DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                  DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                  DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                  DTFAC1(10),DTMIN1(10),IDTMIN(10)
            ! WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
          ELSE
            WRITE(IOUT,1105) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                   DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                   DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                   DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                   DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                   DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                   DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                   DTFAC1(10),DTMIN1(10),IDTMIN(10)
            ! WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
          END IF ! (MIN_ASPECT+MIN_DEFV > ZERO) THEN
          IF(IDTMINS_INT/=0)THEN
            WRITE(IOUT,1208) DTFACS_INT,DTMINS_INT
          END IF
C
          IF (IDTMINS/=0) THEN
            IF (ISMS_SELEC < 3) THEN
              IF(IDTGRS<=0)THEN
                WRITE(IOUT,1108) DTFACS,DTMINS,TOL_SMS,
     .                        NSMSPCG,NCPRISMS,-IDTGRS
              ELSE
                WRITE(IOUT,1108) DTFACS,DTMINS,TOL_SMS,
     .                           NSMSPCG,NCPRISMS,
     .                           IGRPART(IDTGRS)%ID
              END IF
            ELSE
C--         /DT/CST_AMS >- automatic element selection
              DT_CRIT = DTMINS / MAX(EM20,DTFAC1(11))
              IF(IDTGRS<=0)THEN
                WRITE(IOUT,2108) DTFACS,DTMINS,TOL_SMS,
     .                       NSMSPCG,NCPRISMS,DT_CRIT,-IDTGRS
              ELSE
                WRITE(IOUT,2108) DTFACS,DTMINS,TOL_SMS,
     .                       NSMSPCG,NCPRISMS,DT_CRIT,
     .                       IGRPART(IDTGRS)%ID
              END IF
            ENDIF
          END IF
C
        ENDIF ! IF(NODADT == 0)THEN
C-----
        IF(NUMSPH /= 0)THEN
          IF(IDTMIN(51) == 3)THEN
            WRITE(ISTDO,*)
     .      ' ** WARNING SMALL STRAIN FORMULATION FOR SPH'
            WRITE(IOUT,*)
     .      ' ** WARNING SMALL STRAIN FORMULATION IS NOT AVAILABLE FOR SPH,',
     .            ' OPTION /DT/SPHCEL/CST WILL BE OMITTED.'
            IDTMIN(51)=0
          ENDIF
          IF(NODADT == 1)THEN
            WRITE(IOUT,*)
     .      'IMPROVED TIME STEP (NODAL) COMPUTATION ON SPH PARTICLES :'
            DTFAC1(51)=DTFAC1(11)
          ENDIF
          WRITE(IOUT,1151)DTFAC1(51),DTMIN1(51),IDTMIN(51)
        ENDIF
C-----
        WRITE(IOUT,1152)DTFAC1(52),DTMIN1(52),IDTMIN(52)
C-----------------------------------------------
 1000   FORMAT(/,' TIME STEP CONTROL . . . . . . .. . . . . . . ',/,
     .           ' ---------------------------------------------'/)
 1105   FORMAT(
     .    ' BRICK TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .    ' BRICK MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE BRICK) ',I5//
     .    ' QUAD TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .    ' QUAD MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE QUAD). ',I5//
     .    ' SHELL TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .    ' SHELL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE, 3:SMALL STRAIN)',I5//
     .    ' TRUSS TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .    ' TRUSS MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .    ' BEAM TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .    ' BEAM MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .    ' SPRING TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .    ' SPRING MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .    ' AIRBAG TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .    ' AIRBAG MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP). . . . . . . . . . . . . ',I5//
     .    ' CONTACT TIME STEP SCALE FACTOR . . . . . . . . . ',G14.7/
     .    ' CONTACT MINIMUM TIME STEP. . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG(1:STOP, 2:REMOVE NODE FROM INTERF)',I5/ )
 1106   FORMAT(
     .    ' NODAL TIME STEP SCALE FACTOR,  . . . . . . . . . ',G14.7/
     .    ' NODAL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG . . . . . . . . . . . . . . . . . ',I5/
     .    '    1:STOP. . . . . . . . . . . . . . . . . . . . ',/
     .    '    3:INCREASE MASS, ORIGINAL FORMULATION . . . . ',/
     .    '    8:INCREASE MASS, IMPROVED FORMULATION . . . . ',//)
 1107   FORMAT(
     .    ' CONTACT NODAL TIME STEP SCALE FACTOR . . . . . . ',G14.7/
     .    ' CONTACT NODAL MINIMUM TIME STEP. . . . . . . . . ',G14.7/
     .    ' CONTACT NODAL MIN. TIME FLAG . . . . . . . . . . ',I5/
     .    '    3:INCREASE MASS, ORIGINAL FORMULATION . . . . ',/
     .    '    8:INCREASE MASS, IMPROVED FORMULATION . . . . ',//)
 1108   FORMAT(
     .    ' ADVANCED MASS SCALING IS ON . . . . . . . . . . .',/
     .    ' AMS NODAL TIME STEP SCALE FACTOR. . . . . . . . .',G14.7/
     .    ' AMS NODAL MINIMUM TIME STEP . . . . . . . . . . .',G14.7/
     .    ' AMS TOLERANCE ON CONVERGENCE. . . . . . . . . . .',G14.7/
     .    ' AMS MAXIMUM NUMBER OF ITERATIONS FOR CONVERGENCE.',I10/
     .    ' OUTPUT FREQUENCY OF INFORMATION / CONVERGENCE . .',I10/
     .    ' PART GROUP ID (=0 ALL PARTS) . . . . . . . . . . ',I10//)
 1113   FORMAT(
     .    ' GLOBAL TIME STEP IS ON . . . . . . . . . . . . . . .',/
     .    ' GLOBAL TIME STEP SCALE FACTOR. . . . . . .. . . .',G14.7/
     .    ' GLOBAL TIME STEP TOLERANCE ON CONVERGENCE. . .. .',G14.7/
     .    ' NUMBER OF CYCLES FOR GLOBAL TIME STEP UPDATE. . .',I10//)
 1115   FORMAT(
     .    ' BRICK TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .    ' BRICK MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE 3:CST )',I5//
     .    ' BRICK MINIMUM ASPECT RATIO (Tet collapse). . . . :',G14.7/
     .    ' BRICK MINIMUM VOLUME CHANGE . . . . . . . . . . .:',G14.7//
     .    ' QUAD TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .    ' QUAD MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE QUAD). ',I5//
     .    ' SHELL TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .    ' SHELL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE, 3:SMALL STRAIN)',I5//
     .    ' TRUSS TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .    ' TRUSS MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .    ' BEAM TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .    ' BEAM MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .    ' SPRING TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .    ' SPRING MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .    ' AIRBAG TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .    ' AIRBAG MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP). . . . . . . . . . . . . ',I5//
     .    ' INTERFACE TYPE 7 TIME STEP SCALE FACTOR. . . . . ',G14.7/
     .    ' INTERFACE TYPE 7 MINIMUM TIME STEP . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG(1:STOP, 2:REMOVE NODE FROM INTERF)',I5/ )
 1151   FORMAT(
     .    ' SMOOTH PARTICLES TIME STEP SCALE FACTOR. . . . . ',G14.7/
     .    ' SMOOTH PARTICLES MINIMUM TIME STEP . . . . . . . ',G14.7/
     .    ' MIN. TIME FLAG (1:STOP, 2:DELETE, 5:KILL). . . . ',I5  )
 1152   FORMAT(
     .    ' FVMBAG TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .    ' FVMBAG MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .    ' FVMBAG TIME STEP FLAG. . . . . . . . . . . . . . ',I5,/)
 1208   FORMAT(
     .    ' AMS CONTACT TIME STEP IS ON . . . . . . . . . . . . .',/
     .    ' . . . . . .(ALL CONTACTS WILL BE CONCERNED).) . . . .',/
     .    ' AMS CONTACT TIME STEP SCALE FACTOR. . . . . . . . . .',G14.7/
     .    ' AMS CONTACT MINIMUM TIME STEP . . . . . . . . . . . .',G14.7//)
 2108   FORMAT(
     .    ' ADVANCED MASS SCALING IS ON . . . . . . . . . . .',/
     .    ' AMS TIME STEP SCALE FACTOR. . . . . . . . . . . .',G14.7/
     .    ' AMS MINIMUM TIME STEP . . . . . . . . . . . . . .',G14.7/
     .    ' AMS TOLERANCE ON CONVERGENCE. . . . . . . . . . .',G14.7/
     .    ' AMS MAXIMUM NUMBER OF ITERATIONS FOR CONVERGENCE.',I10/
     .    ' OUTPUT FREQUENCY OF INFORMATION / CONVERGENCE . .',I10/
     .    ' AUTO ELEMENT SELECTION - TIME STEP CRITERIA . . .',G14.7/
     .    ' PART GROUP ID (=0 ALL PARTS) . . . . . . . . . . ',I10//)
 4710   FORMAT(
     .    ' ACCURATE TIME STEP COMPUTATION FOR HEPH IS USED .',/)
 4720   FORMAT(
     .    ' ACCURATE TIME STEP FOR SHELLS IS USED. . . . . . ',/)
 4730   FORMAT(
     .    ' ACCURATE TIME STEP FOR 4-NODE TETRAHEDRA IS USED   ',/)
 4740   FORMAT(
     .    ' ACCURATE TIME STEP FOR 10-NODE TETRAHEDRA IS USED  ',/)
C-----------------------------------------------
        RETURN
      END
