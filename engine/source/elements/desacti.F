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
!||    desacti          ../engine/source/elements/desacti.F
!||--- called by ------------------------------------------------------
!||    resol            ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    convecoff        ../engine/source/constraints/thermic/convecoff.F
!||    eloff            ../engine/source/elements/eloff.F
!||    radiatoff        ../engine/source/constraints/thermic/radiatoff.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    groupdef_mod     ../common_source/modules/groupdef_mod.F
!||    sensor_mod       ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE DESACTI(IXS      ,IXQ      ,IXC     ,IXP     ,IXT      ,
     .                   IXR      ,IXTG     ,IPARG   ,IACTIV  , 
     .                   NSENSOR  ,SENSOR_TAB,FSKY    ,X      ,ELBUF_TAB,
     .                   IBCV     ,FCONV    ,IBCR    ,FRADIA  ,IGROUPS  ,
     .                   FACTIV   ,TEMP     ,MCP     ,PM      ,MCP_OFF  ,
     .                   IGRBRIC  ,IGRQUAD  ,IGRSH4N ,IGRSH3N ,IGRTRUSS ,
     .                   IGRBEAM  ,IGRSPRING,GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
      USE SENSOR_MOD
      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER IACTIV(LACTIV,*),IPARG(NPARG,*),
     .        IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),  
     .        IXT(NIXT,*),IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*)
      INTEGER IBCV(*), IBCR(*), IGROUPS(*)
      my_real FSKY(*), FCONV(*), FRADIA(*)
      my_real X(3,*), FACTIV(LRACTIV,*), TEMP(*), MCP(*), PM(NPROPM,*)
      my_real MCP_OFF(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,IFLAG,ISENS,IGSH,IGSH3,IGBR,IGQU,IGBM,IGTR,IGSP,OFFCHANGE
      INTEGER ITHERM_FE
      my_real STARTT, STOPT
C======================================================================|
      OFFCHANGE = 0
      ITHERM_FE = GLOB_THERM%ITHERM_FE
!
      DO N = 1,NACTIV
        ISENS = IACTIV(2,N)
        IFLAG = IACTIV(1,N)
        IF (IACTIV(10,N) == 1) THEN
         IF (ISENS > 0) THEN
C----------------------------------------------------------------------
C   DESACTIVATION DES ELEMENTS
C----------------------------------------------------------------------
          IF (IFLAG == 1 .AND. TT > SENSOR_TAB(ISENS)%TSTART) THEN
            CALL ELOFF(IXS     ,IXQ     ,IXC      ,IXP     ,IXT      ,
     .                 IXR     ,IXTG    ,IPARG    ,
     .                 IACTIV  ,TT      ,IFLAG    ,N       ,ELBUF_TAB,
     .                 X       ,TEMP    ,MCP      ,PM      ,IGROUPS  ,
     .                 MCP_OFF ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N  ,
     .                 IGRTRUSS,IGRBEAM ,IGRSPRING,ITHERM_FE)

            IACTIV(1,N) = 0
            OFFCHANGE=1
            IF (IPARIT /= 0) THEN
              DO I=1,8*LSKY
                FSKY(I)=ZERO
              ENDDO
            ENDIF
C----------------------------------------------------------------------
C   ACTIVATION DES ELEMENTS
C----------------------------------------------------------------------
          ELSEIF (IFLAG == 0 .AND. TT <= SENSOR_TAB(ISENS)%TSTART) THEN
            CALL ELOFF(IXS     ,IXQ     ,IXC      ,IXP     ,IXT      ,
     .                 IXR     ,IXTG    ,IPARG    ,
     .                 IACTIV  ,TT      ,IFLAG    ,N       ,ELBUF_TAB,
     .                 X       ,TEMP    ,MCP      ,PM      ,IGROUPS  ,
     .                 MCP_OFF ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N  ,
     .                 IGRTRUSS,IGRBEAM ,IGRSPRING,ITHERM_FE)

            IACTIV(1,N) = 1
            OFFCHANGE=1
          ENDIF
         ENDIF        

        ELSEIF(IACTIV(10,N) == 2) THEN
C----------------------------------------------------------------------
C   DESACTIVATION DES ELEMENTS AT TIME ZERO
C----------------------------------------------------------------------
          IF(TT == ZERO) THEN
            CALL ELOFF(IXS     ,IXQ     ,IXC      ,IXP     ,IXT      ,
     .                 IXR     ,IXTG    ,IPARG    ,
     .                 IACTIV  ,TT      ,1        ,N       ,ELBUF_TAB,
     .                 X       ,TEMP    ,MCP      ,PM      ,IGROUPS  ,
     .                 MCP_OFF ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N  ,
     .                 IGRTRUSS,IGRBEAM ,IGRSPRING,ITHERM_FE)

            IACTIV(1,N) = 0
            OFFCHANGE=1
          ENDIF
C----------------------------------------------------------------------
C   ACTIVATION DES ELEMENTS
C----------------------------------------------------------------------
          STARTT = FACTIV(1,N)
          STOPT  = FACTIV(2,N)
          IFLAG  = IACTIV(1,N)
          IF(ITHERM_FE > 0) THEN
             STARTT = STARTT / GLOB_THERM%THEACCFACT
             STOPT  = STOPT  / GLOB_THERM%THEACCFACT
          ENDIF
          IF(IFLAG == 0 .AND. TT >= STARTT .AND. TT < STOPT) THEN
            CALL ELOFF(IXS     ,IXQ     ,IXC      ,IXP     ,IXT      ,
     .                 IXR     ,IXTG    ,IPARG    ,
     .                 IACTIV  ,TT      ,IFLAG    ,N       ,ELBUF_TAB,
     .                 X       ,TEMP    ,MCP      ,PM      ,IGROUPS  ,
     .                 MCP_OFF ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N  ,
     .                 IGRTRUSS,IGRBEAM ,IGRSPRING,ITHERM_FE)
            IACTIV(1,N) = 1
            OFFCHANGE=1
          ENDIF
C----------------------------------------------------------------------
C   DESACTIVATION DES ELEMENTS
C----------------------------------------------------------------------
          IF(IFLAG == 1 .AND. TT > STOPT) THEN
            CALL ELOFF(IXS     ,IXQ     ,IXC      ,IXP     ,IXT      ,
     .                 IXR     ,IXTG    ,IPARG    ,
     .                 IACTIV  ,TT      ,1        ,N       ,ELBUF_TAB,
     .                 X       ,TEMP    ,MCP      ,PM      ,IGROUPS  ,
     .                 MCP_OFF ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N  ,
     .                 IGRTRUSS,IGRBEAM ,IGRSPRING,ITHERM_FE)
            IACTIV(1,N) = 0
            OFFCHANGE=1
          ENDIF      
        ENDIF
      ENDDO ! N=1,NACTIV
C
      IF(ITHERM_FE > 0 .AND. OFFCHANGE == 1) THEN
         IF(GLOB_THERM%NUMCONV > 0)  CALL CONVECOFF(IBCV, FCONV, IPARG, IGROUPS, ELBUF_TAB,GLOB_THERM)
         IF(GLOB_THERM%NUMRADIA > 0) CALL RADIATOFF(IBCR, FRADIA,IPARG, IGROUPS, IXS, ELBUF_TAB,GLOB_THERM)
      ENDIF

      IF(ITHERM_FE > 0 .AND. TT == ZERO) THEN 
C     Final call to set MCP_OFF(i) = 0 if all nodes 
            IFLAG=-1
            CALL ELOFF(IXS     ,IXQ     ,IXC      ,IXP     ,IXT      ,
     .                 IXR     ,IXTG    ,IPARG    ,
     .                 IACTIV  ,TT      ,IFLAG    ,N       ,ELBUF_TAB,
     .                 X       ,TEMP    ,MCP      ,PM      ,IGROUPS  ,
     .                 MCP_OFF ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N  ,
     .                 IGRTRUSS,IGRBEAM ,IGRSPRING,ITHERM_FE)
      ENDIF


C
      RETURN
      END

