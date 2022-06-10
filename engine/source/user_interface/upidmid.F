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
#include "my_real.inc"
Chd|====================================================================
Chd|  UPIDMID_DUM                   source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPIDMID_DUM(IERR)
C---------+---------+---+---+--------------------------------------------
#include      "implicit_f.inc"
       INTEGER IERR
       IERR=0
      END
Chd|====================================================================
Chd|  GET_U_MAT                     source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|        RUSER36                       source/elements/spring/ruser36.F
Chd|        GET_U_MAT                     source/user_interface/upidmid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        GET_U_MAT                     source/user_interface/upidmid.F
Chd|====================================================================
      my_real FUNCTION 
     .      GET_U_MAT(IVAR,IM)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"
C
      INTEGER IVAR,IM,NVAR,DEPLA,IADBUF
C=======================================================================
C     NVAR = NUPARAM = NINT(PM(26,IM)) old
C     NVAR = NUPARAM = IPM(9,IM)
C
      DEPLA= 9+(IM-1)*NPROPMI
      CALL GET_VAR_USER_I(19,DEPLA,NVAR)
      IF(IVAR.EQ.0)THEN
C       GET_U_MAT = PM(1,IM)
        DEPLA=1+(IM-1)*NPROPM
        IF (IRESP.EQ.1) THEN
           CALL GET_VAR_USER_F_SP(1,DEPLA,GET_U_MAT)
        ELSE
           CALL GET_VAR_USER_F(1,DEPLA,GET_U_MAT)
        ENDIF
      ELSEIF(IVAR.GT.NVAR)THEN
        WRITE(IOUT,*)
     .      ' **ERROR USER MATERIAL PARAMETER INDEX(',IVAR,
     .      ') EXCEED NUPARAM(',NVAR,')'
        WRITE(ISTDO,*)
     .      ' **ERROR USER MATERIAL PARAMETER INDEX(',IVAR,
     .      ') EXCEED NUPARAM(',NVAR,')'
      ELSE
C       IADBUF = IPM(7,IM)
C       GET_U_MAT = BUFMAT(IADBUF-1+IVAR)
        DEPLA = 7+(IM-1)*NPROPMI
        CALL GET_VAR_USER_I(19,DEPLA,IADBUF)
        DEPLA = IADBUF-1+IVAR
        IF (IRESP.EQ.1) THEN
          CALL GET_VAR_USER_F_SP(2,DEPLA,GET_U_MAT)
        ELSE
          CALL GET_VAR_USER_F(2,DEPLA,GET_U_MAT)
        ENDIF
      ENDIF
C
      RETURN
      END
Cgw029
Chd|====================================================================
Chd|  GET_U_GEO                     source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|        JOINT_BLOCK_STIFFNESS         source/elements/joint/joint_block_stiffness.F
Chd|        RSKEW33                       source/elements/joint/rskew33.F
Chd|        RUSER32                       source/elements/spring/ruser32.F
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|        RUSER35                       source/elements/spring/ruser35.F
Chd|        RUSER36                       source/elements/spring/ruser36.F
Chd|        RUSER44                       source/elements/spring/ruser44.F
Chd|        RUSER46                       source/elements/spring/ruser46.F
Chd|        SPADAH                        source/elements/sph/spadah.F  
Chd|        SPCOMPL                       source/elements/sph/spcompl.F 
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPONOF1                       source/elements/sph/sponof1.F 
Chd|        SPSCOMP                       source/elements/sph/spcompl.F 
Chd|        SPSTABW                       source/elements/sph/spstab.F  
Chd|        XFORC28                       source/elements/xelem/xforc28.F
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|====================================================================
      my_real FUNCTION 
     .      GET_U_GEO(IVAR,IP)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"

      INTEGER IVAR,IP,NVAR,DEPLA
      my_real 
     .      NV,RESULTAT
C
C     GEO(I,J)  == LOCA(ISH3+I+(J-1)*NPROPG)
C     BUFGEO(I) == LOCA(ISH4+I)
C
C
      DEPLA=26+(IP-1)*NPROPG
      IF (IRESP .EQ. 1) THEN
        CALL GET_VAR_USER_F_SP(3,DEPLA,NV)
      ELSE
        CALL GET_VAR_USER_F(3,DEPLA,NV)
      ENDIF
      NVAR = NINT(NV)
C     NVAR = NINT(GEO(26,IP))
      IF(IVAR.GT.NVAR)THEN
        WRITE(IOUT,*)
     .      ' **ERROR USER PROPERTY PARAMETER INDEX(',IVAR,
     .      ') EXCEED NUPARAG(',NVAR,')'
        WRITE(ISTDO,*)
     .      ' **ERROR USER PROPERTY PARAMETER INDEX(',IVAR,
     .      ') EXCEED NUPARAG(',NVAR,')'
      ELSE
        DEPLA=27+(IP-1)*NPROPG
        IF (IRESP .EQ. 1) THEN
          CALL GET_VAR_USER_F_SP(3,DEPLA,NV)
          DEPLA=NINT(NV)-1+IVAR
          CALL GET_VAR_USER_F_SP(4,DEPLA,GET_U_GEO)
        ELSE
          CALL GET_VAR_USER_F(3,DEPLA,NV)
          DEPLA=NINT(NV)-1+IVAR
          CALL GET_VAR_USER_F(4,DEPLA,GET_U_GEO)
        ENDIF
C       GET_U_GEO = BUFGEO(NINT(GEO(27,IP))-1+IVAR)
      ENDIF
C
      RETURN
      END
Cgw029
Chd|====================================================================
Chd|  GET_U_PNU                     source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|        RUSER32                       source/elements/spring/ruser32.F
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|        RUSER35                       source/elements/spring/ruser35.F
Chd|        RUSER36                       source/elements/spring/ruser36.F
Chd|        RUSER44                       source/elements/spring/ruser44.F
Chd|        RUSER46                       source/elements/spring/ruser46.F
Chd|        XFORC28                       source/elements/xelem/xforc28.F
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_PNU(IVAR,IP,K)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"
      INTEGER IVAR,IP,K,DEPLA,NVAR
      my_real
     .      NV
C
C     GEO(I,J)  == LOCA(ISH3+I+(J-1)*NPROPG)
C     BUFGEO(I) == LOCA(ISH4+I)
C
C
C
      DEPLA=K-1+(IP-1)*NPROPG
      IF (IRESP.EQ.1) THEN
        CALL GET_VAR_USER_F_SP(3,DEPLA,NV)
      ELSE
        CALL GET_VAR_USER_F(3,DEPLA,NV)
      ENDIF
      NVAR=NINT(NV)
C     NVAR = NINT(GEO(K-1,IP))
      IF(IVAR.GT.NVAR)THEN
        WRITE(IOUT,*)
     .      ' **ERROR IN FUNCTION GET_U_PNU(INDEX,IP,GEO,BUFGEO,',
     .      K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NVAR,')'
        WRITE(ISTDO,*)
     .      ' **ERROR IN FUNCTION GET_U_PNU(INDEX,IP,GEO,BUFGEO,',
     .      K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NVAR,')'
      ELSE
        DEPLA=K+(IP-1)*NPROPG
        IF (IRESP.EQ.1) THEN
          CALL GET_VAR_USER_F_SP(3,DEPLA,NV)
          DEPLA=NINT(NV)-1+IVAR
          CALL GET_VAR_USER_F_SP(4,DEPLA,NV)
        ELSE
          CALL GET_VAR_USER_F(3,DEPLA,NV)
          DEPLA=NINT(NV)-1+IVAR
          CALL GET_VAR_USER_F(4,DEPLA,NV)
        ENDIF
          GET_U_PNU=NINT(NV)
C       GET_U_PNU = NINT(BUFGEO(NINT(GEO(K,IP))-1+IVAR))
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_MNU                     source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|        RUSER36                       source/elements/spring/ruser36.F
Chd|        GET_U_MNU                     source/user_interface/upidmid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        GET_U_MNU                     source/user_interface/upidmid.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_MNU(IVAR,IM,K)
C------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"
C------------------------------------------
      INTEGER IVAR,IM,K,KFUNC,NVAR,DEPLA,NFUNC
      PARAMETER (KFUNC=29)
C=======================================================================
C     NFUNC = IPM(10,IM))
      DEPLA = 10+(IM-1)*NPROPMI
      CALL GET_VAR_USER_I(19,DEPLA,NFUNC)
C
      IF (IVAR > NFUNC)THEN
        WRITE(IOUT,*)
     .      ' **ERROR IN FUNCTION GET_U_MNU(INDEX,IP,GEO,BUFGEO,',
     .      K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NFUNC,')'
        WRITE(ISTDO,*)
     .      ' **ERROR IN FUNCTION GET_U_MNU(INDEX,IP,GEO,BUFGEO,',
     .      K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NFUNC,')'
      ELSEIF(K.EQ.KFUNC) THEN
C       GET_U_MNU = IFUNC = IPM(10+IVAR,IM))
        DEPLA = 10+IVAR+(IM-1)*NPROPMI
        CALL GET_VAR_USER_I(19,DEPLA,GET_U_MNU)
C
      ENDIF
C
      RETURN
      END
Cgw029
Chd|====================================================================
Chd|  GET_U_PID                     source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|        GET_U_P                       source/user_interface/upidmid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_PID(IP)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"
      INTEGER IP,DEPLA,VAR
C---------------------------------------------
C     GET_U_PID = NINT(GEO(40,IP))
      DEPLA = 1+(IP-1)*NPROPGI
      CALL GET_VAR_USER_I(20,DEPLA,VAR)
      GET_U_PID = VAR
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_MID                     source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|        GET_U_M                       source/user_interface/upidmid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_MID(IM)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"
      INTEGER IM,DEPLA
C-----------------------------------------
C     GET_U_MID =IPM(1,IM)
C---
      DEPLA = 1+(IM-1)*NPROPMI
      CALL GET_VAR_USER_I(19,DEPLA,GET_U_MID)
C---
      RETURN
      END
C=======================================================================
C
C     THESE FUNCTIONS ARE MISSING IN RADIOSS V42b LIBRARY
C     but added in userwi.F
C     
C=======================================================================
Chd|====================================================================
Chd|  GET_U_M                       source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_U_MID                     source/user_interface/upidmid.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_M(MID)
      INTEGER MID,I,GET_U_MID
      EXTERNAL GET_U_MID
#include      "com04_c.inc"
C
      GET_U_M = 0
      DO I=1,NUMMAT
        IF(GET_U_MID(I).EQ.MID)THEN
          GET_U_M = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
C=======================================================================
Chd|====================================================================
Chd|  GET_U_P                       source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_U_PID                     source/user_interface/upidmid.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_P(PID)
      INTEGER PID,I,GET_U_PID
      EXTERNAL GET_U_PID
#include      "com04_c.inc"
C
      GET_U_P = 0
      DO I=1,NUMGEO
        IF(GET_U_PID(I).EQ.PID)THEN
          GET_U_P = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
C
Chd|====================================================================
Chd|  GET_U_PROC                    source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION GET_U_PROC()
#include      "implicit_f.inc"
#include      "task_c.inc"
C=======================================================================
      GET_U_PROC = ISPMD+1
C
      RETURN
      END
C
Chd|====================================================================
Chd|  GET_U_TASK                    source/user_interface/upidmid.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION GET_U_TASK()
#include      "implicit_f.inc"
#include      "scr01_c.inc"
C=======================================================================
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
      GET_U_TASK = OMP_GET_THREAD_NUM()+1
C
      RETURN
      END
