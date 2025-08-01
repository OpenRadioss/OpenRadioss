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
!||    upidmid_dum         ../engine/source/user_interface/upidmid.F
!||--- called by ------------------------------------------------------
!||    radioss2            ../engine/source/engine/radioss2.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      SUBROUTINE UPIDMID_DUM(IERR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER IERR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------       
       IERR=0
      END
      my_real FUNCTION 
     .      GET_U_MAT(IVAR,IM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------     
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IVAR,IM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER NVAR,DEPLA,IADBUF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C     NVAR = NUPARAM = NINT(PM(26,IM)) old
C     NVAR = NUPARAM = IPM(9,IM)
C
      DEPLA= 9+(IM-1)*NPROPMI
      CALL GET_VAR_USER_I(19,DEPLA,NVAR)
      IF(IVAR==0)THEN
C       GET_U_MAT = PM(1,IM)
        DEPLA=1+(IM-1)*NPROPM
        IF (IRESP==1) THEN
           CALL GET_VAR_USER_F_SP(1,DEPLA,GET_U_MAT)
        ELSE
           CALL GET_VAR_USER_F(1,DEPLA,GET_U_MAT)
        ENDIF
      ELSEIF(IVAR>NVAR)THEN
        WRITE(IOUT,*) ' **ERROR USER MATERIAL PARAMETER INDEX(',IVAR,') EXCEED NUPARAM(',NVAR,')'
        WRITE(ISTDO,*)' **ERROR USER MATERIAL PARAMETER INDEX(',IVAR,') EXCEED NUPARAM(',NVAR,')'
      ELSE
C       IADBUF = IPM(7,IM)
C       GET_U_MAT = BUFMAT(IADBUF-1+IVAR)
        DEPLA = 7+(IM-1)*NPROPMI
        CALL GET_VAR_USER_I(19,DEPLA,IADBUF)
        DEPLA = IADBUF-1+IVAR
        IF (IRESP==1) THEN
          CALL GET_VAR_USER_F_SP(2,DEPLA,GET_U_MAT)
        ELSE
          CALL GET_VAR_USER_F(2,DEPLA,GET_U_MAT)
        ENDIF
      ENDIF
C
      RETURN
      END
C  029
      my_real FUNCTION 
     .      GET_U_GEO(IVAR,IP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------     
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IVAR,IP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER NVAR,DEPLA
      my_real NV
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------       
C     GEO(I,J)  == LOCA(ISH3+I+(J-1)*NPROPG)
C     BUFGEO(I) == LOCA(ISH4+I)
      DEPLA=26+(IP-1)*NPROPG
      IF (IRESP == 1) THEN
        CALL GET_VAR_USER_F_SP(3,DEPLA,NV)
      ELSE
        CALL GET_VAR_USER_F(3,DEPLA,NV)
      ENDIF
      NVAR = NINT(NV)
C     NVAR = NINT(GEO(26,IP))
      IF(IVAR>NVAR)THEN
        WRITE(IOUT,*) ' **ERROR USER PROPERTY PARAMETER INDEX(',IVAR,') EXCEED NUPARAG(',NVAR,')'
        WRITE(ISTDO,*)' **ERROR USER PROPERTY PARAMETER INDEX(',IVAR,') EXCEED NUPARAG(',NVAR,')'
      ELSE
        DEPLA=27+(IP-1)*NPROPG
        IF (IRESP == 1) THEN
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

!||====================================================================
!||    get_u_pnu           ../engine/source/user_interface/upidmid.F
!||--- called by ------------------------------------------------------
!||    ruser32             ../engine/source/elements/spring/ruser32.F
!||    ruser33             ../engine/source/elements/joint/ruser33.F
!||    ruser35             ../engine/source/elements/spring/ruser35.F
!||    ruser36             ../engine/source/elements/spring/ruser36.F
!||    ruser44             ../engine/source/elements/spring/ruser44.F
!||    ruser46             ../engine/source/elements/spring/ruser46.F
!||    xanim28             ../engine/source/elements/xelem/xanim28.F
!||    xanim29             ../engine/source/output/anim/generate/xanim29.F
!||    xanim30             ../engine/source/output/anim/generate/xanim30.F
!||    xanim31             ../engine/source/output/anim/generate/xanim31.F
!||    xforc28             ../engine/source/elements/xelem/xforc28.F
!||    xforc29             ../engine/source/elements/xelem/xforc29.F
!||    xforc30             ../engine/source/elements/xelem/xforc30.F
!||    xforc31             ../engine/source/elements/xelem/xforc31.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      INTEGER FUNCTION GET_U_PNU(IVAR,IP,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------      
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IVAR,IP,K
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------     
      INTEGER DEPLA,NVAR 
      my_real NV
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------    
C     GEO(I,J)  == LOCA(ISH3+I+(J-1)*NPROPG)
C     BUFGEO(I) == LOCA(ISH4+I)
C
      GET_U_PNU = 0
      DEPLA=K-1+(IP-1)*NPROPG
      IF (IRESP==1) THEN
        CALL GET_VAR_USER_F_SP(3,DEPLA,NV)
      ELSE
        CALL GET_VAR_USER_F(3,DEPLA,NV)
      ENDIF
      NVAR=NINT(NV)
C     NVAR = NINT(GEO(K-1,IP))
      IF(IVAR>NVAR)THEN
        WRITE(IOUT,*) ' **ERROR IN FUNCTION GET_U_PNU(INDEX,IP,GEO,BUFGEO,',K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NVAR,')'
        WRITE(ISTDO,*)' **ERROR IN FUNCTION GET_U_PNU(INDEX,IP,GEO,BUFGEO,',K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NVAR,')'
      ELSE
        DEPLA=K+(IP-1)*NPROPG
        IF (IRESP==1) THEN
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
!||====================================================================
!||    get_u_mnu        ../engine/source/user_interface/upidmid.F
!||--- called by ------------------------------------------------------
!||    ruser32          ../engine/source/elements/spring/ruser32.F
!||    ruser33          ../engine/source/elements/joint/ruser33.F
!||    ruser35          ../engine/source/elements/spring/ruser35.F
!||    ruser36          ../engine/source/elements/spring/ruser36.F
!||    ruser44          ../engine/source/elements/spring/ruser44.F
!||    ruser46          ../engine/source/elements/spring/ruser46.F
!||    xanim28          ../engine/source/elements/xelem/xanim28.F
!||    xanim29          ../engine/source/output/anim/generate/xanim29.F
!||    xanim30          ../engine/source/output/anim/generate/xanim30.F
!||    xanim31          ../engine/source/output/anim/generate/xanim31.F
!||    xforc28          ../engine/source/elements/xelem/xforc28.F
!||    xforc29          ../engine/source/elements/xelem/xforc29.F
!||    xforc30          ../engine/source/elements/xelem/xforc30.F
!||    xforc31          ../engine/source/elements/xelem/xforc31.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      INTEGER FUNCTION GET_U_MNU(IVAR,IM,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IVAR,IM,K
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER KFUNC,DEPLA,NFUNC
      PARAMETER (KFUNC=29)
C-----------------------------------------------
C   S o u r c e   L i n e s
C----------------------------------------------- 
C     NFUNC = IPM(10,IM))
      DEPLA = 10+(IM-1)*NPROPMI
      CALL GET_VAR_USER_I(19,DEPLA,NFUNC)
C
      IF (IVAR > NFUNC)THEN
        WRITE(IOUT,*) ' **ERROR IN FUNCTION GET_U_MNU(INDEX,IP,GEO,BUFGEO,',K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NFUNC,')'
        WRITE(ISTDO,*)' **ERROR IN FUNCTION GET_U_MNU(INDEX,IP,GEO,BUFGEO,',K,'), INDEX(',IVAR,') EXCEED ALLOCATED VALUE(',NFUNC,')'
      ELSEIF(K==KFUNC) THEN
C       GET_U_MNU = IFUNC = IPM(10+IVAR,IM))
        DEPLA = 10+IVAR+(IM-1)*NPROPMI
        CALL GET_VAR_USER_I(19,DEPLA,GET_U_MNU)
C
      ENDIF
C
      RETURN
      END

!||====================================================================
!||    get_u_pid        ../engine/source/user_interface/upidmid.F
!||--- called by ------------------------------------------------------
!||    get_u_p          ../engine/source/user_interface/upidmid.F
!||    ruser32          ../engine/source/elements/spring/ruser32.F
!||    ruser33          ../engine/source/elements/joint/ruser33.F
!||    ruser35          ../engine/source/elements/spring/ruser35.F
!||    ruser36          ../engine/source/elements/spring/ruser36.F
!||    ruser44          ../engine/source/elements/spring/ruser44.F
!||    ruser46          ../engine/source/elements/spring/ruser46.F
!||    xanim28          ../engine/source/elements/xelem/xanim28.F
!||    xanim29          ../engine/source/output/anim/generate/xanim29.F
!||    xanim30          ../engine/source/output/anim/generate/xanim30.F
!||    xanim31          ../engine/source/output/anim/generate/xanim31.F
!||    xforc28          ../engine/source/elements/xelem/xforc28.F
!||    xforc29          ../engine/source/elements/xelem/xforc29.F
!||    xforc30          ../engine/source/elements/xelem/xforc30.F
!||    xforc31          ../engine/source/elements/xelem/xforc31.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      INTEGER FUNCTION GET_U_PID(IP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------      
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER,INTENT(IN) :: IP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------       
      INTEGER DEPLA,VAR
C-----------------------------------------------
C   S o u r c e  C o d e 
C-----------------------------------------------   
C     GET_U_PID = NINT(GEO(40,IP))
      DEPLA = 1+(IP-1)*NPROPGI
      CALL GET_VAR_USER_I(20,DEPLA,VAR)
      GET_U_PID = VAR
C---
      RETURN
      END
!||====================================================================
!||    get_u_mid        ../engine/source/user_interface/upidmid.F
!||--- called by ------------------------------------------------------
!||    get_u_m          ../engine/source/user_interface/upidmid.F
!||    ruser32          ../engine/source/elements/spring/ruser32.F
!||    ruser33          ../engine/source/elements/joint/ruser33.F
!||    ruser35          ../engine/source/elements/spring/ruser35.F
!||    ruser36          ../engine/source/elements/spring/ruser36.F
!||    ruser44          ../engine/source/elements/spring/ruser44.F
!||    ruser46          ../engine/source/elements/spring/ruser46.F
!||    xanim28          ../engine/source/elements/xelem/xanim28.F
!||    xanim29          ../engine/source/output/anim/generate/xanim29.F
!||    xanim30          ../engine/source/output/anim/generate/xanim30.F
!||    xanim31          ../engine/source/output/anim/generate/xanim31.F
!||    xforc28          ../engine/source/elements/xelem/xforc28.F
!||    xforc29          ../engine/source/elements/xelem/xforc29.F
!||    xforc30          ../engine/source/elements/xelem/xforc30.F
!||    xforc31          ../engine/source/elements/xelem/xforc31.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      INTEGER FUNCTION GET_U_MID(IM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------      
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER DEPLA
C-----------------------------------------
C     GET_U_MID =IPM(1,IM)
C---
      DEPLA = 1+(IM-1)*NPROPMI
      CALL GET_VAR_USER_I(19,DEPLA,GET_U_MID)
C---
      RETURN
      END

!||====================================================================
!||    get_u_m     ../engine/source/user_interface/upidmid.F
!||--- calls      -----------------------------------------------------
!||    get_u_mid   ../engine/source/user_interface/upidmid.F
!||====================================================================
      INTEGER FUNCTION GET_U_M(MID)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: MID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,GET_U_MID
      EXTERNAL GET_U_MID
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      GET_U_M = 0
      DO I=1,NUMMAT
        IF(GET_U_MID(I)==MID)THEN
          GET_U_M = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END

!||====================================================================
!||    get_u_p     ../engine/source/user_interface/upidmid.F
!||--- calls      -----------------------------------------------------
!||    get_u_pid   ../engine/source/user_interface/upidmid.F
!||====================================================================
      INTEGER FUNCTION GET_U_P(PID)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"    
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------    
      INTEGER,INTENT(IN) :: PID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER I,GET_U_PID
      EXTERNAL GET_U_PID
C-----------------------------------------------
C   S o u r c e   L i n e s
C----------------------------------------------- 
      GET_U_P = 0
      DO I=1,NUMGEO
        IF(GET_U_PID(I)==PID)THEN
          GET_U_P = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
C
!||====================================================================
!||    get_u_proc   ../engine/source/user_interface/upidmid.F
!||====================================================================
      INTEGER FUNCTION GET_U_PROC()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      GET_U_PROC = ISPMD+1
C
      RETURN
      END
C
!||====================================================================
!||    get_u_task           ../engine/source/user_interface/upidmid.F
!||--- calls      -----------------------------------------------------
!||    omp_get_thread_num   ../engine/source/engine/openmp_stub.F90
!||====================================================================
      INTEGER FUNCTION GET_U_TASK()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      
      GET_U_TASK = OMP_GET_THREAD_NUM()+1
C
      RETURN
      END
