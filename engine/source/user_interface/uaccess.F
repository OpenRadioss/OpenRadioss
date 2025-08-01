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
!||    uaccess_dum   ../engine/source/user_interface/uaccess.F
!||--- called by ------------------------------------------------------
!||    radioss2      ../engine/source/engine/radioss2.F
!||====================================================================
      SUBROUTINE UACCESS_DUM(IERR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
       INTEGER IERR
       IERR=0
      END
!||====================================================================
!||    get_u_cycle   ../engine/source/user_interface/uaccess.F
!||====================================================================
      INTEGER FUNCTION GET_U_CYCLE()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
C-----------------------------------------------
       GET_U_CYCLE = NCYCLE
       RETURN
       END
#include "my_real.inc"
!||====================================================================
!||    get_u_time   ../engine/source/user_interface/uaccess.F
!||--- called by ------------------------------------------------------
!||    ruser46      ../engine/source/elements/spring/ruser46.F
!||====================================================================
      my_real FUNCTION GET_U_TIME()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com08_c.inc"
C-----------------------------------------------
       GET_U_TIME = TT
       RETURN
       END
!||====================================================================
!||    get_u_accel         ../engine/source/user_interface/uaccess.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      INTEGER FUNCTION GET_U_ACCEL(NACC, AX,AY,AZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NACC
      my_real AX,AY,AZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, L, IACC,D1,D2,D3
C-----------------------------------------------
      IF (NACC<=0) THEN
        AX = ZERO
        AY = ZERO
        AZ = ZERO
        GET_U_ACCEL = -1
      ELSE
        L = (NACC-1)*25
        D1 = L+20
        D2 = L+21
        D3 = L+22
        IF (IRESP == 1) THEN
          CALL GET_VAR_USER_F_SP(12,D1,AX)
          CALL GET_VAR_USER_F_SP(12,D2,AY)
          CALL GET_VAR_USER_F_SP(12,D3,AZ)
        ELSE
          CALL GET_VAR_USER_F(12,D1,AX)
          CALL GET_VAR_USER_F(12,D2,AY)
          CALL GET_VAR_USER_F(12,D3,AZ)
        ENDIF
        GET_U_ACCEL = 0
      ENDIF
C
      RETURN
      END
!||====================================================================
!||    get_u_numacc     ../engine/source/user_interface/uaccess.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      INTEGER FUNCTION GET_U_NUMACC(IDACC)
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
      INTEGER, INTENT(IN) :: IDACC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER DEPLA,J,ID
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------  
      GET_U_NUMACC = 0
      IF(IDACC<=0)THEN
        RETURN
      ENDIF
C---
      DO J=1,NACCELM
        DEPLA = (J-1)*3+1
        CALL GET_VAR_USER_I(5,DEPLA,ID)
        IF(IDACC==ID)THEN
          GET_U_NUMACC = J
          RETURN
        ENDIF
      ENDDO
C---
      RETURN
      END
!||====================================================================
!||    get_u_numnod     ../engine/source/user_interface/uaccess.F
!||--- calls      -----------------------------------------------------
!||    ancmsg           ../engine/source/output/message/message.F
!||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
!||--- uses       -----------------------------------------------------
!||    message_mod      ../engine/share/message_module/message_mod.F
!||====================================================================
      INTEGER FUNCTION GET_U_NUMNOD(IU)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IU
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, JINF, JSUP, ITAB1, ITAB2
      CHARACTER MESS*40
      DATA MESS/'USER SENSOR                             '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      JINF=1
      JSUP=NUMNOD
      J=(JSUP+JINF)/2
   10 CALL GET_VAR_USER_I(13,J,ITAB1)
      IF(JSUP<=JINF.AND.(IU-ITAB1)/=0) THEN
       IF(NSPMD==1) THEN
        CALL ANCMSG(MSGID=186,ANMODE=ANINFO_BLIND,
     .                I1=IU,C1=MESS)
        IERR=IERR+1
       END IF
C   en SPMD la valeur 0 n indique pas une erreur mais l absence du noeud sur le proc
       GET_U_NUMNOD=0
       RETURN
      ENDIF
      IF((IU-ITAB1)==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
       CALL GET_VAR_USER_I(13,J+NUMNOD,ITAB2)
         GET_U_NUMNOD=ITAB2
         RETURN
      ELSE IF (IU-ITAB1<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
C---
      RETURN
      END
C
!||====================================================================
!||    get_u_nod_x         ../engine/source/user_interface/uaccess.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
!||    spmd_glob_dsum      ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_rbcast         ../engine/source/mpi/generic/spmd_rbcast.F
!||====================================================================
      INTEGER FUNCTION GET_U_NOD_X(NOD, X,Y,Z)
C   En SPMD, tous les procs doivent appeler cette fonction
C   sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "scr05_c.inc"
#include "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NOD
      my_real,INTENT(INOUT) :: X,Y,Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,D1,D2,D3, P
      my_real BUFS(6)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
C   get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
C   weight : pointeur 18 (cf resol.F)
         CALL GET_VAR_USER_I(18,NOD,P)
       ELSE
         P = 1
       ENDIF
       IF(P==1) THEN
        L=(NOD-1)*3
        D1=L+1
        D2=L+2
        D3=L+3
        IF (IRESP == 1) THEN
          CALL GET_VAR_USER_F_SP(14,D1,X)
          CALL GET_VAR_USER_F_SP(14,D2,Y)
          CALL GET_VAR_USER_F_SP(14,D3,Z)
        ELSE
          CALL GET_VAR_USER_F(14,D1,X)
          CALL GET_VAR_USER_F(14,D2,Y)
          CALL GET_VAR_USER_F(14,D3,Z)
        ENDIF
       ENDIF
C
C SPMD communication du resultat a ts les procs
C
       IF(NSPMD>1.AND.GETUNOD_NOCOM==0) THEN
         IF(P==1) THEN
           BUFS(1) = X
           BUFS(2) = Y
           BUFS(3) = Z
         ELSE
           BUFS(1) = ZERO
           BUFS(2) = ZERO
           BUFS(3) = ZERO
         ENDIF
         CALL SPMD_GLOB_DSUM(BUFS,3,BUFS(4))
         CALL SPMD_RBCAST(BUFS,BUFS,3,1,0,2)
         X = BUFS(1)
         Y = BUFS(2)
         Z = BUFS(3)
       ENDIF
C
C Fin SPMD
C
       GET_U_NOD_X = 0
       RETURN
      ENDIF
      GET_U_NOD_X = -1
      X = ZERO
      Y = ZERO
      Z = ZERO
C
      RETURN
      END
!||====================================================================
!||    get_u_nod_d         ../engine/source/user_interface/uaccess.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
!||    spmd_glob_dsum      ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_rbcast         ../engine/source/mpi/generic/spmd_rbcast.F
!||====================================================================
      INTEGER FUNCTION GET_U_NOD_D(NOD, DX,DY,DZ)
C   En SPMD, tous les procs doivent appeler cette fonction
C   sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "scr05_c.inc"
#include "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NOD
      my_real,INTENT(INOUT) :: DX,DY,DZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,D1,D2,D3, P
      my_real BUFS(6)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
C   get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
C   weight : pointeur 18 (cf resol.F)
         CALL GET_VAR_USER_I(18,NOD,P)
       ELSE
         P = 1
       ENDIF
       IF(P==1) THEN
        L=(NOD-1)*3
        D1=L+1
        D2=L+2
        D3=L+3
        IF (IRESP == 1) THEN
          CALL GET_VAR_USER_F_SP(15,D1,DX)
          CALL GET_VAR_USER_F_SP(15,D2,DY)
          CALL GET_VAR_USER_F_SP(15,D3,DZ)
        ELSE
          CALL GET_VAR_USER_F(15,D1,DX)
          CALL GET_VAR_USER_F(15,D2,DY)
          CALL GET_VAR_USER_F(15,D3,DZ)
        ENDIF
       ENDIF
C
C SPMD communication du resultat a ts les procs
C
       IF(NSPMD>1.AND.GETUNOD_NOCOM==0) THEN
         IF(P==1) THEN
           BUFS(1) = DX
           BUFS(2) = DY
           BUFS(3) = DZ
         ELSE
           BUFS(1) = ZERO
           BUFS(2) = ZERO
           BUFS(3) = ZERO
         ENDIF
         CALL SPMD_GLOB_DSUM(BUFS,3,BUFS(4))
         CALL SPMD_RBCAST(BUFS,BUFS,3,1,0,2)
         DX = BUFS(1)
         DY = BUFS(2)
         DZ = BUFS(3)
       ENDIF
C
C Fin SPMD
C
       GET_U_NOD_D= 0
       RETURN
      ENDIF
      GET_U_NOD_D= -1
      DX = ZERO
      DY = ZERO
      DZ = ZERO
C
      RETURN
      END
!||====================================================================
!||    get_u_nod_v         ../engine/source/user_interface/uaccess.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
!||    spmd_glob_dsum      ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_rbcast         ../engine/source/mpi/generic/spmd_rbcast.F
!||====================================================================
      INTEGER FUNCTION GET_U_NOD_V(NOD, VX,VY,VZ)
C   En SPMD, tous les procs doivent appeler cette fonction
C   sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "scr05_c.inc"
#include "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD
      my_real
     .        VX,VY,VZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,D1,D2,D3, P
      my_real
     .        BUFS(6)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
C   get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
C   weight : pointeur 18 (cf resol.F)
         CALL GET_VAR_USER_I(18,NOD,P)
       ELSE
         P = 1
       ENDIF
       IF(P==1) THEN
        L=(NOD-1)*3
        D1=L+1
        D2=L+2
        D3=L+3
        IF (IRESP == 1) THEN
          CALL GET_VAR_USER_F_SP(16,D1,VX)
          CALL GET_VAR_USER_F_SP(16,D2,VY)
          CALL GET_VAR_USER_F_SP(16,D3,VZ)
        ELSE
          CALL GET_VAR_USER_F(16,D1,VX)
          CALL GET_VAR_USER_F(16,D2,VY)
          CALL GET_VAR_USER_F(16,D3,VZ)
        ENDIF
C
C SPMD communication du resultat a ts les procs
C
       ENDIF
       IF(NSPMD>1.AND.GETUNOD_NOCOM==0) THEN
         IF(P==1) THEN
           BUFS(1) = VX
           BUFS(2) = VY
           BUFS(3) = VZ
         ELSE
           BUFS(1) = ZERO
           BUFS(2) = ZERO
           BUFS(3) = ZERO
         ENDIF
         CALL SPMD_GLOB_DSUM(BUFS,3,BUFS(4))
         CALL SPMD_RBCAST(BUFS,BUFS,3,1,0,2)
         VX = BUFS(1)
         VY = BUFS(2)
         VZ = BUFS(3)
       ENDIF
C
C Fin SPMD
C
       GET_U_NOD_V= 0
       RETURN
      ENDIF
      GET_U_NOD_V= -1
      VX = ZERO
      VY = ZERO
      VZ = ZERO
C
      RETURN
      END
!||====================================================================
!||    get_u_nod_a         ../engine/source/user_interface/uaccess.F
!||--- calls      -----------------------------------------------------
!||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
!||    spmd_glob_dsum      ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_rbcast         ../engine/source/mpi/generic/spmd_rbcast.F
!||====================================================================
      INTEGER FUNCTION GET_U_NOD_A(NOD, AX,AY,AZ)
C   En SPMD, tous les procs doivent appeler cette fonction
C   sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "scr05_c.inc"
#include "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD
      my_real AX,AY,AZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,D1,D2,D3, P
      my_real BUFS(6)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
C   get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
C   weight : pointeur 18 (cf resol.F)
         CALL GET_VAR_USER_I(18,NOD,P)
       ELSE
         P = 1
       ENDIF
       IF(P==1) THEN
        L=(NOD-1)*3
        D1=L+1
        D2=L+2
        D3=L+3
        IF (IRESP == 1) THEN
          CALL GET_VAR_USER_F_SP(17,D1,AX)
          CALL GET_VAR_USER_F_SP(17,D2,AY)
          CALL GET_VAR_USER_F_SP(17,D3,AZ)
        ELSE
          CALL GET_VAR_USER_F(17,D1,AX)
          CALL GET_VAR_USER_F(17,D2,AY)
          CALL GET_VAR_USER_F(17,D3,AZ)
        ENDIF
       ENDIF
C
C SPMD communication du resultat a ts les procs
C
       IF(NSPMD>1.AND.GETUNOD_NOCOM==0) THEN
         IF(P==1) THEN
           BUFS(1) = AX
           BUFS(2) = AY
           BUFS(3) = AZ
         ELSE
           BUFS(1) = ZERO
           BUFS(2) = ZERO
           BUFS(3) = ZERO
         ENDIF
         CALL SPMD_GLOB_DSUM(BUFS,3,BUFS(4))
         CALL SPMD_RBCAST(BUFS,BUFS,3,1,0,2)
         AX = BUFS(1)
         AY = BUFS(2)
         AZ = BUFS(3)
       ENDIF
C
C Fin SPMD
C
       GET_U_NOD_A= 0
       RETURN
      ENDIF
      GET_U_NOD_A= -1
      AX = ZERO
      AY = ZERO
      AZ = ZERO
C
      RETURN
      END

!||====================================================================
!||    get_u_skew            ../engine/source/user_interface/uaccess.F
!||--- called by ------------------------------------------------------
!||    rskew33               ../engine/source/elements/joint/rskew33.F
!||--- calls      -----------------------------------------------------
!||    get_array_user_f      ../engine/source/user_interface/eng_callback_c.c
!||    get_array_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
!||    get_var_user_i        ../engine/source/user_interface/eng_callback_c.c
!||====================================================================
      INTEGER FUNCTION GET_U_SKEW(IDSKW,N1,N2,N3,VECT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "r4r8_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN) :: VECT(LSKEW)
      INTEGER,INTENT(IN) :: IDSKW,N1,N2,N3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER I,ID,DEPLA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------   
C
      GET_U_SKEW = 0
C
      DO I=1,NUMSKW
        DEPLA = 4+I*LISKN
        CALL GET_VAR_USER_I(9,DEPLA,ID)
C
        IF(ID==IDSKW) THEN
          GET_U_SKEW = I
C
          CALL GET_VAR_USER_I(9,I+1  ,N1)
          CALL GET_VAR_USER_I(9,I+2*2,N2)
          CALL GET_VAR_USER_I(9,I+3*3,N3)
C
          DEPLA = I*LSKEW+1
          IF (IRESP==1) THEN
            CALL get_array_user_f_sp (10,DEPLA,VECT,LSKEW)
          ELSE
            CALL get_array_user_f (10,DEPLA,VECT,LSKEW)
          ENDIF
          RETURN
C
        ENDIF
C
      ENDDO
      RETURN
      END
      my_real
     .   FUNCTION GET_U_UVAR(IEL,ILAYER,IVAR,NUVAR)
C---------+---------+---+---+--------------------------------------------
C This routine is called by SIGEPS29, SIGEPS30, SIGEPS31 ...
C Gives access to user variables for all layers of the element
C---------+---------+---+---+--------------------------------------------
C
C VAR     | SIZE    |TYP|   | DEFINITION
C---------+---------+---+---+--------------------------------------------
C IEL     |  1      | I |   | ELEMENT NUMBER
C ILAYER  |  1      | I |   | LAYER NUMBER
C IVAR    |  1      | I |   | USER VARIABLE NUMBER
C NUVAR   |  1      | I |   | NUMBER OF USER VARIABLES
C---------+---------+---+---+--------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "usrplas_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEL,ILAYER,IVAR,NUVAR
C-----------------------------------------------      
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N
C-----------------------------------------------      
C   S o u r c e   L i n e s
C-----------------------------------------------
      N = (ILAYER-1)*NUVAR+IVAR
      IF (N > 5000) THEN
        N = 5000
        WRITE(IOUT,*) 'USER VARIABLE ACCESS ERROR : BUFFER OVERFLOW'
      ENDIF
      GET_U_UVAR = UUVAR(IEL,N)
C
      RETURN
      END
!||====================================================================
!||    set_spring_elnum   ../engine/source/user_interface/uaccess.F
!||--- called by ------------------------------------------------------
!||    rforc3             ../engine/source/elements/spring/rforc3.F
!||====================================================================
      SUBROUTINE SET_SPRING_ELNUM(JFT,JLT,IXR)
C---------+---------+---+---+--------------------------------------------
C Saves external spring number for local element group
C---------+---------+---+---+--------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vec_spring_num.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT
      INTEGER IXR(NIXR,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID
C-----------------------------------------------      
C   S o u r c e   L i n e s
C-----------------------------------------------
       DO I=JFT,JLT
         SPR_NUM(I) = IXR(NIXR,I)
       ENDDO
      RETURN
      END
      INTEGER
     .   FUNCTION GET_SPRING_ELNUM(IEL)
C---------+---------+---+---+--------------------------------------------
C This routine is called by SIGEPS29, SIGEPS30, SIGEPS31 ...
C Gives external element number
C---------+---------+---+---+--------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vec_spring_num.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEL,NUM
C-----------------------------------------------      
C   S o u r c e   L i n e s
C-----------------------------------------------
      NUM = NINT(SPR_NUM(IEL))
      GET_SPRING_ELNUM = NUM
      RETURN
      END
!||====================================================================
!||    mat_solid_get_nod_x   ../engine/source/user_interface/uaccess.F
!||--- uses       -----------------------------------------------------
!||    restmod               ../engine/share/modules/restart_mod.F
!||    user_interface_mod    ../engine/source/modules/user_interface_mod.F90
!||====================================================================
      SUBROUTINE MAT_SOLID_GET_NOD_X(USER_X)
C---------+---------+---+---+--------------------------------------------
      USE RESTMOD 
      USE user_interface_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real USER_X(LLT,8,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ND1,ND2,ND3,ND4,ND5,ND6,ND7,ND8,ELEM,I
C-----------------------------------------------      
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,LLT
         ELEM=NFT+I
         ND1=IXS(NIXS*(ELEM-1)+2)
         ND2=IXS(NIXS*(ELEM-1)+3)
         ND3=IXS(NIXS*(ELEM-1)+4)
         ND4=IXS(NIXS*(ELEM-1)+5)
         ND5=IXS(NIXS*(ELEM-1)+6)
         ND6=IXS(NIXS*(ELEM-1)+7)
         ND7=IXS(NIXS*(ELEM-1)+8)
         ND8=IXS(NIXS*(ELEM-1)+9)
C         
         IF(ND1 > 0)THEN 
            USER_X(I,1,1)=user_interface_nodes%X(1,ND1)
            USER_X(I,1,2)=user_interface_nodes%X(2,ND1)
            USER_X(I,1,3)=user_interface_nodes%X(3,ND1)
         ELSE
            USER_X(I,1,1)=ZERO
            USER_X(I,1,2)=ZERO
            USER_X(I,1,3)=ZERO
         ENDIF  
C
         IF(ND2 > 0)THEN 
            USER_X(I,2,1)=user_interface_nodes%X(1,ND2)
            USER_X(I,2,2)=user_interface_nodes%X(2,ND2)
            USER_X(I,2,3)=user_interface_nodes%X(3,ND2)
         ELSE
            USER_X(I,2,1)=ZERO
            USER_X(I,2,2)=ZERO
            USER_X(I,2,3)=ZERO
         ENDIF  
C
         IF(ND3 > 0)THEN 
            USER_X(I,3,1)=user_interface_nodes%X(1,ND3)
            USER_X(I,3,2)=user_interface_nodes%X(2,ND3)
            USER_X(I,3,3)=user_interface_nodes%X(3,ND3)
         ELSE
            USER_X(I,3,1)=ZERO
            USER_X(I,3,2)=ZERO
            USER_X(I,3,3)=ZERO
         ENDIF   
C
         IF(ND4 > 0)THEN 
            USER_X(I,4,1)=user_interface_nodes%X(1,ND4)
            USER_X(I,4,2)=user_interface_nodes%X(2,ND4)
            USER_X(I,4,3)=user_interface_nodes%X(3,ND4)
         ELSE
            USER_X(I,4,1)=ZERO
            USER_X(I,4,2)=ZERO
            USER_X(I,4,3)=ZERO
         ENDIF   
C
         IF(ND5 > 0)THEN 
            USER_X(I,5,1)=user_interface_nodes%X(1,ND5)
            USER_X(I,5,2)=user_interface_nodes%X(2,ND5)
            USER_X(I,5,3)=user_interface_nodes%X(3,ND5)
         ELSE
            USER_X(I,5,1)=ZERO
            USER_X(I,5,2)=ZERO
            USER_X(I,5,3)=ZERO
         ENDIF  
C
         IF(ND6 > 0)THEN 
            USER_X(I,6,1)=user_interface_nodes%X(1,ND6)
            USER_X(I,6,2)=user_interface_nodes%X(2,ND6)
            USER_X(I,6,3)=user_interface_nodes%X(3,ND6)
         ELSE
            USER_X(I,6,1)=ZERO
            USER_X(I,6,2)=ZERO
            USER_X(I,6,3)=ZERO
         ENDIF  
C
         IF(ND7 > 0)THEN 
            USER_X(I,7,1)=user_interface_nodes%X(1,ND7)
            USER_X(I,7,2)=user_interface_nodes%X(2,ND7)
            USER_X(I,7,3)=user_interface_nodes%X(3,ND7)
         ELSE
            USER_X(I,7,1)=ZERO
            USER_X(I,7,2)=ZERO
            USER_X(I,7,3)=ZERO
         ENDIF   
C
         IF(ND8 > 0)THEN 
            USER_X(I,8,1)=user_interface_nodes%X(1,ND8)
            USER_X(I,8,2)=user_interface_nodes%X(2,ND8)
            USER_X(I,8,3)=user_interface_nodes%X(3,ND8)
         ELSE
            USER_X(I,8,1)=ZERO
            USER_X(I,8,2)=ZERO
            USER_X(I,8,3)=ZERO
         ENDIF 

      ENDDO
C     

      END
C-----------------------------------------------
!||====================================================================
!||    mat_solid_get_nod_v   ../engine/source/user_interface/uaccess.F
!||--- uses       -----------------------------------------------------
!||    restmod               ../engine/share/modules/restart_mod.F
!||    user_interface_mod    ../engine/source/modules/user_interface_mod.F90
!||====================================================================
      SUBROUTINE MAT_SOLID_GET_NOD_V(USER_V)
C---------+---------+---+---+--------------------------------------------
      USE RESTMOD
      USE user_interface_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real USER_V(LLT,8,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ND1,ND2,ND3,ND4,ND5,ND6,ND7,ND8,ELEM,I
C-----------------------------------------------      
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,LLT
         ELEM=NFT+I
         ND1=IXS(NIXS*(ELEM-1)+2)
         ND2=IXS(NIXS*(ELEM-1)+3)
         ND3=IXS(NIXS*(ELEM-1)+4)
         ND4=IXS(NIXS*(ELEM-1)+5)
         ND5=IXS(NIXS*(ELEM-1)+6)
         ND6=IXS(NIXS*(ELEM-1)+7)
         ND7=IXS(NIXS*(ELEM-1)+8)
         ND8=IXS(NIXS*(ELEM-1)+9)
C         
         IF(ND1 > 0)THEN 
            USER_V(I,1,1)=user_interface_nodes%V(1,ND1)
            USER_V(I,1,2)=user_interface_nodes%V(2,ND1)
            USER_V(I,1,3)=user_interface_nodes%V(3,ND1)
         ELSE
            USER_V(I,1,1)=ZERO
            USER_V(I,1,2)=ZERO
            USER_V(I,1,3)=ZERO
         ENDIF  
C
         IF(ND2 > 0)THEN 
            USER_V(I,2,1)=user_interface_nodes%V(1,ND2)
            USER_V(I,2,2)=user_interface_nodes%V(2,ND2)
            USER_V(I,2,3)=user_interface_nodes%V(3,ND2)
         ELSE
            USER_V(I,2,1)=ZERO
            USER_V(I,2,2)=ZERO
            USER_V(I,2,3)=ZERO
         ENDIF  
C
         IF(ND3 > 0)THEN 
            USER_V(I,3,1)=user_interface_nodes%V(1,ND3)
            USER_V(I,3,2)=user_interface_nodes%V(2,ND3)
            USER_V(I,3,3)=user_interface_nodes%V(3,ND3)
         ELSE
            USER_V(I,3,1)=ZERO
            USER_V(I,3,2)=ZERO
            USER_V(I,3,3)=ZERO
         ENDIF   
C
         IF(ND4 > 0)THEN 
            USER_V(I,4,1)=user_interface_nodes%V(1,ND4)
            USER_V(I,4,2)=user_interface_nodes%V(2,ND4)
            USER_V(I,4,3)=user_interface_nodes%V(3,ND4)
         ELSE
            USER_V(I,4,1)=ZERO
            USER_V(I,4,2)=ZERO
            USER_V(I,4,3)=ZERO
         ENDIF   
C
         IF(ND5 > 0)THEN 
            USER_V(I,5,1)=user_interface_nodes%V(1,ND5)
            USER_V(I,5,2)=user_interface_nodes%V(2,ND5)
            USER_V(I,5,3)=user_interface_nodes%V(3,ND5)
         ELSE
            USER_V(I,5,1)=ZERO
            USER_V(I,5,2)=ZERO
            USER_V(I,5,3)=ZERO
         ENDIF  
C
         IF(ND6 > 0)THEN 
            USER_V(I,6,1)=user_interface_nodes%V(1,ND6)
            USER_V(I,6,2)=user_interface_nodes%V(2,ND6)
            USER_V(I,6,3)=user_interface_nodes%V(3,ND6)
         ELSE
            USER_V(I,6,1)=ZERO
            USER_V(I,6,2)=ZERO
            USER_V(I,6,3)=ZERO
         ENDIF  
C
         IF(ND7 > 0)THEN 
            USER_V(I,7,1)=user_interface_nodes%V(1,ND7)
            USER_V(I,7,2)=user_interface_nodes%V(2,ND7)
            USER_V(I,7,3)=user_interface_nodes%V(3,ND7)
         ELSE
            USER_V(I,7,1)=ZERO
            USER_V(I,7,2)=ZERO
            USER_V(I,7,3)=ZERO
         ENDIF   
C
         IF(ND8 > 0)THEN 
            USER_V(I,8,1)=user_interface_nodes%V(1,ND8)
            USER_V(I,8,2)=user_interface_nodes%V(2,ND8)
            USER_V(I,8,3)=user_interface_nodes%V(3,ND8)
         ELSE
            USER_V(I,8,1)=ZERO
            USER_V(I,8,2)=ZERO
            USER_V(I,8,3)=ZERO
         ENDIF 

      ENDDO

      END
C-----------------------------------------------
