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
Chd|  UACCESS_DUM                   source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UACCESS_DUM(IERR)
C---------+---------+---+---+--------------------------------------------
#include      "implicit_f.inc"
       INTEGER IERR
       IERR=0
      END
Chd|====================================================================
Chd|  GET_U_CYCLE                   source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION GET_U_CYCLE()
C-----------------------------------------------
#include "com01_c.inc"
C-----------------------------------------------
       GET_U_CYCLE = NCYCLE
       RETURN
       END
#include "my_real.inc"
Chd|====================================================================
Chd|  GET_U_TIME                    source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|        RUSER46                       source/elements/spring/ruser46.F
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION GET_U_TIME()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "com08_c.inc"
C-----------------------------------------------
       GET_U_TIME = TT
       RETURN
       END
Chd|====================================================================
Chd|  GET_U_ACCEL                   source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_ACCEL(NACC, AX,AY,AZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
#include      "implicit_f.inc"
#include      "my_real.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com04_c.inc"
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
Chd|====================================================================
Chd|  GET_U_NUMACC                  source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_NUMACC(IDACC)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
      INTEGER IDACC
      INTEGER DEPLA,J,ID
C=======================================================================
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
Chd|====================================================================
Chd|  GET_U_NUMNOD                  source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_NUMNOD(IU)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, JINF, JSUP, ITAB1, ITAB2
      CHARACTER MESS*40
      DATA MESS/'USER SENSOR                             '/
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
Cel en SPMD la valeur 0 n indique pas une erreur mais l absence du noeud sur le proc
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
Chd|====================================================================
Chd|  GET_U_NOD_X                   source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        SPMD_GLOB_DSUM                source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_NOD_X(NOD, X,Y,Z)
Cel En SPMD, tous les procs doivent appeler cette fonction
Cel sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_real.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "scr05_c.inc"
#include "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD
      my_real
     .        X,Y,Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,D1,D2,D3, P
      my_real
     .        BUFS(6)
C-----------------------------------------------
C
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
Cel get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
Cel weight : pointeur 18 (cf resol.F)
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
Chd|====================================================================
Chd|  GET_U_NOD_D                   source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        SPMD_GLOB_DSUM                source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_NOD_D(NOD, DX,DY,DZ)
Cel En SPMD, tous les procs doivent appeler cette fonction
Cel sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_real.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "scr05_c.inc"
#include "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD
      my_real
     .        DX,DY,DZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,D1,D2,D3, P
      my_real
     .        BUFS(6)
C-----------------------------------------------
C
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
Cel get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
Cel weight : pointeur 18 (cf resol.F)
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
Chd|====================================================================
Chd|  GET_U_NOD_V                   source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        SPMD_GLOB_DSUM                source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_NOD_V(NOD, VX,VY,VZ)
Cel En SPMD, tous les procs doivent appeler cette fonction
Cel sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_real.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
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
C
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
Cel get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
Cel weight : pointeur 18 (cf resol.F)
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
Chd|====================================================================
Chd|  GET_U_NOD_A                   source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        SPMD_GLOB_DSUM                source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_NOD_A(NOD, AX,AY,AZ)
Cel En SPMD, tous les procs doivent appeler cette fonction
Cel sinon blocage
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_real.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
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
      my_real
     .        BUFS(6)
C-----------------------------------------------
C
      IF (NOD>0) THEN
       IF(NSPMD>1) THEN
Cel get_proc_user_f retourne 1 si weight(nod) = 1, , 0 sinon
Cel weight : pointeur 18 (cf resol.F)
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
       IF(IMACH==3.AND.NSPMD>1.AND.GETUNOD_NOCOM==0) THEN
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

Chd|====================================================================
Chd|  GET_U_SKEW                    source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|        RSKEW33                       source/elements/joint/rskew33.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_SKEW(IDSKW,N1,N2,N3,VECT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "r4r8_p.inc"
#include      "param_c.inc"
      my_real
     .       VECT(LSKEW)
      INTEGER IDSKW,N1,N2,N3
      INTEGER I,ID,DEPLA
C=======================================================================
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
Chd|====================================================================
Chd|  GET_U_UVAR                    source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
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
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N
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
Chd|====================================================================
Chd|  SET_SPRING_ELNUM              source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
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
#include      "param_c.inc"
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
       DO I=JFT,JLT
         SPR_NUM(I) = IXR(NIXR,I)
       ENDDO
      RETURN
      END
      INTEGER
Chd|====================================================================
Chd|  GET_SPRING_ELNUM              source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
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
      NUM = NINT(SPR_NUM(IEL))
      GET_SPRING_ELNUM = NUM
      RETURN
      END
Chd|====================================================================
Chd|  MAT_SOLID_GET_NOD_X           source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|====================================================================
      SUBROUTINE MAT_SOLID_GET_NOD_X(USER_X)
C---------+---------+---+---+--------------------------------------------
      USE RESTMOD
C-----------------------------------------------
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real USER_X(LLT,8,3)
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ND1,ND2,ND3,ND4,ND5,ND6,ND7,ND8,ELEM,I
      
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
            USER_X(I,1,1)=X(3*(ND1-1)+1)
            USER_X(I,1,2)=X(3*(ND1-1)+2)
            USER_X(I,1,3)=X(3*(ND1-1)+3)
         ELSE
            USER_X(I,1,1)=ZERO
            USER_X(I,1,2)=ZERO
            USER_X(I,1,3)=ZERO
         ENDIF  
C
         IF(ND2 > 0)THEN 
            USER_X(I,2,1)=X(3*(ND2-1)+1)
            USER_X(I,2,2)=X(3*(ND2-1)+2)
            USER_X(I,2,3)=X(3*(ND2-1)+3)
         ELSE
            USER_X(I,2,1)=ZERO
            USER_X(I,2,2)=ZERO
            USER_X(I,2,3)=ZERO
         ENDIF  
C
         IF(ND3 > 0)THEN 
            USER_X(I,3,1)=X(3*(ND3-1)+1)
            USER_X(I,3,2)=X(3*(ND3-1)+2)
            USER_X(I,3,3)=X(3*(ND3-1)+3)
         ELSE
            USER_X(I,3,1)=ZERO
            USER_X(I,3,2)=ZERO
            USER_X(I,3,3)=ZERO
         ENDIF   
C
         IF(ND4 > 0)THEN 
            USER_X(I,4,1)=X(3*(ND4-1)+1)
            USER_X(I,4,2)=X(3*(ND4-1)+2)
            USER_X(I,4,3)=X(3*(ND4-1)+3)
         ELSE
            USER_X(I,4,1)=ZERO
            USER_X(I,4,2)=ZERO
            USER_X(I,4,3)=ZERO
         ENDIF   
C
         IF(ND5 > 0)THEN 
            USER_X(I,5,1)=X(3*(ND5-1)+1)
            USER_X(I,5,2)=X(3*(ND5-1)+2)
            USER_X(I,5,3)=X(3*(ND5-1)+3)
         ELSE
            USER_X(I,5,1)=ZERO
            USER_X(I,5,2)=ZERO
            USER_X(I,5,3)=ZERO
         ENDIF  
C
         IF(ND6 > 0)THEN 
            USER_X(I,6,1)=X(3*(ND6-1)+1)
            USER_X(I,6,2)=X(3*(ND6-1)+2)
            USER_X(I,6,3)=X(3*(ND6-1)+3)
         ELSE
            USER_X(I,6,1)=ZERO
            USER_X(I,6,2)=ZERO
            USER_X(I,6,3)=ZERO
         ENDIF  
C
         IF(ND7 > 0)THEN 
            USER_X(I,7,1)=X(3*(ND7-1)+1)
            USER_X(I,7,2)=X(3*(ND7-1)+2)
            USER_X(I,7,3)=X(3*(ND7-1)+3)
         ELSE
            USER_X(I,7,1)=ZERO
            USER_X(I,7,2)=ZERO
            USER_X(I,7,3)=ZERO
         ENDIF   
C
         IF(ND8 > 0)THEN 
            USER_X(I,8,1)=X(3*(ND8-1)+1)
            USER_X(I,8,2)=X(3*(ND8-1)+2)
            USER_X(I,8,3)=X(3*(ND8-1)+3)
         ELSE
            USER_X(I,8,1)=ZERO
            USER_X(I,8,2)=ZERO
            USER_X(I,8,3)=ZERO
         ENDIF 

      ENDDO
C     

      END
C-----------------------------------------------
Chd|====================================================================
Chd|  MAT_SOLID_GET_NOD_V           source/user_interface/uaccess.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|====================================================================
      SUBROUTINE MAT_SOLID_GET_NOD_V(USER_V)
C---------+---------+---+---+--------------------------------------------
      USE RESTMOD
C-----------------------------------------------
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real USER_V(LLT,8,3)
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ND1,ND2,ND3,ND4,ND5,ND6,ND7,ND8,ELEM,I
      
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
            USER_V(I,1,1)=V(3*(ND1-1)+1)
            USER_V(I,1,2)=V(3*(ND1-1)+2)
            USER_V(I,1,3)=V(3*(ND1-1)+3)
         ELSE
            USER_V(I,1,1)=ZERO
            USER_V(I,1,2)=ZERO
            USER_V(I,1,3)=ZERO
         ENDIF  
C
         IF(ND2 > 0)THEN 
            USER_V(I,2,1)=V(3*(ND2-1)+1)
            USER_V(I,2,2)=V(3*(ND2-1)+2)
            USER_V(I,2,3)=V(3*(ND2-1)+3)
         ELSE
            USER_V(I,2,1)=ZERO
            USER_V(I,2,2)=ZERO
            USER_V(I,2,3)=ZERO
         ENDIF  
C
         IF(ND3 > 0)THEN 
            USER_V(I,3,1)=V(3*(ND3-1)+1)
            USER_V(I,3,2)=V(3*(ND3-1)+2)
            USER_V(I,3,3)=V(3*(ND3-1)+3)
         ELSE
            USER_V(I,3,1)=ZERO
            USER_V(I,3,2)=ZERO
            USER_V(I,3,3)=ZERO
         ENDIF   
C
         IF(ND4 > 0)THEN 
            USER_V(I,4,1)=V(3*(ND4-1)+1)
            USER_V(I,4,2)=V(3*(ND4-1)+2)
            USER_V(I,4,3)=V(3*(ND4-1)+3)
         ELSE
            USER_V(I,4,1)=ZERO
            USER_V(I,4,2)=ZERO
            USER_V(I,4,3)=ZERO
         ENDIF   
C
         IF(ND5 > 0)THEN 
            USER_V(I,5,1)=V(3*(ND5-1)+1)
            USER_V(I,5,2)=V(3*(ND5-1)+2)
            USER_V(I,5,3)=V(3*(ND5-1)+3)
         ELSE
            USER_V(I,5,1)=ZERO
            USER_V(I,5,2)=ZERO
            USER_V(I,5,3)=ZERO
         ENDIF  
C
         IF(ND6 > 0)THEN 
            USER_V(I,6,1)=V(3*(ND6-1)+1)
            USER_V(I,6,2)=V(3*(ND6-1)+2)
            USER_V(I,6,3)=V(3*(ND6-1)+3)
         ELSE
            USER_V(I,6,1)=ZERO
            USER_V(I,6,2)=ZERO
            USER_V(I,6,3)=ZERO
         ENDIF  
C
         IF(ND7 > 0)THEN 
            USER_V(I,7,1)=V(3*(ND7-1)+1)
            USER_V(I,7,2)=V(3*(ND7-1)+2)
            USER_V(I,7,3)=V(3*(ND7-1)+3)
         ELSE
            USER_V(I,7,1)=ZERO
            USER_V(I,7,2)=ZERO
            USER_V(I,7,3)=ZERO
         ENDIF   
C
         IF(ND8 > 0)THEN 
            USER_V(I,8,1)=V(3*(ND8-1)+1)
            USER_V(I,8,2)=V(3*(ND8-1)+2)
            USER_V(I,8,3)=V(3*(ND8-1)+3)
         ELSE
            USER_V(I,8,1)=ZERO
            USER_V(I,8,2)=ZERO
            USER_V(I,8,3)=ZERO
         ENDIF 

      ENDDO
C     

      END
C-----------------------------------------------
