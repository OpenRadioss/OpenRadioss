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
Chd|  ERR_THK_MOD                   source/elements/shell/err_thk.F
Chd|-- called by -----------
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ERR_THK_MOD
#include "my_real.inc"

        my_real , 
     .    DIMENSION(:), ALLOCATABLE :: 
     .      THICK_SH4, THICK_SH3,
     .      THICK_NOD, AREA_NOD
        my_real , 
     .    DIMENSION(:), ALLOCATABLE :: 
     .      AREA_SH4, AREA_SH3

      END MODULE ERR_THK_MOD
Chd|====================================================================
Chd|  ERR_THK                       source/elements/shell/err_thk.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ADMTHKE                       source/model/remesh/admthke.F 
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_NODAREA             source/mpi/anim/spmd_exch_nodarea.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        ERR_THK_MOD                   source/elements/shell/err_thk.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ERR_THK(
     .             IXC     ,IXTG    ,IPARG   ,IAD_ELEM,FR_ELEM ,
     .             WEIGHT  ,X       ,ELBUF_TAB,IPART   ,IPARTC  ,
     .             IPARTTG ,ITASK   ,NODFT   ,NODLT   ,ERR_THK_SH4,
     .             ERR_THK_SH3,SH4TREE,SH3TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ERR_THK_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER  
     .   IXC(NIXC,*), IXTG(NIXTG,*),IPARG(NPARG,*),
     .   IAD_ELEM(2,*), FR_ELEM(*), WEIGHT(*), 
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
      INTEGER ITASK, NODFT, NODLT
      my_real
     .   X(3,*), ERR_THK_SH4(*), ERR_THK_SH3(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SH4FT, SH4LT, SH3FT, SH3LT, IERROR, MLW
      INTEGER N1,N2,N3,N4,
     .        I,N,NG,NEL,LENR,PRT,IADM
C     REAL
       my_real
     .     AREA, A, AT, THK, 
     .     X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .     X31,Y31,Z31,X42,Y42,Z42,X32,Y32,Z32,E3X,E3Y,E3Z,
     .     TN1,TN2,TN3,TN4,TPG1,TPG2,TPG3,TPG4,UNT
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C-----------------------------------------------
      IF(ITASK==0)THEN
        ALLOCATE(
     .   AREA_SH4(NUMELC), AREA_SH3(NUMELTG), AREA_NOD(NUMNOD),
     .   THICK_SH4(NUMELC), THICK_SH3(NUMELTG), THICK_NOD(NUMNOD),
     .   STAT=IERROR)
C
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=71,ANMODE=ANINFO_BLIND,
     .                I1=IERROR)
          CALL ARRET(2)
        END IF
      END IF
C
      CALL MY_BARRIER
C
      SH4FT = 1+ITASK*NUMELC/ NTHREAD
      SH4LT = (ITASK+1)*NUMELC/NTHREAD
      SH3FT = 1+ITASK*NUMELTG/ NTHREAD
      SH3LT = (ITASK+1)*NUMELTG/NTHREAD
C
      AREA_SH4(SH4FT:SH4LT)=ZERO
      AREA_SH3(SH3FT:SH3LT)=ZERO
C
      ERR_THK_SH4(SH4FT:SH4LT)=ZERO
      ERR_THK_SH3(SH3FT:SH3LT)=ZERO
C
      AREA_NOD(NODFT:NODLT)=ZERO
      THICK_NOD(NODFT:NODLT)=ZERO
C
      CALL MY_BARRIER
C
C     elts belonging to non adapted parts
      DO NG=ITASK+1,NGROUP,NTHREAD

       ITY   =IPARG(5,NG)
       IF(ITY/=3.AND.ITY/=7)GOTO 150
       GBUF => ELBUF_TAB(NG)%GBUF

       IF (IDDW>0) CALL STARTIMEG(NG)

       NEL   =IPARG(2,NG)
       NFT   =IPARG(3,NG)
       NPT   =IPARG(6,NG)
       LFT=1
       LLT=MIN(NVSIZ,NEL)

       IF(ITY==3)THEN
        PRT = IPARTC(NFT+1)
        IADM= IPART(10,PRT)
        IF(IADM==0)THEN

         DO I=LFT,LLT
           N=NFT+I
           IF (GBUF%OFF(I) <= ZERO) CYCLE

           N1=IXC(2,N)
           N2=IXC(3,N)
           N3=IXC(4,N)
           N4=IXC(5,N)

           X1=X(1,N1)
           Y1=X(2,N1)
           Z1=X(3,N1)
           X2=X(1,N2)
           Y2=X(2,N2)
           Z2=X(3,N2)
           X3=X(1,N3)
           Y3=X(2,N3)
           Z3=X(3,N3)
           X4=X(1,N4)
           Y4=X(2,N4)
           Z4=X(3,N4)
C
           X31=X3-X1
           Y31=Y3-Y1
           Z31=Z3-Z1
           X42=X4-X2
           Y42=Y4-Y2
           Z42=Z4-Z2

           E3X=Y31*Z42-Z31*Y42
           E3Y=Z31*X42-X31*Z42
           E3Z=X31*Y42-Y31*X42

           E3X=ONE_OVER_8*E3X
           E3Y=ONE_OVER_8*E3Y
           E3Z=ONE_OVER_8*E3Z

           AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
           AREA_SH4(N)=AREA

           THICK_SH4(N)=GBUF%THK(I)
           AT = AREA * THICK_SH4(N)

#include      "lockon.inc"
           AREA_NOD(N1)=AREA_NOD(N1)+AREA
           AREA_NOD(N2)=AREA_NOD(N2)+AREA
           AREA_NOD(N3)=AREA_NOD(N3)+AREA
           AREA_NOD(N4)=AREA_NOD(N4)+AREA
           THICK_NOD(N1)=THICK_NOD(N1)+AT
           THICK_NOD(N2)=THICK_NOD(N2)+AT
           THICK_NOD(N3)=THICK_NOD(N3)+AT
           THICK_NOD(N4)=THICK_NOD(N4)+AT
#include      "lockoff.inc"
         END DO
        END IF
C
       ELSE ! ITY==7
        PRT = IPARTTG(NFT+1)
        IADM= IPART(10,PRT)
        IF(IADM==0)THEN
         DO I=LFT,LLT
           N=NFT+I  
           IF (GBUF%OFF(I) <= ZERO) CYCLE

           N1=IXTG(2,N)
           N2=IXTG(3,N)
           N3=IXTG(4,N)
           X1=X(1,N1)
           Y1=X(2,N1)
           Z1=X(3,N1)
           X2=X(1,N2)
           Y2=X(2,N2)
           Z2=X(3,N2)
           X3=X(1,N3)
           Y3=X(2,N3)
           Z3=X(3,N3)
           X31=X3-X1
           Y31=Y3-Y1
           Z31=Z3-Z1
           X32=X3-X2
           Y32=Y3-Y2
           Z32=Z3-Z2

           E3X=Y31*Z32-Z31*Y32
           E3Y=Z31*X32-X31*Z32
           E3Z=X31*Y32-Y31*X32
           E3X=ONE_OVER_6*E3X
           E3Y=ONE_OVER_6*E3Y
           E3Z=ONE_OVER_6*E3Z

           AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
           AREA_SH3(N)=AREA

           THICK_SH3(N)=GBUF%THK(I)
           AT=  AREA * THICK_SH3(N)

#include      "lockon.inc"
           AREA_NOD(N1) =AREA_NOD(N1)+AREA
           AREA_NOD(N2) =AREA_NOD(N2)+AREA
           AREA_NOD(N3) =AREA_NOD(N3)+AREA
           THICK_NOD(N1)=THICK_NOD(N1)+AT
           THICK_NOD(N2)=THICK_NOD(N2)+AT
           THICK_NOD(N3)=THICK_NOD(N3)+AT
#include      "lockoff.inc"
         END DO
        END IF
       END IF
       IF (IDDW>0) CALL STOPTIMEG(NG)
C
 150   CONTINUE
      END DO
C
      IF(NSPMD > 1 ) THEN
C
        CALL MY_BARRIER
C
        IF(ITASK == 0)THEN
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXCH_NODAREA(AREA_NOD,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
c         LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXCH_NODAREA(THICK_NOD,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
        END IF
      END IF
C
      CALL MY_BARRIER
C
C     elts belonging to adapted parts
      IF(NADMESH /= 0)THEN
        IF(ITASK==0)THEN
          CALL ADMTHKE(
     .       IXC     ,IXTG    ,X       ,IPARG   ,ELBUF_TAB ,
     .       IPART   ,IPARTC  ,IPARTTG ,IAD_ELEM,FR_ELEM   ,
     .       WEIGHT  ,AREA_SH4,AREA_SH3,AREA_NOD,THICK_SH4 ,
     .       THICK_SH3 ,THICK_NOD , ERR_THK_SH4, ERR_THK_SH3,
     .       SH4TREE ,SH3TREE)
        END IF
C
        CALL MY_BARRIER
C
      END IF
C
      DO NG=ITASK+1,NGROUP,NTHREAD

       ITY   =IPARG(5,NG)
       IF(ITY/=3.AND.ITY/=7)GOTO 250
       GBUF => ELBUF_TAB(NG)%GBUF

       IF (IDDW>0) CALL STARTIMEG(NG)

       MLW   =IPARG(1,NG)
       NEL   =IPARG(2,NG)
       NFT   =IPARG(3,NG)
       NPT   = IPARG(6,NG)
       LFT=1
       LLT=MIN(NVSIZ,NEL)

       IF(ITY==3)THEN
        PRT = IPARTC(NFT+1)
        IADM= IPART(10,PRT)
        IF(IADM==0)THEN

         DO I=LFT,LLT
           N=NFT+I
           IF (GBUF%OFF(I) <= ZERO .OR. MLW == 0 .OR. MLW == 13) CYCLE

           N1=IXC(2,N)
           N2=IXC(3,N)
           N3=IXC(4,N)
           N4=IXC(5,N)

           UNT=ONE/THICK_SH4(N)
           TN1=ABS(THICK_NOD(N1)/MAX(EM30,AREA_NOD(N1))*UNT-ONE)
           TN2=ABS(THICK_NOD(N2)/MAX(EM30,AREA_NOD(N2))*UNT-ONE)
           TN3=ABS(THICK_NOD(N3)/MAX(EM30,AREA_NOD(N3))*UNT-ONE)
           TN4=ABS(THICK_NOD(N4)/MAX(EM30,AREA_NOD(N4))*UNT-ONE)

           ERR_THK_SH4(N)=FOURTH*(TN1+TN2+TN3+TN4)

         END DO
        END IF
       ELSEIF(ITY==7)THEN
        PRT = IPARTTG(NFT+1)
        IADM= IPART(10,PRT)
        IF(IADM==0)THEN
         DO I=LFT,LLT
           N=NFT+I
            IF (GBUF%OFF(I) <= ZERO .OR. MLW == 0 .OR. MLW == 13) CYCLE

           N1=IXTG(2,N)
           N2=IXTG(3,N)
           N3=IXTG(4,N)

           UNT=ONE/THICK_SH3(N)
           TN1=ABS(THICK_NOD(N1)/MAX(EM30,AREA_NOD(N1))*UNT-ONE)
           TN2=ABS(THICK_NOD(N2)/MAX(EM30,AREA_NOD(N2))*UNT-ONE)
           TN3=ABS(THICK_NOD(N3)/MAX(EM30,AREA_NOD(N3))*UNT-ONE)

           ERR_THK_SH3(N)=THIRD*(TN1+TN2+TN3)

         END DO
        END IF
       END IF

 250  CONTINUE
      END DO
C
      CALL MY_BARRIER
C

      IF(ITASK==0)
     .   DEALLOCATE(AREA_SH4, AREA_SH3, AREA_NOD,
     .   THICK_SH4, THICK_SH3, THICK_NOD)

      RETURN
      END


