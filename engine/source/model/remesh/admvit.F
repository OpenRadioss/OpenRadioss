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
Chd|  ADMVIT                        source/model/remesh/admvit.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMVIT(IXC   ,IPARTC ,IXTG ,IPARTTG ,IPART ,
     .                  ITASK ,A      ,V    ,AR      ,VR    ,
     .                  SH4TREE,SH3TREE,TEMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IPARTC(*), IXTG(NIXTG,*), IPARTTG(*),
     .        IPART(LIPART1,*), ITASK, SH4TREE(KSH4TREE,*),
     .        SH3TREE(KSH3TREE,*)
       my_real
     .        A(3,*),V(3,*),AR(3,*),VR(3,*), TEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SH4FT, SH4LT, SH3FT, SH3LT
      INTEGER N, NN, LEVEL, IP, NLEV, LL, IERR
      INTEGER SON,M(4),MC,N1,N2,N3,N4,J,NA,NB
      my_real
     .        VV, USDT
C-----------------------------------------------
      USDT=ONE/DT12
C
C     allocation tag
      IF(ITASK==0)THEN
        TAGNOD=0
      END IF
C
      CALL MY_BARRIER
C
C-------
      DO LEVEL=0,LEVELMAX-1

        LL=PSH4KIN(LEVEL+1)-PSH4KIN(LEVEL)
        SH4FT = PSH4KIN(LEVEL)+ 1+ITASK*LL/ NTHREAD
        SH4LT = PSH4KIN(LEVEL)+ (ITASK+1)*LL/NTHREAD

        DO NN=SH4FT,SH4LT
          N    =LSH4KIN(NN)
C
          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)
C
          SON=SH4TREE(2,N)
C
          MC=IXC(3,SON+3)

          IF(TAGNOD(MC)==0)THEN
            TAGNOD(MC)=1
            DO J=1,3
              VV= FOURTH*(V(J,N1)+V(J,N2)+V(J,N3)+V(J,N4)
     .    	       +DT12*(A(J,N1)+A(J,N2)+A(J,N3)+A(J,N4)))
              A(J,MC)=USDT*(VV-V(J,MC))
            END DO
            DO J=1,3
              VV= FOURTH*(VR(J,N1)+VR(J,N2)+VR(J,N3)+VR(J,N4)
     .    	       +DT12*(AR(J,N1)+AR(J,N2)+AR(J,N3)+AR(J,N4)))
              AR(J,MC)=USDT*(VV-VR(J,MC))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(MC)=FOURTH*(TEMP(N1)+TEMP(N2)+TEMP(N3)+TEMP(N4))
          END IF
C
          M(1)=IXC(3,SON  )
          M(2)=IXC(4,SON+1)
          M(3)=IXC(5,SON+2)
          M(4)=IXC(2,SON+3)

          IF(TAGNOD(M(1))==0)THEN
            TAGNOD(M(1))=1
            NA=MIN(N1,N2)
            NB=MAX(N1,N2)
            DO J=1,3
              VV= HALF*(V(J,NA)+V(J,NB)+DT12*(A(J,NA)+A(J,NB)))
              A(J,M(1))=USDT*(VV-V(J,M(1)))
            END DO
            DO J=1,3
              VV= HALF*(VR(J,NA)+VR(J,NB)+DT12*(AR(J,NA)+AR(J,NB)))
              AR(J,M(1))=USDT*(VV-VR(J,M(1)))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(M(1))=HALF*(TEMP(NA)+TEMP(NB))
          END IF

          IF(TAGNOD(M(2))==0)THEN
            TAGNOD(M(2))=1
            NA=MIN(N2,N3)
            NB=MAX(N2,N3)
            DO J=1,3
              VV= HALF*(V(J,NA)+V(J,NB)+DT12*(A(J,NA)+A(J,NB)))
              A(J,M(2))=USDT*(VV-V(J,M(2)))
            END DO
            DO J=1,3
              VV= HALF*(VR(J,NA)+VR(J,NB)+DT12*(AR(J,NA)+AR(J,NB)))
              AR(J,M(2))=USDT*(VV-VR(J,M(2)))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(M(2))=HALF*(TEMP(NA)+TEMP(NB))
          END IF

          IF(TAGNOD(M(3))==0)THEN
            TAGNOD(M(3))=1
            NA=MIN(N3,N4)
            NB=MAX(N3,N4)
            DO J=1,3
              VV= HALF*(V(J,NA)+V(J,NB)+DT12*(A(J,NA)+A(J,NB)))
              A(J,M(3))=USDT*(VV-V(J,M(3)))
            END DO
            DO J=1,3
              VV= HALF*(VR(J,NA)+VR(J,NB)+DT12*(AR(J,NA)+AR(J,NB)))
              AR(J,M(3))=USDT*(VV-VR(J,M(3)))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(M(3))=HALF*(TEMP(NA)+TEMP(NB))
          END IF

          IF(TAGNOD(M(4))==0)THEN
            TAGNOD(M(4))=1
            NA=MIN(N4,N1)
            NB=MAX(N4,N1)
            DO J=1,3
              VV= HALF*(V(J,NA)+V(J,NB)+DT12*(A(J,NA)+A(J,NB)))
              A(J,M(4))=USDT*(VV-V(J,M(4)))
            END DO
            DO J=1,3
              VV= HALF*(VR(J,NA)+VR(J,NB)+DT12*(AR(J,NA)+AR(J,NB)))
              AR(J,M(4))=USDT*(VV-VR(J,M(4)))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(M(4))=HALF*(TEMP(NA)+TEMP(NB))
          END IF
        END DO

        LL=PSH3KIN(LEVEL+1)-PSH3KIN(LEVEL)
        SH3FT = PSH3KIN(LEVEL)+ 1+ITASK*LL/ NTHREAD
        SH3LT = PSH3KIN(LEVEL)+ (ITASK+1)*LL/NTHREAD

        DO NN=SH3FT,SH3LT
          N    =LSH3KIN(NN)
C
          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)
C
          SON=SH3TREE(2,N)
C
          M(1)=IXTG(4,SON+3)
          M(2)=IXTG(2,SON+3)
          M(3)=IXTG(3,SON+3)

          IF(TAGNOD(M(1))==0)THEN
            TAGNOD(M(1))=1
            NA=MIN(N1,N2)
            NB=MAX(N1,N2)
            DO J=1,3
              VV= HALF*(V(J,NA)+V(J,NB)+DT12*(A(J,NA)+A(J,NB)))
              A(J,M(1))=USDT*(VV-V(J,M(1)))
            END DO
            DO J=1,3
              VV= HALF*(VR(J,NA)+VR(J,NB)+DT12*(AR(J,NA)+AR(J,NB)))
              AR(J,M(1))=USDT*(VV-VR(J,M(1)))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(M(1))=HALF*(TEMP(NA)+TEMP(NB))
          END IF

          IF(TAGNOD(M(2))==0)THEN
            TAGNOD(M(2))=1
            NA=MIN(N2,N3)
            NB=MAX(N2,N3)
            DO J=1,3
              VV= HALF*(V(J,NA)+V(J,NB)+DT12*(A(J,NA)+A(J,NB)))
              A(J,M(2))=USDT*(VV-V(J,M(2)))
            END DO
            DO J=1,3
              VV= HALF*(VR(J,NA)+VR(J,NB)+DT12*(AR(J,NA)+AR(J,NB)))
              AR(J,M(2))=USDT*(VV-VR(J,M(2)))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(M(2))=HALF*(TEMP(NA)+TEMP(NB))
          END IF

          IF(TAGNOD(M(3))==0)THEN
            TAGNOD(M(3))=1
            NA=MIN(N3,N1)
            NB=MAX(N3,N1)
            DO J=1,3
              VV= HALF*(V(J,NA)+V(J,NB)+DT12*(A(J,NA)+A(J,NB)))
              A(J,M(3))=USDT*(VV-V(J,M(3)))
            END DO
            DO J=1,3
              VV= HALF*(VR(J,NA)+VR(J,NB)+DT12*(AR(J,NA)+AR(J,NB)))
              AR(J,M(3))=USDT*(VV-VR(J,M(3)))
            END DO
            IF(ITHERM_FE > 0)
     .        TEMP(M(3))=HALF*(TEMP(NA)+TEMP(NB))
          END IF
        END DO
C
        CALL MY_BARRIER
C
      END DO
C
      RETURN
      END     
