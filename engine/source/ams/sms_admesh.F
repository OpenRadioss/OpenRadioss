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
Chd|  SMS_ADMESH_0                  source/ams/sms_admesh.F       
Chd|-- called by -----------
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- calls ---------------
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE SMS_ADMESH_0(A    ,DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .                        SH3TREE  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*), 
     .        SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
C     REAL ou REAL*8
      my_real
     .   A(3,*), DIAG_SMS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, LEVEL, IP, NLEV
      INTEGER SON,M1,M2,M3,M4,MC,N1,N2,N3,N4,J,K
      INTEGER LKINNOD(NUMNOD)
      my_real
     .        A1,A2,A3,A4,AC
C-----------------------------------------------
      LKINNOD=0
      DO LEVEL=LEVELMAX-1,0,-1

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)

          SON=SH4TREE(2,N)

          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)
C
          MC=IXC(4,SON)
          AC= FOURTH*DIAG_SMS(MC)
          DIAG_SMS(N1)=DIAG_SMS(N1)+AC
          DIAG_SMS(N2)=DIAG_SMS(N2)+AC
          DIAG_SMS(N3)=DIAG_SMS(N3)+AC
          DIAG_SMS(N4)=DIAG_SMS(N4)+AC

          DIAG_SMS(MC)=ZERO
          LKINNOD(MC)=1
C
          M1=IXC(3,SON  )
          IF(LKINNOD(M1)==0)THEN
            LKINNOD(M1)=1
            A1=HALF*DIAG_SMS(M1)
            DIAG_SMS(N1)=DIAG_SMS(N1)+A1
            DIAG_SMS(N2)=DIAG_SMS(N2)+A1
            DIAG_SMS(M1)=ZERO
          END IF
C
          M2=IXC(4,SON+1)
          IF(LKINNOD(M2)==0)THEN
            LKINNOD(M2)=1
            A2=HALF*DIAG_SMS(M2)
            DIAG_SMS(N2)=DIAG_SMS(N2)+A2
            DIAG_SMS(N3)=DIAG_SMS(N3)+A2
            DIAG_SMS(M2)=ZERO
          END IF
C
          M3=IXC(5,SON+2)
          IF(LKINNOD(M3)==0)THEN
            LKINNOD(M3)=1
            A3=HALF*DIAG_SMS(M3)
            DIAG_SMS(N3)=DIAG_SMS(N3)+A3
            DIAG_SMS(N4)=DIAG_SMS(N4)+A3
            DIAG_SMS(M3)=ZERO
          END IF
C
          M4=IXC(2,SON+3)
          IF(LKINNOD(M4)==0)THEN
            LKINNOD(M4)=1
            A4=HALF*DIAG_SMS(M4)
            DIAG_SMS(N1)=DIAG_SMS(N1)+A4
            DIAG_SMS(N4)=DIAG_SMS(N4)+A4
            DIAG_SMS(M4)=ZERO
          END IF

        END DO


        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)

          SON=SH3TREE(2,N)

          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)
C
          M1=IXTG(4,SON+3)
          IF(LKINNOD(M1)==0)THEN
            LKINNOD(M1)=1
            A1=HALF*DIAG_SMS(M1)
            DIAG_SMS(N1)=DIAG_SMS(N1)+A1
            DIAG_SMS(N2)=DIAG_SMS(N2)+A1
            DIAG_SMS(M1)=ZERO
          END IF
C
          M2=IXTG(2,SON+3)
          IF(LKINNOD(M2)==0)THEN
            LKINNOD(M2)=1
            A2=HALF*DIAG_SMS(M2)
            DIAG_SMS(N2)=DIAG_SMS(N2)+A2
            DIAG_SMS(N3)=DIAG_SMS(N3)+A2
            DIAG_SMS(M2)=ZERO
          END IF

          M3=IXTG(3,SON+3)
          IF(LKINNOD(M3)==0)THEN
            LKINNOD(M3)=1
            A3=HALF*DIAG_SMS(M3)
            DIAG_SMS(N3)=DIAG_SMS(N3)+A3
            DIAG_SMS(N1)=DIAG_SMS(N1)+A3
            DIAG_SMS(M3)=ZERO
          END IF

        END DO

      END DO

      RETURN
      END
Chd|====================================================================
Chd|  SMS_ADMESH_1                  source/ams/sms_admesh.F       
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE SMS_ADMESH_1(A    ,DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .                        SH3TREE  ,NODNX_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*), 
     .        SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*), NODNX_SMS(*)
C     REAL ou REAL*8
      my_real
     .   A(3,*), DIAG_SMS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, LEVEL, IP, NLEV
      INTEGER SON,M1,M2,M3,M4,MC,N1,N2,N3,N4,J,K
      INTEGER LKINNOD(NUMNOD)
      my_real
     .        A1,A2,A3,A4,AC
C-----------------------------------------------
      LKINNOD=0
      DO LEVEL=LEVELMAX-1,0,-1

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)

          SON=SH4TREE(2,N)

          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)
C
          MC=IXC(4,SON)
          DO J=1,3
            AC= FOURTH*A(J,MC)
            A(J,N1)=A(J,N1)+AC
            A(J,N2)=A(J,N2)+AC
            A(J,N3)=A(J,N3)+AC
            A(J,N4)=A(J,N4)+AC
            A(J,MC)=ZERO
          END DO
          LKINNOD(MC)=1
C
          M1=IXC(3,SON  )
          IF(LKINNOD(M1)==0)THEN
          LKINNOD(M1)=1
          DO J=1,3
            A1=HALF*A(J,M1)
            A(J,N1)=A(J,N1)+A1
            A(J,N2)=A(J,N2)+A1
            A(J,M1)=ZERO
          END DO
          END IF
C
          M2=IXC(4,SON+1)
          IF(LKINNOD(M2)==0)THEN
          LKINNOD(M2)=1
          DO J=1,3
            A2=HALF*A(J,M2)
            A(J,N2)=A(J,N2)+A2
            A(J,N3)=A(J,N3)+A2
            A(J,M2)=ZERO
          END DO
          END IF

          M3=IXC(5,SON+2)
          IF(LKINNOD(M3)==0)THEN
          LKINNOD(M3)=1
          DO J=1,3
            A3=HALF*A(J,M3)
            A(J,N3)=A(J,N3)+A3
            A(J,N4)=A(J,N4)+A3
            A(J,M3)=ZERO
          END DO
          END IF
C
          M4=IXC(2,SON+3)
          IF(LKINNOD(M4)==0)THEN
          LKINNOD(M4)=1
          DO J=1,3
            A4=HALF*A(J,M4)
            A(J,N1)=A(J,N1)+A4
            A(J,N4)=A(J,N4)+A4
            A(J,M4)=ZERO
          END DO
          END IF

        END DO


        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)

          SON=SH3TREE(2,N)

          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)
C
          M1=IXTG(4,SON+3)
          IF(LKINNOD(M1)==0)THEN
          LKINNOD(M1)=1
          DO J=1,3
            A1=HALF*A(J,M1)
            A(J,N1)=A(J,N1)+A1
            A(J,N2)=A(J,N2)+A1
            A(J,M1)=ZERO
          END DO
          END IF
C
          M2=IXTG(2,SON+3)
          IF(LKINNOD(M2)==0)THEN
          LKINNOD(M2)=1
          DO J=1,3
            A2=HALF*A(J,M2)
            A(J,N2)=A(J,N2)+A2
            A(J,N3)=A(J,N3)+A2
            A(J,M2)=ZERO
          END DO
          END IF

          M3=IXTG(3,SON+3)
          IF(LKINNOD(M3)==0)THEN
          LKINNOD(M3)=1
          DO J=1,3
            A3=HALF*A(J,M3)
            A(J,N3)=A(J,N3)+A3
            A(J,N1)=A(J,N1)+A3
            A(J,M3)=ZERO
          END DO
          END IF

        END DO

      END DO
      RETURN
      END
Chd|====================================================================
Chd|  SMS_ADMESH_2                  source/ams/sms_admesh.F       
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE SMS_ADMESH_2(A    ,DIAG_SMS, IXC, IXTG,SH4TREE  ,
     .                        SH3TREE  ,ITASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*), 
     .        SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*), ITASK
C     REAL ou REAL*8
      my_real
     .   A(3,*), DIAG_SMS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, LEVEL, IP, NLEV
      INTEGER SON,M1,M2,M3,M4,MC,N1,N2,N3,N4,J,K,NA,NB
      INTEGER LL, SH4FT, SH4LT, SH3FT, SH3LT
C-----------------------------------------------
C
C     allocation tag
      IF(ITASK.EQ.0)THEN
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

          SON=SH4TREE(2,N)

          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)
C
          MC=IXC(4,SON)
          IF(TAGNOD(MC).EQ.0)THEN
            TAGNOD(MC)=1
            DO J=1,3
              A(J,MC)= FOURTH*(A(J,N1)+A(J,N2)+A(J,N3)+A(J,N4))
            END DO
          END IF
C
          M1=IXC(3,SON  )
          IF(TAGNOD(M1)==0)THEN
            TAGNOD(M1)=1
            NA=MIN(N1,N2)
            NB=MAX(N1,N2)
            DO J=1,3
              A(J,M1)=HALF*(A(J,NA)+A(J,NB))
            END DO
          END IF
C
          M2=IXC(4,SON+1)
          IF(TAGNOD(M2)==0)THEN
            TAGNOD(M2)=1
            NA=MIN(N2,N3)
            NB=MAX(N2,N3)
            DO J=1,3
              A(J,M2)=HALF*(A(J,NA)+A(J,NB))
            END DO
          END IF

          M3=IXC(5,SON+2)
          IF(TAGNOD(M3)==0)THEN
            TAGNOD(M3)=1
            NA=MIN(N3,N4)
            NB=MAX(N3,N4)
            DO J=1,3
              A(J,M3)=HALF*(A(J,NA)+A(J,NB))
            END DO
          END IF
C
          M4=IXC(2,SON+3)
          IF(TAGNOD(M4)==0)THEN
            TAGNOD(M4)=1
            NA=MIN(N4,N1)
            NB=MAX(N4,N1)
            DO J=1,3
              A(J,M4)=HALF*(A(J,NA)+A(J,NB))
            END DO
          END IF

        END DO


        LL=PSH3KIN(LEVEL+1)-PSH3KIN(LEVEL)
        SH3FT = PSH3KIN(LEVEL)+ 1+ITASK*LL/ NTHREAD
        SH3LT = PSH3KIN(LEVEL)+ (ITASK+1)*LL/NTHREAD

        DO NN=SH3FT,SH3LT
          N    =LSH3KIN(NN)

          SON=SH3TREE(2,N)

          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)
C
          M1=IXTG(4,SON+3)
          IF(TAGNOD(M1)==0)THEN
            TAGNOD(M1)=1
            NA=MIN(N1,N2)
            NB=MAX(N1,N2)
            DO J=1,3
              A(J,M1)=HALF*(A(J,NA)+A(J,NB))
            END DO
          END IF
C
          M2=IXTG(2,SON+3)
          IF(TAGNOD(M2)==0)THEN
            TAGNOD(M2)=1
            NA=MIN(N2,N3)
            NB=MAX(N2,N3)
            DO J=1,3
              A(J,M2)=HALF*(A(J,NA)+A(J,NB))
            END DO
          END IF

          M3=IXTG(3,SON+3)
          IF(TAGNOD(M3)==0)THEN
            TAGNOD(M3)=1
            NA=MIN(N3,N1)
            NB=MAX(N3,N1)
            DO J=1,3
              A(J,M3)=HALF*(A(J,NA)+A(J,NB))
            END DO
          END IF

        END DO
C
        CALL MY_BARRIER
C
      END DO

      RETURN
      END

