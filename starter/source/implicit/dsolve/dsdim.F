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
Chd|====================================================================
Chd|  DSDIM0                        source/implicit/dsolve/dsdim.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSDIM0(NDOF  ,IXS   , IXQ    , IXC    , IXT    ,
     2                  IXP   ,IXR   , IXTG   , KXX    ,
     3                  IXX   ,GEO   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), 
     .        IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*),
     .        IXTG(NIXTG,*), KXX(NIXX,*), IXX(*)
      my_real
     .        GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IEL, J, N, IGTYP, NN
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG8

      ALLOCATE(TAG8(NUMNOD))
C
C Remplissage de NDOF (cf DIM_GLOB_K dans ENGINE)
C
      DO I=1,NUMNOD
         NDOF(I)=0
      ENDDO
C
      IF(ISMS==0)THEN
C----   IMPLICIT
        DO IEL=1,NUMELS
           DO J=1,8
              N=IXS(J+1,IEL)
              IF(N/=0)NDOF(N)=3
           ENDDO
        ENDDO
        DO IEL=1,NUMELT
           DO J=1,2
              N=IXT(J+1,IEL)
              IF(N/=0)NDOF(N)=3
           ENDDO
        ENDDO
        DO IEL=1,NUMELC
           DO J=1,4
              N=IXC(J+1,IEL)
              IF(N/=0)NDOF(N)=6
           ENDDO
        ENDDO
        DO IEL=1,NUMELTG
           DO J=1,3
              N=IXTG(J+1,IEL)
              IF(N/=0)NDOF(N)=6
           ENDDO
        ENDDO
        DO IEL=1,NUMELP
           DO J=1,2
              N=IXP(J+1,IEL)
              IF(N/=0)NDOF(N)=6
           ENDDO
        ENDDO
        DO IEL=1,NUMELR
           IGTYP= NINT(GEO(12,IXR(1,IEL)))
           IF (IGTYP==8.OR.IGTYP==13 .OR. IGTYP == 23) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N/=0)NDOF(N)=6
              ENDDO
           ELSEIF (IGTYP==4) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N/=0)NDOF(N)=MAX(NDOF(N),3)
              ENDDO
           ENDIF
        ENDDO
C
C      DO I=1,NRBYKIN
C         N=NPBY(1,I)
C         IF(N/=0)NDOF(N)=6
C      ENDDO
C
      ELSE
C----   AMS
        DO IEL=1,NUMELS
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N/=0)TAG8(N)=0
          ENDDO
          NN=0                           
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N/=0)THEN
              IF(TAG8(N)==0)THEN
                NN=NN+1
                TAG8(N)=1
              END IF                  
            END IF                  
          ENDDO                                    
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N/=0)TAG8(N)=0
          ENDDO                                    
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N/=0)THEN
              IF(TAG8(N)==0)THEN
                NDOF(N)=NN-1
                TAG8(N)=1
              END IF                  
            END IF                  
          ENDDO                                    
        ENDDO
        DO IEL=1,NUMELT
           DO J=1,2
              N=IXT(J+1,IEL)
              IF(N/=0)NDOF(N)=1
           ENDDO
        ENDDO
        DO IEL=1,NUMELC
           DO J=1,4
              N=IXC(J+1,IEL)
              IF(N/=0)NDOF(N)=3
           ENDDO
        ENDDO
        DO IEL=1,NUMELTG
           DO J=1,3
              N=IXTG(J+1,IEL)
              IF(N/=0)NDOF(N)=2
           ENDDO
        ENDDO
        DO IEL=1,NUMELP
           DO J=1,2
              N=IXP(J+1,IEL)
              IF(N/=0)NDOF(N)=1
           ENDDO
        ENDDO
        DO IEL=1,NUMELR
           IGTYP= NINT(GEO(12,IXR(1,IEL)))
           IF (IGTYP==8.OR.IGTYP==13 .OR. IGTYP == 23) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N/=0)NDOF(N)=1
              ENDDO
           ELSEIF (IGTYP==4) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N/=0)NDOF(N)=1
              ENDDO
           ENDIF
        ENDDO
      END IF
      DEALLOCATE(TAG8)
      RETURN
      END

