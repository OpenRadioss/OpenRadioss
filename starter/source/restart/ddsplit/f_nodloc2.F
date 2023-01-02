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
Chd|  F_NODLOC2                     source/restart/ddsplit/f_nodloc2.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE F_NODLOC2(NUMNOD,P  ,NODGLOB,NODLOCAL,IGEO ,
     2                     IPARG,IXS   ,IXQ,IXC    ,IXTG    ,
     3                     IXT  ,IXP   ,IXR,GEO    ,NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNOD,P,NUMNOD_L,
     +        NODGLOB(*), NODLOCAL(*), IGEO(NPROPGI,*),
     +        IPARG(NPARG,*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     +        IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*)
       my_real
     +        GEO(NPROPG,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N_L, NG, NEL, ITY, NFT, K, N, IE
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG
C
! ----------------------
!     allocate 1d array
      ALLOCATE( ITAG(NUMNOD) )
! ----------------------
      DO N = 1, NUMNOD
        ITAG(N) = 0
      END DO
C
      N_L = 0
      DO NG=1,NGROUP
        IF(IPARG(32,NG)==P-1) THEN
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          ITY = IPARG(5,NG)
          IF(ITY==1) THEN
C solide (reste a faire solide 10, 16, 20)
            DO I = 1, NEL
              IE = I+NFT
              DO K = 1, 8
                N = IXS(K+1,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END DO
            END DO
C cas 2D
          ELSEIF(ITY==2)THEN
            DO I = 1, NEL
              IE = I+NFT
              DO K = 1, 4
                N = IXQ(K+1,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END DO
            END DO
          ELSEIF(ITY==3)THEN
C coques
            DO I = 1, NEL
              IE = I+NFT
              DO K = 1, 4
                N = IXC(K+1,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END DO
            END DO
          ELSEIF(ITY==4)THEN
C trusses
            DO I = 1, NEL
              IE = I+NFT
              DO K = 1, 2
                N = IXT(K+1,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END DO
            END DO
          ELSEIF(ITY==5)THEN
C poutres
            DO I = 1, NEL
              IE = I+NFT
              DO K = 1, 2
                N = IXP(K+1,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END DO
            END DO
          ELSEIF(ITY==6)THEN
C ressorts
            DO I = 1, NEL
              IE = I+NFT
              DO K = 1, 2
                N = IXR(K+1,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END DO
              IF(IGEO(11,IXR(1,IE))==12)THEN
                N = IXR(4,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END IF
            END DO
          ELSEIF(ITY==7)THEN
C   probably confusion between int_type and element       ELSEIF(ITY==7.OR.ITY==22)THEN
C triangles
            DO I = 1, NEL
              IE = I+NFT
              DO K = 1, 3
                N = IXTG(K+1,IE)
                IF(ITAG(N)==0) THEN
                  N_L = N_L + 1
                  NODLOCAL(N) = N_L
                  NODGLOB(N_L) = N
                  ITAG(N) = 1
                END IF
              END DO
            END DO
          ELSEIF(ITY==50)THEN
C ur  a faire
          END IF
        END IF
      END DO
C
      DO I = 1, NUMNOD
        IF(ITAG(I)==0) THEN
          IF(NLOCAL(I,P)==1)THEN
            N_L = N_L + 1
            NODLOCAL(I) = N_L
            NODGLOB(N_L)= I
          ELSE
            NODLOCAL(I) = 0
          ENDIF
        END IF
      ENDDO
C
! ----------------------
!     deallocate 1d array
      DEALLOCATE( ITAG )
! ----------------------
      RETURN
      END
