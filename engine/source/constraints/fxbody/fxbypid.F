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
Chd|  FXBYPID                       source/constraints/fxbody/fxbypid.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_CHKW                     source/mpi/generic/spmd_chkw.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_WIOUT                    source/mpi/generic/spmd_wiout.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FXBYPID(IPARG ,         IXS   , IXQ   , IXC       ,
     .                   IXT   , IXP   , IXR   , IXTG  , FXBIPM    , 
     .                   FXBNOD, ONOF  , ITAG  , ONFELT, ELBUF_STR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXS(NIXS,*), IXQ(NIXQ,*),IXC(NIXC,*), 
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),
     .        FXBIPM(*),FXBNOD(*),ONOF,ITAG(*),ONFELT
      TYPE(ELBUF_STRUCT_),TARGET ,DIMENSION(NGROUP)     :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN,NG,MLW,ITY,NEL,NFT,IAD,I,II,NALL,IGOF,ISHFT,IWIOUT
      my_real,
     .    DIMENSION(:), POINTER :: OFFG
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      IF (ISPMD == 0) WRITE(IOUT,*) ' BEGINNING FXBYPID'
      IF (ONOF == 0) THEN
C-----------------------
C     DESACTIVATION DES FXB
C-----------------------
      ELSEIF (ONOF == 1) THEN
C-----------------------
C     ACTIVATION DES FXB
C-----------------------
      ENDIF
C      
      IF(ONFELT == 0.OR.ONFELT == 1)THEN
      NSN=FXBIPM(3)
C-----------------------
C     TAG DES NOEUDS SECONDS
C-----------------------
      DO I=1,NUMNOD
       ITAG(I)=0
      ENDDO
      DO I=1,NSN
       ITAG(FXBNOD(I))=1
      ENDDO
C-----------------------
C     MISE DE OFF A -OFF
C-----------------------
      DO NG=1,NGROUP
       MLW=IPARG(1,NG)
       ITY=IPARG(5,NG)
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       IAD=IPARG(4,NG) - 1
C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
       IF (ITY == 1 .AND. MLW /= 0) THEN       ! loi0, pas de off
         OFFG => ELBUF_STR(NG)%GBUF%OFF
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXS(2,II)) * ITAG(IXS(3,II)) *
     +          ITAG(IXS(4,II)) * ITAG(IXS(5,II)) *
     +          ITAG(IXS(6,II)) * ITAG(IXS(7,II)) *
     +          ITAG(IXS(8,II)) * ITAG(IXS(9,II)) 
          IF(NALL /= 0)THEN
            IF(ONFELT == 1)THEN
             OFFG(I)= ABS(OFFG(I))
             WRITE(IOUT,*)' BRICK ACTIVATION:',IXS(11,II)
            ELSEIF(ONFELT == 0)THEN
             OFFG(I) = -ABS(OFFG(I))
             WRITE(IOUT,*)' BRICK DEACTIVATION:',IXS(11,II)
           ENDIF
         ENDIF
        ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
        IGOF = 1
        DO I = 1,NEL
          IF (OFFG(I) > ZERO) IGOF=0
        ENDDO
        IPARG(8,NG) = IGOF
C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
       ELSEIF(ITY == 3 .and. MLW /= 0)THEN       ! loi0, pas de off
         OFFG => ELBUF_STR(NG)%GBUF%OFF
         DO I=1,NEL
          II=I+NFT
          NALL = ITAG(IXC(2,II)) * ITAG(IXC(3,II)) *
     +           ITAG(IXC(4,II)) * ITAG(IXC(5,II)) 
           IF (NALL /= 0) THEN
             IF (ONFELT == 1) THEN
               OFFG(I) = ABS(OFFG(I))
              WRITE(IOUT,*)' SHELL ACTIVATION:',IXC(7,II)
             ELSEIF (ONFELT == 0) THEN
               OFFG(I) = -ABS(OFFG(I))
              WRITE(IOUT,*)' SHELL DEACTIVATION:',IXC(7,II)
            ENDIF
          ENDIF
         ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
        IGOF = 1
        DO I = 1,NEL
          IF (OFFG(I) > ZERO) IGOF=0
        ENDDO
        IPARG(8,NG) = IGOF
C-----------------------
C     4. ELEMENTS TRUSS
C-----------------------
       ELSEIF(ITY == 4)THEN
        OFFG => ELBUF_STR(NG)%GBUF%OFF
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXT(2,II)) * ITAG(IXT(3,II)) 
         IF(NALL /= 0)THEN
           IF(ONFELT == 1)THEN
             OFFG(I) = ONE
             WRITE(IOUT,*)' TRUSS ACTIVATION:',IXT(5,II)
           ELSEIF(ONFELT == 0)THEN
             OFFG(I) = ZERO
             WRITE(IOUT,*)' TRUSS DEACTIVATION:',IXT(5,II)
           ENDIF
         ENDIF
        ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
        IGOF = 1
        DO I = 1,NEL
          IF (OFFG(I) /= ZERO) IGOF=0
        ENDDO
        IPARG(8,NG) = IGOF
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
       ELSEIF(ITY == 5)THEN
        OFFG => ELBUF_STR(NG)%GBUF%OFF
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXP(2,II)) * ITAG(IXP(3,II)) 
         IF(NALL /= 0)THEN
           IF(ONFELT == 1)THEN
             OFFG(I)= ABS(OFFG(I))
             WRITE(IOUT,*)' BEAM ACTIVATION:',IXP(6,II)
           ELSEIF(ONFELT == 0)THEN
             OFFG(I)= -ABS(OFFG(I))
             WRITE(IOUT,*)' BEAM DEACTIVATION:',IXP(6,II)
           ENDIF
         ENDIF
        ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
        IGOF = 1
        DO I = 1,NEL
         IF(OFFG(I).GT.ZERO) IGOF=0
        ENDDO
        IPARG(8,NG) = IGOF
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
       ELSEIF(ITY == 6.AND.MLW /= 3)THEN
        OFFG => ELBUF_STR(NG)%GBUF%OFF
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXR(2,II)) * ITAG(IXR(3,II)) 
         IF(NALL /= 0)THEN
           IF(ONFELT == 1)THEN
             OFFG(I)= ONE
             WRITE(IOUT,*)' SPRING ACTIVATION:',IXR(NIXR,II)
           ELSEIF(ONFELT == 0)THEN
             OFFG(I)= ZERO
             WRITE(IOUT,*)' SPRING DEACTIVATION:',IXR(NIXR,II)
           ENDIF
         ENDIF
        ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
        IGOF = 1
        DO I = 1,NEL
         IF(OFFG(I) /= ZERO) IGOF=0
        ENDDO
        IPARG(8,NG) = IGOF
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
       ELSEIF(ITY == 7 .and. MLW /= 0)THEN       ! loi0, pas de off
         OFFG => ELBUF_STR(NG)%GBUF%OFF
         DO I=1,NEL
          II=I+NFT
          NALL = ITAG(IXTG(2,II)) * ITAG(IXTG(3,II)) *
     +           ITAG(IXTG(4,II))
           IF(NALL /= 0)THEN
             IF (ONFELT == 1) THEN
               OFFG(I) = ABS(OFFG(I))
              WRITE(IOUT,*)' SH_3N ACTIVATION:',IXTG(6,II)
             ELSEIF (ONFELT == 0) THEN
               OFFG(I) = -ABS(OFFG(I))
              WRITE(IOUT,*)' SH_3N DEACTIVATION:',IXTG(6,II)
            ENDIF
          ENDIF
         ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
        IGOF = 1
        DO I = 1,NEL
          IF (OFFG(I) > ZERO) IGOF=0
        ENDDO
        IPARG(8,NG) = IGOF
C----------------------------------------
       ENDIF
      ENDDO
      IF(NSPMD.GT.1) THEN
C traitement necessaire pour recuperer les elts active ou desactive dans l'ordre
        IWIOUT = 0
        IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
        CALL SPMD_GLOB_ISUM9(IWIOUT,1)
        CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
        IF (IWIOUT.GT.0) THEN
          CALL SPMD_WIOUT(IOUT,IWIOUT)
          IWIOUT = 0
        ENDIF
      ENDIF
C
      ENDIF
      IF (ISPMD == 0) WRITE(IOUT,*) ' END FXBYPID'
c-----------
      RETURN
      END
                  
