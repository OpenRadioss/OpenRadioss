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
Chd|  INIT_SURF_ELM                 source/groups/init_surf_elm.F 
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        SURF_MOD                      share/modules1/surf_mod.F     
Chd|====================================================================
        SUBROUTINE INIT_SURF_ELM( NUMELS  ,NUMELS8,NUMELS10,NUMELC ,NUMELTG ,
     1                            NUMELT  ,NUMELP ,NUMELR  ,NPART  ,IPARTS  ,
     2                            IPARTC  ,IPARTTG,IPARTT  ,IPARTP ,IPARTR  ,
     3                            SURF_ELM,MODE )
        USE SURF_MOD
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
!       INIT_SURF_ELM initializes the structur SURF_ELM
!       SURF_ELM gathers the element ID for each /PART
!       MODE = 1 is for solid/shell and shell3n elememts
!       MODE = 2 is for truss/beam and spring elements
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NUMELS,NUMELS8,NUMELS10,NUMELC ,NUMELTG
        INTEGER, INTENT(IN) :: NUMELT, NUMELP, NUMELR
        INTEGER, INTENT(IN) :: NPART,MODE
        INTEGER, DIMENSION(*), INTENT(IN) :: IPARTS ,IPARTC  ,IPARTTG
        INTEGER, DIMENSION(*), INTENT(IN) :: IPARTT ,IPARTP  ,IPARTR
        TYPE(PART_TYPE), DIMENSION(NPART), INTENT(INOUT) :: SURF_ELM
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*   
!       NPART    : integer
!                  number of /PART    
!       NUMELxxx : integer
!                  number of solid/solid8/10/shell/shell3n...
!       IPARTxxx : integer, dimension=number of element
!                  ID of the /PART for the current I element
!       SURF_ELM : PART_TYPE structur 
!                  %Nxxx : number of element per part
!                  %xxx_PART : ID of the element 
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ID_PART,I
        INTEGER, DIMENSION(NPART) :: NBR_SOL_PART,NBR_SOL10_PART,NBR_SHELL_PART,NBR_TRI_PART
        INTEGER, DIMENSION(NPART) :: NBR_TRUSS_PART,NBR_BEAM_PART,NBR_SPRING_PART
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
!       NBR_SOL(10)_PART : local number of element per part
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
        IF(MODE==1) THEN
        !       ---------------------------------
                NBR_SOL_PART(1:NPART) = 0
                NBR_SOL10_PART(1:NPART) = 0
                NBR_SHELL_PART(1:NPART) = 0
                NBR_TRI_PART(1:NPART) = 0
                !       --------------------        
                DO I=1,NUMELS
                        ID_PART = IPARTS(I)
                        IF(I<=NUMELS8) NBR_SOL_PART(ID_PART) = NBR_SOL_PART(ID_PART) + 1
                        IF(I>NUMELS8) NBR_SOL10_PART(ID_PART) = NBR_SOL10_PART(ID_PART) + 1        
                ENDDO
                DO I=1,NUMELC
                        ID_PART = IPARTC(I)
                        NBR_SHELL_PART(ID_PART) = NBR_SHELL_PART(ID_PART) + 1        
                ENDDO
                DO I=1,NUMELTG
                        ID_PART = IPARTTG(I)
                        NBR_TRI_PART(ID_PART) = NBR_TRI_PART(ID_PART) + 1    
                ENDDO
                !       --------------------
                DO ID_PART=1,NPART                
                        IF(NBR_SOL_PART(ID_PART)>0) ALLOCATE( SURF_ELM(ID_PART)%SOL_PART( NBR_SOL_PART(ID_PART) ) )
                        IF(NBR_SOL10_PART(ID_PART)>0) ALLOCATE( SURF_ELM(ID_PART)%SOL10_PART( NBR_SOL10_PART(ID_PART) ) )
                        IF(NBR_SHELL_PART(ID_PART)>0) ALLOCATE( SURF_ELM(ID_PART)%SHELL_PART( NBR_SHELL_PART(ID_PART) ) )
                        IF(NBR_TRI_PART(ID_PART)>0) ALLOCATE( SURF_ELM(ID_PART)%TRI_PART( NBR_TRI_PART(ID_PART) ) )
                        SURF_ELM(ID_PART)%NSOL = NBR_SOL_PART(ID_PART)
                        SURF_ELM(ID_PART)%NSOL10 = NBR_SOL10_PART(ID_PART)
                        SURF_ELM(ID_PART)%NSHELL = NBR_SHELL_PART(ID_PART)
                        SURF_ELM(ID_PART)%NTRI = NBR_TRI_PART(ID_PART)
                ENDDO
                !       --------------------
                NBR_SOL_PART(1:NPART) = 0
                NBR_SOL10_PART(1:NPART) = 0
                DO I=1,NUMELS
                        ID_PART = IPARTS(I)
                        IF(I<=NUMELS8) THEN
                                NBR_SOL_PART(ID_PART) = NBR_SOL_PART(ID_PART) + 1
                                SURF_ELM(ID_PART)%SOL_PART( NBR_SOL_PART(ID_PART) ) = I
                        ELSE
                                NBR_SOL10_PART(ID_PART) = NBR_SOL10_PART(ID_PART) + 1
                                SURF_ELM(ID_PART)%SOL10_PART( NBR_SOL10_PART(ID_PART) ) = I
                        ENDIF
                ENDDO     
                NBR_SHELL_PART(1:NPART) = 0
                DO I=1,NUMELC
                        ID_PART = IPARTC(I)
                        NBR_SHELL_PART(ID_PART) = NBR_SHELL_PART(ID_PART) + 1
                        SURF_ELM(ID_PART)%SHELL_PART( NBR_SHELL_PART(ID_PART) ) = I           
                ENDDO
                NBR_TRI_PART(1:NPART) = 0
                DO I=1,NUMELTG
                        ID_PART = IPARTTG(I)
                        NBR_TRI_PART(ID_PART) = NBR_TRI_PART(ID_PART) + 1
                        SURF_ELM(ID_PART)%TRI_PART( NBR_TRI_PART(ID_PART) ) = I           
                ENDDO
                !       --------------------
        !       ---------------------------------
        ELSEIF(MODE==2) THEN
        !       ---------------------------------
                NBR_TRUSS_PART(1:NPART) = 0
                NBR_BEAM_PART(1:NPART) = 0
                NBR_SPRING_PART(1:NPART) = 0
                !       --------------------
                DO I=1,NUMELT
                        ID_PART = IPARTT(I)
                        NBR_TRUSS_PART(ID_PART) = NBR_TRUSS_PART(ID_PART) + 1
        
                ENDDO
                DO I=1,NUMELP
                        ID_PART = IPARTP(I)
                        NBR_BEAM_PART(ID_PART) = NBR_BEAM_PART(ID_PART) + 1
        
                ENDDO
                DO I=1,NUMELR
                        ID_PART = IPARTR(I)
                        NBR_SPRING_PART(ID_PART) = NBR_SPRING_PART(ID_PART) + 1
        
                ENDDO
                !       --------------------
                DO ID_PART=1,NPART                
                        IF(NBR_TRUSS_PART(ID_PART)>0) ALLOCATE( SURF_ELM(ID_PART)%TRUSS_PART( NBR_TRUSS_PART(ID_PART) ) )
                        IF(NBR_BEAM_PART(ID_PART)>0) ALLOCATE( SURF_ELM(ID_PART)%BEAM_PART( NBR_BEAM_PART(ID_PART) ) )
                        IF(NBR_SPRING_PART(ID_PART)>0) ALLOCATE( SURF_ELM(ID_PART)%SPRING_PART( NBR_SPRING_PART(ID_PART) ) )
                        SURF_ELM(ID_PART)%NTRUSS = NBR_TRUSS_PART(ID_PART)
                        SURF_ELM(ID_PART)%NBEAM = NBR_BEAM_PART(ID_PART)
                        SURF_ELM(ID_PART)%NSPRING = NBR_SPRING_PART(ID_PART)
                ENDDO
                !       --------------------

                NBR_TRUSS_PART(1:NPART) = 0
                DO I=1,NUMELT
                        ID_PART = IPARTT(I)
                        NBR_TRUSS_PART(ID_PART) = NBR_TRUSS_PART(ID_PART) + 1
                        SURF_ELM(ID_PART)%TRUSS_PART( NBR_TRUSS_PART(ID_PART) ) = I           
                ENDDO
                NBR_BEAM_PART(1:NPART) = 0
                DO I=1,NUMELP
                        ID_PART = IPARTP(I)
                        NBR_BEAM_PART(ID_PART) = NBR_BEAM_PART(ID_PART) + 1
                        SURF_ELM(ID_PART)%BEAM_PART( NBR_BEAM_PART(ID_PART) ) = I           
                ENDDO
                NBR_SPRING_PART(1:NPART) = 0
                DO I=1,NUMELR
                        ID_PART = IPARTR(I)
                        NBR_SPRING_PART(ID_PART) = NBR_SPRING_PART(ID_PART) + 1
                        SURF_ELM(ID_PART)%SPRING_PART( NBR_SPRING_PART(ID_PART) ) = I           
                ENDDO
                !       --------------------
        !       ---------------------------------
        ENDIF

        RETURN
        END SUBROUTINE INIT_SURF_ELM

Chd|====================================================================
Chd|  DEALLOCATE_SURF_ELM           source/groups/init_surf_elm.F 
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        SURF_MOD                      share/modules1/surf_mod.F     
Chd|====================================================================
        SUBROUTINE DEALLOCATE_SURF_ELM(NPART,SURF_ELM,MODE)

        USE SURF_MOD

!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
!       DEALLOCATE_SURF_ELM deallocates the structur SURF_ELM
!       MODE = 1 is for solid/shell and shell3n elememts
!       MODE = 2 is for truss/beam and spring elements
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NPART,MODE
        TYPE(PART_TYPE), DIMENSION(NPART), INTENT(INOUT) :: SURF_ELM
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       
!       NPART    : integer
!                  number of /PART
!       SURF_ELM : PART_TYPE structur 
!                  %Nxxx : number of element per part
!                  %xxx_PART : ID of the element 
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
C-----------------------------------------------
        IF(MODE==1) THEN
        !       --------------------
                DO I=1,NPART                
                        IF(SURF_ELM(I)%NSOL>0) DEALLOCATE( SURF_ELM(I)%SOL_PART )
                        IF(SURF_ELM(I)%NSOL10>0) DEALLOCATE( SURF_ELM(I)%SOL10_PART )
                        IF(SURF_ELM(I)%NSHELL>0) DEALLOCATE( SURF_ELM(I)%SHELL_PART )
                        IF(SURF_ELM(I)%NTRI>0) DEALLOCATE( SURF_ELM(I)%TRI_PART )
                ENDDO
        !       --------------------
        ELSEIF(MODE==2) THEN
        !       --------------------
                DO I=1,NPART                
                        IF(SURF_ELM(I)%NTRUSS>0) DEALLOCATE( SURF_ELM(I)%TRUSS_PART )
                        IF(SURF_ELM(I)%NBEAM>0) DEALLOCATE( SURF_ELM(I)%BEAM_PART )
                        IF(SURF_ELM(I)%NSPRING>0) DEALLOCATE( SURF_ELM(I)%SPRING_PART )
                ENDDO
        !       --------------------
        ENDIF

        RETURN
        END SUBROUTINE DEALLOCATE_SURF_ELM
