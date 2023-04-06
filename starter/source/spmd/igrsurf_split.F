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
Chd|  IGRSURF_SPLIT                 source/spmd/igrsurf_split.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
        SUBROUTINE IGRSURF_SPLIT(CEP,T_MONVOL,IGRSURF,IGRSURF_PROC)
!$COMMENT
!       IGSURF_SPLIT description
!       IGSURF_SPLIT splits the global IGSURF array into local 
!                    IGSURF_PROC arrays in order to save 
!                    CPU time in ddsplit routine (avoid NSPMD 
!                    treatments)
!       
!       IGSURF_SPLIT organization :
!       - 1rst step : count the number of element per surface 
!                     on a given processor and allocate the 
!                     IGRSURF_PROC structure
!       - 2nd step : fill the structure
!$ENDCOMMENT
        
        USE GROUPDEF_MOD
        USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, DIMENSION(*), INTENT(IN) :: CEP
        TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
        TYPE(SURF_), DIMENSION(NSURF,NSPMD), INTENT(INOUT) :: IGRSURF_PROC
        TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL

!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       CEP    : integer ; dimension=NUMNOD
!                CEP gives the id processor of an element

!                   monitor volume array
!       IGRSURF : SURF_ ; dimension=NSURF
!                 global surface property array
!                 %ELTYP --> type of element (shell, triangle...)
!                 %ELEM  --> element id
!                 %NSEG --> total element number
!       IGRSURF_PROC : SURF_ ; dimension=NSURF*NSPMD
!                 local surface property array (=IGRSURF for each proc)
!                 %ELTYP --> type of element (shell, triangle...)
!                 %ELEM  --> element id
!                 %NSEG --> total element number
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NV,IS,NN,J
        INTEGER :: ITY,II,PROC,K1
        INTEGER :: OFFC,OFFTG   
        INTEGER, DIMENSION(NSPMD) :: JJ
        INTEGER :: NJ,NJ1,NJ2,NJ3
        INTEGER :: NSN,NODE_ID,NJET
        INTEGER :: I_AM_HERE
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
        !       --------------------------------------
        !       offset for the CEP array
        OFFC = NUMELS+NUMELQ    
        OFFTG = NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR

        !       1st step : count the number of element per proc and allocate the structure
        K1 = 1

        DO NV=1,NVOLU                   !       NVOLU = number of volume
                IS = T_MONVOL(NV)%EXT_SURFID       !       id of the surface 
                NN = IGRSURF(IS)%NSEG   !       number of element per surface
                JJ(1:NSPMD) = 0         !       proc index
                IGRSURF_PROC(IS,1:NSPMD)%NSEG = 0
                DO J=1,NN
                        ITY = IGRSURF(IS)%ELTYP(J)      !       type of the element 
                        II  = IGRSURF(IS)%ELEM(J)       !       id of the element
                        PROC = 0                        !       id of the proc /= 0 if ITY = 3 or 7
                        IF(ITY==3) THEN
                                PROC = CEP(OFFC+II) + 1
                        ELSEIF(ITY==7) THEN
                                PROC = CEP(OFFTG+II) + 1
                        ENDIF
                        IF(PROC>0) THEN
                                JJ(PROC) = JJ(PROC) + 1
                        ENDIF
                ENDDO
                ! -----------------------------
                !       allocation : several MONVOL can refer to the same surface ID
                DO PROC=1,NSPMD
                        IF(.NOT.ALLOCATED(IGRSURF_PROC(IS,PROC)%ELTYP).AND.JJ(PROC)>0) THEN
                                IGRSURF_PROC(IS,PROC)%NSEG = JJ(PROC)
                                ALLOCATE( IGRSURF_PROC(IS,PROC)%ELTYP( JJ(PROC) ) )
                                ALLOCATE( IGRSURF_PROC(IS,PROC)%ELEM( JJ(PROC) ) )
                        ENDIF
                        ! -----------------------------
                        !   force the NJ1, NJ2, NJ3 nodes on the processor PROC
                        IF(JJ(PROC)>0) THEN
                            NJET = T_MONVOL(NV)%NJET
                            DO NJ = 1, NJET
                                NJ1 = T_MONVOL(NV)%IBAGJET(5, NJ)
                                NJ2 = T_MONVOL(NV)%IBAGJET(6, NJ)
                                NJ3 = T_MONVOL(NV)%IBAGJET(7, NJ)
                                IF (NJ1 /= 0) CALL IFRONTPLUS(NJ1, PROC)
                                IF (NJ2 /= 0) CALL IFRONTPLUS(NJ2, PROC)
                                IF (NJ3 /= 0) CALL IFRONTPLUS(NJ3, PROC)
                            ENDDO

                            IF (T_MONVOL(NV)%NB_FILL_TRI > 0) THEN
                                DO J = 1, T_MONVOL(NV)%NB_FILL_TRI
                                    I_AM_HERE = 0
                                    NODE_ID = T_MONVOL(NV)%FILL_TRI(3 * (J - 1) + 1)
                                    IF (NODE_ID > 0) THEN
                                        CALL IFRONTPLUS(NODE_ID, PROC)
                                        I_AM_HERE = I_AM_HERE + 1
                                    ENDIF
                                    NODE_ID = T_MONVOL(NV)%FILL_TRI(3 * (J - 1) + 2)
                                    IF (NODE_ID > 0) THEN
                                        CALL IFRONTPLUS(NODE_ID, PROC)
                                        I_AM_HERE = I_AM_HERE + 1
                                    ENDIF
                                    NODE_ID = T_MONVOL(NV)%FILL_TRI(3 * (J - 1) + 3)
                                    IF (NODE_ID > 0) THEN
                                        CALL IFRONTPLUS(NODE_ID, PROC)
                                        I_AM_HERE = I_AM_HERE + 1
                                    ENDIF
                                    IF( I_AM_HERE==3 ) THEN
                                        T_MONVOL(NV)%NUMBER_TRI_PER_PROC(PROC) = 
     .                                      T_MONVOL(NV)%NUMBER_TRI_PER_PROC(PROC) + 1
                                    ENDIF
                                ENDDO
                            ENDIF
                        ! -----------------------------
                        ENDIF
                ENDDO
                ! -----------------------------
                K1 = K1 + NIMV
        ENDDO
        !       --------------------------------------
        !       2nd step : fill the structure
        K1 = 1
        DO NV=1,NVOLU                   !       NVOLU = number of volume
                IS = T_MONVOL(NV)%EXT_SURFID      !       id of the surface 
                NN = IGRSURF(IS)%NSEG   !       number of element per surface
                JJ(1:NSPMD) = 0                 !       proc index
                DO J=1,NN
                        ITY = IGRSURF(IS)%ELTYP(J)      !       type of the element 
                        II  = IGRSURF(IS)%ELEM(J)       !       id of the element
                        PROC = 0                        !       id of the proc /= 0 if ITY = 3 or 7
                        IF(ITY==3) THEN
                                PROC = CEP(OFFC+II) + 1
                        ELSEIF(ITY==7) THEN
                                PROC = CEP(OFFTG+II) + 1
                        ENDIF
                        IF(PROC>0) THEN
                                JJ(PROC) = JJ(PROC) + 1
                                IGRSURF_PROC(IS,PROC)%ELTYP(JJ(PROC)) = ITY
                                IGRSURF_PROC(IS,PROC)%ELEM(JJ(PROC)) = II
                        ENDIF
                ENDDO
                K1 = K1 + NIMV
        ENDDO
        !       --------------------------------------
        RETURN
        END SUBROUTINE IGRSURF_SPLIT
