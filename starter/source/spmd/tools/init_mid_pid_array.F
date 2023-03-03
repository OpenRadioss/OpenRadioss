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
Chd|  INIT_MID_PID_ARRAY            source/spmd/tools/init_mid_pid_array.F
Chd|-- called by -----------
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|-- calls ---------------
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
        SUBROUTINE INIT_MID_PID_ARRAY(MODE           ,TAILLE       ,NUMMAT     ,NPART  ,CONCORDANCE_MAT,
     1                                TAB_UMP        ,PID_SHELL    ,PID_TRI    ,PID_SOL,
     2                                MID_PID_SHELL  ,MID_PID_TRI  ,MID_PID_SOL,
     3                                IPART          ,IPM          ,GEO          ,CPUTIME_MP_OLD_2,
     4                                POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL)

        USE MID_PID_MOD

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
!     integer 
      INTEGER, INTENT(IN) :: MODE,TAILLE,NUMMAT,NPART
      INTEGER, DIMENSION(TAILLE), INTENT(INOUT) :: CONCORDANCE_MAT
      INTEGER, DIMENSION(7,TAILLE), INTENT(IN) :: TAB_UMP
      INTEGER, DIMENSION(2,NPART), INTENT(INOUT) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(INOUT) :: POIN_PART_SOL
      INTEGER, DIMENSION(NPROPMI,*), INTENT(IN) :: IPM

      INTEGER, DIMENSION(LIPART1,*), INTENT(IN) :: IPART
      INTEGER, DIMENSION(NUMMAT) :: PID_SHELL,PID_TRI
      INTEGER, DIMENSION(NUMMAT,7) :: PID_SOL
!     real
      my_real, DIMENSION(NPROPG,*), INTENT(IN) :: GEO
      my_real, DIMENSION(*) :: CPUTIME_MP_OLD_2
!     other type
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(INOUT) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(INOUT) :: MID_PID_SOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ELM_TYP,MID,PID,IPID,ID,IGTYP,INDI,IGTYP_LOC,ILAW
      INTEGER :: I,J
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------

C=======================================================================


! ------------------------------------------------------------------------
!       MODE = 0 : create the mid_pid arrays for each element type that 
!                  will be used in INITWG_[X] subroutine (X = SHELL, SOL 
!                  or TRI). The mid_pid arrays will be initialized with 
!                  the Radioss element cost if MODE = 1 is not used. 
!                  If MODE = 1 is used, mid_pid will be initialized to 
!                  the element costs obtained from a previous Radioss run
!               
!       MODE = 1 : initialize the mid_pid arrays to the element costs 
!                  obtained from a previous Radioss run
!
!       MODE = 0 : allocate and initialize the mid_pid_1 arrays for 
!                  each element type :
!                    * check the number of pid properties 
!                      per mid ( %NBR_PID )
!                    * allocate each array(mid)%pid1d ( number of 
!                      pid ) + %cost1d( number of pid )
!
!       MODE = 1 : initialize each array(mid)%pid1d(pid) + %cost1d(pid)
! ------------------------------------------------------------------------

        IF(MODE==0) THEN
                CONCORDANCE_MAT(1:TAILLE) = 0
                DO I=1,TAILLE
                        ELM_TYP = TAB_UMP(7,I)
                        MID = TAB_UMP(3,I)
                        PID = TAB_UMP(4,I)
                        IF(ELM_TYP==3.OR.ELM_TYP==7.OR.ELM_TYP==1.OR.
     .                  ELM_TYP==1004.OR.ELM_TYP==1006.OR.ELM_TYP==1008.OR.
     .                  ELM_TYP==1010.OR.ELM_TYP==1016.OR.ELM_TYP==1020) THEN
                                CONCORDANCE_MAT(I) = 1
                        ENDIF
                ENDDO
        ENDIF                               
        
! -----------------------
!       check the number of pid properties per mid
! -----------------------
!
!       *********************************
!       mode = 0
        IF(MODE==0) THEN
           DO I=1,TAILLE
                J = CONCORDANCE_MAT(I)
                IF(J>0) THEN
                        ELM_TYP = TAB_UMP(7,I)
                        MID = TAB_UMP(3,I)
                        PID = TAB_UMP(4,I)
                        IF(ELM_TYP==3) THEN
                            PID_SHELL(MID) = PID_SHELL(MID) + 1
                        ELSEIF(ELM_TYP==7) THEN
                            PID_TRI(MID) = PID_TRI(MID) + 1        
                        ELSE
                            INDI = 0
                            IF(ELM_TYP==1) THEN
                                    INDI=1
                            ELSEIF(ELM_TYP==1004) THEN
                                    INDI = 6
                            ELSEIF(ELM_TYP==1006) THEN
                                    INDI = 5
                            ELSEIF(ELM_TYP==1008) THEN
                                    INDI = 7
                            ELSEIF(ELM_TYP==1010) THEN
                                    INDI = 2
                            ELSEIF(ELM_TYP==1016) THEN
                                    INDI = 3
                            ELSEIF(ELM_TYP==1020) THEN
                                    INDI = 4
                            ENDIF
                            IF(INDI/=0) PID_SOL(MID,INDI) = PID_SOL(MID,INDI) + 1 
                        ENDIF
                ENDIF
           ENDDO

! -----------------------
!       allocate each array(mid)%pid1d ( number of pid ) + %cost1d
! -----------------------
           DO I=1,NUMMAT
                MID_PID_SHELL(I)%NBR_PID = 0
                MID_PID_TRI(I)%NBR_PID = 0
                MID_PID_SOL(I,1:7)%NBR_PID = 0
                IF(PID_SHELL(I)>0) THEN
                        ALLOCATE( MID_PID_SHELL(I)%PID1D( PID_SHELL(I) ) )
                        ALLOCATE( MID_PID_SHELL(I)%COST1D( PID_SHELL(I) ) )
                        MID_PID_SHELL(I)%PID1D( 1:PID_SHELL(I) ) = 0
                        MID_PID_SHELL(I)%COST1D( 1:PID_SHELL(I) ) = ZERO
                        MID_PID_SHELL(I)%NBR_PID = PID_SHELL(I)
                ENDIF
                IF(PID_TRI(I)>0) THEN
                        ALLOCATE( MID_PID_TRI(I)%PID1D( PID_TRI(I) ) )
                        ALLOCATE( MID_PID_TRI(I)%COST1D( PID_TRI(I) ) )
                        MID_PID_TRI(I)%PID1D( 1 : PID_TRI(I) ) = 0 
                        MID_PID_TRI(I)%COST1D( 1 : PID_TRI(I) ) = ZERO 
                        MID_PID_TRI(I)%NBR_PID = PID_TRI(I)
                ENDIF
                DO INDI=1,7
                        IF(PID_SOL(I,INDI)>0) THEN
                                ALLOCATE( MID_PID_SOL(I,INDI)%PID1D( PID_SOL(I,INDI) ) )
                                ALLOCATE( MID_PID_SOL(I,INDI)%COST1D( PID_SOL(I,INDI) ) )
                                MID_PID_SOL(I,INDI)%PID1D( 1:PID_SOL(I,INDI) ) = 0
                                MID_PID_SOL(I,INDI)%COST1D( 1:PID_SOL(I,INDI) ) = ZERO
                                MID_PID_SOL(I,INDI)%NBR_PID = PID_SOL(I,INDI) 
                        ELSE
                                MID_PID_SOL(I,INDI)%NBR_PID = 0
                        ENDIF
                ENDDO
           ENDDO
        ENDIF

!       end of mode = 0
!       *********************************
!       mode = 0 or 1 
!       mode = 0 --> %cost1d = 0
!       mode = 1 --> %cost1d = old cost 
! -----------------------
!       initialize each array(mid)%pid1d(pid) + %cost1d
! -----------------------
        PID_SHELL(1:NUMMAT) = 0
        PID_TRI(1:NUMMAT) = 0
        PID_SOL(1:NUMMAT,1:7) = 0
        DO I=1,TAILLE
                J = CONCORDANCE_MAT(I)
                IF(J/=0) THEN
                        ELM_TYP = TAB_UMP(7,I)
                        MID = TAB_UMP(3,I)
                        PID = TAB_UMP(4,I)
                        IF(ELM_TYP==3) THEN
                          PID_SHELL(MID) = PID_SHELL(MID) + 1
                          MID_PID_SHELL(MID)%PID1D( PID_SHELL(MID) ) = PID
                          IF(MODE==0) THEN
                            MID_PID_SHELL(MID)%COST1D( PID_SHELL(MID) ) = ZERO
                          ELSE
                            MID_PID_SHELL(MID)%COST1D( PID_SHELL(MID) ) = CPUTIME_MP_OLD_2(J)
                          ENDIF
                        ELSEIF(ELM_TYP==7) THEN
                          PID_TRI(MID) = PID_TRI(MID) + 1        
                          MID_PID_TRI(MID)%PID1D( PID_TRI(MID) ) = PID
                          IF(MODE==0) THEN
                            MID_PID_TRI(MID)%COST1D( PID_TRI(MID) ) = ZERO
                          ELSE
                            MID_PID_TRI(MID)%COST1D( PID_TRI(MID) ) = CPUTIME_MP_OLD_2(J)
                          ENDIF
                        ELSE
                            INDI=0
                            IF(ELM_TYP==1) THEN
                                    INDI=1
                            ELSEIF(ELM_TYP==1004) THEN
                                    INDI = 6
                            ELSEIF(ELM_TYP==1006) THEN
                                    INDI = 5
                            ELSEIF(ELM_TYP==1008) THEN
                                    INDI = 7
                            ELSEIF(ELM_TYP==1010) THEN
                                    INDI = 2
                            ELSEIF(ELM_TYP==1016) THEN
                                    INDI = 3
                            ELSEIF(ELM_TYP==1020) THEN
                                    INDI = 4
                            ENDIF
                            IF(INDI/=0) THEN
                             PID_SOL(MID,INDI) = PID_SOL(MID,INDI) + 1 
                             MID_PID_SOL(MID,INDI)%PID1D( PID_SOL(MID,INDI) ) = PID
                             IF(MODE==0) THEN
                               MID_PID_SOL(MID,INDI)%COST1D( PID_SOL(MID,INDI) ) = ZERO
                             ELSE
                               MID_PID_SOL(MID,INDI)%COST1D( PID_SOL(MID,INDI) ) = CPUTIME_MP_OLD_2(J)
                             ENDIF
                            ENDIF
                        ENDIF
                ENDIF
        ENDDO
! -----------------------
!       initialize the pointer POIN_PART_[x] :
!               POIN_PART gives the location of the (mid ; pid) pair in the mid_pid array
!               POIN_PART(1) = mid
!               POIN_PART(2) = place of the pid in all the properties of the mid
! -----------------------
!       *********************************
!       mode = 0
        IF(MODE==0) THEN
           DO I=1,NPART
                IPID = IPART(2,I)
                ID = IPART(4,I)
                MID = IPART(1,I)        ! Radioss internal mid per /PART
                PID = IPART(2,I)        ! Radioss internal pid per /PART
                IGTYP_LOC=NINT(GEO(12,IPID))
                IF(IPART(1,I) == 0)CYCLE !error message was printed, user must update mat_id
                ILAW = IPM(2,IPART(1,I))
! for /TRIA element, force the IGTYP flag 
! -->  not really the best way to define a TRIA element but the TRIA element dev. was not
! a beautiful dev! TRIA element are solid element but TRIA does not use the solid arrays (too simple!)
! and use the SHELL3N arrays !!!!!!!! 
                IF(ILAW==151.AND.N2D/=0) IGTYP_LOC = 1 
                IGTYP = IGTYP_LOC

                IF(IGTYP==1  .OR. IGTYP==9  .OR. IGTYP==10 .OR. IGTYP==11 .OR.
     .             IGTYP==16 .OR. IGTYP==17 .OR. IGTYP==19 .OR. IGTYP==51 .OR.
     .             IGTYP==52 .OR. IGTYP==0 ) THEN
!               -----------------
!               SHELL ELEMENT
!               -----------------
                        IF(MID_PID_SHELL(MID)%NBR_PID>0) THEN
                                DO J=1,MID_PID_SHELL(MID)%NBR_PID
                                 IF(PID==MID_PID_SHELL(MID)%PID1D(J)) THEN
                                        POIN_PART_SHELL(1,I)=MID
                                        POIN_PART_SHELL(2,I) = J
                                 ENDIF                                
                                ENDDO
                        ENDIF
!               -----------------
!               SHELL3N ELEMENT
!               -----------------
                        IF(MID_PID_TRI(MID)%NBR_PID>0) THEN
                                DO J=1,MID_PID_TRI(MID)%NBR_PID
                                 IF(PID==MID_PID_TRI(MID)%PID1D(J)) THEN
                                        POIN_PART_TRI(1,I)=MID
                                        POIN_PART_TRI(2,I) = J
                                 ENDIF                                
                                ENDDO
                        ENDIF
                ELSEIF(IGTYP==6  .OR. IGTYP==14 .OR. IGTYP==15 .OR.
     .                 IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22 .OR.
     .                 IGTYP==43 ) THEN
!               -----------------
!               SOLID ELEMENT
!               -----------------
                        DO INDI=1,7
                            IF(MID_PID_SOL(MID,INDI)%NBR_PID>0) THEN
                                 DO J=1,MID_PID_SOL(MID,INDI)%NBR_PID
                                        IF(PID==MID_PID_SOL(MID,INDI)%PID1D(J)) THEN
                                           POIN_PART_SOL(1,I,INDI)=MID
                                           POIN_PART_SOL(2,I,INDI) = J   
                                        ENDIF                            
                                 ENDDO
                            ENDIF
                        ENDDO
                ELSEIF(IGTYP==29.OR.IGTYP==30.OR.IGTYP==31 ) THEN
!               -----------------
!               USER ELEMENT
!               -----------------
                        DO INDI=1,7
                            IF(MID_PID_SOL(MID,INDI)%NBR_PID>0) THEN
                                 DO J=1,MID_PID_SOL(MID,INDI)%NBR_PID
                                        IF(PID==MID_PID_SOL(MID,INDI)%PID1D(J)) THEN
                                           POIN_PART_SOL(1,I,INDI)=MID
                                           POIN_PART_SOL(2,I,INDI) = J   
                                        ENDIF                            
                                 ENDDO
                            ENDIF
                        ENDDO
                ENDIF
           ENDDO
        ENDIF
!       end of mode = 0
!       *********************************
        RETURN

        END SUBROUTINE INIT_MID_PID_ARRAY
