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
Chd|  SORT_MID_PID                  source/system/sort_mid_pid.F  
Chd|-- called by -----------
Chd|        GRPSPLIT                      source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        MID_PID_MOD                   share/modules/mid_pid_mod.F   
Chd|====================================================================
       SUBROUTINE SORT_MID_PID(N_SHELL,IGROUC_SHELL,
     1                         POIN_GROUP_MID_SHELL,POIN_GROUP_PID_SHELL,
     2                         MID_SHELL,TAB_LOC,TAB_SHELL_LOC,TAB_MAT)

        USE MID_PID_MOD

!      ----------------------------------------------
!      Sort Shell groups :
!
!      (m1,p1) , (m1,p3), (m1,p1) , (m2,p1) , (m1,p1) , (m56,p7) ... --> sort by mid in MID_PID_SHELL(MID)%PID1D
!      MID_PID_SHELL(m1)%PID1D ( -p1, -p3, -p1, -p1 ...)
!      MID_PID_SHELL(m2)%PID1D ( -p1, ... )
!      MID_PID_SHELL(m56)%PID1D( -p7 ... )
!       
!      add the index of Group NG for each (mi;pj) pairs 
!      MID_PID_SHELL(m1)%GROUP1D( NG4, NG6, NG7 , ...) for m1 mid
!
!      get the number of same pi PID for each mj MID in TAB_LOC(3)
!      MID_PID_SHELL(m1)%PID1D ( -p1, -p3, -p1, -p1 ...) --> 3 p1 ; -p1 becomes +p1
!
!      get the total number of different (mi,pj) pairs --> COMPTEUR_MAT_PROP_SHELL
!
!      MID_SHELL(:) : number of pid per mid EQV to PID_INDEX array
!      PID_INDEX(:) : index of pid per mid
!      COMPTEUR_MAT_PROP_SHELL : number of different (mi,pj) pairs 
!
!       MID_PID_SHELL(MID)%PID1D( : ) --> (mid ; pid) pair
!       MID_PID_SHELL(MID)%GROUP1D --> number of Group for (mid,pid)

C-----------------------------------------------------------------------
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
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::N_SHELL
      INTEGER, DIMENSION(NUMMAT), INTENT(IN) :: MID_SHELL
      INTEGER, DIMENSION(N_SHELL), INTENT(IN) :: IGROUC_SHELL,POIN_GROUP_MID_SHELL,POIN_GROUP_PID_SHELL
      INTEGER, DIMENSION(NGROUP,5),  INTENT(IN) :: TAB_SHELL_LOC
      INTEGER, DIMENSION(N_SHELL,3), INTENT(INOUT) :: TAB_LOC
      my_real, INTENT(IN) :: TAB_MAT(NGROUP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER ::I,J,II,JJ,K,INDI,NBR_MID_PID_GRP
       INTEGER :: NG,NGG,NGG_LOC,ITY
       INTEGER :: MARQUEUR,MARQUEUR_2,MARQUEUR_3,COMPTEUR_MAT_PROP_SHELL,COMPTEUR
       INTEGER :: FIRST,LAST,SHIFT,GR_ID,GR_ID2
       INTEGER :: NBR_MID,NFT
       INTEGER :: MID,MAX_MID
       INTEGER :: PID,MAX_PID,PID_1,PID_2
       INTEGER, DIMENSION(:), ALLOCATABLE :: PID_INDEX,INDEX
       INTEGER, DIMENSION(N_SHELL) :: POIN_MID_SHELL
       my_real POIDS_J,POIDS_J1
       TYPE(MID_PID_TYPE),DIMENSION(NUMMAT) :: MID_PID_SHELL
C-----------------------------------------------

       ALLOCATE( PID_INDEX(NUMMAT) )
       PID_INDEX(1:NUMMAT) = 0

       COMPTEUR = 0
       DO NGG=1,N_SHELL
         NG = IGROUC_SHELL(NGG)
         MID = POIN_GROUP_MID_SHELL(NGG)
         PID = POIN_GROUP_PID_SHELL(NGG)
         IF(.NOT.ALLOCATED(MID_PID_SHELL(MID)%PID1D)) THEN
                ALLOCATE( MID_PID_SHELL(MID)%PID1D( MID_SHELL(MID) ) )
                ALLOCATE( MID_PID_SHELL(MID)%GROUP1D(MID_SHELL(MID) ) )

                MID_PID_SHELL(MID)%PID1D( 1:MID_SHELL(MID) ) = 0
                MID_PID_SHELL(MID)%GROUP1D( 1:MID_SHELL(MID) )  = 0

!                MID_PID_SHELL(MID)%PID1D( 1:MID_SHELL(MID) ) = 0   
                !       first group of (mid;pid)    
                PID_INDEX(MID) = PID_INDEX(MID) + 1 
                MID_PID_SHELL(MID)%PID1D( PID_INDEX(MID) ) = -PID 
                MID_PID_SHELL(MID)%GROUP1D( PID_INDEX(MID) ) = NGG
                COMPTEUR = COMPTEUR + 1
                POIN_MID_SHELL(COMPTEUR) = MID
         ELSE
                !       other group of (mid;pid)
                PID_INDEX(MID) = PID_INDEX(MID) + 1 
                MID_PID_SHELL(MID)%PID1D( PID_INDEX(MID) ) = -PID 
                MID_PID_SHELL(MID)%GROUP1D( PID_INDEX(MID) ) = NGG
         ENDIF
       ENDDO


       MAX_MID = COMPTEUR

!      Get the max number of pid per mid
       MAX_PID = -1
       DO I=1,MAX_MID
         MID = POIN_MID_SHELL(I)
         MAX_PID = MAX( MAX_PID,PID_INDEX(MID) )
       ENDDO

       ALLOCATE( INDEX(MAX_PID) )

       COMPTEUR_MAT_PROP_SHELL = 0
       JJ = 0
       DO II=1,MAX_MID
         MID = POIN_MID_SHELL(II)
         DO I=1,PID_INDEX(MID)
                PID_1 = MID_PID_SHELL(MID)%PID1D( I )
                COMPTEUR = 0
                IF(PID_1 < 0 ) THEN
                        COMPTEUR = 1
                        MID_PID_SHELL(MID)%PID1D( I ) = -PID_1
                        INDEX(COMPTEUR) = I

                        DO J=I+1,PID_INDEX(MID)
                                PID_2 = MID_PID_SHELL(MID)%PID1D( J )
                                IF(PID_1 == PID_2 ) THEN
                                        MID_PID_SHELL(MID)%PID1D( J ) = -PID_2
                                        COMPTEUR = COMPTEUR + 1
                                        INDEX(COMPTEUR) = J
                                ENDIF
                        ENDDO
                ENDIF
                IF(COMPTEUR>0) THEN
                        DO J=1,COMPTEUR
                                JJ = JJ + 1
                                NGG_LOC = MID_PID_SHELL(MID)%GROUP1D( INDEX(J) )
                                TAB_LOC(JJ,1) = NGG_LOC
                                TAB_LOC(JJ,2) = TAB_SHELL_LOC(NGG_LOC,1)  
                                TAB_LOC(JJ,3) = COMPTEUR
                        ENDDO
                        COMPTEUR_MAT_PROP_SHELL = COMPTEUR_MAT_PROP_SHELL + 1
                ENDIF
         ENDDO
       ENDDO     

       DEALLOCATE( INDEX ) 
       DEALLOCATE( PID_INDEX )  
                                
       ! tri poids mat/prop

       I=N_SHELL
       MARQUEUR = 0
       DO WHILE((MARQUEUR==0).and.(I>0))
         MARQUEUR = 1
         DO J =1,I-1
            II=TAB_LOC(J,1)
            MID = TAB_SHELL_LOC(II,3)
            PID = TAB_SHELL_LOC(II,4)
	    GR_ID = TAB_SHELL_LOC(II,2)
            POIDS_J = TAB_MAT(GR_ID)
            JJ=TAB_LOC(J+1,1)
            MID = TAB_SHELL_LOC(JJ,3)
            PID = TAB_SHELL_LOC(JJ,4)
            GR_ID2 = TAB_SHELL_LOC(JJ,2)

            POIDS_J1 = TAB_MAT(GR_ID2)
           IF(POIDS_J<POIDS_J1) then
            MARQUEUR = TAB_LOC(J,1)
            MARQUEUR_2 = TAB_LOC(J,2)
            MARQUEUR_3 = TAB_LOC(J,3)
            TAB_LOC(J,1) = TAB_LOC(J+1,1)
            TAB_LOC(J,2) = TAB_LOC(J+1,2)
            TAB_LOC(J,3) = TAB_LOC(J+1,3)
            TAB_LOC(J+1,1) = MARQUEUR
            TAB_LOC(J+1,2) = MARQUEUR_2
            TAB_LOC(J+1,3) = MARQUEUR_3
            MARQUEUR = 0
           ENDIF
         ENDDO
         I=I-1
       ENDDO

       ! tri nbr elem
       SHIFT = 1
       DO I =1,COMPTEUR_MAT_PROP_SHELL
        J = TAB_LOC(SHIFT,1)
        MID = TAB_SHELL_LOC(J,3)
        PID = TAB_SHELL_LOC(J,4)
        NBR_MID_PID_GRP = TAB_LOC(SHIFT,3)
        FIRST = J 
        LAST = FIRST + NBR_MID_PID_GRP - 1
        MARQUEUR = 0
        II = LAST-FIRST
        DO WHILE(MARQUEUR==0.and.II>0)
         MARQUEUR = 1 
         do JJ = FIRST,II-1
          if(TAB_LOC(JJ,2)<TAB_LOC(JJ+1,2)) then
           MARQUEUR = TAB_LOC(JJ,1)
           MARQUEUR_2 = TAB_LOC(JJ,2)
           MARQUEUR_3 = TAB_LOC(JJ,3)
           TAB_LOC(JJ,1) = TAB_LOC(JJ+1,1)
           TAB_LOC(JJ,2) = TAB_LOC(JJ+1,2)
           TAB_LOC(JJ+1,1) = MARQUEUR
           TAB_LOC(JJ+1,2) = MARQUEUR_2
           TAB_LOC(JJ+1,3) = MARQUEUR_3
           MARQUEUR = 0
          ENDIF
         ENDDO
         II = II - 1
        ENDDO
        SHIFT = SHIFT + NBR_MID_PID_GRP
       ENDDO

       DO I=1,MAX_MID
         MID = POIN_MID_SHELL(I)
         DEALLOCATE( MID_PID_SHELL(MID)%GROUP1D )
         DEALLOCATE( MID_PID_SHELL(MID)%PID1D )      
       ENDDO


       RETURN
       END SUBROUTINE SORT_MID_PID
