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
Chd|  I22GET_PREV_DATA              source/interfaces/int22/i22get_prev_data.F
Chd|-- called by -----------
Chd|        I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        REALLOC_MOD                   share/modules/realloc_mod.F   
Chd|====================================================================
      SUBROUTINE I22GET_PREV_DATA(
     1      X      ,II_STOK, CAND_B  ,CAND_E   ,ITASK,
     2      NBRIC  ,ITAB   , BUFBRIC  ,NCAND,   
     3      IXS    ,NIN )
C============================================================================    
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  This subroutine get data from cut cell buffer
C  in previous cycle and update new buffer with
C  consistent data if brick is sill there
C  Example :
C    * nodes which were master
C    * old volume
C    * list of previous slaves (for merge/demerge)
C    * IIAD22 is also reset for all brick in
C      previous buffer
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
      USE I22EDGE_MOD  
      USE REALLOC_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "timerr_c.inc"
#include      "i22edges.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_B(NCAND),CAND_E(NCAND), NCAND, NIN, 
     .        ITASK, NBRIC, ITAB(*),
     .        BUFBRIC(NBRIC), IXS(NIXS,*), II_STOK
      my_real
     .        :: X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
     
      INTEGER :: POS, IAD, IADE, IB ,IBG    , NBF, NBL
      
      INTEGER                        ::  IE, N_CUT_EDGE
     
      INTEGER :: TAG_INDEX(NBRIC), TAG_INDEX_OLD(NBRIC), I8(9,NBRIC)
      my_real :: R9(9,NBRIC), R49(4,9,NBRIC), R84(9,4,NBRIC)
      
      INTEGER :: I,J,IDX,IBold,NUM
      
      LOGICAL :: bFOUND
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C    INT22 > 0 already true if this subroutine 
C               is called
C-----------------------------------------------
C   S o u r c e   L i n e 
C-----------------------------------------------     
    
      NB  = NCANDB
      IF(NB.GT.I22LEN)THEN
        print *, "**error inter22 : memory size allocation", NB,I22LEN
        stop
        !handle this with reallocate later
      ENDIF
      
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NIN = 1

      IF(ITASK==0.AND.DT1.NE.ZERO)THEN
        
        DO IB=1,NBOLD(NIN)
          IE = BRICK_LIST(NIN,IB)%ID
          IIAD22(NIN, IE) = 0
        ENDDO
               
        TAG_INDEX(1:NBRIC) = 0
        DO I=1,NB
          TAG_INDEX(LIST_B(I)) = I
        ENDDO
        
        TAG_INDEX_OLD(1:NBRIC) = 0
        DO I=1,NBOLD(NIN)
          TAG_INDEX_OLD(LIST_B_OLD(NIN,I)) = I
        ENDDO

        !---NewInBuffer--------!        
        DO IB=1,NB
          IDX = LIST_B(IB)                             
          IF(TAG_INDEX(IDX).NE.0 .AND. TAG_INDEX_OLD(IDX).EQ.0)THEN
            BRICK_LIST(NIN,IB)%NewInBuffer = 1
ccc            print *, "new in buffer, IB=", ib, ixs(11,idx)
          ELSE
            BRICK_LIST(NIN,IB)%NewInBuffer = 0            
          ENDIF
        ENDDO!next IB 
c        !ne devrait pas etre necessaire car on ne travaille plus dans cette plage.
c        DO IB=NB+1,NBOLD(NIN)
c          IDX = TAG_INDEX_OLD(IB)
c          BRICK_LIST(NIN,IB)%NewInBuffer = 0 
c        ENDDO       
                               
        !---NodWasMaster-------!
        DO IB=1,NBOLD(NIN)
          I8(1:8,IB) = BRICK_LIST(NIN,IB)%NODE(1:8)%NodWasMaster    !sauvegarder avant de faire la modification ci-dessous
        ENDDO
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            BRICK_LIST(NIN,IDX)%NODE(1:8)%NodWasMaster = I8(1:8,IB)
          ENDIF
        ENDDO!next IB
        !---WhereWasMAster-----!
        DO IB=1,NBOLD(NIN)
          I8(1:8,IB) = BRICK_LIST(NIN,IB)%NODE(1:8)%WhereWasMaster !sauvegarder avant de faire la modification ci-dessous
        ENDDO
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            BRICK_LIST(NIN,IDX)%NODE(1:8)%WhereWasMaster = I8(1:8,IB)
          ENDIF
        ENDDO!next IB
        
        !--Old master volume---!
        DO IB=1,NBOLD(NIN)
          R9(1,IB) = BRICK_LIST(NIN,IB)%Vold_SCell             !sauvegarder avant de faire la modification ci-dessous
        ENDDO
        BRICK_LIST(NIN,1:NBOLD(NIN))%Vold_SCell = ZERO
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            BRICK_LIST(NIN,IDX)%Vold_SCell = R9(1,IB)
          ENDIF
        ENDDO!next IB
        !should be removed since it is finally updated in sinit22 due to topology change.
        
        !--Old whichcellnode-!
        DO IB=1,NBOLD(NIN)
          I8(1:8,IB) = BRICK_LIST(NIN,IB)%NODE(1:8)%WhichCell !sauvegarder avant de faire la modification ci-dessous
        ENDDO
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            BRICK_LIST(NIN,IDX)%NODE(1:8)%OLD_WhichCell = I8(1:8,IB)
          ENDIF
        ENDDO!next IB       
        !--Old current volume---!
        DO IB=1,NBOLD(NIN)
          R9(1:9,IB) = BRICK_LIST(NIN,IB)%POLY(1:9)%Vnew       !sauvegarder avant de faire la modification ci-dessous
        ENDDO
        DO J=1,9;BRICK_LIST(NIN,1:NBOLD(NIN))%POLY(J)%Vnew = -EP30;ENDDO
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            BRICK_LIST(NIN,IDX)%POLY(1:9)%OLD_Vnew = R9(1:9,IB)
          ENDIF
        ENDDO!next IB
                
        !------SlaveList-------!
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            IF(BRICK_LIST(NIN,IB)%SlaveList%Num==0)CYCLE
            OLD_SlaveList(NIN,IDX)%VOL_unmerged       = BRICK_LIST(NIN,IB)%SlaveList%VOL_unmerged
            OLD_SlaveList(NIN,IDX)%Num                = BRICK_LIST(NIN,IB)%SlaveList%Num
            OLD_SlaveList(NIN,IDX)%NumSlaveNodes      = BRICK_LIST(NIN,IB)%SlaveList%NumSlaveNodes
            NUM                                       = BRICK_LIST(NIN,IB)%SlaveList%Num
            DO J=1,NUM                      
              OLD_SlaveList(NIN,IDX)%FM(J)            = BRICK_LIST(NIN,IB)%SlaveList%FM(J)     
              OLD_SlaveList(NIN,IDX)%FV(J)            = BRICK_LIST(NIN,IB)%SlaveList%FV(J)     
              OLD_SlaveList(NIN,IDX)%IV(J)            = BRICK_LIST(NIN,IB)%SlaveList%IV(J)
              IBold                                   = BRICK_LIST(NIN,IB)%SlaveList%IBV(J)  
              OLD_SlaveList(NIN,IDX)%IBV(J)           = TAG_INDEX(LIST_B_OLD(NIN,IBold))
              OLD_SlaveList(NIN,IDX)%ICELLv(J)        = BRICK_LIST(NIN,IB)%SlaveList%ICELLv(J) 
              OLD_SlaveList(NIN,IDX)%VOL(J)           = BRICK_LIST(NIN,IB)%SlaveList%VOL(J)    
              OLD_SlaveList(NIN,IDX)%NumNOD_Cell(J)   = BRICK_LIST(NIN,IB)%SlaveList%NumNOD_Cell(J)  
              OLD_SlaveList(NIN,IDX)%ListNodID(J,1:8) = BRICK_LIST(NIN,IB)%SlaveList%ListNodID(J,1:8) 
              OLD_SlaveList(NIN,IDX)%SURF_V(J)        = BRICK_LIST(NIN,IB)%SlaveList%SURF_v(J)             
            ENDDO 
            DO J=NUM+1,24                     
              OLD_SlaveList(NIN,IDX)%FM(J)            = 0  
              OLD_SlaveList(NIN,IDX)%FV(J)            = 0
              OLD_SlaveList(NIN,IDX)%IV(J)            = 0
              OLD_SlaveList(NIN,IDX)%IBV(J)           = 0
              OLD_SlaveList(NIN,IDX)%ICELLv(J)        = 0
              OLD_SlaveList(NIN,IDX)%VOL(J)           = ZERO 
              OLD_SlaveList(NIN,IDX)%NumNOD_Cell(J)   = 0 
              OLD_SlaveList(NIN,IDX)%ListNodID(J,1:8) = 0                             
            ENDDO              
          ENDIF
        ENDDO!next IB
        !---OldMasterStrongNode----!
        DO IB=1,NBOLD(NIN)
          I8(1,IB) = BRICK_LIST(NIN,IB)%OldMasterStrongNode !sauvegarder avant de faire la modification ci-dessous
        ENDDO
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            BRICK_LIST(NIN,IDX)%OldMasterStrongNode = I8(1,IB)
          ENDIF
        ENDDO!next IB       
        !---OldMasterStrongNode----!
        DO IB=1,NBOLD(NIN)
          I8(1,IB) = BRICK_LIST(NIN,IB)%NBCUT
        ENDDO
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)                               !position dans le nouveau buffer
            BRICK_LIST(NIN,IDX)%WasCut = 0
            IF(I8(1,IB).GT.0)BRICK_LIST(NIN,IDX)%WasCut = 1
          ENDIF
        ENDDO!next IB           

        !---old_ICODE----!
        DO IB=1,NBOLD(NIN)
          I8(1,IB) = BRICK_LIST(NIN,IB)%ICODE                  !sauvegarder avant de faire la modification ci-dessous
        ENDDO
        BRICK_LIST(NIN,1:NB)%OLD_ICODE = 0
        DO IB=1,NBOLD(NIN)
          IDX = LIST_B_OLD(NIN,IB)                             !doit etre alloue dans resol en meme temps que brick_list
          IF(TAG_INDEX(IDX).GT.0)THEN                          !la brick BUFBBRIC(IDX) est toujours dans le buffer
            IDX = TAG_INDEX(IDX)
            BRICK_LIST(NIN,IDX)%OLD_ICODE = I8(1,IB)
          ENDIF
        ENDDO!next IB       
        
                      
      ENDIF!IF(ITASK==0)THEN


      if(itask==0.AND.IBUG22_tri==1)then
        print *, "I22LEN  =", I22LEN
        print *, "II_STOK =", II_STOK      
        print *, "     NB =", NB
      endif
            
      IF(ITASK==0) THEN
        NBOLD(NIN) = NB
        LIST_B_OLD(NIN,1:NB) = LIST_B(1:NB)            
      ENDIF
     
      
      END SUBROUTINE
