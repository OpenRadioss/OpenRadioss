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
Chd|  WRITE_CUT_CELL_BUFFER         source/interfaces/int22/write_cut_cell_buffer.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE WRITE_CUT_CELL_BUFFER ()      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C This subroutine was introduce for debug purpose
C only. The aim is to print full Cut Cell Buffer
C content and make comparison with openMP result.
C Any difference enable to leads to OpenMP issue.
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INITBUF_MOD
      USE I22BUFBRIC_MOD 
      USE I22TRI_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"   
#include      "inter22.inc"
#include      "tabsiz_c.inc"
#include      "mvsiz_p.inc"
#include      "comlock.inc"
#include      "subvolumes.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
!      CHARACTER,INTENT(IN) :: FILENAME*(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                             :: OMP_GET_THREAD_NUM, IB, ITSK, IUNIT, NIN, J
      EXTERNAL                            :: OMP_GET_THREAD_NUM   
      TYPE(BRICK_ENTITY)         ,POINTER :: pCC
      TYPE(LIST_ADJ)             ,POINTER :: pADJ
      TYPE(CUT_PLANE)            ,POINTER :: pPLANE
      TYPE(LIST_SECND)           ,POINTER :: pSECND      
      TYPE(EDGE_ENTITY)          ,POINTER :: pEDGE
C-----------------------------------------------
C   P r e c o n d i t i o n s
C----------------------------------------------- 
      CALL MY_BARRIER()
      ITSK = OMP_GET_THREAD_NUM()
      
      IF(ITSK/=0)      RETURN
      IF(INT22 == 0) RETURN
C-----------------------------------------------
C   S o u r c e    L i n e s
C-----------------------------------------------
 
      print *, ""
      print *, "  |-------------------------------|"
      print *, "  |    WRITE_CUT_CELL_BUFFER.F    |"
      print *, "  |-------------------------------|"

      NIN = 1

      IUNIT = 2201
      OPEN( unit=IUNIT, file = "CutCellBuffer.txt" ) 
      WRITE(unit=IUNIT,  fmt='(A,I10)') "Writing Cut Cell Buffer, NC=", NCYCLE
      
      
      DO IB=1,NB
        pCC => BRICK_LIST(NIN,IB)
        !INTEGERS
        WRITE(unit=IUNIT,  fmt='(A ,2I10)')    "  (IB,iID)=", IB,pCC%ID
        WRITE(unit=IUNIT,  fmt='(A,11I10)')    " 1.", pCC%NG, pCC%IDLOC, pCC%ICODE, pCC%IDBLE, pCC%NBITS, pCC%NPQTS, pCC%NBCUT      
     .                                       , pCC%MainID , pCC%OldMainStrongNode, pCC%MLW, pCC%ITASK        
        WRITE(unit=IUNIT,  fmt='(A,08I10)    ')" 2.", pCC%SECID_Cell(1:8)     
        WRITE(unit=IUNIT,  fmt='(A,08I10)    ')" 3.", pCC%NODE(1:8)%OLD_WhichCell         
        WRITE(unit=IUNIT,  fmt='(A,08I10)    ')" 4.", pCC%NODE(1:8)%WhichCell         
        WRITE(unit=IUNIT,  fmt='(A,09I10)    ')" 5.", pCC%POLY(1:9)%NumNOD        
        WRITE(unit=IUNIT,  fmt='(A,09I10)    ')" 6.", pCC%POLY(1:9)%NumPOINT          
        WRITE(unit=IUNIT,  fmt='(A,54I10)    ')" 7.", (/ (pCC%POLY(1:9)%FACE(J)%NumPOINT,J=1,6) /)
        WRITE(unit=IUNIT,  fmt='(A,01I10)    ')" 8.", pCC%Seg_add_LFT          
        WRITE(unit=IUNIT,  fmt='(A,01I10)    ')" 9.", pCC%Seg_add_LLT          
        WRITE(unit=IUNIT,  fmt='(A,30I10)    ')"10.", pCC%Adjacent_Brick(1:6,1:5)
        WRITE(unit=IUNIT,  fmt='(A,270I10)   ')"11.", (/ 
     .                                                    (pCC%POLY(1:9)%FACE(J)%Adjacent_Cell(1),J=1,6),
     .                                                    (pCC%POLY(1:9)%FACE(J)%Adjacent_Cell(2),J=1,6),
     .                                                    (pCC%POLY(1:9)%FACE(J)%Adjacent_Cell(3),J=1,6),
     .                                                    (pCC%POLY(1:9)%FACE(J)%Adjacent_Cell(4),J=1,6),
     .                                                    (pCC%POLY(1:9)%FACE(J)%Adjacent_Cell(5),J=1,6)               
     .                                                 /)       
        WRITE(unit=IUNIT,  fmt='(A,54I10)    ')"12.", (/ (pCC%POLY(1:9)%FACE(J)%NAdjCell  ,J=1,6) /)        
        WRITE(unit=IUNIT,  fmt='(A,72I10)    ')"13.", (/ (pCC%POLY(1:9)%ListNodID(J)      ,J=1,8) /)          
        WRITE(unit=IUNIT,  fmt='(A,09I10)    ')"14.", pCC%POLY(1:9)%IsMain          
        WRITE(unit=IUNIT,  fmt='(A,08I10)    ')"15.", pCC%NODE(1:8)%NodWasMain                                                                                        
        WRITE(unit=IUNIT,  fmt='(A,36I10)    ')"16.", (/ (pCC%POLY(1:9)%WhereIsMain(J),J=1,4) /)
        WRITE(unit=IUNIT,  fmt='(A,08I10)    ')"17.", pCC%NODE(1:8)%WhereWasMain                       
        WRITE(unit=IUNIT,  fmt='(A,15I10)    ')"18.", pCC%MergeTarget(1:3,1:5)            
        WRITE(unit=IUNIT,  fmt='(A,01I10)    ')"19.", pCC%Ntarget

        !DOUBLE PRECISION
        WRITE(unit=IUNIT,  fmt='(A,270F20.14)')"21.", (/
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_FLUX(1),J=1,6) ,          
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_FLUX(2),J=1,6) ,
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_FLUX(3),J=1,6) ,
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_FLUX(4),J=1,6) ,
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_FLUX(5),J=1,6) 
     .                                                 /)          
        WRITE(unit=IUNIT,  fmt='(A,270F20.14)')"22.", (/
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_upwFLUX(1),J=1,6) ,          
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_upwFLUX(2),J=1,6) ,
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_upwFLUX(3),J=1,6) ,
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_upwFLUX(4),J=1,6) ,
     .                                                  (pCC%POLY(1:9)%FACE(J)%Adjacent_upwFLUX(5),J=1,6) 
     .                                                 /)         
        WRITE(unit=IUNIT,  fmt='(A,09F20.14) ')"23.", pCC%POLY(1:9)%Adjacent_FLU1          
        WRITE(unit=IUNIT,  fmt='(A,18F20.14) ')"24.", pCC%N(1:6,1:3)         
        WRITE(unit=IUNIT,  fmt='(A,09F20.14) ')"25.", pCC%POLY(1:9)%PHI       
        WRITE(unit=IUNIT,  fmt='(A,09F20.14) ')"26.", pCC%POLY(1:9)%DPHI        
        WRITE(unit=IUNIT,  fmt='(A,01F20.14) ')"28.", pCC%Vnew_SCell         
        WRITE(unit=IUNIT,  fmt='(A,01F20.14) ')"29.", pCC%Vold_SCell     
        WRITE(unit=IUNIT,  fmt='(A,09F20.14) ')"30.", pCC%POLY(1:9)%Vnew  
        WRITE(unit=IUNIT,  fmt='(A,09F20.14) ')"31.", pCC%POLY(1:9)%Vold        
        WRITE(unit=IUNIT,  fmt='(A,09F20.14) ')"32.", pCC%POLY(1:9)%OLD_Vnew   
        WRITE(unit=IUNIT,  fmt='(A,18F20.14) ')"33.", (/ (pCC%POLY(1:9)%CellCenter(J),J=1,3) /)         
        WRITE(unit=IUNIT,  fmt='(A,63F20.14) ')"34.", (/  (pCC%POLY(1:9)%Face(J)  ,J=0,6) /)       
        WRITE(unit=IUNIT,  fmt='(A,06F20.14) ')"35.", pCC%Face_Brick(1:6)         
        WRITE(unit=IUNIT,  fmt='(A,01F20.14) ')"36.", pCC%UncutVol         
        
        !ADJACENT CELL BUFFER
        pADJ => BRICK_LIST(NIN,IB)%ADJ_ELEMS
        DO J=1,48
        WRITE(unit=IUNIT,  fmt='(A,01I10)    ')"   ", pADJ%Num        
        WRITE(unit=IUNIT,  fmt='(A,48I10)    ')"   ", pADJ%Num_inv(1:48)        
        WRITE(unit=IUNIT,  fmt='(A,48I10)    ')"   ", pADJ%IV(1:48)          
        WRITE(unit=IUNIT,  fmt='(A,48I10)    ')"   ", pADJ%IB(1:48)         
        WRITE(unit=IUNIT,  fmt='(A,48I10)    ')"   ", pADJ%ICELL(1:48)                                             
        WRITE(unit=IUNIT,  fmt='(A,48I10)    ')"   ", pADJ%SecndFACE(1:48)                   
        ENDDO

        !CUT PLANE BUFFER
        DO J=1,8
        pPLANE => BRICK_LIST(NIN,IB)%PCUT(J)         
        WRITE(unit=IUNIT,  fmt='(A,03F20.14)' )"   ", pPLANE%N(1:3)        
        WRITE(unit=IUNIT,  fmt='(A,03F20.14) ')"   ", pPLANE%B(1:3)
        WRITE(unit=IUNIT,  fmt='(A,01F20.14) ')"   ", pPLANE%SCUT         
        WRITE(unit=IUNIT,  fmt='(A,01F20.14) ')"   ", pPLANE%SEFF
        WRITE(unit=IUNIT,  fmt='(A,18F20.14) ')"   ", pPLANE%P(1:3,1:6)                                    
        WRITE(unit=IUNIT,  fmt='(A,01I10)    ')"   ", pPLANE%NP
        ENDDO
        
        !SECND CELL LIST
        pSECND => BRICK_LIST(NIN,IB)%SecndList        
        WRITE(unit=IUNIT,  fmt='(A,01F20.14)' )"   ", pSECND%VOL_Unmerged         
        WRITE(unit=IUNIT,  fmt='(A,01I10)' )   "   ", pSECND%Num                 
        WRITE(unit=IUNIT,  fmt='(A,01I10)' )   "   ", pSECND%NumSecndNodes                 
        WRITE(unit=IUNIT,  fmt='(A,24I10)' )   "   ", pSECND%FM(1:24)                  
        WRITE(unit=IUNIT,  fmt='(A,24I10)' )   "   ", pSECND%FV(1:24) 
        WRITE(unit=IUNIT,  fmt='(A,24I10)' )   "   ", pSECND%IV(1:24) 
        WRITE(unit=IUNIT,  fmt='(A,24I10)' )   "   ", pSECND%IBV(1:24) 
        WRITE(unit=IUNIT,  fmt='(A,24I10)' )   "   ", pSECND%ICELLv(1:24)                                     
        WRITE(unit=IUNIT,  fmt='(A,24F20.14)' )"   ", pSECND%VOL(1:24)

        !EDGE BUFFER
        DO J=1,12
        pEDGE => BRICK_LIST(NIN,IB)%Edge(J)
        WRITE(unit=IUNIT,  fmt='(A,02I10)' )   "   ", pEDGE%NODE(1:2)  
        WRITE(unit=IUNIT,  fmt='(A,01I10)' )   "   ", pEDGE%NBCUT
        WRITE(unit=IUNIT,  fmt='(A,02I10)' )   "   ", pEDGE%CUTSHELL(1:2)                  
        WRITE(unit=IUNIT,  fmt='(A,02F20.14)') "   ", pEDGE%CUTCOOR(1:2)          
        WRITE(unit=IUNIT,  fmt='(A,06F20.14)') "   ", pEDGE%CUTPOINT(1:3,1:2)                   
        WRITE(unit=IUNIT,  fmt='(A,06F20.14)') "   ", pEDGE%CUTVEL(1:3,1:2)
        WRITE(unit=IUNIT,  fmt='(A,03F20.14)') "   ", pEDGE%VECTOR(1:3)                                     
        WRITE(unit=IUNIT,  fmt='(A,01F20.14)') "   ", pEDGE%LEN
        ENDDO

        !SECTYPES
        WRITE(unit=IUNIT,  fmt='(8A14)' )    "   ", (pCC%SECTYPE(J),J=1,8)
                                                          
       ENDDO

C      pause

C-----------------------------------------------
      RETURN
      END SUBROUTINE WRITE_CUT_CELL_BUFFER


