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
Chd|  R_BUFBRIC_22                  source/interfaces/int22/r_bufbric_22.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        I22DATAINIT                   source/interfaces/int22/i22datainit.F
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE R_BUFBRIC_22()
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C This subroutine is reading buffer from RESTART file
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD  
      USE I22TRI_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "fluid_scr_c.inc"
#include      "warn_c.inc"
#include      "inter22.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER          :: NIN, I, J, K, IERR1, TRIMATL, N0PHASL, NVPHASL,ib
      INTEGER          :: Isectype(14)  , ISIZ, IASC
      INTEGER, DIMENSION(I22LEN) :: READ_ITAB
      my_real, DIMENSION(I22LEN) :: READ_RTAB

      CHARACTER*14     :: string
C-----------------------------------------------
C   P r e C o n d i t i o n
C-----------------------------------------------  
C     INT22 > 0 - otherwise nothing is done here    
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IERR1 = 0
      TRIMATL = TRIMAT
      N0PHASL = N0PHAS
      NVPHASL = NVPHAS
      IF(TRIMATL.LT.0)then 
        TRIMATL = 0  !law37 TRIMAT =-2 law51 TRIMAT=4
        N0PHASL = 0
        NVPHASL = 0
      ENDIF
C---------------------------------------------------------------------C
C   initialisation du tableau de travail pour l'interface 22 (FSI)    C
C---------------------------------------------------------------------C
      !int22  : number of int22 interface                                                                                                                           
      !i22len : maximum number of bricks  (optim dimensionnement possible)
      IF(INT22>0)THEN      
        IF( IBUG22_CC_LENGTH/=0 )print *, "CUT CELL BUFFER LENGTH, LEN=", I22LEN                                                                                                             
        IF(.NOT.ALLOCATED(NBOLD))            ALLOCATE(NBOLD(INT22)                                            ,STAT=IERR1)                                          
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                     
        IF(.NOT.ALLOCATED(LIST_B_OLD))       ALLOCATE(LIST_B_OLD(INT22,I22LEN)                                ,STAT=IERR1)                                          
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                     
        IF(.NOT.ALLOCATED(BRICK_LIST))       ALLOCATE(BRICK_LIST(INT22,I22LEN)                                ,STAT=IERR1)                                          
        IF(IERR1.GT.0)GOTO 1000
          ISIZ = MAX (I22LAW37, I22LAW51)
          IF(ISIZ.NE.0)THEN
           DO NIN =1,INT22   
             DO IB=1,I22LEN
                 ALLOCATE(BRICK_LIST(NIN,IB)%bakMAT%UVAR(1:ISIZ))
             ENDDO!next IB  
           ENDDO!net NIN 
          ENDIF                                                                                                                             
        IF(.NOT.ALLOCATED(EDGE_LIST))        ALLOCATE(EDGE_LIST(INT22,I22LEN*12)                              ,STAT=IERR1)                                          
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                     
        IF(.NOT.ALLOCATED(IIAD22))           ALLOCATE(IIAD22(INT22,NUMELS+NUMELQ)                             ,STAT=IERR1)                                          
        IF(IERR1.GT.0)GOTO 1000                                                                                                                              
        IF(.NOT.ALLOCATED(UVARL))            ALLOCATE(UVARL(0:NTHREAD-1,I22LEN,N0PHASL+TRIMATL*NVPHASL+I22LAW37) ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                     
        IF(.NOT.ALLOCATED(SuperCellVOL_L))   ALLOCATE(SuperCellVOL_L(0:NTHREAD-1,0:TRIMATL+I22LAW37,I22LEN)             ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(EINT_L))           ALLOCATE(EINT_L(0:NTHREAD-1,I22LEN)                              ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(RHO_L))            ALLOCATE(RHO_L(0:NTHREAD-1,I22LEN)                               ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(MOM_L))            ALLOCATE(MOM_L(1:3,0:NTHREAD-1,I22LEN)                           ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(SIG_L))            ALLOCATE(SIG_L(0:NTHREAD-1,1:6,I22LEN)                           ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(VOLD_L))           ALLOCATE(VOLD_L(0:NTHREAD-1,0:TRIMATL+I22LAW37,I22LEN)                     ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(IMERGEL))          ALLOCATE(IMERGEL(0:NTHREAD-1)                                    ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(OLD_SecndList))    ALLOCATE(OLD_SecndList(INT22,I22LEN)                             ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(UNLINKED_CELLS_L)) ALLOCATE(UNLINKED_CELLS_L(0:NTHREAD-1,2,I22LEN)                  ,STAT=IERR1)          !index2: 1:IB, 2:ICELL                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(N_UNLINKED_L))     ALLOCATE(N_UNLINKED_L(0:NTHREAD-1)                               ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(V22MAX_L))         ALLOCATE(V22MAX_L(0:NTHREAD-1)                                   ,STAT=IERR1)         
        IF(IERR1.GT.0)GOTO 1000                                                                                                                                              
        IF(.NOT.ALLOCATED(DX22MIN_L))        ALLOCATE(DX22MIN_L(0:NTHREAD-1)                                  ,STAT=IERR1)         
        IF(IERR1.GT.0)GOTO 1000    

        IF(.NOT.ALLOCATED(NGB))              ALLOCATE(NGB(1:I22LEN)                                           ,STAT=IERR1)         
        IF(IERR1.GT.0)GOTO 1000    
        IF(.NOT.ALLOCATED(IDB))              ALLOCATE(IDB(1:I22LEN)                                           ,STAT=IERR1)         
        IF(IERR1.GT.0)GOTO 1000    
        IF(.NOT.ALLOCATED(NELB))             ALLOCATE(NELB(1:I22LEN)                                          ,STAT=IERR1)         
        IF(IERR1.GT.0)GOTO 1000    
        IF(.NOT.ALLOCATED(IDLOCB))           ALLOCATE(IDLOCB(1:I22LEN)                                        ,STAT=IERR1)         
        IF(IERR1.GT.0)GOTO 1000   

        IF(.NOT.ALLOCATED(INT22_FCELL_ANIM)) ALLOCATE(INT22_FCELL_ANIM(NUMELS)                                ,STAT=IERR1)                                                   
        IF(IERR1.GT.0)GOTO 1000                                                                                 

        CALL I22DATAINIT()                                                                                                                                                                                                                                                                                                 
        ICAA = 1                                                                                                                                                    
      ENDIF 
      
      !CALL I22DATAINIT_DB()                                                                                                                                                          

C---------------------------------------------------------------------C
C   Lecture du tableau de travail pour l'interface 22 (FSI)           C
C---------------------------------------------------------------------C

      !temporary arg copy is needed while calling READ_* subroutine (ifort & -check arg_temp_created)
      ! readability is better but this is only for restart file and also not a performance issue.

      DO NIN=1,INT22
       !=====general data
                   CALL  READ_I_C( NBOLD(NIN)                                                   ,      1)
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)   
                   LIST_B_OLD(NIN,1:I22LEN)  = READ_ITAB(1:I22LEN)              
       !=====cartesian cell data - INTEGER
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)                              
                   BRICK_LIST(NIN,1:I22LEN)%ID  = READ_ITAB(1:I22LEN)
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)          
                   BRICK_LIST(NIN,1:I22LEN)%NG  = READ_ITAB(1:I22LEN)                    
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)         
                   BRICK_LIST(NIN,1:I22LEN)%IDLOC   = READ_ITAB(1:I22LEN)                     
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%OLD_ICODE  = READ_ITAB(1:I22LEN)
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%OLD_ICODE  = READ_ITAB(1:I22LEN)
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%IDBLE  = READ_ITAB(1:I22LEN)
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%NBITS  = READ_ITAB(1:I22LEN)                              
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)      
                   BRICK_LIST(NIN,1:I22LEN)%NPQTS  = READ_ITAB(1:I22LEN)                        
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)         
                   BRICK_LIST(NIN,1:I22LEN)%NBCUT  = READ_ITAB(1:I22LEN)                     
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)        
                   BRICK_LIST(NIN,1:I22LEN)%MainID  = READ_ITAB(1:I22LEN)                      
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)    
                   BRICK_LIST(NIN,1:I22LEN)%WasCut  = READ_ITAB(1:I22LEN)                          
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)   
                   BRICK_LIST(NIN,1:I22LEN)%NewInBuffer  = READ_ITAB(1:I22LEN)                           
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)  
                   BRICK_LIST(NIN,1:I22LEN)%OldMainStrongNode  = READ_ITAB(1:I22LEN)                            
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)  
                   BRICK_LIST(NIN,1:I22LEN)%MLW  = READ_ITAB(1:I22LEN)                            
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)  
                   BRICK_LIST(NIN,1:I22LEN)%ITASK  = READ_ITAB(1:I22LEN)                            


                   DO J=1,8 
                        CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%SecID_Cell(J)  = READ_ITAB(1:I22LEN) 
                   ENDDO                      
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%Seg_add_LFT  = READ_ITAB(1:I22LEN) 
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%Seg_add_LLT  = READ_ITAB(1:I22LEN) 
                   DO I=1,5
                        DO J=1,6 
                                CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%Adjacent_Brick(J,I)  = READ_ITAB(1:I22LEN) 
                        ENDDO                      
                   ENDDO
                   DO I=1,3
                        DO J=1,5        
                                CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%MergeTarget(I,J)  = READ_ITAB(1:I22LEN) 
                        ENDDO                     
                    ENDDO
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%NTarget  = READ_ITAB(1:I22LEN)                      
                   DO I=1,6 
                        CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%ClosedSurf(I)  = READ_ITAB(1:I22LEN) 
                   ENDDO                      
                   CALL  READ_I_C( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%IsMErgeTarget  = READ_ITAB(1:I22LEN)                  
       !=====cartesian cell data - REAL
                   DO I=1,6
                        DO J=1,3
                                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%N(I,J) = READ_RTAB(1:I22LEN)
                        ENDDO                      
                   ENDDO
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%Vnew_SCell = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%Vold_SCell = READ_RTAB(1:I22LEN)

                   DO I=1,3 
                        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%SCellCenter(I) = READ_RTAB(1:I22LEN)
                   ENDDO
                   DO J=1,6  
                        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%Face_Brick(J) = READ_RTAB(1:I22LEN)
                   ENDDO
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%UncutVol = READ_RTAB(1:I22LEN)
                   DO J=1,3  
                        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%FCELL(J) = READ_RTAB(1:I22LEN)
                   ENDDO
                   DO J=1,3 
                        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%FEXT_CELL(J) = READ_RTAB(1:I22LEN)
                   ENDDO        
                   DO J=0,6  
                        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%SIG(J) = READ_RTAB(1:I22LEN)
                   ENDDO        
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%RHOC = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%MACH = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%DVOL = READ_RTAB(1:I22LEN)
                   DO J=1,2
                        DO I=1,6 
                                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%Poly9woNodes(I,J) = READ_RTAB(1:I22LEN) 
                        ENDDO        
                   ENDDO
       !=====%POLY(1:9) - INTEGER
                   DO J=1,9 
                        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%POLY(J)%NumNOD = READ_ITAB(1:I22LEN)
                   ENDDO                                 
                   DO J=1,9
                        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%POLY(J)%NumPOINT = READ_ITAB(1:I22LEN)
                   ENDDO                      
                   DO I=1,8
                        DO J=1,9 
                                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%ListNodID(I) = READ_ITAB(1:I22LEN)
                        ENDDO                  
                   ENDDO        
                   DO J=1,9 
                        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%POLY(J)%IsMain = READ_ITAB(1:I22LEN)
                   ENDDO                  
                   DO I=1,4
                        DO J=1,9
                                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%WhereIsMain(I) = READ_ITAB(1:I22LEN)
                        ENDDO                   
                   ENDDO
                   DO J=1,9 
                        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%POLY(J)%ID_FREE_NODE = READ_ITAB(1:I22LEN)
                   ENDDO   
       !=====%POLY(1:9) - REAL
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%Adjacent_FLU1 = READ_RTAB(1:I22LEN)
        ENDDO           
        DO J=1,9
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%DDVOL = READ_RTAB(1:I22LEN)
        ENDDO          
        DO J=1,9
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%PHI = READ_RTAB(1:I22LEN)
        ENDDO  
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%dPHI = READ_RTAB(1:I22LEN)
        ENDDO                    
        DO J=1,9
         DO I=1,4 
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%VFRACm(I) = READ_RTAB(1:I22LEN)
         ENDDO         
        ENDDO               
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%Vnew = READ_RTAB(1:I22LEN) 
        ENDDO 
        DO J=1,9
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%Vold = READ_RTAB(1:I22LEN)
        ENDDO 
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%OLD_Vnew = READ_RTAB(1:I22LEN) 
        ENDDO 
        DO I=1,3
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%CellCenter(I) = READ_RTAB(1:I22LEN)
         ENDDO           
        ENDDO
        DO I=0,2
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%DVOL(I) = READ_RTAB(1:I22LEN)
         ENDDO                
        ENDDO
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%cutFace = READ_RTAB(1:I22LEN) 
        ENDDO                
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%DM = READ_RTAB(1:I22LEN) 
        ENDDO                
       !=====%POLY(1:9)%FACE0
        DO J=1,9  
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%NumPOINT = READ_ITAB(1:I22LEN)
        ENDDO    
        DO J=1,9 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%NAdjCell = READ_ITAB(1:I22LEN)
        ENDDO 
        DO I=1,3       
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%Center(I) = READ_RTAB(1:I22LEN)
         ENDDO 
        ENDDO    
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%Surf = READ_RTAB(1:I22LEN)
        ENDDO 
        DO I=1,3        
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%F_FACE(I) = READ_RTAB(1:I22LEN)
         ENDDO 
        ENDDO    
        DO I=1,9       
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%U_N(I) = READ_RTAB(1:I22LEN)
         ENDDO 
        ENDDO 
       !=====%POLY(1:9)%FACE(1:6)
       DO K=1,6
        DO J=1,9 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%NumPOINT = READ_ITAB(1:I22LEN)
        ENDDO      
        DO J=1,9  
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%NAdjCell = READ_ITAB(1:I22LEN)
        ENDDO      
        DO I=1,5       
         DO J=1,9 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Adjacent_Cell(I) = READ_ITAB(1:I22LEN)
         ENDDO      
        ENDDO          
        DO I=1,5       
         DO J=1,9 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Adjacent_FLUX(I) = READ_ITAB(1:I22LEN)
         ENDDO      
        ENDDO 
        DO I=1,5       
         DO J=1,9 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Adjacent_upwFLUX(I) = READ_ITAB(1:I22LEN)
         ENDDO      
        ENDDO 
        DO I=1,3       
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Center(I) = READ_RTAB(1:I22LEN)
         ENDDO      
        ENDDO    
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Surf = READ_RTAB(1:I22LEN)
        ENDDO      
        DO I=1,3        
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Vel(I) = READ_RTAB(1:I22LEN)
         ENDDO      
        ENDDO    
        DO I=1,3        
         DO J=1,9 
          CALL  READ_DB ( READ_RTAB                                 , I22LEN)
          BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%F_FACE(I) = READ_RTAB(1:I22LEN)
         ENDDO      
        ENDDO    
        DO J=1,9 
         CALL  READ_DB ( READ_RTAB                                 , I22LEN)
         BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%U_N = READ_RTAB(1:I22LEN)
        ENDDO      
       ENDDO!next K
       !=====%NODE
        DO J=1,8 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%NODE(J)%OLD_WhichCell = READ_ITAB(1:I22LEN)
        ENDDO                 
        DO J=1,8 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%NODE(J)%WhichCell = READ_ITAB(1:I22LEN)
        ENDDO                 
        DO J=1,8 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%NODE(J)%NodWasMain = READ_ITAB(1:I22LEN)
        ENDDO        
        DO I=1,8 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%NODE(I)%WhereWasMain = READ_ITAB(1:I22LEN)
        ENDDO        
       !=====%ADJ_ELEMS 
        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
        BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%Num = READ_ITAB(1:I22LEN)             
        DO J=1,48
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%Num_inv(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%IV(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%IB(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%ICELL(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%SecndFACE(J) = READ_ITAB(1:I22LEN)
        ENDDO !next J
       !=====%PCUT
        DO I=1,16
                   CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%NP = READ_ITAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%N(1) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%N(2) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%N(3) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%B(1) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%B(2) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%B(3) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%SCUT(1) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%SEFF(1) = READ_RTAB(1:I22LEN)
                   DO J=1,8
                    CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                    BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%P(1,J) = READ_RTAB(1:I22LEN)
                    CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                    BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%P(2,J) = READ_RTAB(1:I22LEN)
                    CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                    BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%P(3,J) = READ_RTAB(1:I22LEN)
                   ENDDO !next J
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%Vel(1) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%Vel(2) = READ_RTAB(1:I22LEN)
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%Vel(3) = READ_RTAB(1:I22LEN)
        ENDDO !next J  
       !=====%SecndList
        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
        BRICK_LIST(NIN,1:I22LEN)%SecndList%VOL_Unmerged = READ_RTAB(1:I22LEN)
        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
        BRICK_LIST(NIN,1:I22LEN)%SecndList%Num = READ_ITAB(1:I22LEN)         
        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
        BRICK_LIST(NIN,1:I22LEN)%SecndList%NumSecndNodes = READ_ITAB(1:I22LEN)   
        DO J=1,24
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%FM(J) = READ_ITAB(1:I22LEN)        
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%FV(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%IV(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%IBV(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%ICELLv(J) = READ_ITAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%VOL(J) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%SURF_v(J) = READ_RTAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%SecndList%NumNOD_Cell(J) = READ_ITAB(1:I22LEN)
                DO K=1,8                     
                        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                        BRICK_LIST(NIN,1:I22LEN)%SecndList%ListNodID(J,K) = READ_ITAB(1:I22LEN)
                ENDDO
        ENDDO !next J
       !=====%Edge
        DO J=1,12
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%NODE(1) = READ_ITAB(1:I22LEN) 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%NODE(2) = READ_ITAB(1:I22LEN)      
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%NBCUT = READ_ITAB(1:I22LEN) 
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTSHELL(1) = READ_ITAB(1:I22LEN)             
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTSHELL(2) = READ_ITAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTCOOR(1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTCOOR(2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(1,1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(2,1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(3,1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(1,2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(2,2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(3,2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(1,1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(2,1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(3,1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(1,2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(2,2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(3,2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%VECTOR(1) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%VECTOR(2) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%VECTOR(3) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                BRICK_LIST(NIN,1:I22LEN)%Edge(J)%LEN = READ_RTAB(1:I22LEN)
        ENDDO        
       !=====%SECTYPE
        DO J=1,8
          DO I=1,I22LEN  
           DO K=1,14        
            CALL  READ_I_C( IASC, 1)
            STRING(K:K) = CHAR(IASC)
           ENDDO    
           BRICK_LIST(NIN,I)%SECTYPE(J) = STRING
          ENDDO
        ENDDO
       !=====%bakMAT
        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
        BRICK_LIST(NIN,1:I22LEN)%bakMAT%rho = READ_RTAB(1:I22LEN)
        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
        BRICK_LIST(NIN,1:I22LEN)%bakMAT%rhoE = READ_RTAB(1:I22LEN)
        DO I=1,3
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%bakMAT%rhoU(I) = READ_RTAB(1:I22LEN)
        ENDDO 
        DO I=1,6 
                   CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                   BRICK_LIST(NIN,1:I22LEN)%bakMAT%sig(I) = READ_RTAB(1:I22LEN)
        ENDDO 
        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
        BRICK_LIST(NIN,1:I22LEN)%bakMAT%ssp = READ_RTAB(1:I22LEN)
        ISIZ = MAX (I22LAW37, I22LAW51)
        IF(ISIZ.NE.0)THEN
          DO K=1,I22LEN
           DO I=1,ISIZ 
                   CALL  READ_DB ( BRICK_LIST(NIN,K)%bakMAT%UVAR(I)                             , 1)
           ENDDO 
          ENDDO
        ENDIF     

       !=====%OLD_SecndList(NIN,1:I22LEN)
        CALL  READ_DB ( READ_RTAB                                 , I22LEN)
        OLD_SecndList(NIN,1:I22LEN)%VOL_Unmerged = READ_RTAB(1:I22LEN)
        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
        OLD_SecndList(NIN,1:I22LEN)%Num = READ_ITAB(1:I22LEN)   
        CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
        OLD_SecndList(NIN,1:I22LEN)%NumSecndNodes = READ_ITAB(1:I22LEN)
        DO J=1,24
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%FM(J) = READ_ITAB(1:I22LEN)    
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%FV(J) = READ_ITAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%IV(J) = READ_ITAB(1:I22LEN)     
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%IBV(J) = READ_ITAB(1:I22LEN)    
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%ICELLv(J) = READ_ITAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%VOL(J) = READ_RTAB(1:I22LEN)
                CALL  READ_DB ( READ_RTAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%SURF_v(J) = READ_RTAB(1:I22LEN)
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%NumNOD_Cell(J) = READ_ITAB(1:I22LEN)
                   DO K=1,8              
                CALL  READ_I_C ( READ_ITAB                                 , I22LEN)
                OLD_SecndList(NIN,1:I22LEN)%ListNodID(J,K) = READ_ITAB(1:I22LEN)
                   ENDDO
        ENDDO !next J
  
        
      ENDDO !next NIN 
      
      
C----------------------------------------------- 
 1000 CONTINUE
      IF(INT22>0)THEN
        IF (IERR1.NE.0) THEN
          WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION (INTER22)'
          WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION (INTER22)'
          CALL ARRET(2)
        END IF 
      ENDIF!(INT22>0)
C-----------------------------------------------      
        
      END SUBROUTINE
