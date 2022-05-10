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
      RECURSIVE SUBROUTINE int22ListCombi( ITASK      ,
     .                                     arg_ListFIX, arg_SizeFIX, arg_ListVAR, arg_SizeVAR ,
     .                                     NINTP      , ICODE      , IDBLE      , lvl         ,
     .                                     RESULT     , bFOUND)   
C-----------------------------------------------
C  D e s c r i p t i o n
C-----------------------------------------------
C
C  Recursive combinatory algorithm.
C
C This subroutines determine from a list of 
C potential combinations all possible combination
C taken into account brick nodes used by each
C polyhedra. furthermore if two polyhedron in this
C list of combinationa re using the same edge then
C this edge has to be taged in IDBLE binary code.
C ListFIX : sublist of combination which is used
C           with other taken from ListVAR
C ListVAR : sublist of combination from which 
c           combination are taken to test a 
C           potential combination. Is Updated 
C           to remove incompatibility with
C           combination in ListFIX. Is expanded
C           into new sublist to test new recursively
C           new list os combiantion
C NBITS   : number of bits which represent the
C           number of intersection points (on brick edges)
C           remains UNCHANGED in the recursion process
C ICODE   : Binary code for intersected edges
C           remains UNCHANGED in the recursion process
C IDBLE   : Binary code for multiple intersection edge
C           remains UNCHANGED in the recursion process
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE I22BUFBRIC_MOD
        USE I22TRI_MOD
        USE I22EDGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "subvolumes.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER,INTENT(IN)    :: arg_SizeFIX,arg_SizeVAR  ,NINTP,ICODE,IDBLE
        INTEGER,INTENT(INOUT) :: RESULT(8)
        INTEGER,INTENT(IN)    :: arg_ListFIX(arg_SizeFIX),arg_ListVAR(arg_SizeVAR)    
        LOGICAL,INTENT(INOUT) :: bFOUND 
        INTEGER,INTENT(IN)    :: ITASK
        
        INTEGER :: SizeFIX,SizeVAR 
        INTEGER :: ListFIX(arg_SizeFIX),ListVAR(arg_SizeVAR)           
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,II,J,K,L,COUNT,CountVAR,CountFIX,ITYP,pCODE,lIDBLE
        INTEGER :: LIST(32+8),SizeL
        INTEGER :: IBIN
        INTEGER :: LLVAR(arg_SizeVAR,arg_SizeVAR-1), SizeLLVAR
        INTEGER :: LLFIX(arg_SizeVAR,arg_SizeFIX+1), SizeLLFIX        
        INTEGER :: SizeLLVAR_bak
        INTEGER :: IKEEP
        INTEGER :: TAG(12),TAG_nod(8),TAG_edg(12), Lnbit, Lcode, Ldble, NNOD, NEDG
        INTEGER :: IDOUBLE(12),lvl
        INTEGER :: TAG_nod_fix(8),TAG_edg_fix(12),TAG_nod_bak(8),TAG_nod_tmp(8)
        LOGICAL :: bool,bCOMPL
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
        IF(INT22==0)        RETURN
        IF(bFOUND)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
ccc        print *, "entering int22ListCombi.F"
ccc        print *, "lvl=", lvl, "with ListVar="
ccc        print *, arg_ListVAR(1:arg_SizeVAR)

        !--------------------------------------------------!
        !  CREATING LOCAL COPY                             !
        !--------------------------------------------------! 
        SizeFIX    = arg_SizeFIX
        SizeVAR    = arg_SizeVAR
        ListFIX    = arg_ListFIX
        ListVAR    = arg_ListVAR
        
        
        !--------------------------------------------------!
        !  COUNTING ACTIVATES BITS                         !
        !--------------------------------------------------! 
        COUNT      = 0
        CountFIX   = 0
        CountVAR   = 0
        !accumulation of intersection points recquired by each combination present in the Fixed List.          
        DO I=1,SizeFIX
          pCODE    = IABS(ListFIX(I))
          ITYP     = GetPolyhedraType(pCODE)
          CountFIX = CountFIX + C_POLYH(ITYP)
        ENDDO !next I
        !accumulation of intersection points recquired by each combination present in the Variable List.    
        DO I=1,SizeVAR
          pCODE    = ListVAR(I)
          ITYP     = GetPolyhedraType(pCODE)
          CountVAR = CountVAR + C_POLYH(ITYP)
        ENDDO !next I      
        COUNT = CountFIX + CountVAR

ccc        print *, COUNT,COUNTfix,COUNTVAR,NINTP

        !--------------------------------------------------!
        !  NOT ENOUGH BITS                                 !
        !--------------------------------------------------! 
        IF(COUNT < NINTP .OR. COUNTfix > NINTP)THEN
          !DO NOTHING : LIST OF COMBINATION IS NOT RETAINED
ccc          print *, "   *** SKIPPED"
           RETURN
        !--------------------------------------------------!
        ! EXACT NUMBER OF BITS - CHECK COMBINATION         !
        !--------------------------------------------------! 
        ELSEIF(COUNTfix == NINTP)THEN
          !List concatenation
          SizeL         = SizeFIX !+ SizeVAR
          LIST(1:SizeL) = (/ ListFIX(1:SizeFIX)   /)!,ListVAR(1:SizeVAR) /)
          !checking that corrects edge bits are activated in full list (IOR criteria)
          IBIN = bCODE(IABS(LIST(1)))
          DO I=2,SizeL
            IBIN = IOR(IBIN,bCODE(IABS(LIST(I))))
          ENDDO
          IF(IBIN/=ICODE)then;
ccc            print*,"return1";RETURN;
            endif
          !checking that corrects multiple intersected edge bits are activated in full list(IAND criteria)
          TAG_edg(1:12) = 0 !sum of activated bits for each of 12 ranks
          DO I=1,SizeL
            pCODE    = IABS(LIST(I))
            ITYP     = GetPolyhedraType(pCODE)
            NEDG     = C_POLYH(ITYP)
            DO J=1,NEDG
              K = IABS(Gcorner(J, pCODE))
              TAG_edg(K) = TAG_edg(K) + 1
            ENDDO
          ENDDO
          Lcode = 0
          Ldble = 0
          Lnbit = 0
          DO J=1,12
            IF(TAG_edg(J).GT.2) RETURN
            IF(TAG_edg(J).EQ.2) Ldble = IBSET(Ldble,12-J)            
            IF(TAG_edg(J).GE.1) THEN
              Lcode = IBSET(Lcode,12-J)
              !Lnbit = Lnbit + 1 
            ENDIF
          ENDDO
          IF(ICODE /= LCODE) RETURN
          IF(IDBLE /= LDBLE) RETURN                
          !Storing the correct combination
#include "lockon.inc"
          RESULT(1:SizeL)=LIST(1:SizeL)
          bfound = .TRUE.
#include "lockoff.inc"
        !--------------------------------------------------!
        !  TOO MUCH BITS                                   !
        !--------------------------------------------------! 
        ELSE
c          print *, "   *** EXPAND"
          IF(arg_SizeVAR==0)  RETURN
          !------------------------------------------------!
          ! UPDATE FIXED LIST AND VARIABLE LIST (NEW LOCAL)!
          !------------------------------------------------!         
          !Expanding the varaible sublist & taking each combination in varaible list to add it in a new fix list.
          SizeLLFIX = SizeFIX + 1
          !SizeLLVAR = SizeVAR - 1 
          !SizeLLVAR_bak =  SizeLLVAR        
          DO II=1,SizeVAR        
            IKEEP = ListVAR(II)
            LLFIX(II,1:SizeLLFIX) = (/ListFIX(1:SizeFIX), IKEEP /)
ccc            IF(SizeLLVAR.GE.1)LLVAR(II,1:SizeLLVAR) = (/ListVAR(1:II-1),ListVAR(II+1:SizeVAR)/)  !retirer IKEEP
            SizeLLVAR = SizeVAR-II
            IF(SizeLLVAR.GE.1)LLVAR(II,1:SizeLLVAR) = (/ListVAR(II+1:SizeVAR)/)  !retirer IKEEP
          ENDDO
          DO II=1,SizeVAR
            SizeLLVAR = SizeVAR-II!SizeLLVAR_bak 
ccc            print *, "=====II = ",II
ccc            print *, "=====LLFIX(II,1:SizeLLFIX) = ",LLFIX(II,1:SizeLLFIX)
ccc            print *, "=====LLVAR(II,1:SizeLLVAR) = ",LLVAR(II,1:SizeLLVAR)                       
                                  
            !------------------------------------------------!
            ! CHECKING FIXED LIST CONSISTENCY                !
            !------------------------------------------------!          
            !--verifier nombre de bits
            CountFIX   = 0
            DO I=1,SizeLLFIX
              pCODE    = IABS(LLFIX(II,I))
              ITYP     = GetPolyhedraType(pCODE)
              CountFIX = CountFIX + C_POLYH(ITYP)
            ENDDO !next I        
            IF(CountFIX .GT. NINTP)CYCLE
            !--verifier les sommets isdoles dans LLFIX : doublon impossible
            TAG_nod_fix(1:8) = 0
            DO I=1,SizeLLFIX
              bCOMPL           = .FALSE.
              pCODE            = LLFIX(II,I)
              IF(pCODE.LT.0) bCOMPL = .TRUE.
              pCODE            = IABS(pCODE)
              ITYP             = GetPolyhedraType(pCODE)
              NNOD             = N_POLYH(ITYP)
              bool             = .false.
              TAG_nod_bak(1:8) = TAG_nod_fix(1:8)              
              IF(bCOMPL)THEN
                TAG_nod_tmp(1:8) = 1
                DO J=1,NNOD
                  K = Gnode(J,pCODE)
                  TAG_nod_tmp(K) = 0
                ENDDO
                DO J=1,8
                  IF( TAG_nod_tmp(J)==0)CYCLE
                  TAG_nod_fix(J) = TAG_nod_fix(J)+1
                  IF(TAG_nod_fix(J) .GE. 2)GOTO 50 !already a complementary polyhjedron, the original one is incomaptible with the previous in the list, so exit                
                ENDDO
              ELSE
                DO J=1,NNOD
                  K = Gnode(J,pCODE)
                  TAG_nod_fix(K) = TAG_nod_fix(K)+1
                  IF(TAG_nod_fix(K) .GE. 2)bool=.true.
                ENDDO
              ENDIF
              !this polyhedron also isolate same nodes. Check complementary polyhedron
              IF(BOOL)THEN
                !restor TAG_nod_fix
                TAG_nod_fix(1:8) = TAG_nod_bak(1:8)       
                !check complementary nodes
                TAG_nod_tmp(1:8) = 1
                DO J=1,NNOD
                  K = Gnode(J,pCODE)
                  TAG_nod_tmp(K) = 0
                ENDDO
                DO J=1,8
                  IF( TAG_nod_tmp(J)==0)CYCLE
                  TAG_nod_fix(J) = TAG_nod_fix(J)+1
                  IF(TAG_nod_fix(J) .GE. 2)GOTO 50                 
                ENDDO
                !set negative ID to tag a complementary polyhedron
                LLFIX(II,I) = -LLFIX(II,I)
              ENDIF
            ENDDO !next I        
            !--verifiers les edges de LLFIX : doublon possible seuelemnt si defini dans IDBLE       
            TAG_edg_fix(1:12) = 0
            DO I=1,SizeLLFIX
              pCODE    = IABS(LLFIX(II,I))
              ITYP     = GetPolyhedraType(pCODE)
              NEDG     = C_POLYH(ITYP)
              DO J=1,NEDG
                TAG_edg_fix(IABS(Gcorner(J,pCODE))) = TAG_edg_fix(IABS(Gcorner(J,pCODE)))+1
                IF(TAG_edg_fix(IABS(Gcorner(J,pCODE))) .GT. 2)GOTO 50
                !IF(TAG_edg_fix(IABS(Gcorner(J,pCODE))).EQ.2 .AND. BTEST(IDBLE,12-IABS(Gcorner(J,pCODE)))==0      )GOTO 50  
                
                !check : 14.31 FEM file abS ( 8:15:59) = ../tests/inter22/INTERSECTIONS/ALL-POLY-TEST/EULER/0.FVM/ALL-POLY-TEST_0000.rad                      
                IF(TAG_edg_fix(IABS(Gcorner(J,pCODE))).EQ.2 )THEN
                  IF(BTEST(IDBLE,12-IABS(Gcorner(J,pCODE))) .EQV. (.FALSE.))GOTO 50 
                ENDIF 
              ENDDO
            ENDDO !next I
            !------------------------------------------------!
            ! VARIABLE LIST REDUCTION                        !
            !------------------------------------------------!
            !--verifier les sommets isoles dans LLVAR : doublon impossible
            !conserver les tags de LLVAR ne pas faire de reset : TAG_nod(1:8) = 0 
            I = 1    
ccc            print *, "LLVAR(II,1:SizeLLVAR)=", LLVAR(II,1:SizeLLVAR)      
            DO WHILE(I < SizeLLVAR)
              TAG_nod (:) = TAG_nod_fix
              pCODE    = LLVAR(II,I)
              ITYP     = GetPolyhedraType(pCODE)
              NNOD     = N_POLYH(ITYP)
              DO J=1,NNOD
                K = Gnode(J,pCODE)
                TAG_nod(K) = TAG_nod(K)+1
                IF(TAG_nod(K) .GE. 2)THEN
                  !removing polyhedra from list
ccc                  print *, "     removing_a:", LLVAR(II,I)
ccc                  print *, "     SizeLLVAR,I=:", SizeLLVAR,I                  
                  SizeLLVAR = SizeLLVAR - 1
                  TAG_nod(K) = TAG_nod(K)-1 !restore previous tag value since polhedra is removed from list              
                  L=I
                  DO WHILE(L<=SizeLLVAR)
                    LLVAR(II,L)=LLVAR(II,L+1)
                    L = L + 1
                  ENDDO
ccc                  print *, " after removing : LLVAR(II,1:SizeLLVAR)=", LLVAR(II,1:SizeLLVAR)
                  GOTO 51
                ENDIF
              ENDDO
   51         CONTINUE
              I = I + 1
            ENDDO !next I 
            !--verifiers les edges de LLVAR : doublon possible seuelement si defini dans IDBLE                 
            !conserver les tags de LLFIX, ne pas faire de reset : TAG_edg(1:12) = 0
            I = 1
            DO WHILE(I < SizeLLVAR)
ccc              print *, "testing LLVAR=", LLVAR(II,I)
              Tag_edg(:) = Tag_edg_fix
              pCODE    = LLVAR(II,I)
              ITYP     = GetPolyhedraType(pCODE)
              NEDG     = C_POLYH(ITYP)
              DO J=1,NEDG
                K = IABS(Gcorner(J,pCODE))
                TAG_edg(K) = TAG_edg(K)+1
                !suppression
                IF(TAG_edg(K) .GT. 2 .OR. (TAG_edg(K).EQ.2 .AND. .NOT.BTEST(IDBLE,12-K)))THEN
                  !removing polyhedra from list
ccc                  print *, "     removing_b:", LLVAR(II,I)  
ccc                  print *, "     SizeLLVAR,I=:", SizeLLVAR,I                                     
                  SizeLLVAR = SizeLLVAR - 1
                  TAG_edg(K) = TAG_edg(K)-1 !restore previous tag value since polhedra is removed from list 
                  L = I             
                  DO WHILE (L <= SizeLLVAR)
                    LLVAR(II,L)=LLVAR(II,L+1)
                    L = L + 1
                  ENDDO 
ccc                  print *, " after removing : LLVAR(II,1:SizeLLVAR)=", LLVAR(II,1:SizeLLVAR)                  
                  GOTO 52            
                ENDIF              
              ENDDO
   52         CONTINUE              
              I = I + 1
            ENDDO !next I 
            !------------------------------------------------!
            ! RECURSION                                      !
            !------------------------------------------------!
c            print *, "     passed and call recursion"
            CALL INT22LISTCOMBI(ITASK,LLFIX(II,1:SizeLLFIX),SizeLLFIX,LLVAR(II,1:SizeLLVAR),SizeLLVAR,NINTP,ICODE,IDBLE,lvl+1,
     .                          RESULT, bFOUND)
   50       CONTINUE
          ENDDO ! II=1,SizeVAR
        ENDIF!(COUNT)
      END SUBROUTINE



