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
Chd|  XFEM_CRACK_INIT               source/elements/xfem/xfem_crack_init.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE XFEM_CRACK_INIT(
     .           IPARG   ,IXC     ,IXTG    ,INOD_CRK ,NODLEVXF,
     .           INDX_CRK,NCRKPART,CRKSHELL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCRKPART
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),INOD_CRK(*),
     .        NODLEVXF(*),INDX_CRK(*)
      TYPE (XFEM_SHELL_)  , DIMENSION(NLEVMAX)     :: CRKSHELL
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER NG,ITY,LFT,LLT,N,I,J,IPRT,NEL,NFT,IXFEM,ILEV,NCOUNT,
     .   SHCOUNT,NCOUNTG
      INTEGER TAG_ARRAY(NUMNOD),TAG_SHELLS_C(NUMELC),TAG_SHELLS_TG(NUMELTG),
     .   NLEVXF,NN(4),TYPE_SHELLS_C(NUMELC),TYPE_SHELLS_TG(NUMELTG)
C=======================================================================
      DO ILEV=1,NLEVMAX
        CRKSHELL(ILEV)%CRKNUMSHELL = 0
        CRKSHELL(ILEV)%CRKNUMSH4   = 0
        CRKSHELL(ILEV)%CRKNUMSH3   = 0
      ENDDO
C-----------------------------------------------
      DO ILEV=1,NLEVMAX
        TAG_ARRAY=0
        TAG_SHELLS_C=0
        TAG_SHELLS_TG=0
        SHCOUNT =0
        TYPE_SHELLS_C=0
        TYPE_SHELLS_TG=0
c
        DO NG=1,NGROUP          
          IXFEM =IPARG(54,NG)   
          IF (IXFEM > 0) THEN   
            NEL   =IPARG(2,NG)   
            NFT   =IPARG(3,NG)   
            ITY   =IPARG(5,NG)   
            NLEVXF=IPARG(65,NG)  
            LFT=1                
            LLT=NEL              
            IF (ITY == 3) THEN
C-----------------------------------------------
C             COQUES 4N
C-----------------------------------------------
              DO I=LFT,LLT                                  
                N = I + NFT                                 
                TAG_ARRAY(IXC(2,N))=1                                     
                TAG_ARRAY(IXC(3,N))=1                                     
                TAG_ARRAY(IXC(4,N))=1                                     
                TAG_ARRAY(IXC(5,N))=1                                     
C
                NN(1) = INOD_CRK(IXC(2,N))                                
                NN(2) = INOD_CRK(IXC(3,N))                                
                NN(3) = INOD_CRK(IXC(4,N))                                
                NN(4) = INOD_CRK(IXC(5,N))                                
C
                NODLEVXF(NN(1)) = NLEVXF                                  
                NODLEVXF(NN(2)) = NLEVXF                                  
                NODLEVXF(NN(3)) = NLEVXF                                  
                NODLEVXF(NN(4)) = NLEVXF                                  
C
                TAG_SHELLS_C(N) = ILEV                                    
                TYPE_SHELLS_C(N) = 4                                      
                CRKSHELL(ILEV)%CRKNUMSHELL = CRKSHELL(ILEV)%CRKNUMSHELL+1 
                CRKSHELL(ILEV)%CRKNUMSH4   = CRKSHELL(ILEV)%CRKNUMSH4+1                    
              ENDDO                                         
            ELSEIF (ITY == 7) THEN
C-----------------------------------------------
C             COQUES 3N
C-----------------------------------------------
              DO I=LFT,LLT                                  
                N = I + NFT                                 
                TAG_ARRAY(IXTG(2,N))=1                      
                TAG_ARRAY(IXTG(3,N))=1                      
                TAG_ARRAY(IXTG(4,N))=1                      
C
                NN(1) = INOD_CRK(IXTG(2,N))                 
                NN(2) = INOD_CRK(IXTG(3,N))                 
                NN(3) = INOD_CRK(IXTG(4,N))                 
C
                NODLEVXF(NN(1)) = NLEVXF                    
                NODLEVXF(NN(2)) = NLEVXF                    
                NODLEVXF(NN(3)) = NLEVXF                    
C
                TAG_SHELLS_TG(N) = ILEV                     
                TYPE_SHELLS_TG(N) = 3                       
                CRKSHELL(ILEV)%CRKNUMSHELL = CRKSHELL(ILEV)%CRKNUMSHELL+1    
                CRKSHELL(ILEV)%CRKNUMSH3   = CRKSHELL(ILEV)%CRKNUMSH3+1      
              ENDDO                                         
            ENDIF
          ENDIF  ! IXFEM > 0
        ENDDO    ! NG=1,NGROUP
C-------------------------------
C       CRACK Shells Post treatments
C-------------------------------
        SHCOUNT = CRKSHELL(ILEV)%CRKNUMSHELL
        ALLOCATE (CRKSHELL(ILEV)%PHANTOML(SHCOUNT))
        ALLOCATE (CRKSHELL(ILEV)%ELTYPE(SHCOUNT))
C
        SHCOUNT=0
        DO I=1, NUMELC
           IF (TAG_SHELLS_C(I) > 0)THEN
               SHCOUNT = SHCOUNT+1
               CRKSHELL(ILEV)%PHANTOML(SHCOUNT)=I
               CRKSHELL(ILEV)%ELTYPE(SHCOUNT)=TYPE_SHELLS_C(I)
           ENDIF
        ENDDO
C
        DO I=1,NUMELTG
           IF (TAG_SHELLS_TG(I) > 0)THEN
               SHCOUNT = SHCOUNT+1
               CRKSHELL(ILEV)%PHANTOML(SHCOUNT)=I
               CRKSHELL(ILEV)%ELTYPE(SHCOUNT)=TYPE_SHELLS_TG(I)
           ENDIF
        ENDDO
C
      ENDDO  !       DO ILEV=1,NLEVMAX
c
C-----------------------------------------------------------------------
C     Numeros globaux des shell crack (by level)
C-------------------------------
      NCOUNT = 0
      DO ILEV=1,NLEVMAX
        ALLOCATE(CRKSHELL(ILEV)%PHANTOMG(CRKSHELL(ILEV)%CRKNUMSHELL))
C
        DO I=1,CRKSHELL(ILEV)%CRKNUMSHELL
          NCOUNT=NCOUNT+1
          CRKSHELL(ILEV)%PHANTOMG(I)=NCOUNT
        ENDDO
      ENDDO
C
C Crack Nodes Post treatments
C
C-------------------------------
C Crack parts (by level)
C-------------------------------
      DO ILEV=1,NLEVMAX
        IF (CRKSHELL(ILEV)%CRKNUMSHELL > 0) THEN
          NCRKPART = NCRKPART + 1
          INDX_CRK(NCRKPART) = ILEV
        ENDIF
      ENDDO
C
C-------------------------------
C Numeros globaux des NODES (unconnected) for shell crack (by level)
C-------------------------------
C
      NCOUNTG = 0
      DO ILEV=1,NLEVMAX
        ALLOCATE(CRKSHELL(ILEV)%XNODEG(4,CRKSHELL(ILEV)%CRKNUMSHELL))
        ALLOCATE(CRKSHELL(ILEV)%XNODEL(4,CRKSHELL(ILEV)%CRKNUMSHELL))
C
        NCOUNT = 0
        DO I=1,CRKSHELL(ILEV)%CRKNUMSHELL
         DO J=1,4
           NCOUNT = NCOUNT + 1
           NCOUNTG = NCOUNTG + 1
           CRKSHELL(ILEV)%XNODEG(J,I) = NCOUNTG ! global num de phantom nodes 
           CRKSHELL(ILEV)%XNODEL(J,I) = NCOUNT  ! or = 4*(I -1) + J = Num nodes phantomes d'un seul ilev
         END DO
        ENDDO
      ENDDO
C---
      NCRKNODG = 4*CRKSHELL(1)%CRKNUMSHELL ! = NCOUNT
C-----------
      RETURN
      END
