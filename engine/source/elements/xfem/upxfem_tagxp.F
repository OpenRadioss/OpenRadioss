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
Chd|  UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRK_TAGXP3                    source/elements/xfem/upxfem_tagxp.F
Chd|        CRK_TAGXP4                    source/elements/xfem/upxfem_tagxp.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        UPENRIC3_N3                   source/elements/xfem/upenric3_nx.F
Chd|        UPENRIC3_N4                   source/elements/xfem/upenric3_nx.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPXFEM_TAGXP(XFEM_TAB,
     .                        IPARG    ,IXC     ,NGROUC  ,IGROUC  ,IXTG    ,
     .                        IADC_CRK ,IEL_CRK ,INOD_CRK,ELCUTC  ,NODEDGE ,
     .                        ENRTAG   ,CRKEDGE ,XEDGE4N ,XEDGE3N ,ITAB    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
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
#include      "com_xfem1.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),NGROUC,IGROUC(*),IEL_CRK(*),
     .   ELCUTC(2,*),IADC_CRK(*),INOD_CRK(*),IXTG(NIXTG,*),ITAB(*),
     .   NODEDGE(2,*),ENRTAG(NUMNOD,*),XEDGE4N(4,*),XEDGE3N(3,*)
C
      TYPE(ELBUF_STRUCT_), TARGET ,DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,NG,JFT,JLT,NEL,NXLAY,NF1,IXFEM,OFF,ITG1,ITG2,ACTIFXFEM
C=======================================================================
      ITG1 = 1+NUMELC
      ITG2 = 1+4*ECRKXFEC
C-----------------------------------------------
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO IG = 1, NGROUC
          NG = IGROUC(IG)
          OFF   = IPARG(8,NG)
          IXFEM = IPARG(54,NG) * IPARG(70,NG)  ! IXFEM * ACTIVEXFEM
          IF (OFF == 0 .and. IXFEM > 0) THEN
            IF (IDDW > 0) CALL STARTIMEG(NG)                                    
C---
            ITY   = IPARG(5,NG)                                                 
            NEL   = IPARG(2,NG)                                                 
            NFT   = IPARG(3,NG)                                                 
            NXLAY = IPARG(59,NG)                                                
            LFT   = 1                                                           
            LLT   = MIN(NVSIZ,NEL)                                              
            JFT = LFT                                                           
            JLT = LLT                                                           
C---
            IF (ITY == 3) THEN                                                
              CALL CRK_TAGXP4(IPARG  ,IXC     ,NFT    ,JFT     ,JLT   ,       
     .                        ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,ENRTAG,         
     .                        NXLAY  ,CRKEDGE ,XEDGE4N,ITAB    )                        
            ELSEIF (ITY == 7) THEN                                            
              CALL CRK_TAGXP3(                                                
     .          IPARG         ,IXTG          ,NFT          ,JFT     ,JLT   ,  
     .          ELCUTC(1,ITG1),IADC_CRK(ITG2),IEL_CRK(ITG1),INOD_CRK,ENRTAG,  
     .          NXLAY         ,CRKEDGE       ,XEDGE3N      ,ITAB    )                  
            ENDIF                                                             
C---
            IF (IDDW > 0) CALL STOPTIMEG(NG)                                  
          ENDIF
        ENDDO
!$OMP END DO
C-----------------------------------------------
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO IG = 1, NGROUC
          NG = IGROUC(IG)
          OFF   = IPARG(8,NG)
          IXFEM = IPARG(54,NG) * IPARG(70,NG)  ! IXFEM * ACTIVEXFEM
          IF (OFF == 0 .and. IXFEM > 0) THEN
          IF (IDDW > 0) CALL STARTIMEG(NG)
C---
            ITY   = IPARG(5,NG)
            NEL   = IPARG(2,NG)
            NFT   = IPARG(3,NG)
            LFT   = 1 
            LLT   = MIN(NVSIZ,NEL)
            JFT=LFT
            JLT=LLT
C---
            IF (ITY == 3) THEN
              CALL UPENRIC3_N4(XFEM_TAB(NG,1:NXEL),
     .                     IPARG    ,IXC     ,NFT    ,JFT     ,JLT  ,
     .                     ELCUTC   ,IADC_CRK,IEL_CRK,INOD_CRK,IXFEM,
     .                     CRKEDGE  ,XEDGE4N )
            ELSEIF (ITY == 7) THEN
              CALL UPENRIC3_N3(XFEM_TAB(NG,1:NXEL),
     .            IPARG         ,IXTG          ,NFT          ,JFT     ,JLT  ,
     .            ELCUTC(1,ITG1),IADC_CRK(ITG2),IEL_CRK(ITG1),INOD_CRK,IXFEM,
     .            CRKEDGE       ,XEDGE3N       )
            ENDIF
C---
            IF (IDDW > 0) CALL STOPTIMEG(NG)
          ENDIF
        ENDDO
!$OMP END DO
c-------------
      RETURN
      END
