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
Chd|  INTTI2                        source/interfaces/interf/intti2.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTTI12V                      source/interfaces/interf/intti12.F
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTTI2(IPARI,X     ,V      ,
     .                  A    ,VR    ,AR    ,KHIE  ,MS     ,
     .                  IN   ,WEIGHT,WA    ,SKEW  ,
     .                  INTBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), WEIGHT(*),KHIE
C     REAL
      my_real
     .   X(*), V(*), A(*),AR(*),VR(*), MS(*),IN(*),
     .   WA(*),SKEW(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY, JI, JB, NMN, NINT,K,ITIED,ILAGM
      INTEGER :: ILEV,II,J
C-----------------------------------------------
C LES WA NE SONT PLUS UTILISES !!! 
C=======================================================================
      DO N=NINTER,1,-1
        NTY   = IPARI(7,N)                                             
        ILAGM = 0                                                     
        ILAGM = IPARI(33,N)                                           
        IF (ILAGM/=0) CYCLE                                   
        IF (NPARI>=26)THEN                                          
          IF(IPARI(26,N)/=KHIE) CYCLE                           
        ENDIF                                                         
        JI  =IPARI(1,N)                                                 
        JB  =IPARI(2,N)                                                 
        NMN =IPARI(6,N)                                             
        NINT=N                                                        
        IF (NTY == 2) THEN                                            
          IF(IPARI(5,N)>0) THEN !       check if NSN=IPARI(5,N)>0
        !       Optimization :
        !       If IPARI(5,N)=NSN=0, some variables are loaded in INTTI2V
        !       if the number of TYPE2 interface is important (>3000) and if the number of
        !       spmd domain is quite important (NSPMD>50), the initialisation time is important :
        !       for_array_copy_in and other initialisations represent up to 5% of total CPUTIME
        !       and break the scalability of the code
           CALL INTTI2V(                                               
     .          IPARI(1,N),X         ,V        , 
     .          A         ,VR        ,AR        ,MS        ,IN       , 
     .          WEIGHT    ,INTBUF_TAB(N) )
          ELSE
        !       WARNING : if NSN==0 and ILEV==2 or 4, ones needs to scale the mass 
                ILEV = IPARI(20,N)
                IF(ILEV==2.OR.ILEV==4) THEN
                        DO II=1,NMN
                                J=INTBUF_TAB(N)%MSR(II)
                                MS(J)=INTBUF_TAB(N)%NMAS(II)
                        ENDDO
                ENDIF
          ENDIF
        ELSEIF (NTY == 12) THEN                                       
          IF(IMACH/=3 .OR. ISPMD==0)THEN                            
            CALL INTTI12V(                                            
     .           IPARI(1,N),INTBUF_TAB(N)      ,X         ,V        , 
     .            A        ,MS       ,WEIGHT   ,WA        ,SKEW     ) 
          ENDIF                                                       
        ENDIF                                                         
      ENDDO
C-----------
      RETURN
      END
