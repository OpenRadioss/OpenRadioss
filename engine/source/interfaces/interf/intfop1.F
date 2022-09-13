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
Chd|  INTFOP1                       source/interfaces/interf/intfop1.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I14CMP                        source/interfaces/int14/i14cmp.F
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|        INTVO2                        source/interfaces/inter2d/intvo2.F
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTFOP1(
     1       IPARI ,X      ,A    ,
     2       ICODT ,FSAV    ,WA      ,V      ,MS   ,
     3       DT2T  ,NELTST  ,ITYPTST ,ITAB   ,STIFN,
     4       NPC   ,TF      ,FSKYI   ,ISKY   ,VR   ,
     5       FCONT ,IN      ,IGRSURF ,BUFSF  ,FNCONT,
     6       FTCONT,ICONTACT,RCONTACT,NUM_IMP,NS_IMP,
     8       NE_IMP ,NT_IMP  ,SENSOR  ,INTBUF_TAB,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr04_c.inc"
#include      "scr06_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST 
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),NT_IMP
      INTEGER IPARI(NPARI,*), ICODT(*),ITAB(*), 
     .        NPC(*), ISKY(*), 
     .        ICONTACT(*)
C     REAL
      my_real DT2T, 
     .   X(*), A(*), FSAV(NTHVKI,*) , WA(*), V(*),MS(*),
     .   STIFN(*), TF(*),FSKYI(LSKY,NFSKYI),VR(3,*),FCONT(3,*),IN(*),
     .   BUFSF(*), FNCONT(3,*),FTCONT(3,*),RCONTACT(*),SENSOR(NSENSR,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, IAD,IDUM, ISENS
      my_real TS
C-----------------------------------------------
#include "sync_dir.inc"
C-----------------------------------------------
      IF (IMPL_S==1) IAD = 1
C
      DO 200 N=1,NINTER
       NTY   =IPARI(7,N)
       IF(NTY==14.OR.NTY==15) I7KGLO = 1
       IF(ISPMD/=0) GOTO 200
       IF(NTY==3.OR.NTY==4.OR.NTY==5
     .    .OR.NTY==14.OR.NTY==15)THEN
#include "lockon.inc"
        IF(IPARI(16,N)==NCYCLE)THEN
#include "lockoff.inc"
        ELSE
         IPARI(16,N) = NCYCLE
#include "lockoff.inc"
C 
         ISENS = 0
         IF(NTY == 5 ) ISENS = IPARI(64,N) ! INTERFACE SENSOR NUMBER

         IF(ISENS/=0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
            TS = SENSOR(2,ISENS)
         ELSE
            TS = TT
         ENDIF
C
         IF(N2D==0)THEN
          IF (NTY==14) THEN
           CALL I14CMP(IPARI(1,N),INTBUF_TAB(N),IGRSURF ,BUFSF ,
     2              A ,X ,V ,WA ,FSAV(1,N) ,
     3              IN ,STIFN ,FCONT ,MS ,FSKYI , 
     4              ISKY ,NPC ,TF ,H3D_DATA)
          ELSEIF (NTY==15) THEN
           CALL I15CMP(IPARI(1,N),INTBUF_TAB(N),
     2            IGRSURF,BUFSF,A, X, V,
     3            WA, FSAV(1,N),IN, STIFN, FCONT,
     4            MS, FSKYI,ISKY, NPC, TF,
     5            DT2T, NELTST,ITYPTST, ITAB,H3D_DATA)
          ELSE
           IF(IMPL_S/=1)THEN
              IF(TT>=TS) THEN
                CALL INTVO3(
     1        IPARI(1,N) ,X,A    ,
     2        ICODT      ,FSAV(1,N)    ,WA           ,V ,MS       ,
     3        FSKYI      ,ISKY         ,FCONT   ,FNCONT ,FTCONT   ,
     4        ICONTACT   ,RCONTACT     ,IDUM    ,IDUM   ,IDUM     ,
     5          STIFN      ,ITAB,INTBUF_TAB(N),H3D_DATA)
             ENDIF
           ELSE
             NUM_IMP(N) =0
              IF(TT>=TS) THEN
                CALL INTVO3(
     1          IPARI(1,N) ,X ,A        ,
     2          ICODT      ,FSAV(1,N)    ,WA           ,V ,MS       ,
     3          FSKYI      ,ISKY         ,FCONT   ,FNCONT ,FTCONT   ,
     4          ICONTACT   ,RCONTACT,NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD),
     5          STIFN      ,ITAB,INTBUF_TAB(N),H3D_DATA)
              ENDIF
             IAD=IAD+NUM_IMP(N)
           END IF
          END IF
         ELSE
          IF(TT>=TS) THEN
            CALL INTVO2(
     1        IPARI(1,N) ,INTBUF_TAB(N),X   ,A ,
     2        ICODT      ,FSAV(1,N)    ,FSKYI        ,ISKY   ,
     3        FCONT      ,FNCONT       , FTCONT     ,H3D_DATA )
          ENDIF
         ENDIF
        ENDIF
       ELSEIF(NTY==6)THEN
#include "lockon.inc"
         IF(IPARI(16,N)==NCYCLE)THEN
#include "lockoff.inc"
         ELSE
           IPARI(16,N) = NCYCLE
#include "lockoff.inc"
           CALL I6MAIN(
     1        IPARI(1,N),X ,A        ,
     2        ICODT     ,NPC          ,TF           ,V ,FSAV(1,N),
     3        DT2T      ,NELTST       ,ITYPTST      ,FSKYI ,ISKY ,
     4        FCONT     ,MS           ,INTBUF_TAB(N),H3D_DATA )
         ENDIF
       ELSEIF(NTY==7.OR.NTY==22)THEN
       ELSEIF(NTY==10)THEN
       ELSEIF(NTY==11)THEN
       ENDIF
  200 CONTINUE
C
#include "lockon.inc"
      
      IF (IMPL_S==1) NT_IMP=IAD-1
#include "lockoff.inc"

C      
      RETURN
      END
C
