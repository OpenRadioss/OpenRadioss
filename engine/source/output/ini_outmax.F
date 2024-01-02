Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  INI_TMAX                      source/output/ini_outmax.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        INI_TMNORM2                   source/output/outmaxsubr.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SIG2D2PIN                     source/output/outmaxsubr.F    
Chd|        SIG3D2PIN                     source/output/outmaxsubr.F    
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE INI_TMAX(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .           IXS  ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .           IXC  ,IXTG   ,IXT    ,IXP     ,IXR     ,
     .           X  ,D       ,V       ,IAD_ELEM,FR_ELEM ,
     .             WEIGHT ,IPM   ,IGEO    ,STACK   ,ITASK   )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD            
      USE OUTMAX_MOD
      USE STACK_MOD
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN):: ITASK
      INTEGER, DIMENSION(NPARG,NGROUP) ,INTENT(IN):: IPARG
      INTEGER, DIMENSION(NIXC,NUMELC) ,INTENT(IN):: IXC
      INTEGER, DIMENSION(NIXTG,NUMELTG) ,INTENT(IN):: IXTG
      INTEGER, DIMENSION(NIXS,NUMELS) ,INTENT(IN):: IXS
      INTEGER, DIMENSION(NIXQ,NUMELQ) ,INTENT(IN):: IXQ
      INTEGER, DIMENSION(NIXT,NUMELT) ,INTENT(IN):: IXT
      INTEGER, DIMENSION(NIXP,NUMELP) ,INTENT(IN):: IXP
      INTEGER, DIMENSION(NIXR,NUMELR) ,INTENT(IN):: IXR
      INTEGER, DIMENSION(6,NUMELS10) ,INTENT(IN):: IXS10
      INTEGER, DIMENSION(8,NUMELS16) ,INTENT(IN):: IXS16
      INTEGER, DIMENSION(12,NUMELS20) ,INTENT(IN):: IXS20
      INTEGER, DIMENSION(2,NSPMD+1)   ,INTENT(IN):: IAD_ELEM
      INTEGER, DIMENSION(SFR_ELEM)     ,INTENT(IN):: FR_ELEM
      INTEGER, DIMENSION(NUMNOD)     ,INTENT(IN):: WEIGHT
      INTEGER, DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN):: IPM
      INTEGER, DIMENSION(NPROPGI,NUMGEO) ,INTENT(IN):: IGEO
      my_real, DIMENSION(NPROPG,NUMGEO) ,INTENT(IN):: GEO
      my_real, DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real, DIMENSION(3,NUMNOD) ,INTENT(IN):: X,D,V
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N, I, J,NG,ITY,NEL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
c-----doing TT=0 initialization instead of Starter; elementary first      
       IF(ITASK == 0) NMAX_E = 0
       IF (TT==ZERO) THEN
         IF (ITASK==0) THEN
           DO NG=1,NGROUP
             ITY=IPARG(5,NG)
             NEL=IPARG(2,NG)
             IF (ITY /= 1 .AND. ITY /= 3 .AND. ITY /= 7) CYCLE
             GBUF => ELBUF_TAB(NG)%GBUF
             NMAX_E = NMAX_E + GBUF%G_TM_YIELD
             NMAX_E = NMAX_E + GBUF%G_TM_SEQ  
             NMAX_E = NMAX_E + GBUF%G_TM_EINT 
             NMAX_E = NMAX_E + GBUF%G_TM_DMG  
             NMAX_E = NMAX_E + GBUF%G_TM_SIG  
             NMAX_E = NMAX_E + GBUF%G_TM_STRA 
           END DO
           NMAX_N = LMAX_DIS+LMAX_VEL+LMAX_NSIG+LMAX_NSTRA
           IF (NMAX_N>0) THEN
             IF (LMAX_DIS>0) TM_PDIS = ZERO
             IF (LMAX_VEL>0) TM_PVEL = ZERO
             IF (LMAX_NSIG>0) THEN
               TM_PNSIG(1:NUMNOD) = ZERO
               TM_PNSIG(NUMNOD+1:2*NUMNOD) = EP30
             END IF
             IF (LMAX_NSTRA>0) THEN
               TM_PNSTRA(1:NUMNOD) = ZERO
               TM_PNSTRA(NUMNOD+1:2*NUMNOD) = EP30
             END IF
           END IF
         END IF !(ITASK==0) THEN
C-----------------         
         CALL MY_BARRIER()
C-----------------         
         CALL UPD_TMAX(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .           IXS  ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .           IXC  ,IXTG   ,IXT    ,IXP     ,IXR     ,
     .           X  ,D       ,V       ,IAD_ELEM,FR_ELEM ,
     .             WEIGHT ,IPM   ,IGEO    ,STACK   ,ITASK   )
C
       ELSEIF (ITASK==0) THEN
         DO NG=1,NGROUP
           ITY=IPARG(5,NG)
           NEL=IPARG(2,NG)
           IF (ITY /= 1 .AND. ITY /= 3 .AND. ITY /= 7) CYCLE
           GBUF => ELBUF_TAB(NG)%GBUF
           NMAX_E = NMAX_E + GBUF%G_TM_YIELD
           NMAX_E = NMAX_E + GBUF%G_TM_SEQ  
           NMAX_E = NMAX_E + GBUF%G_TM_EINT 
           NMAX_E = NMAX_E + GBUF%G_TM_DMG  
           NMAX_E = NMAX_E + GBUF%G_TM_SIG  
           NMAX_E = NMAX_E + GBUF%G_TM_STRA 
          SELECT CASE (ITY)
            CASE(1)
               IF (GBUF%G_TM_SIG>0) THEN
                 CALL SIG3D2PIN(GBUF%TM_SIG1,GBUF%TM_SIG3,GBUF%TM_PSIG,NEL)
               END IF
               IF (GBUF%G_TM_STRA>0) THEN
                 CALL SIG3D2PIN(GBUF%TM_STRA1,GBUF%TM_STRA3,GBUF%TM_PSTRA,NEL)
               END IF
            CASE(3,7)
              IF (GBUF%G_TM_SIG>0) THEN
                CALL SIG2D2PIN(GBUF%TM_SIG1,GBUF%TM_SIG3,GBUF%TM_PSIG,NEL)
              END IF
              IF (GBUF%G_TM_STRA>0) THEN
                CALL SIG2D2PIN(GBUF%TM_STRA1,GBUF%TM_STRA3,GBUF%TM_PSTRA,NEL)
              END IF
          END SELECT
         END DO
         NMAX_N = LMAX_DIS+LMAX_VEL+LMAX_NSIG+LMAX_NSTRA
         IF (NMAX_N>0) THEN
           IF (LMAX_DIS>0) CALL INI_TMNORM2(TM_DIS,TM_PDIS,NUMNOD)
           IF (LMAX_VEL>0) CALL INI_TMNORM2(TM_VEL,TM_PVEL,NUMNOD)
           IF (LMAX_NSIG>0)CALL SIG3D2PIN(TM_NSIG1,TM_NSIG3,TM_PNSIG,NUMNOD)
           IF (LMAX_NSTRA>0)CALL SIG3D2PIN(TM_NSTRA1,TM_NSTRA3,TM_PNSTRA,NUMNOD)
         END IF
       END IF !(TT==ZERO) THEN

      RETURN
      END
