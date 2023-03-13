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
Chd|  UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- called by -----------
Chd|        INI_TMAX                      source/output/ini_outmax.F    
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GPSTRA_SOLID                  source/output/outmaxsubr.F    
Chd|        GPS_SOLID                     source/output/outmaxsubr.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SIG3DPIN2H                    source/output/outmaxsubr.F    
Chd|        TM_DMGL25_SHELL               source/output/outmaxsubr.F    
Chd|        TM_DMG_SHELLS                 source/output/outmaxsubr.F    
Chd|        TM_DMG_SOLID                  source/output/outmaxsubr.F    
Chd|        TM_SEQ_SHELL                  source/output/outmaxsubr.F    
Chd|        TM_SEQ_SOLID                  source/output/outmaxsubr.F    
Chd|        TM_SIG_SHELL                  source/output/outmaxsubr.F    
Chd|        TM_SIG_SOLID                  source/output/outmaxsubr.F    
Chd|        TM_STRA_SHELL                 source/output/outmaxsubr.F    
Chd|        TM_STRA_SOLID                 source/output/outmaxsubr.F    
Chd|        TM_VONM_SHELL                 source/output/outmaxsubr.F    
Chd|        TM_VONM_SOLID                 source/output/outmaxsubr.F    
Chd|        UPD_TMNORM2                   source/output/outmaxsubr.F    
Chd|        UPD_TMTENS                    source/output/outmaxsubr.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE UPD_TMAX(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .  	       IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,D       ,V       ,IAD_ELEM,FR_ELEM ,
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
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
      INTEGER  N, I, J,NG,ITY,NEL,IVISC,NFT,IFLU,MX,TSHELL,
     .         IGTYP,ISOLNOD,ISROT,NPTR,NPTS,NPTT,NLAY,NPT,
     .         JHBE,IMAT,IPID,MATLY(MVSIZ*100),IPMAT,IADR,
     .         ISUBSTACK,MTN
      INTEGER  NODFTSK,NODLTSK,NGFTSK,NGLTSK
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      my_real
     .  NORM,S(3),VALUE(MVSIZ),RHO,TEN1(MVSIZ,6),TEN3(MVSIZ,6),
     .  TP2(MVSIZ,2),T2D1(MVSIZ,3),T2D3(MVSIZ,3),MASS(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      IF (NMAX_E > 0 )THEN   
        NGFTSK   = 1+ITASK*NGROUP/ NTHREAD
        NGLTSK   = (ITASK+1)*NGROUP/NTHREAD
       DO NG=NGFTSK,NGLTSK
         IF (IPARG(8,NG)==1) CYCLE                                 
         ITY=IPARG(5,NG)
         IF (ITY /= 1 .AND. ITY /= 3 .AND. ITY /= 7) CYCLE
         MTN=IPARG(1,NG)
         NEL=IPARG(2,NG)
         NFT=IPARG(3,NG)
         IVISC = IPARG(61,NG)
         IFLU=IPARG(7,NG)+IPARG(11,NG)
         ISOLNOD = IPARG(28,NG)
         IGTYP  = IPARG(38,NG)
         ISROT  = IPARG(41,NG)
         JHBE   = IPARG(23,NG)
         GBUF => ELBUF_TAB(NG)%GBUF
        SELECT CASE (ITY)
          CASE(1)
            TSHELL = 0
            IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) TSHELL = 1
            NLAY = ELBUF_TAB(NG)%NLAY                
            NPTR = ELBUF_TAB(NG)%NPTR                 
            NPTS = ELBUF_TAB(NG)%NPTS                 
            NPTT = ELBUF_TAB(NG)%NPTT
            IF (TSHELL==1) THEN
              IF (JHBE==14 .OR. JHBE==15) NPTT = 1
              IF (JHBE==16) NPTS = 1
            ELSE
              NLAY = 1 
              IF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
                NPTS = 1
                NPTT = 1
              END IF 
            END IF
            NPT  = NPTT*NPTS*NPTR*NLAY
            IF (GBUF%G_TM_YIELD>0 ) THEN  !VONM main value
              VALUE(1:NEL) = ZERO            
              LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
              CALL TM_VONM_SOLID(IVISC,GBUF%SIG,LBUF%VISC,VALUE,NEL)
              GBUF%TM_YIELD(1:NEL)= MAX(GBUF%TM_YIELD(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_EINT>0 ) THEN  
               VALUE(1:NEL) = ZERO            
               LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
               IF (MTN == 151) CYCLE
               IF(IFLU == 0)THEN
                  MX=IXS(1,NFT+1)
                  RHO = PM(89,MX)
                  VALUE(1:NEL) = GBUF%EINT(1:NEL)/MAX(EM20,RHO)   !
               ELSE
                  VALUE(1:NEL) = GBUF%EINT(1:NEL)/MAX(EM20,GBUF%RHO(1:NEL))  !
               ENDIF
              GBUF%TM_EINT(1:NEL)= MAX(GBUF%TM_EINT(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_SEQ>0 ) THEN  
              VALUE(1:NEL) = ZERO            
              CALL TM_SEQ_SOLID(ELBUF_TAB(NG),NLAY,NPTR,NPTS,NPTT,IVISC,VALUE,NEL) 
              GBUF%TM_SEQ(1:NEL)= MAX(GBUF%TM_SEQ(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_DMG>0 ) THEN  
              VALUE(1:NEL) = ZERO            
              CALL TM_DMG_SOLID(ELBUF_TAB(NG),NLAY,NPTR,NPTS,NPTT,VALUE,NEL)
              GBUF%TM_DMG(1:NEL)= MAX(GBUF%TM_DMG(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_SIG>0) THEN
              CALL TM_SIG_SOLID(ELBUF_TAB(NG),NLAY,NPTR,NPTS,NPTT,IVISC,
     .                          GBUF%TM_SIG1,GBUF%TM_SIG3,GBUF%TM_PSIG,NEL)
            END IF
            IF (GBUF%G_TM_STRA>0) THEN
              CALL TM_STRA_SOLID(ELBUF_TAB(NG),NLAY,NPTR,NPTS,NPTT,
     .                          GBUF%TM_STRA1,GBUF%TM_STRA3,GBUF%TM_PSTRA,NEL)
            END IF
          CASE(3,7)
            NLAY = ELBUF_TAB(NG)%NLAY                
            NPTR = ELBUF_TAB(NG)%NPTR                 
            NPTS = ELBUF_TAB(NG)%NPTS 
            NPT =  ELBUF_TAB(NG)%BUFLY(1)%NPTT
            ISUBSTACK = IPARG(71,NG)
            IF(ITY == 3)THEN
              IMAT = IXC(1,NFT+1)				
              IPID = IXC(6,NFT+1)				
            ELSE
              IMAT = IXTG(1,NFT+1)
              IPID = IXTG(5,NFT+1)				
            ENDIF
            IF (GBUF%G_TM_YIELD>0 ) THEN  !VONM main value
              VALUE(1:NEL) = ZERO            
              CALL TM_VONM_SHELL(GBUF%FOR,VALUE,NEL)
              GBUF%TM_YIELD(1:NEL)= MAX(GBUF%TM_YIELD(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_EINT>0 ) THEN  ! /ENER
               VALUE(1:NEL) = ZERO            
               IF (MTN == 151) CYCLE
               RHO = PM(1,IMAT)
               DO I=1,NEL
             	  MASS(I) = RHO*GBUF%VOL(I)
                  VALUE(I) = (GBUF%EINT(I)+ GBUF%EINT(I+NEL))/MAX(EM20,MASS(I))
               ENDDO
              GBUF%TM_EINT(1:NEL)= MAX(GBUF%TM_EINT(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_SEQ>0 ) THEN  
              VALUE(1:NEL) = ZERO            
              CALL TM_SEQ_SHELL(ELBUF_TAB(NG),NLAY,NPTR,NPTS,VALUE,NEL)
              GBUF%TM_SEQ(1:NEL)= MAX(GBUF%TM_SEQ(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_DMG>0 ) THEN  
              VALUE(1:NEL) = ZERO            
              IF (MTN == 25) THEN
                SELECT CASE (IGTYP)
                  CASE(10)
                    DO N=1,NPT
                      IADR = (N-1)*NEL
                      DO I=1,NEL
                        MATLY(IADR+I)=IMAT
                      END DO
                    END DO 
                  CASE(11)
                    IPMAT = 100				    
                    DO N=1,NPT
                      IADR = (N-1)*NEL
                      DO I=1,NEL
                        MATLY(IADR+I)=IGEO(IPMAT+N,IPID)
                      END DO
                    END DO 
                  CASE(17,51,52)
                    IPMAT = 2 + NLAY			   
                    DO N=1,NLAY
                      IADR = (N-1)*NEL			   
                      DO I=1,NEL
                        MATLY(IADR+I) = STACK%IGEO(IPMAT+N,ISUBSTACK)
                      END DO				  
                    END DO	
                END SELECT
                CALL TM_DMGL25_SHELL(ELBUF_TAB(NG),NLAY,NPTR,NPTS,IGTYP,
     .                               PM,MATLY,VALUE,NEL)
              ELSE
                CALL TM_DMG_SHELLS(ELBUF_TAB(NG),NLAY,NPTR,NPTS,VALUE,NEL)
              END IF
              GBUF%TM_DMG(1:NEL)= MAX(GBUF%TM_DMG(1:NEL),VALUE(1:NEL))
            END IF
            IF (GBUF%G_TM_SIG>0) THEN
              CALL TM_SIG_SHELL(ELBUF_TAB(NG),NLAY,NPTR,NPTS,
     .                          GBUF%TM_SIG1,GBUF%TM_SIG3,GBUF%TM_PSIG,NEL)
            END IF
            IF (GBUF%G_TM_STRA>0) THEN
              CALL TM_STRA_SHELL(ELBUF_TAB(NG),NPTR,NPTS,
     .                       GBUF%TM_STRA1,GBUF%TM_STRA3,GBUF%TM_PSTRA,NEL)
            END IF
        END SELECT
       END DO
      END IF !(NMAX_E > 0 )THEN   
      IF (NMAX_N > 0 )THEN   
        NODFTSK   = 1+ITASK*NUMNOD/ NTHREAD
        NODLTSK   = (ITASK+1)*NUMNOD/NTHREAD
       IF (LMAX_DIS>0) CALL UPD_TMNORM2(D,TM_DIS,TM_PDIS,NODFTSK,NODLTSK)
       IF (LMAX_VEL>0) CALL UPD_TMNORM2(V,TM_VEL,TM_PVEL,NODFTSK,NODLTSK)
       IF (LMAX_NSIG>0 .AND.(NCYCLE <= 1 .OR. MOD(NCYCLE,NCY_GPS)==0)) THEN
!$OMP SINGLE
         CALL GPS_SOLID(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .          IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .          IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .          X	,IAD_ELEM,FR_ELEM ,WEIGHT  ,GPSTMP  ,IGPSTAG )
!$OMP END SINGLE
         CALL SIG3DPIN2H(GPSTMP,P2TMP,NODFTSK,NODLTSK,IGPSTAG)
         CALL UPD_TMTENS(GPSTMP,P2TMP,TM_NSIG1,TM_NSIG3,TM_PNSIG,NODFTSK,NODLTSK,IGPSTAG)
       END IF
C-----------------         
         CALL MY_BARRIER()
C-----------------         
       IF (LMAX_NSTRA>0 .AND.(NCYCLE <= 1 .OR. MOD(NCYCLE,NCY_GPSTR)==0)) THEN
!$OMP SINGLE
           CALL GPSTRA_SOLID(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .          IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .          IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .          X	,IAD_ELEM,FR_ELEM ,WEIGHT  ,GPSTMP  ,IGPSTRATAG)
!$OMP END SINGLE
         CALL SIG3DPIN2H(GPSTMP,P2TMP,NODFTSK,NODLTSK,IGPSTRATAG)
         CALL UPD_TMTENS(GPSTMP,P2TMP,TM_NSTRA1,TM_NSTRA3,TM_PNSTRA,NODFTSK,NODLTSK,IGPSTRATAG)
       END IF
      END IF !(NMAX_N > 0 )THEN   
C-----------------         
         CALL MY_BARRIER()
C-----------------         

      RETURN
      END
