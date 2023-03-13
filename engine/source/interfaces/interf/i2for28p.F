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
Chd|  I2FOR28P                      source/interfaces/interf/i2for28p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FOR28P_CIN                  source/interfaces/interf/i2for28p_cin.F
Chd|        I2FOR28P_PEN                  source/interfaces/interf/i2for28p_pen.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR28P(
     .           X           ,V           ,VR          ,A           ,AR          ,
     .           MS          ,IN          ,STIFN       ,STIFR       ,WEIGHT      ,
     .           IRECTM      ,NSV         ,IRTLM       ,CSTS        ,SKEW        ,
     .           DR          ,DL          ,FINI        ,FSAV        ,FNCONT      ,
     .           NSN         ,I0          ,I2SIZE      ,IADI2       ,FSKYI2      ,
     .           STFN_PEN    ,STFR_PEN    ,VISC        ,PENFLAG     ,IROT        ,
     .           NOINT       ,NODNX_SMS   ,DMINT2      ,DT2T        ,NELTST      ,
     .           ITYPTST     ,MMAS        ,SMAS        ,SINER       ,MINER       ,
     .           MSR         ,IPENA       ,ADI         ,IDEL2       ,NIR         ,
     .           NMN         ,DPARA       ,
     .           NMAS        ,DMAST       ,ADM         ,H3D_DATA    ,FNCONTP     ,
     .           FTCONTP     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSN,NMN,NOINT,NELTST,ITYPTST,IDEL2,I0,I2SIZE,PENFLAG,IROT,NIR
      INTEGER  IRECTM(4,*),NSV(*),MSR(*),IRTLM(*),WEIGHT(*),IADI2(4,*),
     .   NODNX_SMS(*),IPENA(*)
      my_real
     .   VISC,DT2T
      my_real
     .   X(3,*),V(3,*),A(3,*),VR(3,*),AR(3,*),MS(*),IN(*),FINI(6,4,*),
     .   DL(3,4,*),DR(3,4,*),STIFN(*),STIFR(*),STFN_PEN(*),STFR_PEN(*),
     .   FSAV(*),FNCONT(3,*),DMINT2(4,*),
     .   CSTS(2,*),SMAS(*),MMAS(*),SINER(*),MINER(*),ADI(*),SKEW(9,*),
     .   FSKYI2(I2SIZE,*),
     .   DPARA(7,*),DMAST,ADM(*),NMAS(*),FNCONTP(3,*)   ,FTCONTP(3,*) 
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISN,NI,I0BASE,NINDXC,NINDXP
      INTEGER IADX(NSN),INDXC(NSN),INDXP(NSN)
C===============================================
      INDXC(1:NSN) = 0      
      INDXP(1:NSN) = 0      
      IADX(1:NSN)  = 0      
      NINDXC = 0     
      NINDXP = 0     
      I0BASE = I0
      NI     = 0     
C----------------      
      DO I=1,NSN
        ISN = ABS(NSV(I))
        IF (WEIGHT(ISN) == 1) THEN
          NI = NI+1
          IADX(I) = NI
        ENDIF
c
        IF (IPENA(I) == 0) THEN     
          NINDXC = NINDXC + 1       
          INDXC(I) = I              
        ELSE                        
          NINDXP = NINDXP + 1
          INDXP(I) = I              
        ENDIF                       
      ENDDO
c-----------
      IF (NINDXC > 0) THEN
        I0 = I0BASE
        CALL I2FOR28P_CIN(
     1       NSN      ,NMN     ,A      ,IRECTM  ,DPARA   ,
     2       MSR      ,NSV     ,IRTLM  ,MS      ,WEIGHT  ,
     3       AR       ,IN      ,X      ,STIFN   ,STIFR   ,
     4       FSKYI2   ,IADI2   ,FSAV   ,DMAST   ,ADM     ,
     5       MMAS     ,I0      ,NIR    ,I2SIZE  ,IDEL2   ,
     6       SMAS     ,SINER   ,V      ,CSTS    ,FNCONT  ,
     7       INDXC    ,IADX    ,H3D_DATA,FNCONTP     ,FTCONTP)
c
      ENDIF
c
      IF (NINDXP > 0) THEN
        I0 = I0BASE
        CALL I2FOR28P_PEN(                                               
     .       X         ,V         ,VR        ,A         ,AR        ,
     .       MS        ,IN        ,STIFN     ,STIFR     ,WEIGHT    ,
     .       NSV       ,IRTLM     ,CSTS      ,SKEW      ,DL        ,
     .       DR        ,FINI      ,FSAV      ,FNCONT    ,NSN       ,
     .       I0        ,I2SIZE    ,IADI2     ,FSKYI2    ,STFN_PEN  ,
     .       STFR_PEN  ,VISC      ,PENFLAG   ,IROT      ,NOINT     ,
     .       NODNX_SMS ,DMINT2    ,DT2T      ,NELTST    ,ITYPTST   ,
     .       IRECTM    ,INDXP     ,IADX      ,
     .       H3D_DATA  ,FNCONTP     ,FTCONTP)
c
      ENDIF 
      I0 = I0BASE + NI                                                         
c-----------
      RETURN
      END SUBROUTINE I2FOR28P
