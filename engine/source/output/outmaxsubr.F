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
Chd|  GPS_SOLID                     source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        SPMD_EXCH_V                   source/mpi/generic/spmd_exch_v.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE GPS_SOLID(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .  	       IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,IAD_ELEM,FR_ELEM ,WEIGHT  ,SIG_N   ,
     .             ITAGPS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
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
      INTEGER, DIMENSION(SFR_ELEM)    ,INTENT(IN):: FR_ELEM
      INTEGER, DIMENSION(NUMNOD)      ,INTENT(IN):: WEIGHT
      INTEGER, DIMENSION(NUMNOD)     ,INTENT(INOUT):: ITAGPS
      my_real, DIMENSION(NPROPG,NUMGEO) ,INTENT(IN):: GEO
      my_real, DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real, DIMENSION(3,NUMNOD) ,INTENT(IN):: X
      my_real, DIMENSION(NUMNOD,6) ,INTENT(OUT):: SIG_N
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)  :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,LENR,NSIZ
      my_real
     .  FAC
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE, TARGET :: VCOM
      my_real
     .       , DIMENSION(:,:), POINTER :: AFLU, VFLU
C-----------------------------------------------
C
      NSIZ = 7
      ALLOCATE(VCOM(NSIZ,NUMNOD))
      AFLU=>VCOM(1:3,1:NUMNOD)
      VFLU=>VCOM(4:6,1:NUMNOD)
      VCOM = ZERO

       SIG_N(1:NUMNOD,1:6) = ZERO
       ITAGPS(1:NUMNOD) = 0
         CALL TENSGPS3(ELBUF_TAB ,VFLU	  ,AFLU    ,IPARG   ,GEO     ,
     .  	       IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,ITAGPS  ,PM	  )
         IF(NSPMD > 1)THEN
           VCOM(7,1:NUMNOD) =ITAGPS(1:NUMNOD) 
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_V(VCOM,NSIZ,IAD_ELEM,FR_ELEM,LENR)
           ITAGPS(1:NUMNOD)=NINT(VCOM(7,1:NUMNOD)) 
         ENDIF
         DO N=1,NUMNOD
           IF (ITAGPS(N) == 0) CYCLE
           FAC = ONE/ITAGPS(N)
           SIG_N(N,1:3)=FAC*VFLU(1:3,N)
           SIG_N(N,4:6)=FAC*AFLU(1:3,N)
         ENDDO

      DEALLOCATE(VCOM)

      RETURN
      END
Chd|====================================================================
Chd|  GPSTRA_SOLID                  source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        SPMD_EXCH_N                   source/mpi/generic/spmd_exch_n.F
Chd|        SPMD_EXCH_NODAREAI            source/mpi/anim/spmd_exch_nodareai.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE GPSTRA_SOLID(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .  	       IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,IAD_ELEM,FR_ELEM ,WEIGHT  ,STR_N   ,
     .             ITAGPS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
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
      INTEGER, DIMENSION(SFR_ELEM)    ,INTENT(IN):: FR_ELEM
      INTEGER, DIMENSION(NUMNOD)      ,INTENT(IN):: WEIGHT
      INTEGER, DIMENSION(NUMNOD)     ,INTENT(INOUT):: ITAGPS
      my_real, DIMENSION(NPROPG,NUMGEO) ,INTENT(IN):: GEO
      my_real, DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real, DIMENSION(3,NUMNOD) ,INTENT(IN):: X
      my_real, DIMENSION(NUMNOD,6) ,INTENT(OUT):: STR_N
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,LENR
      my_real
     .  FAC
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: AFLU, VFLU
C-----------------------------------------------
C
      ALLOCATE(AFLU(3,NUMNOD))
      ALLOCATE(VFLU(3,NUMNOD))


       STR_N(1:NUMNOD,1:6) = ZERO
         DO N=1,NUMNOD
           ITAGPS(N) = 0
           VFLU(1:3,N) = ZERO
           AFLU(1:3,N) = ZERO
         ENDDO
         CALL TENSGPSTRAIN(ELBUF_TAB,VFLU	 ,AFLU    ,IPARG   ,GEO     ,
     .  	       IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,ITAGPS  ,PM	  )
         IF(NSPMD > 1)THEN
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
           CALL SPMD_EXCH_N(VFLU,IAD_ELEM,FR_ELEM,LENR)
           CALL SPMD_EXCH_N(AFLU,IAD_ELEM,FR_ELEM,LENR)
         ENDIF
         DO N=1,NUMNOD
           IF (ITAGPS(N) == 0) CYCLE
           FAC = ONE/ITAGPS(N)
           STR_N(N,1:3)=FAC*VFLU(1:3,N)
           STR_N(N,4:6)=FAC*AFLU(1:3,N)
         ENDDO

      DEALLOCATE(AFLU)
      DEALLOCATE(VFLU)

      RETURN
      END
Chd|====================================================================
Chd|  SIG2DPIN2                     source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        SIG2D2PIN                     source/output/outmaxsubr.F    
Chd|        TM_SIG_SHELL                  source/output/outmaxsubr.F    
Chd|        TM_STRA_SHELL                 source/output/outmaxsubr.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIG2DPIN2(SIG,PIN,NEL)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NEL
      my_real, DIMENSION(NEL,3) ,INTENT(IN):: SIG
      my_real, DIMENSION(NEL,2) ,INTENT(OUT):: PIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S1,T1,DET,R
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=1,NEL 
        T1 = SIG(I,1)+SIG(I,2)
        DET = SIG(I,1)*SIG(I,2)-SIG(I,3)*SIG(I,3)
        R = SQRT(T1*T1-FOUR*DET)
        PIN(I,1) = HALF*(T1+R)
        PIN(I,2) = HALF*(T1-R)
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  SIG3DPIN2                     source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        SIG3D2PIN                     source/output/outmaxsubr.F    
Chd|        TM_SIG_SOLID                  source/output/outmaxsubr.F    
Chd|        TM_STRA_SOLID                 source/output/outmaxsubr.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIG3DPIN2(SIG,PIN,NEL)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NEL
      my_real, DIMENSION(NEL,6) ,INTENT(IN) :: SIG
      my_real, DIMENSION(NEL,2) ,INTENT(OUT):: PIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S1,T1,T2,T3,S(6),P1,DETB,DETS,R,ALPHA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=1,NEL 
        S1 = SIG(I,4)*SIG(I,4)+SIG(I,5)*SIG(I,5)+SIG(I,6)*SIG(I,6)
        IF (S1 > EM10) THEN
          T1 = THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          S(1:3) = SIG(I,1:3) - T1
          S(4:6) = SIG(I,4:6) 
          T2 = S(1)*S(1)+S(2)*S(2)+S(3)*S(3) + TWO*S1
          P1 = SQRT(T2/SIX)
          DETS = S(1)*(S(2)*S(3)-S(6)*S(6))-S(4)*(S(4)*S(3)-S(5)*S(6))
     .          +S(5)*(S(4)*S(6)-S(2)*S(5))           
          DETB = DETS/P1
          R = HALF*DETB
          IF (R <= -ONE) THEN
            ALPHA = THIRD*PI 
          ELSEIF (R >= ONE) THEN
            ALPHA = ZERO
          ELSE
            ALPHA = THIRD*ACOS(R)
          END IF
          PIN(I,1) =  T1 + TWO * P1* COS(ALPHA)
          PIN(I,2) =  T1 + TWO * P1* COS(ALPHA+2*PI/3)
        ELSE
          PIN(I,1) = MAX(SIG(I,1),SIG(I,2),SIG(I,3))
          PIN(I,2) = MIN(SIG(I,1),SIG(I,2),SIG(I,3))
        END IF
      END DO
C      
      RETURN
      END
Chd|====================================================================
Chd|  SIG3D2PIN                     source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        INI_TMAX                      source/output/ini_outmax.F    
Chd|-- calls ---------------
Chd|        SIG3DPIN2                     source/output/outmaxsubr.F    
Chd|====================================================================
      SUBROUTINE SIG3D2PIN(SIG1,SIG3,PIN,NEL)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NEL
      my_real, DIMENSION(NEL,6) ,INTENT(IN):: SIG1,SIG3
      my_real, DIMENSION(NEL,2) ,INTENT(OUT):: PIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   P2(NEL,2)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      CALL SIG3DPIN2(SIG1,P2,NEL)
      CALL SIG3DPIN2(SIG3,PIN,NEL)
      PIN(1:NEL,1) = P2(1:NEL,1)
C      
      RETURN
      END
Chd|====================================================================
Chd|  SIG2D2PIN                     source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        INI_TMAX                      source/output/ini_outmax.F    
Chd|-- calls ---------------
Chd|        SIG2DPIN2                     source/output/outmaxsubr.F    
Chd|====================================================================
      SUBROUTINE SIG2D2PIN(SIG1,SIG3,PIN,NEL)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NEL
      my_real, DIMENSION(NEL,3) ,INTENT(IN):: SIG1,SIG3
      my_real, DIMENSION(NEL,2) ,INTENT(OUT):: PIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   P2(NEL,2)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      CALL SIG2DPIN2(SIG1,P2,NEL)
      CALL SIG2DPIN2(SIG3,PIN,NEL)
      PIN(1:NEL,1) = P2(1:NEL,1)
C      
      RETURN
      END
Chd|====================================================================
Chd|  INI_TMNORM2                   source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        INI_TMAX                      source/output/ini_outmax.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INI_TMNORM2(TM_D,TM_NORM2,NNOD)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NNOD
      my_real, DIMENSION(3,NNOD) ,INTENT(IN):: TM_D
      my_real, DIMENSION(NNOD) ,INTENT(OUT):: TM_NORM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S(3)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=1,NNOD 
        S(1:3) = TM_D(1:3,I)
        TM_NORM2(I) = S(1)*S(1)+S(2)*S(2)+S(3)*S(3) 
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_VONM_SOLID                 source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        TM_SEQ_SOLID                  source/output/outmaxsubr.F    
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TM_VONM_SOLID(IVISC,SIG,VISC,VALUE,NEL)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IVISC,NEL
      my_real, DIMENSION(NEL,6) ,INTENT(IN):: SIG,VISC
      my_real, DIMENSION(NEL) ,INTENT(INOUT):: VALUE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S(6),S1,S2,S3,VONM,P,VONM2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
        DO I=1,NEL
          S(1:6) = SIG(I,1:6)				  
          IF(IVISC > 0 ) S(1:6) = S(1:6) + VISC(I,1:6)				  
          P = - (S(1)  + S(2) + S(3) ) * THIRD
          S1= S(1) + P
          S2= S(2) + P
          S3= S(3) + P
          VONM2= THREE*(S(4)*S(4)  +  S(5)*S(5) + S(6)*S(6) +
     .    	   HALF*(S1*S1 + S2*S2  + S3*S3) )
          VONM= SQRT(VONM2) ! can be optimized
          VALUE(I) = MAX(VALUE(I),VONM)
        ENDDO 
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_SEQ_SOLID                  source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        TM_VONM_SOLID                 source/output/outmaxsubr.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_SEQ_SOLID(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,IVISC,VALUE,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,NPTT,NEL,IVISC
      my_real, DIMENSION(NEL) ,INTENT(INOUT):: VALUE
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT
      my_real
     .   S(6),S1,S2,S3,VONM
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
        GBUF => ELBUF_TAB%GBUF
        DO IL=1,NLAY
          DO IT=1,NPTT
            DO IR=1,NPTR
              DO IS=1,NPTS
                LBUF =>  ELBUF_TAB%BUFLY(IL)%LBUF(IR,IS,IT)        
                IF (ELBUF_TAB%BUFLY(IL)%L_SEQ > 0) THEN
                  DO I=1,NEL
                    VALUE(I) = MAX(VALUE(I),LBUF%SEQ(I))
                  ENDDO
                ELSE
                  CALL TM_VONM_SOLID(IVISC,LBUF%SIG,LBUF%VISC,VALUE,NEL)
                ENDIF ! IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0)
              ENDDO ! DO IS=1,NPTS
            ENDDO ! DO IR=1,NPTR
          ENDDO ! DO IT=1,NPTT
        ENDDO ! DO IL=1,NLAY
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_DMG_SOLID                  source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_DMG_SOLID(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,VALUE,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,NPTT,NEL
      my_real, DIMENSION(NEL) ,INTENT(INOUT):: VALUE
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT,NFAIL,IIR
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      my_real,
     .  DIMENSION(:), POINTER  :: DFMAX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
        DO IL=1,NLAY 
         NFAIL = ELBUF_TAB%BUFLY(IL)%NFAIL								    
         DO IS=1,NPTS					
           DO IT=1,NPTT					
             DO IIR=1,NPTR	
               DO IR=1,NFAIL 											   
                 DFMAX=>ELBUF_TAB%BUFLY(IL)%FAIL(IIR,IS,IT)%FLOC(IR)%DAMMX
                 DO I=1,NEL  			       
                   VALUE(I) = MAX(VALUE(I),DFMAX(I))
                 ENDDO
               ENDDO 
             ENDDO
           ENDDO
          ENDDO
        ENDDO 
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_SIG_SOLID                  source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        SIG3DPIN2                     source/output/outmaxsubr.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_SIG_SOLID(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,IVISC,
     .                        TEN1,TEN3,P2,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,NPTT,NEL,IVISC
      my_real, DIMENSION(NEL,6) ,INTENT(INOUT):: TEN1,TEN3
      my_real, DIMENSION(NEL,2) ,INTENT(INOUT):: P2
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT,JJ(6)
      my_real
     .   S(NEL,6),S1,S2,S3,SP2(NEL,2)
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       DO I=1,6
         JJ(I) = NEL*(I-1)
       ENDDO
       DO IL=1,NLAY
         DO IT=1,NPTT
           DO IR=1,NPTR
             DO IS=1,NPTS
               LBUF =>  ELBUF_TAB%BUFLY(IL)%LBUF(IR,IS,IT)        
               DO I = 1,NEL
                 S(I,1:6) = LBUF%SIG(JJ(1:6)+I)				  
               END DO 
               IF(IVISC > 0 ) THEN
                 DO I = 1,NEL
                   S(I,1:6) = S(I,1:6) + LBUF%VISC(JJ(1:6)+I)				  
                 END DO 
               END IF
               CALL SIG3DPIN2(S,SP2,NEL)
               DO I = 1,NEL
                 IF (SP2(I,1) > P2(I,1)) THEN
                    TEN1(I,1:6) = S(I,1:6)
                    P2(I,1) = SP2(I,1)                     
                 END IF
                 IF (SP2(I,2) < P2(I,2)) THEN
                    TEN3(I,1:6) = S(I,1:6)
                    P2(I,2) = SP2(I,2)                     
                 END IF
               END DO                
             ENDDO ! DO IS=1,NPTS
           ENDDO ! DO IR=1,NPTR
         ENDDO ! DO IT=1,NPTT
       ENDDO ! DO IL=1,NLAY
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_STRA_SOLID                 source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        SIG3DPIN2                     source/output/outmaxsubr.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_STRA_SOLID(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,
     .                         TEN1,TEN3,P2,NEL) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,NPTT,NEL
      my_real, DIMENSION(NEL,6) ,INTENT(INOUT):: TEN1,TEN3
      my_real, DIMENSION(NEL,2) ,INTENT(INOUT):: P2
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT,JJ(6)
      my_real
     .   S(NEL,6),S1,S2,S3,SP2(NEL,2)
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       DO I=1,6
         JJ(I) = NEL*(I-1)
       ENDDO
       DO IL=1,NLAY
         DO IT=1,NPTT
           DO IR=1,NPTR
             DO IS=1,NPTS
               LBUF =>  ELBUF_TAB%BUFLY(IL)%LBUF(IR,IS,IT)  
C !!!!! law 12,14                
               IF (ELBUF_TAB%BUFLY(IL)%L_STRA > 0) THEN
                 DO I = 1,NEL
                   S(I,1:6) = LBUF%STRA(JJ(1:6)+I)				  
                 END DO 
                 CALL SIG3DPIN2(S,SP2,NEL)
                 DO I = 1,NEL
                   IF (SP2(I,1) > P2(I,1)) THEN
                      TEN1(I,1:6) = S(I,1:6)
                      P2(I,1) = SP2(I,1)                     
                   END IF
                   IF (SP2(I,2) < P2(I,2)) THEN
                      TEN3(I,1:6) = S(I,1:6)
                      P2(I,2) = SP2(I,2)                     
                   END IF
                 END DO                
               END IF !(ELBUF_TAB%BUFLY(IL)%L_STRA > 0) THEN
             ENDDO ! DO IS=1,NPTS
           ENDDO ! DO IR=1,NPTR
         ENDDO ! DO IT=1,NPTT
       ENDDO ! DO IL=1,NLAY
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_VONM_SHELL                 source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        TM_SEQ_SHELL                  source/output/outmaxsubr.F    
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TM_VONM_SHELL(FOR,VALUE,NEL)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NEL
      my_real, DIMENSION(NEL,5) ,INTENT(IN):: FOR
      my_real, DIMENSION(NEL) ,INTENT(INOUT):: VALUE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S(3),S1,S2,S12,VONM2,VONM
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
        DO I=1,NEL
          S1 = FOR(I,1)				  
          S2 = FOR(I,2)				  
          S12 = FOR(I,3)				  
          VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12 
          VONM= SQRT(VONM2)
          VALUE(I) = MAX(VALUE(I),VONM)
        ENDDO 
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_SEQ_SHELL                  source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        TM_VONM_SHELL                 source/output/outmaxsubr.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_SEQ_SHELL(ELBUF_TAB,NLAY,NPTR,NPTS,VALUE,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,NEL
      my_real, DIMENSION(NEL) ,INTENT(INOUT):: VALUE
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT
      my_real
     .   S(3),S1,S2,S3,VONM
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO IS = 1,NPTS
        DO IR = 1,NPTR
          DO IL =1,NLAY
            BUFLY => ELBUF_TAB%BUFLY(IL)
            DO IT=1,BUFLY%NPTT
              LBUF  => BUFLY%LBUF(IR,IS,IT)
              IF (BUFLY%L_SEQ > 0) THEN
                DO I=1,NEL
                  VALUE(I) = MAX(VALUE(I),LBUF%SEQ(I))
                ENDDO
              ELSE
                CALL TM_VONM_SHELL(LBUF%SIG,VALUE,NEL)
              ENDIF ! IF (ELBUF_TAB(NG)%BUFLY(IL)%L_SEQ > 0)
            ENDDO ! IT=1,
          ENDDO ! ILAY =1
        ENDDO ! IR = 1
      ENDDO ! DO IS = 1
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_SIG_SHELL                  source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        SIG2DPIN2                     source/output/outmaxsubr.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_SIG_SHELL(ELBUF_TAB,NLAY,NPTR,NPTS,TEN1,TEN3,P2,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,NEL
      my_real, DIMENSION(NEL,3) ,INTENT(INOUT):: TEN1,TEN3
      my_real, DIMENSION(NEL,2) ,INTENT(INOUT):: P2
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT,JJ(3)
      my_real
     .   S(NEL,3),S1,S2,S3,SP2(NEL,2)
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       DO I=1,3
         JJ(I) = NEL*(I-1)
       ENDDO
      DO IS = 1,NPTS
        DO IR = 1,NPTR
          DO IL =1,NLAY
            BUFLY => ELBUF_TAB%BUFLY(IL)
            DO IT=1,BUFLY%NPTT
              LBUF  => BUFLY%LBUF(IR,IS,IT)
               DO I = 1,NEL
                 S(I,1:3) = LBUF%SIG(JJ(1:3)+I)				  
               END DO 
               CALL SIG2DPIN2(S,SP2,NEL)
               DO I = 1,NEL
                 IF (SP2(I,1) > P2(I,1)) THEN
                    TEN1(I,1:3) = S(I,1:3)
                    P2(I,1) = SP2(I,1)                     
                 END IF
                 IF (SP2(I,2) < P2(I,2)) THEN
                    TEN3(I,1:3) = S(I,1:3)
                    P2(I,2) = SP2(I,2)                     
                 END IF
               END DO                
            ENDDO ! IT=1,
          ENDDO ! ILAY =1
        ENDDO ! IR = 1
      ENDDO ! DO IS = 1
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_STRA_SHELL                 source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        SIG2DPIN2                     source/output/outmaxsubr.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_STRA_SHELL(ELBUF_TAB,NPTR,NPTS,TEN1,TEN3,P2,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NPTR,NPTS,NEL
      my_real, DIMENSION(NEL,3) ,INTENT(INOUT):: TEN1,TEN3
      my_real, DIMENSION(NEL,2) ,INTENT(INOUT):: P2
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT,JJ(3),NPG,LENS,NG,PTS
      my_real
     .   S(NEL,3),S1,S2,S3,SP2(NEL,2)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       DO I=1,3
         JJ(I) = NEL*(I-1)
       ENDDO
       GBUF => ELBUF_TAB%GBUF
       NPG = NPTS*NPTR
      IF (NPG == 1) THEN
        DO I = 1,NEL
          S(I,1:3) = GBUF%STRA(JJ(1:3)+I)				  
        END DO 
        CALL SIG2DPIN2(S,SP2,NEL)
        DO I = 1,NEL
          IF (SP2(I,1) > P2(I,1)) THEN
             TEN1(I,1:3) = S(I,1:3)
             P2(I,1) = SP2(I,1)                     
          END IF
          IF (SP2(I,2) < P2(I,2)) THEN
             TEN3(I,1:3) = S(I,1:3)
             P2(I,2) = SP2(I,2)                     
          END IF
        END DO                
      ELSE
        LENS = NEL*GBUF%G_STRPG/NPG
        DO IS = 1,NPTS
          DO IR = 1,NPTR
            NG = NPTR*(IS-1) + IR
            PTS = (NG-1)*LENS+1
               DO I = 1,NEL
                 S(I,1:3) = GBUF%STRPG(PTS+JJ(1:3)+I)				  
               END DO 
               CALL SIG2DPIN2(S,SP2,NEL)
               DO I = 1,NEL
                 IF (SP2(I,1) > P2(I,1)) THEN
                    TEN1(I,1:3) = S(I,1:3)
                    P2(I,1) = SP2(I,1)                     
                 END IF
                 IF (SP2(I,2) < P2(I,2)) THEN
                    TEN3(I,1:3) = S(I,1:3)
                    P2(I,2) = SP2(I,2)                     
                 END IF
               END DO                
          ENDDO ! IR = 1
        ENDDO ! DO IS = 1
      END IF !(NPG == 1) THEN
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_DMG_SHELLS                 source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_DMG_SHELLS(ELBUF_TAB,NLAY,NPTR,NPTS,VALUE,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,NEL
      my_real, DIMENSION(NEL) ,INTENT(INOUT):: VALUE
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT,NFAIL,IIR
      my_real, DIMENSION(:), POINTER  :: DFMAX
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
       DO IL=1,NLAY 
         BUFLY => ELBUF_TAB%BUFLY(IL)
         NFAIL = BUFLY%NFAIL								    
         DO IS=1,NPTS					
           DO IR=1,NPTR					
             DO IT=1,BUFLY%NPTT	
               DO IIR=1,NFAIL 											   
                 DFMAX=>BUFLY%FAIL(IR,IS,IT)%FLOC(IIR)%DAMMX
                 DO I=1,NEL  			       
                   VALUE(I) = MAX(VALUE(I),DFMAX(I))
                 ENDDO
               ENDDO 
             ENDDO
           ENDDO
          ENDDO
       ENDDO 
C      
      RETURN
      END
Chd|====================================================================
Chd|  TM_DMGL25_SHELL               source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TM_DMGL25_SHELL(ELBUF_TAB,NLAY,NPTR,NPTS,IGTYP,
     .                           PM,MATLY,VALUE,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN):: NLAY,NPTR,NPTS,IGTYP,NEL
      INTEGER,DIMENSION((MVSIZ*100)),INTENT(IN):: MATLY
      my_real, DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real, DIMENSION(NEL) ,INTENT(INOUT):: VALUE
      TYPE (ELBUF_STRUCT_),  TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IL,IR,IS,IT,NFAIL,IIR,IPT,IADR,JJ(3)
      my_real
     .   VG(5),VLY(5),VE(5)
      my_real,
     .  DIMENSION(MVSIZ) :: DMAX,WPMAX,EPST1,EPST2,EPSF1,EPSF2
      my_real,
     .  DIMENSION(:), POINTER  :: DFMAX
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
       DO IL=1,NLAY 
         BUFLY => ELBUF_TAB%BUFLY(IL)
         NFAIL = BUFLY%NFAIL								    
         DO IS=1,NPTS					
           DO IR=1,NPTR					
             DO IT=1,BUFLY%NPTT	
               DO IIR=1,NFAIL 											   
                 DFMAX=>BUFLY%FAIL(IR,IS,IT)%FLOC(IIR)%DAMMX
                 DO I=1,NEL  			       
                   VALUE(I) = MAX(VALUE(I),DFMAX(I))
                 END DO
               END DO
             END DO
           END DO
         END DO !DO IS=1
       END DO
C       
       IF(IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .    IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN   
         DO I=1,3
           JJ(I) = NEL*(I-1)
         ENDDO
         DO IL=1,NLAY 
           BUFLY => ELBUF_TAB%BUFLY(IL)
           DO IPT=1,BUFLY%NPTT	
             IADR = (IPT - 1)*NEL			      
             DO I=1,NEL  					  
               J = IADR + I 
               VLY(1:5) = ZERO
               VG(1:5) = ZERO
               DO IR=1,NPTR
                 DO IS=1,NPTS
                   LBUF => BUFLY%LBUF(IR,IS,IPT)        
                   DMAX(I) = ONE/PM(64,MATLY(J))				
                   WPMAX(I)= ONE/PM(41,MATLY(J))				
                   EPST1(I)= PM(60,MATLY(J))  			     
                   EPST2(I)= PM(61,MATLY(J))  			     
                   EPSF1(I)= ONE/PM(98,MATLY(J))				
                   EPSF2(I)= ONE/PM(99,MATLY(J))
C        
                   VG(1) = MAX(VG(1),LBUF%DAM(JJ(1)+I)*DMAX(I)) 
                   VG(2) = MAX(VG(2),LBUF%DAM(JJ(2)+I)*DMAX(I)) 
                   VG(3) = MAX(VG(3),ABS(LBUF%PLA(I))*WPMAX(I)) 
                   IF(LBUF%CRAK(JJ(1)+I) > ZERO) VG(4)=  MAX(VG(4),
     .         	     (LBUF%CRAK(JJ(1)+I)+EPST1(I))*EPSF1(I)) 
                   IF(LBUF%CRAK(JJ(2)+I) > ZERO )VG(5) = MAX(VG(5), 
     .         	     (LBUF%CRAK(JJ(2)+I)+EPST2(I))*EPSF2(I))
                 END DO
               END DO
               VLY(1) =VLY(1) + VG(1) 
               VLY(2) =VLY(2) + VG(2) 
               VLY(3) =VLY(3) + VG(3) 
               VLY(4) =VLY(4) + VG(4) 
               VLY(5) =VLY(5) + VG(5)
C                 	 
               VALUE(I) = MAX(VALUE(I),VLY(1),VLY(2),VLY(3),VLY(4),VLY(5))
             END DO  ! I=1,NEL
           END DO !IPT=1,BUFLY%NPTT	
         END DO !IL=1,NLAY 
       END IF !(IGTYP == 10 .OR. IGTYP == 11 .OR. 
C      
      RETURN
      END
Chd|====================================================================
Chd|  UPD_TMNORM2                   source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPD_TMNORM2(D,TM_D,TM_NORM2,NFT,NLT)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NFT,NLT
      my_real, DIMENSION(3,NUMNOD) ,INTENT(IN):: D
      my_real, DIMENSION(NUMNOD) ,INTENT(INOUT):: TM_NORM2
      my_real, DIMENSION(3,NUMNOD) ,INTENT(INOUT):: TM_D
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S(3),NORM2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=NFT,NLT 
        S(1:3) = D(1:3,I)
        NORM2 = S(1)*S(1)+S(2)*S(2)+S(3)*S(3)
        IF (NORM2 > TM_NORM2(I)) THEN
          TM_NORM2(I) = NORM2
          TM_D(1:3,I) = S(1:3)
        END IF        
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  UPD_TMTENS                    source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPD_TMTENS(NSIG,PNSIG,TM_NSIG1,TM_NSIG3,TM_PNSIG,NFT,NLT,IGPSTAG)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NFT,NLT
      INTEGER  ,DIMENSION(NUMNOD) ,INTENT(IN):: IGPSTAG
      my_real, DIMENSION(NUMNOD,6) ,INTENT(IN):: NSIG
      my_real, DIMENSION(NUMNOD,2) ,INTENT(IN):: PNSIG
      my_real, DIMENSION(NUMNOD,2) ,INTENT(INOUT):: TM_PNSIG
      my_real, DIMENSION(NUMNOD,6) ,INTENT(INOUT):: TM_NSIG1,TM_NSIG3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S(3),NORM2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=NFT,NLT 
        IF (IGPSTAG(I)==0) CYCLE      
        IF (PNSIG(I,1) > TM_PNSIG(I,1)) THEN
          TM_PNSIG(I,1) = PNSIG(I,1)
          TM_NSIG1(I,1:6) = NSIG(I,1:6)
        END IF        
        IF (PNSIG(I,2) < TM_PNSIG(I,2)) THEN
          TM_PNSIG(I,2) = PNSIG(I,2)
          TM_NSIG3(I,1:6) = NSIG(I,1:6)
        END IF        
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  SIG3DPIN2H                    source/output/outmaxsubr.F    
Chd|-- called by -----------
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIG3DPIN2H(SIG,PIN,NFT,NLT,IGPSTAG)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NFT,NLT
      INTEGER  ,DIMENSION(NUMNOD) ,INTENT(IN):: IGPSTAG
      my_real, DIMENSION(NUMNOD,6) ,INTENT(IN) :: SIG
      my_real, DIMENSION(NUMNOD,2) ,INTENT(INOUT):: PIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S1,T1,T2,T3,S(6),P1,DETB,DETS,R,ALPHA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=NFT,NLT
        IF (IGPSTAG(I)==0) CYCLE      
        S1 = SIG(I,4)*SIG(I,4)+SIG(I,5)*SIG(I,5)+SIG(I,6)*SIG(I,6)
        IF (S1 > EM10) THEN
          T1 = THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          S(1:3) = SIG(I,1:3) - T1
          S(4:6) = SIG(I,4:6) 
          T2 = S(1)*S(1)+S(2)*S(2)+S(3)*S(3) + TWO*S1
          P1 = SQRT(T2/SIX)
          DETS = S(1)*(S(2)*S(3)-S(6)*S(6))-S(4)*(S(4)*S(3)-S(5)*S(6))
     .          +S(5)*(S(4)*S(6)-S(2)*S(5))           
          DETB = DETS/P1
          R = HALF*DETB
          IF (R <= -ONE) THEN
            ALPHA = THIRD*PI 
          ELSEIF (R >= ONE) THEN
            ALPHA = ZERO
          ELSE
            ALPHA = THIRD*ACOS(R)
          END IF
          PIN(I,1) =  T1 + TWO * P1* COS(ALPHA)
          PIN(I,2) =  T1 + TWO * P1* COS(ALPHA+2*PI/3)
        ELSE
          PIN(I,1) = MAX(SIG(I,1),SIG(I,2),SIG(I,3))
          PIN(I,2) = MIN(SIG(I,1),SIG(I,2),SIG(I,3))
        END IF
      END DO
C      
      RETURN
      END
