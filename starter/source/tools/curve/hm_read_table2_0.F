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
Chd|  HM_READ_TABLE2_0              source/tools/curve/hm_read_table2_0.F
Chd|-- called by -----------
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INTERFACE_TABLE_MOD           share/modules1/table_mod.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_TABLE2_0(NTABLE0, TABLE ,L,NFUNCT,UNITAB, LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTABLE0,NFUNCT,L
C     REAL
      TYPE(TTABLE) TABLE(*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      my_real, INTENT(IN) :: UNITAB(LUNIT, NUNITS)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITYPE, IBID
      INTEGER I, J, K, II, N, ID, NDIM, NX(4), NY,
     .        JREC, NPTS, STAT, IDS, LL, LX(2)
      INTEGER,DIMENSION(:),ALLOCATABLE :: JPERM1,JPERM2
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: ITAG
      INTEGER IERROR, NF  
C     REAL
      my_real
     .   BID, XK, XX, X1, X2, X234(3), YY, Y1, Y2, R, XMIN, XMAX,
     .    TIME, FUNCT,SCALEY
      my_real,
     .        DIMENSION(2) :: XD2
      my_real,
     .        DIMENSION(:),ALLOCATABLE :: XV1, XSTOR1, XSTOR2
      my_real,
     .        DIMENSION(:,:),ALLOCATABLE :: XV2
      CHARACTER TITR*nchartitle, MESS*40, KEY*ncharfield
      DATA MESS/' FUNCTION & TABLE DEFINITION            '/
      LOGICAL :: IS_CRYPTED, IS_AVAILABLE
C======================================================================|
!     Initialization
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/TABLE/0')
      DO I=1,NTABLE0 
       CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                         OPTION_TITR = TITR, 
     .                         OPTION_ID   = ID) 
       CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C-----------------------------------------------
       NX(1) = 0
       NX(2) = 0
       NX(3) = 0
       NX(4) = 0
       CALL HM_GET_INTV('ORDER', NDIM, IS_AVAILABLE, LSUBMODEL)
       IF(NDIM/=1.AND.NDIM/=2.AND.NDIM/=3.AND.NDIM/=4)THEN
         CALL ANCMSG(MSGID=777,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=LL,
     .               C1=TITR)
       END IF
       IF(NDIM > 0)CALL HM_GET_INTV('N1', NX(1), IS_AVAILABLE, LSUBMODEL)
       IF(NDIM > 1)CALL HM_GET_INTV('N2', NX(2), IS_AVAILABLE, LSUBMODEL)
       IF(NDIM > 2)CALL HM_GET_INTV('N3', NX(3), IS_AVAILABLE, LSUBMODEL)
       IF(NDIM > 3)CALL HM_GET_INTV('N4', NX(4), IS_AVAILABLE, LSUBMODEL)
       IF(NDIM==1)THEN
         CYCLE
       ENDIF
       L=L+1
       TABLE(L)%NOTABLE=ID
       TABLE(L)%NDIM=NDIM
       ALLOCATE(TABLE(L)%X(NDIM),STAT=stat)
       IF(STAT/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='TABLE')

       DO K=1,NDIM
         IF(NX(K) < 2)THEN
           CALL ANCMSG(MSGID=778,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=K)
         END IF !NX(K) < 2
       ENDDO !K=1,NDIM
       !ndim = 1 
       ALLOCATE(TABLE(L)%X(1)%VALUES(NX(1)),STAT=stat)
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                   MSGTYPE=MSGERROR,
     .                                   C1='TABLE')
C
      ! read abscissa values for this parameter           
       DO N =1, NX(1)
         CALL HM_GET_FLOAT_ARRAY_INDEX('temparray2d_N1', XK, N, IS_AVAILABLE, LSUBMODEL, UNITAB)            
         TABLE(L)%X(1)%VALUES(N)= XK
       END DO
       !ndim = 2 
       IF(NDIM > 1 ) THEN 
         ALLOCATE(TABLE(L)%X(2)%VALUES(NX(2)),STAT=stat)
         IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                   MSGTYPE=MSGERROR,
     .                                   C1='TABLE')
        ! read abscissa values for this parameter           
         DO N =1, NX(2)
           CALL HM_GET_FLOAT_ARRAY_INDEX('temparray2d_N2',XK,N,IS_AVAILABLE, LSUBMODEL, UNITAB)
           TABLE(L)%X(2)%VALUES(N)= XK
         END DO
         IF(NDIM > 2 ) THEN 
            ALLOCATE(TABLE(L)%X(3)%VALUES(NX(3)),STAT=stat)
            IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                     MSGTYPE=MSGERROR,
     .                                     C1='TABLE')
             DO N =1, NX(3)
               CALL HM_GET_FLOAT_ARRAY_INDEX('temparray2d_N3',XK,N,IS_AVAILABLE, LSUBMODEL, UNITAB)
               TABLE(L)%X(3)%VALUES(N)= XK
             END DO
           IF(NDIM > 3 ) THEN 
             ALLOCATE(TABLE(L)%X(4)%VALUES(NX(4)),STAT=stat)
             IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .      MSGTYPE=MSGERROR,                                
     .      C1='TABLE')                                      
             DO N =1, NX(4)                                                                         
              CALL HM_GET_FLOAT_ARRAY_INDEX('temparray2d_N4',XK,N,IS_AVAILABLE, LSUBMODEL, UNITAB)  
              TABLE(L)%X(4)%VALUES(N)= XK                                                           
             END DO                                                                                 
           ENDIF !NDIM > 3
         ENDIF!(NDIM > 2 )
       ENDIF !(NDIM > 1 )   
       ! number of ordinate values                                                                
       NY=1                                                                                      
       DO K=1,NDIM                                                                               
         NY=NY*SIZE(TABLE(L)%X(K)%VALUES)                                                        
       END DO                                                                                    
       ALLOCATE(TABLE(L)%Y,STAT=stat)                                                            

       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,                                       
     .                                MSGTYPE=MSGERROR,                                          
     .                        C1='TABLE')                                                        

       ALLOCATE(TABLE(L)%Y%VALUES(NY),STAT=stat)                                                 

       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,                                       
     .                                MSGTYPE=MSGERROR,                                          
     .                        C1='TABLE')                                                        
C
       ! read ordinate values                                                                    
       DO N = 1, NY                                                                              
        CALL HM_GET_FLOAT_ARRAY_INDEX('ENG_FUNCT_yValues',YY,N,IS_AVAILABLE, LSUBMODEL, UNITAB)  
        TABLE(L)%Y%VALUES(N)=YY                                                                  
       ENDDO !N = 1, NY                                                                          


       IF (IS_CRYPTED)THEN                                     
         WRITE(IOUT,'(A)')'CONFIDENTIAL DATA'
       ELSE
         WRITE(IOUT,2100) TABLE(L)%NOTABLE, TABLE(L)%NDIM
         DO K=1,TABLE(L)%NDIM
           NX(K)=SIZE( TABLE(L)%X(K)%VALUES )
           WRITE(IOUT,2200) K
           WRITE(IOUT,2250) (TABLE(L)%X(K)%VALUES(N),N=1,NX(K))
         END DO
         NY=SIZE(TABLE(L)%Y%VALUES)
         WRITE(IOUT,2300)
         WRITE(IOUT,2350) (TABLE(L)%Y%VALUES(N),N=1,NY)
       END IF
      END DO  
      RETURN

C-----------------------------------------------------------------
2000  FORMAT(//
     .        '    TABLES'/
     .        '    ------'/
     .        '    NUMBER OF TABLES . . . . . . . . . . =',I10/)
2100  FORMAT(/'    TABLE ID . . . . . . . . . . . . . . =',I10/
     .        '    NUMBER OF PARAMETERS . . . . . . . . =',I10/)
2200  FORMAT(/'    VALUES FOR PARAMETER NUMBER. . . . . .',I4,':'/)
2250  FORMAT((3X,5(1X,G20.13))/)
2300  FORMAT(/'    ORDINATE VALUES . . . . . . . . . . . :'/)
2350  FORMAT((3X,5(1X,G20.13))/)
      END
