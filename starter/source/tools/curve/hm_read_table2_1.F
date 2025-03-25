Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    hm_read_table2_1           ../starter/source/tools/curve/hm_read_table2_1.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_table2             ../starter/source/tools/curve/hm_read_table.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../starter/source/output/message/message.F
      !||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    simple_checksum            ../starter/source/tools/curve/simple_checksum.cpp
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||    submodel_mod               ../starter/share/modules1/submodel_mod.F
      !||    table_mod                  ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_TABLE2_1(NFUNCT, NTABLE1, TABLE ,ITAB  ,UNITAB, LSUBMODEL )       
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
      use simple_checksum_mod
      use myqsort_d_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer :: NFUNCT
      integer :: NTABLE1
      integer ,intent(inout) :: itab
      TYPE(SUBMODEL_DATA) ,dimension(NSUBMOD) ,INTENT(IN)    :: LSUBMODEL
      TYPE(TTABLE)        ,dimension(NFUNCT)  ,INTENT(INOUT) :: TABLE
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer :: I,II,K,N,NDIM,sizeh,STAT
      integer :: ifun,nfun
      integer :: IDEB,IFIN,IOK 
      integer :: FUNC_ID,TABLE_ID
      integer :: KK,LL,NN,NF,NP,N1,N11,N12,N13,KK1
      integer :: i1,i2,countx
      integer :: ierror,errorstop
      integer :: nx1,nx2,nx3,nx4,ny
      integer :: NX(4),NOK(4)
      my_real :: X0,X1,X2,X3,YY,Y1,Y2,R,XMIN,XMAX,SCALEY
      my_real :: X234(3)
      double precision :: h1,h2
      double precision :: chksum
      double precision :: hasht(5)
      double precision ,dimension(:) ,allocatable :: hash
      integer ,dimension(:)   ,allocatable :: jperm1,jperm2
      integer ,dimension(:)   ,allocatable :: idfun,lenx,idxtab
      integer ,dimension(:)   ,allocatable :: nv1,nv2,nv3
      my_real ,dimension(:)   ,allocatable :: xx1,xx2,xx3,xx4,yfac
      integer ,dimension(:,:) ,allocatable :: itag
      character(LEN=NCHARTITLE) :: TITR
      logical :: IS_ENCRYPTED, IS_AVAILABLE
!===================================================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/TABLE/1')

      DO I=1,NTABLE1 

        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = TABLE_ID) 
        ! --------------------------------------------------------------------

        CALL HM_GET_INTV('ORDER', NDIM, IS_AVAILABLE, LSUBMODEL)
        IF (NDIM/=1.AND.NDIM/=2.AND.NDIM/=3.AND.NDIM/=4)THEN
          CALL ANCMSG(MSGID=777, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .                I1=LL, C1=TITR)
        END IF
        NX(:) = 0

        IF (NDIM == 1) THEN
          CYCLE
        END IF
!
        ITAB = ITAB + 1
        TABLE(ITAB)%NOTABLE = TABLE_ID
        TABLE(ITAB)%NDIM    = NDIM
        ALLOCATE(TABLE(ITAB)%X(NDIM),STAT=stat)
        IF (STAT/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='TABLE')

        CALL HM_GET_INTV('curverows', NFUN, IS_AVAILABLE, LSUBMODEL) !number of functions
        IF (NFUN == 1) THEN
            CALL ANCMSG(MSGID=778,  MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .                       I1=TABLE_ID, C1=TITR, I2=NFUN)
        END IF
!       
        allocate (idfun(nfun))
        allocate (idxtab(nfun))
        allocate (xx2(nfun))
        allocate (xx3(nfun))
        allocate (xx4(nfun))
        allocate (yfac(nfun))
        allocate (hash(nfun))
        allocate (lenx(nfun))
        allocate (jperm2(nfun))
        allocate (nv1(nfun))
        allocate (nv2(nfun))
        allocate (nv3(nfun))
!       
        ! check function IDs   
        ! count sum of all X abscissa values for all functions
        nx1  = 0                                                                                      
        nx2  = 0
        nx3  = 0
        nx4  = 0
        XMIN =  EP20                                                                                  
        XMAX = -EP20                                                                                  
        errorstop = 0
        sizeh = 5
        IDFUN(:) = 0
        K = 0                                                                                       
        DO ifun = 1,NFUN
          CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',FUNC_ID,ifun,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('A'               ,X1     ,ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('B'               ,X2     ,ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('C'               ,X3     ,ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX ('Fscale_array'   ,SCALEY ,ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)   
!
          DO NF = 1,NFUNCT
            IF (TABLE(NF)%NOTABLE == FUNC_ID) THEN
              IF (SCALEY == ZERO) SCALEY = ONE
              IDFUN(ifun) = FUNC_ID
              IDXTAB(ifun) = NF
              xx2(ifun) = X1
              xx3(ifun) = X2
              xx4(ifun) = X3
              YFAC(ifun)  = SCALEY
              hasht(1) = FUNC_ID
              hasht(2) = X1
              hasht(3) = X2
              hasht(4) = X3
              hasht(5) = SCALEY
              call simple_checksum(hasht,sizeh,chksum)
              hash(ifun) = chksum
              LENX(ifun) = SIZE(TABLE(NF)%X(1)%VALUES)
              nx1 = nx1 + lenx(ifun)                                                                     
              EXIT
            END IF
          END DO                          
!
          IF (IDFUN(ifun) == 0) THEN
            CALL ANCMSG(MSGID=781, MSGTYPE=MSGERROR, ANMODE=ANINFO,    
     .                  I1=TABLE_ID,  C1=TITR, I2=FUNC_ID)
            errorstop = 1       
          END IF
          nx(1) = nx1
        ENDDO ! ifun = 1,NFUN
!-----------------------------------------------------------------------------------------
!       CHECK INPUT ERRORS
!-----------------------------------------------------------------------------------------
        ! check duplicated input lines
!
        call myqsort_d(nfun,hash,jperm2,ierror)
        h1 = hash(1)
        do ifun = 2,nfun
          h2 = hash(ifun)
          i1 = jperm2(ifun-1)
          i2 = jperm2(ifun)
          if (h1 == h2) then
            call ancmsg(MSGID=3087, MSGTYPE=MSGERROR, ANMODE=ANINFO, 
     .                  I1=TABLE_ID,C1=titr, I2=idfun(i2))                            
            errorstop = 1       
          end if
          h1 = h2
        END DO
!-----------------------------------------------------------------------------------------        
        ! check contradictory data (different funct+scale values for identical abscissa set)
        sizeh = ndim-1
        do ifun = 1,nfun
          hasht(1) = xx2(ifun)
          hasht(2) = xx3(ifun)
          hasht(3) = xx4(ifun)
          call simple_checksum(hasht,sizeh,chksum)
          hash(ifun) = chksum
        end do  
        ! hash of all abscissa combinations
!
        call myqsort_d(nfun,hash,jperm2,ierror)
        h1 = hash(1)
        do ifun = 2,nfun
          h2 = hash(ifun)
          i1 = jperm2(ifun-1)
          i2 = jperm2(ifun)
          if (h1 == h2 .and. (idfun(i1) /= idfun(i2) .or. yfac(i1) /= yfac(i2))) then
            call ancmsg(MSGID=3088, MSGTYPE=MSGERROR, ANMODE=ANINFO, 
     .          I1=TABLE_ID,C1=titr, I2=idfun(i1),R1=yfac(i1),I3=idfun(i2),R2=yfac(i2))                       
            errorstop = 1
          end if
          h1 = h2
        end do  
!-----------------------------------------------------------------------------------------
        ! check if input grid of abscissa values in all dimensons is complete
!-----------------------------------------------------------------------------------------
        if (ndim > 1) then
          call myqsort(nfun,xx2,jperm2,ierror)           
          nx2 = 1
          nv1(:) = 1
          do ifun = 2,nfun
            x2 = xx2(ifun)
            if (x2 == xx2(nx2)) then
              nv1(nx2) = nv1(nx2) + 1
            else if (x2 > xx2(nx2)) then
              nx2 = nx2 + 1
              xx2(nx2) = x2
            end if
          end do
          nx(2) = nx2
          do ifun = 2,nx2
            if (nv1(ifun) /= nv1(ifun-1)) then
             CALL ANCMSG(MSGID=3089, MSGTYPE=MSGERROR, ANMODE=ANINFO, 
     .                  I1=TABLE_ID,C1=TITR)
            errorstop = 1       
            end if
          end do                                     
        end if
        if (ndim > 2) then
          call myqsort(nfun,xx3,jperm2,ierror)           
          nx3 = 1
          nv2(:) = 1
          do ifun = 2,nfun
            x2 = xx3(ifun)
            if (x2 == xx3(nx3)) then
              nv2(nx3) = nv2(nx3) + 1
            else if (x2 > xx3(nx3)) then
              nx3 = nx3 + 1
              xx3(nx3) = x2
            end if
          end do
          nx(3) = nx3
          do ifun = 2,nx3
            if (nv2(ifun) /= nv2(ifun-1)) then
             CALL ANCMSG(MSGID=3089, MSGTYPE=MSGERROR, ANMODE=ANINFO, 
     .                  I1=TABLE_ID,C1=TITR)
            errorstop = 1       
            end if
          end do                                     
        end if
        if (ndim > 3) then
          call myqsort(nfun,xx4,jperm2,ierror)           
          nx4 = 1
          nv3(:) = 1
          do ifun = 2,nfun
            x2 = xx4(ifun)
            if (x2 == xx4(nx4)) then
              nv3(nx4) = nv3(nx4) + 1
            else if (x2 > xx4(nx4)) then
              nx4 = nx4 + 1
              xx4(nx4) = x2
            end if
          end do
          nx(4) = nx4
          do ifun = 2,nx4
            if (nv3(ifun) /= nv3(ifun-1)) then
             CALL ANCMSG(MSGID=3089, MSGTYPE=MSGERROR, ANMODE=ANINFO, 
     .                  I1=TABLE_ID,C1=TITR)
            errorstop = 1       
            end if
          end do                                     
        end if
!----------------------------------------------------------------------------
        !< create common principal abscissa vector for all input functions
!----------------------------------------------------------------------------
        allocate (xx1(nx1))
        allocate (jperm1(nx1))
!
        XMIN =  EP20                                                                
        XMAX = -EP20                                                                
        K = 0                                                                     
        DO ifun = 1,NFUN
          II = IDXTAB(ifun)
          DO NP=1,LENX(ifun)                                                
            K = K+1 
            xx1(K) = TABLE(II)%X(1)%VALUES(NP)                                 
            XMIN  = MIN(XMIN,xx1(K))                                            
            XMAX  = MAX(XMAX,xx1(K))                                            
          END DO                                                                
        END DO  !ifun = 1, NFUN    
        !< sort global X abscissa vector  
        call myqsort(nx1,xx1,jperm1,ierror)

        !< save ordered abscissa values, skipping doubles
        countx = 1                               
        X1 = xx1(countx)                          
        DO K = 2,nx1                              
          X1 = xx1(K)                            
          IF (xx1(K) > xx1(countx)) THEN                 
            countx = countx+1                         
            xx1(countx) = xx1(K)                    
          END IF                                  
        END DO
        nx1 = countx         
!
        if (errorstop == 1) return                           
!----------------------------------------------------------------------------
        !< ALLOCATE AND FILL TABLE ABSCISSA AND VALUES
!----------------------------------------------------------------------------
        allocate(table(itab)%x(1)%values(nx1),STAT=stat)
        if (stat /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP, MSGTYPE=MSGERROR)           
        allocate(table(itab)%y,STAT=stat)
        if (stat /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP, MSGTYPE=MSGERROR)           
        ny = nx1

        table(itab)%x(1)%values(1:nx1) = xx1(1:nx1)                   
!
        if (ndim > 1) then
          ny = ny * nx2
          allocate(table(itab)%x(2)%values(nx2))
          table(itab)%x(2)%values(1:nx2) = xx2(1:nx2)
        end if
        if (ndim > 2) then
          ny = ny * nx3
          allocate(table(itab)%x(3)%values(nx3))
          table(itab)%x(3)%values(1:nx3) = xx3(1:nx3)
        end if
        if (ndim > 3) then
          ny = ny * nx4
          allocate(table(itab)%x(4)%values(nx4))
          table(itab)%x(4)%values(1:nx4) = xx4(1:nx4)
        end if
!
        allocate(table(itab)%y%values(ny),STAT=stat)
        IF (stat /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR, C1='TABLE')
!-----------------------------------------------------------------------------------------
        !< Calculate table values for full grid of abscissa in all directions
!-----------------------------------------------------------------------------------------
        ALLOCATE(ITAG(nx1,NX2*MAX(1,NX3)*MAX(1,NX4)), STAT=stat)                                     
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR, C1='ITAG')                          
        ITAG(:,:) = 0                                                   
        nx(1) = nx1
        nx(2) = nx2
        nx(3) = nx3
        nx(4) = nx4
!
        DO ifun = 1, NFUN
          CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',FUNC_ID,ifun,IS_AVAILABLE,LSUBMODEL)          
          CALL HM_GET_FLOAT_ARRAY_INDEX ('A'              ,X234(1),ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX ('B'              ,X234(2),ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX ('C'              ,X234(3),ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX ('Fscale_array'   ,SCALEY ,ifun,IS_AVAILABLE,LSUBMODEL,UNITAB)   
!
          IF (SCALEY == ZERO) SCALEY = ONE 
          NOK=0                                                   
          DO N=2,NDIM                                             
            IDEB=1                                                
            IFIN=NX(N)                                            
            IOK=0                                                 
            DO WHILE(IOK==0)                                      
              IF(IFIN-IDEB==1)THEN                                
                K=IDEB                                            
                IF (TABLE(ITAB)%X(N)%VALUES(K)==X234(N-1)) THEN        
                ELSE                                              
                  K=K+1                                           
                END IF                                            
                IOK=1                                             
              ELSE                                                
                K=(IDEB+IFIN)/2                                   
                IF(TABLE(ITAB)%X(N)%VALUES(K) > X234(N-1))THEN       
                  IFIN=K                                          
                ELSEIF(TABLE(ITAB)%X(N)%VALUES(K) < X234(N-1))THEN   
                  IDEB=K                                          
                ELSE                                              
                  IOK=1                                           
                END IF                                            
              END IF                                              
              NOK(N)=K                                            
            END DO                                                
          END DO                                                  
!
          DO NF=1,NFUNCT                                              
            IF(TABLE(NF)%NOTABLE==FUNC_ID)THEN                         
              NOK(1)=1                                                
              DO NP=1,SIZE(TABLE(NF)%X(1)%VALUES)                     
                X1=TABLE(NF)%X(1)%VALUES(NP)                          
                DO WHILE(X1 > TABLE(ITAB)%X(1)%VALUES(NOK(1)))           
                  NOK(1)=NOK(1)+1                                     
                END DO                                                
                NN=1                                                  
                KK=NOK(1)                                             
                DO N=2,NDIM                                           
                  NN=NN*NX(N-1)                                       
                  KK=NN*(NOK(N)-1)+KK                                 
                END DO                                                
                TABLE(ITAB)%Y%VALUES(KK)=TABLE(NF)%Y%VALUES(NP)*SCALEY   
                KK = MAX(1,NX(3))*NX(2)*(MAX(1,NOK(4))-1) + NX(2)*(MAX(1,NOK(3))-1) + NOK(2)
                ITAG(NOK(1),KK)=1
              END DO                                                  
              EXIT                                                    
            END IF                                                    
          END DO                                                      
        END DO     !  ifun = 1, NFUN
C-------------------------------------------------------------------------------
C interpolation
        DO KK=1,NX(2)*MAX(1,NX(3))*MAX(1,NX(4))    
          N11=1                                    
          DO WHILE(ITAG(N11,KK)==0)                
            N11=N11+1                              
          END DO                                   
          N12=N11+1                                
          DO WHILE(ITAG(N12,KK)==0)                
            N12=N12+1                              
            IF(N12 > nx1)THEN                    
c error  (function 1 pt)
            END IF                           
          END DO                             
          X1=TABLE(ITAB)%X(1)%VALUES(N11)       
          X2=TABLE(ITAB)%X(1)%VALUES(N12)       
          KK1=nx1*(KK-1)+N11               
          Y1=TABLE(ITAB)%Y%VALUES(KK1)          
          KK1=nx1*(KK-1)+N12               
          Y2=TABLE(ITAB)%Y%VALUES(KK1)          
          DO N1=1,N12                        
            IF(N1/=N11) THEN                 
              X0=TABLE(ITAB)%X(1)%VALUES(N1)    
              R=(X2-X0)/(X2-X1)              
              YY=R*Y1+(ONE-R)*Y2              
              KK1=nx1*(KK-1)+N1            
              TABLE(ITAB)%Y%VALUES(KK1)=YY      
              ITAG(N1,KK)=1                  
            END IF                           
          END DO                             
 200      CONTINUE                         
          N13=N12+1                        
          DO WHILE(N13 <= nx1)           
            IF (ITAG(N13,KK) == 0) THEN    
              N13=N13+1                    
            ELSE                           
              EXIT                         
            ENDIF                          
          END DO                           
          IF(N13 > nx1)THEN              
             X1=TABLE(ITAB)%X(1)%VALUES(N11)    
             X2=TABLE(ITAB)%X(1)%VALUES(N12)    
             KK1=nx1*(KK-1)+N11            
             Y1=TABLE(ITAB)%Y%VALUES(KK1)       
             KK1=nx1*(KK-1)+N12            
             Y2=TABLE(ITAB)%Y%VALUES(KK1)       
             DO N1=N12+1,nx1               
               X0=TABLE(ITAB)%X(1)%VALUES(N1)   
               R=(X2-X0)/(X2-X1)             
               YY=R*Y1+(ONE-R)*Y2             
               KK1=nx1*(KK-1)+N1           
               TABLE(ITAB)%Y%VALUES(KK1)=YY     
               ITAG(N1,KK)=1                 
             END DO                          
          ELSE                                
              N11=N12                           
              N12=N13                           
              IF(N12 > N11+1)THEN               
                X1=TABLE(ITAB)%X(1)%VALUES(N11)    
                X2=TABLE(ITAB)%X(1)%VALUES(N12)    
                KK1=nx1*(KK-1)+N11            
                Y1=TABLE(ITAB)%Y%VALUES(KK1)       
                KK1=nx1*(KK-1)+N12            
                Y2=TABLE(ITAB)%Y%VALUES(KK1)       
                DO N1=N11+1,N12-1               
                  X0=TABLE(ITAB)%X(1)%VALUES(N1)   
                  R=(X2-X0)/(X2-X1)             
                  YY=R*Y1+(ONE-R)*Y2             
                  KK1=nx1*(KK-1)+N1           
                  TABLE(ITAB)%Y%VALUES(KK1)=YY     
                  ITAG(N1,KK)=1                 
                END DO                          
              END IF                            
              GO TO 200                         
          END IF                              
        END DO    ! KK
!-----------------------------------------------------------------------------------------
        deallocate(itag)
        deallocate (xx1)      
        deallocate (xx2)      
        deallocate (xx3)      
        deallocate (xx4)      
        deallocate (hash)     
        deallocate (lenx)     
        deallocate (yfac)     
        deallocate (jperm1)   
        deallocate (jperm2)   
        deallocate (nv1)      
        deallocate (nv2)      
        deallocate (nv3)      
        deallocate (idxtab)   
        deallocate (idfun)
!-----------------------------------------------------------------
!       OUTPUT OF TABLE VALUES
!-----------------------------------------------------------------
        NY=SIZE(TABLE(ITAB)%Y%VALUES)
        IF (IS_ENCRYPTED)THEN                                     
          WRITE(IOUT,'(A)')'CONFIDENTIAL DATA'
        ELSE
          WRITE(IOUT,2000) NTABLE1
          WRITE(IOUT,2100) TABLE(ITAB)%NOTABLE, TABLE(ITAB)%NDIM
          DO K=1,TABLE(ITAB)%NDIM
            NX(K) = SIZE(TABLE(ITAB)%X(K)%VALUES)
            WRITE(IOUT,2200) K
            WRITE(IOUT,2250) (TABLE(ITAB)%X(K)%VALUES(N),N=1,NX(K))
          END DO
          NY = SIZE(TABLE(ITAB)%Y%VALUES)
          WRITE(IOUT,2300)
          WRITE(IOUT,2350) (TABLE(ITAB)%Y%VALUES(N),N=1,NY)
        END IF

      END DO  ! NTABLE1
!
      RETURN
!-----------------------------------------------------------------
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
C-----------------------------------------------------------------
      END
