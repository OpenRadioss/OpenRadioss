!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    law87_upd_mod   ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat          ../starter/source/materials/updmat.F
      !||====================================================================
      module law87_upd_mod
      contains
      ! ======================================================================================================================
      ! \brief Updating material parameters of /MAT/LAW87
      ! \details Updating material parameters of /MAT/LAW87
      ! ======================================================================================================================
      !||====================================================================
      !||    law87_upd                ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat                   ../starter/source/materials/updmat.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    crityld2000              ../starter/source/materials/mat/mat087/law87_upd.F90
      !||    finter                   ../starter/source/tools/curve/finter.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    invert                   ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    prodmatvect              ../starter/source/materials/mat/mat087/law87_upd.F90
      !||    r_yld2000                ../starter/source/materials/mat/mat087/law87_upd.F90
      !||    table_mat_vinterp        ../starter/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    table_mat_vinterp_mod    ../starter/source/materials/tools/table_mat_vinterp.F
      !||    table_mod                ../starter/share/modules1/table_mod.F
      !||====================================================================
      subroutine law87_upd(                                                    &
        iout     ,titr     ,mat_id   ,matparam )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use matparam_def_mod
        use constant_mod
        use message_mod
        use table_mod
        use table_mat_vinterp_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
#include     "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)                    :: iout     !< Output file unit
        character(len=nchartitle), intent(in)  :: titr     !< Title of the material
        integer, intent(in)                    :: mat_id   !< Material ID
        type(matparam_struct_), intent(inout)  :: matparam !< Material parameters data structure
!-----------------------------------------------
!   F u n c t i o n s
!-----------------------------------------------
        my_real :: finter
        external finter
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: k,j,error,iflag,niter,iok,ipos(1,2)
        my_real ::                                                             &
            gamma,delta,fct,yield,dx,dy,scale,g1,g2,g3,g13,g23,g33,g15,g25,g35,&
            dydx,df1,df2,df3,df13,df23,df33,df15,df25,df35,f1,f2,al1,al2,al3,  &
            al4,al5,al6,al7,al8,s00,s45,s90,sb,dv1a7,dv11a8,dw11a8,r00,r45,r90,&
            rb,x1,x2,x11,x22 ,v1,v11,w11,tal7,tal8,aswift,epso,qvoce,beta,ko,  &
            alpha,nexp,conjtf4a8,tf4a8,dal78(2),expv,kswift,kvoce,puis,pla,    &
            residu,g(2),dal(8),dftest(6,6),dg(2,2),al(8),f(8),df(6,6),         &
            dfinv(6,6),res(6,6),test(3,3),testinv(3,3),dginv(2,2),dyld_dp(1),  &
            xvec(1,2),yld(1),expa,expam2,k1,k2,ahs,bhs,mhs,eps0hs,nhs,hmart,   &
            temp0,expo,aexp,atemp,vm0
!      
        logical  is_encrypted
!=======================================================================
        is_encrypted = .false.
        call hm_option_is_encrypted(is_encrypted)
!
        !< Recovering the material parameters
        s00    = matparam%uparam(3)
        s45    = matparam%uparam(4)
        s90    = matparam%uparam(5)
        sb     = matparam%uparam(6)
        r00    = matparam%uparam(7)
        r45    = matparam%uparam(8)
        r90    = matparam%uparam(9)
        rb     = matparam%uparam(10)
        expa   = matparam%uparam(12)
        expam2 = expa - 2 
        iflag  = matparam%iparam(1)
!
        !< Initialisation of the yield stress at 0.002 plastic strain
        pla = two*em03
        !< - Swift-Voce yield stress type
        if (iflag == 1) then 
          aswift = matparam%uparam(13) 
          nexp   = matparam%uparam(14)
          alpha  = matparam%uparam(15) 
          epso   = matparam%uparam(16) 
          qvoce  = matparam%uparam(17)
          beta   = matparam%uparam(18)
          ko     = matparam%uparam(19)
          puis   = exp(nexp*log((pla + epso)))
          kswift = aswift*puis
          expv   = exp(-beta*pla)
          kvoce  = ko + qvoce*(one - expv)                                       
          yield  = alpha*kswift + (one-alpha)*kvoce
        !< - Hansel Spittel yield stress type
        elseif (iflag == 2) then 
          k1     = matparam%uparam(13) 
          k2     = matparam%uparam(14)
          ahs    = matparam%uparam(15) 
          bhs    = matparam%uparam(16)
          mhs    = matparam%uparam(17)
          eps0hs = matparam%uparam(18)
          nhs    = matparam%uparam(19)
          hmart  = matparam%uparam(20)
          temp0  = matparam%uparam(21)   
          vm0    = matparam%uparam(32)
          expo   = exp(nhs*log(pla + eps0hs))
          aexp   = (bhs - ahs)*exp(-mhs*expo)
          atemp  = k1 + k2*temp0
          yield  = (bhs-aexp)*atemp + hmart*vm0
        !< - Tabulated yield stress type + Orthotropic 3 direction yield stress type
        else 
          xvec(1,1) = pla
          xvec(1,2) = zero
          ipos(1,1) = 1
          ipos(1,2) = 1
          call table_mat_vinterp(matparam%table(1),1,1,ipos,xvec,yld,dyld_dp)
          yield = yld(1)
        endif  
!
        !< Start newton loops to find al parameters from 1 to 6
        residu  = ep30
        niter   = 0
        iok     = 0
        al(1:8) = one
        gamma   = two_third
        delta   = -third
        do while (abs(residu) > em06 .and. niter < 100 .and. iok == 0)
!
          call crityld2000(fct,gamma, delta, expa, al)      
          f(1) = fct - two * (yield /s00)**expa
!    
          call crityld2000(fct,delta,gamma,  expa, al)
          f(2) = fct - two * (yield /s90)**expa
!    
          call crityld2000(fct,delta,delta,  expa, al)
          f(3) = fct - two * (yield /sb)**expa
!    
          call r_yld2000(dx,dy,gamma, delta, expa, al)
          f(4) = (one-r00)*dx - (two + r00)*dy
!    
          call r_yld2000(dx,dy, delta,gamma, expa, al)
          f(5) = (two+r90)*dx + (r90 -one)*dy
!    
          call r_yld2000(dx,dy,delta, delta, expa, al)
          f(6) = (one+two *rb)*dx - (two + rb)*dy
! 
          !< Jacobian matrix
          df1 = (al(1)*two_third + al(2)*third)*abs(al(1)*two_third + al(2)*third)**expam2
          df(1,1) = two_third*expa*df1
          df(1,2) = third*expa *df1
          df13 = (al(3)*two_third - two*al(4)*third)*abs(al(3)*two_third - two*al(4)*third)**expam2
          df(1,3) = two_third*expa*df13
          df(1,4) = -two_third*expa*df13
          df15 = (two*al(5)*two_third - al(6)*third)*abs(two*al(5)*two_third - al(6)*third)**expam2
          df(1,5) = two*two_third*expa*df15
          df(1,6) = -third*expa*df15
!
          df2 = (-al(1)*third - al(2)*two_third)*abs(-al(1)*third - al(2)*two_third)**expam2
          df(2,1) = -third *expa *df2
          df(2,2) = -two_third*expa *df2
          df23 = (-al(3)*third + two*al(4)*two_third)*abs(-al(3)*third + two*al(4)*two_third)**expam2
          df(2,3) = -third      *expa*df23
          df(2,4) = two*two_third *expa*df23
          df25 = (-two*al(5)*third + al(6)*two_third)*abs(-two*al(5)*third + al(6)*two_third)**expam2
          df(2,5) = -two_third*expa*df25
          df(2,6) =  two_third*expa*df25
!
          df3 = (-al(1)*third + al(2)*third)*abs(-al(1)*third + al(2)*third)**expam2
          df(3,1) = -third*expa*df3
          df(3,2) = third*expa *df3
          df33 = (-al(3)*third - two*al(4)*third)*abs(-al(3)*third - two*al(4)*third)**expam2
          df(3,3) = -third  *expa*df33
          df(3,4) = -two_third *expa*df33
          df35 = (-two*al(5)*third - al(6)*third)*abs(-two*al(5)*third - al(6)*third)**expam2
          df(3,5) = -two*third*expa*df35
          df(3,6) = -third     *expa*df35
!
          g1 = al(1)*two_third+al(2)*third
          df(4,1) = ( (one-r00  )*g1 + two_third*(expa-one)*( al(1)*(one-r00)+al(2)*(two+r00) )) *abs(g1)**expam2
          df(4,2) = ( (two+r00)*g1 + third *(expa-one)*( al(1)*(one-r00)+al(2)*(two+r00) )) *abs(g1)**expam2
          g13 = al(3)*two_third-two*al(4)*third
          df(4,3) = ( (one-r00  )*g13      + two_third*(expa-one)*( al(3)*(one-r00)-two*al(4)*(two+r00) )) *abs(g13)**expam2
          df(4,4) = (-two*(two+r00)*g13 - third *(expa-one)*( al(3)*(one-r00)-two*al(4)*(two+r00) )) *abs(g13)**expam2
          g15 = two*al(5)*two_third-al(6)*third
          df(4,5) = ( two*(one-r00)*g15 + two*two_third*(expa-one)*( two*al(5)*(one-r00)-al(6)*(two+r00) )) *abs(g15)**expam2
          df(4,6) = ( -(two+r00)*g15   - third*(expa-one)      *( two*al(5)*(one-r00)-al(6)*(two+r00) )) *abs(g15)**expam2
!
          g2 = -al(1)*third-al(2)*two_third
          df(5,1) = ( (two+r90)*g2 -third  *(expa-one)*( al(1)*(two+r90)+al(2)*(one-r90) )) *abs(g2)**expam2
          df(5,2) = ( (one-r90  )*g2 -two_third *(expa-one)*( al(1)*(two+r90)+al(2)*(one-r90) )) *abs(g2)**expam2
          g23 = -al(3)*third+two*al(4)*two_third
          df(5,3) = ( (two+r90)   *g23  -third    *(expa-one)*( al(3)*(two+r90)-two*al(4)*(one-r90) )) *abs(g23)**expam2
          df(5,4) = (-two*(one-r90)*g23  + two_third  *(expa-one)*( al(3)*(two+r90)-two*al(4)*(one-r90) )) *abs(g23)**expam2
          g25 = -two*al(5)*third + al(6)*two_third
          df(5,5) = ( two*(two+r90)*g25 - two*third*(expa-one)*( two*al(5)*(two+r90)-al(6)*(one-r90) )) *abs(g25)**expam2
          df(5,6) = ( -      (one-r90)*g25 + two_third*(expa-one)    *( two*al(5)*(two+r90)-al(6)*(one-r90) )) *abs(g25)**expam2
!
          g3 = -al(1)*third + al(2)*third
          df(6,1) = ( (one+two*rb)*g3 -third  *(expa-one)*( al(1)*(one+two*rb)+al(2)*(two+rb) )) *abs(g3)**expam2
          df(6,2) = ( (two+rb   )*g3 +third *(expa-one)* ( al(1)*(one+two*rb)+al(2)*(two+rb) )) *abs(g3)**expam2
          g33 = -al(3)*third-two*al(4)*third
          df(6,3) = ( (one+two*rb)   *g33  -third    *(expa-one)*( al(3)*(one+two*rb)-two*al(4)*(two+rb) )) *abs(g33)**expam2
          df(6,4) = (-two*(two+rb) *g33  -third    *(expa-one)*( al(3)*(one+two*rb)-two*al(4)*(two+rb) )) *abs(g33)**expam2
          g35 = -two*al(5)*third - al(6)*third
          df(6,5) = ( two*(one+two*rb)*g35 - two*third*(expa-one)*( two*al(5)*(one+two*rb)-al(6)*(two+rb) )) *abs(g35)**expam2
          df(6,6) = ( -      (two+rb) *g35 - third     *(expa-one)*( two*al(5)*(one+two*rb)-al(6)*(two+rb) )) *abs(g35)**expam2
!
          call invert(df,dfinv,6,error)
!
          call prodmatvect(dfinv, f, dal, 6)
     
          do k = 1, 6
            al(k) = al(k) - dal(k)
            if (al(k) > ep05)then
                iok = 1
                call ancmsg(msgid=1608 ,                                       &
                            msgtype=msgerror,                                  &
                            anmode=aninfo_blind_2,                             &
                            i1=mat_id,                                         &
                            c1=titr)
               exit
            endif
          enddo
          residu = sqrt(f(1)**2+f(2)**2+f(3)**2+f(4)**2+f(5)**2+f(6)**2)/6
          niter = niter + 1 
        enddo
!
        !< Compute al(7) and al(8)
        x1  = (al(1)+al(2))/3;
        x2  = (al(1)-al(2))/3;
        x11 = (al(3)     + two*al(4) + two*al(5)+al(6)      ) /nine
        x22 = (two*al(5)+      al(6) - al(3)     -two *al(4))/three
!
        !< Loop
        residu = ep30
        niter  = 0
        do while (abs(residu) > em06 .and. niter < 100 .and. iok == 0) 
!
          tal7 = sqrt(max(zero,x2**2  + four * al(7)**2) )/two
          tal8 = sqrt(max(zero,x22**2 + four * al(8)**2) )
          tf4a8=      (three*x11 + tal8 ) / four 
          conjtf4a8 = (three*x11 - tal8 ) / four 
  
          v1  = expa*(tal7)**(expa-1)
          v11 = expa*(conjtf4a8)*(abs(conjtf4a8))**(expa-2)
          w11 = expa*(tf4a8)*(abs(tf4a8))**(expa-2)
  
          f1= tal7**expa + abs(conjtf4a8)**expa + abs(tf4a8)**expa
          f2 =v1* (x2**2/(two*tal7) )   +  three_half*x11*(v11+w11) + half*(x22**2/tal8) * (w11-v11)          
          
          g(1) = f1 - two*(yield/s45)**expa
          g(2) = f2 - (two*expa/(one+r45))*(yield/s45)**expa  
  
          dv1a7  = expa*(expa-one)*al(7) * tal7**(expa-3)
          dv11a8 = -(expa*(expa-one)*al(8) /tal8 ) *  abs(conjtf4a8)**(expa-2)
          dw11a8 =  (expa*(expa-one)*al(8) /tal8 ) *  abs(tf4a8 ) **(expa-2)
  
          dg(1,1) = (expa * abs(tal7)**(expa-1)) * al(7)/tal7  
          dg(1,2) = expa *( al(8) /tal8) * ( tf4a8 * abs (tf4a8)**(expa-2) - conjtf4a8 *abs (conjtf4a8)**(expa-2)  )                               
          dg(2,1) = dv1a7 *  x2**2 /(two*tal7) - four * v1 * al(7) * x2**2  / sqrt(max(zero,x2**2  + four * al(7)**2) )**3
          dg(2,2) = three_half*x11 * (dv11a8 + dw11a8) + half * (x22**2/tal8) * (dw11a8- dv11a8) - (w11 - v11)*al(8)*x22**2/tal8**3

          call invert (dg,dginv,2,error)
          call prodmatvect(dginv, g, dal78, 2)
          do k = 1, 2
            al(6+k) = al(6+k) - dal78(k)
            if (al(6+k) > ep05) then
              iok = 1
              call ancmsg(msgid=1608 ,                                         &                            
                          msgtype=msgerror,                                    &
                          anmode=aninfo_blind_2,                               & 
                          i1=mat_id,                                           &
                          c1=titr)
              exit
            endif
          enddo
          residu = sqrt(g(1)**2+g(2)**2)/2
          niter = niter + 1 
        enddo ! while iterations

        if (iok ==0) then
          do k=1,8
            if (al(k) <= zero .or. al(k) > ten) then
              iok = 1
              call ancmsg(msgid=1608 ,                                         &
                          msgtype=msgerror,                                    &
                          anmode=aninfo_blind_2,                               &
                          i1=mat_id,                                           &
                          c1=titr)                   
              exit
            endif
          enddo
        endif
!
        !< Update the material parameters
        matparam%uparam(3) = al(1)
        matparam%uparam(4) = al(2)    
        matparam%uparam(5) = al(3)
        matparam%uparam(6) = al(4)
        matparam%uparam(7) = al(5)
        matparam%uparam(8) = al(6)
        matparam%uparam(9) = al(7)
        matparam%uparam(10)= al(8)
!
        !< Printing the fitted alpha parameters
        if (is_encrypted) then
          write(iout,'(5x,a,//)')'confidential data'
        else  
          if (iok == 0) then
            write(iout,1000) 
            write(iout,1001) trim(titr), mat_id, 87
            write(iout,1200) al(1),al(2),al(3),al(4),al(5),al(6),al(7),al(8)
          endif
        endif  
!
 1000 format(/                                                                 &
     5X,'-------------------------------------------------------',/            &     
     5X,'      UPDATE MATERIAL MODEL: BARLAT YLD2000            ',/,           & 
     5X,'-------------------------------------------------------',/)
 1001 format(/                                                                 &
     5X,A,/,                                                                   &
     5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . . . .=',I10/,        &
     5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . . . .=',I10/)
 1200 format(/                                                                 &
     5X,'FITTED BARLAT 2000 ANISOTROPY COEFFICIENTS:            ',/,           &
     5X,'-------------------------------------------            ',/,           &
     5X,'ANISOTROPY COEFFICIENT ALPHA1. . . . . . . . . . . . .=',1PG20.13/    &
     5X,'ANISOTROPY COEFFICIENT ALPHA2. . . . . . . . . . . . .=',1PG20.13/    &
     5X,'ANISOTROPY COEFFICIENT ALPHA3. . . . . . . . . . . . .=',1PG20.13/    &
     5X,'ANISOTROPY COEFFICIENT ALPHA4. . . . . . . . . . . . .=',1PG20.13/    &
     5X,'ANISOTROPY COEFFICIENT ALPHA5. . . . . . . . . . . . .=',1PG20.13/    &
     5X,'ANISOTROPY COEFFICIENT ALPHA6. . . . . . . . . . . . .=',1PG20.13/    &
     5X,'ANISOTROPY COEFFICIENT ALPHA7. . . . . . . . . . . . .=',1PG20.13/    &
     5X,'ANISOTROPY COEFFICIENT ALPHA8. . . . . . . . . . . . .=',1PG20.13/)
!
      end subroutine law87_upd
!
      !||====================================================================
      !||    crityld2000    ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- called by ------------------------------------------------------
      !||    law87_upd      ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- uses       -----------------------------------------------------
      !||    message_mod    ../starter/share/message_module/message_mod.F
      !||====================================================================
      subroutine crityld2000(                                                  &
        f        ,g        ,d        ,aa       ,al       )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use message_mod
        use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
#include     "my_real.inc"
!-----------------------------------------------
!     A r g u m e n t s
!-----------------------------------------------
        my_real,  intent(in)               :: aa
        my_real,  dimension(8), intent(in) :: al 
        my_real , intent(in)               :: g
        my_real , intent(in)               :: d
        my_real , intent(out)              :: f
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!=======================================================================
        f =  ( abs(g*al(1)      - d*al(2) ) ) **aa                             &
           + ( abs(g*al(3)      + d*two*al(4)) )**aa                           &
           + ( abs(g*two*al(5)+ d*al(6)))**aa
      end subroutine crityld2000
!
      !||====================================================================
      !||    r_yld2000      ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- called by ------------------------------------------------------
      !||    law87_upd      ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- uses       -----------------------------------------------------
      !||    message_mod    ../starter/share/message_module/message_mod.F
      !||====================================================================
      subroutine r_yld2000(dx,dy,gamma, delta, a, al)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use message_mod
        use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
#include     "my_real.inc"
!-----------------------------------------------
!     A r g u m e n t s
!-----------------------------------------------
        my_real, intent(in)               :: a
        my_real, dimension(8), intent(in) :: al 
        my_real, intent(in)               :: gamma
        my_real, intent(in)               :: delta
        my_real, intent(out)              :: dx
        my_real, intent(out)              :: dy
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: idfc,idfd ,aa
        my_real :: s00,s45,s90,sb,r00,r45,r90,rb  
!=======================================================================
       aa = a - 2 
       dx = al(1)*(gamma*al(1)-delta*al(2))    *(abs(gamma*al(1)    - delta*al(2)     ))**aa  &
      +     al(3)*(gamma*al(3)+delta*two*al(4))*(abs(gamma*al(3)    + delta*two*al(4)))**aa   &
      + two*al(5)*(gamma*two*al(5)+delta*al(6))*(abs(gamma*two*al(5)+delta*al(6)))**aa    
       dy = -al(2)*(gamma*al(1)-delta*al(2))  *(abs(gamma*al(1)-delta*al(2)  ))**aa           &
      +   2*al(4)*(gamma*al(3)+delta*2*al(4))*(abs(gamma*al(3)+delta*2*al(4)))**aa           &
      +     al(6)*(gamma*2*al(5)+delta*al(6))*(abs(gamma*2*al(5)+delta*al(6)))**aa
      end subroutine r_yld2000
!
      !||====================================================================
      !||    prodmat        ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- uses       -----------------------------------------------------
      !||    message_mod    ../starter/share/message_module/message_mod.F
      !||====================================================================
      subroutine prodmat(a, b, c, n)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use message_mod
        use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
#include     "my_real.inc"
!-----------------------------------------------
!     A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)                    :: n
        my_real, dimension(n,n), intent(in)    :: a
        my_real, dimension(n,n), intent(in)    :: b
        my_real, dimension(n,n), intent(inout) :: c
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer i, j, k
!=======================================================================
!
        do i = 1, n
           do j = 1, n
              c(i,j) = zero
           enddo
        enddo
        do i = 1, n
           do j = 1, n
              do k = 1, n
                 c(i,j) = c(i,j) + a(i,k) * b(k,j)
              enddo
           enddo
        enddo
!
      end subroutine prodmat
!
      !||====================================================================
      !||    prodmatvect    ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- called by ------------------------------------------------------
      !||    law87_upd      ../starter/source/materials/mat/mat087/law87_upd.F90
      !||--- uses       -----------------------------------------------------
      !||    message_mod    ../starter/share/message_module/message_mod.F
      !||====================================================================
      subroutine prodmatvect(a, b, c, n)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use message_mod
        use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
#include     "my_real.inc"
!-----------------------------------------------
!     A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)                    :: n
        my_real, dimension(n,n), intent(in)    :: a
        my_real, dimension(n),   intent(in)    :: b
        my_real, dimension(n),   intent(inout) :: c
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer i, j, k
!=======================================================================
!
        do i = 1, n
           c(i) = zero
        enddo
        do i = 1, n
           do j = 1, n
               c(i) = c(i) + a(i,j) * b(j)
           enddo
        enddo
!
      end subroutine prodmatvect 
      end module law87_upd_mod
