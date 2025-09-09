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
      module table_mat_spline_fit_mod
        use iso_c_binding
        implicit none
        interface
          subroutine table_mat_spline_fit(s_raw,x_raw,y_raw,nout,x_out,y_out,lambda,info) bind(C, name="cpp_table_mat_spline_fit")
            use iso_c_binding
            integer(c_int), value :: s_raw
            real(c_double), dimension(s_raw) :: x_raw
            real(c_double), dimension(s_raw) :: y_raw
            integer(c_int), value :: nout
            real(c_double), dimension(nout+1) :: x_out
            real(c_double), dimension(nout+1) :: y_out
            real(c_double), value :: lambda
            real(c_double), intent(out) :: info
          end subroutine table_mat_spline_fit
        end interface 
      end module table_mat_spline_fit_mod