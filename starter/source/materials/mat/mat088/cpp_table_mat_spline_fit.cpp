#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <iomanip>
#include <limits>
#include <algorithm>

using namespace std;

double bspline_basis(int i, int k, double t, const vector<double>& knots) {

    const double tol = 1e-12;
    int n_bases = knots.size() - k - 1;

    if (std::abs(t - knots.back()) < tol) {
        return (i == n_bases - 1) ? 1.0 : 0.0;
    }

    if (k == 0) {
        if (t >= knots[i] && t < knots[i + 1])
            return 1.0;
        else
            return 0.0;
    }

    double a = 0.0, b = 0.0;
    double denom1 = knots[i + k] - knots[i];
    double denom2 = knots[i + k + 1] - knots[i + 1];

    if (denom1 > 0.0)
        a = ((t - knots[i]) / denom1) * bspline_basis(i, k - 1, t, knots);
    if (denom2 > 0.0)
        b = ((knots[i + k + 1] - t) / denom2) * bspline_basis(i + 1, k - 1, t, knots);

    return a + b;
}

void build_bspline_matrix(const vector<double>& x, const vector<double>& knots, int degree,
                          vector<vector<double>>& B) {
    int n = x.size();
    int n_bases = knots.size() - degree - 1;
    B.assign(n, vector<double>(n_bases, 0.0));

    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n_bases; ++j)
            B[i][j] = bspline_basis(j, degree, x[i], knots);
}

double bspline_derivative(int i, int k, double x, const vector<double>& knots) {
    double left = 0.0, right = 0.0;

    if (k == 0) return 0.0;

    double denom_left = knots[i + k] - knots[i];
    if (denom_left > 0.0)
        left = k / denom_left * bspline_basis(i, k - 1, x, knots);

    double denom_right = knots[i + k + 1] - knots[i + 1];
    if (denom_right > 0.0)
        right = k / denom_right * bspline_basis(i + 1, k - 1, x, knots);

    return x*(left - right);
}

void build_second_derivative_matrix(int n, vector<vector<double>>& D) {
    D.assign(n - 2, vector<double>(n, 0.0));
    for (int i = 0; i < n - 2; ++i) {
        D[i][i] = 1.0;
        D[i][i + 1] = -2.0;
        D[i][i + 2] = 1.0;
    }
}

void transpose(const vector<vector<double>>& A, vector<vector<double>>& At) {
    int m = A.size(), n = A[0].size();
    At.assign(n, vector<double>(m));
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < n; ++j)
            At[j][i] = A[i][j];
}

void matmul(const vector<vector<double>>& A, const vector<vector<double>>& B, vector<vector<double>>& C) {
    int m = A.size(), n = B[0].size(), p = A[0].size();
    C.assign(m, vector<double>(n, 0.0));
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < n; ++j)
            for (int k = 0; k < p; ++k)
                C[i][j] += A[i][k] * B[k][j];
}

void matvec(const vector<vector<double>>& A, const vector<double>& x, vector<double>& y) {
    int m = A.size(), n = A[0].size();
    y.assign(m, 0.0);
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < n; ++j)
            y[i] += A[i][j] * x[j];
}

void solve_symmetric_system(vector<vector<double>> A, vector<double> b, vector<double>& x) {
    int n = A.size();
    x = b;
    for (int i = 0; i < n; ++i) {
        double pivot = A[i][i];
        for (int j = i + 1; j < n; ++j) {
            double alpha = A[j][i] / pivot;
            for (int k = i; k < n; ++k)
                A[j][k] -= alpha * A[i][k];
            x[j] -= alpha * x[i];
        }
    }
    for (int i = n - 1; i >= 0; --i) {
        for (int j = i + 1; j < n; ++j)
            x[i] -= A[i][j] * x[j];
        x[i] /= A[i][i];
    }
}

void project_halfspaces_dykstra(const vector<vector<double>>& A,
                                const vector<double>& b,
                                vector<double>& x,
                                int max_iters = 200,
                                double tol = 1e-12) {
    const int m = (int)A.size();
    if (m == 0) return;
    const int n = (int)A[0].size();

    vector<vector<double>> p(m, vector<double>(n, 0.0)); // “corrections” Dykstra
    vector<double> xk = x, xprev(n);

    for (int it=0; it<max_iters; ++it){
        xprev = xk;
        for (int i=0; i<m; ++i){
            vector<double> y(n);
            for (int j=0;j<n;++j) y[j] = xk[j] + p[i][j];

            const vector<double>& ai = A[i];
            double ai2 = 0.0, dot = 0.0;
            for (int j=0;j<n;++j){ ai2 += ai[j]*ai[j]; dot += ai[j]*y[j]; }
            if (ai2 <= 1e-20) continue;

            vector<double> z = y;
            if (dot < b[i]){
                double corr = (b[i] - dot) / ai2;
                for (int j=0;j<n;++j) z[j] += corr * ai[j];
            }

            for (int j=0;j<n;++j){
                p[i][j] = y[j] - z[j]; 
                xk[j]   = z[j];
            }

        }
        double diff = 0.0;
        for (int j=0;j<n;++j){ double d = xk[j]-xprev[j]; diff += d*d; }
        if (sqrt(diff) < tol) break;
    }
    x = xk;
}

void solve_qp_pgd(const vector<vector<double>>& H, const vector<double>& rhs,
                  const vector<vector<double>>& Bp, vector<double>& c,
                  int max_iters, double tol, double alpha) {
    
    int n = H.size();
    c.assign(n, 0.0);

    vector<double> c_prev(n,0.0), yk(n,0.0), grad(n,0.0);
    solve_symmetric_system(H, rhs, c); 
    c_prev = c;
    double tk = 1.0;
    for (int it=0; it<max_iters; ++it){
        // yk = c + ((t_{k-1}-1)/t_k)*(c - c_prev)
        double tk1 = 0.5*(1.0 + sqrt(1.0 + 4.0*tk*tk));
        double beta = (tk - 1.0)/tk1;
        for(int i=0;i<n;++i) yk[i] = c[i] + beta*(c[i]-c_prev[i]);
    
        // grad = H*yk - rhs
        for(int i=0;i<n;++i){ double s=0.0; for(int j=0;j<n;++j) s += H[i][j]*yk[j]; grad[i] = s - rhs[i]; }

        double grad_norm = 0.0;
        for (double g : grad) grad_norm += g*g;
        grad_norm = sqrt(grad_norm);      
        if (grad_norm > 1e-2) {      
            alpha *= 0.5;            
        } else if (grad_norm < 1e-6) { 
            alpha *= 1.1;            
        }
        if (alpha < 1e-6) alpha = 1e-6;
        if (alpha > 1.0)  alpha =  1.0;
    
        vector<double> cnew(n);
        for(int i=0;i<n;++i) cnew[i] = yk[i] - alpha*grad[i];
    
        vector<vector<double>> A = Bp;  
        vector<double> b(A.size(), 0.0);
        
        for (int i=0; i<(int)A.size(); ++i) {
            double norm = 0.0;
            for (double v : A[i]) norm += v*v;
            norm = sqrt(norm);
            if (norm > 1e-12) {
                for (double &v : A[i]) v /= norm;
                b[i] /= norm;
            }
        }
        project_halfspaces_dykstra(Bp, b, cnew, 200, 1e-12);
    
        // Convergence & update
        double pg_norm = 0.0;
        for (int i=0;i<n;++i){ double d = yk[i] - cnew[i]; pg_norm += d*d; }
        pg_norm = sqrt(pg_norm);
        if (it == 0) tol = 1.0e-8*pg_norm; // tol relatif sur la première itération
        if (pg_norm < tol) break;
        
        // Update variables
        c_prev = c;
        c = cnew;
        tk = tk1;
    }
}

void build_monotonic_sigma_derivative_matrix(const vector<double>& x_eval, const vector<double>& knots, int degree,
                                             vector<vector<double>>& Bp) {
    int rows = x_eval.size();
    int cols = knots.size() - degree - 1;
    Bp.assign(rows - 1, vector<double>(cols, 0.0));

    vector<vector<double>> B;
    build_bspline_matrix(x_eval, knots, degree, B);

    for (int i = 1; i < rows; ++i) {
        double dx = x_eval[i] - x_eval[i - 1];
        for (int j = 0; j < cols; ++j) {
            double sig_i   = x_eval[i]     * B[i][j];
            double sig_im1 = x_eval[i - 1] * B[i - 1][j];
            Bp[i - 1][j] = (sig_i - sig_im1) / dx;
        }
    }
}

//----------------------------------------------------------------------------------------------------------------
// C Interface to Fortran
// ---------------------------------------------------------------------------------------------------------------
extern "C" {
    void cpp_table_mat_spline_fit(int s_inp, double* x_inp,double* y_inp,int nout, double* x_out, 
                                  double* y_out, double lambda, double& info) {
 
        vector<double> x_raw, y_raw;
        double x, y;

        x_raw.clear();
        y_raw.clear();
        for (int i = 0; i < s_inp; ++i) {
          x = x_inp[i];
          y = y_inp[i];
          if (x != 0.0 && y != 0.0) {
              x_raw.push_back(x);
              y_raw.push_back(y/x);
          } else if (x == 0.0 && y == 0.0) {
              x_raw.push_back(0.0);
              y_raw.push_back(0.0);
          }
        }

        int degree = 3;
        int n = x_raw.size();
        int n_knots = std::max(10, std::min(n / 10, 30));

        double xmin = x_raw.front(), xmax = x_raw.back();
        vector<double> knots;
        for (int i = 0; i < degree; ++i) knots.push_back(xmin);
        for (int i = 0; i < n_knots; ++i)
            knots.push_back(xmin + i * (xmax - xmin) / (n_knots - 1));
        for (int i = 0; i < degree; ++i) knots.push_back(xmax);
        int n_bases = knots.size() - degree - 1;
       
        vector<vector<double>> B, D, Bt, Dt, H, tmp;
        build_bspline_matrix(x_raw, knots, degree, B);
        build_second_derivative_matrix(n_bases, D);

        transpose(B, Bt);    
        transpose(D, Dt);
    
        matmul(Bt, B, H);
        matmul(Dt, D, tmp);
        for (int i = 0; i < n_bases; ++i)
            for (int j = 0; j < n_bases; ++j)
                H[i][j] += lambda * tmp[i][j];


        // For numerical stability, add a small value to diagonal
        double maxdiag = 0.0;
        for (int i=0;i<n_bases;++i) maxdiag = max(maxdiag, H[i][i]);
        double eps = 1e-4 * maxdiag;
        for (int i = 0; i < n_bases; ++i)
          H[i][i] += eps;
    
        // Right-hand side: B^T * y_raw
        vector<double> rhs;
        matvec(Bt, y_raw, rhs);
        
        // Build derivative constraint matrix
        int nderiv = 100;

        // Points where to enforce derivative constraints
        vector<double> x_eval(nderiv);
        for (int i = 0; i < nderiv; ++i)
            x_eval[i] = xmin + i * (xmax - xmin) / (nderiv - 1);
        vector<vector<double>> Bp;
        build_monotonic_sigma_derivative_matrix(x_eval, knots, degree, Bp);

        // Compute the solver step size
        double maxHii = 0.0;
        for (int i=0;i<(int)H.size();++i) maxHii = max(maxHii, H[i][i]);
        double alpha = (maxHii > 0.0) ? 0.25 / maxHii : 1e-3;

        // Max iterations and tolerance
        int max_iters = 10000;
        double tol = 1e-09;

        // Solve the QP problem using projected gradient descent
        vector<double> c;
        solve_qp_pgd(H, rhs, Bp, c, max_iters, tol, alpha);
        
        // Compute and report the constraint violation
        vector<double> deriv;
        matvec(Bp, c, deriv);
        double violation_sum = 0.0;
        for (double d : deriv) if (d < 0) violation_sum += -d;
        info = violation_sum;

        // Evaluate the fitted spline at output points
        int nd = 0;
        for (int i = 0; i < nout; ++i) {
            double x = xmin + (xmax - xmin) * i / (nout - 1);
            double sig = 0.0;
            for (int j = 0; j < n_bases; ++j) {
                sig += c[j] * bspline_basis(j, degree, x, knots);
            }
            x_out[nd] = x;
            y_out[nd] = x*sig;
            nd = nd + 1;
            if (i < nout - 1) {
              double x_next = xmin + (xmax - xmin) * (i+1) / (nout - 1);
              if (x <= 0.0 && x_next >= 0.0) {
                x_out[nd] = 0.0;
                y_out[nd] = 0.0;
                nd = nd + 1;
              }
            }
        }
    }
}
