#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// this function is a C++ version of the kfe function provided by
// Richter, Ulrich, Janczyk (2023) Journal of Mathematica Modeling
// [[Rcpp::export]]
int cpp_kfe(NumericVector& pdf_u,
             NumericVector& pdf_l,
             NumericVector& xx,
             const int nt, const int nx,
             const double dt, const double dx,
             double sigma, const bool r_stepping,
             const NumericVector b_vals,
             const NumericVector mu_vals,
             const NumericVector dt_b_vals,
             const NumericVector x_vec) {

  if (pdf_u.size() != nt+1){
    Rcerr << "pdf-upper has wrong size!" << std::endl; return -1;
  }
  if (pdf_l.size() != nt+1) {
    Rcerr << "pdf-lower has wrong size!" << std::endl; return -1;
  }
  if (b_vals.size() != nt+1) {
    Rcerr << "b_vals has wrong size!" << std::endl; return -1;
  }
  if (mu_vals.size() != nt+1) {
    Rcerr << "mu_vals has wrong size!" << std::endl; return -1;
  }
  if (dt_b_vals.size() != nt+1) {
    Rcerr << "dt_b_vals has wrong size!" << std::endl; return -1;
  }
  if (pdf_l.size() != nt+1) {
    Rcerr << "pdf-lower has wrong size!" << std::endl; return -1;
  }
  if (xx.size() != nx+1) {
    Rcerr << "x_vals has wrong size!" << std::endl; return -1;
  }
  if (x_vec.size() != nx+1) {
    Rcerr << "x_vec has wrong size!" << std::endl; return -1;
  }

  NumericVector f(nx+1, 0.);  // storing the solution
  NumericVector mu_old(nx+1, 0.);
  NumericVector mu_new(nx+1, 0.);


  for (int n=1; n<=nt; ++n) {
    // Rannacher time-stepping, required for Dirac initial
    double theta = 0.5;
    if (n <= 4 && r_stepping) {
      theta = 1.0;
    }

    // at old time step
    const double J_old = b_vals[n-1];
    for (int i=0; i<nx+1; ++i) {
      mu_old[i] = (mu_vals[n-1] - dt_b_vals[n-1] * x_vec[i]) / J_old;
    }
    const double sigma_old = sigma / J_old;
    const double Lold = 1.0 / dx * sigma_old * sigma_old / 2.0;

    // at new time step
    const double J_new = b_vals[n];
    for (int i=0; i<nx+1; ++i) {
      mu_new[i] = (mu_vals[n] - dt_b_vals[n] * x_vec[i]) / J_new;
    }
    const double sigma_new = sigma / J_new;
    const double L_new = 1.0 / dx * sigma_new * sigma_new / 2.0;

    for (int i=0; i<nx+1; ++i) {
      f[i] = 2.0/3.0 * dx * xx[i];
    }

    for (int i=1;i<nx;++i) {
      f[i] += 1.0/6.0 * dx * (xx[i-1]+xx[i+1])
              + (theta-1.0) * dt * ( (-Lold - 0.5 * mu_old[i-1]) * xx[i-1]
                                    + 2.0 * Lold * xx[i]
                                    +(-Lold + 0.5 * mu_old[i+1]) * xx[i+1] );
    }

    if ( (f[0]!=0) || (f[nx]!=0) ) {
      Rcerr << "rhs not zero on thresholds!" << std::endl; return -1;
    }

    NumericVector a(nx+1, 0.), b(nx+1, 0.), c(nx+1, 0.);
    for (int i=0; i<nx+1; ++i) {
      a[i] = 1./6. * dx + dt * theta * (-L_new - 0.5 * mu_new[i]);
      b[i] = 2./3. * dx + dt * theta * (2.0 * L_new);
      c[i] = 1./6. * dx + dt * theta * (-L_new + 0.5 * mu_new[i]);
    }
    // adapt for boundary
    a[0]  = 0.0; b[0]  = 1.0; c[0]  = 0.0;
    a[nx] = 0.0; b[nx] = 1.0; c[nx] = 0.0;
    // solve
    c[0] /= b[0];
    for (int i=1;i<nx; ++i) {
      c[i] /= (b[i]-c[i-1]*a[i]);
    }
    f[0] /= b[0];
    for (int i=1;i<=nx;++i) {
      f[i] = (f[i]-f[i-1]*a[i])/(b[i]-c[i-1]*a[i]);
    }
    xx[nx] = f[nx];
    for (int i=nx-1; i>=0; --i) {
      xx[i] = f[i]-c[i]*xx[i+1];
    }

    pdf_u[n] = 0.5 * sigma_new*sigma_new/ dx / dx *
      (3.0 * xx[nx-1] - 1.5 * xx[nx-2] + 1.0 / 3.0 * xx[nx -3]);
    pdf_l[n] = 0.5 * sigma_new*sigma_new / dx / dx *
      (3.0 * xx[1] - 1.5 * xx[2] + 1.0 / 3.0 * xx[3]);

  }
  return 1;
}
