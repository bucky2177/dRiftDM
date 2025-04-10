#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// this function is a C++ version of the kfe function provided by
// Richter, Ulrich, Janczyk (2023) Journal of Mathematical Psychology
// [[Rcpp::export]]
int cpp_kfe(NumericVector& pdf_u,
             NumericVector& pdf_l,
             NumericVector& xx,
             const int nt,
             const int nx,
             const double dt,
             const double dx,
             const double sigma,
             const NumericVector& b_vals,
             const NumericVector& mu_vals,
             const NumericVector& dt_b_vals,
             const NumericVector& x_vec) {

  if (pdf_u.size() != nt+1){
    stop("pdf-upper has wrong size!");
  }
  if (pdf_l.size() != nt+1) {
    stop("pdf-lower has wrong size!");
  }
  if (b_vals.size() != nt+1) {
    stop("b_vals has wrong size!");
  }
  if (mu_vals.size() != nt+1) {
    stop("mu_vals has wrong size!");
  }
  if (dt_b_vals.size() != nt+1) {
    stop("dt_b_vals has wrong size!");
  }
  if (xx.size() != nx+1) {
    stop("xx has wrong size!");
  }
  if (x_vec.size() != nx+1) {
    stop("x_vec has wrong size!");
  }

  NumericVector f(nx+1, 0.);  // storing the solution
  NumericVector mu_old(nx+1, 0.);
  NumericVector mu_new(nx+1, 0.);


  for (int n=1; n<=nt; ++n) {
    double theta = 0.5;

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

    // decide discretization of 1st order term. if possible central
    // but one-sided if drift dominates
    double dx1 = -0.5;
    double dx2 =  0.0;
    double dx3 =  0.5;
    for (int i=1;i<nx;++i)
    {
      if (-L_new + dx3 * mu_new[i]>0)
      {
        dx1 = -1.0; dx2 = 1.0; dx3 = 0.0;
        break;
      }
      else if (-L_new + dx1 * mu_new[i]>0)
      {
        dx1 = 0.0; dx2 = -1.0; dx3 = 1.0;
        break;
      }
    }

   // assemble rhs side (without old solution)
   // assemble without factor (theta -1)
   // afterwards we determine theta such that the result is
   // always positive
   f[0] = 0.0;
   f[nx] = 0.0;

   for (int i=1;i<nx;++i)
     f[i] =
        dt *( (     -Lold + dx1 * mu_old[i]) * xx[i-1]
       +      (+2. * Lold + dx2 * mu_old[i]  ) * xx[i]
       +      (     -Lold + dx3 * mu_old[i]) * xx[i+1] );

      // find maximum factor 'tt=theta-1' such that ( dx * x + tt * f >=0 )
   double tt = -0.5;
   for (int i=1;i<nx;++i)
     if (f[i]>0)
       tt = std::max(tt, -dx * xx[i] / f[i]);
   theta = std::min(1.0,tt+1.0);

   // now assemble rhs
   double fmin = 1.0;
   for (int i=0;i<nx+1;++i)
   {
     f[i] = dx * xx[i] + (theta-1.0) * f[i];
     fmin = std::min(fmin,f[i]);
   }

    NumericVector a(nx+1, 0.), b(nx+1, 0.), c(nx+1, 0.);
    for (int i=0; i<nx+1; ++i) {

      //// ASSEMBLE MATRIX
      // lumped mass matrix
      a[i] =      dt * theta * (-L_new      + dx1 * mu_new[i]);
      b[i] = dx + dt * theta * (2.0 * L_new + dx2 * mu_new[i]);
      c[i] =      dt * theta * (-L_new      + dx3 * mu_new[i]);
    }
    if ( (f[0]!=0) || (f[nx]!=0) ) {
      stop("rhs not zero on thresholds!");
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
