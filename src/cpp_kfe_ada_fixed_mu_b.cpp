#include <Rcpp.h>
#include <iostream>
#include <fstream>
using namespace Rcpp;

// this function is a C++ version of the kfe function provided by
// Richter, Ulrich, Janczyk (2023) Journal of Mathematical Psychology
// [[Rcpp::export]]
int cpp_kfe_ada_fixed_mu_b(NumericVector& pdf_u,
            NumericVector& pdf_l,
            NumericVector& xx,
            const int nt,
            const int nx,
            const double dtbase,
            const double dx,
            const double sigma,
            const double b_val,
            const double mu_val,
            const NumericVector& x_vec) {

  if (pdf_u.size() != nt+1){
    stop("pdf-upper has wrong size!");
  }
  if (pdf_l.size() != nt+1) {
    stop("pdf-lower has wrong size!");
  }
  if (! (b_val>0)){
    stop("b_val must be positive!");
  }
  if (xx.size() != nx+1) {
    stop("xx has wrong size!");
  }
  if (x_vec.size() != nx+1) {
    stop("x_vec has wrong size!");
  }

  const double tol = 1.e-11;

  NumericVector f(nx+1, 0.);  // storing the solution
  NumericVector a(nx+1, 0.), b(nx+1, 0.), c(nx+1, 0.), xxnew(nx+1,0.);

  double acc = 0;
  double tt = 0.0;

  const double J0 = b_val;  // b is fixed, and we can compute some stuff outside the loop.
  const double sigma0 = sigma / J0;
  const double L0 = 1.0/dx * sigma0 * sigma0 / 2.0;
  const double mu0 = mu_val/J0;

  for (int n=1; n<=nt; ++n) {
    // for time-stepping, the actual values will be interpolated between new and old
    tt = (n-1)*dtbase;
    double dt = dtbase;
    double time = 0;
    int nsteps = 0;

    while (time < dtbase-tol) // iterate until we complete the time step
    {
      f[0] = 0.0;
      f[nx] = 0.0;

      for (int i=1;i<nx;++i)
        f[i] = dx * xx[i] - dt * 0.5 *
          ( (     -L0 - .5 * mu0) * xx[i-1]
          + (+2. * L0           ) * xx[i]
          + (     -L0 + .5 * mu0) * xx[i+1] );

      // set matrix. a,b is never overwritten and can be used as number
      const double a = dt * 0.5 * (-L0 - 0.5 * mu0);
      const double b = dx + dt * .5 * (2.0 * L0);
      for (int i=0; i<nx+1; ++i) {
//        b[i] = dx + dt * .5 * (2.0 * L0);
        c[i] =      dt * .5 * (     -L0 + .5 * mu0);
      }
      if ( (f[0]!=0) || (f[nx]!=0) ) {
        stop("rhs not zero on thresholds!");
      }
      c[0]  = 0.0; // a[0]=a[nx]=0
      c[nx] = 0.0; // b[0]=b[bx]=1
      // solve
      for (int i=1;i<nx; ++i)
        c[i] /= (b-c[i-1]*a);

      for (int i=1;i<nx;++i)
        f[i] = (f[i]-f[i-1]*a)/(b-c[i-1]*a);

      bool stepok = true;
      xxnew[nx] = f[nx];
      for (int i=nx-1; i>=0; --i)
      {
        xxnew[i] = f[i]-c[i]*xxnew[i+1];
        if (xxnew[i] < - tol)
        {
          stepok = false;
          break;
        }
      }
      if (stepok)
      {
        for (int i=0;i<=nx;++i)
          xx[i] = xxnew[i];
//          std::ofstream OUT("x.out",std::ios::app);
//          OUT << tt << "\t" << dt << std::endl;
//          OUT.close();
          time += dt;
          tt += dt;
          dt = std::min(dtbase, 2.0 * dt);    // increase timestep
          dt = std::min(dt, dtbase - time); // not larger than step
      }
      else // repeat step
        dt = 0.125 * dt;

      if (nsteps > 1000) {
        stop("Extreme Parameters: number of adaptive steps exceeded!");
      }

      ++nsteps;
    }

    pdf_u[n] = 0.5 * sigma0*sigma0/ dx / dx *
        (3.0 * xx[nx-1] - 1.5 * xx[nx-2] + 1.0 / 3.0 * xx[nx -3]);
    pdf_l[n] = 0.5 * sigma0*sigma0 / dx / dx *
        (3.0 * xx[1] - 1.5 * xx[2] + 1.0 / 3.0 * xx[3]);
    if (pdf_u[n]<-tol)
      pdf_u[n] = 0.5 * sigma0*sigma0/ dx / dx *
        (1.0 * xx[nx-1]);
    if (pdf_l[n]<-tol)
      pdf_l[n] = 0.5 * sigma0*sigma0/ dx / dx *
        (1.0 * xx[1]);


    acc += pdf_u[n] + pdf_l[n];
    if (acc * dx * dtbase > 0.999)
      break;
  }

  return 1;
}
