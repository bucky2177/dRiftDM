#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// this function is a C++ version of the kfe function provided by
// Richter, Ulrich, Janczyk (2023) Journal of Mathematica Modeling
// [[Rcpp::export]]
int cpp_kfe_drift(
	    const double E,  // expected value of normal distributed drift
	    const double V,  // variance of normal dist. drift
	    NumericVector& pdf_u,
	    NumericVector& pdf_l,
	    NumericVector& xx,
	    const int nt, const int nx,
	    const double dt, const double dx,
	    double sigma,
	    const double b_val) {

  if (pdf_u.size() != nt+1){
    Rcerr << "pdf-upper has wrong size!" << std::endl; return -1;
  }
  if (pdf_l.size() != nt+1) {
    Rcerr << "pdf-lower has wrong size!" << std::endl; return -1;
  }

  if (pdf_l.size() != nt+1) {
    Rcerr << "pdf-lower has wrong size!" << std::endl; return -1;
  }
  if (xx.size() != nx+1) {
    Rcerr << "x_vals has wrong size!" << std::endl; return -1;
  }

  NumericVector f(nx+1, 0.);  // storing the solution

  // set pdf's to zero as we will add for different drift-points
  for (int n=0; n<=nt; ++n) {
    pdf_u[n]=0;
    pdf_l[n]=0;
  }

  sigma = sigma/b_val;   // transform sigma to domain size. (fixed b)

  const double NQ = 5;
  const double X[5] = {-2.02018287, -0.95857246,  0.        ,  0.95857246,  2.02018287};
  const double W[5] = {0.01995324, 0.39361932, 0.94530872, 0.39361932, 0.01995324};


  for (int q=0;q<NQ;++q) // loop over quad-points
    {
      double wgt = W[q]/sqrt(3.141592653589793);   // quadrature weight
      double mu = sqrt(2.0*V)*X[q]+E;              // quadrature point (value of drift)
      mu = mu/b_val;  // transform to domain of correct size (fixed b)

      for (int n=1; n<=nt; ++n) {
	double theta = 0.5;

	const double L = 1.0 / dx * sigma * sigma / 2.0;

	// decide discretization of 1st order term. if possible central
	// but one-sided if drift dominates
	double dx1 = -0.5;
	double dx2 =  0.0;
	double dx3 =  0.5;
	for (int i=1;i<nx;++i)
	  {
	    if (-L + dx3 * mu>0)
	      {
		dx1 = -1.0; dx2 = 1.0; dx3 = 0.0;
		break;
	      }
	    else if (-L + dx1 * mu>0)
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
	    dt *( (     -L + dx1 * mu) * xx[i-1]
		  +      (+2. * L + dx2 * mu) * xx[i]
		  +      (     -L + dx3 * mu) * xx[i+1] );

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
	  a[i] =      dt * theta * (-L      + dx1 * mu);
	  b[i] = dx + dt * theta * (2.0 * L + dx2 * mu);
	  c[i] =      dt * theta * (-L      + dx3 * mu);
	}
	if ( (f[0]!=0) || (f[nx]!=0) ) {
	  Rcerr << "rhs not zero on thresholds!" << std::endl; return -1;
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


	// add to pdf with correct quadrature weight
	pdf_u[n] += wgt * (0.5 * sigma*sigma/ dx / dx *
			   (3.0 * xx[nx-1] - 1.5 * xx[nx-2] + 1.0 / 3.0 * xx[nx -3]));
	pdf_l[n] += wgt * (0.5 * sigma*sigma / dx / dx *
			   (3.0 * xx[1] - 1.5 * xx[2] + 1.0 / 3.0 * xx[3]));

      }
    }
  return 1;
}
