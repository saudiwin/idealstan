#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

<<<<<<< HEAD
RCPP_MODULE(stan_fit4binary_absence_inflate_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> >("model_binary_absence_inflate")
=======
RCPP_MODULE(stan_fit4binary_absence_inflate_fix_bill_sigma_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> >("model_binary_absence_inflate_fix_bill_sigma")
>>>>>>> c944809126e048a39b2578becc79588fbc11943e

    .constructor<SEXP,SEXP>()


<<<<<<< HEAD
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_namespace::model_binary_absence_inflate, boost::random::ecuyer1988> ::constrained_param_names)
=======
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_namespace::model_binary_absence_inflate_fix_bill_sigma, boost::random::ecuyer1988> ::constrained_param_names)
>>>>>>> c944809126e048a39b2578becc79588fbc11943e
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

<<<<<<< HEAD
RCPP_MODULE(stan_fit4binary_absence_inflate_fix_sigma_abs_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> >("model_binary_absence_inflate_fix_sigma_abs")
=======
RCPP_MODULE(stan_fit4binary_absence_inflate_fix_bill_sigma_abs_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> >("model_binary_absence_inflate_fix_bill_sigma_abs")
>>>>>>> c944809126e048a39b2578becc79588fbc11943e

    .constructor<SEXP,SEXP>()


<<<<<<< HEAD
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::constrained_param_names)
=======
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_bill_sigma_abs, boost::random::ecuyer1988> ::constrained_param_names)
>>>>>>> c944809126e048a39b2578becc79588fbc11943e
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

<<<<<<< HEAD
RCPP_MODULE(stan_fit4binary_absence_inflate_nofix_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> >("model_binary_absence_inflate_nofix")
=======
RCPP_MODULE(stan_fit4binary_absence_inflate_fix_person_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> >("model_binary_absence_inflate_fix_person")
>>>>>>> c944809126e048a39b2578becc79588fbc11943e

    .constructor<SEXP,SEXP>()


<<<<<<< HEAD
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::constrained_param_names)
=======
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_namespace::model_binary_absence_inflate_fix_person, boost::random::ecuyer1988> ::constrained_param_names)
>>>>>>> c944809126e048a39b2578becc79588fbc11943e
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

<<<<<<< HEAD
RCPP_MODULE(stan_fit4grm_absence_inflate_mod) {


    class_<rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> >("model_grm_absence_inflate")
=======
RCPP_MODULE(stan_fit4binary_absence_inflate_fix_person_bill_sigma_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> >("model_binary_absence_inflate_fix_person_bill_sigma")
>>>>>>> c944809126e048a39b2578becc79588fbc11943e

    .constructor<SEXP,SEXP>()


<<<<<<< HEAD
    .method("call_sampler", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::constrained_param_names)
=======
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_namespace::model_binary_absence_inflate_fix_person_bill_sigma, boost::random::ecuyer1988> ::constrained_param_names)
>>>>>>> c944809126e048a39b2578becc79588fbc11943e
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

<<<<<<< HEAD
RCPP_MODULE(stan_fit4grm_absence_inflate_nofix_mod) {


    class_<rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> >("model_grm_absence_inflate_nofix")
=======
RCPP_MODULE(stan_fit4binary_absence_inflate_fix_person_bill_sigma_abs_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> >("model_binary_absence_inflate_fix_person_bill_sigma_abs")
>>>>>>> c944809126e048a39b2578becc79588fbc11943e

    .constructor<SEXP,SEXP>()


<<<<<<< HEAD
=======
    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_person_bill_sigma_abs_namespace::model_binary_absence_inflate_fix_person_bill_sigma_abs, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4binary_absence_inflate_fix_sigma_abs_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> >("model_binary_absence_inflate_fix_sigma_abs")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_fix_sigma_abs_namespace::model_binary_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4binary_absence_inflate_nofix_mod) {


    class_<rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> >("model_binary_absence_inflate_nofix")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_binary_absence_inflate_nofix_namespace::model_binary_absence_inflate_nofix, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4grm_absence_inflate_mod) {


    class_<rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> >("model_grm_absence_inflate")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_grm_absence_inflate_namespace::model_grm_absence_inflate, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4grm_absence_inflate_nofix_mod) {


    class_<rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> >("model_grm_absence_inflate_nofix")

    .constructor<SEXP,SEXP>()


>>>>>>> c944809126e048a39b2578becc79588fbc11943e
    .method("call_sampler", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_grm_absence_inflate_nofix_namespace::model_grm_absence_inflate_nofix, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ratingscale_absence_inflate_mod) {


    class_<rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> >("model_ratingscale_absence_inflate")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_namespace::model_ratingscale_absence_inflate, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ratingscale_absence_inflate_fix_sigma_abs_mod) {


    class_<rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> >("model_ratingscale_absence_inflate_fix_sigma_abs")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_fix_sigma_abs_namespace::model_ratingscale_absence_inflate_fix_sigma_abs, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ratingscale_absence_inflate_generate_mod) {


    class_<rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> >("model_ratingscale_absence_inflate_generate")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_generate_namespace::model_ratingscale_absence_inflate_generate, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ratingscale_absence_inflate_nofix_mod) {


    class_<rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> >("model_ratingscale_absence_inflate_nofix")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ratingscale_absence_inflate_nofix_namespace::model_ratingscale_absence_inflate_nofix, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
