
Prerequisites
=============

To build and run this software, the following other software has to be installed first.

* the Gnu make utility

  tested with Version 3.81, but should work with others as well
  http://ftp.gnu.org/gnu/make/make-3.81.tar.bz2
  SHA256 (make-3.81.tar.bz2) = f3e69023771e23908f5d5592954d8271d3d6af09693cecfd29cee6fde8550dc

* bash

  tested with 4.1, but should work with other versions

  http://ftp.gnu.org/gnu/bash/bash-4.1.tar.gz
  http://ftp.gnu.org/gnu/bash/bash-4.1-patches//bash41-00{1-9}

  SHA256 (bash/bash-4.1.tar.gz) = 3f627124a83c6d34db503a923e20710d370573a29dd5d11d6f116d1aee7be1da
  SHA256 (bash/bash41-001) = a6e47fa108f853d0f0999520509c11680d37c8b7823b92b96d431766dd620278
  SHA256 (bash/bash41-002) = 322e229de186b3bd87dedabfbad8386716f103e87ff00cd1b2db844e0dff78f8
  SHA256 (bash/bash41-003) = 91763dddbbb98c3ff7deb3faea3b3ad6e861e7bfd2e46c045c0d1d85d1b3256d
  SHA256 (bash/bash41-004) = 78c063ba34c1f390a5bf2e5727624ca2e253bbef49ce187cabb240eee7f4ff9e
  SHA256 (bash/bash41-005) = 519639d8d1664be74d7ec15879d16337fe8c71af8d648b02f84ccdd4fb739c1a
  SHA256 (bash/bash41-006) = 5986abcf33c0b087bd5670f1ae6a6400a8ce6ab7e7c4de18b9826d0ee10f2c49
  SHA256 (bash/bash41-007) = 74012a2c28ba4fb532c3eb69155ac870aac8d53990fa4e1e52cdc173d4c205a7
  SHA256 (bash/bash41-008) = 4e2c2c251432cdf6b84dd5b4fd06d698f4dbeabb56a6c247ca8bf18443481215
  SHA256 (bash/bash41-009) = bd4006964ae88a5ed8032c16208130084efd43866afe19bb88a167e0c0f156d1

* Python

  tested with versions 2.6 and 2.7, but should work with other 2.x versions as well
  http://www.python.org/ftp/python/2.7.1/Python-2.7.1.tgz
  SHA256 (python/Python-2.7.1.tgz) = ca13e7b1860821494f70de017202283ad73b1fb7bd88586401c54ef958226ec8

* Python library

  tested with 1.2.0, but should work with others as well

  http://pypi.python.org/packages/source/p/py/py-1.2.0.tar.gz
  SHA256 (py-1.2.0.tar.gz) = 1b7349b52aeee1fc69d2fb6e30e6c642179ae78f8a671041c4759fe0fcd1d287

* Python tables

  tested with 2.2.1, but should work with others as well

  http://www.pytables.org/download/stable/tables-2.2.1.tar.gz
  SHA256 (tables-2.2.1.tar.gz) = 65dbdb7d3d6f37be01ebd9ececae8b165b60c4648381abef9d08d76c1a1a6cb1

* ocaml

  tested with 3.11.2, but should also with other versions
  SHA256 (ocaml-3.11.2.tar.bz2) = 86f3387a0d7e7c8be2a3c53af083a5a726e333686208d5ea0dd6bb5ac3f58143

* ocaml findlib

  tested with 1.2.6, but should also with other versions
  http://download.camlcity.org/download/findlib-1.2.6.tar.gz
  SHA256 (findlib-1.2.6.tar.gz) = badf1d81a96322491e7ffa144c22fe04b106340b8279c15d54bd4b4d3d2ddfac

* ocamlgsl

  tested with 0.6.0

  http://oandrieu.nerim.net/ocaml/gsl/ocamlgsl-0.6.0.tar.gz
  SHA256 (ocamlgsl-0.6.0.tar.gz) = ddb9e432dc1a431cad332a8e7e6685d2facbde2fa15ecab9315741f7d11b588c

* qhull

  tested with 2003.1

  http://www.qhull.org/download/qhull-2003.1.tar.gz
  http://www.qhull.org/download/poly.c-qh_gethash.patch

  SHA256 (qhull-2003.1.tar.gz) = 68725c96603a426da748d38d0f83e7a9dd6a0bfc483525debe04001846475b0b
  SHA256 (poly.c-qh_gethash.patch) = fb42b86fa80e1d4e671043a07ff3bdf4ad641094f93630241877fce096916737

* parmetis

  Tested with 3.1

  http://glaros.dtc.umn.edu/gkhome/fetch/sw/parmetis/ParMetis-3.1.tar.gz
  SHA256 (ParMetis-3.1.tar.gz) = 11485828fe25436dc6d41cb048bc4b744969b51d4b751996007612aa95d5ff16

* hdf5

  Tested with 1.6.9

  ftp://ftp.hdfgroup.org/HDF5/prev-releases/hdf5-1.6.9/src/hdf5-1.6.9.tar.gz
  SHA256 (hdf5-1.6.9.tar.gz) = 15ff93cbbf40c2c9a84638e53f8b0dfe7e92b142b337702a300c7db106fce89b

* blas

  Tested with 1.0

  http://www.netlib.org/blas/blas.tgz
  SHA256 (blas.tgz) = bc2f25898141c3ed9513abe3b3f15e00f0d2e8881c7f26b74950cdee45fb541d

* petsc

  Tested with 2.3.3

  ftp://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-2.3.3-p0.tar.gz
  SHA256 (petsc-2.3.3-p0.tar.gz) = a08d0f2022349321a26ae6c63f0ad5b069d37f48028377c549b81cc3e7b97ddf

* sundials

  !!! WARNING: Only works with the outdated version 2.3.0 !!!

  https://computation.llnl.gov/casc/sundials/download/code/sundials-2.3.0.tar.gz
  SHA256 (sundials-2.3.0.tar.gz) = c3a9ec0679ee37b1684925117de2a3b61290a4ab00c4b4f853d5795d09e7ee4e

* an MPI implementation

  tested with openmpi 1.4.3, but should work with other mpi implementations as well

  http://www.open-mpi.org/software/ompi/v1.4/downloads/openmpi-1.4.3.tar.bz2
  SHA256 (openmpi-1.4.3.tar.bz2) = 220b72b1c7ee35469ff74b4cfdbec457158ac6894635143a33e9178aa3981015


