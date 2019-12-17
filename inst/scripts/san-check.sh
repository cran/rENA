export  _R_CHECK_CRAN_INCOMING_='1',
export  _R_CHECK_CRAN_INCOMING_REMOTE_='1',
export  _R_CHECK_FORCE_SUGGESTS_='1',
export  _R_CLASS_MATRIX_ARRAY_='1',
export  R_REMOTES_STANDALONE='1',
export  R_REMOTES_NO_ERRORS_FROM_WARNINGS='1'

RD CMD build .

RD CMD check rENA_0.2.0.1.tar.gz --as-cran
