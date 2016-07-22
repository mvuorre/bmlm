    // Participant-level varying effects obtained from vectorized and
    // standardized z_U matrix
    matrix[J, K] U;

    // Sample varying effects from Cholesky factorized covariance matrix
    // diag_pre_multiply(tau, L_Omega) = Cholesky covariance matrix
    U = (diag_pre_multiply(tau, L_Omega) * z_U)';
