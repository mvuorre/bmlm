    // Participant-level varying effects
    matrix[J, K] U;
    U = (diag_pre_multiply(Tau, L_Omega) * z_U)';
