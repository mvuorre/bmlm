    matrix[K, K] Omega;         // Correlation matrix
    matrix[K, K] Sigma;         // Covariance matrix

    // Average mediation parameters
    real covab;                 // a-b covariance
    real corrab;                // a-b correlation
    real me;                    // Mediated effect
    real c;                     // Total effect
    real pme;                   // % mediated effect

    // Person-specific mediation parameters
    vector[J] u_a;
    vector[J] u_b;
    vector[J] u_cp;
    vector[J] u_dy;
    vector[J] u_dm;
    vector[J] u_c;
    vector[J] u_me;
    vector[J] u_pme;

    // Re-named tau parameters for easy output
    real tau_cp;
    real tau_b;
    real tau_a;
    real tau_dy;
    real tau_dm;

    tau_cp = Tau[1];
    tau_b = Tau[2];
    tau_a = Tau[3];
    tau_dy = Tau[4];
    tau_dm = Tau[5];

    Omega = L_Omega * L_Omega';
    Sigma = quad_form_diag(Omega, Tau);

    covab = Sigma[3,2];
    corrab = Omega[3,2];
    me = a*b + covab;
    c = cp + me;
    pme = me / c;

    for (j in 1:J) {
        u_a[j] = a + U[j, 3];
        u_b[j] = b + U[j, 2];
        u_me[j] = (a + U[j, 3]) * (b + U[j, 2]);
        u_cp[j] = cp + U[j, 1];
        u_dy[j] = dy + U[j, 4];
        u_dm[j] = dm + U[j, 5];
        u_c[j] = u_cp[j] + u_me[j];
        u_pme[j] = u_me[j] / u_c[j];
    }
