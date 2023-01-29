M_empirical_temp <- function (Linf = NULL, Winf = NULL, K_l = NULL, K_w = NULL, temp = NULL,
                              tmax = NULL, tm50 = NULL, GSI = NULL, Wdry = NULL, Wwet = NULL,
                              Bl = NULL, schooling = FALSE, method, boot = NULL, CI = 95)
{
    if (!is.null(boot) & class(boot) == "lfqBoot") {
        bootOut <- boot
        bootRaw <- boot$bootRaw
        if (any(method == "Pauly_Linf") & any(is.null(bootRaw$Linf),
            is.null(bootRaw$K), is.null(temp)))
            stop("Pauly_Linf requires temp and a boot object with columns Linf and K")
        if (any(method == "Then_growth") & any(is.null(bootRaw$Linf),
            is.null(bootRaw$K)))
            stop("Then_growth requires a boot object with columns Linf and K")
        if (any(method == "Pauly_Linf")) {
            Ms <- round(10^(-0.0066 - 0.279 * log10(bootRaw$Linf) +
                0.6543 * log10(bootRaw$K) + 0.4634 * log10(temp)),
                3)
            if (schooling == TRUE) {
                Ms <- 0.8 * Ms
            }
            bootRaw[, ncol(bootRaw) + 1] <- Ms
            colnames(bootRaw) <- c(colnames(bootRaw)[-ncol(bootRaw)],
                "M_Pauly")
        }
        if (any(method == "Then_growth")) {
            bootRaw[, ncol(bootRaw) + 1] <- round(4.118 * (bootRaw$K^0.73) *
                (bootRaw$Linf^-0.33), 3)
            colnames(bootRaw) <- c(colnames(bootRaw)[-ncol(bootRaw)],
                "M_Then")
        }
        tmp <- as.data.frame(bootRaw[, (ncol(boot$bootRaw) +
            1:length(method))])
        resMaxDen <- vector("numeric", ncol(tmp))
        resMed <- vector("numeric", ncol(tmp))
        ciList <- vector("list", ncol(tmp))
        for (i in seq(length(method))) {
            resMed[i] <- median(tmp[, i], na.rm = TRUE)
            citmp <- (100 - CI)/2/100
            ciList[[i]] <- quantile(tmp[, i], probs = c(citmp,
                1 - citmp), na.rm = TRUE)
            x <- try(ks::kde(as.numeric(na.omit(tmp[, i]))),
                TRUE)
            if (class(x) != "try-error") {
                ind <- which(x$estimate > x$cont["99%"])
                resMaxDen[i] <- mean(x$eval.points[ind])
            }
            else {
                if ((length(unique(as.character(tmp[, i]))) ==
                  1 && all(!is.na(tmp[, i]))) | (length(unique(as.character(na.omit(tmp[,
                  i])))) == 1)) {
                  resMaxDen[i] <- unique(as.numeric(na.omit(tmp[,
                    i])))
                }
                else {
                  resMaxDen[i] <- NA
                }
            }
        }
        resMaxDen <- c(boot$maxDen, resMaxDen)
        names(resMaxDen) <- c(names(boot$maxDen), colnames(bootRaw)[ncol(bootRaw)])
        resCIs <- cbind(boot$CI, t(do.call(rbind, ciList)))
        colnames(resCIs) <- names(resMaxDen)
        rownames(resCIs) <- c("lo", "up")
        resMed <- c(boot$median, resMed)
        names(resMed) <- names(resMaxDen)
        ret <- list()
        ret$bootRaw <- bootRaw
        ret$seed <- boot$seed
        ret$maxDen <- resMaxDen
        ret$median <- resMed
        ret$CI <- resCIs
        if ("multiCI" %in% names(boot))
            ret$multiCI <- boot$multiCI
        class(ret) <- "lfqBoot"
        return(ret)
    }
    else if (!is.null(boot) & class(boot) != "lfqBoot") {
        stop("You provided an object for boot, but it does not have class 'lfqBoot'. Please check.")
    }
    else {
        if (any(method == "AlversonCarney") & any(is.null(tmax),
            is.null(K_l)))
            stop("AlversonCarney requires K_l and tmax")
        if (any(method == "Gislason") & any(is.null(Linf), is.null(K_l),
            is.null(Bl)))
            stop("Gislason requires Linf, K_l, and Bl")
        if (any(method == "GundersonDygert") & is.null(GSI))
            stop("GundersonDygert requires GSI")
        if (any(method == "Hoenig") & is.null(tmax))
            stop("Hoenig requires tmax")
        if (any(method == "Lorenzen") & is.null(Wwet))
            stop("Lorenzen requires Wwet")
        if (any(method == "Pauly_Linf") & any(is.null(Linf),
            is.null(K_l), is.null(temp)))
            stop("Pauly_Linf requires Linf, K_l, and temp")
        if (any(method == "Pauly_Winf") & any(is.null(Winf),
            is.null(K_w), is.null(temp)))
            stop("Pauly_Winf requires Winf, K_w, and temp")
        if (any(method == "PetersonWroblewski") & is.null(Wdry))
            stop("PetersonWroblewski requires Wdry")
        if (any(method == "RikhterEfanov") & any(is.null(tm50)))
            stop("RikhterEfanov requires K_l and tm50")
        if (any(method == "Roff") & any(is.null(tm50), is.null(K_l)))
            stop("Roff requires K_l and tm50")
        if (any(method == "Then_tmax") & any(is.null(tmax)))
            stop("Then_max requires tmax")
        if (any(method == "Then_growth") & any(is.null(Linf),
            is.null(K_l)))
            stop("Then_growth requires Linf and K_l")
        n <- length(method)
        if (any(method == "Hoenig"))
            n <- n + 1
        M_mat <- matrix(NA, n, 1L)
        dimnames(M_mat) <- list(rep(NA, n), c("M"))
        ind <- 0
        if (any(method == "AlversonCarney")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round((3 * K_l)/(exp(K_l * (0.38 *
                tmax)) - 1), 3)
            dimnames(M_mat)[[1]][ind] <- list("Alverson and Carney (1975)")
        }
        if (any(method == "GundersonDygert")) {
            ind <- ind + 1
            M <- round(0.03 + 1.68 * GSI, 3)
            dimnames(M_mat)[[1]][ind] <- list("Gunderson and Dygert (1988)")
        }
        if (any(method == "Hoenig")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(4.22/(tmax^0.982), 3)
            dimnames(M_mat)[[1]][ind] <- list("Hoenig (1983) - Joint Equation")
            ind <- ind + 1
            M_mat[ind, 1] <- round(exp(1.46 - 1.01 * log(tmax)),
                3)
            dimnames(M_mat)[[1]][ind] <- list("Hoenig (1983) - Fish Equation")
        }
        if (any(method == "Lorenzen")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(3 * (Wwet^-0.288), 3)
            dimnames(M_mat)[[1]][ind] <- list("Lorenzen (1996)")
        }
        if (any(method == "Pauly_Linf")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(10^(-0.0066 - 0.279 * log10(Linf) +
                0.6543 * log10(K_l) + 0.4634 * log10(temp)),
                3)
            dimnames(M_mat)[[1]][ind] <- list("Pauly (1980) - Length Equation")
            if (schooling == TRUE) {
                M_mat[ind, 1] <- 0.8 * M_mat[ind, 1]
            }
        }
        if (any(method == "Pauly_Winf")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(10^(-0.2107 - 0.0824 * log10(Winf) +
                0.6757 * log10(K_w) + 0.4627 * log10(temp)),
                3)
            dimnames(M_mat)[[1]][ind] <- list("Pauly (1980) - Weight Equation")
            if (schooling == TRUE) {
                M_mat[ind, 1] <- 0.8 * M_mat[ind, 1]
            }
        }
        if (any(method == "PetersonWroblewski")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(1.92 * (Wdry^-0.25), 3)
            dimnames(M_mat)[[1]][ind] <- list("Peterson and Wroblewski (1984)")
        }
        if (any(method == "RikhterEfanov")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(1.521/(tm50^0.72) - 0.155,
                3)
            dimnames(M_mat)[[1]][ind] <- list("Rikhter and Efanov (1976)")
        }
        if (any(method == "Roff")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round((3 * K_l)/(exp(K_l * tm50) -
                1), 3)
            dimnames(M_mat)[[1]][ind] <- list("Roff (1984)")
        }
        if (any(method == "Then_tmax")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(4.899 * tmax^-0.916, 3)
            dimnames(M_mat)[[1]][ind] <- list("Then (2015) - tmax")
        }
        if (any(method == "Then_growth")) {
            ind <- ind + 1
            M_mat[ind, 1] <- round(4.118 * (K_l^0.73) * (Linf^-0.33),
                3)
            dimnames(M_mat)[[1]][ind] <- list("Then (2015) - growth")
        }
        if (any(method == "Gislason")) {
            Ml <- round(exp(0.55 - 1.61 * log(Bl) + 1.44 * log(Linf) +
                log(K_l)), 3)
            M_mat <- as.data.frame(matrix(c(Bl, Ml), byrow = FALSE,
                ncol = 2))
            colnames(M_mat) <- c("Bl", "Ml")
        }
        if (any(method == "Gislason2")) {
            Ml <- round(exp(0.55 - 1.61 * log(Bl) + 1.44 * log(Linf) + log(K_l)), 3)
            Ml[Bl <= 10] <- round(exp(0.55 - 1.61 * log(10) + 1.44 * log(Linf) + log(K_l)), 3)
            M_mat <- as.data.frame(matrix(c(Bl, Ml), byrow = FALSE, ncol = 2))
            colnames(M_mat) <- c("Bl", "Ml")
        }
        if (any(method == "Lorenzen_2022")) {
            Ml <- round(exp(0.28 - 1.3 * log(Bl/Linf) + 1.08 * log(K_l)), 3)
            M_mat <- as.data.frame(matrix(c(Bl, Ml), byrow = FALSE, ncol = 2))
            colnames(M_mat) <- c("Bl", "Ml")
        }
        return(M_mat)
    }
}
