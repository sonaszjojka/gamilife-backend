package edu.pjwstk.auth.service.impl;

import edu.pjwstk.auth.service.EmailVerificationService;
import edu.pjwstk.auth.service.ForgotPasswordCodeService;
import edu.pjwstk.auth.service.SecureCodesAndTokensService;
import edu.pjwstk.auth.service.TokenService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class SecureCodesAndTokensServiceImpl implements SecureCodesAndTokensService {

    private final TokenService tokenService;
    private final ForgotPasswordCodeService forgotPasswordCodeService;
    private final EmailVerificationService emailVerificationService;

    @Override
    public void revokeAllTokensAndCodesForUser(UUID userId) {
        tokenService.revokeAllActiveRefreshTokensByUserId(userId);
        forgotPasswordCodeService.revokeAllActiveForgotPasswordCodesByUserId(userId);
        emailVerificationService.revokeAllActiveEmailVerificationCodesByUserId(userId);
    }
}
