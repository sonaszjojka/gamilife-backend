package pl.gamilife.auth.application.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.application.service.SecureCodesAndTokensService;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.service.EmailVerificationService;
import pl.gamilife.auth.domain.service.ForgotPasswordCodeService;

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
