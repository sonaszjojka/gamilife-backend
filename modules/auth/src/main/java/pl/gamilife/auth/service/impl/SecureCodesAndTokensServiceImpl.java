package pl.gamilife.auth.service.impl;

import pl.gamilife.auth.service.EmailVerificationService;
import pl.gamilife.auth.service.ForgotPasswordCodeService;
import pl.gamilife.auth.service.SecureCodesAndTokensService;
import pl.gamilife.auth.service.TokenService;
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
