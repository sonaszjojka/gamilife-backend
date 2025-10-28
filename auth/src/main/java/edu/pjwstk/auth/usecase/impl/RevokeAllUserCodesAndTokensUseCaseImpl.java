package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.persistence.repository.EmailVerificationRepository;
import edu.pjwstk.auth.persistence.repository.ForgotPasswordCodeRepository;
import edu.pjwstk.auth.persistence.repository.RefreshTokenRepository;
import edu.pjwstk.auth.usecase.RevokeAllUserCodesAndTokensUseCase;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class RevokeAllUserCodesAndTokensUseCaseImpl implements RevokeAllUserCodesAndTokensUseCase {

    private final ForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final EmailVerificationRepository emailVerificationRepository;
    private final RefreshTokenRepository refreshTokenRepository;

    @Override
    @Transactional
    public void execute(UUID userId) {
        forgotPasswordCodeRepository.revokeAllActiveForgotPasswordCodesByUserId(userId);
        emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
        refreshTokenRepository.revokeAllActiveRefreshTokensByUserId(userId);
    }
}
