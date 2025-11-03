package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.repository.JpaForgotPasswordCodeRepository;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.usecase.RevokeAllUserCodesAndTokensUseCase;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class RevokeAllUserCodesAndTokensUseCaseImpl implements RevokeAllUserCodesAndTokensUseCase {

    private final JpaForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final JpaRefreshTokenRepository refreshTokenRepository;

    @Override
    @Transactional
    public void execute(UUID userId) {
        forgotPasswordCodeRepository.revokeAllActiveForgotPasswordCodesByUserId(userId);
        emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
        refreshTokenRepository.revokeAllActiveRefreshTokensByUserId(userId);
    }
}
