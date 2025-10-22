package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.persistence.repository.RefreshTokenRepository;
import edu.pjwstk.auth.usecase.GenerateAuthTokenPairUseCase;
import edu.pjwstk.auth.usecase.RotateUserTokensUseCase;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.authApi.dto.RotateUserTokensCommand;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RotateUserTokensUseCaseImpl implements RotateUserTokensUseCase {

    private final RefreshTokenRepository refreshTokenRepository;
    private final GenerateAuthTokenPairUseCase generateAuthTokenPairUseCase;

    @Override
    public AuthTokens execute(RotateUserTokensCommand cmd) {
        refreshTokenRepository.revokeAllActiveRefreshTokensByUserId(cmd.userId());

        return generateAuthTokenPairUseCase.execute(
                cmd.userId(),
                cmd.email(),
                cmd.isEmailVerified()
        );
    }
}
