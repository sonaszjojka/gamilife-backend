package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.RotateUserTokensCommand;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.RevokeAllUserCodesAndTokensUseCase;
import edu.pjwstk.auth.usecase.RotateUserTokensUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RotateUserTokensUseCaseImpl implements RotateUserTokensUseCase {

    private final RevokeAllUserCodesAndTokensUseCase revokeAllUserCodesAndTokensUseCase;
    private final TokenService tokenService;

    @Override
    public AuthTokens execute(RotateUserTokensCommand cmd) {
        revokeAllUserCodesAndTokensUseCase.execute(cmd.userId());

        return tokenService.generateTokenPair(
                cmd.userId(),
                cmd.email(),
                cmd.isEmailVerified()
        );
    }
}
