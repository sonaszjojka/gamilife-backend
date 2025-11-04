package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.RotateUserTokensCommand;
import edu.pjwstk.auth.service.SecureCodesAndTokensService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.RotateUserTokensUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RotateUserTokensUseCaseImpl implements RotateUserTokensUseCase {

    private final TokenService tokenService;
    private final SecureCodesAndTokensService secureCodesAndTokensService;

    @Override
    public AuthTokens execute(RotateUserTokensCommand cmd) {
        secureCodesAndTokensService.revokeAllTokensAndCodesForUser(cmd.userId());

        return tokenService.generateTokenPair(
                cmd.userId(),
                cmd.email(),
                cmd.isEmailVerified()
        );
    }
}
