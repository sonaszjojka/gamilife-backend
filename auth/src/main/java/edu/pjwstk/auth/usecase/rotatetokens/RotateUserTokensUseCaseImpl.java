package edu.pjwstk.auth.usecase.rotatetokens;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.service.SecureCodesAndTokensService;
import edu.pjwstk.auth.service.TokenService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RotateUserTokensUseCaseImpl implements RotateUserTokensUseCase {

    private final TokenService tokenService;
    private final SecureCodesAndTokensService secureCodesAndTokensService;

    @Override
    public AuthTokens executeInternal(RotateUserTokensCommand cmd) {
        secureCodesAndTokensService.revokeAllTokensAndCodesForUser(cmd.userId());

        return tokenService.generateTokenPair(
                cmd.userId(),
                cmd.email(),
                cmd.isEmailVerified()
        );
    }
}
