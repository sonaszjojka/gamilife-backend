package pl.gamilife.auth.application.rotatetokens;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.auth.service.SecureCodesAndTokensService;
import pl.gamilife.auth.service.TokenService;

@Service
@Transactional
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
