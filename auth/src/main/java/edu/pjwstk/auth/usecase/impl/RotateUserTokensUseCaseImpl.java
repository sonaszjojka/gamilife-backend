package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.usecase.GenerateAuthTokenPairUseCase;
import edu.pjwstk.auth.usecase.RevokeAllUserCodesAndTokensUseCase;
import edu.pjwstk.auth.usecase.RotateUserTokensUseCase;
import edu.pjwstk.api.auth.dto.RotateUserTokensCommand;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RotateUserTokensUseCaseImpl implements RotateUserTokensUseCase {

    private final RevokeAllUserCodesAndTokensUseCase revokeAllUserCodesAndTokensUseCase;
    private final GenerateAuthTokenPairUseCase generateAuthTokenPairUseCase;

    @Override
    public AuthTokens execute(RotateUserTokensCommand cmd) {
        revokeAllUserCodesAndTokensUseCase.execute(cmd.userId());

        return generateAuthTokenPairUseCase.execute(
                cmd.userId(),
                cmd.email(),
                cmd.isEmailVerified()
        );
    }
}
