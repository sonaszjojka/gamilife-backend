package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.usecase.command.LoginUserCommand;
import edu.pjwstk.auth.usecase.result.LoginUserResult;
import edu.pjwstk.auth.exceptions.CannotCurrentlyCreateNewEmailVerificationCodeException;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.usecase.GenerateAuthTokenPairUseCase;
import edu.pjwstk.auth.usecase.LoginUserUseCase;
import edu.pjwstk.auth.usecase.SendEmailVerificationCodeUseCase;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class LoginUserUseCaseImpl implements LoginUserUseCase {

    private final UserApi userApi;
    private final PasswordEncoder passwordEncoder;
    private final GenerateAuthTokenPairUseCase generateAuthTokenPairUseCase;
    private final SendEmailVerificationCodeUseCase sendEmailVerificationCodeUseCase;

    @Override
    @Transactional
    public LoginUserResult execute(LoginUserCommand loginUserCommand) {
        SecureUserInfoApiDto user = userApi
                .getSecureUserDataByEmail(loginUserCommand.email())
                .orElseThrow(() -> new InvalidCredentialsException("Login credentials are invalid"));

        if (!passwordEncoder.matches(loginUserCommand.password(), user.password())) {
            throw new InvalidCredentialsException("Login credentials are invalid");
        }

        if (!user.isEmailVerified()) {
            try {
                sendEmailVerificationCodeUseCase.execute(user.userId());
            } catch (CannotCurrentlyCreateNewEmailVerificationCodeException ignored) {
            }
        }

        AuthTokens tokens = generateAuthTokenPairUseCase.execute(user.userId(), user.email(), user.isEmailVerified());

        return new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                user.isEmailVerified(),
                tokens
        );
    }
}
