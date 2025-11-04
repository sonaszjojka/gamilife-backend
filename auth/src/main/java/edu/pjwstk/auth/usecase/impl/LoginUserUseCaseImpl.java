package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exceptions.CannotCurrentlyCreateNewEmailVerificationCodeException;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.service.EmailVerificationService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.LoginUserUseCase;
import edu.pjwstk.auth.usecase.command.LoginUserCommand;
import edu.pjwstk.auth.usecase.result.LoginUserResult;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class LoginUserUseCaseImpl implements LoginUserUseCase {

    private final UserApi userApi;
    private final PasswordEncoder passwordEncoder;
    private final TokenService tokenService;
    private final EmailVerificationService emailVerificationService;

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
                String code = emailVerificationService.generateAndSaveEmailVerificationCode(user.userId());
                emailVerificationService.sendEmailVerificationCode(
                        user.userId(),
                        user.email(),
                        code
                );
            } catch (CannotCurrentlyCreateNewEmailVerificationCodeException ignored) {
            }
        }

        AuthTokens tokens = tokenService.generateTokenPair(user.userId(), user.email(), user.isEmailVerified());

        return new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                user.isEmailVerified(),
                tokens
        );
    }
}
