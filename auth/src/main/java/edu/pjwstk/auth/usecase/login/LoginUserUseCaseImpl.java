package edu.pjwstk.auth.usecase.login;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.CannotCurrentlyCreateNewEmailVerificationCodeException;
import edu.pjwstk.auth.exception.domain.InvalidCredentialsException;
import edu.pjwstk.auth.service.EmailVerificationService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@AllArgsConstructor
public class LoginUserUseCaseImpl implements LoginUserUseCase {

    private final UserApi userApi;
    private final PasswordEncoder passwordEncoder;
    private final TokenService tokenService;
    private final EmailVerificationService emailVerificationService;

    @Override
    public LoginUserResult execute(LoginUserCommand cmd) {
        SecureUserInfoApiDto user = userApi
                .getSecureUserDataByEmail(cmd.email())
                .orElseThrow(() -> new InvalidCredentialsException("Login credentials are invalid"));

        if (!passwordEncoder.matches(cmd.password(), user.password())) {
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
                user.isTutorialCompleted(),
                tokens
        );
    }
}
