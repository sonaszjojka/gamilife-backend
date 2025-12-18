package pl.gamilife.auth.usecase.login;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.SecureUserInfoApiDto;
import pl.gamilife.auth.exception.domain.CannotCurrentlyCreateNewEmailVerificationCodeException;
import pl.gamilife.auth.exception.domain.InvalidCredentialsException;
import pl.gamilife.auth.service.EmailVerificationService;
import pl.gamilife.auth.service.TokenService;
import pl.gamilife.auth.usecase.common.LoginUserResult;

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
                user.money(),
                tokens
        );
    }
}
