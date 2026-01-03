package pl.gamilife.auth.application.usecase.login;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.exception.domain.CannotCurrentlyCreateNewEmailVerificationCodeException;
import pl.gamilife.auth.domain.exception.domain.InvalidCredentialsException;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.service.EmailVerificationService;

@Service
@Transactional
@AllArgsConstructor
public class LoginUserUseCaseImpl implements LoginUserUseCase {

    private final UserContext userContext;
    private final PasswordEncoder passwordEncoder;
    private final TokenService tokenService;
    private final EmailVerificationService emailVerificationService;

    @Override
    public LoginUserResult execute(LoginUserCommand cmd) {
        SecureUserDetails user = userContext
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
                        code
                );
            } catch (CannotCurrentlyCreateNewEmailVerificationCodeException ignored) {
                // Empty for safety purposes
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
