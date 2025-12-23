package pl.gamilife.auth.application.usecase.changepassword;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.service.SecureCodesAndTokensService;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.exception.domain.InvalidCredentialsException;
import pl.gamilife.auth.domain.exception.domain.OldAndNewPasswordAreTheSameException;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.validator.PasswordValidator;

@Service
@AllArgsConstructor
public class ChangePasswordUseCaseImpl implements ChangePasswordUseCase {

    private final PasswordValidator passwordValidator;
    private final PasswordEncoder passwordEncoder;
    private final SecureCodesAndTokensService secureCodesAndTokensService;
    private final TokenService tokenService;
    private final UserContext userContext;

    @Override
    public AuthTokens execute(ChangePasswordCommand cmd) {
        passwordValidator.validate(cmd.newPassword());

        SecureUserDetails user = userContext.getSecureUserDataById(cmd.userId()).orElseThrow(
                () -> new InvalidCredentialsException("Invalid user")
        );

        if (!passwordEncoder.matches(cmd.providedPassword(), user.password())) {
            throw new InvalidCredentialsException("Invalid password");
        }

        if (passwordEncoder.matches(cmd.newPassword(), user.password())) {
            throw new OldAndNewPasswordAreTheSameException();
        }

        userContext.updateUserPassword(cmd.userId(), cmd.newPassword());

        secureCodesAndTokensService.revokeAllTokensAndCodesForUser(cmd.userId());

        return tokenService.generateTokenPair(
                cmd.userId(),
                user.email(),
                user.isEmailVerified()
        );
    }
}
