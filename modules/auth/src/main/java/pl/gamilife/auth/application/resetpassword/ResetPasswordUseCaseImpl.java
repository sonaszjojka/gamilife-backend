package pl.gamilife.auth.application.resetpassword;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.domain.exception.domain.OldAndNewPasswordAreTheSameException;
import pl.gamilife.auth.domain.model.ForgotPasswordCode;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.ForgotPasswordCodeRepository;
import pl.gamilife.auth.domain.validator.PasswordValidator;
import pl.gamilife.auth.service.ForgotPasswordCodeService;
import pl.gamilife.auth.service.SecureCodesAndTokensService;
import pl.gamilife.shared.kernel.exception.domain.ResetPasswordGenericException;

import java.time.LocalDateTime;

@Service
@Transactional
@AllArgsConstructor
public class ResetPasswordUseCaseImpl implements ResetPasswordUseCase {

    private final ForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final ForgotPasswordCodeService forgotPasswordCodeService;
    private final UserContext userContext;
    private final PasswordEncoder passwordEncoder;
    private final SecureCodesAndTokensService secureCodesAndTokensService;
    private final PasswordValidator passwordValidator;

    @Override
    public Void execute(ResetPasswordCommand cmd) {
        passwordValidator.validate(cmd.newPassword());

        ForgotPasswordCode forgotPasswordCode = forgotPasswordCodeRepository
                .findByCodeAndRevokedAndExpiresAtIsGreaterThan(
                        forgotPasswordCodeService.hashCode(cmd.code()),
                        false,
                        LocalDateTime.now()
                )
                .orElseThrow(ResetPasswordGenericException::new);

        SecureUserDetails user = userContext.getSecureUserDataById(forgotPasswordCode.getUserId())
                .orElseThrow(ResetPasswordGenericException::new);

        if (passwordEncoder.matches(cmd.newPassword(), user.password())) {
            throw new OldAndNewPasswordAreTheSameException();
        }

        userContext.resetUserPassword(
                forgotPasswordCode.getUserId(),
                passwordEncoder.encode(cmd.newPassword())
        );

        secureCodesAndTokensService.revokeAllTokensAndCodesForUser(forgotPasswordCode.getUserId());

        return null;
    }
}
