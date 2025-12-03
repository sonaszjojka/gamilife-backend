package pl.gamilife.auth.usecase.resetpassword;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.SecureUserInfoApiDto;
import pl.gamilife.auth.exception.domain.OldAndNewPasswordAreTheSameException;
import pl.gamilife.auth.models.ForgotPasswordCode;
import pl.gamilife.auth.repository.JpaForgotPasswordCodeRepository;
import pl.gamilife.auth.service.ForgotPasswordCodeService;
import pl.gamilife.auth.service.SecureCodesAndTokensService;
import pl.gamilife.auth.validators.PasswordValidator;
import pl.gamilife.infrastructure.core.exception.common.domain.ResetPasswordGenericException;

import java.time.LocalDateTime;

@Service
@Transactional
@AllArgsConstructor
public class ResetPasswordUseCaseImpl implements ResetPasswordUseCase {

    private final JpaForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final ForgotPasswordCodeService forgotPasswordCodeService;
    private final UserApi userApi;
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

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(forgotPasswordCode.getUserId())
                .orElseThrow(ResetPasswordGenericException::new);

        if (passwordEncoder.matches(cmd.newPassword(), user.password())) {
            throw new OldAndNewPasswordAreTheSameException();
        }

        userApi.resetUserPassword(
                forgotPasswordCode.getUserId(),
                passwordEncoder.encode(cmd.newPassword())
        );

        secureCodesAndTokensService.revokeAllTokensAndCodesForUser(forgotPasswordCode.getUserId());

        return null;
    }
}
