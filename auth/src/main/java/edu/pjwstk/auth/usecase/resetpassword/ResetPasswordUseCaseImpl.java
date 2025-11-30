package edu.pjwstk.auth.usecase.resetpassword;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.OldAndNewPasswordAreTheSameException;
import edu.pjwstk.auth.models.ForgotPasswordCode;
import edu.pjwstk.auth.repository.JpaForgotPasswordCodeRepository;
import edu.pjwstk.auth.service.ForgotPasswordCodeService;
import edu.pjwstk.auth.service.SecureCodesAndTokensService;
import edu.pjwstk.auth.validators.PasswordValidator;
import edu.pjwstk.core.exception.common.domain.ResetPasswordGenericException;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
