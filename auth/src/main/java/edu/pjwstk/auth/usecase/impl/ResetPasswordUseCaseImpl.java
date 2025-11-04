package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.usecase.command.ResetPasswordCommand;
import edu.pjwstk.auth.exceptions.OldAndNewPasswordAreTheSameException;
import edu.pjwstk.auth.models.ForgotPasswordCode;
import edu.pjwstk.auth.repository.JpaForgotPasswordCodeRepository;
import edu.pjwstk.auth.usecase.ResetPasswordUseCase;
import edu.pjwstk.auth.usecase.RevokeAllUserCodesAndTokensUseCase;
import edu.pjwstk.auth.util.ForgotPasswordCodeUtil;
import edu.pjwstk.api.auth.exception.ResetPasswordGenericException;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
@Validated
public class ResetPasswordUseCaseImpl implements ResetPasswordUseCase {

    private final JpaForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final ForgotPasswordCodeUtil forgotPasswordCodeUtil;
    private final UserApi userApi;
    private final RevokeAllUserCodesAndTokensUseCase revokeAllUserCodesAndTokensUseCase;
    private final PasswordEncoder passwordEncoder;

    @Override
    @Transactional
    public void execute(ResetPasswordCommand command) {
        ForgotPasswordCode forgotPasswordCode = forgotPasswordCodeRepository
                .findByCodeAndRevokedAndExpiresAtIsGreaterThan(
                        forgotPasswordCodeUtil.hashCode(command.code()),
                        false,
                        LocalDateTime.now()
                )
                .orElseThrow(ResetPasswordGenericException::new);

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(forgotPasswordCode.getUserId())
                .orElseThrow(ResetPasswordGenericException::new);

        if (passwordEncoder.matches(command.newPassword(), user.password())) {
            throw new OldAndNewPasswordAreTheSameException();
        }

        userApi.resetUserPassword(
                forgotPasswordCode.getUserId(),
                passwordEncoder.encode(command.newPassword())
        );

        revokeAllUserCodesAndTokensUseCase.execute(forgotPasswordCode.getUserId());
    }
}
