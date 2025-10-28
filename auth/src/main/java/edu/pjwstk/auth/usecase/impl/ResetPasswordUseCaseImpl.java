package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.ForgotPasswordCode;
import edu.pjwstk.auth.dto.service.ResetPasswordCommand;
import edu.pjwstk.auth.persistence.repository.ForgotPasswordCodeRepository;
import edu.pjwstk.auth.usecase.ResetPasswordUseCase;
import edu.pjwstk.auth.usecase.RevokeAllUserCodesAndTokensUseCase;
import edu.pjwstk.auth.util.ForgotPasswordCodeUtil;
import edu.pjwstk.common.authApi.exception.ResetPasswordGenericException;
import edu.pjwstk.common.userApi.UserApi;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

@Service
@AllArgsConstructor
@Validated
public class ResetPasswordUseCaseImpl implements ResetPasswordUseCase {

    private final ForgotPasswordCodeRepository repository;
    private final ForgotPasswordCodeUtil forgotPasswordCodeUtil;
    private final UserApi userApi;
    private final RevokeAllUserCodesAndTokensUseCase revokeAllUserCodesAndTokensUseCase;
    private final PasswordEncoder passwordEncoder;

    @Override
    @Transactional
    public void execute(ResetPasswordCommand command) {
        ForgotPasswordCode forgotPasswordCode = repository
                .findValidByCode(forgotPasswordCodeUtil.hashCode(command.code()))
                .orElseThrow(ResetPasswordGenericException::new);

        userApi.resetUserPassword(
                forgotPasswordCode.userId(),
                passwordEncoder.encode(command.newPassword())
        );

        revokeAllUserCodesAndTokensUseCase.execute(forgotPasswordCode.userId());
    }
}
