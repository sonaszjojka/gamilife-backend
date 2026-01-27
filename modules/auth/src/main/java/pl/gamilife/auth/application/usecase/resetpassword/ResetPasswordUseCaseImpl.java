package pl.gamilife.auth.application.usecase.resetpassword;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.service.SecureCodesAndTokensService;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.exception.domain.OldAndNewPasswordAreTheSameException;
import pl.gamilife.auth.domain.model.ForgotPasswordCode;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.ForgotPasswordCodeRepository;
import pl.gamilife.auth.domain.service.ForgotPasswordCodeService;
import pl.gamilife.auth.domain.validator.PasswordValidator;
import pl.gamilife.shared.kernel.exception.domain.ResetPasswordGenericException;

import java.time.Instant;
import java.util.UUID;

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
    private final TokenService tokenService;

    @Override
    public AuthTokens execute(ResetPasswordCommand cmd) {
        passwordValidator.validate(cmd.newPassword());

        ForgotPasswordCode forgotPasswordCode = forgotPasswordCodeRepository
                .findByCodeAndRevokedAndExpiresAtIsGreaterThan(
                        forgotPasswordCodeService.hashCode(cmd.code()),
                        false,
                        Instant.now()
                )
                .orElseThrow(ResetPasswordGenericException::new);

        if (!checkIfUserIsOwnerIfAuthenticated(forgotPasswordCode.getUserId(), cmd.authenticatedUserId())) {
            throw new ResetPasswordGenericException();
        }

        SecureUserDetails user = userContext.getSecureUserDataById(forgotPasswordCode.getUserId())
                .orElseThrow(ResetPasswordGenericException::new);

        if (passwordEncoder.matches(cmd.newPassword(), user.password())) {
            throw new OldAndNewPasswordAreTheSameException();
        }

        userContext.updateUserPassword(
                forgotPasswordCode.getUserId(),
                passwordEncoder.encode(cmd.newPassword())
        );

        secureCodesAndTokensService.revokeAllTokensAndCodesForUser(forgotPasswordCode.getUserId());

        return cmd.authenticatedUserId() != null
                ? tokenService.generateTokenPair(user.userId(), user.email(), user.isEmailVerified())
                : null;
    }

    private boolean checkIfUserIsOwnerIfAuthenticated(UUID codeOwnerUserId, UUID authenticatedUserId) {
        return authenticatedUserId == null || authenticatedUserId.equals(codeOwnerUserId);
    }
}
