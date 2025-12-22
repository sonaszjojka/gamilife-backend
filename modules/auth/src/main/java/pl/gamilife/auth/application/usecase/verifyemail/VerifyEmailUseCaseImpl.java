package pl.gamilife.auth.application.usecase.verifyemail;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.exception.domain.EmailVerificationCodeExpiredException;
import pl.gamilife.auth.domain.exception.domain.InvalidEmailVerificationCodeException;
import pl.gamilife.auth.domain.model.EmailVerificationCode;
import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.EmailVerificationRepository;
import pl.gamilife.auth.domain.service.EmailVerificationService;

import java.time.LocalDateTime;

@Service
@Transactional
@AllArgsConstructor
public class VerifyEmailUseCaseImpl implements VerifyEmailUseCase {

    private final EmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationService emailVerificationService;
    private final TokenService tokenService;
    private final UserContext userContext;

    @Override
    public LoginUserResult execute(VerifyEmailCommand cmd) {
        String hashedCode = emailVerificationService.hashCode(cmd.code());

        EmailVerificationCode emailVerificationCode = emailVerificationRepository.findByUserIdAndCode(cmd.userId(), hashedCode)
                .orElseThrow(() -> new InvalidEmailVerificationCodeException("Invalid verification code."));

        if (emailVerificationCode.isRevoked() || emailVerificationCode.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new EmailVerificationCodeExpiredException("Email verification code has expired.");
        }

        BasicUserDetails user = userContext.confirmUserEmailVerification(cmd.userId());

        AuthTokens tokens = tokenService.generateTokenPair(user.userId(), user.email(), true);

        return new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                true,
                false,
                user.money(),
                tokens
        );
    }
}
