package pl.gamilife.auth.usecase.verifyemail;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.auth.exception.domain.EmailVerificationCodeExpiredException;
import pl.gamilife.auth.exception.domain.InvalidEmailVerificationCodeException;
import pl.gamilife.auth.models.EmailVerificationCode;
import pl.gamilife.auth.repository.JpaEmailVerificationRepository;
import pl.gamilife.auth.service.EmailVerificationService;
import pl.gamilife.auth.service.TokenService;
import pl.gamilife.auth.usecase.common.LoginUserResult;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Service
@Transactional
@AllArgsConstructor
public class VerifyEmailUseCaseImpl implements VerifyEmailUseCase {

    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationService emailVerificationService;
    private final TokenService tokenService;
    private final UserApi userApi;

    @Override
    public LoginUserResult execute(VerifyEmailCommand cmd) {
        String hashedCode = emailVerificationService.hashCode(cmd.code());

        EmailVerificationCode emailVerificationCode = emailVerificationRepository.findByUserIdAndCode(cmd.userId(), hashedCode)
                .orElseThrow(() -> new InvalidEmailVerificationCodeException("Invalid verification code."));

        if (emailVerificationCode.isRevoked() || emailVerificationCode.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new EmailVerificationCodeExpiredException("Email verification code has expired.");
        }

        BasicUserInfoApiDto user = userApi.confirmUserEmailVerification(cmd.userId());

        AuthTokens tokens = tokenService.generateTokenPair(user.userId(), user.email(), true);

        return new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                true,
                false,
                tokens
        );
    }
}
