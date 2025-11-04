package edu.pjwstk.auth.usecase.verifyemail;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.auth.exceptions.InvalidVerificationCodeException;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.service.EmailVerificationService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class VerifyEmailUseCaseImpl implements VerifyEmailUseCase {

    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationService emailVerificationService;
    private final TokenService tokenService;
    private final UserApi userApi;

    @Override
    public LoginUserResult executeInternal(VerifyEmailCommand cmd) {
        String hashedCode = emailVerificationService.hashCode(cmd.code());

        emailVerificationRepository.findByUserIdAndCode(cmd.userId(), hashedCode)
                .orElseThrow(() -> new InvalidVerificationCodeException("Invalid verification code."));

        BasicUserInfoApiDto user = userApi.confirmUserEmailVerification(cmd.userId());

        AuthTokens tokens = tokenService.generateTokenPair(user.userId(), user.email(), true);

        return new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                true,
                tokens
        );
    }
}
