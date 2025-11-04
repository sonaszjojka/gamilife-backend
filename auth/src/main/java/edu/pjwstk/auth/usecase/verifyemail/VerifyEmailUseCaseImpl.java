package edu.pjwstk.auth.usecase.verifyemail;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.auth.exceptions.InvalidVerificationCodeException;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.service.EmailVerificationService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.login.LoginUserResult;
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
    public LoginUserResult execute(VerifyEmailCommand code) {
        String hashedCode = emailVerificationService.hashCode(code.code());

        emailVerificationRepository.findByUserIdAndCode(code.userId(), hashedCode)
                .orElseThrow(() -> new InvalidVerificationCodeException("Invalid verification code."));

        BasicUserInfoApiDto user = userApi.confirmUserEmailVerification(code.userId());

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
