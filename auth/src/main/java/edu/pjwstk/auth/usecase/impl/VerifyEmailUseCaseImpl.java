package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.usecase.result.LoginUserResult;
import edu.pjwstk.auth.usecase.command.VerifyEmailCommand;
import edu.pjwstk.auth.exceptions.InvalidVerificationCodeException;
import edu.pjwstk.auth.models.EmailVerificationCode;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.usecase.VerifyEmailUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.auth.util.VerificationCodeUtil;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class VerifyEmailUseCaseImpl implements VerifyEmailUseCase {

    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final VerificationCodeUtil verificationCodeUtil;
    private final TokenProvider tokenProvider;
    private final UserApi userApi;

    @Override
    public LoginUserResult execute(VerifyEmailCommand code) {
        String hashedCode = verificationCodeUtil.hashCode(code.code());

        emailVerificationRepository.findByUserIdAndCode(code.userId(), hashedCode)
                .orElseThrow(() -> new InvalidVerificationCodeException("Invalid verification code."));

        BasicUserInfoApiDto user = userApi.confirmUserEmailVerification(code.userId());

        AuthTokens tokens = tokenProvider.generateTokenPair(user.userId(), user.email(), true);

        return new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                true,
                tokens
        );
    }
}
