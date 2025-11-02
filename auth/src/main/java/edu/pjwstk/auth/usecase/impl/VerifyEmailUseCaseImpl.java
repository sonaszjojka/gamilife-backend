package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.EmailVerification;
import edu.pjwstk.auth.dto.service.EmailVerificationCode;
import edu.pjwstk.auth.dto.service.LoginUserResult;
import edu.pjwstk.auth.exceptions.InvalidVerificationCodeException;
import edu.pjwstk.auth.persistence.repository.EmailVerificationRepository;
import edu.pjwstk.auth.usecase.VerifyEmailUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.auth.util.VerificationCodeUtil;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class VerifyEmailUseCaseImpl implements VerifyEmailUseCase {

    private final EmailVerificationRepository emailVerificationRepository;
    private final VerificationCodeUtil verificationCodeUtil;
    private final TokenProvider tokenProvider;
    private final UserApi userApi;

    @Override
    public LoginUserResult execute(EmailVerificationCode code) {
        String hashedCode = verificationCodeUtil.hashCode(code.code());

        EmailVerification codeFromDb = emailVerificationRepository.findByUserIdAndCode(code.userId(), hashedCode)
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
