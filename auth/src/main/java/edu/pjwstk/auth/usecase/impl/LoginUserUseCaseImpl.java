package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.usecase.GenerateAuthTokenPairUseCase;
import edu.pjwstk.auth.usecase.LoginUserUseCase;
import edu.pjwstk.auth.usecase.SendEmailVerificationCodeUseCase;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.SecureUserInfoApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class LoginUserUseCaseImpl implements LoginUserUseCase {

    private final UserApi userApi;
    private final PasswordEncoder passwordEncoder;
    private final GenerateAuthTokenPairUseCase generateAuthTokenPairUseCase;
    private final SendEmailVerificationCodeUseCase sendEmailVerificationCodeUseCase;

    @Override
    @Transactional
    public AuthTokens execute(LoginUserDto loginUserDto) {
        SecureUserInfoApiDto user = userApi
                .getSecureUserDataByEmail(loginUserDto.email())
                .orElseThrow(() -> new InvalidCredentialsException("Login credentials are invalid"));

        if (!passwordEncoder.matches(loginUserDto.password(), user.password())) {
            throw new InvalidCredentialsException("Login credentials are invalid");
        }

        if (!user.isEmailVerified()) {
            sendEmailVerificationCodeUseCase.execute(user.userId());
        }

        return generateAuthTokenPairUseCase.execute(user.userId(), user.email(), user.isEmailVerified());
    }
}
