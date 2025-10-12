package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.usecase.GenerateAuthTokenPairUseCase;
import edu.pjwstk.auth.usecase.LoginUserUseCase;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
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

    @Override
    @Transactional
    public AuthTokens execute(LoginUserDto loginUserDto) {
        BasicUserInfoApiDto user = userApi
                .getUserByEmail(loginUserDto.email())
                .orElseThrow(() -> new InvalidCredentialsException("Login credentials are invalid"));

        if (!passwordEncoder.matches(loginUserDto.password(), user.password())) {
            throw new InvalidCredentialsException("Login credentials are invalid");
        }

        return generateAuthTokenPairUseCase.execute(user.userId(), user.email());
    }
}
